//! Generate Rust code for VPP API files
//!
//! A small Rust-based code generator that produces Rust bindings and helper scaffolding for VPP
//! `.api` files for implementing VPP plugins. This crate can be run from a build script (see
//! [`Builder`]) and emits both Rust source and a JSON representation of the parsed API.
//!
//! # Design rationale (why a Rust crate, not an extension to `vppapigen.py`)
//!
//! - Insulation against upstream changes: integrating with vppapigen.py via code outside of the vpp
//!   repository like this would need to make the assumption that the vppapigen.py internal API
//!   doesn't change in non-backwards-compatible ways which would be unreasonable to
//!   assume. Implementing the parser standalone avoids that issue, although it does now mean that
//!   any extensions to the API grammer used by plugins or their imports would need to be
//!   implemented here.
//! - Dependency control: shipping the generator as a crate avoids introducing an extra Python
//!   dependencies into build systems that cannot be expressed via crate dependencies.
//! - Robustness against environment: performing a `cargo build` when set into an unrelated Python
//!   venv could lead to spurious failures if the venv doesn't have all of the Python dependencies
//!   needed by vppapigen.py. Implementing in Rust avoids that.
//!
//! # Parsing strategy: why PEG (Parsing Expression Grammar) over CFG
//!
//! The implementation chooses a PEG-style parser (implemented in `parser.rs`) rather than a
//! traditional context-free grammar (CFG) parser generator for several practical reasons:
//!
//! - Determinism and simplicity: PEGs are deterministic and describe a single unambiguous parse for
//!   any input (given the grammar and choice ordering).
//!   For the `.api` format — which is relatively small, regular and unambiguous — a PEG results in
//!   simpler grammars and parsing code without needing extra disambiguation rules.
//! - Ergonomics in Rust: mature PEG libraries and small hand-written PEG parsers are
//!   straightforward to implement and embed in a Rust crate. CFG tools (LR, LALR) are typically
//!   geared toward generating parser tables and a runtime which is less ergonomic to integrate into
//!   a small generator crate and tends to complicate error reporting and tooling.
//! - Better error locality: PEGs (and hand-written recursive-descent parsers) make it easier to
//!   attach localized error messages and recover cleanly for diagnostics or partial parsing.
//!
//! Trade-offs and caveats:
//! - PEG grammars do not support left-recursive rules naturally. For the `.api` grammar this is not
//!   a practical limitation because the syntax is not left-recursive and is well-suited to a PEG
//!   style.
//! - CFG-based parser generators can handle certain ambiguous grammars more naturally and can
//!   produce more compact parser tables for very large and complex grammars. Here, the space of
//!   constructs is small and well-bounded, so the simplicity and determinism of PEG were preferred.

#![warn(
    missing_docs,
    missing_copy_implementations,
    missing_debug_implementations
)]

use std::{
    env,
    fs::{DirBuilder, File},
    io::Write,
    path::Path,
};

use thiserror::Error;

use crate::{
    json::generate_json,
    parser::{ApiParser, Field, FieldSize, Message, Type, Union, VL_API_PREFIX, VL_API_SUFFIX},
};

mod json;
mod parser;

/// Errors that can occur during API file parsing and code generation
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum Error {
    /// Parser error
    #[error("Parser error")]
    Parser(#[from] parser::Error),
    /// Input/output error
    #[error("I/O error")]
    Io(#[from] std::io::Error),
    /// Error whilst generating JSON
    #[error("Failed to generate JSON")]
    Json(#[from] serde_json::Error),
    /// Attempt to use functionality that isn't yet implemented
    #[error("{0}")]
    Unimplemented(String),
}

fn to_upper_camel_case(s: &str) -> String {
    s.split('_')
        .flat_map(|word| {
            let word = word.to_ascii_lowercase();
            let mut chars = word.chars();
            let capital = chars.next().map(|x| x.to_ascii_uppercase());
            capital.into_iter().chain(chars).collect::<Vec<_>>()
        })
        .collect()
}

fn to_rust_type(r#type: &str) -> Result<String, Error> {
    Ok(if let Some(t) = r#type.strip_prefix(VL_API_PREFIX) {
        if let Some(t) = t.strip_suffix(VL_API_SUFFIX) {
            to_upper_camel_case(t)
        } else {
            r#type.to_string()
        }
    } else if r#type == "string" {
        return Err(Error::Unimplemented(
            "string type not implemented".to_string(),
        ));
    } else {
        r#type.to_string()
    })
}

/// Generate Rust code for the VPP handling of APIs of from a `.api` file
///
/// # Examples
///
/// Example of use from a build script:
///
/// ```no_run
/// use std::{env, path::PathBuf};
///
/// let output_dir = PathBuf::from(env::var("OUT_DIR").unwrap()).join("src");
/// vpp_plugin_api_gen::Builder::new("example.api", &output_dir.to_string_lossy())
///     .expect("unable to generate API binding")
///     .generate()
///     .expect("unable to generate API binding");
/// ```
///
/// This can then be include in the plugin as follows:
///
/// ```ignore
/// mod example_api {
///     include!(concat!(env!("OUT_DIR"), "/src/example_api.rs"));
/// }
/// ```
#[derive(Debug)]
pub struct Builder {
    parser: ApiParser,

    module: String,
    output_file: File,
    output_json_file: File,
}

impl Builder {
    /// Construct a new `Builder` for the given input file and outputting to the given directory
    ///
    /// Both a `<api-module>_api.rs` and a `<api-module>.api.json` file will be generated in
    /// the output directory.
    pub fn new(input_file: &str, output_dir: &str) -> Result<Self, Error> {
        let in_build_script = env::var("OUT_DIR").is_ok() && env::var("CARGO_MANIFEST_DIR").is_ok();
        if in_build_script {
            println!("cargo:cargo-rerun-if-changed={}", input_file);
        }

        // Get file name without path
        let input_file_name = Path::new(input_file).iter().next_back().unwrap();
        let module = Path::new(input_file_name)
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .to_string();

        DirBuilder::new().recursive(true).create(output_dir)?;
        let output_file = Path::new(output_dir).join(format!("{}_api.rs", module));
        let output_json_file = Path::new(output_dir).join(format!("{}.api.json", module));

        let parser = ApiParser::new(input_file)?;

        if in_build_script {
            for import in parser.imports() {
                println!("cargo:cargo-rerun-if-changed={}", import);
            }
        }

        Ok(Self {
            parser,
            module,
            output_file: File::create(&output_file)?,
            output_json_file: File::create(&output_json_file)?,
        })
    }

    /// Generate Rust code for the API file
    ///
    /// Both a `<api-module>_api.rs` and a `<api-module>.api.json` file will be generated in
    /// the output directory.
    pub fn generate(self) -> Result<(), Error> {
        ApiGenContext {
            parser: &self.parser,
            module: self.module,
            output_file: self.output_file,
            output_json_file: self.output_json_file,
        }
        .generate()
    }
}

/// Helper structure for API code generation
///
/// Mark parser as non-mutable to avoid borrow-check errors when passing references obtained from
/// parser into `&mut self` methods.
struct ApiGenContext<'a> {
    parser: &'a ApiParser,

    module: String,
    output_file: File,
    output_json_file: File,
}

impl ApiGenContext<'_> {
    fn generate_message(&mut self, id: usize, message: &Message) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(message.name());

        if let Some(comment) = message.comment() {
            writeln!(
                self.output_file,
                "#[doc = \"{}\"]",
                comment.replace("\"", "\\\"")
            )?;
        }
        let opt_derives = if message.manual_print() {
            ""
        } else {
            "Debug, PartialEq, "
        };
        writeln!(self.output_file, "#[derive({}Copy, Clone)]", opt_derives)?;
        writeln!(self.output_file, "#[repr(C, packed)]")?;
        writeln!(self.output_file, "pub struct {} {{", upper_camel_name)?;
        for field in message.fields() {
            // TODO: array types
            writeln!(
                self.output_file,
                "    pub {}: {},",
                field.name,
                to_rust_type(&field.r#type)?
            )?;
        }
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        writeln!(self.output_file, "impl {} {{", upper_camel_name)?;
        writeln!(self.output_file, "    pub const MSG_ID: u16 = {};", id)?;
        writeln!(self.output_file)?;
        writeln!(self.output_file, "    pub fn msg_id() -> u16 {{")?;
        writeln!(self.output_file, "        msg_id_base() + Self::MSG_ID")?;
        writeln!(self.output_file, "    }}")?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        writeln!(self.output_file, "impl Default for {} {{", upper_camel_name)?;
        writeln!(self.output_file, "    fn default() -> Self {{")?;
        writeln!(self.output_file, "        Self {{")?;
        for field in message.fields() {
            if field.name == "_vl_msg_id" {
                writeln!(self.output_file, "            _vl_msg_id: Self::msg_id(),")?;
            } else {
                writeln!(
                    self.output_file,
                    "            {}: Default::default(),",
                    field.name,
                )?;
            }
        }
        writeln!(self.output_file, "        }}")?;
        writeln!(self.output_file, "    }}")?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        self.generate_endian_swap(message.name(), message.fields())?;

        writeln!(
            self.output_file,
            "unsafe extern \"C\" fn {}_endian(a: *mut {}, to_net: bool) {{",
            message.name(),
            upper_camel_name
        )?;
        writeln!(
            self.output_file,
            "    ::vpp_plugin::vlibapi::EndianSwap::endian_swap(&mut *a, to_net);"
        )?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        writeln!(
            self.output_file,
            "unsafe extern \"C\" fn {}_format(s: *mut u8, args: *mut ::vpp_plugin::bindings::va_list) -> *mut u8 {{",
            message.name()
        )?;
        writeln!(
            self.output_file,
            "    let mut args = ::std::mem::transmute::<*mut ::vpp_plugin::bindings::va_list, ::vpp_plugin::macro_support::va_list::VaList<'_>>(args);"
        )?;
        writeln!(
            self.output_file,
            "    let t = args.get::<*const {}>();",
            upper_camel_name
        )?;
        writeln!(
            self.output_file,
            "    let mut s = ::vpp_plugin::vppinfra::vec::Vec::from_raw(s);"
        )?;
        writeln!(
            self.output_file,
            "    s.extend(format!(\"{{:?}}\", &*t).as_bytes());"
        )?;
        writeln!(self.output_file, "    s.into_raw()")?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;
        writeln!(
            self.output_file,
            "unsafe extern \"C\" fn {}_calc_size(_a: *mut {}) -> ::vpp_plugin::bindings::uword {{",
            message.name(),
            upper_camel_name
        )?;
        // TODO: variable array types
        writeln!(
            self.output_file,
            "    std::mem::size_of::<{}>() as ::vpp_plugin::bindings::uword",
            upper_camel_name
        )?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;
        Ok(())
    }

    fn generate_messages(&mut self) -> Result<(), Error> {
        for (id, message) in self.parser.messages().iter().enumerate() {
            self.generate_message(id, message)?;
        }
        Ok(())
    }

    fn generate_alias(&mut self, alias: &Field) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(&alias.name);
        // TODO: generate newtypes for manual_print
        if let Some(FieldSize::Fixed(length)) = alias.size {
            writeln!(
                self.output_file,
                "pub type {} = [{}; {}];",
                upper_camel_name,
                to_rust_type(&alias.r#type)?,
                length
            )?;
        } else {
            writeln!(
                self.output_file,
                "pub type {} = {};",
                upper_camel_name,
                to_rust_type(&alias.r#type)?
            )?;
        }

        Ok(())
    }

    fn generate_aliases(&mut self) -> Result<(), Error> {
        for message in self.parser.aliases() {
            self.generate_alias(message)?;
        }
        Ok(())
    }

    fn generate_enums(&mut self) -> Result<(), Error> {
        if let Some(e) = self.parser.enums().first() {
            return Err(Error::Unimplemented(format!(
                "Generating code for enums is not yet implemented (enum type {})",
                e.name()
            )));
        }
        if let Some(e) = self.parser.enumflags().first() {
            return Err(Error::Unimplemented(format!(
                "Generating code for enumflags is not yet implemented (enumflag type {})",
                e.name()
            )));
        }
        Ok(())
    }

    fn generate_union(&mut self, un: &Union) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(un.name());

        if let Some(comment) = un.comment() {
            writeln!(
                self.output_file,
                "#[doc = \"{}\"]",
                comment.replace("\"", "\\\"")
            )?;
        }
        writeln!(self.output_file, "#[derive(Copy, Clone)]",)?;
        writeln!(self.output_file, "#[repr(C, packed)]")?;
        writeln!(self.output_file, "pub union {} {{", upper_camel_name)?;
        for field in un.fields() {
            // TODO: array types
            writeln!(
                self.output_file,
                "    pub {}: {},",
                field.name,
                to_rust_type(&field.r#type)?
            )?;
        }
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;
        writeln!(
            self.output_file,
            "unsafe extern \"C\" fn {}_endian(a: *mut {}, _to_net: bool) {{",
            un.name(),
            upper_camel_name
        )?;
        for field in un.fields() {
            // TODO: array types
            match field.r#type.as_str() {
                "u8" | "string" | "bool" => {
                    writeln!(
                        self.output_file,
                        "    // (*a).{} = (*a).{} (no-op)",
                        field.name, field.name
                    )?;
                }
                "u16" | "u32" | "u64" | "i16" | "i32" | "i64" => {
                    writeln!(
                        self.output_file,
                        "    (*a).{} = (*a).{}.to_be();",
                        field.name, field.name
                    )?;
                }
                "f64" => {
                    writeln!(
                        self.output_file,
                        "    (*a).{} = f64::from_be_bytes((*a).{}.to_be_bytes());",
                        field.name, field.name
                    )?;
                }
                _ => {
                    writeln!(
                        self.output_file,
                        "    {}_endian(::std::ptr::addr_of_mut!((*a).{}), to_net);",
                        field.r#type, field.name
                    )?;
                }
            }
        }
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;
        writeln!(
            self.output_file,
            "unsafe extern \"C\" fn {}_calc_size(_a: *mut {}) -> ::vpp_plugin::bindings::uword {{",
            un.name(),
            upper_camel_name
        )?;
        // TODO: variable array types
        writeln!(
            self.output_file,
            "    std::mem::size_of::<{}>() as ::vpp_plugin::bindings::uword",
            upper_camel_name
        )?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;
        Ok(())
    }

    fn generate_unions(&mut self) -> Result<(), Error> {
        for un in self.parser.unions() {
            self.generate_union(un)?;
        }
        Ok(())
    }

    fn generate_endian_swap(&mut self, name: &str, fields: &[Field]) -> Result<(), Error> {
        writeln!(
            self.output_file,
            "impl ::vpp_plugin::vlibapi::EndianSwap for {} {{",
            to_upper_camel_case(name)
        )?;
        writeln!(
            self.output_file,
            "    fn endian_swap(&mut self, to_net: bool) {{",
        )?;
        // Suppress potential used variable warning
        writeln!(self.output_file, "        let _ = to_net;",)?;
        for field in fields {
            // TODO: array types
            match field.r#type.as_str() {
                "u8" | "string" | "bool" => {
                    writeln!(
                        self.output_file,
                        "        // self.{} = self.{} (no-op)",
                        field.name, field.name
                    )?;
                }
                "u16" | "u32" | "u64" | "i16" | "i32" | "i64" => {
                    writeln!(
                        self.output_file,
                        "        self.{} = self.{}.to_be();",
                        field.name, field.name
                    )?;
                }
                "f64" => {
                    writeln!(
                        self.output_file,
                        "        self.{} = f64::from_be_bytes(self.{}.to_be_bytes());",
                        field.name, field.name
                    )?;
                }
                _ => {
                    writeln!(
                        self.output_file,
                        "        ::vpp_plugin::vlibapi::EndianSwap::endian_swap(&mut self.{}, to_net);",
                        field.name
                    )?;
                }
            }
        }
        writeln!(self.output_file, "    }}",)?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        Ok(())
    }

    fn generate_type(&mut self, t: &Type) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(t.name());

        let opt_derives = if t.manual_print() {
            ""
        } else {
            "Debug, PartialEq, Default, "
        };
        writeln!(self.output_file, "#[derive({}Copy, Clone)]", opt_derives)?;
        writeln!(self.output_file, "#[repr(C, packed)]")?;
        writeln!(self.output_file, "pub struct {} {{", upper_camel_name)?;
        for field in t.fields() {
            // TODO: array types
            writeln!(
                self.output_file,
                "    pub {}: {},",
                field.name,
                to_rust_type(&field.r#type)?
            )?;
        }
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;
        if !t.manual_endian() {
            self.generate_endian_swap(t.name(), t.fields())?;
        }
        writeln!(
            self.output_file,
            "unsafe extern \"C\" fn vl_api_{}_t_calc_size(_a: *mut {}) -> ::vpp_plugin::bindings::uword {{",
            t.name(),
            upper_camel_name
        )?;
        // TODO: variable array types
        writeln!(
            self.output_file,
            "    std::mem::size_of::<{}>() as ::vpp_plugin::bindings::uword",
            upper_camel_name
        )?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;
        Ok(())
    }

    fn generate_types(&mut self) -> Result<(), Error> {
        for t in self.parser.types() {
            self.generate_type(t)?;
        }
        Ok(())
    }

    fn generate_register(&mut self) -> Result<(), Error> {
        writeln!(self.output_file, "pub trait Handlers {{")?;
        for service in self.parser.services() {
            let caller_upper_camel = to_upper_camel_case(service.caller());
            if service.reply() == "null" {
                writeln!(
                    self.output_file,
                    "    fn {}(vm: &::vpp_plugin::vlib::BarrierHeldMainRef, mp: &{});",
                    service.caller(),
                    caller_upper_camel
                )?;
            } else {
                // TODO: support variable array type for reply
                let reply_message = format!(
                    "::vpp_plugin::vlibapi::Message<{}>",
                    to_upper_camel_case(service.reply())
                );
                let retval_in_reply_msg = self
                    .parser
                    .message(service.reply())
                    .map(|reply| reply.has_retval())
                    .unwrap_or_default();
                if retval_in_reply_msg {
                    writeln!(
                        self.output_file,
                        "    fn {}(vm: &::vpp_plugin::vlib::BarrierHeldMainRef, mp: &{}) -> Result<{}, i32>;",
                        service.caller(),
                        caller_upper_camel,
                        reply_message
                    )?;
                } else {
                    writeln!(
                        self.output_file,
                        "    fn {}(vm: &::vpp_plugin::vlib::BarrierHeldMainRef, mp: &{}) -> {};",
                        service.caller(),
                        caller_upper_camel,
                        reply_message
                    )?;
                }
            }
        }
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        for service in self.parser.services() {
            let caller_upper_camel = to_upper_camel_case(service.caller());
            writeln!(
                self.output_file,
                "unsafe extern \"C\" fn {}_handler_raw<H: Handlers>(mp: *mut {}) {{",
                service.caller(),
                caller_upper_camel
            )?;
            writeln!(
                self.output_file,
                "    let vm = ::vpp_plugin::vlib::BarrierHeldMainRef::from_ptr_mut("
            )?;
            writeln!(
                self.output_file,
                "        ::vpp_plugin::bindings::vlib_get_main_not_inline(),"
            )?;
            writeln!(self.output_file, "    );")?;
            writeln!(self.output_file, "    let mp = &*mp;")?;
            if service.reply() == "null" {
                writeln!(self.output_file, "    H::{}(vm, mp);", service.caller())?;
            } else {
                let retval_in_reply_msg = self
                    .parser
                    .message(service.reply())
                    .map(|reply| reply.has_retval())
                    .unwrap_or_default();
                if retval_in_reply_msg {
                    writeln!(
                        self.output_file,
                        "    let mut reply = match H::{}(vm, mp) {{",
                        service.caller()
                    )?;
                    writeln!(self.output_file, "        Ok(reply) => reply,")?;
                    writeln!(
                        self.output_file,
                        "        Err(retval) => {} {{",
                        to_upper_camel_case(service.reply())
                    )?;
                    writeln!(self.output_file, "            retval,")?;
                    writeln!(self.output_file, "            ..Default::default()")?;
                    writeln!(self.output_file, "        }}")?;
                    writeln!(self.output_file, "        .into(),")?;
                    writeln!(self.output_file, "    }};")?;
                } else {
                    writeln!(
                        self.output_file,
                        "    let mut reply = H::{}(vm, mp);",
                        service.caller()
                    )?;
                }
                writeln!(
                    self.output_file,
                    "    ::vpp_plugin::vlibapi::registration_scope(|s| {{"
                )?;
                // TODO: check for client_index field in caller
                // TODO: check for context field in caller and reply
                writeln!(
                    self.output_file,
                    "        if let Some(reg) = s.from_client_index(vm, mp.client_index) {{"
                )?;
                writeln!(self.output_file, "            reply.context = mp.context;",)?;
                writeln!(
                    self.output_file,
                    "            {}_endian(::std::ptr::addr_of_mut!(*reply), true);",
                    service.reply()
                )?;
                writeln!(self.output_file, "            reg.send_message(reply);")?;
                writeln!(self.output_file, "        }}")?;
                writeln!(self.output_file, "    }})")?;
            }
            writeln!(self.output_file, "}}")?;
            writeln!(self.output_file)?;
        }

        writeln!(
            self.output_file,
            "pub const MESSAGE_COUNT: u16 = {};",
            self.parser.messages().len()
        )?;
        writeln!(self.output_file)?;

        writeln!(
            self.output_file,
            "static MSG_ID_BASE: ::std::sync::atomic::AtomicU16 = ::std::sync::atomic::AtomicU16::new(0);"
        )?;
        writeln!(self.output_file)?;
        writeln!(self.output_file, "pub fn msg_id_base() -> u16 {{")?;
        writeln!(
            self.output_file,
            "    MSG_ID_BASE.load(::std::sync::atomic::Ordering::Relaxed)"
        )?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        writeln!(
            self.output_file,
            "pub fn {}_register_messages<H: Handlers>() {{",
            self.module
        )?;
        writeln!(self.output_file, "    unsafe {{")?;
        writeln!(
            self.output_file,
            "        let am = ::vpp_plugin::bindings::vlibapi_helper_get_main();"
        )?;
        writeln!(
            self.output_file,
            "        let mut json_api_repr = ::vpp_plugin::vppinfra::vec::Vec::from_raw((*am).json_api_repr);"
        )?;
        writeln!(self.output_file, "        json_api_repr.push(",)?;
        writeln!(
            self.output_file,
            "            concat!(include_str!(\"{}.api.json\"), \"\\0\")",
            self.module
        )?;
        writeln!(self.output_file, "                .as_ptr()",)?;
        writeln!(self.output_file, "                .cast_mut(),",)?;
        writeln!(self.output_file, "        );",)?;
        writeln!(
            self.output_file,
            "        (*am).json_api_repr = json_api_repr.into_raw();"
        )?;
        writeln!(self.output_file)?;
        writeln!(
            self.output_file,
            "        let msg_id_base = ::vpp_plugin::bindings::vl_msg_api_get_msg_ids(",
        )?;
        writeln!(
            self.output_file,
            "            c\"{}_{:08x}\".as_ptr() as *mut ::std::os::raw::c_char,",
            self.module,
            self.parser.file_crc()
        )?;
        writeln!(self.output_file, "            MESSAGE_COUNT as i32,",)?;
        writeln!(self.output_file, "        );",)?;
        writeln!(self.output_file)?;
        for message in self.parser.messages() {
            writeln!(
                self.output_file,
                "        ::vpp_plugin::bindings::vl_msg_api_add_msg_name_crc("
            )?;
            writeln!(self.output_file, "            am,")?;
            writeln!(
                self.output_file,
                "            c\"{}_{:08x}\".as_ptr() as *mut ::std::os::raw::c_char,",
                message.name(),
                message.crc()
            )?;
            writeln!(
                self.output_file,
                "            {}::MSG_ID as u32 + msg_id_base as u32,",
                to_upper_camel_case(message.name()),
            )?;
            writeln!(self.output_file, "        );")?;
            writeln!(self.output_file)?;
        }
        for service in self.parser.services() {
            let caller = self.parser.message(service.caller()).unwrap();
            let caller_upper_camel = to_upper_camel_case(service.caller());
            writeln!(
                self.output_file,
                "        let mut c = vpp_plugin::bindings::vl_msg_api_msg_config_t {{"
            )?;
            writeln!(
                self.output_file,
                "            id: {}::MSG_ID as i32 + msg_id_base as i32,",
                caller_upper_camel,
            )?;
            writeln!(
                self.output_file,
                "            name: c\"{}\".as_ptr() as *mut ::std::os::raw::c_char,",
                service.caller()
            )?;
            writeln!(
                self.output_file,
                "            handler: {}_handler_raw::<H> as *mut ::std::os::raw::c_void,",
                service.caller()
            )?;
            writeln!(
                self.output_file,
                "            endian: {}_endian as *mut ::std::os::raw::c_void,",
                service.caller()
            )?;
            writeln!(
                self.output_file,
                "            format_fn: {}_format as *mut ::std::os::raw::c_void,",
                service.caller()
            )?;
            writeln!(
                self.output_file,
                "            tojson: std::ptr::null_mut(),"
            )?;
            writeln!(
                self.output_file,
                "            fromjson: std::ptr::null_mut(),"
            )?;
            writeln!(
                self.output_file,
                "            calc_size: {}_calc_size as *mut ::std::os::raw::c_void,",
                service.caller()
            )?;
            writeln!(self.output_file, "            ..Default::default()")?;
            writeln!(self.output_file, "        }};")?;
            writeln!(self.output_file, "        c.set_traced(1);")?;
            writeln!(self.output_file, "        c.set_replay(1);")?;
            // TODO: enforce always auto-endian?
            if caller.auto_endian() {
                writeln!(self.output_file, "        c.set_is_autoendian(1);")?;
            }
            writeln!(
                self.output_file,
                "        ::vpp_plugin::bindings::vl_msg_api_config(std::ptr::addr_of_mut!(c));"
            )?;
            writeln!(self.output_file)?;

            if service.reply() != "null" {
                let reply = self.parser.message(service.reply()).unwrap();
                let reply_upper_camel = to_upper_camel_case(service.reply());
                writeln!(
                    self.output_file,
                    "        let mut c = vpp_plugin::bindings::vl_msg_api_msg_config_t {{"
                )?;
                writeln!(
                    self.output_file,
                    "            id: {}::MSG_ID as i32 + msg_id_base as i32,",
                    reply_upper_camel
                )?;
                writeln!(
                    self.output_file,
                    "            name: c\"{}\".as_ptr() as *mut ::std::os::raw::c_char,",
                    service.reply()
                )?;
                writeln!(
                    self.output_file,
                    "            handler: ::std::ptr::null_mut(),"
                )?;
                writeln!(
                    self.output_file,
                    "            endian: {}_endian as *mut ::std::os::raw::c_void,",
                    service.reply()
                )?;
                writeln!(
                    self.output_file,
                    "            format_fn: {}_format as *mut ::std::os::raw::c_void,",
                    service.reply()
                )?;
                writeln!(
                    self.output_file,
                    "            tojson: std::ptr::null_mut(),"
                )?;
                writeln!(
                    self.output_file,
                    "            fromjson: std::ptr::null_mut(),"
                )?;
                writeln!(
                    self.output_file,
                    "            calc_size: {}_calc_size as *mut ::std::os::raw::c_void,",
                    service.reply()
                )?;
                writeln!(self.output_file, "            ..Default::default()")?;
                writeln!(self.output_file, "        }};")?;
                writeln!(self.output_file, "        c.set_traced(1);")?;
                writeln!(self.output_file, "        c.set_replay(1);")?;
                // TODO: enforce always auto-endian?
                if reply.auto_endian() {
                    writeln!(self.output_file, "        c.set_is_autoendian(1);")?;
                }
                writeln!(
                    self.output_file,
                    "        ::vpp_plugin::bindings::vl_msg_api_config(std::ptr::addr_of_mut!(c));"
                )?;
                writeln!(self.output_file)?;
            }

            if let Some(stream_message_name) = service.stream_message() {
                let stream_message = self.parser.message(stream_message_name).unwrap();
                let stream_message_upper_camel = to_upper_camel_case(stream_message_name);
                writeln!(
                    self.output_file,
                    "        let mut c = vpp_plugin::bindings::vl_msg_api_msg_config_t {{"
                )?;
                writeln!(
                    self.output_file,
                    "            id: {}::MSG_ID as i32 + msg_id_base as i32,",
                    stream_message_upper_camel
                )?;
                writeln!(
                    self.output_file,
                    "            name: c\"{}\".as_ptr() as *mut ::std::os::raw::c_char,",
                    stream_message_name
                )?;
                writeln!(
                    self.output_file,
                    "            handler: ::std::ptr::null_mut(),"
                )?;
                writeln!(
                    self.output_file,
                    "            endian: {}_endian as *mut ::std::os::raw::c_void,",
                    stream_message_name
                )?;
                writeln!(
                    self.output_file,
                    "            format_fn: {}_format as *mut ::std::os::raw::c_void,",
                    stream_message_name
                )?;
                writeln!(
                    self.output_file,
                    "            tojson: std::ptr::null_mut(),"
                )?;
                writeln!(
                    self.output_file,
                    "            fromjson: std::ptr::null_mut(),"
                )?;
                writeln!(
                    self.output_file,
                    "            calc_size: {}_calc_size as *mut ::std::os::raw::c_void,",
                    stream_message_name
                )?;
                writeln!(self.output_file, "            ..Default::default()")?;
                writeln!(self.output_file, "        }};")?;
                writeln!(self.output_file, "        c.set_traced(1);")?;
                writeln!(self.output_file, "        c.set_replay(1);")?;
                // TODO: enforce always auto-endian?
                if stream_message.auto_endian() {
                    writeln!(self.output_file, "        c.set_is_autoendian(1);")?;
                }
                writeln!(
                    self.output_file,
                    "        ::vpp_plugin::bindings::vl_msg_api_config(std::ptr::addr_of_mut!(c));"
                )?;
                writeln!(self.output_file)?;
            }
        }

        writeln!(
            self.output_file,
            "        MSG_ID_BASE.store(msg_id_base, ::std::sync::atomic::Ordering::Relaxed);"
        )?;
        writeln!(self.output_file, "    }}")?;
        writeln!(self.output_file, "}}")?;

        Ok(())
    }

    fn generate(mut self) -> Result<(), Error> {
        self.output_json_file
            .write_all(generate_json(self.parser)?.as_bytes())?;
        self.generate_aliases()?;
        self.generate_enums()?;
        self.generate_unions()?;
        self.generate_types()?;
        self.generate_messages()?;
        self.generate_register()?;
        Ok(())
    }
}
