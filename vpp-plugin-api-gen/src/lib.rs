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
    parser::{
        Alias, ApiParser, CountDescriptor, Enum, Field, FieldSize, Message, Type, Union,
        VL_API_PREFIX, VL_API_SUFFIX,
    },
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

trait ApiParserRustExt {
    fn to_rust_type(&self, r#type: &str) -> Result<String, Error>;
    fn to_rust_vla_elem_type(&self, r#type: &str) -> Result<String, Error>;
}

impl ApiParserRustExt for ApiParser {
    fn to_rust_type(&self, r#type: &str) -> Result<String, Error> {
        Ok(
            if let Some(t) = r#type.strip_prefix(VL_API_PREFIX)
                && let Some(t) = t.strip_suffix(VL_API_SUFFIX)
            {
                let global_type = self.lookup_global_type(t)?;
                if let Some(import_module) = global_type.import_module() {
                    format!("super::{}_api::{}", import_module, to_upper_camel_case(t))
                } else {
                    to_upper_camel_case(t)
                }
            } else {
                r#type.to_string()
            },
        )
    }

    fn to_rust_vla_elem_type(&self, r#type: &str) -> Result<String, Error> {
        Ok(match r#type {
            // Note: u8 excluded here as it already has an alignment of 1 byte
            "u16" | "i16" | "u32" | "i32" | "u64" | "i64" | "f64" => {
                format!(
                    "::vpp_plugin::vlibapi::num_unaligned::Unaligned{}",
                    to_upper_camel_case(r#type)
                )
            }
            _ => self.to_rust_type(r#type)?,
        })
    }
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

enum EndianSwapInput<'a> {
    Fields(&'a [Field]),
    Alias(&'a Field),
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
    fn generate_field(&mut self, field: &Field) -> Result<(), Error> {
        if field.r#type == "string" {
            match &field.size {
                Some(FieldSize::Fixed(size)) => {
                    writeln!(
                        self.output_file,
                        "    pub {}: ::vpp_plugin::vlibapi::ApiFixedString<{}>,",
                        field.name, size,
                    )?;
                    return Ok(());
                }
                Some(FieldSize::Variable(None)) => {
                    writeln!(
                        self.output_file,
                        "    pub {}: ::vpp_plugin::vlibapi::ApiString,",
                        field.name,
                    )?;
                    return Ok(());
                }
                _ => {}
            }
        }

        match &field.size {
            Some(FieldSize::Fixed(size)) => {
                writeln!(
                    self.output_file,
                    "    pub {}: [{}; {}],",
                    field.name,
                    self.parser.to_rust_type(&field.r#type)?,
                    size,
                )?;
            }
            Some(FieldSize::Variable(_)) => {
                writeln!(
                    self.output_file,
                    "    pub {}: [{}; 0],",
                    field.name,
                    self.parser.to_rust_vla_elem_type(&field.r#type)?,
                )?;
            }
            None => {
                writeln!(
                    self.output_file,
                    "    pub {}: {},",
                    field.name,
                    self.parser.to_rust_type(&field.r#type)?,
                )?;
            }
        }
        Ok(())
    }

    /// Generate an implementation of `fmt::Debug`
    ///
    /// Variable-length arrays are omitted, other than strings. Since the types are packed, fields
    /// are copied out to temporaries, except for a last field containing a string, which can't
    /// be copied and so is referenced in place (which is sound since generated types and
    /// `ApiString` have an alignment of 1).
    fn generate_debug_trait(
        &mut self,
        name: &str,
        fields: &[Field],
        last_field_contains_string: bool,
    ) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(name);
        let by_ref = |i: usize| last_field_contains_string && i == fields.len() - 1;
        let printed = |i: usize, field: &Field| {
            by_ref(i) || !matches!(field.size, Some(FieldSize::Variable(_)))
        };

        writeln!(
            self.output_file,
            "impl ::std::fmt::Debug for {} {{",
            upper_camel_name
        )?;
        // Suppress warnings about tmp__vl_msg_id (and any similar)
        writeln!(self.output_file, "    #[allow(non_snake_case)]")?;
        writeln!(
            self.output_file,
            "    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {{"
        )?;
        for (i, field) in fields.iter().enumerate() {
            if printed(i, field) {
                writeln!(
                    self.output_file,
                    "        let tmp_{} = {}self.{};",
                    field.name,
                    if by_ref(i) { "&" } else { "" },
                    field.name
                )?;
            }
        }
        writeln!(
            self.output_file,
            "        f.debug_struct(\"{}\")",
            upper_camel_name
        )?;
        for (i, field) in fields.iter().enumerate() {
            if printed(i, field) {
                writeln!(
                    self.output_file,
                    "            .field(\"{}\", &tmp_{})",
                    field.name, field.name
                )?;
            }
        }
        if fields
            .iter()
            .enumerate()
            .all(|(i, field)| printed(i, field))
        {
            writeln!(self.output_file, "            .finish()")?;
        } else {
            writeln!(self.output_file, "            .finish_non_exhaustive()")?;
        }
        writeln!(self.output_file, "    }}")?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        Ok(())
    }

    /// Generate the derives for a message or type
    ///
    /// Types containing a string can't be `Copy` or `Clone`, since `ApiString` isn't, because a
    /// copy would only contain the length of the string, not its contents.
    fn generate_derives(&mut self, opt_derives: &str, contains_string: bool) -> Result<(), Error> {
        let derives = if contains_string {
            opt_derives.trim_end_matches(", ").to_string()
        } else {
            format!("{}Copy, Clone", opt_derives)
        };
        if !derives.is_empty() {
            writeln!(self.output_file, "#[derive({})]", derives)?;
        }
        Ok(())
    }

    fn generate_vla_accessors(&mut self, count_field: &str, field: &Field) -> Result<(), Error> {
        let vla_elem_type = self.parser.to_rust_vla_elem_type(&field.r#type)?;
        writeln!(self.output_file, "    #[allow(dead_code)]")?;
        writeln!(
            self.output_file,
            "    pub unsafe fn {}(&self) -> &[{}] {{",
            field.name, vla_elem_type
        )?;
        writeln!(self.output_file, "        unsafe {{",)?;
        writeln!(
            self.output_file,
            "            ::std::slice::from_raw_parts(std::ptr::addr_of!(self.{}).cast(), self.{} as usize)",
            field.name, count_field
        )?;
        writeln!(self.output_file, "        }}",)?;
        writeln!(self.output_file, "    }}")?;
        writeln!(self.output_file)?;
        writeln!(self.output_file, "    #[allow(dead_code)]")?;
        writeln!(
            self.output_file,
            "    pub unsafe fn {}_mut(&mut self) -> &mut [{}] {{",
            field.name, vla_elem_type
        )?;
        writeln!(self.output_file, "        unsafe {{",)?;
        writeln!(
            self.output_file,
            "            std::slice::from_raw_parts_mut(std::ptr::addr_of_mut!(self.{}).cast(), self.{} as usize)",
            field.name, count_field
        )?;
        writeln!(self.output_file, "        }}",)?;
        writeln!(self.output_file, "    }}")?;

        Ok(())
    }

    fn generate_message(&mut self, id: usize, message: &Message) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(message.name());

        if let Some(comment) = message.comment() {
            writeln!(
                self.output_file,
                "#[doc = \"{}\"]",
                comment.replace("\"", "\\\"")
            )?;
        }
        let contains_string = matches!(
            message.vla(self.parser),
            Some((_, CountDescriptor::String(_)))
        );
        let opt_derives =
            if message.manual_print() || message.vla_non_recursive().is_some() || contains_string {
                ""
            } else if message.vla(self.parser).is_some() {
                "Debug, "
            } else {
                "Debug, PartialEq, "
            };
        self.generate_derives(opt_derives, contains_string)?;
        writeln!(self.output_file, "#[repr(C, packed)]")?;
        writeln!(self.output_file, "pub struct {} {{", upper_camel_name)?;
        for field in message.fields() {
            self.generate_field(field)?;
        }
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        writeln!(self.output_file, "impl {} {{", upper_camel_name)?;
        writeln!(self.output_file, "    pub const MSG_ID: u16 = {};", id)?;
        writeln!(self.output_file)?;
        writeln!(self.output_file, "    pub fn msg_id() -> u16 {{")?;
        writeln!(self.output_file, "        msg_id_base() + Self::MSG_ID")?;
        writeln!(self.output_file, "    }}")?;
        if let Some((field, count_descr)) = message.vla(self.parser) {
            writeln!(self.output_file)?;
            if let Some(FieldSize::Variable(Some(count_field))) = &field.size {
                self.generate_vla_accessors(count_field, field)?;
                writeln!(self.output_file)?;
            }
            writeln!(self.output_file, "    #[allow(dead_code)]")?;
            match count_descr {
                CountDescriptor::Field { path, r#type } => {
                    let var_name = path.last().cloned().unwrap_or_default();
                    writeln!(
                        self.output_file,
                        "    pub fn new_message({}: {}) -> ::vpp_plugin::vlibapi::Message<Self> {{",
                        var_name, r#type
                    )?;
                    // Avoid clippy::unnecessary_cast warning by only casting when the count field isn't a u32
                    let count_expr = if r#type == "u32" {
                        var_name.clone()
                    } else {
                        format!("{} as u32", var_name)
                    };
                    writeln!(
                        self.output_file,
                        "        let size = ::std::mem::size_of::<Self>() as u32 + {} * ::std::mem::size_of::<{}>() as u32;",
                        count_expr,
                        self.parser.to_rust_type(&field.r#type)?,
                    )?;
                    writeln!(
                        self.output_file,
                        "        let mut message = unsafe {{ ::std::mem::transmute::<::vpp_plugin::vlibapi::Message<u8>, ::vpp_plugin::vlibapi::Message<Self>>(::vpp_plugin::vlibapi::Message::new_bytes(size)) }};",
                    )?;
                    writeln!(
                        self.output_file,
                        "        message._vl_msg_id = Self::msg_id();",
                    )?;
                    writeln!(
                        self.output_file,
                        "        message.{} = {};",
                        path.join("."),
                        var_name,
                    )?;
                }
                CountDescriptor::String(path) => {
                    writeln!(
                        self.output_file,
                        "    pub fn new_message(length: u32) -> ::vpp_plugin::vlibapi::Message<Self> {{",
                    )?;
                    writeln!(
                        self.output_file,
                        "        let size = ::std::mem::size_of::<Self>() as u32 + length;",
                    )?;
                    writeln!(
                        self.output_file,
                        "        let mut message = unsafe {{ ::std::mem::transmute::<::vpp_plugin::vlibapi::Message<u8>, ::vpp_plugin::vlibapi::Message<Self>>(::vpp_plugin::vlibapi::Message::new_bytes(size)) }};",
                    )?;
                    writeln!(
                        self.output_file,
                        "        message._vl_msg_id = Self::msg_id();",
                    )?;
                    writeln!(
                        self.output_file,
                        "        unsafe {{ message.{}.set_len(length); }}",
                        path.join("."),
                    )?;
                }
            }
            writeln!(self.output_file, "        message",)?;
            writeln!(self.output_file, "    }}")?;
            writeln!(self.output_file)?;
        }
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

        self.generate_endian_swap(message.name(), EndianSwapInput::Fields(message.fields()))?;

        // Manually implement fmt::Debug so that the zero-length (but actually variable-length)
        // field isn't printed to avoid misleading anyone looking at the output, or because the
        // message contains a string and so can't be copied, which the derived implementation
        // requires for packed types
        if message.vla_non_recursive().is_some() || (contains_string && !message.manual_print()) {
            self.generate_debug_trait(message.name(), message.fields(), contains_string)?;
        }

        writeln!(
            self.output_file,
            "unsafe extern \"C\" fn {}_endian(a: *mut {}, to_net: bool) {{",
            message.name(),
            upper_camel_name
        )?;
        writeln!(self.output_file, "    unsafe {{")?;
        writeln!(
            self.output_file,
            "        ::vpp_plugin::vlibapi::EndianSwap::endian_swap(&mut *a, to_net);"
        )?;
        writeln!(self.output_file, "    }}")?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        writeln!(
            self.output_file,
            "unsafe extern \"C\" fn {}_format(s: *mut u8, args: *mut ::vpp_plugin::bindings::va_list) -> *mut u8 {{",
            message.name()
        )?;
        writeln!(self.output_file, "    unsafe {{")?;
        writeln!(
            self.output_file,
            "        let mut args = ::std::mem::transmute::<*mut ::vpp_plugin::bindings::va_list, ::vpp_plugin::macro_support::va_list::VaList<'_>>(args);"
        )?;
        writeln!(
            self.output_file,
            "        let t = args.get::<*const {}>();",
            upper_camel_name
        )?;
        writeln!(
            self.output_file,
            "        let mut s = ::vpp_plugin::vppinfra::vec::Vec::from_raw(s);"
        )?;
        writeln!(
            self.output_file,
            "        s.extend(format!(\"{{:?}}\", *t).as_bytes());"
        )?;
        writeln!(self.output_file, "        s.into_raw()")?;
        writeln!(self.output_file, "    }}")?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;
        if let Some((field, count_descr)) = message.vla(self.parser) {
            write!(self.output_file, "pub ",)?;
            writeln!(
                self.output_file,
                "unsafe extern \"C\" fn {}_calc_size(a: *mut {}) -> ::vpp_plugin::bindings::uword {{",
                message.name(),
                upper_camel_name
            )?;
            writeln!(self.output_file, "    unsafe {{")?;
            write!(
                self.output_file,
                "        ::std::mem::size_of::<{}>() as ::vpp_plugin::bindings::uword",
                upper_camel_name,
            )?;
            match count_descr {
                CountDescriptor::Field {
                    path: count_path,
                    r#type: count_type,
                } => match count_type.as_str() {
                    "u8" => {
                        writeln!(
                            self.output_file,
                            " + (*a).{} as ::vpp_plugin::bindings::uword * ::std::mem::size_of::<{}>() as ::vpp_plugin::bindings::uword",
                            count_path.join("."),
                            self.parser.to_rust_type(&field.r#type)?,
                        )?;
                    }
                    "u16" | "u32" | "u64" | "i16" | "i32" | "i64" => {
                        writeln!(
                            self.output_file,
                            " + {}::from_be((*a).{}) as ::vpp_plugin::bindings::uword * ::std::mem::size_of::<{}>() as ::vpp_plugin::bindings::uword",
                            self.parser.to_rust_type(&count_type)?,
                            count_path.join("."),
                            self.parser.to_rust_type(&field.r#type)?,
                        )?;
                    }
                    _ => {
                        return Err(Error::Unimplemented(format!(
                            "Unexpected type of variable-length array count field {} in message {}",
                            count_path.join("."),
                            message.name()
                        )));
                    }
                },
                CountDescriptor::String(string_path) => {
                    writeln!(
                        self.output_file,
                        " + u32::from_be((*a).{}.len()) as ::vpp_plugin::bindings::uword",
                        string_path.join("."),
                    )?;
                }
            }
            writeln!(self.output_file, "    }}")?;
        } else {
            writeln!(
                self.output_file,
                "unsafe extern \"C\" fn {}_calc_size(_a: *mut {}) -> ::vpp_plugin::bindings::uword {{",
                message.name(),
                upper_camel_name
            )?;
            writeln!(
                self.output_file,
                "    ::std::mem::size_of::<{}>() as ::vpp_plugin::bindings::uword",
                upper_camel_name
            )?;
        }
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

    fn generate_alias(&mut self, alias: &Alias) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(&alias.field().name);

        let opt_derives = if alias.manual_print() {
            ""
        } else {
            "Debug, PartialEq, Default, "
        };
        writeln!(self.output_file, "#[derive({}Copy, Clone)]", opt_derives)?;
        writeln!(self.output_file, "#[repr(C, packed)]")?;
        if let Some(FieldSize::Fixed(length)) = alias.field().size {
            writeln!(
                self.output_file,
                "pub struct {}(pub[{}; {}]);",
                upper_camel_name,
                self.parser.to_rust_type(&alias.field().r#type)?,
                length
            )?;
        } else {
            writeln!(
                self.output_file,
                "pub struct {}(pub {});",
                upper_camel_name,
                self.parser.to_rust_type(&alias.field().r#type)?
            )?;
        }
        if !alias.manual_endian() {
            self.generate_endian_swap(&alias.field().name, EndianSwapInput::Alias(alias.field()))?;
        }

        Ok(())
    }

    fn generate_aliases(&mut self) -> Result<(), Error> {
        for message in self.parser.aliases() {
            self.generate_alias(message)?;
        }
        Ok(())
    }

    fn generate_enum(&mut self, e: &Enum) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(&e.name);

        // Since:
        // 1. We don't parse the memory and instead cast the message pointer due to VPP API
        //    restrictions (no length passed to message handler); and
        // 2. In Rust it's UB to construct an instance of an enum that doesn't match one of its
        //    variants;
        // then we cannot use generate an enum type here. Instead, the best that can be done to
        // help with type safety is to use a newtype wrapper around the primitive type.

        writeln!(
            self.output_file,
            "#[derive(Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]",
        )?;
        writeln!(self.output_file, "#[repr(C, packed)]")?;
        writeln!(
            self.output_file,
            "pub struct {}(pub {});",
            upper_camel_name, e.size,
        )?;

        writeln!(self.output_file)?;

        for variant in &e.variants {
            writeln!(
                self.output_file,
                "pub const {}: {} = {}({});",
                variant.id, upper_camel_name, upper_camel_name, variant.value,
            )?;
        }
        if !e.variants.is_empty() {
            writeln!(self.output_file)?;
        }

        writeln!(
            self.output_file,
            "impl ::std::fmt::Debug for {} {{",
            upper_camel_name
        )?;
        writeln!(
            self.output_file,
            "    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {{"
        )?;
        writeln!(self.output_file, "        match *self {{")?;
        for variant in &e.variants {
            writeln!(
                self.output_file,
                "            {} => f.write_str(\"{}\"),",
                variant.id, variant.id,
            )?;
        }
        writeln!(self.output_file, "            _ => {{")?;
        writeln!(self.output_file, "                let tmp = self.0;")?;
        writeln!(self.output_file, "                tmp.fmt(f)")?;
        writeln!(self.output_file, "            }}")?;
        writeln!(self.output_file, "        }}")?;
        writeln!(self.output_file, "    }}")?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        writeln!(
            self.output_file,
            "impl ::vpp_plugin::vlibapi::EndianSwap for {} {{",
            upper_camel_name
        )?;
        writeln!(
            self.output_file,
            "    unsafe fn endian_swap(&mut self, to_net: bool) {{",
        )?;
        // Suppress potential used variable warning
        writeln!(self.output_file, "        let _ = to_net;",)?;
        match e.size.as_str() {
            "u8" => {
                writeln!(self.output_file, "        // *self = Self(self.0) (no-op)",)?;
            }
            "u16" | "u32" | "u64" | "i16" | "i32" | "i64" => {
                writeln!(self.output_file, "        *self = Self(self.0.to_be());",)?;
            }
            _ => {
                return Err(Error::Unimplemented(format!(
                    "Unexpected size type {} for enum {}",
                    e.size, e.name
                )));
            }
        }
        writeln!(self.output_file, "    }}",)?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        Ok(())
    }

    fn generate_enumflag(&mut self, e: &Enum) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(&e.name);

        writeln!(
            self.output_file,
            "#[derive(Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]",
        )?;
        writeln!(self.output_file, "#[repr(C, packed)]")?;
        writeln!(
            self.output_file,
            "pub struct {}({});",
            upper_camel_name, e.size,
        )?;
        writeln!(self.output_file)?;
        writeln!(self.output_file, "::vpp_plugin::bitflags::bitflags! {{",)?;
        writeln!(
            self.output_file,
            "    impl {}: {} {{",
            upper_camel_name, e.size,
        )?;

        let upper_name = format!("{}_", e.name.to_uppercase());
        for variant in &e.variants {
            // For ease of use, strip off the enumflag name being used as part of each defined
            // flag, since this is redundant as the flags are already namespaced by the enumflag
            // type
            let id = variant
                .id
                .strip_prefix(&upper_name)
                .unwrap_or(variant.id.as_str());
            writeln!(
                self.output_file,
                "        const {} = {};",
                id, variant.value,
            )?;
        }
        writeln!(self.output_file, "    }}")?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        writeln!(
            self.output_file,
            "impl ::std::fmt::Debug for {} {{",
            upper_camel_name
        )?;
        writeln!(
            self.output_file,
            "    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {{"
        )?;
        writeln!(
            self.output_file,
            "        ::vpp_plugin::bitflags::parser::to_writer(self, f)"
        )?;
        writeln!(self.output_file, "    }}")?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        writeln!(
            self.output_file,
            "impl ::vpp_plugin::vlibapi::EndianSwap for {} {{",
            upper_camel_name
        )?;
        writeln!(
            self.output_file,
            "    unsafe fn endian_swap(&mut self, to_net: bool) {{",
        )?;
        // Suppress potential used variable warning
        writeln!(self.output_file, "        let _ = to_net;",)?;
        match e.size.as_str() {
            "u8" => {
                writeln!(
                    self.output_file,
                    "        // *self = Self::from_bits_retain(self.bits()) (no-op)",
                )?;
            }
            "u16" | "u32" | "u64" | "i16" | "i32" | "i64" => {
                writeln!(
                    self.output_file,
                    "        *self = Self::from_bits_retain(self.bits().to_be());",
                )?;
            }
            _ => {
                return Err(Error::Unimplemented(format!(
                    "Unexpected size type {} for enum {}",
                    e.size, e.name
                )));
            }
        }
        writeln!(self.output_file, "    }}",)?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        Ok(())
    }

    fn generate_enums(&mut self) -> Result<(), Error> {
        for e in self.parser.enums() {
            self.generate_enum(e)?;
        }
        for e in self.parser.enumflags() {
            self.generate_enumflag(e)?;
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
            self.generate_field(field)?;
        }
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;
        // TODO: enforce manual_endian for unions?

        // Note: no use of variable-length arrays is already enforced by the parser

        Ok(())
    }

    fn generate_unions(&mut self) -> Result<(), Error> {
        for un in self.parser.unions() {
            self.generate_union(un)?;
        }
        Ok(())
    }

    fn generate_endian_swap(&mut self, name: &str, input: EndianSwapInput) -> Result<(), Error> {
        writeln!(
            self.output_file,
            "impl ::vpp_plugin::vlibapi::EndianSwap for {} {{",
            to_upper_camel_case(name)
        )?;
        writeln!(
            self.output_file,
            "    unsafe fn endian_swap(&mut self, to_net: bool) {{",
        )?;
        // Suppress potential used variable warning
        writeln!(self.output_file, "        let _ = to_net;")?;
        // To avoid complicating the code generation further
        writeln!(self.output_file, "        #[allow(unused_unsafe)]")?;
        writeln!(self.output_file, "        unsafe {{")?;
        let fields = match input {
            EndianSwapInput::Fields(fields) => fields,
            EndianSwapInput::Alias(field) => std::slice::from_ref(field),
        };
        for field in fields {
            let field_name = match input {
                EndianSwapInput::Fields(_) => &field.name,
                EndianSwapInput::Alias(_) => "0",
            };

            let mut gen_count_variable = |count_field| {
                let count_field = fields
                    .iter()
                    .find(|field| &field.name == count_field)
                    .ok_or_else(|| {
                        Error::Unimplemented(format!(
                            "Unable to find variable count field {}",
                            count_field
                        ))
                    })?;
                let vla_elem_type = self.parser.to_rust_vla_elem_type(&field.r#type)?;
                writeln!(self.output_file, "            let count = if to_net {{",)?;
                writeln!(
                    self.output_file,
                    "                {}::from_be(self.{})",
                    count_field.r#type, count_field.name,
                )?;
                writeln!(self.output_file, "            }} else {{",)?;
                writeln!(
                    self.output_file,
                    "                self.{}",
                    count_field.name,
                )?;
                writeln!(self.output_file, "            }};",)?;
                writeln!(
                    self.output_file,
                    "            let array = ::std::slice::from_raw_parts_mut(std::ptr::addr_of_mut!(self.{}) as *mut {}, count as usize);",
                    field_name, vla_elem_type,
                )?;
                Ok::<_, Error>(())
            };

            match field.r#type.as_str() {
                "u8" | "bool" => {
                    writeln!(
                        self.output_file,
                        "            // self.{} = self.{} (no-op)",
                        field_name, field_name
                    )?;
                }
                "string" => {
                    writeln!(
                        self.output_file,
                        "            ::vpp_plugin::vlibapi::EndianSwap::endian_swap(&mut self.{}, to_net);",
                        field_name,
                    )?;
                }
                "u16" | "u32" | "u64" | "i16" | "i32" | "i64" => match &field.size {
                    Some(FieldSize::Fixed(size)) => {
                        writeln!(self.output_file, "            for i in 0..{} {{", size)?;
                        writeln!(
                            self.output_file,
                            "                self.{}[i] = self.{}[i].to_be();",
                            field_name, field_name
                        )?;
                        writeln!(self.output_file, "        }}",)?;
                    }
                    Some(FieldSize::Variable(Some(count_field))) => {
                        gen_count_variable(count_field)?;
                        writeln!(self.output_file, "            for elem in array {{")?;
                        writeln!(self.output_file, "                *elem = elem.to_be();",)?;
                        writeln!(self.output_file, "            }}")?;
                    }
                    Some(FieldSize::Variable(None)) => {
                        return Err(Error::Unimplemented(format!(
                            "variable length array field {} without count",
                            field_name
                        )));
                    }
                    None => {
                        writeln!(
                            self.output_file,
                            "            self.{} = self.{}.to_be();",
                            field_name, field.name
                        )?;
                    }
                },
                "f64" => {
                    writeln!(
                        self.output_file,
                        "            // self.{} = self.{} (no-op according to VPP API)",
                        field_name, field_name
                    )?;
                }
                _ => match &field.size {
                    Some(FieldSize::Fixed(size)) => {
                        writeln!(self.output_file, "            for i in 0..{} {{", size)?;
                        writeln!(
                            self.output_file,
                            "                ::vpp_plugin::vlibapi::EndianSwap::endian_swap(&mut self.{}[i], to_net);",
                            field_name
                        )?;
                        writeln!(self.output_file, "            }}",)?;
                    }
                    Some(FieldSize::Variable(Some(count_field))) => {
                        gen_count_variable(count_field)?;
                        writeln!(self.output_file, "            for elem in array {{")?;
                        writeln!(
                            self.output_file,
                            "                ::vpp_plugin::vlibapi::EndianSwap::endian_swap(elem, to_net);",
                        )?;
                        writeln!(self.output_file, "            }}",)?;
                    }
                    Some(FieldSize::Variable(None)) => {
                        return Err(Error::Unimplemented(format!(
                            "variable length array field {} without count",
                            field_name
                        )));
                    }
                    None => {
                        // Copy out the value to a temporary since the structs are packed and so it
                        // may not be properly aligned
                        writeln!(
                            self.output_file,
                            "            ::vpp_plugin::vlibapi::EndianSwap::endian_swap(&mut self.{}, to_net);",
                            field_name,
                        )?;
                    }
                },
            }
        }
        writeln!(self.output_file, "        }}",)?;
        writeln!(self.output_file, "    }}",)?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        Ok(())
    }

    fn generate_type(&mut self, t: &Type) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(t.name());

        let contains_string = matches!(t.vla(self.parser), Some((_, CountDescriptor::String(_))));
        let opt_derives = if t.manual_print() {
            ""
        } else if t.vla_non_recursive().is_some() || contains_string {
            "Default, "
        } else {
            "Debug, PartialEq, Default, "
        };
        self.generate_derives(opt_derives, contains_string)?;
        writeln!(self.output_file, "#[repr(C, packed)]")?;
        writeln!(self.output_file, "pub struct {} {{", upper_camel_name)?;
        for field in t.fields() {
            self.generate_field(field)?;
        }
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        if let Some((field, _)) = t.vla(self.parser)
            && let Some(FieldSize::Variable(Some(count_field))) = &field.size
        {
            writeln!(self.output_file, "impl {} {{", upper_camel_name)?;
            self.generate_vla_accessors(count_field, field)?;
            writeln!(self.output_file, "}}")?;
            writeln!(self.output_file)?;
        }

        // Manually implement fmt::Debug so that the zero-length (but actually variable-length)
        // field isn't printed to avoid misleading anyone looking at the output, or because the
        // type contains a string and so can't be copied, which the derived implementation
        // requires for packed types
        if t.vla_non_recursive().is_some() || (contains_string && !t.manual_print()) {
            self.generate_debug_trait(t.name(), t.fields(), contains_string)?;
        }

        if !t.manual_endian() {
            self.generate_endian_swap(t.name(), EndianSwapInput::Fields(t.fields()))?;
        }
        Ok(())
    }

    fn generate_types(&mut self) -> Result<(), Error> {
        for t in self.parser.types() {
            self.generate_type(t)?;
        }
        Ok(())
    }

    fn generate_register(&mut self) -> Result<(), Error> {
        // Avoid generating empty, unused traits/functions
        if self.parser.services().is_empty() {
            return Ok(());
        }

        writeln!(self.output_file, "pub trait Handlers {{")?;
        for service in self.parser.services() {
            let caller_upper_camel = to_upper_camel_case(service.caller());
            let caller_message = self.parser.message(service.caller());
            let reply_message = if service.reply() == "null" {
                None
            } else {
                self.parser.message(service.reply())
            };
            let caller_message_vla = caller_message
                .map(|message| message.vla(self.parser).is_some())
                .unwrap_or(false);
            let reply_message_vla = reply_message
                .map(|message| message.vla(self.parser).is_some())
                .unwrap_or(false);
            // If the caller message is a VLA, then it's the callers of the trait have a responsibility to ensure the memory for any VLA VLA is valid, consistent with the count field.
            // If the reply message is a VLA, then it's the trait implementation's responsibility to ensure the memory for any VLA in the reply is valid, consistent with the count field.
            let unsafe_str = if caller_message_vla || reply_message_vla {
                "unsafe "
            } else {
                ""
            };
            if service.reply() == "null" {
                writeln!(
                    self.output_file,
                    "    {}fn {}(vm: &::vpp_plugin::vlib::BarrierHeldMainRef, mp: &{});",
                    unsafe_str,
                    service.caller(),
                    caller_upper_camel
                )?;
            } else {
                let reply_message = format!(
                    "::vpp_plugin::vlibapi::Message<{}>",
                    to_upper_camel_case(service.reply())
                );
                let retval_in_reply_msg = self
                    .parser
                    .message(service.reply())
                    .map(|reply| reply.has_retval())
                    .unwrap_or_default();
                if let Some(stream_message) = service.stream_message() {
                    let stream_message = format!(
                        "::vpp_plugin::vlibapi::Stream<{}>",
                        to_upper_camel_case(stream_message),
                    );
                    if retval_in_reply_msg {
                        writeln!(
                            self.output_file,
                            "    {}fn {}(vm: &::vpp_plugin::vlib::BarrierHeldMainRef, mp: &{}, stream: {}) -> Result<{}, i32>;",
                            unsafe_str,
                            service.caller(),
                            caller_upper_camel,
                            stream_message,
                            reply_message
                        )?;
                    } else {
                        writeln!(
                            self.output_file,
                            "    {}fn {}(vm: &::vpp_plugin::vlib::BarrierHeldMainRef, mp: &{}, stream: {}) -> {};",
                            unsafe_str,
                            service.caller(),
                            caller_upper_camel,
                            stream_message,
                            reply_message
                        )?;
                    }
                } else if service.stream() {
                    writeln!(
                        self.output_file,
                        "    {}fn {}(vm: &::vpp_plugin::vlib::BarrierHeldMainRef, mp: &{}, stream: ::vpp_plugin::vlibapi::Stream<{}>);",
                        unsafe_str,
                        service.caller(),
                        caller_upper_camel,
                        to_upper_camel_case(service.reply()),
                    )?;
                } else if retval_in_reply_msg {
                    writeln!(
                        self.output_file,
                        "    {}fn {}(vm: &::vpp_plugin::vlib::BarrierHeldMainRef, mp: &{}) -> Result<{}, i32>;",
                        unsafe_str,
                        service.caller(),
                        caller_upper_camel,
                        reply_message
                    )?;
                } else {
                    writeln!(
                        self.output_file,
                        "    {}fn {}(vm: &::vpp_plugin::vlib::BarrierHeldMainRef, mp: &{}) -> {};",
                        unsafe_str,
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
            writeln!(self.output_file, "    unsafe {{")?;
            writeln!(
                self.output_file,
                "        let vm = ::vpp_plugin::vlib::BarrierHeldMainRef::from_ptr_mut("
            )?;
            writeln!(
                self.output_file,
                "            ::vpp_plugin::bindings::vlib_get_main_not_inline(),"
            )?;
            writeln!(self.output_file, "        );")?;
            writeln!(self.output_file, "        let mp = &*mp;")?;
            if service.reply() == "null" {
                writeln!(self.output_file, "        H::{}(vm, mp);", service.caller())?;
            } else {
                writeln!(
                    self.output_file,
                    "        ::vpp_plugin::vlibapi::registration_scope(|s| {{"
                )?;
                // TODO: check for client_index field in caller
                // TODO: check for context field in caller and reply
                writeln!(
                    self.output_file,
                    "            if let Some(reg) = s.from_client_index(vm, mp.client_index) {{"
                )?;
                let retval_in_reply_msg = self
                    .parser
                    .message(service.reply())
                    .map(|reply| reply.has_retval())
                    .unwrap_or_default();
                let stream_message_arg = if service.stream_message().is_some() {
                    ", ::vpp_plugin::vlibapi::Stream::new(reg)"
                } else {
                    ""
                };
                if service.stream() && service.stream_message().is_none() {
                    writeln!(
                        self.output_file,
                        "                H::{}(vm, mp, ::vpp_plugin::vlibapi::Stream::new(reg));",
                        service.caller()
                    )?;
                } else if retval_in_reply_msg {
                    writeln!(
                        self.output_file,
                        "                let mut reply = match H::{}(vm, mp{}) {{",
                        service.caller(),
                        stream_message_arg,
                    )?;
                    writeln!(self.output_file, "                    Ok(reply) => reply,")?;
                    writeln!(
                        self.output_file,
                        "                    Err(retval) => {} {{",
                        to_upper_camel_case(service.reply())
                    )?;
                    writeln!(self.output_file, "                        retval,")?;
                    writeln!(
                        self.output_file,
                        "                        ..Default::default()"
                    )?;
                    writeln!(self.output_file, "                    }}")?;
                    writeln!(self.output_file, "                    .into(),")?;
                    writeln!(self.output_file, "                }};")?;
                } else {
                    writeln!(
                        self.output_file,
                        "                let mut reply = H::{}(vm, mp{});",
                        service.caller(),
                        stream_message_arg,
                    )?;
                }
                if !service.stream() || service.stream_message().is_some() {
                    writeln!(
                        self.output_file,
                        "                reply.context = mp.context;",
                    )?;
                    writeln!(
                        self.output_file,
                        "                {}_endian(::std::ptr::addr_of_mut!(*reply), true);",
                        service.reply()
                    )?;
                    writeln!(self.output_file, "                reg.send_message(reply);")?;
                }
                writeln!(self.output_file, "            }}")?;
                writeln!(self.output_file, "        }})")?;
                writeln!(self.output_file, "    }}")?;
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
