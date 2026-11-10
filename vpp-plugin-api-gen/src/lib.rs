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
    collections::HashSet,
    env,
    fs::{DirBuilder, File},
    io::Write,
    path::{Path, PathBuf},
};

use thiserror::Error;

use crate::{
    json::generate_json,
    parser::{
        Alias, ApiParser, CountDescriptor, Enum, Field, FieldSize, Message, TaggedUnion, Type,
        TypeDetails, Union,
    },
    well_known::{WellKnownAlias, WellKnownType},
};

mod json;
mod parser;
mod well_known;

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
    /// A message used in a service is missing a field required by the generated handler
    #[error(
        "Message \"{message}\" used in service for \"{service}\" is missing required field \"u32 {field}\""
    )]
    ServiceMessageMissingField {
        /// Name of the service caller message
        service: String,
        /// Name of the message missing the field (either the caller or reply message)
        message: String,
        /// Name of the missing field
        field: &'static str,
    },
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

fn has_tagged_unions(fields: &[Field]) -> bool {
    fields.iter().any(|field| field.tagged_union().is_some())
}

/// Name of the Rust constant for an enumflag variant
///
/// For ease of use, the enumflag name is stripped off the front of each flag, since this is
/// redundant as the flags are already namespaced by the enumflag type.
fn enumflag_const_name<'a>(enum_name: &str, variant_id: &'a str) -> &'a str {
    variant_id
        .strip_prefix(&format!("{}_", enum_name.to_uppercase()))
        .unwrap_or(variant_id)
}

/// Rust path of the constant for a case of a tagged union, for use as a match pattern
fn case_path(tagged_union: &TaggedUnion, case: &str) -> String {
    // The constants are defined alongside the discriminator type, which may live in an imported
    // module
    let prefix = match tagged_union.r#enum.import_module() {
        Some(import_module) => format!("super::{}_api::", import_module),
        None => String::new(),
    };
    match tagged_union.r#enum.details {
        // Enumflag constants are associated constants of the type rather than being defined at
        // module scope. Note that if the path didn't resolve to a constant, the pattern would
        // instead be a catch-all binding.
        TypeDetails::EnumFlag { .. } => format!(
            "{}{}::{}",
            prefix,
            to_upper_camel_case(&tagged_union.enum_name),
            enumflag_const_name(&tagged_union.enum_name, case)
        ),
        _ => format!("{}{}", prefix, case),
    }
}

trait ApiParserRustExt {
    fn to_rust_type(&self, r#type: &str) -> Result<String, Error>;
    fn to_rust_vla_elem_type(&self, r#type: &str) -> Result<String, Error>;
}

impl ApiParserRustExt for ApiParser {
    fn to_rust_type(&self, r#type: &str) -> Result<String, Error> {
        Ok(
            if let Some((t, global_type)) = self.lookup_field_type(r#type)? {
                if let Some(import_module) = global_type.import_module() {
                    format!("super::{}_api::{}", import_module, to_upper_camel_case(t))
                } else {
                    to_upper_camel_case(t)
                }
            } else if r#type == "bool" {
                // A Rust bool must only ever hold 0 or 1, but nothing stops the sender of a
                // message from putting any other value in, so use u8 to avoid undefined
                // behaviour when such a message is received
                "u8".to_string()
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
///
/// If the API file imports other API files that live elsewhere, for example those installed by
/// VPP, then the directories to search for them can be specified using
/// [`include_dir`](Builder::include_dir):
///
/// ```no_run
/// use std::{env, path::PathBuf};
///
/// let output_dir = PathBuf::from(env::var("OUT_DIR").unwrap()).join("src");
/// vpp_plugin_api_gen::Builder::new("example.api", &output_dir.to_string_lossy())
///     .expect("unable to generate API binding")
///     .include_dir(".")
///     .include_dir("/usr/include")
///     .generate()
///     .expect("unable to generate API binding");
/// ```
#[derive(Debug)]
pub struct Builder {
    input_file: String,
    include_dirs: Vec<PathBuf>,

    module: String,
    output_file: PathBuf,
    output_json_file: PathBuf,
}

fn in_build_script() -> bool {
    env::var("OUT_DIR").is_ok() && env::var("CARGO_MANIFEST_DIR").is_ok()
}

impl Builder {
    /// Construct a new `Builder` for the given input file and outputting to the given directory
    ///
    /// Both a `<api-module>_api.rs` and a `<api-module>.api.json` file will be generated in
    /// the output directory.
    pub fn new(input_file: &str, output_dir: &str) -> Result<Self, Error> {
        if in_build_script() {
            println!("cargo:rerun-if-changed={}", input_file);
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

        Ok(Self {
            input_file: input_file.to_string(),
            include_dirs: Vec::new(),
            module,
            output_file,
            output_json_file,
        })
    }

    /// Add a directory to search for imported API files
    ///
    /// Directories are searched in the order they were added, and the first one containing the
    /// imported file is used. If no include directories are added, then imports are resolved
    /// relative to the current directory.
    ///
    /// When run from a build script, only the imported files that were actually used are
    /// reported to Cargo, so adding a file to an earlier include directory that would shadow
    /// one of them won't trigger regeneration until the build script reruns for another reason.
    pub fn include_dir(mut self, path: impl AsRef<Path>) -> Self {
        self.include_dirs.push(path.as_ref().to_path_buf());
        self
    }

    /// Generate Rust code for the API file
    ///
    /// Both a `<api-module>_api.rs` and a `<api-module>.api.json` file will be generated in
    /// the output directory.
    pub fn generate(self) -> Result<(), Error> {
        let parser = ApiParser::new(&self.input_file, &self.include_dirs)?;

        if in_build_script() {
            for import in parser.imported_files() {
                println!("cargo:rerun-if-changed={}", import.display());
            }
        }

        // Only create (and truncate) the output files once parsing has succeeded
        ApiGenContext {
            parser: &parser,
            module: self.module,
            output_file: File::create(&self.output_file)?,
            output_json_file: File::create(&self.output_json_file)?,
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

    fn generate_debug_trait(&mut self, name: &str, fields: &[Field]) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(name);

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
        // Copy fields out of the packed struct, since references can't be taken to them
        for field in fields {
            if !matches!(field.size, Some(FieldSize::Variable(_))) {
                writeln!(
                    self.output_file,
                    "        let tmp_{} = self.{};",
                    field.name, field.name
                )?;
            }
        }
        writeln!(
            self.output_file,
            "        let mut builder = f.debug_struct(\"{}\");",
            upper_camel_name
        )?;
        let mut has_vla = false;
        for field in fields {
            if matches!(field.size, Some(FieldSize::Variable(_))) {
                // Don't print the zero-length (but actually variable-length) field to avoid
                // misleading anyone looking at the output
                has_vla = true;
            } else if let Some(tagged_union) = field.tagged_union() {
                // Only print the arm selected by the discriminator
                writeln!(
                    self.output_file,
                    "        match tmp_{} {{",
                    tagged_union.discriminator
                )?;
                for arm in &tagged_union.arms {
                    writeln!(
                        self.output_file,
                        "            // SAFETY: the arm is selected by the discriminator, and any initialised bit pattern is valid for it"
                    )?;
                    writeln!(
                        self.output_file,
                        "            {} => builder.field(\"{}\", &unsafe {{ tmp_{}.{} }}),",
                        case_path(tagged_union, &arm.case),
                        field.name,
                        field.name,
                        arm.field.name
                    )?;
                }
                writeln!(
                    self.output_file,
                    "            #[allow(unreachable_patterns)]"
                )?;
                writeln!(
                    self.output_file,
                    "            _ => builder.field(\"{}\", &format_args!(\"<unknown>\")),",
                    field.name
                )?;
                writeln!(self.output_file, "        }};")?;
            } else {
                writeln!(
                    self.output_file,
                    "        builder.field(\"{}\", &tmp_{});",
                    field.name, field.name
                )?;
            }
        }
        if has_vla {
            writeln!(self.output_file, "        builder.finish_non_exhaustive()")?;
        } else {
            writeln!(self.output_file, "        builder.finish()")?;
        }
        writeln!(self.output_file, "    }}")?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        Ok(())
    }

    /// Generate a PartialEq implementation that, for tagged unions, compares only the arm
    /// selected by the discriminator
    fn generate_partial_eq_trait(&mut self, name: &str, fields: &[Field]) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(name);

        writeln!(
            self.output_file,
            "impl PartialEq for {} {{",
            upper_camel_name
        )?;
        writeln!(
            self.output_file,
            "    fn eq(&self, other: &Self) -> bool {{"
        )?;
        // Compare non-union fields first so that the discriminators are known to be equal
        // before the union arms are compared. Variable-length fields are zero-length so are
        // skipped. Fields are copied out of the packed struct using blocks, since references
        // can't be taken to them.
        let fixed_fields = || {
            fields
                .iter()
                .filter(|field| !matches!(field.size, Some(FieldSize::Variable(_))))
        };
        let mut first = true;
        for field in fixed_fields().filter(|field| field.tagged_union().is_none()) {
            writeln!(
                self.output_file,
                "        {}({{ self.{} }} == {{ other.{} }})",
                if first { "" } else { "&& " },
                field.name,
                field.name
            )?;
            first = false;
        }
        for (field, tagged_union) in
            fixed_fields().filter_map(|field| Some((field, field.tagged_union()?)))
        {
            writeln!(
                self.output_file,
                "        {}match self.{} {{",
                if first { "" } else { "&& " },
                tagged_union.discriminator
            )?;
            for arm in &tagged_union.arms {
                writeln!(
                    self.output_file,
                    "            // SAFETY: the arm is selected by the discriminator, and any initialised bit pattern is valid for it"
                )?;
                writeln!(
                    self.output_file,
                    "            {} => {{ let (a, b) = unsafe {{ (self.{}.{}, other.{}.{}) }}; a == b }},",
                    case_path(tagged_union, &arm.case),
                    field.name,
                    arm.field.name,
                    field.name,
                    arm.field.name
                )?;
            }
            // There's no way of knowing which bytes of the union are meaningful, so treat as
            // not equal
            writeln!(
                self.output_file,
                "            #[allow(unreachable_patterns)]"
            )?;
            writeln!(self.output_file, "            _ => false,")?;
            writeln!(self.output_file, "        }}")?;
            first = false;
        }
        if first {
            writeln!(self.output_file, "        true")?;
        }
        writeln!(self.output_file, "    }}")?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        Ok(())
    }

    /// Generate a Default implementation
    ///
    /// For messages, the message ID is filled in. Tagged unions are zero-initialised, so that
    /// all bytes are initialised whichever arm is later read, and then the arm selected by the
    /// default value of the discriminator (if any) is set to its default value.
    fn generate_default_trait(
        &mut self,
        name: &str,
        fields: &[Field],
        is_message: bool,
    ) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(name);
        let has_tagged_unions = has_tagged_unions(fields);

        writeln!(self.output_file, "impl Default for {} {{", upper_camel_name)?;
        writeln!(self.output_file, "    fn default() -> Self {{")?;
        if has_tagged_unions {
            writeln!(self.output_file, "        let mut value = Self {{")?;
        } else {
            writeln!(self.output_file, "        Self {{")?;
        }
        for field in fields {
            if is_message && field.name == "_vl_msg_id" {
                writeln!(self.output_file, "            _vl_msg_id: Self::msg_id(),")?;
            } else if field.tagged_union().is_some() {
                writeln!(
                    self.output_file,
                    "            // SAFETY: all-zero bytes are valid for a union"
                )?;
                writeln!(
                    self.output_file,
                    "            {}: unsafe {{ ::std::mem::zeroed() }},",
                    field.name,
                )?;
            } else {
                writeln!(
                    self.output_file,
                    "            {}: Default::default(),",
                    field.name,
                )?;
            }
        }
        if has_tagged_unions {
            writeln!(self.output_file, "        }};")?;
            for field in fields {
                let Some(tagged_union) = field.tagged_union() else {
                    continue;
                };
                writeln!(
                    self.output_file,
                    "        match value.{} {{",
                    tagged_union.discriminator
                )?;
                for arm in &tagged_union.arms {
                    writeln!(
                        self.output_file,
                        "            {} => value.{}.{} = Default::default(),",
                        case_path(tagged_union, &arm.case),
                        field.name,
                        arm.field.name
                    )?;
                }
                writeln!(
                    self.output_file,
                    "            #[allow(unreachable_patterns)]"
                )?;
                writeln!(self.output_file, "            _ => {{}}")?;
                writeln!(self.output_file, "        }}")?;
            }
            writeln!(self.output_file, "        value")?;
        } else {
            writeln!(self.output_file, "        }}")?;
        }
        writeln!(self.output_file, "    }}")?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

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
        let has_tagged_unions = has_tagged_unions(message.fields());
        // Debug is implemented manually for messages with a VLA to avoid printing the
        // zero-length field and for tagged unions to only print the selected arm
        let debug = !message.manual_print();
        let manual_debug = debug && (message.vla_non_recursive().is_some() || has_tagged_unions);
        let derive_debug = debug && !manual_debug;
        let partial_eq = !message.manual_print() && message.vla(self.parser).is_none();
        let manual_partial_eq = partial_eq && has_tagged_unions;
        let derive_partial_eq = partial_eq && !manual_partial_eq;
        writeln!(
            self.output_file,
            "#[derive({}{}Copy, Clone)]",
            if derive_debug { "Debug, " } else { "" },
            if derive_partial_eq { "PartialEq, " } else { "" },
        )?;
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

        self.generate_default_trait(message.name(), message.fields(), true)?;

        self.generate_endian_swap(message.name(), EndianSwapInput::Fields(message.fields()))?;

        if manual_debug {
            self.generate_debug_trait(message.name(), message.fields())?;
        }
        if manual_partial_eq {
            self.generate_partial_eq_trait(message.name(), message.fields())?;
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

        let well_known = WellKnownAlias::detect(&self.module, alias);
        let opt_derives = if well_known.is_some() {
            "PartialEq, Default, "
        } else if alias.manual_print() {
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

        if let Some(well_known) = well_known {
            self.generate_well_known_alias_impls(&alias.field().name, well_known)?;
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
        // If the enum has no gaps, this is unreachable. Not worth the effort of working this out,
        // so just suppress the warning.
        writeln!(
            self.output_file,
            "            #[allow(unreachable_patterns)]"
        )?;
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

        for variant in &e.variants {
            writeln!(
                self.output_file,
                "        const {} = {};",
                enumflag_const_name(&e.name, &variant.id),
                variant.value,
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

        // Snapshot each discriminator in host byte order before any field is swapped, so that
        // tagged union arms are selected correctly regardless of field order.
        let mut discriminators_seen = HashSet::new();
        for tagged_union in fields.iter().filter_map(Field::tagged_union) {
            let discriminator = &tagged_union.discriminator;
            if !discriminators_seen.insert(discriminator) {
                continue;
            }
            writeln!(
                self.output_file,
                "            let mut tmp_{} = self.{};",
                discriminator, discriminator
            )?;
            writeln!(self.output_file, "            if !to_net {{",)?;
            writeln!(
                self.output_file,
                "                ::vpp_plugin::vlibapi::EndianSwap::endian_swap(&mut tmp_{}, false);",
                discriminator,
            )?;
            writeln!(self.output_file, "            }}",)?;
        }

        for field in fields {
            let place = match input {
                EndianSwapInput::Fields(_) => format!("self.{}", field.name),
                EndianSwapInput::Alias(_) => "self.0".to_string(),
            };

            if let Some(tagged_union) = field.tagged_union() {
                writeln!(
                    self.output_file,
                    "            match tmp_{} {{",
                    tagged_union.discriminator
                )?;
                for arm in &tagged_union.arms {
                    writeln!(
                        self.output_file,
                        "                {} => {{",
                        case_path(tagged_union, &arm.case)
                    )?;
                    // Union arms can't be variable-length arrays, so there are no siblings needed
                    // to look up a count
                    self.generate_field_endian_swap(
                        &arm.field,
                        &format!("{}.{}", place, arm.field.name),
                        &[],
                        "                    ",
                    )?;
                    writeln!(self.output_file, "                }}")?;
                }
                // If the enum has no gaps, this is unreachable. Not worth the effort of working this out,
                // so just suppress the warning.
                writeln!(
                    self.output_file,
                    "                #[allow(unreachable_patterns)]"
                )?;
                writeln!(self.output_file, "                _ => {{}}")?;
                writeln!(self.output_file, "            }}")?;
            } else {
                self.generate_field_endian_swap(field, &place, fields, "            ")?;
            }
        }
        writeln!(self.output_file, "        }}",)?;
        writeln!(self.output_file, "    }}",)?;
        writeln!(self.output_file, "}}")?;
        writeln!(self.output_file)?;

        Ok(())
    }

    /// Generate code to endian swap a single field that isn't a tagged union
    ///
    /// `place` is the expression for the field, e.g. `self.foo`, and `fields` are the field's
    /// siblings, used to look up the count of a variable-length array. Each line of generated
    /// code is prefixed with `indent`.
    fn generate_field_endian_swap(
        &mut self,
        field: &Field,
        place: &str,
        fields: &[Field],
        indent: &str,
    ) -> Result<(), Error> {
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
            writeln!(self.output_file, "{indent}let count = if to_net {{")?;
            writeln!(
                self.output_file,
                "{indent}    {}::from_be(self.{})",
                count_field.r#type, count_field.name,
            )?;
            writeln!(self.output_file, "{indent}}} else {{")?;
            writeln!(self.output_file, "{indent}    self.{}", count_field.name)?;
            writeln!(self.output_file, "{indent}}};")?;
            writeln!(
                self.output_file,
                "{indent}let array = ::std::slice::from_raw_parts_mut(std::ptr::addr_of_mut!({place}) as *mut {vla_elem_type}, count as usize);",
            )?;
            Ok::<_, Error>(())
        };

        match field.r#type.as_str() {
            "u8" | "bool" => {
                writeln!(self.output_file, "{indent}// {place} = {place} (no-op)")?;
            }
            "string" => {
                writeln!(
                    self.output_file,
                    "{indent}::vpp_plugin::vlibapi::EndianSwap::endian_swap(&mut {place}, to_net);",
                )?;
            }
            // Primitives may not be aligned within packed types, so they must be swapped by value
            // rather than by taking a reference
            "u16" | "u32" | "u64" | "i16" | "i32" | "i64" => match &field.size {
                Some(FieldSize::Fixed(size)) => {
                    writeln!(self.output_file, "{indent}for i in 0..{size} {{")?;
                    writeln!(
                        self.output_file,
                        "{indent}    {place}[i] = {place}[i].to_be();"
                    )?;
                    writeln!(self.output_file, "{indent}}}")?;
                }
                Some(FieldSize::Variable(Some(count_field))) => {
                    gen_count_variable(count_field)?;
                    writeln!(self.output_file, "{indent}for elem in array {{")?;
                    writeln!(self.output_file, "{indent}    *elem = elem.to_be();")?;
                    writeln!(self.output_file, "{indent}}}")?;
                }
                Some(FieldSize::Variable(None)) => {
                    return Err(Error::Unimplemented(format!(
                        "variable length array field {} without count",
                        field.name
                    )));
                }
                None => {
                    writeln!(self.output_file, "{indent}{place} = {place}.to_be();")?;
                }
            },
            "f64" => {
                writeln!(
                    self.output_file,
                    "{indent}// {place} = {place} (no-op according to VPP API)"
                )?;
            }
            _ => match &field.size {
                Some(FieldSize::Fixed(size)) => {
                    writeln!(self.output_file, "{indent}for i in 0..{size} {{")?;
                    writeln!(
                        self.output_file,
                        "{indent}    ::vpp_plugin::vlibapi::EndianSwap::endian_swap(&mut {place}[i], to_net);",
                    )?;
                    writeln!(self.output_file, "{indent}}}")?;
                }
                Some(FieldSize::Variable(Some(count_field))) => {
                    gen_count_variable(count_field)?;
                    writeln!(self.output_file, "{indent}for elem in array {{")?;
                    writeln!(
                        self.output_file,
                        "{indent}    ::vpp_plugin::vlibapi::EndianSwap::endian_swap(elem, to_net);",
                    )?;
                    writeln!(self.output_file, "{indent}}}")?;
                }
                Some(FieldSize::Variable(None)) => {
                    return Err(Error::Unimplemented(format!(
                        "variable length array field {} without count",
                        field.name
                    )));
                }
                None => {
                    writeln!(
                        self.output_file,
                        "{indent}::vpp_plugin::vlibapi::EndianSwap::endian_swap(&mut {place}, to_net);",
                    )?;
                }
            },
        }

        Ok(())
    }

    fn generate_type(&mut self, t: &Type) -> Result<(), Error> {
        let upper_camel_name = to_upper_camel_case(t.name());

        let well_known = WellKnownType::detect(&self.module, t);
        let implied_fields = well_known::implied_fields(self.parser, &self.module, t);
        let fields = implied_fields.as_deref().unwrap_or(t.fields());
        let manual_print = t.manual_print() && well_known.is_none();

        let has_tagged_unions = has_tagged_unions(fields);
        // Debug is implemented manually for types with a VLA to avoid printing the zero-length
        // field, for tagged unions to only print the selected arm, and for well-known types
        let debug = !manual_print;
        let manual_debug =
            debug && (well_known.is_some() || t.vla_non_recursive().is_some() || has_tagged_unions);
        let derive_debug = debug && !manual_debug;
        let partial_eq = !manual_print && t.vla(self.parser).is_none();
        let manual_partial_eq = partial_eq && has_tagged_unions;
        let derive_partial_eq = partial_eq && !manual_partial_eq;
        let manual_default = !manual_print && has_tagged_unions;
        let derive_default = !manual_print && !manual_default;
        writeln!(
            self.output_file,
            "#[derive({}{}{}Copy, Clone)]",
            if derive_debug { "Debug, " } else { "" },
            if derive_partial_eq { "PartialEq, " } else { "" },
            if derive_default { "Default, " } else { "" },
        )?;
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

        if manual_debug {
            match well_known {
                Some(well_known) => self.generate_well_known_type_impls(t.name(), well_known)?,
                None => self.generate_debug_trait(t.name(), fields)?,
            }
        }
        if manual_partial_eq {
            self.generate_partial_eq_trait(t.name(), fields)?;
        }
        if manual_default {
            self.generate_default_trait(t.name(), fields, false)?;
        }

        if !t.manual_endian() {
            self.generate_endian_swap(t.name(), EndianSwapInput::Fields(fields))?;
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

    fn generate_counters(&mut self) -> Result<(), Error> {
        for counters in self.parser.counters() {
            writeln!(self.output_file, "#[derive(::vpp_plugin::ErrorCounters)]")?;
            writeln!(
                self.output_file,
                "pub enum {}ErrorCounter {{",
                to_upper_camel_case(counters.name())
            )?;
            for e in counters.elements() {
                let upper_camel_name = to_upper_camel_case(e.name());
                writeln!(
                    self.output_file,
                    "    #[error_counter(name = \"{}\", description = \"{}\", severity = {})]",
                    e.name(),
                    e.description(),
                    e.severity().to_uppercase()
                )?;
                writeln!(self.output_file, "    {},", upper_camel_name)?;
            }
            writeln!(self.output_file, "}}")?;
        }

        Ok(())
    }

    /// Check that service messages have the fields that the generated handlers rely on
    ///
    /// vppapigen doesn't enforce these, but generated handlers that reply need `client_index`
    /// in the caller to find the registration and, unless the reply is a stream of messages
    /// sent by the handler, `context` in both the caller and the reply.
    fn check_services(&self) -> Result<(), Error> {
        let require_field = |service: &str, message: &str, field: &'static str| {
            let has_field = self
                .parser
                .message(message)
                .is_some_and(|m| m.has_scalar_field(field, "u32"));
            if has_field {
                Ok(())
            } else {
                Err(Error::ServiceMessageMissingField {
                    service: service.to_string(),
                    message: message.to_string(),
                    field,
                })
            }
        };

        for service in self.parser.services() {
            if service.reply() == "null" {
                continue;
            }
            require_field(service.caller(), service.caller(), "client_index")?;
            if !service.stream() || service.stream_message().is_some() {
                require_field(service.caller(), service.caller(), "context")?;
                require_field(service.caller(), service.reply(), "context")?;
            }
        }

        Ok(())
    }

    fn generate(mut self) -> Result<(), Error> {
        self.check_services()?;
        self.output_json_file
            .write_all(generate_json(self.parser)?.as_bytes())?;
        self.generate_aliases()?;
        self.generate_enums()?;
        self.generate_unions()?;
        self.generate_types()?;
        self.generate_messages()?;
        self.generate_register()?;
        self.generate_counters()?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Generate code for an API file in the tests directory
    fn generate_file(filename: &str) -> Result<(), Error> {
        let output_dir = env::temp_dir().join(format!(
            "vpp-plugin-api-gen-test-{}-{}",
            std::process::id(),
            Path::new(filename).file_stem().unwrap().to_string_lossy()
        ));
        let result = Builder::new(
            &format!("{}/{}", env!("CARGO_MANIFEST_DIR"), filename),
            &output_dir.to_string_lossy(),
        )
        .and_then(Builder::generate);
        let _ = std::fs::remove_dir_all(&output_dir);
        result
    }

    /// Generate code for an API file and expect an error, returning that error
    fn generate_file_expect_error(filename: &str) -> Error {
        match generate_file(filename) {
            Ok(()) => panic!("unexpectedly generated {} successfully", filename),
            Err(e) => e,
        }
    }

    #[test]
    fn service_fields() {
        generate_file("tests/service_fields.api").unwrap();
    }

    #[test]
    fn service_caller_missing_client_index() {
        assert_eq!(
            generate_file_expect_error("tests/service_caller_missing_client_index.api").to_string(),
            "Message \"create\" used in service for \"create\" is missing required field \"u32 client_index\""
        );
    }

    #[test]
    fn service_caller_missing_context() {
        assert_eq!(
            generate_file_expect_error("tests/service_caller_missing_context.api").to_string(),
            "Message \"create\" used in service for \"create\" is missing required field \"u32 context\""
        );
    }

    #[test]
    fn service_reply_missing_context() {
        assert_eq!(
            generate_file_expect_error("tests/service_reply_missing_context.api").to_string(),
            "Message \"create_reply\" used in service for \"create\" is missing required field \"u32 context\""
        );
    }

    #[test]
    fn enumflag_const_name_strips_prefix() {
        assert_eq!(enumflag_const_name("tu_flags", "TU_FLAGS_A"), "A");
        // Variants not prefixed with the enumflag name are left as is
        assert_eq!(enumflag_const_name("tu_flags", "OTHER_A"), "OTHER_A");
    }
}
