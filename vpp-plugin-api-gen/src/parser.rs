use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::read_to_string,
    sync::Arc,
};

use indexmap::IndexMap;
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum Statement {
    Define {
        name: String,
        options: Vec<OptionStatement>,
        fields: Vec<Field>,
        flags: Vec<Flag>,
        comment: Option<String>,
    },
    Option(OptionStatement),
    TypedefBlock {
        name: String,
        options: Vec<OptionStatement>,
        fields: Vec<Field>,
        flags: Vec<Flag>,
    },
    Typedef {
        field: Field,
        _flags: Vec<Flag>,
    },
    Enum {
        name: String,
        size: String,
        block_statements: Vec<EnumStatement>,
    },
    EnumFlag {
        name: String,
        size: String,
        block_statements: Vec<EnumStatement>,
    },
    Union {
        name: String,
        options: Vec<OptionStatement>,
        fields: Vec<Field>,
        comment: Option<String>,
    },
    Import(String),
    Service(Vec<Service>),
}

fn options_from_block_statements(block_statements: &[BlockStatement]) -> Vec<OptionStatement> {
    block_statements
        .iter()
        .filter_map(|stmt| match stmt {
            BlockStatement::Option(o) => Some(o),
            _ => None,
        })
        .cloned()
        .collect()
}

fn fields_from_block_statements(block_statements: Vec<BlockStatement>) -> Vec<Field> {
    block_statements
        .into_iter()
        .filter_map(|stmt| match stmt {
            BlockStatement::Declaration(field) => Some(field),
            _ => None,
        })
        .collect()
}

impl Statement {
    fn define(
        name: String,
        block_statements: Vec<BlockStatement>,
        flags: Vec<Flag>,
        comment: Option<String>,
    ) -> Self {
        Self::Define {
            name,
            options: options_from_block_statements(&block_statements),
            fields: fields_from_block_statements(block_statements),
            flags,
            comment,
        }
    }

    fn typedef(field: Field, flags: Vec<Flag>) -> Self {
        Self::Typedef {
            field,
            _flags: flags,
        }
    }

    fn typedef_block(
        name: String,
        block_statements: Vec<BlockStatement>,
        flags: Vec<Flag>,
    ) -> Self {
        Self::TypedefBlock {
            name,
            options: options_from_block_statements(&block_statements),
            fields: fields_from_block_statements(block_statements),
            flags,
        }
    }

    fn union(name: String, block_statements: Vec<BlockStatement>, comment: Option<String>) -> Self {
        Self::Union {
            name,
            options: options_from_block_statements(&block_statements),
            fields: fields_from_block_statements(block_statements),
            comment,
        }
    }
}

fn crc_data_for_fields(fields: &[Field]) -> String {
    let crc_data_vec: Vec<_> = fields.iter().map(|field| field.crc_data()).collect();
    format!("[{}]", crc_data_vec.join(", "))
}

fn crc_data_for_enum_block_statements(block_statements: &[EnumStatement]) -> String {
    let mut next_value = 0;
    let crc_data_vec: Vec<_> = block_statements
        .iter()
        .filter_map(|s| {
            match s {
                EnumStatement::Variant {
                    id,
                    value: explicit_value,
                    options,
                } => {
                    let value = if let Some(value) = explicit_value {
                        *value
                    } else {
                        next_value
                    };
                    next_value = value + 1;
                    // Variants marked as backwards_compatible aren't part of the CRC calculation
                    if options.iter().any(|o| o.option == "backwards_compatible") {
                        None
                    } else {
                        Some(format!("['{id}', {value}]"))
                    }
                }
            }
        })
        .collect();
    format!("[{}]", crc_data_vec.join(", "))
}

impl Statement {
    fn crc_data(&self) -> Option<String> {
        match self {
            Self::Define { fields, .. } => Some(crc_data_for_fields(fields)),
            Self::Option(option_statement) => Some(option_statement.option.clone()),
            Self::TypedefBlock { fields, .. } => Some(crc_data_for_fields(fields)),
            Self::Typedef { .. } => {
                // As per vppapigen.py
                Some("[]".to_string())
            }
            Self::Union { fields, .. } => Some(crc_data_for_fields(fields)),
            Self::Enum {
                block_statements, ..
            } => Some(crc_data_for_enum_block_statements(block_statements)),
            Self::EnumFlag {
                block_statements, ..
            } => Some(crc_data_for_enum_block_statements(block_statements)),
            Self::Import(_) | Self::Service(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OptionStatement {
    option: String,
    value: Option<String>,
}

impl OptionStatement {
    pub fn option(&self) -> &str {
        &self.option
    }

    pub fn value(&self) -> Option<&str> {
        self.value.as_deref()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FieldSize {
    Variable(Option<String>),
    Fixed(u32),
}

#[derive(Debug, Clone)]
pub struct Field {
    pub(crate) r#type: String,
    pub(crate) name: String,
    pub(crate) options: Vec<OptionStatement>,
    pub(crate) size: Option<FieldSize>,
}

impl Field {
    fn vla(&self) -> bool {
        matches!(self.size, Some(FieldSize::Variable(_)))
    }
}

#[derive(Debug, Clone)]
pub enum BlockStatement {
    Declaration(Field),
    Option(OptionStatement),
}

#[derive(Debug, Clone)]
pub enum EnumStatement {
    Variant {
        id: String,
        value: Option<u32>,
        options: Vec<OptionStatement>,
    },
}

#[derive(Debug, Clone)]
pub struct Service {
    caller: String,
    reply: String,
    events: Vec<String>,
    stream: bool,
    stream_message: Option<String>,
}

impl Service {
    pub fn caller(&self) -> &str {
        &self.caller
    }

    pub fn reply(&self) -> &str {
        &self.reply
    }

    pub fn stream(&self) -> bool {
        self.stream
    }

    pub fn events(&self) -> &[String] {
        &self.events
    }

    pub fn stream_message(&self) -> Option<&str> {
        self.stream_message.as_deref()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Flag {
    ManualPrint,
    ManualEndian,
    DontTrace,
    AutoEndian,
    AutoReply,
    IndexReply,
}

peg::parser! {
    grammar api_parser() for str {
        pub rule statement_list() -> Vec<Statement>
            = s:(statement()*) _ { s }

        rule statement() -> Statement
            = define()
            / typedef()
            / o:option() { Statement::Option(o) }
            / import()
            / enum_rule()
            / enumflag()
            / union()
            / service()
            // / paths()
            // / counters()

        rule define() -> Statement
            = c:comment() ___ "define" __ name:identifier() _ "{" b:block_statements() _ "}" _ ";" { Statement::define(name, b, vec![], Some(c)) }
            / c:comment() f:flag_list() _ "define" __ name:identifier() _ "{" b:block_statements() _ "}" _ ";" { Statement::define(name, b, f, Some(c)) }
            / _ "define" __ name:identifier() _ "{" b:block_statements() _ "}" _ ";" { Statement::define(name, b, vec![], None) }
            / f:flag_list() _ "define" __ name:identifier() _ "{" b:block_statements() _ "}" _ ";" { Statement::define(name, b, f, None) }

        // Whitespace (including comments)
        rule __ = ([' ' | '\t' | '\n' | '\r'] / comment())+

        // Optional whitespace
        rule _ = quiet!{([' ' | '\t' | '\n' | '\r'] / comment())*}

        // Optional whitespace, no comment
        rule ___ = [' ' | '\t' | '\n' | '\r']*

        rule identifier() -> String
            = id:$(quiet!{['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*}) { id.to_string() }
            / expected!("identifier")

        rule block_statements() -> Vec<BlockStatement>
            = s:(block_statement()+) { s }

        rule block_statement() -> BlockStatement
            = o:option() { BlockStatement::Option(o) }
            / d:declaration() { BlockStatement::Declaration(d) }

        rule declaration() -> Field
            = _ t:identifier() __ n:identifier() _ ";" { Field { r#type: t, name: n, options: vec![], size: None } }
            / _ t:identifier() __ n:identifier() _ "[" _ o:field_options() _ "]" _ ";" { Field { r#type: t, name: n, options: o, size: None } }
            / _ t:identifier() __ n:identifier() _ "[" _ "]" _ ";" { Field { r#type: t, name: n, options: vec![], size: Some(FieldSize::Variable(None)) } }
            / _ t:identifier() __ name:identifier() _ "[" _ n:number() _ "]" _ ";" { Field { r#type: t, name, options: vec![], size: Some(FieldSize::Fixed(n)) } }
            / _ t:identifier() __ n:identifier() _ "[" _ vla:identifier() _ "]" _ ";" { Field { r#type: t, name: n, options: vec![], size: Some(FieldSize::Variable(Some(vla))) } }

        rule number() -> u32
            = "0x" n:$((['0'..='9' | 'a'..='f' | 'A'..='F'])+) {? u32::from_str_radix(n, 16).or(Err("u32")) }
            / n:$(['0'..='9']+) {? n.parse().or(Err("u32")) }

        rule option() -> OptionStatement
            = _ "option" __ o:identifier() _ ";" { OptionStatement { option: o, value: None } }
            / _ "option" __ o:identifier() _ "=" _ v:option_value() _ ";" { OptionStatement { option: o, value: Some(v) } }

        rule option_value() -> String
            = "true" { "true".to_string() }
            / "false" { "false".to_string() }
            / string_literal()

        rule string_literal() -> String
            = "\"" s:$("\\\"" / [^'"'])* "\"" { s.into_iter().collect() }

        rule field_options() -> Vec<OptionStatement>
            = field_option() ++ ","

        rule field_option() -> OptionStatement
            = _ o:identifier() _ "=" _ v:option_value() { OptionStatement { option: o, value: Some(v) } }

        rule comment() -> String
            = ___ c:$(quiet!{"/*"} (!"*/"[_])* "*/") { c.to_string() }
            / ___ quiet!{"//"} c:$([^'\n'])* { format!("/*{} */", c.into_iter().collect::<String>()) }

        rule typedef() -> Statement
            = f:flag_list() _ "typedef" __ name:identifier() _ "{" b:block_statements() _ "}" _ ";" { Statement::typedef_block(name, b, f) }
            / _ "typedef" __ name:identifier() _ "{" b:block_statements() _ "}" _ ";" { Statement::typedef_block(name, b, vec![]) }
            / f:flag_list() _ "typedef" __ d:declaration() { Statement::typedef(d, f) }
            / _ "typedef" __ d:declaration() { Statement::typedef(d, vec![]) }

        rule import() -> Statement
            = _ "import" _ i:string_literal() _ ";" { Statement::Import(i) }

        rule enum_rule() -> Statement
            = _ "enum" __ name:identifier() _ "{" b:enum_statements() _ "}" _ ";" { Statement::Enum { name, size: "u32".to_string(), block_statements: b }}
            / _ "enum" __ name:identifier() _ ":" _ size:enum_size() _ "{" b:enum_statements() _ "}" _ ";" { Statement::Enum { name, size, block_statements: b }}

        rule enum_size() -> String
            = s:$("u8" / "u16" / "u32" / "i8" / "i16" / "i32") { s.to_string() }

        rule enum_statements() -> Vec<EnumStatement>
            = s:(enum_statement()+) { s }

        rule enum_statement() -> EnumStatement
            = _ id:identifier() _ "," { EnumStatement::Variant { id, value: None, options: vec![] } }
            / _ id:identifier() _ "=" _ v:number() _ "," { EnumStatement::Variant { id, value: Some(v), options: vec![] } }
            / _ id:identifier() _ "[" o:field_options() _ "]" _ "," { EnumStatement::Variant { id, value: None, options: o } }
            / _ id:identifier() _ "=" _ v:number() _ "[" o:field_options() _ "]" _ "," { EnumStatement::Variant { id, value: Some(v), options: o } }

        rule enumflag() -> Statement
            = _ "enumflag" __ name:identifier() _ "{" b:enum_statements() _ "}" _ ";" { Statement::EnumFlag { name, size: "u32".to_string(), block_statements: b } }
            / _ "enumflag" __ name:identifier() _ ":" _ size:enumflag_size() _ "{" b:enum_statements() _ "}" _ ";" { Statement::EnumFlag { name, size, block_statements: b } }

        rule enumflag_size() -> String
            = s:$("u8" / "u16" / "u32") { s.to_string() }

        rule union() -> Statement
            = c:comment() ___ "union" __ name:identifier() _ "{" b:block_statements() _ "}" _ ";" { Statement::union(name, b, Some(c)) }
            / _ "union" __ name:identifier() _ "{" b:block_statements() _ "}" _ ";" { Statement::union(name, b, None) }

        rule service() -> Statement
            = _ "service" _ "{" s:service_statements() _ "}" _ ";" { Statement::Service(s) }

        rule service_statements() -> Vec<Service>
            = s:(service_statement()+) { s }

        rule service_statement() -> Service
            = _ "rpc" _ caller:identifier() _ "returns" _ reply:identifier() _ ";" { Service { caller, reply, events: vec![], stream: false, stream_message: None } }
            / _ "rpc" _ caller:identifier() _ "returns" _ "stream" _ reply:identifier() _ ";" { Service { caller, reply, events: vec![], stream: true, stream_message: None } }
            / _ "rpc" _ caller:identifier() _ "returns" _ reply:identifier() _ "stream" _ stream:identifier() _ ";" { Service { caller, reply, events: vec![], stream: true, stream_message: Some(stream) } }
            / _ "rpc" _ caller:identifier() _ "returns" _ reply:identifier() _ "events" _ events:event_list() _ ";" { Service { caller, reply, events, stream: false, stream_message: None } }

        rule event_list() -> Vec<String>
            = event()+

        rule event() -> String
            = _ id:identifier() { id }

        rule flag_list() -> Vec<Flag>
            = s:(flag()+) { s }

        rule flag() -> Flag
            = _ "manual_print" { Flag::ManualPrint }
            / _ "manual_endian" { Flag::ManualEndian }
            / _ "dont_trace" { Flag::DontTrace }
            / _ "autoendian" { Flag::AutoEndian }
            / _ "autoreply" { Flag::AutoReply }
            / _ "indexreply" { Flag::IndexReply }
    }
}

pub(crate) const VL_API_PREFIX: &str = "vl_api_";
pub(crate) const VL_API_SUFFIX: &str = "_t";

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum Error {
    #[error("I/O error for {0}")]
    Io(String, #[source] std::io::Error),
    #[error("Parse error in {0}: {1}")]
    Parser(String, peg::error::ParseError<peg::str::LineCol>),
    #[error("Unable to resolve type {type_name}")]
    TypeResolution { type_name: String },
    #[error("Redefinition of type {type_name}")]
    TypeRedefinition { type_name: String },
    #[error("{message} is missing reply message ({reply_message}) or service definition")]
    MissingReplyMessageForMessage {
        message: String,
        reply_message: String,
    },
    #[error("Service definition refers to undefined message {0}")]
    ServiceRefersUndefinedMessage(String),
    #[error("Service definition refers to undefined message {0} in reply")]
    ServiceRefersUndefinedMessageReply(String),
    #[error("Service definition refers to undefined event {0} in message {1}")]
    ServiceRefersUndefinedEvent(String, String),
    #[error("Service caller message {0} also used as a reply in a service")]
    ServiceCallerUsedAsReply(String),
    #[error("Missing caller message {0} for reply {1}")]
    MissingCallerMessageForReply(String, String),
    #[error("An explicit service definition is require for {0} due to the presence of {1}")]
    ExplicitGetServiceRequiredDetails(String, String),
    #[error("Missing details message {0} for dump message {1}")]
    MissingDetailsMessageForDump(String, String),
    #[error("Missing {0} or {1} message for details message {2}")]
    MissingDumpGetForDetailsMessage(String, String, String),
    #[error("Variable length array field \"{0}\" must be the last field in type \"{1}\"")]
    VlaFieldNotLast(String, String),
    #[error("Variable length array field \"{0}\" is present in union \"{1}\" which isn't allowed")]
    VlaFieldUnion(String, String),
    #[error("Array variable \"{0}\" in field \"{1}\" not found")]
    ArrayVariableNotFound(String, String),
    #[error("String field \"{0}\" must be declared as an array")]
    StringNotArray(String),
}

fn parse_file(filename: &str) -> Result<Vec<Statement>, Error> {
    let file = File::open(filename).map_err(|e| Error::Io(filename.to_string(), e))?;
    let input = read_to_string(file).map_err(|e| Error::Io(filename.to_string(), e))?;
    api_parser::statement_list(&input).map_err(|e| Error::Parser(filename.to_string(), e))
}

impl Field {
    pub fn crc_data(&self) -> String {
        if let Some(size) = &self.size {
            let (length, lengthfield) = match size {
                FieldSize::Fixed(length) => (*length, "None".to_string()),
                FieldSize::Variable(None) => (0, "None".to_string()),
                FieldSize::Variable(Some(lengthfield)) => (0, format!("'{lengthfield}'")),
            };
            format!(
                "['{}', '{}', {}, {}]",
                self.r#type, self.name, length, lengthfield
            )
        } else {
            format!("['{}', '{}']", self.r#type, self.name)
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub id: String,
    pub value: u32,
}

#[derive(Debug, Clone)]
pub enum TypeDetails {
    TypedefBlock {
        fields: Vec<Field>,
    },
    Typedef {
        r#type: String,
        size: Option<FieldSize>,
    },
    Union {
        fields: Vec<Field>,
    },
    Enum {
        size: String,
        variants: Vec<EnumVariant>,
    },
    EnumFlag {
        size: String,
        variants: Vec<EnumVariant>,
    },
}

#[derive(Debug, Clone)]
pub struct TypeEntry {
    crc: crc32fast::Hasher,
    vla: bool,
    pub details: TypeDetails,
}

#[derive(Debug, Clone)]
pub struct Message {
    name: String,
    fields: Vec<Field>,
    options: Vec<OptionStatement>,
    flags: Vec<Flag>,
    crc: u32,
    comment: Option<String>,
}

impl Message {
    pub fn auto_reply(&self) -> bool {
        self.flags.contains(&Flag::AutoReply)
    }

    pub fn index_reply(&self) -> bool {
        self.flags.contains(&Flag::IndexReply)
    }

    pub fn auto_endian(&self) -> bool {
        self.flags.contains(&Flag::AutoEndian)
    }

    pub fn manual_print(&self) -> bool {
        self.flags.contains(&Flag::ManualPrint)
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn crc(&self) -> u32 {
        self.crc
    }

    pub fn fields(&self) -> &[Field] {
        &self.fields
    }

    pub fn has_retval(&self) -> bool {
        self.fields
            .iter()
            .any(|field| field.name == "retval" && field.r#type == "i32" && field.size.is_none())
    }

    pub fn comment(&self) -> Option<&str> {
        self.comment.as_deref()
    }

    pub fn options(&self) -> &[OptionStatement] {
        &self.options
    }
}

#[derive(Debug, Clone)]
pub struct Union {
    name: String,
    fields: Vec<Field>,
    _options: Vec<OptionStatement>,
    comment: Option<String>,
}

impl Union {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn fields(&self) -> &[Field] {
        &self.fields
    }

    pub fn comment(&self) -> Option<&str> {
        self.comment.as_deref()
    }

    #[cfg(test)]
    pub fn options(&self) -> &[OptionStatement] {
        &self._options
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    name: String,
    fields: Vec<Field>,
    _options: Vec<OptionStatement>,
    flags: Vec<Flag>,
}

impl Type {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn fields(&self) -> &[Field] {
        &self.fields
    }

    #[cfg(test)]
    pub fn options(&self) -> &[OptionStatement] {
        &self._options
    }

    pub fn manual_print(&self) -> bool {
        self.flags.contains(&Flag::ManualPrint)
    }

    pub fn manual_endian(&self) -> bool {
        self.flags.contains(&Flag::ManualPrint)
    }
}

#[derive(Debug)]
pub struct Enum {
    name: String,
    _size: String,
    _block_statements: Vec<EnumStatement>,
}

impl Enum {
    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub struct ApiParser {
    global_types_by_name: IndexMap<String, Arc<TypeEntry>>,

    options: Vec<OptionStatement>,
    messages: Vec<Message>,
    services: Vec<Service>,
    imports: Vec<String>,
    aliases: Vec<Field>,
    unions: Vec<Union>,
    types: Vec<Type>,
    enums: Vec<Enum>,
    enumflags: Vec<Enum>,

    file_crc: u32,
}

impl ApiParser {
    pub fn new(filename: &str) -> Result<Self, Error> {
        let statements = parse_file(filename)?;

        let mut me = Self {
            global_types_by_name: Default::default(),
            options: Default::default(),
            messages: Default::default(),
            services: Default::default(),
            imports: Default::default(),
            aliases: Default::default(),
            unions: Default::default(),
            types: Default::default(),
            enums: Default::default(),
            enumflags: Default::default(),
            file_crc: 0,
        };

        me.do_imports(&statements)?;
        // Side effect: register types
        me.type_entries_from_statements(&statements)?;

        me.process(statements)?;

        Ok(me)
    }

    fn do_imports(&mut self, statements: &[Statement]) -> Result<(), Error> {
        for stmt in statements {
            let Statement::Import(import_filename) = stmt else {
                continue;
            };

            let imported_statements = parse_file(import_filename)?;
            self.do_imports(&imported_statements)?;

            self.type_entries_from_statements(&imported_statements)?;
        }

        Ok(())
    }

    fn crc_for_fields(&self, fields: &[Field], debug: bool) -> Result<crc32fast::Hasher, Error> {
        let mut crc_hash = crc32fast::Hasher::new();
        let crc_data = crc_data_for_fields(fields);
        if debug {
            println!("crc_data: {}", crc_data);
        }
        crc_hash.update(crc_data.as_bytes());

        for field in fields {
            if let Some(type_name) = field.r#type.strip_prefix(VL_API_PREFIX)
                && let Some(type_name) = type_name.strip_suffix(VL_API_SUFFIX)
            {
                if let Some(type_entry) = self.global_types_by_name.get(type_name) {
                    crc_hash.combine(&type_entry.crc);
                } else {
                    return Err(Error::TypeResolution {
                        type_name: type_name.to_string(),
                    });
                }
            }
        }

        Ok(crc_hash)
    }

    fn validate_vla(&self, name: &str, fields: &[Field], is_union: bool) -> Result<bool, Error> {
        let mut vla = false;

        for (i, field) in fields.iter().enumerate() {
            if field.r#type == "string" && field.size.is_none() {
                return Err(Error::StringNotArray(field.name.clone()));
            }

            vla = field.vla();
            if let Some(type_name) = field.r#type.strip_prefix(VL_API_PREFIX)
                && let Some(type_name) = type_name.strip_suffix(VL_API_SUFFIX)
            {
                if let Some(type_entry) = self.global_types_by_name.get(type_name) {
                    vla = type_entry.vla;
                } else {
                    return Err(Error::TypeResolution {
                        type_name: type_name.to_string(),
                    });
                }
            }
            if vla && is_union {
                return Err(Error::VlaFieldUnion(field.name.clone(), name.to_string()));
            }
            if vla && i != fields.len() - 1 {
                return Err(Error::VlaFieldNotLast(field.name.clone(), name.to_string()));
            }

            if let Some(FieldSize::Variable(Some(length_var))) = &field.size
                && !fields.iter().any(|field| &field.name == length_var)
            {
                return Err(Error::ArrayVariableNotFound(
                    field.name.clone(),
                    length_var.clone(),
                ));
            }
        }

        Ok(vla)
    }

    fn type_entries_from_statements(
        &mut self,
        statements: &[Statement],
    ) -> Result<Vec<(String, Arc<TypeEntry>)>, Error> {
        let mut types = vec![];
        for stmt in statements {
            let (name, t) = match stmt {
                Statement::Define { name, fields, .. } => {
                    self.validate_vla(name, fields, false)?;
                    continue;
                }
                Statement::TypedefBlock { name, fields, .. } => {
                    let vla = self.validate_vla(name, fields, false)?;
                    let crc = self.crc_for_fields(fields, false)?;
                    (
                        name.clone(),
                        TypeEntry {
                            crc,
                            vla,
                            details: TypeDetails::TypedefBlock {
                                fields: fields.clone(),
                            },
                        },
                    )
                }
                Statement::Typedef { field, .. } => {
                    let mut crc = crc32fast::Hasher::new();
                    if let Some(FieldSize::Variable(Some(length_var))) = &field.size {
                        return Err(Error::ArrayVariableNotFound(
                            field.name.clone(),
                            length_var.clone(),
                        ));
                    }
                    // As per vppapigen.py
                    crc.update("[]".as_bytes());
                    (
                        field.name.clone(),
                        TypeEntry {
                            crc,
                            vla: field.vla(),
                            details: TypeDetails::Typedef {
                                r#type: field.r#type.clone(),
                                size: field.size.clone(),
                            },
                        },
                    )
                }
                Statement::Union { name, fields, .. } => {
                    self.validate_vla(name, fields, true)?;
                    let crc = self.crc_for_fields(fields, false)?;
                    (
                        name.clone(),
                        TypeEntry {
                            crc,
                            // validate_vla ensures this
                            vla: false,
                            details: TypeDetails::Union {
                                fields: fields.clone(),
                            },
                        },
                    )
                }
                Statement::Enum {
                    name,
                    size,
                    block_statements,
                }
                | Statement::EnumFlag {
                    name,
                    size,
                    block_statements,
                } => {
                    let mut crc = crc32fast::Hasher::new();
                    crc.update(crc_data_for_enum_block_statements(block_statements).as_bytes());
                    let mut next_value = 0;
                    let variants: Vec<_> = block_statements
                        .iter()
                        .map(|s| match s {
                            EnumStatement::Variant {
                                id,
                                value: explicit_value,
                                ..
                            } => {
                                let value = if let Some(value) = explicit_value {
                                    *value
                                } else {
                                    next_value
                                };
                                next_value = value + 1;
                                EnumVariant {
                                    id: id.clone(),
                                    value,
                                }
                            }
                        })
                        .collect();
                    let details = if matches!(stmt, Statement::Enum { .. }) {
                        TypeDetails::Enum {
                            size: size.clone(),
                            variants,
                        }
                    } else {
                        TypeDetails::EnumFlag {
                            size: size.clone(),
                            variants,
                        }
                    };
                    (
                        name.clone(),
                        TypeEntry {
                            crc,
                            vla: false,
                            details,
                        },
                    )
                }
                Statement::Option(_) | Statement::Import(_) | Statement::Service(_) => continue,
            };
            let type_entry = Arc::new(t);
            types.push((name.clone(), type_entry.clone()));

            if self
                .global_types_by_name
                .insert(name.clone(), type_entry)
                .is_some()
            {
                return Err(Error::TypeRedefinition { type_name: name });
            }
        }
        Ok(types)
    }

    fn process(&mut self, statements: Vec<Statement>) -> Result<(), Error> {
        let mut file_crc = crc32fast::Hasher::new();

        for stmt in statements {
            if let Some(crc_data) = stmt.crc_data() {
                file_crc.update(crc_data.as_bytes());
            }

            match stmt {
                Statement::Define {
                    name,
                    options,
                    fields,
                    flags,
                    comment,
                } => {
                    let crc = self.crc_for_fields(&fields, false)?;

                    let implicit_fields = [Field {
                        r#type: "u16".to_string(),
                        name: "_vl_msg_id".to_string(),
                        options: vec![],
                        size: None,
                    }];
                    let fields = implicit_fields
                        .into_iter()
                        .chain(fields.into_iter())
                        .collect();

                    let define = Message {
                        name: name.clone(),
                        options: options.clone(),
                        fields,
                        flags,
                        crc: crc.finalize(),
                        comment,
                    };
                    let auto_reply = define.auto_reply();
                    let index_reply = define.index_reply();
                    self.messages.push(define);

                    if auto_reply || index_reply {
                        let mut auto_fields = vec![
                            Field {
                                r#type: "u16".to_string(),
                                name: "_vl_msg_id".to_string(),
                                options: vec![],
                                size: None,
                            },
                            Field {
                                r#type: "u32".to_string(),
                                name: "context".to_string(),
                                options: vec![],
                                size: None,
                            },
                            Field {
                                r#type: "i32".to_string(),
                                name: "retval".to_string(),
                                options: vec![],
                                size: None,
                            },
                        ];
                        if !auto_reply {
                            auto_fields.push(Field {
                                r#type: "u32".to_string(),
                                name: "index".to_string(),
                                options: vec![],
                                size: None,
                            });
                        }

                        // crc doesn't include _vl_msg_id field
                        let crc = self.crc_for_fields(&auto_fields[1..], false)?;
                        let define = Message {
                            name: format!("{}_reply", name),
                            options,
                            fields: auto_fields,
                            flags: Default::default(),
                            crc: crc.finalize(),
                            comment: None,
                        };
                        self.messages.push(define);
                    }
                }
                Statement::Typedef { field, .. } => self.aliases.push(field),
                Statement::TypedefBlock {
                    name,
                    options,
                    fields,
                    flags,
                } => {
                    let un = Type {
                        name: name.clone(),
                        _options: options.clone(),
                        fields,
                        flags,
                    };
                    self.types.push(un);
                }
                Statement::Union {
                    name,
                    options,
                    fields,
                    comment,
                } => {
                    let un = Union {
                        name: name.clone(),
                        _options: options.clone(),
                        fields,
                        comment,
                    };
                    self.unions.push(un);
                }
                Statement::Service(services) => {
                    self.services.extend(services.iter().cloned());
                }
                Statement::Import(import) => self.imports.push(import),
                Statement::Option(option) => self.options.push(option),
                Statement::Enum {
                    name,
                    size,
                    block_statements,
                } => {
                    self.enums.push(Enum {
                        name,
                        _size: size,
                        _block_statements: block_statements,
                    });
                }
                Statement::EnumFlag {
                    name,
                    size,
                    block_statements,
                } => {
                    self.enumflags.push(Enum {
                        name,
                        _size: size,
                        _block_statements: block_statements,
                    });
                }
            }
        }

        let messages_unordered: HashSet<_> = self
            .messages
            .iter()
            .map(|message| message.name.clone())
            .collect();
        let mut seen_services: HashSet<String> = HashSet::new();
        let service_by_caller: HashMap<_, _> = self
            .services
            .iter()
            .map(|service| (service.caller.clone(), service.clone()))
            .collect();
        let service_by_reply: HashMap<_, _> = self
            .services
            .iter()
            .map(|service| (service.reply.clone(), service.clone()))
            .collect();

        for (request_name, service) in &service_by_caller {
            if !messages_unordered.contains(request_name) {
                return Err(Error::ServiceRefersUndefinedMessage(request_name.clone()));
            }
            if service.reply != "null" && !messages_unordered.contains(&service.reply) {
                return Err(Error::ServiceRefersUndefinedMessageReply(
                    service.reply.clone(),
                ));
            }
            if service_by_reply.contains_key(request_name) {
                return Err(Error::ServiceCallerUsedAsReply(request_name.clone()));
            }
            for event in &service.events {
                if !messages_unordered.contains(event) {
                    return Err(Error::ServiceRefersUndefinedEvent(
                        event.clone(),
                        request_name.clone(),
                    ));
                }
                seen_services.insert(event.clone());
            }
        }

        // Create services implicitly, as vppapigen.py does
        for message in self.messages.iter().map(|message| &message.name) {
            if seen_services.contains(message) {
                continue;
            }

            if let Some(base_message) = message.strip_suffix("_reply") {
                if service_by_caller.contains_key(base_message) {
                    continue;
                }
                if !messages_unordered.contains(base_message) {
                    return Err(Error::MissingCallerMessageForReply(
                        base_message.to_string(),
                        message.clone(),
                    ));
                }
                continue;
            }

            if let Some(base_message) = message.strip_suffix("_dump") {
                if service_by_caller.contains_key(message) {
                    continue;
                }
                let details_message = format!("{base_message}_details");
                if messages_unordered.contains(&details_message) {
                    self.services.push(Service {
                        caller: message.clone(),
                        reply: details_message,
                        events: vec![],
                        stream: true,
                        stream_message: None,
                    });
                } else {
                    return Err(Error::MissingDetailsMessageForDump(
                        details_message,
                        message.clone(),
                    ));
                }
                continue;
            }

            if let Some(base_message) = message.strip_suffix("_details") {
                let get_message = format!("{base_message}_get");
                if messages_unordered.contains(&get_message) {
                    if service_by_caller.contains_key(&get_message) {
                        continue;
                    }
                    return Err(Error::ExplicitGetServiceRequiredDetails(
                        get_message,
                        message.clone(),
                    ));
                }
                let dump_message = format!("{base_message}_dump");
                if messages_unordered.contains(&dump_message) {
                    continue;
                }
                return Err(Error::MissingDumpGetForDetailsMessage(
                    get_message,
                    dump_message,
                    message.clone(),
                ));
            }

            if service_by_caller.contains_key(message) {
                continue;
            }

            let reply_message = format!("{message}_reply");
            if messages_unordered.contains(&reply_message) {
                self.services.push(Service {
                    caller: message.clone(),
                    reply: reply_message,
                    events: vec![],
                    stream: false,
                    stream_message: None,
                });
            } else {
                return Err(Error::MissingReplyMessageForMessage {
                    message: message.to_string(),
                    reply_message,
                });
            }
        }

        self.file_crc = file_crc.finalize();

        Ok(())
    }

    pub fn options(&self) -> &[OptionStatement] {
        &self.options
    }

    pub fn messages(&self) -> &[Message] {
        &self.messages
    }

    pub fn services(&self) -> &[Service] {
        &self.services
    }

    pub fn imports(&self) -> &[String] {
        &self.imports
    }

    pub fn unions(&self) -> &[Union] {
        &self.unions
    }

    pub fn aliases(&self) -> &[Field] {
        &self.aliases
    }

    pub fn types(&self) -> &[Type] {
        &self.types
    }

    pub fn enums(&self) -> &[Enum] {
        &self.enums
    }

    pub fn enumflags(&self) -> &[Enum] {
        &self.enumflags
    }

    pub fn file_crc(&self) -> u32 {
        self.file_crc
    }

    pub fn message(&self, name: &str) -> Option<&Message> {
        // Assumes number of messages is low such that a linear search isn't a bottleneck
        self.messages.iter().find(|message| message.name == name)
    }

    pub fn global_types(&self) -> impl Iterator<Item = (&String, &Arc<TypeEntry>)> {
        self.global_types_by_name.iter()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{ApiParser, Error, FieldSize, OptionStatement};
    use pretty_assertions::assert_eq;

    #[test]
    fn sanity() {
        let api = ApiParser::new(&format!("{}/tests/sanity.api", env!("CARGO_MANIFEST_DIR")))
            .inspect_err(|e| println!("{}", e))
            .expect("failed");

        let options: Vec<_> = api
            .options()
            .iter()
            .map(|option| (option.option.clone(), option.value.clone()))
            .collect();
        assert_eq!(
            options,
            [("version".to_string(), Some("1.0.0".to_string()))]
        );
        let expected_imports: &[String] = &[];
        assert_eq!(api.imports(), expected_imports);

        let message = api
            .message("memclnt_create")
            .expect("missing memclnt_create message");
        assert_eq!(message.name(), "memclnt_create");
        let options = message.options();
        let expected_fields = [
            // Implicit field
            ("u16", "_vl_msg_id", &[] as &[OptionStatement], &None),
            ("u32", "context", &[], &None),
            ("i32", "ctx_quota", &[], &None),
            ("u64", "input_queue", &[], &None),
            ("string", "name", &[], &Some(FieldSize::Fixed(64))),
            ("u32", "api_versions", &[], &Some(FieldSize::Fixed(8))),
        ];
        assert_eq!(options.len(), 1, "{:?}", options);
        assert_eq!(options[0].option(), "deprecated");
        assert_eq!(options[0].value(), None);
        assert_eq!(
            message.comment(),
            Some("/*\n * Create a client registration\n */")
        );
        let fields: Vec<_> = message
            .fields()
            .iter()
            .map(|field| {
                (
                    field.r#type.as_str(),
                    field.name.as_str(),
                    field.options.as_slice(),
                    &field.size,
                )
            })
            .collect();
        assert_eq!(fields, &expected_fields);

        let message = api.message("all_types").expect("missing all_types message");
        let expected_fields = [
            // Implicit field
            ("u16", "_vl_msg_id", &[] as &[OptionStatement], &None),
            ("u32", "client_index", &[], &None),
            ("u32", "context", &[], &None),
            ("u8", "f0", &[], &None),
            ("i8", "f1", &[], &None),
            ("u16", "f2", &[], &None),
            ("i16", "f3", &[], &None),
            ("u32", "f4", &[], &None),
            ("i32", "f5", &[], &None),
            ("u64", "f6", &[], &None),
            ("i64", "f7", &[], &None),
            ("f64", "f8", &[], &None),
            ("bool", "f9", &[], &None),
            ("string", "f10", &[], &Some(FieldSize::Variable(None))),
        ];
        let fields: Vec<_> = message
            .fields()
            .iter()
            .map(|field| {
                (
                    field.r#type.as_str(),
                    field.name.as_str(),
                    field.options.as_slice(),
                    &field.size,
                )
            })
            .collect();
        assert_eq!(fields, &expected_fields);

        // Test autoreply-created reply

        let message = api
            .message("all_types_reply")
            .expect("missing all_types_reply message");
        let expected_fields = [
            // Implicit field
            ("u16", "_vl_msg_id", &[] as &[OptionStatement], &None),
            ("u32", "context", &[], &None),
            ("i32", "retval", &[], &None),
        ];
        let fields: Vec<_> = message
            .fields()
            .iter()
            .map(|field| {
                (
                    field.r#type.as_str(),
                    field.name.as_str(),
                    field.options.as_slice(),
                    &field.size,
                )
            })
            .collect();
        assert_eq!(fields, &expected_fields);

        let message = api.message("comments").expect("missing comments message");
        let expected_fields = [
            // Implicit field
            ("u16", "_vl_msg_id", &[] as &[OptionStatement], &None),
            ("u32", "client_index", &[], &None),
            ("u32", "context", &[], &None),
            ("u8", "f0", &[], &None),
            ("u8", "f1", &[], &None),
        ];
        let fields: Vec<_> = message
            .fields()
            .iter()
            .map(|field| {
                (
                    field.r#type.as_str(),
                    field.name.as_str(),
                    field.options.as_slice(),
                    &field.size,
                )
            })
            .collect();
        assert_eq!(fields, &expected_fields);

        let message = api
            .message("sw_interface_event")
            .expect("missing sw_interface_event message");
        let expected_fields = [
            // Implicit field
            ("u16", "_vl_msg_id", &[] as &[OptionStatement], &None),
            ("u32", "client_index", &[], &None),
            ("u32", "pid", &[], &None),
        ];
        let fields: Vec<_> = message
            .fields()
            .iter()
            .map(|field| {
                (
                    field.r#type.as_str(),
                    field.name.as_str(),
                    field.options.as_slice(),
                    &field.size,
                )
            })
            .collect();
        assert_eq!(fields, &expected_fields);

        let service = api
            .services()
            .iter()
            .find(|service| service.caller() == "want_interface_events")
            .expect("missing service for want_interface_events");
        assert_eq!(service.reply(), "want_interface_events_reply");
        assert_eq!(service.events(), ["sw_interface_event"]);
        assert_eq!(service.stream_message(), None);

        let message = api
            .message("sw_interface_tx_placement_details")
            .expect("missing sw_interface_tx_placement_details message");
        let expected_fields = [
            // Implicit field
            ("u16", "_vl_msg_id", &[] as &[OptionStatement], &None),
            ("u32", "client_index", &[], &None),
            ("u32", "context", &[], &None),
        ];
        let fields: Vec<_> = message
            .fields()
            .iter()
            .map(|field| {
                (
                    field.r#type.as_str(),
                    field.name.as_str(),
                    field.options.as_slice(),
                    &field.size,
                )
            })
            .collect();
        assert_eq!(fields, &expected_fields);

        let service = api
            .services()
            .iter()
            .find(|service| service.caller() == "sw_interface_tx_placement_get")
            .expect("missing service for sw_interface_tx_placement_get");
        assert_eq!(service.reply(), "sw_interface_tx_placement_get_reply");
        assert_eq!(service.events(), [] as [String; 0]);
        assert_eq!(
            service.stream_message(),
            Some("sw_interface_tx_placement_details")
        );

        // Test typedef

        let t = api
            .aliases()
            .iter()
            .find(|t| t.name == "ip4_address")
            .expect("missing ip4_address typedef");
        assert_eq!(t.r#type, "u8");
        assert_eq!(t.options, []);
        assert_eq!(t.size, Some(FieldSize::Fixed(4)));

        // Test union

        let un = api
            .unions()
            .iter()
            .find(|un| un.name() == "address_union")
            .expect("missing address_union union");
        assert_eq!(un.options(), []);
        let expected_fields = [
            (
                "vl_api_ip4_address_t",
                "ip4",
                &[] as &[OptionStatement],
                &None,
            ),
            ("vl_api_ip6_address_t", "ip6", &[], &None),
        ];
        let fields: Vec<_> = un
            .fields()
            .iter()
            .map(|field| {
                (
                    field.r#type.as_str(),
                    field.name.as_str(),
                    field.options.as_slice(),
                    &field.size,
                )
            })
            .collect();
        assert_eq!(fields, &expected_fields);

        // Test block typedef

        let t = api
            .types()
            .iter()
            .find(|un| un.name() == "address")
            .expect("missing address type");
        assert_eq!(t.options(), []);
        let expected_fields = [
            (
                "vl_api_address_family_t",
                "af",
                &[] as &[OptionStatement],
                &None,
            ),
            ("vl_api_address_union_t", "un", &[], &None),
        ];
        let fields: Vec<_> = t
            .fields()
            .iter()
            .map(|field| {
                (
                    field.r#type.as_str(),
                    field.name.as_str(),
                    field.options.as_slice(),
                    &field.size,
                )
            })
            .collect();
        assert_eq!(fields, &expected_fields);

        let message = api
            .message("variable_array")
            .expect("missing variable_array message");
        let expected_fields = [
            // Implicit field
            ("u16", "_vl_msg_id", &[] as &[OptionStatement], &None),
            ("u32", "client_index", &[], &None),
            ("u32", "context", &[], &None),
            ("u8", "nitems", &[], &None),
            (
                "u64",
                "configs",
                &[],
                &Some(FieldSize::Variable(Some("nitems".to_string()))),
            ),
        ];
        let fields: Vec<_> = message
            .fields()
            .iter()
            .map(|field| {
                (
                    field.r#type.as_str(),
                    field.name.as_str(),
                    field.options.as_slice(),
                    &field.size,
                )
            })
            .collect();
        assert_eq!(fields, &expected_fields);

        let message = api
            .message("variable_array_reply")
            .expect("missing variable_array_reply message");
        let expected_fields = [
            // Implicit field
            ("u16", "_vl_msg_id", &[] as &[OptionStatement], &None),
            ("u32", "context", &[], &None),
            ("i32", "retval", &[], &None),
            ("u32", "count", &[], &None),
            (
                "vl_api_module_version_t",
                "api_versions",
                &[],
                &Some(FieldSize::Variable(Some("count".to_string()))),
            ),
        ];
        let fields: Vec<_> = message
            .fields()
            .iter()
            .map(|field| {
                (
                    field.r#type.as_str(),
                    field.name.as_str(),
                    field.options.as_slice(),
                    &field.size,
                )
            })
            .collect();
        assert_eq!(fields, &expected_fields);

        let message = api
            .message("field_option")
            .expect("missing field_option message");
        let expected_fields = [
            // Implicit field
            ("u16", "_vl_msg_id", &[] as &[OptionStatement], &None),
            ("u32", "context", &[], &None),
            (
                "bool",
                "keepalive",
                &[OptionStatement {
                    option: "default".to_string(),
                    value: Some("true".to_string()),
                }],
                &None,
            ),
        ];
        let fields: Vec<_> = message
            .fields()
            .iter()
            .map(|field| {
                (
                    field.r#type.as_str(),
                    field.name.as_str(),
                    field.options.as_slice(),
                    &field.size,
                )
            })
            .collect();
        assert_eq!(fields, &expected_fields);

        let message_crcs: Vec<_> = api
            .messages()
            .iter()
            .map(|define| (define.name.as_str(), define.crc))
            .collect();

        // for (name, crc) in &message_crcs {
        //     println!("(\"{}\".to_string(), 0x{:x}),", name, crc);
        // }

        // Generate with:
        // ${VPP_WS}/src/tools/vppapigen/vppapigen.py --input tests/sanity.api --outputdir /tmp --output /tmp/sanity_test.h
        // grep vl_msg_api_add_msg_name_crc /tmp/sanity.api.c | sed 's/.*"\(.*\)_\(.*\)"/("\1", 0x\2)/'
        assert_eq!(
            message_crcs,
            [
                ("memclnt_create", 0x9c5e1c2f),
                ("memclnt_create_reply", 0x42ec4560),
                ("string_literal", 0x51077d14),
                ("string_literal_reply", 0xe8d4e804),
                ("all_types", 0x805c14b8),
                ("all_types_reply", 0xe8d4e804),
                ("no_return", 0xc3a3a452),
                ("comments", 0xd4c3ea6b),
                ("comments_reply", 0xe8d4e804),
                ("want_interface_events", 0x476f5a08),
                ("want_interface_events_reply", 0xe8d4e804),
                ("sw_interface_event", 0xbd5ff46a),
                ("sw_interface_tx_placement_get", 0x51077d14),
                ("sw_interface_tx_placement_get_reply", 0xe8d4e804),
                ("sw_interface_tx_placement_details", 0x51077d14),
                ("union_message", 0xfee40e11),
                ("union_message_reply", 0xe8d4e804),
                ("variable_array", 0x6b8730a3),
                ("variable_array_reply", 0xbc9c2f62),
                ("field_option", 0x0101538d),
                ("field_option_reply", 0xe8d4e804),
                ("ip_table_dump", 0x51077d14),
                ("ip_table_details", 0x8c1bbc26),
                ("flags", 0x76c22d9e),
                ("flags_reply", 0xe8d4e804),
                ("str_request", 0xd170b6e7),
                ("str_request_reply", 0xe8d4e804),
            ]
        );

        let services: Vec<_> = api
            .services()
            .iter()
            .map(|service| {
                (
                    service.caller.as_str(),
                    service.reply.as_str(),
                    service.stream,
                    service.stream_message(),
                )
            })
            .collect();
        assert_eq!(
            services,
            [
                ("no_return", "null", false, None),
                (
                    "want_interface_events",
                    "want_interface_events_reply",
                    false,
                    None
                ),
                (
                    "sw_interface_tx_placement_get",
                    "sw_interface_tx_placement_get_reply",
                    true,
                    Some("sw_interface_tx_placement_details")
                ),
                ("memclnt_create", "memclnt_create_reply", false, None),
                ("string_literal", "string_literal_reply", false, None),
                ("all_types", "all_types_reply", false, None),
                ("comments", "comments_reply", false, None),
                ("union_message", "union_message_reply", false, None),
                ("variable_array", "variable_array_reply", false, None),
                ("field_option", "field_option_reply", false, None),
                ("ip_table_dump", "ip_table_details", true, None),
                ("flags", "flags_reply", false, None),
                ("str_request", "str_request_reply", false, None),
            ]
        );

        assert_eq!(api.file_crc(), 0xeabe8f26);
    }

    /// Parse a file and expect an error, returning that error
    fn parse_file_expect_error(filename: &str) -> Error {
        match ApiParser::new(&format!("{}/{}", env!("CARGO_MANIFEST_DIR"), filename)) {
            Ok(_) => panic!("unexpectedly parsed {} successfully", filename),
            Err(e) => e,
        }
    }

    #[test]
    fn type_resolution_error() {
        assert_eq!(
            parse_file_expect_error("tests/type_resolution_error.api").to_string(),
            "Unable to resolve type does_not_exist"
        );
    }

    #[test]
    fn type_redefinition_error() {
        assert_eq!(
            parse_file_expect_error("tests/type_redefinition_error.api").to_string(),
            "Redefinition of type address_family"
        );
    }

    #[test]
    fn missing_reply_message_for_message() {
        assert_eq!(
            parse_file_expect_error("tests/missing_reply_message_for_message.api").to_string(),
            "create is missing reply message (create_reply) or service definition"
        );
    }

    #[test]
    fn service_definition_refers_undefined_message() {
        assert_eq!(
            parse_file_expect_error("tests/service_refers_undefined_message.api").to_string(),
            "Service definition refers to undefined message create"
        );
    }

    #[test]
    fn service_definition_refers_undefined_message_reply() {
        assert_eq!(
            parse_file_expect_error("tests/service_refers_undefined_message_reply.api").to_string(),
            "Service definition refers to undefined message create_reply in reply"
        );
    }

    #[test]
    fn service_caller_used_as_reply() {
        assert_eq!(
            parse_file_expect_error("tests/service_caller_used_as_reply.api").to_string(),
            "Service caller message delete also used as a reply in a service"
        );
    }

    #[test]
    fn missing_caller_message_for_reply() {
        assert_eq!(
            parse_file_expect_error("tests/missing_caller_message_for_reply.api").to_string(),
            "Missing caller message delete for reply delete_reply"
        );
    }

    #[test]
    fn service_refers_undefined_event() {
        assert_eq!(
            parse_file_expect_error("tests/service_refers_undefined_event.api").to_string(),
            "Service definition refers to undefined event sw_interface_event in message want_interface_events"
        );
    }

    #[test]
    fn explicit_get_service_required_details() {
        assert_eq!(
            parse_file_expect_error("tests/explicit_get_service_required_details.api").to_string(),
            "An explicit service definition is require for sw_interface_tx_placement_get due to the presence of sw_interface_tx_placement_details"
        );
    }

    #[test]
    fn missing_details_message_for_dump() {
        assert_eq!(
            parse_file_expect_error("tests/missing_details_message_for_dump.api").to_string(),
            "Missing details message ip_table_details for dump message ip_table_dump"
        );
    }

    #[test]
    fn missing_dump_get_message_for_details() {
        assert_eq!(
            parse_file_expect_error("tests/missing_dump_get_message_for_details.api").to_string(),
            "Missing ip_table_get or ip_table_dump message for details message ip_table_details"
        );
    }

    #[test]
    fn vla_field_not_last() {
        assert_eq!(
            parse_file_expect_error("tests/vla_field_not_last.api").to_string(),
            "Variable length array field \"configs\" must be the last field in type \"variable_array\""
        );
    }

    #[test]
    fn vla_field_not_last_typedef_block() {
        assert_eq!(
            parse_file_expect_error("tests/vla_field_not_last_typedef_block.api").to_string(),
            "Variable length array field \"configs\" must be the last field in type \"vla_type\""
        );
    }

    #[test]
    fn vla_field_not_last_typedef_block2() {
        assert_eq!(
            parse_file_expect_error("tests/vla_field_not_last_typedef_block2.api").to_string(),
            "Variable length array field \"vla\" must be the last field in type \"variable_array\""
        );
    }

    #[test]
    fn vla_field_not_last_string_typedef() {
        assert_eq!(
            parse_file_expect_error("tests/vla_field_not_last_string_typedef.api").to_string(),
            "Variable length array field \"vla\" must be the last field in type \"variable_array\""
        );
    }

    #[test]
    fn vla_field_union() {
        assert_eq!(
            parse_file_expect_error("tests/vla_field_union.api").to_string(),
            "Variable length array field \"configs\" is present in union \"vla_union\" which isn't allowed"
        );
    }

    #[test]
    fn vla_typedef() {
        assert_eq!(
            parse_file_expect_error("tests/vla_typedef.api").to_string(),
            "Array variable \"configs\" in field \"nitems\" not found"
        );
    }

    #[test]
    fn vla_field_not_found() {
        assert_eq!(
            parse_file_expect_error("tests/vla_field_not_found.api").to_string(),
            "Array variable \"configs\" in field \"nitems\" not found"
        );
    }

    #[test]
    fn string_not_array() {
        assert_eq!(
            parse_file_expect_error("tests/string_not_array.api").to_string(),
            "String field \"s\" must be declared as an array"
        );
    }
}
