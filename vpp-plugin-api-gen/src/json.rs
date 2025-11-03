use std::sync::Arc;

use indexmap::IndexMap;
use serde::{
    Serialize,
    ser::{SerializeMap, SerializeSeq},
};

use crate::parser::{
    ApiParser, EnumVariant, Field, FieldSize, Message, OptionStatement, Service, TypeDetails,
    TypeEntry,
};

#[derive(Debug, Serialize)]
struct ApiFile<'a> {
    types: Vec<JsonTypeEntry>,
    messages: &'a [Message],
    unions: Vec<JsonTypeEntry>,
    enums: Vec<JsonTypeEntry>,
    enumflags: Vec<JsonTypeEntry>,
    services: IndexMap<String, Service>,
    options: IndexMap<String, Option<String>>,
    aliases: IndexMap<String, JsonAlias>,
    vl_api_version: String,
    imports: &'a [String],
    counters: &'a [()],
    paths: &'a [()],
}

#[derive(Debug, Serialize)]
struct ApiMessageInfo<'a> {
    crc: String,
    options: IndexMap<String, Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    comment: Option<&'a str>,
}

fn option_statements_to_json_map(options: &[OptionStatement]) -> IndexMap<String, Option<String>> {
    options
        .iter()
        .map(|opt| {
            (
                opt.option().to_string(),
                opt.value().map(|s| String::from(s).replace("\"", "")),
            )
        })
        .collect()
}

impl Serialize for Message {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let info = ApiMessageInfo {
            crc: format!("0x{:08x}", self.crc()),
            options: option_statements_to_json_map(self.options()),
            comment: self.comment(),
        };
        let mut seq = serializer.serialize_seq(Some(1 + self.fields().len() + 1))?;

        seq.serialize_element(self.name())?;
        for field in self.fields() {
            seq.serialize_element(field)?;
        }
        seq.serialize_element(&info)?;

        seq.end()
    }
}

impl Serialize for Service {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut len = 1;
        if self.stream() {
            len += 1;
        }
        if self.stream_message().is_some() {
            len += 1;
        }
        if !self.events().is_empty() {
            len += 1;
        }

        let mut map = serializer.serialize_map(Some(len))?;
        map.serialize_entry("reply", self.reply())?;
        if self.stream() {
            map.serialize_entry("stream", &self.stream())?;
        }
        if let Some(stream_msg) = self.stream_message() {
            map.serialize_entry("stream_msg", stream_msg)?;
        }
        if !self.events().is_empty() {
            map.serialize_entry("events", self.events())?;
        }

        map.end()
    }
}

impl Serialize for Field {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut len = 2;
        if !self.options.is_empty() {
            len += 1;
        }
        len += match &self.size {
            None => 0,
            Some(FieldSize::Fixed(_)) => 1,
            Some(FieldSize::Variable(None)) => 1,
            Some(FieldSize::Variable(Some(_))) => 2,
        };
        let mut seq = serializer.serialize_seq(Some(len))?;
        seq.serialize_element(&self.r#type)?;
        seq.serialize_element(&self.name)?;
        match &self.size {
            None => {}
            Some(FieldSize::Fixed(length)) => seq.serialize_element(&length)?,
            Some(FieldSize::Variable(None)) => seq.serialize_element(&0u32)?,
            Some(FieldSize::Variable(Some(variable))) => {
                seq.serialize_element(&0u32)?;
                seq.serialize_element(variable)?;
            }
        }
        if !self.options.is_empty() {
            seq.serialize_element(&option_statements_to_json_map(&self.options))?;
        }
        seq.end()
    }
}

#[derive(Debug)]
struct JsonAlias(Arc<TypeEntry>);

impl Serialize for JsonAlias {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match &self.0.details {
            TypeDetails::Typedef { r#type, size, .. } => {
                let mut len = 2;
                len += match size {
                    Some(FieldSize::Fixed(_)) => 1,
                    Some(FieldSize::Variable(None)) => 1,
                    _ => 0,
                };
                let mut seq = serializer.serialize_map(Some(len))?;
                seq.serialize_entry("type", r#type)?;
                match size {
                    Some(FieldSize::Fixed(length)) => seq.serialize_entry("length", &length)?,
                    Some(FieldSize::Variable(None)) => seq.serialize_entry("length", &0u32)?,
                    _ => {}
                }
                seq.end()
            }
            _ => serializer.serialize_none(),
        }
    }
}

#[derive(Debug)]
struct JsonTypeEntry(String, Arc<TypeEntry>);

#[derive(Debug, Serialize)]
struct JsonEnumInfo<'a> {
    enumtype: &'a str,
}

impl Serialize for JsonTypeEntry {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match &self.1.details {
            TypeDetails::TypedefBlock { fields, .. } => {
                let mut seq = serializer.serialize_seq(Some(1 + fields.len()))?;
                seq.serialize_element(&self.0)?;
                for field in fields {
                    seq.serialize_element(field)?;
                }
                seq.end()
            }
            TypeDetails::Typedef { .. } => unreachable!(),
            TypeDetails::Union { fields, .. } => {
                let mut seq = serializer.serialize_seq(Some(1 + fields.len()))?;
                seq.serialize_element(&self.0)?;
                for field in fields {
                    seq.serialize_element(field)?;
                }
                seq.end()
            }
            TypeDetails::Enum { size, variants } => {
                let len = 1 + variants.len() + 1;
                let mut seq = serializer.serialize_seq(Some(len))?;
                seq.serialize_element(&self.0)?;
                for variant in variants {
                    seq.serialize_element(variant)?;
                }
                let enuminfo = JsonEnumInfo { enumtype: size };
                seq.serialize_element(&enuminfo)?;
                seq.end()
            }
            TypeDetails::EnumFlag { size, variants } => {
                let len = 1 + variants.len() + 1;
                let mut seq = serializer.serialize_seq(Some(len))?;
                seq.serialize_element(&self.0)?;
                for variant in variants {
                    seq.serialize_element(variant)?;
                }
                let enuminfo = JsonEnumInfo { enumtype: size };
                seq.serialize_element(&enuminfo)?;
                seq.end()
            }
        }
    }
}

impl Serialize for EnumVariant {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(2))?;
        seq.serialize_element(&self.id)?;
        seq.serialize_element(&self.value)?;
        seq.end()
    }
}

pub(crate) fn generate_json(parser: &ApiParser) -> Result<String, serde_json::Error> {
    let services = parser
        .services()
        .iter()
        .map(|service| (service.caller().to_string(), service.clone()))
        .collect();
    let types = parser
        .global_types()
        .filter_map(|(n, t)| {
            if matches!(t.details, TypeDetails::TypedefBlock { .. }) {
                Some(JsonTypeEntry(n.clone(), t.clone()))
            } else {
                None
            }
        })
        .collect();
    let unions = parser
        .global_types()
        .filter_map(|(n, t)| {
            if matches!(t.details, TypeDetails::Union { .. }) {
                Some(JsonTypeEntry(n.clone(), t.clone()))
            } else {
                None
            }
        })
        .collect();
    let enums = parser
        .global_types()
        .filter_map(|(n, t)| {
            if matches!(t.details, TypeDetails::Enum { .. }) {
                Some(JsonTypeEntry(n.clone(), t.clone()))
            } else {
                None
            }
        })
        .collect();
    let enumflags = parser
        .global_types()
        .filter_map(|(n, t)| {
            if matches!(t.details, TypeDetails::EnumFlag { .. }) {
                Some(JsonTypeEntry(n.clone(), t.clone()))
            } else {
                None
            }
        })
        .collect();
    let aliases = parser
        .global_types()
        .filter_map(|(n, t)| {
            if matches!(t.details, TypeDetails::Typedef { .. }) {
                Some((n.clone(), JsonAlias(t.clone())))
            } else {
                None
            }
        })
        .collect();
    let api_file = ApiFile {
        types,
        messages: parser.messages(),
        unions,
        enums,
        enumflags,
        services,
        options: option_statements_to_json_map(parser.options()),
        aliases,
        vl_api_version: format!("0x{:08x}", parser.file_crc()),
        imports: parser.imports(),
        counters: &[],
        paths: &[],
    };
    serde_json::to_string_pretty(&api_file)
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use crate::{json::generate_json, parser::ApiParser};
    use pretty_assertions::assert_eq;

    #[test]
    fn sanity() {
        let api = ApiParser::new(&format!("{}/tests/sanity.api", env!("CARGO_MANIFEST_DIR")))
            .expect("failed");
        let json = generate_json(&api).expect("generating JSON failed");

        let expected_json: serde_json::Value = serde_json::from_reader(
            File::open(format!(
                "{}/tests/sanity.api.json",
                env!("CARGO_MANIFEST_DIR")
            ))
            .expect("unable to open tests/sanity.api.json"),
        )
        .expect("parsing expected JSON failed");
        let expected_json = serde_json::to_string_pretty(&expected_json)
            .expect("generating expected JSON string failed");

        assert_eq!(json, expected_json);
    }
}
