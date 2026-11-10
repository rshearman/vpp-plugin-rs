//! Types with well-known implementations
//!
//! Most of the address and prefix types in VPP's `vnet/ip/ip_types.api` are marked
//! `manual_print`, since VPP formats them by hand as e.g. `192.0.2.1` and `2001:db8::/32`. Rather
//! than leave every plugin to implement the same traits for its own generated copy of these types,
//! `Debug` implementations producing the same well-known formats are generated for them, along
//! with `PartialEq` and `Default` which `manual_print` would otherwise suppress, and conversions to
//! and from the [`std::net`] types.
//!
//! Each type is only treated as well-known if its definition has the expected shape, falling back
//! to the usual behaviour otherwise.

use std::io::Write;

use crate::{
    ApiGenContext, Error, has_tagged_unions,
    parser::{Alias, ApiParser, Field, FieldSize, TaggedUnion, Type},
    to_upper_camel_case,
};

/// Module corresponding to VPP's `vnet/ip/ip_types.api`, whose types have well-known
/// implementations
const IP_TYPES_MODULE: &str = "ip_types";

/// Aliases from `ip_types.api` with well-known implementations
pub(crate) enum WellKnownAlias {
    /// IPv4 or IPv6 address, convertible to and from the named [`std::net`] type
    IpAddress(&'static str),
    /// Alias of a prefix type, formatted the same as it
    Prefix,
}

impl WellKnownAlias {
    /// Returns the well-known implementation for an alias in the given module, if any
    pub(crate) fn detect(module: &str, alias: &Alias) -> Option<Self> {
        if module == IP_TYPES_MODULE {
            well_known_alias(alias)
        } else {
            None
        }
    }
}

fn well_known_alias(alias: &Alias) -> Option<WellKnownAlias> {
    let field = alias.field();
    match (field.name.as_str(), field.r#type.as_str(), &field.size) {
        ("ip4_address", "u8", Some(FieldSize::Fixed(4))) => {
            Some(WellKnownAlias::IpAddress("Ipv4Addr"))
        }
        ("ip6_address", "u8", Some(FieldSize::Fixed(16))) => {
            Some(WellKnownAlias::IpAddress("Ipv6Addr"))
        }
        ("address_with_prefix", "vl_api_prefix_t", None)
        | ("ip4_address_with_prefix", "vl_api_ip4_prefix_t", None)
        | ("ip6_address_with_prefix", "vl_api_ip6_prefix_t", None) => Some(WellKnownAlias::Prefix),
        _ => None,
    }
}

/// Typedefs from `ip_types.api` with well-known implementations
pub(crate) enum WellKnownType {
    /// IPv4 or IPv6 address, convertible to and from [`std::net::IpAddr`]
    Address,
    /// Address and prefix length
    Prefix,
}

impl WellKnownType {
    /// Returns the well-known implementation for a typedef in the given module, if any
    pub(crate) fn detect(module: &str, t: &Type) -> Option<Self> {
        if module == IP_TYPES_MODULE {
            well_known_type(t)
        } else {
            None
        }
    }
}

/// Returns true if the fields have exactly the given types and names, with none being arrays
fn fields_are(fields: &[Field], expected: &[(&str, &str)]) -> bool {
    fields.len() == expected.len()
        && fields.iter().zip(expected).all(|(field, (r#type, name))| {
            field.r#type == *r#type && field.name == *name && field.size.is_none()
        })
}

/// Returns true if the tagged union selects an `ip4` or `ip6` arm using the `af` field
fn selects_ip_address(tagged_union: &TaggedUnion) -> bool {
    tagged_union.discriminator == "af"
        && tagged_union.r#enum.import_module().is_none()
        && tagged_union
            .arms
            .iter()
            .map(|arm| (arm.field.name.as_str(), arm.case.as_str()))
            .eq([("ip4", "ADDRESS_IP4"), ("ip6", "ADDRESS_IP6")])
}

fn well_known_type(t: &Type) -> Option<WellKnownType> {
    let fields = t.fields();
    let address_fields = [
        ("vl_api_address_family_t", "af"),
        ("vl_api_address_union_t", "un"),
    ];
    let prefix_fields = |address_type| [(address_type, "address"), ("u8", "len")];
    match t.name() {
        "address"
            if fields_are(fields, &address_fields)
                && fields[1].tagged_union().is_some_and(selects_ip_address) =>
        {
            Some(WellKnownType::Address)
        }
        "prefix" if fields_are(fields, &prefix_fields("vl_api_address_t")) => {
            Some(WellKnownType::Prefix)
        }
        "ip4_prefix" if fields_are(fields, &prefix_fields("vl_api_ip4_address_t")) => {
            Some(WellKnownType::Prefix)
        }
        "ip6_prefix" if fields_are(fields, &prefix_fields("vl_api_ip6_address_t")) => {
            Some(WellKnownType::Prefix)
        }
        _ => None,
    }
}

/// Fields of a typedef in the given module with semantics implied beyond its definition, if any
///
/// This is used in place of the typedef's own fields when generating its traits.
pub(crate) fn implied_fields(parser: &ApiParser, module: &str, t: &Type) -> Option<Vec<Field>> {
    if module == IP_TYPES_MODULE {
        mprefix_fields(parser, t)
    } else {
        None
    }
}

/// Fields of `ip_types.api`'s `mprefix`, with its address unions selected by its `af` field
///
/// Both address unions are selected by the address family, but there's no way of expressing
/// that in the API file so they are defined without a discriminator, leaving no way to
/// generate `Debug`, `PartialEq`, `Default` or `EndianSwap` for them. So mark them as selected by
/// `af` in the same way as `address`'s union.
fn mprefix_fields(parser: &ApiParser, t: &Type) -> Option<Vec<Field>> {
    let mprefix_fields = [
        ("vl_api_address_family_t", "af"),
        ("u16", "grp_address_length"),
        ("vl_api_address_union_t", "grp_address"),
        ("vl_api_address_union_t", "src_address"),
    ];
    if t.name() != "mprefix"
        || !fields_are(t.fields(), &mprefix_fields)
        || has_tagged_unions(t.fields())
    {
        return None;
    }
    let address = parser.types().iter().find(|t| {
        t.name() == "address" && matches!(well_known_type(t), Some(WellKnownType::Address))
    })?;
    let tagged_union = address.fields()[1].tagged_union.clone();
    Some(
        t.fields()
            .iter()
            .map(|field| {
                let mut field = field.clone();
                if field.r#type == "vl_api_address_union_t" {
                    field.tagged_union = tagged_union.clone();
                }
                field
            })
            .collect(),
    )
}

impl ApiGenContext<'_> {
    /// Generate the well-known implementations for an alias
    pub(crate) fn generate_well_known_alias_impls(
        &mut self,
        name: &str,
        well_known: WellKnownAlias,
    ) -> Result<(), Error> {
        match well_known {
            WellKnownAlias::IpAddress(std_type) => self.generate_ip_address_impls(name, std_type),
            WellKnownAlias::Prefix => self.generate_forwarding_debug(name),
        }
    }

    /// Generate the well-known implementations for a typedef, in place of its Debug
    pub(crate) fn generate_well_known_type_impls(
        &mut self,
        name: &str,
        well_known: WellKnownType,
    ) -> Result<(), Error> {
        match well_known {
            WellKnownType::Address => self.generate_address_impls(),
            WellKnownType::Prefix => self.generate_prefix_debug(name),
        }
    }

    /// Generate Debug and conversions to and from the [`std::net`] type for an IP address alias
    fn generate_ip_address_impls(&mut self, name: &str, std_type: &str) -> Result<(), Error> {
        writeln!(
            self.output_file,
            r#"impl ::std::fmt::Debug for {name} {{
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {{
        ::std::fmt::Debug::fmt(&::std::net::{std_type}::from(*self), f)
    }}
}}

impl From<{name}> for ::std::net::{std_type} {{
    fn from(address: {name}) -> Self {{
        Self::from(address.0)
    }}
}}

impl From<::std::net::{std_type}> for {name} {{
    fn from(address: ::std::net::{std_type}) -> Self {{
        Self(address.octets())
    }}
}}
"#,
            name = to_upper_camel_case(name),
        )?;
        Ok(())
    }

    /// Generate Debug forwarding to the aliased type
    fn generate_forwarding_debug(&mut self, name: &str) -> Result<(), Error> {
        writeln!(
            self.output_file,
            r#"impl ::std::fmt::Debug for {name} {{
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {{
        let inner = self.0;
        ::std::fmt::Debug::fmt(&inner, f)
    }}
}}
"#,
            name = to_upper_camel_case(name),
        )?;
        Ok(())
    }

    /// Generate Debug for a prefix type, in the form `<address>/<len>`
    fn generate_prefix_debug(&mut self, name: &str) -> Result<(), Error> {
        writeln!(
            self.output_file,
            r#"impl ::std::fmt::Debug for {name} {{
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {{
        let address = self.address;
        let len = self.len;
        write!(f, "{{:?}}/{{}}", address, len)
    }}
}}
"#,
            name = to_upper_camel_case(name),
        )?;
        Ok(())
    }

    /// Generate Debug and conversions to and from [`std::net::IpAddr`] for `address`
    ///
    /// The shape of the type has already been checked by [`well_known_type`].
    fn generate_address_impls(&mut self) -> Result<(), Error> {
        writeln!(
            self.output_file,
            r#"/// Error converting an [`Address`] with an unknown address family
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct UnknownAddressFamily(pub AddressFamily);

impl ::std::fmt::Display for UnknownAddressFamily {{
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {{
        write!(f, "unknown address family {{:?}}", self.0)
    }}
}}

impl ::std::error::Error for UnknownAddressFamily {{}}

impl TryFrom<Address> for ::std::net::IpAddr {{
    type Error = UnknownAddressFamily;

    fn try_from(address: Address) -> Result<Self, Self::Error> {{
        let af = address.af;
        match af {{
            // SAFETY: the arm is selected by the discriminator, and any initialised bit pattern is valid for it
            ADDRESS_IP4 => Ok(Self::V4(unsafe {{ address.un.ip4 }}.into())),
            // SAFETY: the arm is selected by the discriminator, and any initialised bit pattern is valid for it
            ADDRESS_IP6 => Ok(Self::V6(unsafe {{ address.un.ip6 }}.into())),
            _ => Err(UnknownAddressFamily(af)),
        }}
    }}
}}

impl From<::std::net::IpAddr> for Address {{
    fn from(address: ::std::net::IpAddr) -> Self {{
        // Start from the default so that all bytes of the union are initialised
        let mut value = Self::default();
        match address {{
            ::std::net::IpAddr::V4(address) => {{
                value.af = ADDRESS_IP4;
                value.un.ip4 = address.into();
            }}
            ::std::net::IpAddr::V6(address) => {{
                value.af = ADDRESS_IP6;
                value.un.ip6 = address.into();
            }}
        }}
        value
    }}
}}

impl ::std::fmt::Debug for Address {{
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {{
        match ::std::net::IpAddr::try_from(*self) {{
            Ok(address) => ::std::fmt::Debug::fmt(&address, f),
            Err(e) => write!(f, "<{{}}>", e),
        }}
    }}
}}
"#,
        )?;
        Ok(())
    }
}
