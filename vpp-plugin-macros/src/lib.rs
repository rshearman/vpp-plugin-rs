#![warn(
    missing_docs,
    missing_copy_implementations,
    missing_debug_implementations
)]

//! Macros for writing VPP plugins in Rust
//!
//! This crate provides procedural macros to assist in writing VPP plugins in Rust.

use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::quote;
use syn::spanned::Spanned;

#[derive(Default)]
struct PluginRegister {
    version: Option<String>,
    description: Option<String>,
}

impl syn::parse::Parse for PluginRegister {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        const EXPECTED_KEYS: &[&str] = &["version", "description"];

        let mut info = PluginRegister::default();
        let mut seen_keys = HashSet::new();
        loop {
            if input.is_empty() {
                break;
            }
            let key = syn::Ident::parse(input)?.to_string();

            if seen_keys.contains(&key) {
                panic!("Duplicated key \"{key}\". Keys can only be specified once.");
            }

            input.parse::<syn::Token![:]>()?;

            match key.as_str() {
                "version" => {
                    info.version = Some(<syn::LitStr as syn::parse::Parse>::parse(input)?.value())
                }
                "description" => {
                    info.description =
                        Some(<syn::LitStr as syn::parse::Parse>::parse(input)?.value())
                }
                _ => {
                    return Err(syn::Error::new(
                        key.span(),
                        format!("Unknown key \"{key}\". Valid keys are: {EXPECTED_KEYS:?}."),
                    ))
                }
            }

            input.parse::<syn::Token![,]>()?;

            seen_keys.insert(key);
        }
        Ok(info)
    }
}

/// Register the plugin so that it can be loaded by VPP
///
/// Must only be done once per shared object, or linker errors will result.
///
/// # Attributes
///
/// - `version`: (required, string literal) The version string of the plugin. Must be at most 63 characters.
/// - `description`: (optional, string literal) A description of the plugin.
///
/// # Examples
///
/// ```
/// # use vpp_plugin::vlib_plugin_register;
/// vlib_plugin_register! {
///     version: "1.0",
///     description: "Example",
/// }
/// ```
#[proc_macro]
pub fn vlib_plugin_register(ts: TokenStream) -> TokenStream {
    let PluginRegister {
        version,
        description,
    } = syn::parse_macro_input!(ts as PluginRegister);

    let mut version_elems = version
        .expect("Missing required attribute \"version\"")
        .into_bytes();
    if version_elems.len() > 63 {
        panic!("Version string exceeds limit of 63 characters");
    }
    // Pad with zeroes to 64 bytes
    version_elems.extend(std::iter::repeat_n(0, (version_elems.len()..64).count()));
    let description = if let Some(description) = description {
        let description = format!("{}\0", description);
        quote!(#description.as_ptr() as *const ::std::os::raw::c_char)
    } else {
        quote!(std::ptr::null_mut())
    };

    let output = quote!(
        #[doc(hidden)]
        #[link_section = ".vlib_plugin_registration"]
        #[no_mangle]
        #[allow(non_upper_case_globals, non_snake_case)]
        #[used]
        pub static mut vlib_plugin_registration: ::vpp_plugin::bindings::vlib_plugin_registration_t = ::vpp_plugin::bindings::vlib_plugin_registration_t {
            cacheline0: ::vpp_plugin::bindings::__IncompleteArrayField::new(),
            _bitfield_align_1: [0; 0],
            _bitfield_1: ::vpp_plugin::bindings::__BindgenBitfieldUnit::new([0; 1]),
            version: [#(#version_elems as ::std::os::raw::c_char),*],
            version_required: [0; 64],
            overrides: [0; 256],
            early_init: ::std::ptr::null_mut(),
            description: #description,
        };
    );

    // eprintln!("{}", output);

    output.into()
}
