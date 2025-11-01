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

/// Marks a function as an VPP plugin init function.
///
/// Multiple init functions are supported, but the invocation order is not guaranteed.
///
/// # Examples
///
/// ```
/// # use vpp_plugin::{vlib::BarrierHeldMainRef, vlib_init_function, vppinfra::error::ErrorStack};
///
/// #[vlib_init_function]
/// fn foo(_vm: &mut BarrierHeldMainRef) -> Result<(), ErrorStack> {
///   println!("Hello, world!");
///   Ok(())
/// }
/// ```
#[proc_macro_attribute]
pub fn vlib_init_function(_attribute: TokenStream, function: TokenStream) -> TokenStream {
    let item: syn::Item = syn::parse_macro_input!(function);
    if let syn::Item::Fn(function) = item {
        let syn::ItemFn {
            attrs,
            block,
            vis,
            sig:
                syn::Signature {
                    ident,
                    unsafety,
                    constness,
                    abi,
                    inputs,
                    output,
                    ..
                },
            ..
        } = function;

        // Ensure that visibility modifier is not present
        match vis {
            syn::Visibility::Inherited => {}
            _ => panic!("#[vlib_init_function] methods must not have visibility modifiers"),
        }

        let init_fn_ident = syn::parse_str::<syn::Ident>(format!("__{}", ident).as_ref())
            .expect("Unable to create identifier");
        let ctor_fn_ident = syn::parse_str::<syn::Ident>(
            format!("__vlib_add_init_function_init_{}", ident).as_ref(),
        )
        .expect("Unable to create identifier");
        let dtor_fn_ident =
            syn::parse_str::<syn::Ident>(format!("__vlib_rm_init_function_{}", ident).as_ref())
                .expect("Unable to create identifier");
        let init_list_elt_ident =
            syn::parse_str::<syn::Ident>(format!("_VLIB_INIT_FUNCTION_INIT_{}", ident).as_ref())
                .expect("Unable to create identifier");
        let ident_lit = format!("{}\0", ident);

        let output = quote!(
            #(#attrs)*
            #vis #unsafety #abi #constness fn #ident(#inputs) #output #block

            unsafe extern "C" fn #init_fn_ident(vm: *mut ::vpp_plugin::bindings::vlib_main_t) -> *mut ::vpp_plugin::bindings::clib_error_t
            {
                if let Err(e) = #ident(::vpp_plugin::vlib::BarrierHeldMainRef::from_ptr_mut(vm)) {
                    e.into_raw()
                } else {
                    std::ptr::null_mut()
                }
            }

            static mut #init_list_elt_ident: ::vpp_plugin::bindings::_vlib_init_function_list_elt_t = ::vpp_plugin::bindings::_vlib_init_function_list_elt_t {
                f: None,
                name: std::ptr::null_mut(),
                next_init_function: std::ptr::null_mut(),
                runs_before: std::ptr::null_mut(),
                runs_after: std::ptr::null_mut(),
                init_order: std::ptr::null_mut(),
            };

            #[::vpp_plugin::macro_support::ctor::ctor(crate_path = ::vpp_plugin::macro_support::ctor)]
            unsafe fn #ctor_fn_ident () {
                unsafe {
                    let vgm = ::vpp_plugin::bindings::vlib_helper_get_global_main();
                    #init_list_elt_ident.next_init_function = (*vgm).init_function_registrations;
                    (*vgm).init_function_registrations = std::ptr::addr_of_mut!(#init_list_elt_ident);
                    #init_list_elt_ident.f = Some(#init_fn_ident);
                    #init_list_elt_ident.name = #ident_lit.as_ptr() as *mut ::std::os::raw::c_char;
                }
            }

            #[::vpp_plugin::macro_support::ctor::dtor(crate_path = ::vpp_plugin::macro_support::ctor)]
            unsafe fn #dtor_fn_ident() {
                let vgm = ::vpp_plugin::bindings::vlib_helper_get_global_main();

                let mut this = (*vgm).init_function_registrations;
                if this.is_null() {
                    return;
                }
                if this == std::ptr::addr_of_mut!(#init_list_elt_ident) {
                    (*vgm).init_function_registrations = (*this).next_init_function;
                    return;
                }

                let mut prev = this;
                this = (*this).next_init_function;
                while !this.is_null() {
                    if this == std::ptr::addr_of_mut!(#init_list_elt_ident) {
                        (*prev).next_init_function = (*this).next_init_function;
                        return;
                    }
                    prev = this;
                    this = (*this).next_init_function;
                }
            }
        );

        // eprintln!("{}", output);

        output.into()
    } else {
        panic!("#[vlib_init_function] items must be functions");
    }
}
