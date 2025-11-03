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

/// Derives the NextNodes trait for a VPP next node enum
///
/// Only unit variants are allowed and they must not have explicit values.
///
/// # Attributes
///
/// - Each variant must have a `#[next_node = "<node-name>"]` attribute, where `<node-name>` is
///   the name of a VPP node.
///
/// # Examples
///
/// ```
/// # use vpp_plugin::NextNodes;
/// #[derive(NextNodes)]
/// enum ExampleNextNode {
///     #[next_node = "drop"]
///    Drop,
/// }
/// ```
#[proc_macro_derive(NextNodes, attributes(next_node))]
pub fn derive_next_nodes(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let syn::DeriveInput { ident, data, .. } = input;
    if let syn::Data::Enum(data) = data {
        let mut next_nodes = vec![];
        for variant in &data.variants {
            if let Some((_, exp_discriminant)) = &variant.discriminant {
                return syn::Error::new_spanned(
                    exp_discriminant,
                    "Use of explicit discriminants not allowed",
                )
                .into_compile_error()
                .into();
            }
            if !matches!(variant.fields, syn::Fields::Unit) {
                return syn::Error::new_spanned(&variant.fields, "Only unit variants can be used")
                    .into_compile_error()
                    .into();
            }
            let next_node_attr = variant
                .attrs
                .iter()
                .find(|x| x.path().is_ident("next_node"))
                .expect("Missing attribute \"next_node\"");
            let syn::Meta::NameValue(next_node_name_value) = &next_node_attr.meta else {
                return syn::Error::new_spanned(next_node_attr, "Unsupported \"next_node\" attribute syntax. Should be #[next_node = \"<node-name>\".").into_compile_error().into();
            };
            let syn::Expr::Lit(next_node) = &next_node_name_value.value else {
                return syn::Error::new_spanned(next_node_name_value, "Unsupported \"next_node\" attribute syntax. Should be #[next_node = \"<node-name>\".").into_compile_error().into();
            };
            let syn::Lit::Str(next_node) = &next_node.lit else {
                return syn::Error::new_spanned(next_node, "Unsupported \"next_node\" attribute syntax. Should be #[next_node = \"<node-name>\".").into_compile_error().into();
            };
            next_nodes.push(format!("{}\0", next_node.value()));
        }
        let n_next_nodes = data.variants.len();

        let output = quote!(
            #[automatically_derived]
            unsafe impl ::vpp_plugin::vlib::node::NextNodes for #ident {
                type CNamesArray = [*mut ::std::os::raw::c_char; #n_next_nodes];
                const C_NAMES: Self::CNamesArray = [#(#next_nodes.as_ptr() as *mut ::std::os::raw::c_char),*];

                fn into_u16(self) -> u16 {
                    self as u16
                }
            }
        );

        // eprintln!("{}", output);

        output.into()
    } else {
        panic!("#[derive(NextNodes)] can only be used on enums");
    }
}

struct ErrorCounterAttribute {
    name: Option<String>,
    description: String,
    severity: syn::Ident,
}

impl syn::parse::Parse for ErrorCounterAttribute {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        const VALID_SEVERITIES: &[&str] = &["INFO", "WARNING", "ERROR", "CRITICAL"];

        let mut name_or_description_kw = syn::Ident::parse(input)?;

        let name = if name_or_description_kw == "name" {
            input.parse::<syn::Token![=]>()?;
            let name = <syn::LitStr as syn::parse::Parse>::parse(input)?.value();
            input.parse::<syn::Token![,]>()?;
            name_or_description_kw = syn::Ident::parse(input)?;
            Some(name)
        } else {
            None
        };

        if name_or_description_kw != "description" {
            return Err(syn::Error::new(
                name_or_description_kw.span(),
                "Expected: description = \"<...>\", severity = <severity>".to_string(),
            ));
        }

        input.parse::<syn::Token![=]>()?;
        let description = <syn::LitStr as syn::parse::Parse>::parse(input)?.value();

        input.parse::<syn::Token![,]>()?;

        let severity_kw = syn::Ident::parse(input)?;

        if severity_kw != "severity" {
            return Err(syn::Error::new(
                severity_kw.span(),
                "Expected: description = \"<...>\", severity = <severity>".to_string(),
            ));
        }

        input.parse::<syn::Token![=]>()?;
        let severity = syn::Ident::parse(input)?;
        if !VALID_SEVERITIES.contains(&severity.to_string().as_str()) {
            return Err(syn::Error::new(
                severity.span(),
                format!(
                    "Invalid severity \"{}\". Valid severities are: {:?}.",
                    severity, VALID_SEVERITIES
                ),
            ));
        }

        Ok(Self {
            name,
            description,
            severity,
        })
    }
}

/// Derives the ErrorCounters trait for a VPP error counter enum
///
/// Only unit variants are allowed and they must not have explicit values.
///
/// # Attributes
///
/// Each variant must have a `#[error_counter(...)]` attribute, with the following key-value pairs:
/// - `name = "<name>"`: (optional) The name of the error counter. If not provided, the variant name will be used.
/// - `description = "<description>"`: (required) A description of the error counter
/// - `severity = <severity>`: (required) The severity of the error counter. Must be one of
///   `INFO`, `WARNING`, `ERROR`, or `CRITICAL`.
///
/// # Examples
///
/// ```
/// # use vpp_plugin::ErrorCounters;
/// #[derive(ErrorCounters)]
/// enum ExampleErrors {
///     #[error_counter(name = "drop", description = "Example drop", severity = ERROR)]
///     Drop,
/// }
/// ```
#[proc_macro_derive(ErrorCounters, attributes(error_counter))]
pub fn derive_error_counters(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let syn::DeriveInput { ident, data, .. } = input;
    if let syn::Data::Enum(data) = data {
        let mut error_counter_desc = vec![];
        for variant in &data.variants {
            if let Some((_, exp_discriminant)) = &variant.discriminant {
                return syn::Error::new_spanned(
                    exp_discriminant,
                    "Use of explicit discriminants not allowed",
                )
                .into_compile_error()
                .into();
            }
            if !matches!(variant.fields, syn::Fields::Unit) {
                return syn::Error::new_spanned(&variant.fields, "Only unit variants can be used")
                    .into_compile_error()
                    .into();
            }
            let error_counter_attr = variant
                .attrs
                .iter()
                .find(|x| x.path().is_ident("error_counter"))
                .expect("Missing attribute \"error_counter\"");
            let syn::Meta::List(error_counter_list) = &error_counter_attr.meta else {
                return syn::Error::new_spanned(error_counter_attr, "Unsupported \"error_counter\" attribute syntax. Should be #[error_counter(description = \"...\")]").into_compile_error().into();
            };
            let ErrorCounterAttribute {
                name,
                description,
                severity,
            } = match syn::parse2::<ErrorCounterAttribute>(error_counter_list.tokens.clone()) {
                Ok(v) => v,
                Err(e) => return proc_macro::TokenStream::from(e.to_compile_error()),
            };
            let name = if let Some(name) = name {
                format!("{}\0", name)
            } else {
                format!("{}\0", variant.ident)
            };
            let description = format!("{}\0", description);
            let severity = syn::parse_str::<syn::Ident>(&format!(
                "vl_counter_severity_e_VL_COUNTER_SEVERITY_{}",
                severity
            ))
            .expect("Unable to create identifier");

            error_counter_desc.push(quote!(
                ::vpp_plugin::bindings::vlib_error_desc_t {
                    name: #name.as_ptr() as *mut std::ffi::c_char,
                    desc: #description.as_ptr() as *mut std::ffi::c_char,
                    severity: ::vpp_plugin::bindings::#severity,
                    stats_entry_index: 0,
                },
            ));
        }
        let n_error_counters = data.variants.len();

        let output = quote!(
            #[automatically_derived]
            unsafe impl ::vpp_plugin::vlib::node::ErrorCounters for #ident {
                type CDescriptionsArray = [::vpp_plugin::bindings::vlib_error_desc_t; #n_error_counters];
                const C_DESCRIPTIONS: Self::CDescriptionsArray = [
                    #(#error_counter_desc)*
                ];

                fn into_u16(self) -> u16 {
                    self as u16
                }
            }
        );

        // eprintln!("{}", output);

        output.into()
    } else {
        panic!("#[derive(ErrorCounters)] can only be used on enums");
    }
}

const CPU_MARCH_TO_CPU_AND_TARGET_FEATURE: &[(&str, Option<&str>, Option<&str>)] = &[
    ("scalar", None, None),
    ("hsw", Some("x86_64"), Some("avx2")),
];

#[derive(Default)]
struct Node {
    name: Option<syn::LitStr>,
    instance: Option<syn::Ident>,
    runtime_data_default: Option<syn::Ident>,
    format_trace: Option<syn::Ident>,
}

impl Node {
    fn parse(&mut self, meta: syn::meta::ParseNestedMeta) -> Result<(), syn::Error> {
        const EXPECTED_KEYS: &[&str] =
            &["name", "instance", "runtime_data_default", "format_trace"];

        if meta.path.is_ident("name") {
            self.name = Some(meta.value()?.parse()?);
            Ok(())
        } else if meta.path.is_ident("instance") {
            self.instance = Some(meta.value()?.parse()?);
            Ok(())
        } else if meta.path.is_ident("runtime_data_default") {
            self.runtime_data_default = Some(meta.value()?.parse()?);
            Ok(())
        } else if meta.path.is_ident("format_trace") {
            self.format_trace = Some(meta.value()?.parse()?);
            Ok(())
        } else {
            Err(syn::Error::new(
                meta.path.span(),
                format!(
                    "Unknown attribute \"{:?}\". Valid keys are: {EXPECTED_KEYS:?}.",
                    meta.path.get_ident()
                ),
            ))
        }
    }
}

/// Registers a VPP node and associated function
///
/// This registers an internal VPP node, which is the most common type of node (as opposed to
/// processing or input nodes).
///
/// In addition, node functions are also registered, compiled for multiple CPU architectures and
/// target features to allow VPP to select the optimal implementation for the current CPU. This
/// only has an effect for code inlined into the node functions. In other words, `Node::function`
/// should be marked as `#[inline(always)]` and any other functions directly or indirectly called
/// in performance-critical paths should be similarly marked.
///
/// # Attributes
///
/// The macro takes key-value attributes as follows:
/// - `name`: (required, string literal) The name of the VPP node.
/// - `instance`: (required, ident) The instance of the node.
/// - `runtime_data_default`: (optional, ident) An identifier for a constant value of type
///   `Node::RuntimeData` to use as the default runtime data for this node.
/// - `format_trace`: (optional, ident) An identifier for a function with the signature
///   `fn(&mut MainRef, &mut NodeRef<...>, &TraceData) -> String` to format trace
///   data for this node.
///
/// # Examples
///
/// ```
/// # use std::fmt;
/// # use vpp_plugin::{vlib::{self, node::Node}, vlib_node, ErrorCounters, NextNodes};
/// #[derive(Copy, Clone)]
/// struct ExampleTrace;
///
/// impl fmt::Display for ExampleTrace {
///     fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
///         Ok(())
///     }
/// }
///
/// fn format_example_trace(
///     _vm: &mut vlib::MainRef,
///     _node: &mut vlib::NodeRef<ExampleNode>,
///     t: &ExampleTrace,
/// ) -> String {
///     t.to_string()
/// }
/// # #[derive(NextNodes)]
/// # enum ExampleNextNode {
/// #     #[next_node = "drop"]
/// #    _Drop,
/// # }
/// # #[derive(ErrorCounters)]
/// # enum ExampleErrorCounter {
/// #     #[error_counter(description = "Drops", severity = ERROR)]
/// #     _Drop,
/// # }
///
/// static EXAMPLE_NODE: ExampleNode = ExampleNode::new();
///
/// #[vlib_node(
///     name = "example",
///     instance = EXAMPLE_NODE,
///     format_trace = format_example_trace,
/// )]
/// struct ExampleNode;
///
/// impl ExampleNode {
///     const fn new() -> Self {
///         Self
///     }
/// }
///
/// impl vlib::node::Node for ExampleNode {
///     type Vector = ();
///     type Scalar = ();
///     type Aux = ();
///
///     type NextNodes = ExampleNextNode;
///     type RuntimeData = ();
///     type TraceData = ExampleTrace;
///     type Errors = ExampleErrorCounter;
///     type FeatureData = ();
///
///     #[inline(always)]
///     unsafe fn function(
///         &self,
///         _vm: &mut vlib::MainRef,
///         _node: &mut vlib::NodeRuntimeRef<ExampleNode>,
///         _frame: &mut vlib::FrameRef<Self>,
///     ) -> u16 {
///         todo!()
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn vlib_node(attributes: TokenStream, s: TokenStream) -> TokenStream {
    let mut attrs = Node::default();
    let node_parser = syn::meta::parser(|meta| attrs.parse(meta));
    syn::parse_macro_input!(attributes with node_parser);
    let Node {
        name,
        instance,
        runtime_data_default,
        format_trace,
    } = attrs;
    let item: syn::Item = syn::parse_macro_input!(s);
    if let syn::Item::Struct(syn::ItemStruct {
        attrs,
        vis,
        struct_token,
        ident,
        generics,
        fields,
        semi_token,
    }) = item
    {
        let name = name.expect("Missing attribute \"name\". This is required.");
        let name_lit = format!("{}\0", name.value());
        let instance = instance.expect("Missing attribute \"instance\". This is required.");
        let reg_ident =
            syn::parse_str::<syn::Ident>(format!("{}_NODE_REGISTRATION", ident).as_ref())
                .expect("Unable to create identifier");
        let add_node_fn_ident = syn::parse_str::<syn::Ident>(
            format!("__vlib_add_node_registration_{}", ident).as_ref(),
        )
        .expect("Unable to create identifier");
        let rm_node_fn_ident =
            syn::parse_str::<syn::Ident>(format!("__vlib_rm_node_registration_{}", ident).as_ref())
                .expect("Unable to create identifier");
        let (format_trace_output, format_trace) = match format_trace {
            Some(format_trace) => {
                let thunk_format_trace =
                    syn::parse_str::<syn::Ident>(format!("__{}", format_trace).as_ref())
                        .expect("Unable to create identifier");
                (
                    quote!(
                        unsafe extern "C" fn #thunk_format_trace(s: *mut u8, args: *mut ::vpp_plugin::bindings::va_list) -> *mut u8 {
                            let mut args = std::mem::transmute::<_, ::vpp_plugin::macro_support::va_list::VaList<'_>>(args);
                            let vm = args.get::<*const ::vpp_plugin::bindings::vlib_main_t>().cast_mut();
                            let node = args.get::<*const ::vpp_plugin::bindings::vlib_node_t>().cast_mut();
                            let t = args.get::<*const <#ident as ::vpp_plugin::vlib::node::Node>::TraceData>();
                            let str = #format_trace(&mut ::vpp_plugin::vlib::MainRef::from_ptr_mut(vm), &mut #reg_ident.node_from_ptr(node), &*t);
                            let mut s = ::vpp_plugin::vppinfra::vec::Vec::from_raw(s);
                            s.extend(str.as_bytes());
                            s.into_raw()
                        }
                    ),
                    quote!(Some(#thunk_format_trace)),
                )
            }
            None => (quote!(), quote!(None)),
        };
        let runtime_data = match runtime_data_default {
            Some(runtime_data_default) => {
                // It would be ideal to use #runtime_data::default() here, but we cannot call that in a const context and we need to here
                quote!(::std::ptr::addr_of!(#runtime_data_default) as *mut ::std::os::raw::c_void)
            }
            None => quote!({
                ::vpp_plugin::const_assert!(::std::mem::size_of::<<#ident as ::vpp_plugin::vlib::node::Node>::RuntimeData>() == 0);
                std::ptr::null_mut()
            }),
        };

        let mut march_outputs = vec![];

        for (cpu_march, cpu, target_feature) in CPU_MARCH_TO_CPU_AND_TARGET_FEATURE {
            let raw_node_fn_ident =
                syn::parse_str::<syn::Ident>(format!("__{}_{}", ident, cpu_march).as_ref())
                    .expect("Unable to create identifier");
            let registration_ident = syn::parse_str::<syn::Ident>(
                format!("{}_FN_REGISTRATION_{}", ident, cpu_march).as_ref(),
            )
            .expect("Unable to create identifier");
            let reg_fn_ident = syn::parse_str::<syn::Ident>(
                format!("{}_multiarch_register_{}", ident, cpu_march).as_ref(),
            )
            .expect("Unable to create identifier");
            let march_variant_ident = syn::parse_str::<syn::Ident>(
                format!(
                    "clib_march_variant_type_t_CLIB_MARCH_VARIANT_TYPE_{}",
                    cpu_march
                )
                .as_ref(),
            )
            .expect("Unable to create identifier");
            let cpu_condition = if let Some(cpu) = cpu {
                quote!(#[cfg(target_arch = #cpu)])
            } else {
                quote!()
            };
            let target_feature = if let Some(target_feature) = target_feature {
                quote!(#[target_feature(enable = #target_feature)])
            } else {
                quote!()
            };

            let output = quote!(
                #cpu_condition
                #target_feature
                #[doc(hidden)]
                unsafe extern "C" fn #raw_node_fn_ident(
                    vm: *mut ::vpp_plugin::bindings::vlib_main_t,
                    node: *mut ::vpp_plugin::bindings::vlib_node_runtime_t,
                    frame: *mut ::vpp_plugin::bindings::vlib_frame_t,
                ) -> ::vpp_plugin::bindings::uword {
                    unsafe {
                        <#ident as ::vpp_plugin::vlib::node::Node>::function(
                            &#instance,
                            ::vpp_plugin::vlib::MainRef::from_ptr_mut(vm),
                            #reg_ident.node_runtime_from_ptr(node),
                            #reg_ident.frame_from_ptr(frame),
                        )
                    }.into()
                }

                #cpu_condition
                #[doc(hidden)]
                static mut #registration_ident: ::vpp_plugin::bindings::vlib_node_fn_registration_t = ::vpp_plugin::bindings::vlib_node_fn_registration_t{
                    function: Some(#raw_node_fn_ident),
                    march_variant: ::vpp_plugin::bindings::#march_variant_ident,
                    next_registration: std::ptr::null_mut(),
                };

                #cpu_condition
                #[doc(hidden)]
                #[::vpp_plugin::macro_support::ctor::ctor(crate_path = ::vpp_plugin::macro_support::ctor)]
                fn #reg_fn_ident() {
                    unsafe {
                        #reg_ident.register_node_fn(std::ptr::addr_of!(#registration_ident).cast_mut());
                    }
                }
            );
            march_outputs.push(output);
        }

        let output = quote!(
            #(#attrs)*
            #vis #struct_token #ident #generics #fields #semi_token

            #[::vpp_plugin::macro_support::ctor::ctor(crate_path = ::vpp_plugin::macro_support::ctor)]
            fn #add_node_fn_ident() {
                unsafe {
                    #reg_ident.register();
                }
            }

            #[::vpp_plugin::macro_support::ctor::dtor(crate_path = ::vpp_plugin::macro_support::ctor)]
            fn #rm_node_fn_ident() {
                unsafe {
                    #reg_ident.unregister();
                }
            }

            #format_trace_output

            static #reg_ident: ::vpp_plugin::vlib::node::NodeRegistration<#ident, { <<#ident as ::vpp_plugin::vlib::node::Node>::NextNodes as ::vpp_plugin::vlib::node::NextNodes>::C_NAMES.len() } > = ::vpp_plugin::vlib::node::NodeRegistration::new(
                ::vpp_plugin::bindings::_vlib_node_registration {
                    function: None,
                    name: #name_lit.as_ptr() as *mut ::std::os::raw::c_char,
                    type_: ::vpp_plugin::bindings::vlib_node_type_t_VLIB_NODE_TYPE_INTERNAL,
                    error_counters: <<#ident as ::vpp_plugin::vlib::node::Node>::Errors as ::vpp_plugin::vlib::node::ErrorCounters>::C_DESCRIPTIONS.as_ptr().cast_mut(),
                    format_trace: #format_trace,
                    runtime_data: #runtime_data,
                    runtime_data_bytes: {
                        ::vpp_plugin::const_assert!(::std::mem::size_of::<<ExampleNode as ::vpp_plugin::vlib::node::Node>::RuntimeData>() <= u8::MAX as usize);
                        ::vpp_plugin::const_assert!(::std::mem::align_of::<<ExampleNode as ::vpp_plugin::vlib::node::Node>::RuntimeData>() <= ::vpp_plugin::vlib::node::RUNTIME_DATA_ALIGN);
                        ::std::mem::size_of::<<#ident as ::vpp_plugin::vlib::node::Node>::RuntimeData>() as u8
                    },
                    vector_size: {
                        ::vpp_plugin::const_assert!(::std::mem::size_of::<<ExampleNode as ::vpp_plugin::vlib::node::Node>::Vector>() <= u8::MAX as usize);
                        ::vpp_plugin::const_assert!(::std::mem::align_of::<<ExampleNode as ::vpp_plugin::vlib::node::Node>::Vector>() <= ::vpp_plugin::vlib::node::FRAME_DATA_ALIGN);
                        ::std::mem::size_of::<<#ident as ::vpp_plugin::vlib::node::Node>::Vector>() as u8
                    },
                    aux_size: {
                        ::vpp_plugin::const_assert!(::std::mem::size_of::<<ExampleNode as ::vpp_plugin::vlib::node::Node>::Aux>() <= u8::MAX as usize);
                        ::vpp_plugin::const_assert!(::std::mem::align_of::<<ExampleNode as ::vpp_plugin::vlib::node::Node>::Aux>() <= ::vpp_plugin::vlib::node::FRAME_DATA_ALIGN);
                        ::std::mem::size_of::<<#ident as ::vpp_plugin::vlib::node::Node>::Aux>() as u8
                    },
                    scalar_size: {
                        ::vpp_plugin::const_assert!(::std::mem::size_of::<<ExampleNode as ::vpp_plugin::vlib::node::Node>::Scalar>() <= u16::MAX as usize);
                        ::vpp_plugin::const_assert!(::std::mem::align_of::<<ExampleNode as ::vpp_plugin::vlib::node::Node>::Scalar>() <= ::vpp_plugin::vlib::node::FRAME_DATA_ALIGN);
                        ::std::mem::size_of::<<#ident as ::vpp_plugin::vlib::node::Node>::Scalar>() as u16
                    },
                    n_errors: <<#ident as ::vpp_plugin::vlib::node::Node>::Errors as ::vpp_plugin::vlib::node::ErrorCounters>::C_DESCRIPTIONS.len()
                        as u16,
                    n_next_nodes: <<#ident as ::vpp_plugin::vlib::node::Node>::NextNodes as ::vpp_plugin::vlib::node::NextNodes>::C_NAMES.len()
                        as u16,
                    next_nodes: <<#ident as ::vpp_plugin::vlib::node::Node>::NextNodes as ::vpp_plugin::vlib::node::NextNodes>::C_NAMES,
                    ..::vpp_plugin::bindings::_vlib_node_registration::new()
                });

            #(#march_outputs)*
        );

        // eprintln!("{}", output);

        output.into()
    } else {
        panic!("#[vlib_node] items must be structs");
    }
}

struct FeatureInit {
    identifier: Option<syn::Ident>,
    arc_name: Option<String>,
    node: Option<syn::Ident>,
    runs_before: Vec<String>,
    runs_after: Vec<String>,
    feature_data_type: Option<syn::Type>,
}

impl syn::parse::Parse for FeatureInit {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        const EXPECTED_KEYS: &[&str] = &[
            "identifier",
            "arc_name",
            "node",
            "runs_before",
            "runs_after",
            "feature_data_type",
        ];

        let mut info = FeatureInit {
            identifier: None,
            arc_name: None,
            node: None,
            runs_before: Vec::new(),
            runs_after: Vec::new(),
            feature_data_type: None,
        };
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
                "identifier" => info.identifier = Some(syn::Ident::parse(input)?),
                "arc_name" => {
                    info.arc_name = Some(<syn::LitStr as syn::parse::Parse>::parse(input)?.value())
                }
                "node" => info.node = Some(syn::Ident::parse(input)?),
                "runs_before" => {
                    let runs_before_input;
                    syn::bracketed!(runs_before_input in input);
                    let runs_before = syn::punctuated::Punctuated::<syn::LitStr, syn::Token![,]>::parse_terminated(&runs_before_input)?;
                    info.runs_before = runs_before.into_iter().map(|s| s.value()).collect();
                }
                "runs_after" => {
                    let runs_after_input;
                    syn::bracketed!(runs_after_input in input);
                    let runs_after = syn::punctuated::Punctuated::<syn::LitStr, syn::Token![,]>::parse_terminated(&runs_after_input)?;
                    info.runs_after = runs_after.into_iter().map(|s| s.value()).collect();
                }
                "feature_data_type" => info.feature_data_type = Some(syn::Type::parse(input)?),
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

/// Registers a VPP feature
///
/// Allowing the VPP node to be executed in a feature arc. Note that the feature won't be
/// executed until enabled.
///
/// # Attributes
///
/// Each variant must have a `#[error_counter(...)]` attribute, with the following key-value pairs:
/// - `identifier = <ident>`: (required) An identifier of a static of type
///   `vpp_plugin::vnet::feature::FeatureRegistration` that will be declared and registered by
///   the macro.
/// - `arc_name = "<name>"`: (required, string literal) The name of the feature arc the node will
///   be registered to.
/// - `node = <ident>`: (required, string literal) The name of a node type registered using
///   [`vlib_node`].
/// - `runs_before = [("<feature-name>")*]`: (optional, string literal) A list of features that
///   should be executed in the feature arc before this feature is executed.
/// - `runs_after = [("<feature-name>")*]`: (optional, string literal) A list of features that
///   should be executed in the feature arc after this feature is executed.
///
/// # Examples
///
/// ```
/// # use vpp_plugin::{vlib::{self, node::Node}, vlib_node, vnet_feature_init, ErrorCounters, NextNodes};
/// # #[derive(NextNodes)]
/// # enum ExampleNextNode {
/// #     #[next_node = "drop"]
/// #    _Drop,
/// # }
/// # #[derive(ErrorCounters)]
/// # enum ExampleErrorCounter {}
/// #
/// # static EXAMPLE_NODE: ExampleNode = ExampleNode::new();
/// #
/// #[vlib_node(
///     name = "example",
///     instance = EXAMPLE_NODE,
/// )]
/// struct ExampleNode;
///
/// // ...
///
/// # impl ExampleNode {
/// #     const fn new() -> Self {
/// #         Self
/// #     }
/// # }
/// # impl vlib::node::Node for ExampleNode {
/// #     type Vector = ();
/// #     type Scalar = ();
/// #     type Aux = ();
/// #
/// #     type NextNodes = ExampleNextNode;
/// #     type RuntimeData = ();
/// #     type TraceData = ();
/// #     type Errors = ExampleErrorCounter;
/// #     type FeatureData = ();
/// #
/// #     #[inline(always)]
/// #     unsafe fn function(
/// #         &self,
/// #         _vm: &mut vlib::MainRef,
/// #         _node: &mut vlib::NodeRuntimeRef<Self>,
/// #         _frame: &mut vlib::FrameRef<Self>,
/// #     ) -> u16 {
/// #         todo!()
/// #     }
/// # }
/// vnet_feature_init! {
///     identifier: EXAMPLE_FEAT,
///     arc_name: "ip4-unicast",
///     node: ExampleNode,
///     runs_before: ["ip4-flow-classify"],
///     runs_after: ["ip4-sv-reassembly-feature"],
/// }
/// ```
#[proc_macro]
pub fn vnet_feature_init(ts: TokenStream) -> TokenStream {
    let FeatureInit {
        identifier,
        arc_name,
        node,
        runs_before,
        runs_after,
        feature_data_type,
    } = syn::parse_macro_input!(ts as FeatureInit);

    let ident = identifier.expect("Missing key \"identifier\". This is required.");
    let ctor_fn_ident =
        syn::parse_str::<syn::Ident>(format!("__vnet_add_feature_registration_{}", ident).as_ref())
            .expect("Unable to create identifier");
    let dtor_fn_ident =
        syn::parse_str::<syn::Ident>(format!("__vnet_rm_feature_registration_{}", ident).as_ref())
            .expect("Unable to create identifier");
    let arc_name = arc_name.expect("Missing key \"arc_name\". This is required.");
    let node = node.expect("Missing key \"node\". This is required.");
    let reg_ident = syn::parse_str::<syn::Ident>(format!("{}_NODE_REGISTRATION", node).as_ref())
        .expect("Unable to create identifier");
    let arc_name_lit = format!("{arc_name}\0");
    let runs_before_output = if runs_before.is_empty() {
        quote!(std::ptr::null_mut())
    } else {
        let runs_before = runs_before.iter().map(|s| {
            let s = format!("{s}\0");
            quote!(#s.as_ptr() as *mut ::std::os::raw::c_char)
        });
        quote!(&[#(#runs_before),*, std::ptr::null_mut()] as *const *mut ::std::os::raw::c_char as *mut *mut ::std::os::raw::c_char)
    };
    let runs_after_output = if runs_after.is_empty() {
        quote!(std::ptr::null_mut())
    } else {
        let runs_after = runs_after.iter().map(|s| {
            let s = format!("{s}\0");
            quote!(#s.as_ptr() as *mut ::std::os::raw::c_char)
        });
        quote!(&[#(#runs_after),*, std::ptr::null_mut()] as *const *mut ::std::os::raw::c_char as *mut *mut ::std::os::raw::c_char)
    };
    let feature_data_type = feature_data_type
        .unwrap_or_else(|| syn::parse_str::<syn::Type>("()").expect("Unable to create identifier"));

    let output = quote!(
        #[doc(hidden)]
        #[::vpp_plugin::macro_support::ctor::ctor(crate_path = ::vpp_plugin::macro_support::ctor)]
        fn #ctor_fn_ident() {
            unsafe { #ident.register(); }
        }

        #[doc(hidden)]
        #[::vpp_plugin::macro_support::ctor::dtor(crate_path = ::vpp_plugin::macro_support::ctor)]
        fn #dtor_fn_ident() {
            unsafe { #ident.unregister(); }
        }

        static #ident: ::vpp_plugin::vnet::feature::FeatureRegistration<#feature_data_type> = unsafe {
            ::vpp_plugin::vnet::feature::FeatureRegistration::new(
                ::vpp_plugin::bindings::vnet_feature_registration_t {
                    arc_name: #arc_name_lit.as_ptr() as *mut ::std::os::raw::c_char,
                    runs_before: #runs_before_output,
                    runs_after: #runs_after_output,
                    enable_disable_cb: None,
                    ..::vpp_plugin::bindings::vnet_feature_registration_t::new()
                },
                &#reg_ident,
            )
        };
    );

    // eprintln!("{}", output);

    output.into()
}
