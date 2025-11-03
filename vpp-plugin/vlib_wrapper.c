#include <stdarg.h>

#include "vlib_wrapper.h"

vlib_global_main_t *
vlib_helper_get_global_main (void)
{
  return &vlib_global_main;
}

void vlib_helper_remove_node_from_registrations (
  vlib_global_main_t *vgm, vlib_node_registration_t *node)
{
  VLIB_REMOVE_FROM_LINKED_LIST(vgm->node_registrations, node, next_registration);
}

void vlib_helper_remove_feature_from_registrations (
  vnet_feature_main_t *fm, vnet_feature_registration_t *r)
{
    VLIB_REMOVE_FROM_LINKED_LIST(fm->next_feature, r, next);
}

void vlib_helper_remove_cli_command(
  vlib_cli_main_t *cm, vlib_cli_command_t *x)
{
    VLIB_REMOVE_FROM_LINKED_LIST(cm->cli_command_registrations, x, next_cli_command);
}

u32 vlib_helper_buffer_alloc(vlib_main_t *vm, u32 *buffers, u32 n_buffers)
{
    return vlib_buffer_alloc(vm, buffers, n_buffers);
}

void vlib_helper_buffer_free(vlib_main_t * vm, u32 *buffers, u32 n_buffers)
{
    vlib_buffer_free(vm, buffers, n_buffers);
}

// Need a wrapper pending https://github.com/rust-lang/rust/issues/44930
u8 *vlib_helper_format_vnet_sw_if_index_name (u8 *s, ...)
{
    va_list args;
    va_start(args, s);
    s = format_vnet_sw_if_index_name(s, &args);
    va_end(args);
    return s;
}

// Need a wrapper pending https://github.com/rust-lang/rust/issues/44930
uword vlib_helper_unformat_vnet_sw_interface(unformat_input_t * input, ...)
{
    uword ret;
    va_list args;
    va_start(args, input);
    ret = unformat_vnet_sw_interface(input, &args);
    va_end(args);
    return ret;
}

// Need a wrapper pending https://github.com/rust-lang/rust/issues/44930
u8 *vlib_helper_format_ip4_header (u8 *s, ...)
{
    va_list args;
    va_start(args, s);
    s = format_ip4_header(s, &args);
    va_end(args);
    return s;
}

// Need a wrapper pending https://github.com/rust-lang/rust/issues/44930
u8 *vlib_helper_format_ip6_header (u8 *s, ...)
{
    va_list args;
    va_start(args, s);
    s = format_ip6_header(s, &args);
    va_end(args);
    return s;
}

void *vlib_helper_feature_next_with_data (u32 *next0, vlib_buffer_t *b0, u32 n_data_bytes)
{
    return vnet_feature_next_with_data(next0, b0, n_data_bytes);
}

uword vlib_helper_unformat_get_input(unformat_input_t * input)
{
    return unformat_get_input(input);
}

void vlib_helper_unformat_free(unformat_input_t * input)
{
    unformat_free(input);
}

vl_api_registration_t *
vl_api_helper_client_index_to_registration(u32 index)
{
    return vl_api_client_index_to_registration(index);
}
