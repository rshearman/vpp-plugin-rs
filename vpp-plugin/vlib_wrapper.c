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
