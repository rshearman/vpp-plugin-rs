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

uword vlib_helper_unformat_get_input(unformat_input_t * input)
{
    return unformat_get_input(input);
}

void vlib_helper_unformat_free(unformat_input_t * input)
{
    unformat_free(input);
}

uword vlib_helper_unformat_check_input(unformat_input_t * i)
{
    return unformat_check_input(i);
}

vl_api_registration_t *
vl_api_helper_client_index_to_registration(u32 index)
{
    return vl_api_client_index_to_registration(index);
}

api_main_t *
vlibapi_helper_get_main(void)
{
    return my_api_main;
}

void
vl_api_helper_send_msg(vl_api_registration_t *rp, u8 *elem)
{
    vl_api_send_msg(rp, elem);
}

void vlib_helper_zero_simple_counter(vlib_simple_counter_main_t *cm, u32 index)
{
    vlib_zero_simple_counter(cm, index);
}

void vlib_helper_zero_combined_counter(vlib_combined_counter_main_t *cm, u32 index)
{
    vlib_zero_combined_counter(cm, index);
}

// Calls back into Rust code
void vpp_plugin_rs_poll_async_coroutine(struct async_context *ctx);
f64 vpp_plugin_rs_next_timer_duration(struct async_context *ctx);

// This needs to be in C because vlib_process_wait_for_event_or_clock does setjmp and is longjmp'd back into by the
// VPP runtime and this may not play well with Rust code and lifetimes
void vlib_helper_process_node_loop(
    vlib_main_t *vm,
    struct async_context *context)
{
    uword *event_data = NULL;

    while (1) {
        vpp_plugin_rs_poll_async_coroutine(context);

        f64 dt = vpp_plugin_rs_next_timer_duration(context);

        if (dt == CLIB_F64_MAX)
            vlib_process_wait_for_event(vm);
        else
            vlib_process_wait_for_event_or_clock(vm, dt);

        // Drain waiting event - we don't care about the event type or data because this is just used in the Rust code
        // as a wakeup
        vlib_process_get_events(vm, &event_data);
        vec_reset_length(event_data);
    }
}

u16
vnet_helper_ip4_header_checksum(ip4_header_t *ip)
{
    return ip4_header_checksum(ip);
}
