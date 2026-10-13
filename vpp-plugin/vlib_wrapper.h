#include <vnet/vnet.h>
#include <vnet/plugin/plugin.h>
#include <vnet/interface_funcs.h>
#include <vnet/ip/format.h>
#include <vnet/ip/ip4_packet.h>
#include <vnet/ip/ip6_packet.h>
#include <vnet/udp/udp_packet.h>
#include <vlibapi/api.h>
#include <vlibmemory/api.h>

vlib_global_main_t *
vlib_helper_get_global_main (void);

void vlib_helper_remove_node_from_registrations (
  vlib_global_main_t *vgm, vlib_node_registration_t *node);

void vlib_helper_remove_feature_from_registrations (
  vnet_feature_main_t *fm, vnet_feature_registration_t *r);

void vlib_helper_remove_cli_command(
  vlib_cli_main_t *cm, vlib_cli_command_t *x);

u32 vlib_helper_buffer_alloc(vlib_main_t * vm, u32 * buffers, u32 n_buffers);
void vlib_helper_buffer_free(vlib_main_t * vm, u32 *buffers, u32 n_buffers);

u8 *vlib_helper_format_vnet_sw_if_index_name (u8 *s, ...);
uword vlib_helper_unformat_vnet_sw_interface(unformat_input_t * input, ...);
u8 *vlib_helper_format_ip4_header (u8 *s, ...);
u8 *vlib_helper_format_ip6_header (u8 *s, ...);

uword vlib_helper_unformat_get_input(unformat_input_t * input);
void vlib_helper_unformat_free(unformat_input_t * input);
uword vlib_helper_unformat_check_input(unformat_input_t * i);

vl_api_registration_t *vl_api_helper_client_index_to_registration(u32 index);
api_main_t *vlibapi_helper_get_main(void);
void vl_api_helper_send_msg(vl_api_registration_t *rp, u8 *elem);

void vlib_helper_zero_simple_counter(vlib_simple_counter_main_t * cm, u32 index);
void vlib_helper_zero_combined_counter(vlib_combined_counter_main_t * cm, u32 index);

struct async_context;

void vlib_helper_process_node_loop(
    vlib_main_t *vm,
    struct async_context *context) __attribute__((noreturn));

u16 vnet_helper_ip4_header_checksum(ip4_header_t *ip);
