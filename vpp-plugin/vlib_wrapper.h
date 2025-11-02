#include <vnet/vnet.h>
#include <vnet/plugin/plugin.h>
#include <vnet/interface_funcs.h>
#include <vnet/ip/format.h>
#include <vnet/ip/ip4_packet.h>
#include <vnet/ip/ip6_packet.h>

vlib_global_main_t *
vlib_helper_get_global_main (void);

void vlib_helper_remove_node_from_registrations (
  vlib_global_main_t *vgm, vlib_node_registration_t *node);

u32 vlib_helper_buffer_alloc(vlib_main_t * vm, u32 * buffers, u32 n_buffers);
void vlib_helper_buffer_free(vlib_main_t * vm, u32 *buffers, u32 n_buffers);

u8 *vlib_helper_format_vnet_sw_if_index_name (u8 *s, ...);
u8 *vlib_helper_format_ip4_header (u8 *s, ...);
u8 *vlib_helper_format_ip6_header (u8 *s, ...);

void *vlib_helper_feature_next_with_data(u32 * next0, vlib_buffer_t * b0, u32 n_data_bytes);
