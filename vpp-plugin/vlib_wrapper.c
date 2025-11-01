#include <stdarg.h>

#include "vlib_wrapper.h"

vlib_global_main_t *
vlib_helper_get_global_main (void)
{
  return &vlib_global_main;
}
