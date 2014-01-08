#include "../x/xglib.h"
#include "rand.h"

static GRand *r;

void rand_init (void)
{
  r = xg_rand_new ();
}

void rand_init_with_seed (guint32 seed)
{
  r = xg_rand_new_with_seed (seed);
}

gint32 rand_int_range (gint32 begin, gint32 end)
{
  g_return_val_if_fail (begin < end, rand_int_range (end, begin));
  g_return_val_if_fail (begin != end, begin);

  return xg_rand_int_range (r, begin, end);
}


