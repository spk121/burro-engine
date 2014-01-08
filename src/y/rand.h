#ifndef BURRO_RAND_H
#define BURRO_RAND_H

void rand_init (void);
void rand_init_with_seed (guint32 seed);
gint32 rand_int_range (gint32 begin, gint32 end);

#endif
