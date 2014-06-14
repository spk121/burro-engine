/* libtga.c - library boilerplate for targa parser

  Copyright (C) 2014 Michael L. Gran <spk121@yahoo.com>

  This library is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>

#include "config.h"
#include "libtga.h"
#include "libtga-private.h"

/**
 * SECTION:libtga
 * @short_description: libtga context
 *
 * The context contains the default values for the library user,
 * and is passed to all library operations.
 */

/**
 * tga_ctx:
 *
 * Opaque object representing the library context.
 */
struct tga_ctx {
  int refcount;
  void (*log_fn)(struct tga_ctx *ctx,
                 int priority, const char *file, int line, const char *fn,
                 const char *format, va_list args);
  void *userdata;
  int log_priority;
};

void
tga_log(struct tga_ctx *ctx,
        int priority, const char *file, int line, const char *fn,
        const char *format, ...)
{
  va_list args;

  va_start(args, format);
  ctx->log_fn(ctx, priority, file, line, fn, format, args);
  va_end(args);
}


static void
log_stderr(struct tga_ctx *ctx,
           int priority, const char *file, int line, const char *fn,
           const char *format, va_list args)
{
  fprintf(stderr, "libtga: %s: ", fn);
  vfprintf(stderr, format, args);
}

/**
 * tga_get_userdata:
 * @ctx: tga library context
 *
 * Retrieve stored data pointer from library context. This might be useful
 * to access from callbacks like a custom logging function.
 *
 * Returns: stored userdata
 **/
TGA_EXPORT void *
tga_get_userdata(struct tga_ctx *ctx)
{
  if (ctx == NULL)
    return NULL;
  return ctx->userdata;
}

/**
 * tga_set_userdata:
 * @ctx: tga library context
 * @userdata: data pointer
 *
 * Store custom @userdata in the library context.
 **/
TGA_EXPORT void
tga_set_userdata(struct tga_ctx *ctx, void *userdata)
{
  if (ctx == NULL)
    return;
  ctx->userdata = userdata;
}

static int
log_priority(const char *priority)
{
  char *endptr;
  int prio;

  prio = strtol(priority, &endptr, 10);
  if (endptr[0] == '\0' || isspace(endptr[0]))
    return prio;
  if (strncmp(priority, "err", 3) == 0)
    return LOG_ERR;
  if (strncmp(priority, "info", 4) == 0)
    return LOG_INFO;
  if (strncmp(priority, "debug", 5) == 0)
    return LOG_DEBUG;
  return 0;
}

/**
 * tga_new:
 *
 * Create tga library context. This reads the tga configuration
 * and fills in the default values.
 *
 * The initial refcount is 1, and needs to be decremented to
 * release the resources of the tga library context.
 *
 * Returns: a new tga library context
 **/
TGA_EXPORT int
tga_new(struct tga_ctx **ctx)
{
  const char *env;
  struct tga_ctx *c;

  c = calloc(1, sizeof(struct tga_ctx));
  if (!c)
    return TARGA_OUT_OF_MEMORY;

  c->refcount = 1;
  c->log_fn = log_stderr;
  c->log_priority = LOG_ERR;

  /* environment overwrites config */
  env = secure_getenv("BURRO_TGA_LOG");
  if (env != NULL)
    tga_set_log_priority(c, log_priority(env));

  info(c, "ctx %p created\n", c);
  dbg(c, "log_priority=%d\n", c->log_priority);
  *ctx = c;
  return 0;
}

/**
 * tga_ref:
 * @ctx: tga library context
 *
 * Take a reference of the tga library context.
 *
 * Returns: the passed tga library context
 **/
TGA_EXPORT struct
tga_ctx *tga_ref(struct tga_ctx *ctx)
{
  if (ctx == NULL)
    return NULL;
  ctx->refcount++;
  return ctx;
}

/**
 * tga_unref:
 * @ctx: tga library context
 *
 * Drop a reference of the tga library context.
 *
 **/
TGA_EXPORT struct
tga_ctx *tga_unref(struct tga_ctx *ctx)
{
  if (ctx == NULL)
    return NULL;
  ctx->refcount--;
  if (ctx->refcount > 0)
    return NULL;
  info(ctx, "context %p released\n", ctx);
  free(ctx);
  return NULL;
}

/**
 * tga_set_log_fn:
 * @ctx: tga library context
 * @log_fn: function to be called for logging messages
 *
 * The built-in logging writes to stderr. It can be
 * overridden by a custom function, to plug log messages
 * into the user's logging functionality.
 *
 **/
TGA_EXPORT
void tga_set_log_fn(struct tga_ctx *ctx,
                    void (*log_fn)(struct tga_ctx *ctx,
                                   int priority, const char *file,
                                   int line, const char *fn,
                                   const char *format, va_list args))
{
  ctx->log_fn = log_fn;
  info(ctx, "custom logging function %p registered\n", log_fn);
}

/**
 * tga_get_log_priority:
 * @ctx: tga library context
 *
 * Returns: the current logging priority
 **/
TGA_EXPORT
int tga_get_log_priority(struct tga_ctx *ctx)
{
  return ctx->log_priority;
}

/**
 * tga_set_log_priority:
 * @ctx: tga library context
 * @priority: the new logging priority
 *
 * Set the current logging priority. The value controls which messages
 * are logged.
 **/
TGA_EXPORT
void tga_set_log_priority(struct tga_ctx *ctx, int priority)
{
  ctx->log_priority = priority;
}

#if 0
struct tga_list_entry;
struct tga_list_entry *tga_list_entry_get_next(struct tga_list_entry *list_entry);
const char *tga_list_entry_get_name(struct tga_list_entry *list_entry);
const char *tga_list_entry_get_value(struct tga_list_entry *list_entry);
#endif

TGA_EXPORT tga_image_ctx_t *
tga_image_ref(tga_image_ctx_t *x)
{
  if (!x)
    return NULL;
  x->refcount++;
  return x;
}

TGA_EXPORT tga_image_ctx_t *
tga_image_unref(tga_image_ctx_t *x)
{
  if (x == NULL)
    return NULL;
  x->refcount--;
  if (x->refcount > 0)
    return NULL;
  dbg(x->ctx, "context %p released\n", x);
  tga_image_free(x->image);
  free(x);
  return NULL;
}

TGA_EXPORT struct tga_ctx *
tga_image_get_ctx(tga_image_ctx_t *x)
{
  return x->ctx;
}

TGA_EXPORT int
tga_image_new_from_memory(struct tga_ctx *ctx, const uint8_t *mem, size_t len,
                          tga_image_ctx_t **x)
{
  tga_image_ctx_t *t;
  targa_error_t ret;

  t = calloc(1, sizeof(tga_image_ctx_t));
  if (!t)
    return TARGA_OUT_OF_MEMORY;
  ret = tga_parse_memory(mem, len, t);
  if (ret != TARGA_OK)
    {
      free (t);
      return ret;
    }
  t->refcount = 1;
  t->ctx = ctx;
  *x = t;
  return 0;
}

#if 0
TGA_EXPORT struct tga_list_entry *
tga_image_get_some_list_entry(tga_image_ctx_t *x)
{
  return NULL;
}
#endif
