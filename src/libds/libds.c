/*
  ds - something with ds

  Copyright (C) 2011 Someone <someone@example.com>

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <cairo.h>

#include "libds.h"
#include "libds-private.h"
#include "list_bg.h"
#include "list_obj.h"

/**
 * SECTION:libds
 * @short_description: libds context
 *
 * The context contains the default values for the library user,
 * and is passed to all library operations.
 */

/**
 * ds_ctx:
 *
 * Opaque object representing a ds renderer
 */
struct ds_ctx {
  int refcount;
  void (*log_fn)(struct ds_ctx *ctx,
		 int priority, const char *file, int line, const char *fn,
		 const char *format, va_list args);
  void *userdata;
  int log_priority;

  // Screen
  int width, height;
  cairo_t *screen_context;
  cairo_surface_t *screen_surface;

  // Backdrop
  uint32_t backdrop;

  // List of draw objects
  ds_list_bg_t *bg_list;
  ds_list_obj_t *obj_list;

  // List of objects

};

void
ds_log(struct ds_ctx *ctx,
	   int priority, const char *file, int line, const char *fn,
	   const char *format, ...)
{
  va_list args;

  va_start(args, format);
  ctx->log_fn(ctx, priority, file, line, fn, format, args);
  va_end(args);
}

static void
log_stderr(struct ds_ctx *ctx,
	   int priority, const char *file, int line, const char *fn,
	   const char *format, va_list args)
{
  fprintf(stderr, "libds: %s: ", fn);
  vfprintf(stderr, format, args);
}

/**
 * ds_get_userdata:
 * @ctx: ds library context
 *
 * Retrieve stored data pointer from library context. This might be useful
 * to access from callbacks like a custom logging function.
 *
 * Returns: stored userdata
 **/
DS_EXPORT void *
ds_get_userdata(struct ds_ctx *ctx)
{
  if (ctx == NULL)
    return NULL;
  return ctx->userdata;
}

/**
 * ds_set_userdata:
 * @ctx: ds library context
 * @userdata: data pointer
 *
 * Store custom @userdata in the library context.
 **/
DS_EXPORT void
ds_set_userdata(struct ds_ctx *ctx, void *userdata)
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
 * ds_new:
 *
 * Create ds library context. This reads the ds configuration
 * and fills in the default values.
 *
 * The initial refcount is 1, and needs to be decremented to
 * release the resources of the ds library context.
 *
 * Returns: a new ds library context
 **/
DS_EXPORT ds_error_t
ds_new(struct ds_ctx **ctx, int width, int height)
{
  const char *env;
  struct ds_ctx *c;
  ds_error_t ret;

  c = calloc(1, sizeof(struct ds_ctx));
  if (!c)
    return DS_ERROR_OUT_OF_MEMORY;

  c->refcount = 1;
  c->log_fn = log_stderr;
  c->log_priority = LOG_ERR;

  /* environment overwrites config */
  env = secure_getenv("BURRO_DS_LOG");
  if (env != NULL)
    ds_set_log_priority(c, log_priority(env));

  c->width = width;
  c->height = height;

  c->screen_surface = xcairo_image_surface_create (c, CAIRO_FORMAT_ARGB32,
						   width, height);
  if (c->screen_surface == NULL) {
    free (c);
    return DS_ERROR_SURFACE_CREATION_FAILURE;
  }
  c->screen_context = xcairo_create (c, c->screen_surface);
  if (c->screen_context == NULL) {
    free(c);
    return DS_ERROR_CONTEXT_CREATION_FAILURE;
  }
  ret = xcairo_set_antialias (c, c->screen_context, CAIRO_ANTIALIAS_NONE);
  if (ret != DS_OK) {
    free (c);
    return ret;
  }

  c->backdrop = COLOR_BLACK;

  info(c, "ctx %p created\n", c);
  dbg(c, "log_priority=%d\n", c->log_priority);
  dbg(c, "screen size %d, %d", width, height);
  *ctx = c;
  return 0;
}

/**
 * ds_ref:
 * @ctx: ds library context
 *
 * Take a reference of the ds library context.
 *
 * Returns: the passed ds library context
 **/
DS_EXPORT struct ds_ctx *
ds_ref(struct ds_ctx *ctx)
{
  if (ctx == NULL)
    return NULL;
  ctx->refcount++;
  return ctx;
}

/**
 * ds_unref:
 * @ctx: ds library context
 *
 * Drop a reference of the ds library context.
 *
 **/
DS_EXPORT struct ds_ctx *
ds_unref(struct ds_ctx *ctx)
{
  if (ctx == NULL)
    return NULL;
  ctx->refcount--;
  if (ctx->refcount > 0)
    return NULL;

  xcairo_destroy (ctx, ctx->screen_context);
  xcairo_surface_destroy (ctx, ctx->screen_surface);

  info(ctx, "context %p released\n", ctx);

  free(ctx);
  return NULL;
}

/**
 * ds_set_log_fn:
 * @ctx: ds library context
 * @log_fn: function to be called for logging messages
 *
 * The built-in logging writes to stderr. It can be
 * overridden by a custom function, to plug log messages
 * into the user's logging functionality.
 *
 **/
DS_EXPORT void
ds_set_log_fn(struct ds_ctx *ctx,
		  void (*log_fn)(struct ds_ctx *ctx,
				 int priority, const char *file,
				 int line, const char *fn,
				 const char *format, va_list args))
{
  ctx->log_fn = log_fn;
  info(ctx, "custom logging function %p registered\n", log_fn);
}

/**
 * ds_get_log_priority:
 * @ctx: ds library context
 *
 * Returns: the current logging priority
 **/
DS_EXPORT int ds_get_log_priority(struct ds_ctx *ctx)
{
  return ctx->log_priority;
}

/**
 * ds_set_log_priority:
 * @ctx: ds library context
 * @priority: the new logging priority
 *
 * Set the current logging priority. The value controls which messages
 * are logged.
 **/
DS_EXPORT void
ds_set_log_priority(struct ds_ctx *ctx, int priority)
{
  ctx->log_priority = priority;
}

DS_EXPORT void
ds_set_backdrop(struct ds_ctx *ctx, uint32_t bd)
{
  ctx->backdrop = bd;
}

DS_EXPORT uint32_t
ds_get_backdrop(struct ds_ctx *ctx)
{
  return ctx->backdrop;
}

DS_EXPORT ds_error_t
ds_render(ds_ctx_t *ctx, uint32_t **data, int *width, int *height, int *stride)
{
  uint8_t r8, g8, b8;
  int ret;

  PTR_UNPACK24(&(ctx->backdrop), r8, g8, b8);
  xcairo_set_source_rgb (ctx, ctx->screen_context, (double)r8, (double)g8,
			 (double)b8);
  ret = xcairo_paint (ctx, ctx->screen_context);
  if (ret < 0) {
    *stride = 0;
    *data = NULL;
    *width = 0;
    *height = 0;
    return ret;
  }

  ret = xcairo_image_surface_get_argb32_stride(ctx, ctx->screen_surface,
					       stride);
  if (ret < 0)
    return ret;
  ret = xcairo_image_surface_get_argb32_data(ctx, ctx->screen_surface, data);
  if (ret < 0)
    return ret;
  *width = ctx->width;
  *height = ctx->height;
  return DS_OK;
}

struct ds_list_entry;
struct ds_list_entry *ds_list_entry_get_next(struct ds_list_entry *list_entry);
const char *ds_list_entry_get_name(struct ds_list_entry *list_entry);
const char *ds_list_entry_get_value(struct ds_list_entry *list_entry);

struct ds_thing {
  struct ds_ctx *ctx;
  int refcount;
};

DS_EXPORT struct ds_thing *
ds_thing_ref(struct ds_thing *thing)
{
  if (!thing)
    return NULL;
  thing->refcount++;
  return thing;
}

DS_EXPORT struct ds_thing *
ds_thing_unref(struct ds_thing *thing)
{
  if (thing == NULL)
    return NULL;
  thing->refcount--;
  if (thing->refcount > 0)
    return NULL;
  dbg(thing->ctx, "context %p released\n", thing);
  free(thing);
  return NULL;
}

DS_EXPORT struct ds_ctx *
ds_thing_get_ctx(struct ds_thing *thing)
{
  return thing->ctx;
}

DS_EXPORT int
ds_thing_new_from_string(struct ds_ctx *ctx, const char *string, struct ds_thing **thing)
{
  struct ds_thing *t;

  t = calloc(1, sizeof(struct ds_thing));
  if (!t)
    return -ENOMEM;

  t->refcount = 1;
  t->ctx = ctx;
  *thing = t;
  return 0;
}

DS_EXPORT struct ds_list_entry *
ds_thing_get_some_list_entry(struct ds_thing *thing)
{
  return NULL;
}
