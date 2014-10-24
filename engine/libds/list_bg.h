// This file can be processed using m4 to set the value type
// for this linked list.
// For example, by parsing this file with the m4 macro preprocessor like this
//   m4 -D bg=bg list.c > list_bg.c
// it will create a list that operates on "ds_bg_t" objects

// M4 commands
//
//
//
//
//
//
//
//
//
//

//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//


#ifndef LIBDS_LIST_bg_H
#define LIBDS_LIST_bg_H

#include "libds-private.h"


#if 1 == 0
typedef void ds_bg_t ;
#endif

struct ds_bg_node_impl {
  struct ds_bg_node_impl *next;
  struct ds_bg_node_impl *prev;
  ds_bg_t *value;
};

struct ds_bg_list_impl {
  struct ds_bg_node_impl root;
  size_t count;
};

typedef struct ds_bg_node_impl ds_bg_node_t;
typedef struct ds_bg_list_impl ds_bg_list_t;

DS_LOCAL ds_error_t
ds_bg_list_create_empty (ds_ctx_t *ctx, ds_bg_list_t **L);
DS_LOCAL ds_error_t
ds_bg_list_create_from (ds_ctx_t *ctx, size_t count, const ds_bg_t **objects,
		     ds_bg_list_t **L);
DS_LOCAL size_t
ds_bg_list_size (ds_ctx_t *ctx, ds_bg_list_t *list);
DS_LOCAL ds_bg_t *
ds_bg_list_node_value (ds_ctx_t *ctx, ds_bg_list_t *list, ds_bg_node_t *node);
DS_LOCAL void
ds_bg_list_node_set_value (ds_ctx_t *ctx, ds_bg_list_t *list, ds_bg_node_t *node,
			const ds_bg_t *obj);
DS_LOCAL ds_bg_node_t *
  ds_bg_list_next_node (ds_ctx_t *ctx, ds_bg_list_t *list, ds_bg_node_t *node);
DS_LOCAL ds_bg_node_t *
  ds_bg_list_prev_node (ds_ctx_t *ctx, ds_bg_list_t *list, ds_bg_node_t *node);
DS_LOCAL ds_error_t
ds_bg_list_get_at (ds_ctx_t *ctx, ds_bg_list_t *list, size_t position,
		ds_bg_t **obj);
DS_LOCAL ds_error_t
ds_bg_list_set_at (ds_ctx_t *ctx, ds_bg_list_t *list, size_t position,
		const ds_bg_t *obj);
DS_LOCAL ds_error_t
ds_bg_list_search_from_to (ds_ctx_t *ctx, ds_bg_list_t *list, size_t start_index,
			size_t end_index, ds_bg_t *obj, ds_bg_node_t **N);
DS_LOCAL ds_error_t
ds_bg_list_indexof_from_to (ds_ctx_t *ctx, ds_bg_list_t *list, size_t start_index,
			 size_t end_index, ds_bg_t *obj, size_t *I);
DS_LOCAL ds_error_t
ds_bg_list_add_first (ds_ctx_t *ctx, ds_bg_list_t *list, ds_bg_t *obj,
		   ds_bg_node_t **N);
DS_LOCAL ds_error_t
ds_bg_list_add_last (ds_ctx_t *ctx, ds_bg_list_t *list, ds_bg_t *obj,
		  ds_bg_node_t **N);
DS_LOCAL ds_error_t
ds_bg_list_add_before (ds_ctx_t *ctx, ds_bg_list_t *list, ds_bg_node_t *node,
		    ds_bg_t *obj, ds_bg_node_t **N);
DS_LOCAL ds_error_t
ds_bg_list_add_after (ds_ctx_t *ctx, ds_bg_list_t *list, ds_bg_node_t *node,
		   ds_bg_t *obj, ds_bg_node_t **N);
DS_LOCAL ds_error_t
ds_bg_list_add_at (ds_ctx_t *ctx, ds_bg_list_t *list, size_t position, ds_bg_t *obj,
		ds_bg_node_t **N);
DS_LOCAL ds_error_t
  ds_bg_list_remove_node (ds_ctx_t *ctx, ds_bg_list_t *list, ds_bg_node_t *node);
DS_LOCAL ds_error_t
  ds_bg_list_remove_at (ds_ctx_t *ctx, ds_bg_list_t *list, size_t position);
DS_LOCAL ds_error_t
  ds_bg_list_remove (ds_ctx_t *ctx, ds_bg_list_t *list, ds_bg_t *obj, bool *B);
DS_LOCAL void
ds_bg_list_free (ds_ctx_t *ctx, ds_bg_list_t *list);


#endif
