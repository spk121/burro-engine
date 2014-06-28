// This file can be processed using m4 to set the value type
// for this linked list.
// For example, by parsing this file with the m4 macro preprocessor like this
//   m4 -D XTYPE=bg list.c > list_bg.c
// it will create a list that operates on "ds_bg_t" objects

// M4 commands
// changecom(`M4COMMENT')
// define(`TYPE', XTYPE)
// define(`HAS_TYPE', `1')
// define(`LIBDS_LIST_H', concat3(`LIBDS_LIST_',TYPE,`_H'))
// define(`concat3',`$1$2$3')
// define(`ds_list_impl', concat3(`ds_',TYPE,`_list_impl'))
// define(`ds_list_t', concat3(`ds_',TYPE,`_list_t'))
// define(`ds_node_impl', concat3(`ds_',TYPE,`_node_impl'))
// define(`ds_node_t', concat3(`ds_',TYPE,`_node_t'))
// define(`ds_VALUE_t', concat3(`ds_',TYPE,`_t'))

// define(`ds_list_create_empty', concat3(`ds_',TYPE,`_list_create_empty'))
// define(`ds_list_create_from', concat3(`ds_',TYPE,`_list_create_from'))
// define(`ds_list_size', concat3(`ds_',TYPE,`_list_size'))
// define(`ds_list_node_value', concat3(`ds_',TYPE,`_list_node_value'))
// define(`ds_list_node_set_value', concat3(`ds_',TYPE,`_list_node_set_value'))
// define(`ds_list_next_node', concat3(`ds_',TYPE,`_list_next_node'))
// define(`ds_list_prev_node', concat3(`ds_',TYPE,`_list_prev_node'))
// define(`ds_list_get_at', concat3(`ds_',TYPE,`_list_get_at'))
// define(`ds_list_set_at', concat3(`ds_',TYPE,`_list_set_at'))
// define(`ds_list_search_from_to', concat3(`ds_',TYPE,`_list_search_from_to'))
// define(`ds_list_indexof_from_to', concat3(`ds_',TYPE,`_list_indexof_from_to'))
// define(`ds_list_add_first', concat3(`ds_',TYPE,`_list_add_first'))
// define(`ds_list_add_last', concat3(`ds_',TYPE,`_list_add_last'))
// define(`ds_list_add_before', concat3(`ds_',TYPE,`_list_add_before'))
// define(`ds_list_add_after', concat3(`ds_',TYPE,`_list_add_after'))
// define(`ds_list_add_at', concat3(`ds_',TYPE,`_list_add_at'))
// define(`ds_list_remove_node', concat3(`ds_',TYPE,`_list_remove_node'))
// define(`ds_list_remove_at', concat3(`ds_',TYPE,`_list_remove_at'))
// define(`ds_list_remove', concat3(`ds_',TYPE,`_list_remove'))
// define(`ds_list_free', concat3(`ds_',TYPE,`_list_free'))


#ifndef LIBDS_LIST_H
#define LIBDS_LIST_H

#include "libds-private.h"


#if HAS_TYPE == 0
typedef void ds_VALUE_t ;
#endif

struct ds_node_impl {
  struct ds_node_impl *next;
  struct ds_node_impl *prev;
  ds_VALUE_t *value;
};

struct ds_list_impl {
  struct ds_node_impl root;
  size_t count;
};

typedef struct ds_node_impl ds_node_t;
typedef struct ds_list_impl ds_list_t;

DS_LOCAL ds_error_t
ds_list_create_empty (ds_ctx_t *ctx, ds_list_t **L);
DS_LOCAL ds_error_t
ds_list_create_from (ds_ctx_t *ctx, size_t count, const ds_VALUE_t **objects,
		     ds_list_t **L);
DS_LOCAL size_t
ds_list_size (ds_ctx_t *ctx, ds_list_t *list);
DS_LOCAL ds_VALUE_t *
ds_list_node_value (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node);
DS_LOCAL void
ds_list_node_set_value (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node,
			const ds_VALUE_t *obj);
DS_LOCAL ds_node_t *
  ds_list_next_node (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node);
DS_LOCAL ds_node_t *
  ds_list_prev_node (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node);
DS_LOCAL ds_error_t
ds_list_get_at (ds_ctx_t *ctx, ds_list_t *list, size_t position,
		ds_VALUE_t **obj);
DS_LOCAL ds_error_t
ds_list_set_at (ds_ctx_t *ctx, ds_list_t *list, size_t position,
		const ds_VALUE_t *obj);
DS_LOCAL ds_error_t
ds_list_search_from_to (ds_ctx_t *ctx, ds_list_t *list, size_t start_index,
			size_t end_index, ds_VALUE_t *obj, ds_node_t **N);
DS_LOCAL ds_error_t
ds_list_indexof_from_to (ds_ctx_t *ctx, ds_list_t *list, size_t start_index,
			 size_t end_index, ds_VALUE_t *obj, size_t *I);
DS_LOCAL ds_error_t
ds_list_add_first (ds_ctx_t *ctx, ds_list_t *list, ds_VALUE_t *obj,
		   ds_node_t **N);
DS_LOCAL ds_error_t
ds_list_add_last (ds_ctx_t *ctx, ds_list_t *list, ds_VALUE_t *obj,
		  ds_node_t **N);
DS_LOCAL ds_error_t
ds_list_add_before (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node,
		    ds_VALUE_t *obj, ds_node_t **N);
DS_LOCAL ds_error_t
ds_list_add_after (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node,
		   ds_VALUE_t *obj, ds_node_t **N);
DS_LOCAL ds_error_t
ds_list_add_at (ds_ctx_t *ctx, ds_list_t *list, size_t position, ds_VALUE_t *obj,
		ds_node_t **N);
DS_LOCAL ds_error_t
  ds_list_remove_node (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node);
DS_LOCAL ds_error_t
  ds_list_remove_at (ds_ctx_t *ctx, ds_list_t *list, size_t position);
DS_LOCAL ds_error_t
  ds_list_remove (ds_ctx_t *ctx, ds_list_t *list, ds_VALUE_t *obj, bool *B);
DS_LOCAL void
ds_list_free (ds_ctx_t *ctx, ds_list_t *list);


#endif
