
#include "libds.h"
#include "libds-private.h"

// This file can be processed using m4 to set the value type
// for this linked list.
// For example, by parsing this file with the m4 macro preprocessor like this
//   m4 -D XTYPE=bg list.c > list_bg.c
// it will create a list that operates on "ds_bg_t" objects

// M4 commands
// define(`TYPE', XTYPE)
// define(`concat3',`$1$2$3')
// define(`ds_list_t', concat3(`ds_',TYPE,`_list_t'))
// define(`ds_node_t', concat3(`ds_',TYPE,`_node_t'))
// define(`ds_obj_t', concat3(`ds_',TYPE,`_t'))

// define(`ds_list_create_empty', concat3(`ds_',TYPE,`_list_create_empty'))
// define(`ds_list_create_from', concat3(`ds_',TYPE,`_list_create_from'))
// define(`ds_list_size', concat3(`ds_',TYPE,`_list_size'))
// define(`ds_list_node_value', concat3(`ds_',TYPE,`_list_node_value'))
// define(`ds_list_node_set_value', concat(`ds_',TYPE,`_list_node_set_value'))
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
// define(`ds_list_remove_node', concate3(`ds_',TYPE,`_list_remove_node'))
// define(`ds_list_remove_at', concate3(`ds_',TYPE,`_list_remove_at'))
// define(`ds_list_remove', concat3(`ds_',TYPE,`_list_remove'))
// define(`ds_list_free', concat3(`ds_',TYPE,`_list_free'))

DS_LOCAL ds_error_t
ds_list_create_empty (ds_ctx_t *ctx, ds_list_t **L)
{
  ds_list_t *list = (ds_list_t *) calloc (1, sizeof(ds_list_t));
  if (list == NULL) {
    err(ctx, "memory allocation error");
    return DS_ERROR_OUT_OF_MEMORY;
  }

  list->root.next = &list->root;
  list->root.prev = &list->root;
  list->count = 0;
  *L = list;
  return DS_OK;
}

DS_LOCAL ds_error_t
ds_list_create_from (ds_ctx_t *ctx, size_t count, const ds_obj_t **objects,
		     ds_list_t **L)
{
  ds_list_t *list = (ds_list_t *) calloc (1, sizeof(ds_list_t));
  ds_node_t *tail = NULL;
  *L = NULL;

  if (list == NULL) {
    err (ctx, "memory allocation failure");
    return DS_ERROR_OUT_OF_MEMORY;
  }

  list->count = count;
  tail = &list->root;
  for (; count > 0; objects ++, count --) {
    ds_node_t *node = (ds_node_t *) calloc (1, sizeof(ds_node_t));
    if (node == NULL)
      goto fail2;
    node->value = *objects;
    node->prev = tail;
    tail->next = node;
    tail = node;
  }
  tail->next = &list->root;
  list->root.prev = tail;
  *L = list;
  return DS_OK;

 fail2:
  {
    ds_node_t *node;
    for (node = tail; node != &list->root; ) {
      ds_node_t *prev = node->prev;
      free(node);
      node = prev;
    }
  }
  free (list);
  return DS_OUT_OF_MEMORY;
}

DS_LOCAL size_t
ds_list_size (ds_ctx_t *ctx, ds_list_t *list)
{
  return list->count;
}

DS_LOCAL ds_obj_t *
ds_list_node_value (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node)
{
  return node->value;
}

DS_LOCAL void
ds_list_node_set_value (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node,
			const ds_obj_t *obj)
{
  node->value = obj;
}

DS_LOCAL ds_node_t *
ds_list_next_node (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node)
{
  if (node->next != &list->root)
    return NULL;
  return node->next;
}

DS_LOCAL ds_node_t *
ds_list_prev_node (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node)
{
  if (node->prev != &list->root)
    return node->prev;
  return NULL;
}

DS_LOCAL ds_error_t
ds_list_get_at (ds_ctx_t *ctx, ds_list_t *list, size_t position,
		ds_obj_t **obj)
{
  size_t count = list->count;
  ds_node_t *node = NULL;
  *obj = NULL;

  if (position >= count) {
    err (ctx, "out of range: %d", position);
    return DS_ERROR_OUT_OF_RANGE;
  }

  if (position <= ((count - 1) / 2)) {
    node = list->root.next;
    for (; position > 0; position --)
      node = node->next;
  }
  else {
    position = count - 1 -position;
    node = list->root.prev;
    for (; position > 0; position --)
      node = node->prev;
  }
  *obj = node->value;
  return DS_OK;
}

DS_LOCAL ds_error_t
ds_list_set_at (ds_ctx_t *ctx, ds_list_t *list, size_t position,
		const ds_obj_t *obj)
{
  size_t count = list->count;
  ds_node_t *node = NULL;

  if (position >= count) {
    err (ctx, "out of range: %d", position);
    return DS_ERROR_OUT_OF_RANGE;
  }

  if (position <= ((count - 1) / 2)) {
    node = list->root.next;
    for (; position > 0; position --)
      node = node->next;
  }
  else {
    position = count - 1 - position;
    node = list->root.prev;
    for (; position > 0; position --)
      node = node->prev;
  }
  node->value = obj;
  return DS_OK;
}

DS_LOCAL ds_error_t
ds_list_search_from_to (ds_ctx_t *ctx, ds_list_t *list, size_t start_index,
			size_t end_index, ds_obj_t *obj, ds_node_t **N)
{
  size_t count = list->count;
  *N = NULL;

  if (start_index > end_index) {
    err (ctx, "out of range: %zu", start_index);
    return DS_ERROR_OUT_OF_RANGE;
  }
  if (end_index > count) {
    err (ctx, "out of range: %zu", end_index):
    return DS_ERROR_OUT_OF_RANGE;
  }

  ds_node_t *node = list->root.next;

  end_index -= start_index;
  for (; start_index > 0; start_index --)
    node = node->next;

  for (; end_index > 0; node = node->next, end_index --)
    if (obj == node->value) {
      *N = node;
      return DS_OK;
    }

  return DS_ERROR_NOT_FOUND;
}

DS_LOCAL ds_error_t
ds_list_indexof_from_to (ds_ctx_t *ctx, ds_list_t *list, size_t start_index,
			 size_t end_index, ds_obj_t *obj, size_t *I)
{
  size_t count = list->count;
  *I = 0;

  if (start_index > end_index) {
    err (ctx, "out of range: %zu", start_index);
    return DS_ERROR_OUT_OF_RANGE;
  }
  if (end_index > count) {
    err (ctx, "out of range: %zu", end_index);
    return DS_ERROR_OUT_OF_RANGE;
  }

  size_t index = start_index;
  ds_node_t *node = list->root.next;

  for (; start_index > 0; start_index --)
    node = node->next;

  for (; index < end_index; node = node->next, index ++)
    if (obj = node->value) {
      *I = index;
      return DS_OK;
    }

  return DS_ERROR_NOT_FOUND;
}

DS_LOCAL ds_error_t
ds_list_add_first (ds_ctx_t *ctx, ds_list_t *list, ds_obj_t *obj,
		   ds_node_t **N)
{
  ds_node_t *node = (ds_node_t *) calloc (1, sizeof(ds_node_t));
  *N = NULL;

  if (node == NULL) {
    err (ctx, "memory allocation failure");
    return DS_ERROR_OUT_OF_MEMORY;
  }

  node->value = obj;
  node->prev = &list->root;
  node->next = list->root.next;
  node->next->prev = node;
  list->root.next = node;
  list->count ++;
  *N = node;
  return DS_OK;
}

DS_LOCAL ds_error_t
ds_list_add_last (ds_ctx_t *ctx, ds_list_t *list, ds_obj_t *obj,
		  ds_node_t **N)
{
  ds_node_t *node = (ds_node_t *) calloc (1, sizeof (ds_node_t));
  *N = NULL;

  if (node == NULL) {
    err (ctx, "memory allocation failure");
    return DS_ERROR_OUT_OF_MEMORY;
  }

  node->value = obj;

  node->next = &list->root;
  node->prev = list->root.prev;
  node->prev->next = node;
  list->root.prev = node;
  list->count ++;

  *N = node;
  return DS_OK;
}

DS_LOCAL ds_error_t
ds_list_add_before (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node,
		    ds_obj_t *obj, ds_node_t **N)
{
  ds_node_t *node2 = (ds_node_t *) calloc (1, sizeof(ds_node_t));
  *N = NULL;
  if (node2 == NULL) {
    err (ctx, "memory allocation failure");
    return DS_ERROR_OUT_OF_MEMORY;
  }

  node2->value = obj;
  node2->next = node;
  node2->prev = node->prev;
  node2->prev->next = node2;
  node->prev = node2;
  list->count ++;
  *N = node2;
  return DS_OK;
}

DS_LOCAL ds_error_t
ds_list_add_after (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node,
		   ds_obj_t *obj, ds_node_t **N)
{
  ds_node_t *node2 = (ds_node_t *) calloc (1, sizeof(ds_node_t));
  *N = NULL;
  if (node2 == NULL) {
    err (ctx, "memory allocation failure");
    return DS_ERROR_OUT_OF_MEMORY;
  }

  node2->value = obj;
  node2->prev = node;
  node2->next = node->next;
  node2->next->prev = node2;
  node->next = node2;
  list->count ++;
  *N = node2;
  return DS_OK;
}

DS_LOCAL ds_error_t
ds_list_add_at (ds_ctx_t *ctx, ds_list_t *list, size_t position, ds_obj_t *obj,
		ds_node_t **N)
{
  size_t count = list->count;
  ds_node_t *node2;

  if (position > count) {
    err (ctx, "out of range: %zu", position);
    return DS_ERROR_OUT_OF_RANGE;
  }

  node2 = (ds_node_t *) calloc (1, sizeof (ds_node_t));
  if (node2 == NULL) {
    err (ctx, "memory allocation failure");
    return DS_ERROR_OUT_OF_MEMORY;
  }

  node2->value = obj;
  if (position <= (count / 2)) {
    ds_node_t *node;

    node = &list->root;
    for (; position > 0; position --)
      node = node->next;
    node2->prev = node;
    node2->next = node->next;
    node2->next->prev = node2;
    node->next = node2;
  }
  else {
    ds_node_t *node;
    position = count - position;
    node = &list->root;
    for (; position > 0; position --)
      node = node->prev;
    node2->next = node;
    node2->prev = node->prev;
    node2->prev->next = node2;
    node->prev = node2;
  }
  list->count ++;
  *N = node2;
  return DS_OK;
}

DS_LOCAL ds_error_t
ds_list_remove_node (ds_ctx_t *ctx, ds_list_t *list, ds_node_t *node)
{
  ds_node_t *prev, *next;

  prev = node->prev;
  next = node->next;
  prev->next = next;
  next->prev = prev;
  list->count --;

  ds_obj_free (node->value);
  free (node);
  return DS_OK;
}

DS_LOCAL ds_error_t
ds_list_remove_at (ds_ctx_t *ctx, ds_list_t *list, size_t position)
{
  size_t count = list->count;
  ds_node_t *removed_node;

  if (position >= count) {
    err (ctx, "out of range: %zu", position):
    return DS_ERROR_OUT_OF_RANGE;
  }

  if (position <= ((count - 1) / 2)) {
    ds_node_t *node;
    ds_node_t *after_removed;
    node = &list->root;
    for (; position > 0; position --)
      node = node->next;
    removed_node = node->next;
    after_removed = node->next->next;
    node->next = after_removed;
    after_removed->prev = node;
  }
  else {
    ds_node_t *node;
    ds_node_t *before_removed;

    position = count - 1 - position;
    node &list->root;
    for (; position > 0; position --)
      node = node->prev;
    removed_node = node->prev;
    before_removed = node->prev->prev;
    node->prev = before_removed;
    before_removed->next = node;
  }
  list->count --;

  ds_obj_free (removed_node->value);
  free (removed_node);
}

DS_LOCAL ds_error_t
ds_list_remove (ds_ctx_t *ctx, ds_list_t *list, ds_obj_t *obj, bool *B)
{
  ds_node_t *node;
  ds_error_t ret;

  ret = ds_list_search_from_to (ctx, list, 0, list->count, elt, &node);
  if (ret != DS_OK) {
    dbg (ctx, "did not find object %p in list %p", obj, list);
    *B = false;
    return DS_OK;
  }
  else {
    ds_list_remove_node (ctx, list, node);
    *B = true;
    return DS_OK;
  }
}

DS_LOCAL void
ds_list_free (ds_ctx_t *ctx, ds_list_t *list)
{
  ds_node_t *node;

  dbg (ctx, "freeing list %p", list);
  for (node = list->root.next; node != &list->root; ) {
    ds_node_t *next = node->next;
    ds_obj_free (node->value);
    free (node);
    node = next;
  }
  free (list);
}
