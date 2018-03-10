/** @file pixbuf.c
 *  @brief Arrays of pixel data
 */

#include <stdint.h>
#include <stdbool.h>
#include "../x.h"
#include "guile.h"
#include "pixbuf.h"

SCM pixbuf_tag;

SCM_DEFINE(G_pixbuf_make, "make-pixbuf", 3, 0, 0, (SCM s_isize, SCM s_jsize, SCM s_argb32), "\
Create a new pixbuf with the given dimensions and a constant color value.")
{
    SCM_ASSERT (scm_is_integer(s_isize), s_isize, SCM_ARG1, "make-pixbuf");
    SCM_ASSERT (scm_is_integer(s_jsize), s_jsize, SCM_ARG2, "make-pixbuf");
    SCM_ASSERT (scm_is_unsigned_integer (s_argb32, 0, 0xFFFFFFFF), s_argb32, SCM_ARG3, "make-pixbuf");

    size_t c_isize = scm_to_size_t (s_isize);
    size_t c_jsize = scm_to_size_t (s_jsize);
    uint32_t c_argb32 = scm_to_uint32 (s_argb32);
    scm_remember_upto_here_2 (s_isize, s_jsize);
    scm_remember_upto_here_1 (s_argb32);
    
    size_t c_stride;
    
    if (c_isize % 8 == 0)
        c_stride = c_isize;
    else
        c_stride = c_isize + (8 - (c_isize % 8));

    /* FIXME: Optimization?  Is scm_gc_malloc_pointerless aligned to a
     * dword boundary? */
    pixbuf_t *pbuf = scm_gc_malloc (sizeof (pixbuf_t), "pixbuf");
    pbuf->width = c_isize;
    pbuf->height = c_jsize;
    pbuf->stride = c_stride;
    pbuf->data = scm_gc_malloc_pointerless (c_jsize * c_stride * sizeof(uint32_t), "pixbuf");
    memset (pbuf->data, 0, c_jsize * c_stride * sizeof (uint32_t));
    for (size_t j = 0; j < c_jsize; j ++)
    {
        for (size_t i = 0; i < c_isize; i ++)
        {
            pbuf->data[j*c_stride + i] = c_argb32;
        }
    }
            
    return scm_make_foreign_object_1 (pixbuf_tag, pbuf);
}    

SCM_DEFINE (G_pixbuf_from_image_file, "make-pixbuf-from-image-file", 1, 0, 0, (SCM filename), "\
Make a pixbuf from a file that contains some sort of ARGB32 data, like a png.")
{
    SCM_ASSERT (scm_is_string (filename), filename, SCM_ARG1, "pixbuf-from-image-file");
    
    char *c_filename = scm_to_locale_string (filename);
    char *path = xg_find_data_file (c_filename);
    free (c_filename);
    if (path == NULL)
        return SCM_BOOL_F;
    
    GdkPixbuf *pb = xgdk_pixbuf_new_from_file (path);
    if (pb == NULL)
        return SCM_BOOL_F;

    if (xgdk_pixbuf_is_argb32 (pb) == false && xgdk_pixbuf_is_xrgb32 (pb) == false)
    {
        xg_object_unref (pb);
        g_critical ("failed to load %s as an ARGB32 or XRGB32 pixbuf", path);
        g_free (path);
    }
    else
    {
        int img_width, img_height, img_stride;
        bool opaque = xgdk_pixbuf_is_xrgb32 (pb) == true && xgdk_pixbuf_is_argb32 (pb) == false;
        if (opaque)
        {
            GdkPixbuf *pb2 = gdk_pixbuf_add_alpha (pb, FALSE, 0, 0, 0);
            g_object_unref(pb);
            pb = pb2;
        }

        xgdk_pixbuf_get_width_height_stride (pb, &img_width, &img_height, &img_stride);

        pixbuf_t *pbuf = scm_gc_malloc (sizeof (pixbuf_t), "pixbuf");
        pbuf->width = img_width;
        pbuf->height = img_height;
        pbuf->stride = img_stride;
        pbuf->data = scm_gc_malloc_pointerless (img_height * img_stride * sizeof(uint32_t), "pixbuf");
        memset (pbuf->data, 0, img_height * img_stride * sizeof(uint32_t));
        uint32_t *c32 = xgdk_pixbuf_get_argb32_pixels(pb);    
        for (size_t j = 0; j < img_height; j ++)
        {
            for (size_t i = 0; i < img_width; i ++)
            {
                // Convert from GDKPixbuf ABGR to Cairo ARGB
                uint32_t val = c32[j * img_stride + i];
                val = (val & 0xFF00FF00) | ((val >> 16) & 0xFF) | ((val & 0xFF) << 16);

                // Convert from GDK un-premultiplied alpha to Cairo pre-multiplied alpha
                unsigned a = val >> 24;
                unsigned r = (((val >> 16) & 0xFF) * a) / 256;
                unsigned g = (((val >> 8) & 0xFF) * a) / 256;
                unsigned b = (((val >> 0) & 0xFF) * a) / 256;
                pbuf->data[j * img_stride + i] = a << 24 | r << 16 | g << 8 | b;
            }
        }
        g_debug ("loaded pixbuf %s", path);
        g_free (path);
        g_object_unref (pb);
        return scm_make_foreign_object_1 (pixbuf_tag, pbuf);
    }
    g_return_val_if_reached (SCM_BOOL_F);
}

SCM_DEFINE (G_pixbuf_p, "pixbuf?", 1, 0, 0, (SCM x), "\
Return #t if the argument is a pixbuf. #f otherwise.")
{
    if (SCM_IS_A_P (x, pixbuf_tag))
        return SCM_BOOL_T;
    return SCM_BOOL_F;
}

SCM_DEFINE (G_pixbuf_shape, "pixbuf-shape", 1, 0, 0, (SCM x), "\
Return #t if the argument is a pixbuf. #f otherwise.")
{
    scm_assert_foreign_object_type (pixbuf_tag, x);
    pixbuf_t *pb = scm_foreign_object_ref (x, 0);
    return scm_list_2 (scm_from_size_t (pb->width), scm_from_size_t (pb->height));
}

SCM_DEFINE (G_pixbuf_copy, "copy-pixbuf", 1, 0, 0, (SCM x), "\
Return a newly allocated pixbuf which is a copy of the argument.")
{
    scm_assert_foreign_object_type (pixbuf_tag, x);
    pixbuf_t *input = scm_foreign_object_ref (x, 0);

    /* FIXME: Optimization?  Is scm_gc_malloc_pointerless aligned to a
     * dword boundary? */
    pixbuf_t *pbuf = scm_gc_malloc (sizeof (pixbuf_t), "pixbuf");
    pbuf->width = input->width;
    pbuf->height = input->height;
    pbuf->stride = input->stride;
    pbuf->data = scm_gc_malloc_pointerless (input->height * input->stride * sizeof(uint32_t), "pixbuf");
    memcpy (pbuf->data, input->data, input->height * input->stride * sizeof(uint32_t));
    return scm_make_foreign_object_1 (pixbuf_tag, pbuf);
}

SCM_DEFINE (G_pixbuf_equal_p, "pixbuf-equal?", 2, 0, 0, (SCM a, SCM b), "\
Return #t if the pixbuf arguments have the same size and image.")
{
    scm_assert_foreign_object_type (pixbuf_tag, a);
    scm_assert_foreign_object_type (pixbuf_tag, b);
    pixbuf_t *A = scm_foreign_object_ref (a, 0);
    pixbuf_t *B = scm_foreign_object_ref (b, 0);

    if ((A->width != B->width) || (A->height != B->height) || (A->stride != B->stride))
        return SCM_BOOL_F;
    if (memcmp (A->data, B->data, A->stride * A->height * sizeof (uint32_t)) == 0)
        return SCM_BOOL_T;
    return SCM_BOOL_F;
}

SCM_DEFINE (G_pixbuf_fill_x, "pixbuf-fill!", 2, 0, 0, (SCM x, SCM argb32), "\
Set every pixel of a pixbuf to the given ARGB32 colorref.")
{
    scm_assert_foreign_object_type (pixbuf_tag, x);
    SCM_ASSERT (scm_is_unsigned_integer (argb32, 0, 0xFFFFFFFF), argb32, SCM_ARG2, "pixbuf-fill!");
    pixbuf_t *pb = scm_foreign_object_ref (x, 0);
    uint32_t val = scm_to_uint32 (argb32);
    for (size_t j = 0; j < pb->height; j ++)
    {
        for (size_t i = 0; i < pb->width; i ++)
        {
            pb->data[j * pb->stride + i] = val;
        }
    }
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_pixbuf_set_x, "pixbuf-set!", 4, 0, 0, (SCM x, SCM i, SCM j, SCM argb32), "\
Set the specified pixel of a pixbuf to the given ARGB32 colorref.")
{
    scm_assert_foreign_object_type (pixbuf_tag, x);
    pixbuf_t *pb = scm_foreign_object_ref (x, 0);
    SCM_ASSERT (scm_is_unsigned_integer (i, 0, pb->width), i, SCM_ARG2, "pixbuf-set!");
    SCM_ASSERT (scm_is_unsigned_integer (j, 0, pb->height), j, SCM_ARG3, "pixbuf-set!");
    SCM_ASSERT (scm_is_unsigned_integer (argb32, 0, 0xFFFFFFFF), argb32, SCM_ARG4, "pixbuf-set!");
    
    uint32_t val = scm_to_uint32 (argb32);
    pb->data[scm_to_size_t (j) * pb->stride + scm_to_size_t (i)] = val;
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (g_pixbuf_copy_x, "pixbuf-copy!", 6, 2, 0, \
            (SCM source, SCM istart, SCM jstart, SCM isize, SCM jsize, SCM dest, SCM istart2, SCM jstart2), "\
Copy a region from a source pixbuf into a destination pixbuf, modifying the destination pixbuf.")
{
    scm_assert_foreign_object_type (pixbuf_tag, source);
    pixbuf_t *pb_source = scm_foreign_object_ref (source, 0);
    SCM_ASSERT (scm_is_unsigned_integer (istart, 0, pb_source->width), istart, SCM_ARG2, "pixbuf-copy!");
    SCM_ASSERT (scm_is_unsigned_integer (jstart, 0, pb_source->height), jstart, SCM_ARG3, "pixbuf-copy!");
    SCM_ASSERT (scm_is_unsigned_integer (isize, 1, pb_source->width), jstart, SCM_ARG4, "pixbuf-copy!");
    SCM_ASSERT (scm_is_unsigned_integer (jsize, 1, pb_source->height), jstart, SCM_ARG5, "pixbuf-copy!");
    scm_assert_foreign_object_type (pixbuf_tag, dest);
    pixbuf_t *pb_dest = scm_foreign_object_ref (dest, 0);
    SCM_ASSERT (scm_is_unsigned_integer (istart2, 0, pb_dest->width), istart, SCM_ARG7, "pixbuf-copy!");
    SCM_ASSERT (scm_is_unsigned_integer (jstart2, 0, pb_dest->height), jstart, 8, "pixbuf-copy!");

    size_t c_istart = scm_to_size_t (istart);
    size_t c_jstart = scm_to_size_t (jstart);
    size_t c_istart2 = scm_to_size_t (istart2);
    size_t c_jstart2 = scm_to_size_t (jstart2);
    size_t c_isize = scm_to_size_t (isize);
    size_t c_jsize = scm_to_size_t (jsize);
    if ((c_istart + c_isize > pb_source->width)
        || (c_jstart + c_jsize > pb_source->height))
        scm_misc_error ("pixbuf-copy!", "source region is invalid", SCM_EOL);
    else if ((c_istart2 + c_isize > pb_dest->width)
             || (c_jstart2 + c_jsize > pb_dest->width))
        scm_misc_error ("pixbuf-copy!", "destination region is invalid", SCM_EOL);
    else
    {
        for (size_t j = 0; j < c_jsize; j++)
        {
            memmove (&(pb_dest->data[  (c_jstart2 + j) * pb_dest->stride   + c_istart2]),
                     &(pb_source->data[( c_jstart + j) * pb_source->stride + c_istart]),
                     c_isize * sizeof(uint32_t));
                             
        }
    }
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_pixbuf_to_array, "pixbuf->array", 1, 0, 0, (SCM x), "\
Return a newly allocated array that contains the pixel contents of the argument.\n\
No stride padding is added to the array.  This is a slow procedure.")
{
    scm_assert_foreign_object_type (pixbuf_tag, x);
    pixbuf_t *pbuf = scm_foreign_object_ref (x, 0);

    SCM arr = scm_make_typed_array(scm_from_utf8_symbol("u32"),
                                   scm_from_uint32(0),
                                   scm_list_2 (scm_from_size_t (pbuf->width),
                                               scm_from_size_t (pbuf->height)));

    for (size_t j = 0; j < pbuf->height; j ++)
    {
        for (size_t i = 0; i < pbuf->width; i ++)
        {
            scm_array_set_x (arr,
                             scm_from_uint32 (pbuf->data[j * pbuf->stride + i]),
                             scm_list_2 (scm_from_size_t (i), scm_from_size_t (j)));
        }
    }
    scm_remember_upto_here_1 (x);
    return arr;
}

void
pixbuf_init_guile_procedures (void)
{
    pixbuf_tag = scm_make_foreign_object_type (scm_from_utf8_symbol ("pixbuf"),
                                               scm_list_1 (scm_from_utf8_symbol ("data")),
                                               NULL);
#include "pixbuf.x"
    scm_c_export ("make-pixbuf",
                  "make-pixbuf-from-image-file",
                  "pixbuf?",
                  "pixbuf-shape",
                  "copy-pixbuf",
                  "pixbuf-equal?",
                  "pixbuf-fill!",
                  "pixbuf-set!",
                  "pixbuf-copy!",
                  "pixbuf->array",
                  NULL);
}

#pragma GCC diagnostic pop

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
