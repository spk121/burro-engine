/*-----------------------------------------------------------------------------

  pixbuf.h -- a Guile foreign object of ARGB32 pixels
  Copyright (C) 2018
  Michael L. Gran (spk121)

  GPL3+
  -----------------------------------------------------------------------------*/
/** @file pixbuf.h
    @brief unpacked pixel images

    This is a Guile object that stores a 2D array of ARGB32 values.  The
    array has a height, a width, and a stride.

*/

#ifndef BURRO_PIXBUF_H
#define BURRO_PIXBUF_H

typedef struct pixbuf_tag
{
    size_t width;
    size_t height;
    size_t stride;
    uint32_t *data;
} pixbuf_t;


/** Creates a new pixbuf with the give dimensions and constant value.
    Guile API name is 'make-pixbuf'.
    @param width
        width in pixels
    @param height
        height in pixels
    @param argb32
        a 32-bit integer ARGB32 value to which each pixel will be set
*/
SCM G_pixbuf_make (SCM isize, SCM jsize, SCM argb32);

/** Creates a new pixbuf from a filename-specified PNG or JPEG.
    The contents of the image must be RGB32 or ARGB32.
    @param filename
        A filename, as a string.  If it is a relative path,
        it will be searched for in the usual places.
*/        
SCM G_pixbuf_make_from_image_file (SCM filename);

/** Return #t if X is a pixbuf.
    @param x
       any scheme object
*/
SCM G_pixbuf_p (SCM x);

/** Return the 2-element list (isize jsize) describing the dimensions of the pixbuf.
    @param x
        a pixbuf
*/
SCM G_pixbuf_shape (SCM x);

/** Create a newly allocated copy of a pixbuf
    @param x
        a pixbuf
*/
SCM G_pixbuf_copy (SCM x);

/** Compare two pixbufs to see if they have the same size and contents.
    @param a
        a pixbuf
    @param b
        a pixbuf
*/
SCM G_pixbuf_equal (SCM a, SCM b);

/** Set the contents of a pixbuf to a given color, without changing the dimensions.
    @param dest
        a pixbuf
    @param argb32
        a 32-bit colorval
*/
SCM G_pixbuf_fill_x (SCM dest, SCM argb32);

/** Set a single pixel of a pixbuf to a given color.
    @param x
        a pixbuf
    @param i
        the column of a pixel
    @param j
        the row of a pixel
    @param argb32
        a 32-bit colorval
*/
SCM G_pixbuf_set_x (SCM x, SCM i, SCM j, SCM argb32);

/** Copy a rectangular region from one pixbuf into another.
    @param source
       a pixbuf
    @param istart
       an integer. the starting column from the source pixbuf
    @param jstart
       an integer. the starting row from the source pixbuf
    @param isize
       an integer. the number of columns of the region
    @param jsize
       An integer.  The number of rows.
    @param dest
       The destination pixbuf
    @param istart2
       An integer. The starting column on the destination pixbuf.
    @param jstart2
       An integer. The starting row on the destination pixbuf.
*/
SCM G_pixbuf_copy_x (SCM source, SCM istart, SCM jstart, SCM isize, SCM jsize, SCM dest, SCM istart2, SCM jstart2);

/** Return a newly allocated array that has the same row count, column
    count, and contents as X.  No stride padding is added to the array.
    This is a slow operation; not good for real-time use.
    @param x
        a pixbuf
*/
SCM G_pixbuf_to_array (SCM x);

extern SCM pixbuf_tag;

cairo_surface_t* pixbuf_render_to_new_cairo_surface (SCM pixbuf, bool colorswap, double brightness);
cairo_surface_t* pixbuf_render_region_to_new_cairo_surface (SCM pixbuf, size_t i_start, size_t j_start, size_t i_size, size_t j_size,
                                                            bool colorswap, double brightness);
void pixbuf_copy_region_to_cairo_surface (SCM pixbuf, size_t i_start, size_t j_start, size_t i_size, size_t j_size,
                                          cairo_surface_t *surf, size_t i2_start, size_t j2_start,
                                          bool colorswap, double brightness);


void pixbuf_init_guile_procedures (void);

#endif
