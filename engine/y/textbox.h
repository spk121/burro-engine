/** @file textbox.h
    Control and rendering of proper text

*/

#ifndef BURRO_TEXTBOX_H
#define BURRO_TEXTBOX_H

#include "../x/xguile.h"
#include "../x/xcairo.h"
#include "../x/xpango.h"

/** The maximum length of text, in codepoints, that a textbox may
    hold.  Approximately one screenful of data. */
#define TEXTBOX_CODEPOINT_COUNT_MAX (80*40)

/** A reference to the TEXTBOX foreign object type. */
extern SCM textbox_tag;

/** The Guile global that hold the current textbox. */
extern SCM G_textbox_var;

// The layout is created from cairo_t *main_screen_context.
// 
struct _textbox_t
{
    int priority;
    
    // This layout object is created with
    // pango_cairo_create_layout(main_screen_context).  It needs to be
    // updated if the cairo context changes its transform.

    // pango_layout_set_markup set the text

    // pango_cairo_show_layout renders it.
    PangoLayout *layout;
#if 0
    // A run of UTF-8 encoded text possibly including Pango GMarkup
    // tags.

    // We need to create a new attribute for Pango markup that
    // denotes a clickable region.
    // pango_attr_type_region();
    PangoAttrType *hotspot_attr_type;

    // pango_parse_markup() parsed marked up text into
    // these things.
    const char *markup_text;
    int length;
    gunichar accel_marker;
    PangoAttrList *attr_list;
    char *text;
    gunichar accel_char;
    GError *error;
#endif
};  

typedef struct _textbox_t textbox_t;

int textbox_get_priority (SCM textbox);
PangoLayout *textbox_get_layout (SCM textbox);

void textbox_init_guile_procedures (void);

#endif

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
