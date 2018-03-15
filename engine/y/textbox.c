#include "textbox.h"
#include <pango/pangocairo.h>

/* from draw.c */
extern cairo_t *main_screen_context;

SCM textbox_tag;
SCM_GLOBAL_VARIABLE_INIT (G_textbox_var, "%textbox-var", SCM_BOOL_F);

#define ERR_MSG_STORE_LEN (1024)
const char err_msg_store[ERR_MSG_STORE_LEN];

static void
finalize_textbox (SCM x)
{
    textbox_t *tb = scm_foreign_object_ref (x, 0);
    g_object_unref (tb->layout);

    scm_foreign_object_set_x (x, 0, NULL);
    free (tb);
}

SCM_DEFINE (G_make_textbox, "make-textbox", 1, 0, 0, (SCM s_markup), "\
Given a string of text, generate rendered text.  The input text may\n\
contain Pango markup tags.")
{
    SCM_ASSERT_TYPE (scm_is_string (s_markup), s_markup, SCM_ARG1, "make-textbox", "string");
    if (scm_c_string_length (s_markup) > TEXTBOX_CODEPOINT_COUNT_MAX)
        scm_misc_error ("make-textbox", "input string is too long", SCM_EOL);
    
    /* Convert the scheme string containing markup into a C UTF-8
     * string. */
    size_t markup_len;
    char *markup = scm_to_utf8_stringn (s_markup, &markup_len);

    textbox_t *tb = malloc (sizeof (textbox_t));
    memset (tb, 0, sizeof (textbox_t));

    tb->layout = pango_cairo_create_layout (main_screen_context);

    /* Set the font. */
    PangoFontDescription *desc = pango_font_description_from_string("Serif 16");
    pango_layout_set_font_description (tb->layout, desc);
    pango_font_description_free (desc);

    /* Set up word wrapping. */
    // int width, height;
    // pango_layout_get_size (tb->layout, &width, &height);
    pango_layout_set_width (tb->layout, 640 * PANGO_SCALE);
    pango_layout_set_height (tb->layout, 480 * PANGO_SCALE);
    pango_layout_set_wrap (tb->layout, PANGO_WRAP_WORD_CHAR);

    /* Assign the text. */
    pango_layout_set_markup (tb->layout, markup, markup_len);

    free (markup);
    return scm_make_foreign_object_1 (textbox_tag, tb);
}

SCM_DEFINE(G_textbox_get_text, "textbox-get-text", 1, 0, 0, (SCM s_tb), "\
Returns a string containing the text in the textbox.")
{
    scm_assert_foreign_object_type (textbox_tag, s_tb);
    textbox_t *tb = scm_foreign_object_ref (s_tb, 0);
    return scm_from_utf8_string (pango_layout_get_text (tb->layout));
}

SCM_DEFINE(G_textbox_show, "textbox-show", 1, 0, 0, (SCM s_tb), "\
Set this textbox as the visible text.")
{
    scm_assert_foreign_object_type (textbox_tag, s_tb);
    scm_variable_set_x (G_textbox_var, s_tb);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(G_textbox_hide, "textbox-hide", 0, 0, 0, (void), "\
Hide the text.")
{
    scm_variable_set_x (G_textbox_var, SCM_BOOL_F);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(G_textbox_xy_to_index, "textbox-xy-to-index", 3, 0, 0, (SCM s_tb, SCM s_x, SCM s_y), "\
Converts a location in pixel coordinates to a codepoint index of a textbox's text.\n\
Returns #f if the position is not near a character, or a two-element\n\
list (index, trailing) otherwise.")
{
    textbox_t *tb = scm_foreign_object_ref (s_tb, 0);
    int x = scm_to_double (s_x) * PANGO_SCALE;
    int y = scm_to_double (s_y) * PANGO_SCALE;
    int index, trailing;
    gboolean ret = pango_layout_xy_to_index (tb->layout, x, y, &index, &trailing);
    if (!ret)
        return SCM_BOOL_F;

    /* The UTF-8 index needs to be converted into UTF32 index. */
    const char *str = pango_layout_get_text (tb->layout);
    char *p = str;
    int offset = 0;
    while (p - str < index)
    {
        p = g_utf8_next_char(p);
        offset++;
    }
    
    return scm_list_2 (scm_from_int (offset), scm_from_int (trailing));
    return SCM_BOOL_F;
}

int textbox_get_priority (SCM s_textbox)
{
    textbox_t *o = scm_foreign_object_ref (s_textbox, 0);
    return o->priority;
}

PangoLayout *textbox_get_layout (SCM s_textbox)
{
    textbox_t *o = scm_foreign_object_ref (s_textbox, 0);
    return o->layout;
}

void
textbox_init_guile_procedures (void)
{
    textbox_tag = scm_make_foreign_object_type (scm_from_utf8_symbol ("textbox"),
                                                scm_list_1 (scm_from_utf8_symbol ("data")),
                                                finalize_textbox);
    #include "textbox.x"
    scm_c_export ("%textbox-var",
                  "make-textbox",
                  "textbox-get-text",
                  "textbox-show",
                  "textbox-hide",
                  "textbox-xy-to-index",
                  NULL);
}


/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
