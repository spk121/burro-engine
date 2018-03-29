#include <gtk/gtk.h>
#include <libguile.h>

#include "burro_canvas.h"
struct _BurroCanvas
{
    GtkDrawingArea parent;
    guint32        backdrop;
    gboolean       blank_flag;
    gboolean       colorswap_flag;
    gdouble        brightness;

    cairo_t *context;
    cairo_surface_t *surface;
    gboolean dirty;
};

static void draw ();

G_DEFINE_TYPE(BurroCanvas, burro_canvas, GTK_TYPE_DRAWING_AREA);

BurroCanvas *canvas_cur = NULL;

static gboolean
signal_draw (GtkWidget *widget,
                cairo_t *cr,
                gpointer data)
{
    BurroCanvas *canvas = BURRO_CANVAS(widget);
    
    cairo_set_source_surface (cr, canvas->surface, 0, 0);
    cairo_paint (cr);
    return FALSE;
}

static int
tick (GtkWidget *widget, GdkFrameClock *frame_clock, void *user_data)
{
    if (gtk_widget_is_visible (widget))
    {
        if (canvas_cur->dirty)
        {
            // Draw it up on a backbuffer
            draw();

            // Invalidate the widget so that the "draw" signal get
            // called, which is where we actually blit it to the
            // widget.
            gdk_window_invalidate_rect (gtk_widget_get_window (widget), NULL, FALSE);
            
            canvas_cur->dirty = FALSE;
        }
    }
    else
    {
        //audio_pause ();
        // Figure out a way to sleep until the next gtk event comes in
    }
    return TRUE;
}

static void draw_backdrop_color ()
{
    double r = 0.0, g = 0.0, b = 0.0;
    guint32 c32 = canvas_cur->backdrop;
    r = ((double)((c32 & 0x00ff0000) >> 16)) / 255.0;
    g = ((double)((c32 & 0x0000ff00) >> 8)) / 255.0;
    b = ((double)((c32 & 0x000000ff))) / 255.0;
    if (canvas_cur->colorswap_flag)
    {
        double tmp = r;
        r = b;
        b = tmp;
    }
    r = CLAMP(r * canvas_cur->brightness, 0.0, 1.0); 
    g = CLAMP(g * canvas_cur->brightness, 0.0, 1.0); 
    b = CLAMP(b * canvas_cur->brightness, 0.0, 1.0); 
    cairo_set_source_rgb (canvas_cur->context, r, g, b);
    cairo_paint (canvas_cur->context);
}

static void draw ()
{
    draw_backdrop_color ();
    if (canvas_cur->blank_flag)
        goto end_draw;
#if 0
    for (int priority = PRIORITY_COUNT - 1; priority >= 0; priority --)
    {
        for (int layer = BG_MAIN_3; layer >= BG_MAIN_0; layer --)
        {
            if (bg_is_shown (layer) && bg_get_priority (layer) == priority)
                draw_background_layer (layer);
        }
        SCM obj_display_list = scm_variable_ref(G_obj_display_list);
        for (int id = scm_to_int (scm_length (obj_display_list)) - 1; id >= 0; id --)
        {
            SCM obj = scm_list_ref (obj_display_list, scm_from_int (id));
            if (obj_get_priority (obj) == priority)
                draw_obj (obj);
        }
        SCM textbox = scm_variable_ref (G_textbox_var);
        if (scm_is_true(textbox) && textbox_get_priority (textbox) == priority)
            draw_textbox (textbox);
    }
#endif
end_draw:

    //if (console_is_visible ())
    //    draw_console_layer ();
    cairo_surface_mark_dirty (canvas_cur->surface);
}


static void
burro_canvas_init (BurroCanvas *win)
{
    gtk_widget_set_events (GTK_WIDGET(win),
                           GDK_EXPOSURE_MASK
                           | GDK_LEAVE_NOTIFY_MASK
                           | GDK_BUTTON_PRESS_MASK
                           | GDK_POINTER_MOTION_MASK
                           | GDK_POINTER_MOTION_HINT_MASK);
    gtk_widget_set_size_request(GTK_WIDGET(win), BURRO_CANVAS_WIDTH, BURRO_CANVAS_HEIGHT);

    win->blank_flag = FALSE;
    win->colorswap_flag = FALSE;
    win->brightness = 1.0;

    win->surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                               BURRO_CANVAS_WIDTH, BURRO_CANVAS_HEIGHT);
    win->context = cairo_create (win->surface);
    cairo_set_antialias (win->context, CAIRO_ANTIALIAS_NONE);

    g_signal_connect (G_OBJECT (win), "draw", G_CALLBACK (signal_draw), NULL);

    gtk_widget_add_tick_callback (GTK_WIDGET(win), tick, NULL, NULL);

    win->dirty = TRUE;
    canvas_cur = win;
}

static void
burro_canvas_dispose (GObject *object)
{
    BurroCanvas *win;

    win = BURRO_CANVAS (object);
    if (win->context)
    {
        cairo_destroy (win->context);
        win->context = NULL;
    }
    if (win->surface)
    {
        cairo_surface_destroy (win->surface);
        win->surface = NULL;
    }
    G_OBJECT_CLASS (burro_canvas_parent_class)->dispose (object);
}

static void
burro_canvas_class_init (BurroCanvasClass *class)
{
    
}

BurroCanvas *
burro_canvas_new ()
{
    return g_object_new (BURRO_TYPE_CANVAS, "canvas", NULL);
}

SCM_DEFINE(G_burro_canvas_set_blank, "set-blank", 1, 0, 0, (SCM flag), "\
Given a FLAG, this sets the canvas's blank parameter.  When blank is\n\
#t, the canvas is drawn.  When it is #f, it is just the background\n\
color.")
{
    g_return_val_if_fail (canvas_cur != NULL, SCM_UNSPECIFIED);
    
    if (scm_is_true(flag))
        canvas_cur->blank_flag = TRUE;
    else
        canvas_cur->blank_flag = FALSE;

    canvas_cur->dirty = TRUE;
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(G_burro_canvas_get_blank, "get-blank", 0, 0, 0, (void), "\
Returns the canvas's blank parameter.")
{
    g_return_val_if_fail (canvas_cur != NULL, SCM_UNSPECIFIED);

    return scm_from_bool (canvas_cur->blank_flag);
}

SCM_DEFINE(G_burro_canvas_set_colorswap, "set-colorswap", 1, 0, 0, (SCM flag), "\
Given a FLAG, this sets the canvas's colorswap parameter.  When\n\
colorswap is #t, the canvas is drawn with red and blue swapped.  When\n\
it is #f, is is draw normally.")
    
{
    g_return_val_if_fail (canvas_cur != NULL, SCM_UNSPECIFIED);
    
    if (scm_is_true(flag))
        canvas_cur->colorswap_flag = TRUE;
    else
        canvas_cur->colorswap_flag = FALSE;
    canvas_cur->dirty = TRUE;
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(G_burro_canvas_get_colorswap, "get-colorswap", 0, 0, 0, (void), "\
Returns the canvas's colorswap parameter.")
{
    g_return_val_if_fail (canvas_cur != NULL, SCM_UNSPECIFIED);

    return scm_from_bool (canvas_cur->colorswap_flag);
}

SCM_DEFINE(G_burro_canvas_set_brightness, "set-brightness", 1, 0, 0, (SCM val), "\
This adjusts the brightness of the canvas. Useful values are between\n\
0.1 (dark) and about 2.0 (probably saturated), where 1.0 is normal\n\
brightness.")
{
    g_return_val_if_fail (canvas_cur != NULL, SCM_UNSPECIFIED);

    if (scm_is_real (val))
    {
        gdouble brightness = scm_to_double (val);
        if (brightness < 0.0)
            brightness = 0.0;
        canvas_cur->brightness = brightness;
    }
    canvas_cur->dirty = TRUE;
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(G_burro_canvas_get_brightness, "get-brightness", 0, 0, 0, (void), "\
Returns the canvas's brightness parameter.")
{
    g_return_val_if_fail (canvas_cur != NULL, SCM_UNSPECIFIED);
    
    return scm_from_double (canvas_cur->brightness);
}

SCM_DEFINE(G_burro_canvas_set_backdrop, "set-backdrop", 1, 0, 0, (SCM color), "\
Given a 32-bit RGB colorval, this sets the canvas backdrop to that color.")
{
    g_return_val_if_fail (canvas_cur != NULL, SCM_UNSPECIFIED);

    canvas_cur->backdrop = scm_to_uint32 (color);
    canvas_cur->dirty = TRUE;
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(G_burro_canvas_get_backdrop, "get-backdrop", 0, 0, 0, (void), "\
Returns a 32-bit RGB color, which is the canvas backdrop color.")
{
    g_return_val_if_fail (canvas_cur != NULL, SCM_UNSPECIFIED);
    
    return scm_from_uint32 (canvas_cur->backdrop);
}

void
burro_canvas_init_guile_procedures ()
{
    burro_canvas_vram_init_guile_procedures ();
    
#include "burro_canvas.x"
    scm_c_export ("set-blank",
                  "get-blank",
                  "set-colorswap",
                  "get-colorswap",
                  "set-brightness",
                  "get-brightness",
                  "set-backdrop",
                  "get-backdrop",
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

