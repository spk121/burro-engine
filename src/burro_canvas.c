#include <gtk/gtk.h>

#include "burro_canvas.h"
// #include "burro_canvas_background.h"
// #include "burro_canvas_object.h"
// #include "burro_console.h"

#define BURRO_CANVAS_WIDTH  512
#define BURRO_CANVAS_HEIGHT 384
#define BURRO_SPRITESHEET_WIDTH 512
#define BURRO_SPRITESHEET_HEIGHT 512
#define BURRO_SPRITESHEET_MEMSIZE (512*512*4)
#define BURRO_TILE_WIDTH 16
#define BURRO_TILE_HEIGHT 16
#define BURRO_FONT_WIDTH 8
#define BURRO_FONT_HEIGHT 13
#define BURRO_CONSOLE_COLS (BURRO_CANVAS_WIDTH/BURRO_FONT_WIDTH)
#define BURRO_CONSOLE_ROWS (BURRO_CANVAS_HEIGHT/BURRO_FONT_HEIGHT)
#define BURRO_BACKGROUNDS_COUNT 4

/** Information about a single layer of a multi-layer background. */
typedef struct
{
    /** BG is displayed when true */
    gboolean enable;

    /** the "user" or screen location of the rotation center of the
     * background */
    double x, y;

    /** the "device" location of the rotation center of the
     * background */
    double rotation_center_x, rotation_center_y;

    /** the expansion factor of the background: 1.0 = 1 pixel per
     * pixel */
    double expansion;

    /** the rotation angle of the background about its rotation
     * center, in radians */
    double rotation;

    /** when true, there are changes to the background that need
     *  to be rendered. */
    double dirty;

    gboolean colorswap;

    /** Factor to adjust the brightness or darkness of the background.
        Default is 1.0.  When 0.0, all background colors are
        black.  */
    double brightness;

    int width, height;
    
    GArray *data;
    
    /** Cache for the Cairo renderings of background layers.  */
    cairo_surface_t *surface;
} bg_t;

typedef struct
{
    /** priority aka z-level. 0 to 3 where 0 is foreground */
    int priority;

    /** location of top-left corner of sprite in sprite sheet */
    int sheet_i, sheet_j;

    /** the "user" or screen location of the rotation center of the sprite */
    double x, y;

    /** the "device" location of the rotation center, aka, it bitmap row and column of its hotspot*/
    double rotation_center_x, rotation_center_y;

    /** the expansion factor of the sprite: 1.0 = 1 pixel per pixel */
    double expansion;

    /** the rotation angle of the sprite about its rotation center, in radians */
    double rotation;

    /** Flip object vertically or horizontally */
    gboolean hflip;
    gboolean vflip;

    /* Invert color of object */
    gboolean colorswap;

    /** Adjust color of object: from  0.0 to 1.0 */
    double brightness;
    
    /** size of sprite in pixels */
    int width, height;

    /* pixel data */
    GArray *data;
    
    /** Cache for the Cairo renderings of background layers.  */
    cairo_surface_t *surf;
} obj_t;


typedef struct 
{
  GObject parent;
  gint row, col;
  guint16 rendition;
    guint32 cells[BURRO_CONSOLE_ROWS * BURRO_CONSOLE_COLS];
  gboolean cursor_visible;
  GTimer *timer;
} console_t;



struct _BurroCanvas
{
    GtkDrawingArea parent;

    guint32 backdrop_color;
#if 0
    /* Either a single bitmap or a tilesheet of 16x16 pixel
     * tiles. ARGB32. */
    guint32 bg_sheet[512*512];

    /* Whether the bg is visible */
    gboolean bg_enable;
    /* The width and height of the background, in pixels. */
    double bg_width, bg_height;
    /* The screen location of rotation center of the background, in
     * pixels, where 0,0 is the top-left of the sceen. */
    double bg_x, bg_y;
    /* The rotation center of the background. */
    double bg_rotation_center_x, bg_rotation_center_y;
    /* The rotation angle of the background about its rotation center,
     * in radians. */
    double bg_rotation;
    /* The scaling of the background, where 1.0 is standard. */
    double bg_expansion;
    /** The rotation angle of the background about its rotation
     * center, in radians */
    double bg_rotation;

    /* When true, invert the bg colors. */
    gboolean bg_colorswap;
    /** Factor to adjust the brightness or darkness of the background.
        Default is 1.0.  When 0.0, all background colors are
        black.  */
    double bg_brightness;

    /* When true, we construct a background using 16x16-pixel tiles
     * from the bg_sheet, according to the information in the bg_map
     * parameters.  When false, we construct the background using the
     * bg_sheet as a single image, according to the information in the
     * bg_bmp parameters. */
    gboolean bg_is_map;

    /* 32-bit tile index parameters.
     * - 8-bits tile i
     * - 8-bits tile j
     * - 16-bits of TBD
     */
    guint32 bg_map[64*64];

    /* The row and column, in the bg_sheet, of the top-left corner
     * of the bitmap.  The bg_width and bg_height parameters will
     * be used to determine the bottom-right corner. */
    guint bg_sheet_i, bg_sheet_j;

    /* Probably a sprite sheet of various sized sprites. ARGB32. */
    guint32 obj_sheet[512*512];

    /* 1 byte: spritesheet i where 1 = 2pixels
     * 1 byte: spritesheet j where 1 = 2pixels
     * 1 byte: spritesheet width where 1 = 2 pixels
     * 1 byte: spritesheet height where 1 = 2 pixels
     * 1 byte: rotation, where 1 = pi/128
     * 1 byte: scaling, where 1 = 1/8, but, 0 = 1.0
     * 1 byte signed: brightness = -127 is dark, 0, is normal, 127 is bright
     * 1 byte: VISIBLE_FLAG, HFLIP_FLAG, VFLIP_FLAG, COLORSWAP_FLAG
     */
    guint64 obj_oam[128];

    int width, height;
    
    GArray *data;
    
    /** Cache for the Cairo renderings of background layers.  */
    cairo_surface_t *surface;
    
    bg_t bg[4];
    GHashTable *obj;
    // textbox_t tb;
    console_t console;
#endif
};

G_DEFINE_TYPE(BurroCanvas, burro_canvas, GTK_TYPE_DRAWING_AREA);

static void
burro_canvas_init (BurroCanvas *win)
{
    win->backdrop_color = 0x000000;

    gtk_widget_set_events (GTK_WIDGET(win), GDK_EXPOSURE_MASK
                           | GDK_LEAVE_NOTIFY_MASK
                           | GDK_BUTTON_PRESS_MASK
                           | GDK_POINTER_MOTION_MASK
                           | GDK_POINTER_MOTION_HINT_MASK);
    gtk_widget_set_size_request(GTK_WIDGET(win), BURRO_CANVAS_WIDTH, BURRO_CANVAS_HEIGHT);
}

static void
burro_canvas_dispose (GObject *object)
{
    BurroCanvas *win;

    win = BURRO_CANVAS (object);

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

void
burro_canvas_backdrop_get_color_rgb (BurroCanvas *canvas, double *r, double *g, double *b)
{
    uint32_t c32 = canvas->backdrop_color;
    *r = ((double)((c32 & 0x00ff0000) >> 16)) / 255.0;
    *g = ((double)((c32 & 0x0000ff00) >> 8)) / 255.0;
    *b = ((double)((c32 & 0x000000ff))) / 255.0;
}

void burro_canvas_backdrop_set_color (BurroCanvas *canvas, uint32_t color)
{
    canvas->backdrop_color = color;
}

gboolean
burro_canvas_draw_cb (GtkWidget *widget,
                      cairo_t *cr,
                      gpointer data)
{
    BurroCanvas *canvas = BURRO_CANVAS (widget);
    double r = 0.0, g = 0.0, b = 0.0;

    // Layer 1: a backdrop of a single color
    burro_canvas_backdrop_get_color_rgb (canvas, &r, &g, &b);
    cairo_set_source_rgb (cr, r, g, b);
    cairo_paint(cr);


end_draw:
    return FALSE;
}

gboolean
burro_canvas_button_press_event_cb (GtkWidget *widget,
                                    GdkEvent *event,
                                    gpointer user_data)
{
    BurroCanvas *canvas = BURRO_CANVAS(widget);
    return FALSE;
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

