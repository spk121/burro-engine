#include <gtk/gtk.h>
#include "burro_console.h"

#ifdef SMALL_FONT
# include "6x9.h"
typedef glyph_fixed6x9_row_t glyph_row_t;
#define FIXED_COUNT (FIXED6x9_COUNT)
#define fixed_glyphs (fixed6x9_glyphs)
#define FIXED_MAXWIDTH (FIXED6x9_MAXWIDTH)
#define FIXED_MAXHEIGHT (FIXED6x9_MAXHEIGHT)
#else
# include "8x13.h"
typedef glyph_fixed8x13_row_t glyph_row_t;
#define FIXED_COUNT (FIXED8x13_COUNT)
#define FIXED_MAXWIDTH (FIXED8x13_MAXWIDTH)
#define FIXED_MAXHEIGHT (FIXED8x13_MAXHEIGHT)
#define fixed_glyphs (fixed8x13_glyphs)
#endif

// COLOR takes 4 bits
// Note that  DEFAULT has different meanings in the
// foreground and background cases.
#define COLOR_FG_DEFAULT        0b000000000000000
#define COLOR_FG_BLACK          0b000000000000001
#define COLOR_FG_RED            0b000000000000010
#define COLOR_FG_GREEN          0b000000000000011
#define COLOR_FG_YELLOW         0b000000000000100
#define COLOR_FG_BLUE           0b000000000000101
#define COLOR_FG_MAGENTA        0b000000000000110
#define COLOR_FG_CYAN           0b000000000000111
#define COLOR_FG_WHITE          0b000000000001000
#define COLOR_FG_TRANSPARENT    0b000000000001001
#define COLOR_FG_MASK           0b000000000001111
#define COLOR_FG_OFFSET         0

#define COLOR_BG_DEFAULT        0b000000000000000
#define COLOR_BG_BLACK          0b000000000010000
#define COLOR_BG_RED            0b000000000100000
#define COLOR_BG_GREEN          0b000000000110000
#define COLOR_BG_YELLOW         0b000000001000000
#define COLOR_BG_BLUE           0b000000001010000
#define COLOR_BG_MAGENTA        0b000000001100000
#define COLOR_BG_CYAN           0b000000001110000
#define COLOR_BG_WHITE          0b000000010000000
#define COLOR_BG_TRANSPARENT    0b000000010010000
#define COLOR_BG_MASK           0b000000011110000
#define COLOR_BG_OFFSET         4

#define INTENSITY_NORMAL        0b000000000000000
#define INTENSITY_FAINT         0b000000100000000
#define INTENSITY_BOLD          0b000001000000000
#define INTENSITY_MASK          0b000001100000000
#define INTENSITY_OFFSET        8

#define POLARITY_POSITIVE       0b000000000000000
#define POLARITY_NEGATIVE       0b000010000000000
#define POLARITY_MASK           0b000010000000000
#define POLARITY_OFFSET         10

#define BLINK_NONE              0b000000000000000
#define BLINK_SLOW              0b000100000000000
#define BLINK_FAST              0b001000000000000
#define BLINK_MASK              0b001100000000000
#define BLINK_OFFSET            11

#define UNDERLINE_NONE          0b000000000000000
#define UNDERLINE_SINGLY        0b010000000000000
#define UNDERLINE_DOUBLY        0b100000000000000
#define UNDERLINE_MASK          0b110000000000000
#define UNDERLINE_OFFSET        13

#define COMPOSE(render,codepoint) (((uint32_t)(render) << 16)|(codepoint))
#define RENDERING(x) ((uint16_t)(((x) & 0xFFFF0000) >> 16))
#define CODEPOINT(x) ((uint16_t)((x) & 0x0000FFFF))

#define NUM_COLORS 10
#define NUM_INTENSITIES 3
#define FAST_BLINK_TIME 300 /* milliseconds */
#define SLOW_BLINK_TIME 500 /* milliseconds */
#define TAB 8       /* spaces per tab */
#define VTAB 6      /* lines per vtab */

static const uint32_t fg_palette[NUM_COLORS *
                                 NUM_INTENSITIES] = {
    /*                   normal     faint        bold        */
    /* default     */ 0xffcccc88, 0xff888888, 0xffffff88,
    /* black       */ 0xff000000, 0xff000000, 0xff000000,
    /* red         */ 0xffcc0000, 0xff880000, 0xffff0000,
    /* green       */ 0xff00cc00, 0xff008800, 0xff00ff00,
    /* yellow      */ 0xffcccc00, 0xff888800, 0xffffff00,
    /* blue        */ 0xff0000cc, 0xff000088, 0xff0000ff,
    /* magenta     */ 0xffcc00cc, 0xff880088, 0xffff00ff,
    /* cyan        */ 0xff00cccc, 0xff008888, 0xff00ffff,
    /* white       */ 0xffcccccc, 0xff888888, 0xffffffff,
    /* transparent */ 0x00000000, 0x00000000, 0x00000000,
};

static const uint32_t bg_palette[NUM_COLORS] = {
    /*                   background        */
    /* default     */ 0x00000000,
    /* black       */ 0xff000000,
    /* red         */ 0xffcc0000,
    /* green       */ 0xff00cc00,
    /* yellow      */ 0xffcccc00,
    /* blue        */ 0xff0000cc,
    /* magenta     */ 0xffcc00cc,
    /* cyan        */ 0xff00cccc,
    /* white       */ 0xffcccccc,
    /* transparent */ 0x00000000,
};


struct _BurroConsole
{
  GObject parent;
  gint row, col;
  guint16 rendition;
  guint32 cells[BURRO_CONSOLE_ROWS * BURRO_CONSOLE_COLS]
  gboolean cursor_visible;
  GTimer *timer;
};

G_DEFINE_TYPE(BurroConsole, burro_consloe, G_TYPE_OBJECT);

static void
burro_console_init (BurroConsole *console)
{
  console->row = 0;
  console->col = 0;
  console->rendition = 0;
  memset (console->cells, 0, sizeof (console_cells));
  cursor_visible = TRUE;
  console->timer = g_timer_new();
}

static void
burro_console_class_init (BurroConsoleClass *class)
{
  GObjectClass *object_class = G_OBJECT_CLASS (class);

  object_class->dispose = burro_console_dispose;
  object_class->finalize = burro_console_finalize;
}

static void
burro_console_dispose (GObject *gobject)
{
  BurroConsole *console = BURRO_CONSOLE(gobject);

  // Free all referenced types here
  if (console->timer)
    {
      g_timer_destroy (console->timer);
      console->timer = NULL;
    }
  
  G_OBJECT_CLASS (burro_console_parent_class)->dispose (gobject);
}

static void
burro_console_finalize (GObject  *gobject);
{
  BurroConsole *console = BURRO_CONSOLE(gobject);

  // Free all allocated memory here.

  G_OBJECT_CLASS (burro_console_parent_class)->finalize (gobject);
}

static void
burro_console_set_rendition (BurroConsole *console, guint16 val, guint16 mask)
{
  console->rendition &= ~mask;
  console->rendition |= (val & mask);
}

void
