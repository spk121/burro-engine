#ifndef BURRO_CANVAS_VRAM_H
#define BURRO_CANVAS_VRAM_H

#include <libguile.h>
#include <gtk/gtk.h>

/* VRAM
 *
 * To avoid overloading memory, Burro Engine has strictly limited space
 * to store graphics and audio.  This system is called VRAM for 
 * historical and perverse reasons.
 *
 * When complete, there are 9 VRAM banks which can be used for
 * - 32bpp pixmaps   for plain pixmaps, or for spritesheets
 * - 32bpp tilemaps  for map-and-tile backgrounds
 * - Opus-compressed 48,000 Hz audio, which is about 8kB/sec
 *     + They can be stored in either 1MB or 64kB units
 * - Tune data (a MIDI-like format) at 16 B / note.
 *
 * There are four large banks, one medium bank, and 1 small bank.
 *
 *  BANK      RAW    GRAPHICS         OPUS AUDIO   SFX AUDIO      TUNE
 *  VRAM_A,   1MB,   512x512x4        ~8 minutes   64 x ~8 sec    64k notes
 *  VRAM_B,
 *  VRAM_C,
 *  VRAM_D,
 *  VRAM_E,   64kB   128x128x4        ~8 seconds   1 x ~8 sec     4k notes
 *  VRAM_F,   16kB   64x64x4                                      1k notes
 *  VRAM_G,
 *  VRAM_H,
 *  VRAM_I,
 *  VRAM_J
 *
 */

enum {
    VRAM_TYPE_RAW,
    VRAM_TYPE_IMAGE,		/* 32-bit image data */
    VRAM_TYPE_TILEMAP,		/* 32-bit maps for map-and-tile backgrounds */
    VRAM_TYPE_OPUS,		/* Complete Opus audio file */
    VRAM_TYPE_TUNE,		/* 16 bytes/note MIDI-like data */
    VRAM_N_TYPES
};

typedef struct vram_info_tag {
    int type;
    int width;			/* for IMAGE and TILEMAP */
    int height;			/* for IMAGE and TILEMAP */
    int size;			/* for OPUS or TUNE, length in bytes */
    char *filename;		/* the source of the contents */
} vram_info_t;

/* MAP AND TILE
 *
 * if a 512x512 IMAGE background is too small, one can make
 * a 1024x1024 map-and-tile background using a large VRAM
 * as a tile sheet, and a different VRAM as a MAP.
 *
 * VRAM_A,..D creates a map/tile background of 8k by 8k
 * VRAM_E is 2k x 2k
 * VRAM_F, etc is 1k by 1k
 *
 * For Map and Tile, the 512x512 IMAGE is a tilesheet
 * of 16x16 pixel tiles, forming a 32x32 tile grid
 *
 * The map data is 4 bytes per tile
 * - tilesheet column
 * - tilesheet row
 * - flags
 * - stuff
 */

/* TUNES
 * Each note is 16 bytes
 * 4 byte float start time
 * 4 byte float length
 * 4 byte float frequency
 * 4 byte of instrument_number + volume + flags
 */

/** Index of a VRAM bank. */
typedef enum {
    VRAM_NONE,
    VRAM_A,                     /**< 256k uint32, usually for main spritesheet  */
    VRAM_B,                     /**< 256k uint32, usually for sub spritesheet  */
    VRAM_C,                     /**< 256k uint32  */
    VRAM_D,                     /**< 256k uint32  */
    VRAM_E,                     /**< 64k uint32, usually for bg maps  */
    VRAM_F,                     /**< 4k uint32, usually for bg maps  */
    VRAM_G,                     /**< 4k uint32, usually for bg maps  */
    VRAM_H,                     /**< 4k uint32, usually for bg maps  */
    VRAM_I,                     /**< 4k uint32, usually for bg maps  */
    VRAM_J,                     /**< 4k uint32, usually for bg maps  */
    VRAM_COUNT,
} vram_bank_t;

////////////////////////////////////////////////////////////////
//

#define VRAM_NONE_U32_HEIGHT (0)
#define VRAM_NONE_U32_WIDTH (0)
#define VRAM_NONE_U32_SIZE (0)
#define VRAM_NONE_U32_OFFSET (0)
#define VRAM_NONE_U32_PTR (0)

////////////////////////////////////////////////////////////////
//
// VRAM ABCD: 4MB of storage

#define VRAM_A_U32_HEIGHT (512)
#define VRAM_A_U32_WIDTH (512)
#define VRAM_B_U32_HEIGHT (512)
#define VRAM_B_U32_WIDTH (512)
#define VRAM_C_U32_HEIGHT (512)
#define VRAM_C_U32_WIDTH (512)
#define VRAM_D_U32_HEIGHT (512)
#define VRAM_D_U32_WIDTH (512)

#define VRAM_A_U32_SIZE (VRAM_A_U32_HEIGHT*VRAM_A_U32_WIDTH)
#define VRAM_B_U32_SIZE (VRAM_B_U32_HEIGHT*VRAM_B_U32_WIDTH)
#define VRAM_C_U32_SIZE (VRAM_C_U32_HEIGHT*VRAM_C_U32_WIDTH)
#define VRAM_D_U32_SIZE (VRAM_D_U32_HEIGHT*VRAM_D_U32_WIDTH)
#define VRAM_ABCD_U32_SIZE \
  (VRAM_A_U32_SIZE + VRAM_B_U32_SIZE + VRAM_C_U32_SIZE + VRAM_D_U32_SIZE)

#define VRAM_A_U32_OFFSET (0)
#define VRAM_B_U32_OFFSET (VRAM_A_U32_OFFSET + VRAM_A_U32_SIZE)
#define VRAM_C_U32_OFFSET (VRAM_B_U32_OFFSET + VRAM_B_U32_SIZE)
#define VRAM_D_U32_OFFSET (VRAM_C_U32_OFFSET + VRAM_C_U32_SIZE)

extern  uint32_t vram_ABCD_store[VRAM_ABCD_U32_SIZE];

#define VRAM_A_U32_PTR    (vram_ABCD_store + VRAM_A_U32_OFFSET)
#define VRAM_B_U32_PTR    (vram_ABCD_store + VRAM_B_U32_OFFSET)
#define VRAM_C_U32_PTR    (vram_ABCD_store + VRAM_C_U32_OFFSET)
#define VRAM_D_U32_PTR    (vram_ABCD_store + VRAM_D_U32_OFFSET)

////////////////////////////////////////////////////////////////
//
// VRAM EFGHI

#define VRAM_E_U32_HEIGHT (256)
#define VRAM_E_U32_WIDTH (256)
#define VRAM_F_U32_HEIGHT (64)
#define VRAM_F_U32_WIDTH (64)
#define VRAM_G_U32_HEIGHT (64)
#define VRAM_G_U32_WIDTH (64)
#define VRAM_H_U32_HEIGHT (64)
#define VRAM_H_U32_WIDTH (64)
#define VRAM_I_U32_HEIGHT (64)
#define VRAM_I_U32_WIDTH (64)
#define VRAM_J_U32_HEIGHT (64)
#define VRAM_J_U32_WIDTH (64)

#define VRAM_E_U32_SIZE (VRAM_E_U32_HEIGHT*VRAM_E_U32_WIDTH)
#define VRAM_F_U32_SIZE (VRAM_F_U32_HEIGHT*VRAM_F_U32_WIDTH)
#define VRAM_G_U32_SIZE (VRAM_G_U32_HEIGHT*VRAM_G_U32_WIDTH)
#define VRAM_H_U32_SIZE (VRAM_H_U32_HEIGHT*VRAM_H_U32_WIDTH)
#define VRAM_I_U32_SIZE (VRAM_I_U32_HEIGHT*VRAM_I_U32_WIDTH)
#define VRAM_J_U32_SIZE (VRAM_J_U32_HEIGHT*VRAM_J_U32_WIDTH)
#define VRAM_EFGHIJ_U32_SIZE \
    (VRAM_E_U32_SIZE + VRAM_F_U32_SIZE + VRAM_G_U32_SIZE + VRAM_H_U32_SIZE \
     + VRAM_I_U32_SIZE + VRAM_J_U32_SIZE)

#define VRAM_E_U32_OFFSET (0)
#define VRAM_F_U32_OFFSET (VRAM_E_U32_OFFSET + VRAM_E_U32_SIZE)
#define VRAM_G_U32_OFFSET (VRAM_F_U32_OFFSET + VRAM_F_U32_SIZE)
#define VRAM_H_U32_OFFSET (VRAM_G_U32_OFFSET + VRAM_G_U32_SIZE)
#define VRAM_I_U32_OFFSET (VRAM_H_U32_OFFSET + VRAM_H_U32_SIZE)
#define VRAM_J_U32_OFFSET (VRAM_I_U32_OFFSET + VRAM_I_U32_SIZE)

extern uint32_t vram_EFGHIJ_store[VRAM_EFGHIJ_U32_SIZE];

#define VRAM_E_U32_PTR (vram_EFGHIJ_store + VRAM_E_U32_OFFSET)
#define VRAM_F_U32_PTR (vram_EFGHIJ_store + VRAM_F_U32_OFFSET)
#define VRAM_G_U32_PTR (vram_EFGHIJ_store + VRAM_G_U32_OFFSET)
#define VRAM_H_U32_PTR (vram_EFGHIJ_store + VRAM_H_U32_OFFSET)
#define VRAM_I_U32_PTR (vram_EFGHIJ_store + VRAM_I_U32_OFFSET)
#define VRAM_J_U32_PTR (vram_EFGHIJ_store + VRAM_J_U32_OFFSET)

////////////////////////////////////////////////////////////////

gboolean vram_validate_int_as_vram_bank_t (int x);

void vram_init (void);

const char *
vram_get_bank_name (vram_bank_t bank);

/** Return the size, in 32-bit words, of a VRAM bank.
 *  @param [in] bank
 *  @return size of bank of VRAM in 32-bit words
 */
int vram_get_u32_size (vram_bank_t bank);

int vram_get_u32_height (vram_bank_t bank);
int vram_get_u32_width (vram_bank_t bank);

/** Return a pointer to the beginning of the VRAM bank.
 *  @param [in] bank index
 *  @return a pointer to the VRAM bank
 */
uint32_t *vram_get_u32_ptr (vram_bank_t bank);

/** Zero-fill the contents of a VRAM bank.
 *  @param [in] bank index
 */
void vram_zero_bank (vram_bank_t bank);

////////////////////////////////////////////////////////////////
SCM _scm_from_vram_bank_t (vram_bank_t x);
vram_bank_t _scm_to_vram_bank_t (SCM x);
gboolean _scm_is_vram_bank_t (SCM x);

/** Register VRAM procedures with the script engine. */
void burro_canvas_vram_init_guile_procedures (void);

enum {
    VRAM_COLUMN_NAME,                /* The name of the bank, like "VRAM A" */
    VRAM_COLUMN_TYPE,
    VRAM_COLUMN_FILENAME,
    VRAM_COLUMN_SIZE,
    VRAM_N_COLUMNS
};

GtkListStore *vram_info_list_store_new();
void vram_info_list_store_update(GtkListStore *list_store);

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
