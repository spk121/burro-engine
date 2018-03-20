 #include <stdint.h>
#include <stdbool.h>
#include "../x.h"
#include "console.h"
#include "sheet.h"
#include "matrix.h"
#include "vram.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-conversion"

const char
sheet_index_name[SHEET_MAIN_OBJ + 1][16] = {
    [SHEET_MAIN_BG] = "SHEET_MAIN_BG",
    [SHEET_MAIN_OBJ] = "SHEET_MAIN_OBJ",
};

sheet_t sheets[SHEET_COUNT];

static const char *sheet_get_index_name (sheet_index_t index);


void
sheet_init ()
{
    memset(sheets, 0, sizeof(sheets));
}

void
sheet_assign_memory (sheet_index_t index, matrix_size_t size,
                     vram_bank_t bank)
{
    sheets[index].bank = bank;
    sheets[index].size = size;
    matrix_attach_to_vram(size, bank, &(sheets[index].storage), &(sheets[index].data));
}

int
sheet_get_height (sheet_index_t index)
{
    return matrix_get_height(sheets[index].size);
}

int
sheet_get_width (sheet_index_t index)
{
    return matrix_get_width(sheets[index].size);
}

int
sheet_get_height_in_tiles (sheet_index_t index)
{
    return matrix_get_height(sheets[index].size) / TILE_HEIGHT;
}

int
sheet_get_width_in_tiles (sheet_index_t index)
{
    return matrix_get_width(sheets[index].size) / TILE_WIDTH;
}

int
sheet_get_u32_size (sheet_index_t index)
{
    return matrix_get_u32_size(sheets[index].size);
} 

uint32_t *
sheet_get_u32_storage (sheet_index_t index)
{
    return sheets[index].storage;
}

uint32_t **
sheet_get_u32_data (sheet_index_t index)
{
    return sheets[index].data;
}

void sheet_set_data_from_file (sheet_index_t id, const char *filename)
{
    char *path = xg_find_data_file (filename);
    g_return_if_fail (path != NULL);
    GdkPixbuf *pb = xgdk_pixbuf_new_from_file (path);
    g_return_if_fail (pb != NULL);
    if (xgdk_pixbuf_is_argb32 (pb) == false)
    {
        xg_object_unref (pb);
        g_critical ("failed to load %s as an ARGB32 pixbuf", path);
        g_free (path);
    }
    else
    {
        int img_width, img_height, img_stride;
        int sheets_width, sheets_height;
        int width, height;

        xgdk_pixbuf_get_width_height_stride (pb,
                                            &img_width, &img_height,
                                            &img_stride);
        uint32_t *img_store = xgdk_pixbuf_get_argb32_pixels (pb);

        sheets_width = sheet_get_width(sheets[id].size);
        sheets_height = sheet_get_height(sheets[id].size);

        width = MIN(img_width, sheets_width);
        height = MIN(img_height, sheets_height);
        
        for (unsigned j = 0; j < height; j ++)
        {
            for (unsigned i = 0; i < width ; i ++)
            {
                uint32_t val = img_store[j * img_stride + i];
                // Convert from GDKPixbuf ABGR to Cairo ARGB
                val = (val & 0xFF00FF00) | ((val >> 16) & 0xFF) | ((val & 0xFF) << 16);

                // Convert from GDK un-premultiplied alpha to Cairo pre-multiplied alpha
                unsigned a = val >> 24;
                unsigned r = (((val >> 16) & 0xFF) * a) / 256;
                unsigned g = (((val >> 8) & 0xFF) * a) / 256;
                unsigned b = (((val >> 0) & 0xFF) * a) / 256;

                sheets[id].data[j][i] = a << 24 | r << 16 | g << 8 | b;
            }
        }
        g_debug ("loaded pixbuf %s as %s", path, sheet_get_index_name (id));
        g_free (path);
        g_object_unref (pb);
    }
}

////////////////////////////////////////////////////////////////
bool sheet_validate_int_as_sheet_index_t (int x)
{
    return (x >= (int) SHEET_MAIN_BG && x <= (int) SHEET_MAIN_OBJ);
}

bool sheet_validate_sheet_index_t (sheet_index_t x)
{
    return (x >= SHEET_MAIN_BG && x <= SHEET_MAIN_OBJ);
}

SCM _scm_from_sheet_index_t (sheet_index_t x)
{
    return scm_from_int ((int) x);
}

sheet_index_t _scm_to_sheet_index_t (SCM x)
{
    return (sheet_index_t) scm_to_int (x);
}

bool _scm_is_sheet_index_t (SCM x)
{
    return scm_is_integer(x) && sheet_validate_int_as_sheet_index_t (scm_to_int (x));
}


static const char *
sheet_get_index_name (sheet_index_t index)
{
    g_assert (sheet_validate_sheet_index_t (index));
    return sheet_index_name[index];
}


SCM_DEFINE (G_sheet_assign_memory, "sheetAssignMemory", 3, 0, 0,
            (SCM id, SCM size, SCM bank), "\
Set the size and VRAM storage of a given sheet")
{
    SCM_ASSERT(_scm_is_sheet_index_t (id), id, SCM_ARG1, "sheetAssignMemory");
    SCM_ASSERT(_scm_is_matrix_size_t (size), size, SCM_ARG2, "sheetAssignMemory");
    SCM_ASSERT(_scm_is_vram_bank_t (bank), bank, SCM_ARG3, "sheetAssignMemory");

    sheet_assign_memory (scm_to_int (id), scm_to_int (size), scm_to_int (bank));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_sheet_dump_memory_assignment, "sheet-dump-memory-assignment", 0, 0, 0, (void), "\
Returns the matrix size and VRAM bank of the BG layer.")
{
    for (sheet_index_t i = SHEET_MAIN_BG; i <= SHEET_MAIN_OBJ; i++)
    {
        char *c_str = g_strdup_printf("%s %dx%d %s",
                                      sheet_get_index_name(i),
                                      matrix_get_width(sheets[i].size),
                                      matrix_get_height(sheets[i].size),
                                      vram_get_bank_name(sheets[i].bank));
        console_write_latin1_string(c_str);
        console_move_down(1);
        console_move_to_column(0);
        g_free(c_str);
    }
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_sheet_get_height, "sheet-get-height", 1, 0, 0, (SCM id), "\
return the height, in pixels, of a data sheet.")
{
    SCM_ASSERT(_scm_is_sheet_index_t (id), id, SCM_ARG1, "sheet-get-height");

    return scm_from_int (sheet_get_height (_scm_to_sheet_index_t (id)));
}

SCM_DEFINE (G_sheet_get_width, "sheet-get-width", 1, 0, 0, (SCM id), "\
return the width, in pixels, of a data sheet.")
{
    SCM_ASSERT(_scm_is_sheet_index_t (id), id, SCM_ARG1, "sheet-get-width");

    return scm_from_int (sheet_get_width (_scm_to_sheet_index_t (id)));
}

SCM_DEFINE (G_sheet_set_data_from_file, "sheet-set-bmp-from-file",
            2, 0, 0, (SCM id, SCM filename), "\
Copies the contents of an ARGB32 image into the sheet.  Note that the \n\
sheet's size and VRAM must first be set using 'sheet-init'")
{
    SCM_ASSERT(_scm_is_sheet_index_t (id), id, SCM_ARG1, "sheet-set-data-from-file");
    SCM_ASSERT(scm_is_string (filename), filename, SCM_ARG2, "sheet-set-data-from-file");
    
    char *str = scm_to_locale_string (filename);
    sheet_set_data_from_file (scm_to_int (id), str);
    free (str);
    return SCM_UNSPECIFIED;
}

SCM_VARIABLE_INIT (G_SHEET_MAIN_BG, "SHEET_MAIN_BG", scm_from_int (SHEET_MAIN_BG));
SCM_VARIABLE_INIT (G_SHEET_MAIN_OBJ, "SHEET_MAIN_OBJ", scm_from_int (SHEET_MAIN_OBJ));

SCM_VARIABLE_INIT (G_TILE_WIDTH, "TILE_WIDTH", scm_from_int (TILE_WIDTH));
SCM_VARIABLE_INIT (G_TILE_WIDTH, "TILE_HEIGHT", scm_from_int (TILE_HEIGHT));

void
sheet_init_guile_procedures (void)
{
#include "sheet.x"
    scm_c_export ("SHEET_MAIN_BG",
                  "SHEET_MAIN_OBJ",
                  "TILE_WIDTH",
                  "TILE_HEIGHT",
                  "sheetAssignMemory",
                  "sheet-dump-memory-assignment",
                  "sheet-set-bmp-from-file",
                  "sheet-get-width",
                  "sheet-get-height",
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
