#include <stdint.h>
#include <stdbool.h>
#include "../x.h"
#include "sheet.h"
#include "vram.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-conversion"

sheet_t sheets[SHEET_COUNT];

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
                sheets[id].data[j][i] = img_store[j * img_stride + i];
            }
        }
        if (id == SHEET_MAIN_OBJ)
            g_debug ("loaded pixbuf %s as bg main sheet", path);
        else
            g_debug ("loaded pixbuf %s as bg sub sheet", path);
        g_free (path);
        g_object_unref (pb);
    }
}

SCM_DEFINE (G_sheet_assign_memory, "sheet-assign-memory", 3, 0, 0,
            (SCM id, SCM size, SCM bank), "\
Set the size and VRAM storage of a given sheet")
{
    sheet_assign_memory (scm_to_int (id), scm_to_int (size), scm_to_int (bank));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_sheet_set_data_from_file, "sheet-set-data-from-file",
            2, 0, 0, (SCM id, SCM filename), "\
Copies the contents of an ARGB32 image into the sheet.  Note that the \n\
sheet's size and VRAM must first be set using 'sheet-init'")
{
    char *str = scm_to_locale_string (filename);
    sheet_set_data_from_file (scm_to_int (id), str);
    free (str);
    return SCM_UNSPECIFIED;
}

SCM_VARIABLE_INIT (G_SHEET_MAIN_BG, "SHEET_MAIN_BG", scm_from_int (SHEET_MAIN_BG));
SCM_VARIABLE_INIT (G_SHEET_MAIN_OBJ, "SHEET_MAIN_OBJ", scm_from_int (SHEET_MAIN_OBJ));
SCM_VARIABLE_INIT (G_SHEET_SUB_BG, "SHEET_SUB_BG", scm_from_int (SHEET_SUB_BG));
SCM_VARIABLE_INIT (G_SHEET_SUB_OBJ, "SHEET_SUB_OBJ", scm_from_int (SHEET_SUB_OBJ));

void
sheet_init_guile_procedures (void)
{
#include "sheet.x"
    scm_c_export ("SHEET_MAIN_BG",
                  "SHEET_SUB_BG",
                  "SHEET_MAIN_OBJ",
                  "SHEET_SUB_OBJ",
                  "sheet-assign-memory",
                  "sheet-set-data-from-file",
                  "sheet-get-width",
                  "sheet-get-height",
                  "sheet-get-u32-size",
                  // "sheet->bytevector",
                  // "sheet->list-of-bytevectors",
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
