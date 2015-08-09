#include "matrix.h"

static int matrix_width[MATRIX_N_SIZES] = {
    [MATRIX_16x16] = 16,
    [MATRIX_16x32] = 16,
    [MATRIX_16x64] = 16,
    [MATRIX_32x16] = 32,
    [MATRIX_32x32] = 32,
    [MATRIX_32x64] = 32,
    [MATRIX_64x16] = 16,
    [MATRIX_64x32] = 64,
    [MATRIX_64x64] = 64,
    [MATRIX_64x128] = 64,
    [MATRIX_128x64] = 128,
    [MATRIX_128x128] = 128,
    [MATRIX_256x256] = 256,
    [MATRIX_512x256] = 512,
    [MATRIX_256x512] = 256,
    [MATRIX_512x512] = 512
};

static int matrix_height[MATRIX_N_SIZES] = {
    [MATRIX_16x16] = 16,
    [MATRIX_16x32] = 32,
    [MATRIX_16x64] = 64,
    [MATRIX_32x16] = 16,
    [MATRIX_32x32] = 32,
    [MATRIX_32x64] = 64,
    [MATRIX_64x16] = 16,
    [MATRIX_64x32] = 32,
    [MATRIX_64x64] = 64,
    [MATRIX_64x128] = 128,
    [MATRIX_128x64] = 64,
    [MATRIX_128x128] = 128,
    [MATRIX_256x256] = 256,
    [MATRIX_512x256] = 256,
    [MATRIX_256x512] = 512,
    [MATRIX_512x512] = 512
};

static int matrix_u32_size[MATRIX_N_SIZES] = {
    [MATRIX_16x16] = 16*16,
    [MATRIX_16x32] = 16*32,
    [MATRIX_16x64] = 16*64,
    [MATRIX_32x16] = 32*16,
    [MATRIX_32x32] = 32*32,
    [MATRIX_32x64] = 32*64,
    [MATRIX_64x16] = 64*16,
    [MATRIX_64x32] = 64*32,
    [MATRIX_64x64] = 64*64,
    [MATRIX_64x128] = 64*128,
    [MATRIX_128x64] = 128*64,
    [MATRIX_128x128] = 128*128,
    [MATRIX_256x256] = 256*256,
    [MATRIX_512x256] = 512*256,
    [MATRIX_256x512] = 256*512,
    [MATRIX_512x512] = 512*512
};


int matrix_get_width (matrix_size_t size)
{
    matrix_assert_valid_size (size);

    return matrix_width[size];
}

int matrix_get_height (matrix_size_t size)
{
    matrix_assert_valid_size (size);

    return matrix_height[size];
}

int matrix_get_u32_size (matrix_size_t size)
{
    matrix_assert_valid_size (size);

    return matrix_u32_size[size];
}

/** Assigns a pointer to the location of a VRAM bank.  Also
 *  allocates and assigns a vector of pointers to each row
 *  inside of a vram bank.
 *  @param [in] size - the intended size of the data matrix
 *  @param [in] vram - the VRAM bank that shall hold the data matrix
 *  @param [out] storage
 *     - the address of a pointer that shall be assigned the location
 *       of a VRAM bank
 *  @param [out] data - the address of a pointer that shall be assigned
 *     a vector of pointers to the beginning of each row in the
 *     VRAM bank.
 */
void       matrix_attach_to_vram        (matrix_size_t size,
                                         vram_bank_t vram,
                                         uint32_t **storage,
                                         uint32_t ***data)
{
    matrix_assert_valid_size (size);
    // vram_assert_valid_index (vram);
    g_assert_nonnull (storage);
    g_assert_nonnull (data);
    g_assert_cmpint (matrix_get_u32_size(size), <=, vram_get_u32_size(vram));

    *storage = vram_get_u32_ptr (vram);
    g_free (*data);
    *data = g_new0(uint32_t *, (size_t) matrix_get_height(size));
    for (int j = 0; j < matrix_get_height (size); j ++)
        *data[j] = *storage + j * matrix_get_width(size);
}

/** Finds the locations of rows of a submatrix within a VRAM bank
 *  @param [in] full_matrix_size - the size of the matrix of data overlayed on this VRAM bank
 *  @param [in] vram - the VRAM bank that holds the matrix
 *  @param [in] submatrix_size - the size of the submatrix within the full matrix
 *  @param [in] col_offset - the column location of the top-left of the submatrix within the matrix
 *  @param [in] row_offset - the row location of the top-left of the submatrix within the matrix
 *  @param [out] data - the address of a pointer that shall be assigned
 *     a vector of pointers to the beginning of each row of the submatrix within in the
 *     VRAM bank.
 */
void       submatrix_attach_to_vram_with_offset (matrix_size_t full_matrix_size,
                                                 vram_bank_t vram,
                                                 matrix_size_t submatrix_size,
                                                 int col_offset,
                                                 int row_offset,
                                                 uint32_t ***data)
{
    matrix_assert_valid_size (full_matrix_size);
    matrix_assert_valid_size (submatrix_size);
    // vram_assert_valid_index (vram);
    g_assert_nonnull (data);
    g_assert_cmpint (matrix_get_u32_size(full_matrix_size), <=, vram_get_u32_size(vram));
    g_assert_cmpint (matrix_get_u32_size(submatrix_size), <=, matrix_get_u32_size (full_matrix_size));
    g_assert_cmpint (col_offset, >=, 0);
    g_assert_cmpint (row_offset, >=, 0);
    g_assert_cmpint (col_offset + matrix_get_width (submatrix_size), <=, matrix_get_width (full_matrix_size));
    g_assert_cmpint (row_offset + matrix_get_height (submatrix_size), <=, matrix_get_height (full_matrix_size));

    uint32_t *storage = vram_get_u32_ptr (vram);
    g_free (*data);
    *data = g_new0(uint32_t *, (size_t) matrix_get_height(submatrix_size));
    for (int j = 0; j < matrix_get_height (submatrix_size); j ++)
        *data[j] = storage + (row_offset + j) * matrix_get_width(full_matrix_size) + col_offset;
    
}


SCM_VARIABLE_INIT (G_MATRIX_16x16, "MATRIX_16x16", scm_from_int (MATRIX_16x16));
SCM_VARIABLE_INIT (G_MATRIX_16x32, "MATRIX_16x32", scm_from_int (MATRIX_16x32));
SCM_VARIABLE_INIT (G_MATRIX_16x64, "MATRIX_16x64", scm_from_int (MATRIX_16x64));
SCM_VARIABLE_INIT (G_MATRIX_32x16, "MATRIX_32x16", scm_from_int (MATRIX_32x16));
SCM_VARIABLE_INIT (G_MATRIX_32x32, "MATRIX_32x32", scm_from_int (MATRIX_32x32));
SCM_VARIABLE_INIT (G_MATRIX_32x64, "MATRIX_32x64", scm_from_int (MATRIX_32x64));
SCM_VARIABLE_INIT (G_MATRIX_64x16, "MATRIX_64x16", scm_from_int (MATRIX_64x16));
SCM_VARIABLE_INIT (G_MATRIX_64x32, "MATRIX_64x32", scm_from_int (MATRIX_64x32));
SCM_VARIABLE_INIT (G_MATRIX_64x64, "MATRIX_64x64", scm_from_int (MATRIX_64x64));
SCM_VARIABLE_INIT (G_MATRIX_64x128, "MATRIX_64x128", scm_from_int (MATRIX_64x128));
SCM_VARIABLE_INIT (G_MATRIX_128x64, "MATRIX_128x64", scm_from_int (MATRIX_128x64));
SCM_VARIABLE_INIT (G_MATRIX_128x128, "MATRIX_128x128", scm_from_int (MATRIX_128x128));
SCM_VARIABLE_INIT (G_MATRIX_256x256, "MATRIX_256x256", scm_from_int (MATRIX_256x256));
SCM_VARIABLE_INIT (G_MATRIX_256x512, "MATRIX_256x512", scm_from_int (MATRIX_256x512));
SCM_VARIABLE_INIT (G_MATRIX_512x256, "MATRIX_512x256", scm_from_int (MATRIX_512x256));
SCM_VARIABLE_INIT (G_MATRIX_512x512, "MATRIX_512x512", scm_from_int (MATRIX_512x512));

SCM_DEFINE (G_matrix_get_width, "matrix-get-width", 1, 0, 0, (SCM matrix_size), "\
Returns the width of a matrix, given its size category")
{
    SCM_ASSERT (scm_is_integer (matrix_size), matrix_size, SCM_ARG1, "matrix-get-width");
    
    int i = scm_to_int (matrix_size);
    /* if (i < MATRIX_16x16 || i > MATRIX_512x512) */
    /*     scm_out_of_range_error ("matrix-get-width", matrix_size); */

    return scm_from_int (matrix_width[i]);
}

SCM_DEFINE (G_matrix_get_height, "matrix-get-height", 1, 0, 0, (SCM matrix_size), "\
Returns the height of a matrix, given its size category")
{
    SCM_ASSERT (scm_is_integer (matrix_size), matrix_size, SCM_ARG1, "matrix-get-width");
    
    int i = scm_to_int (matrix_size);
    /* if (i < MATRIX_16x16 || i > MATRIX_512x512) */
    /*     scm_out_of_range_error ("matrix-get-width", matrix_size); */

    return scm_from_int (matrix_height[i]);    
}

SCM_DEFINE (G_matrix_get_u32_size, "matrix-get-u32-size", 1, 0, 0, (SCM matrix_size), "\
Returns the size of a matrix in 4-byte words, given its size category")
{
    SCM_ASSERT (scm_is_integer (matrix_size), matrix_size, SCM_ARG1, "matrix-get-width");
    
    int i = scm_to_int (matrix_size);
    /* if (i < MATRIX_16x16 || i > MATRIX_512x512) */
    /*     scm_out_of_range_error ("matrix-get-width", matrix_size); */

    return scm_from_int (matrix_u32_size[i]);    
    
}

SCM_DEFINE (G_matrix_to_bytevector, "matrix->bytevector", 2, 0, 0, (SCM matrix_size, SCM vram), "\
Returns a uint32 bytevector pointing to the contents of a matrix overlayed on a given VRAM bank")
{
    SCM_ASSERT (scm_is_integer (matrix_size), matrix_size, SCM_ARG1, "matrix->bytevector");
    SCM_ASSERT (scm_is_integer (vram), vram, SCM_ARG1, "matrix->bytevector");
    int i = scm_to_int (matrix_size);
    int v = scm_to_int (vram);

    /* if (i < MATRIX_16x16 || i > MATRIX_512x512) */
    /*     scm_out_of_range_error ("matrix->bytevector", matrix_size); */
    /* if (v < VRAM_0 || v > VRAM_I) */
    /*     scm_out_of_range_error ("matrix->bytevector", vram); */
    
    // First make a pointer
    SCM pointer = scm_from_pointer (vram_get_u32_ptr (i), NULL);

    // Then make a bytevector
    SCM len = scm_from_size_t (matrix_u32_size[i]);
    SCM zero_offset = scm_from_size_t (0);
    SCM uvec_type = scm_from_int (SCM_ARRAY_ELEMENT_TYPE_U32);
        
    return scm_pointer_to_bytevector (pointer, len, zero_offset, uvec_type);
    
}

#if 0
// FIXME: build up the list row by row...

SCM_DEFINE (G_matrix_to_list_of_bytevector, "matrix->list-of-bytevectors", 2, 0, 0, (SCM matrix_size, SCM vram), "\
Returns a list of uint32 bytevectors pointing to the rows of a matrix overlayed on a given VRAM bank")
{
    SCM_ASSERT (scm_is_integer (matrix_size), matrix_size, SCM_ARG1, "matrix->list-of-bytevectors");
    SCM_ASSERT (scm_is_integer (vram), vram, SCM_ARG1, "matrix->list-of-bytevectors");
    int i = scm_to_int (matrix_size);
    int v = scm_to_int (vram);

    if (i < MATRIX_16x16 || i > MATRIX_512x512)
        scm_out_of_range_error ("matrix->list-of-bytevectors", matrix_size);
    if (v < VRAM_0 || v > VRAM_I)
        scm_out_of_range_error ("matrix->list-of-bytevectors", vram);
    
    // First make a pointer
    bg_index_t i = scm_to_int (id);
    SCM pointer = scm_from_pointer (vram_get_u32ptr (i), NULL);

    // Then make a bytevector
    SCM len = scm_from_size_t (matrix_u32_size[i]);
    SCM zero_offset = scm_from_size_t (0);
    SCM uvec_type = SCM_ARRAY_ELEMENT_U32;
        
    return scm_pointer_to_bytevector (pointer, len, zero_offset, uvec_type);
}
#endif

SCM_DEFINE (G_submatrix_to_list_of_bytevectors, "submatrix->list-of-bytevectors", 2, 0, 0,
            (SCM matrix_size, SCM vram, SCM submatrix_size, SCM col_offset, SCM row_offset), "\
Returns a list of uint32 bytevectors pointing to the rows of a submatrix overlayed on a matrix overlayed on a given VRAM bank")
{
    return SCM_UNSPECIFIED;
}


void       matrix_init_guile_procedures ()
{
#include "matrix.x"
    scm_c_export ("matrix-get-width",
                  "matrix-get-height",
                  "matrix-get-u32-size",
                  "matrix->bytevector",
                  // "submatrix->list-of-bytevectors",
                  NULL);
}

