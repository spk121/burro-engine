/** @file matrix.h
 *  @brief An interface to overlay 2D matrices of UINT32 data onto VRAM storage.
 */
#ifndef BURRO_MATRIX_H
#define BURRO_MATRIX_H

#include "../x.h"
#include "vram.h"

typedef enum {
    MATRIX_16x16 = 0,             /**< 1x1 16px block, 1k VRAM  */
    MATRIX_16x32,             /**< 1x2 16px blocks, 2k VRAM  */
    MATRIX_16x64,             /**< 1x4 16px blocks, 4k VRAM  */
    MATRIX_32x16,             /**< 2x1 16px blocks, 2k VRAM  */
    MATRIX_32x32,             /**< 2x2 16px blocks, 4k VRAM  */
    MATRIX_32x64,             /**< 2x4 16px blocks, 8k VRAM  */
    MATRIX_64x16,             /**< 4x1 16px blocks, 4k VRAM  */
    MATRIX_64x32,             /**< 4x2 16px blocks, 8k VRAM  */
    MATRIX_64x64,             /**< 4x4 16px blocks, 16k VRAM  */
    MATRIX_64x128,            /**< 4x8 16px blocks, 32k VRAM  */
    MATRIX_128x64,            /**< 8x4 16px blocks, 32k VRAM  */
    MATRIX_128x128,           /**< 8x8 16px blocks, 64k VRAM  */
    MATRIX_256x256,           /**< 16x16 16px blocks, 256k VRAM */
    MATRIX_512x256,           /**< 32x16 16px blocks, 512k VRAM */
    MATRIX_256x512,           /**< 16x32 16px blocks, 512k VRAM */
    MATRIX_512x512,           /**< 32x32 16px blocks, 1024k VRAM */
    MATRIX_N_SIZES
} matrix_size_t;

bool matrix_validate_int_as_matrix_size_t (int x);

#define matrix_assert_valid_size(_x) \
    g_assert(_x >= 0 && _x < MATRIX_N_SIZES)


int       matrix_get_width             (matrix_size_t size);
int       matrix_get_height            (matrix_size_t size);
int       matrix_get_u32_size          (matrix_size_t size);

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
                                         uint32_t *data[512]);

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
                                                 uint32_t ***rows);

////////////////////////////////////////////////////////////////
SCM _scm_from_matrix_size_t (matrix_size_t x);
matrix_size_t _scm_to_matrix_size_t (SCM x);
bool _scm_is_matrix_size_t (SCM x);

void       matrix_init_guile_procedures ();

#endif
