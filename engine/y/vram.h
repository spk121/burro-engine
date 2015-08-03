/** @file vram.h
    A set of pre-allocated, memory-aligned buffers used for storage
    of BG maps, BG bitmaps, BG tilesheets, and OBJ spritesheets.
*/

#ifndef BURRO_VRAM_H
#define BURRO_VRAM_H

#include <stdint.h>
#include "../x.h"

/** Index of a VRAM bank. */
enum vram_bank_tag {
    VRAM_0,                     /**< space for main tilesheet  */
    VRAM_1,                     /**< space for sub tilesheet  */
    VRAM_A,                     /**< 64k uint32, usually for main spritesheet  */
    VRAM_B,                     /**< 64k uint32, usually for sub spritesheet  */
    VRAM_C,                     /**< 64k uint32  */
    VRAM_D,                     /**< 64k uint32  */
    VRAM_E,                     /**< 16k uint32, usually for bg maps  */
    VRAM_F,                     /**< 1k uint32, usually for bg maps  */
    VRAM_G,                     /**< 1k uint32, usually for bg maps  */
    VRAM_H,                     /**< 1k uint32, usually for bg maps  */
    VRAM_I,                     /**< 1k uint32, usually for bg maps  */
    VRAM_ABCD,                  /**< 256k uint32, use A, B, C, and D
                                 * as single block, main screen only  */
    VRAM_AB,                    /**< 128k uint32, use A and B as a
                                 * single block, main screen only  */
    VRAM_CD,                    /**< 128k uint32, use C and D as a
                                 * single block, main screen only  */
    VRAM_COUNT,
};

typedef enum vram_bank_tag vram_bank_t;


////////////////////////////////////////////////////////////////
//

// VRAM 0 and 1: 2MB of storage.
// Best used as 512x512px BG Tilesheets

#define VRAM_0_U32_SIZE (512*512)
#define VRAM_1_U32_SIZE (512*512)

#define VRAM_0_U32_OFFSET 0
#define VRAM_1_U32_OFFSET (VRAM_0_U32_OFFSET + VRAM_0_U32_SIZE)

uint32_t vram_01[VRAM_0_U32_SIZE + VRAM_1_U32_SIZE] __attribute__((aligned (16)));

#define VRAM_0_U32_PTR (vram_01 + VRAM_0_U32_OFFSET)
#define VRAM_1_U32_PTR (vram_01 + VRAM_1_U32_OFFSET)

////////////////////////////////////////////////////////////////
//
// VRAM ABCD: 1MB of storage
// Best used for BG BMP and for OBJ SPRITESHEETS
// Can be used as 4 smaller chunks, 2 medium chunks, or 1 larger chunk.
// Best used as 256x256px BG BMP or OBJ Spritesheets

#define VRAM_A_U32_SIZE (256*256)
#define VRAM_B_U32_SIZE (256*256)
#define VRAM_C_U32_SIZE (256*256)
#define VRAM_D_U32_SIZE (256*256)
#define VRAM_AB_U32_SIZE (VRAM_A_U32_SIZE + VRAM_B_U32_SIZE)
#define VRAM_CD_U32_SIZE (VRAM_C_U32_SIZE + VRAM_D_U32_SIZE)
#define VRAM_ABCD_U32_SIZE (VRAM_AB_U32_SIZE + VRAM_CD_U32_SIZE)

#define VRAM_A_U32_OFFSET (0)
#define VRAM_B_U32_OFFSET (VRAM_A_U32_OFFSET + VRAM_A_U32_SIZE)
#define VRAM_C_U32_OFFSET (VRAM_B_U32_OFFSET + VRAM_B_U32_SIZE)
#define VRAM_D_U32_OFFSET (VRAM_C_U32_OFFSET + VRAM_C_U32_SIZE)

#define VRAM_ABCD_U32_OFFSET (VRAM_A_U32_OFFSET)
#define VRAM_AB_U32_OFFSET (VRAM_A_U32_OFFSET)
#define VRAM_CD_U32_OFFSET (VRAM_C_U32_OFFSET)

uint32_t vram_ABCD_store[VRAM_A_U32_SIZE + VRAM_B_U32_SIZE + VRAM_C_U32_SIZE + VRAM_D_U32_SIZE] __attribute__ ((aligned (16)));

#define VRAM_A_U32_PTR    (vram_ABCD_store + VRAM_A_U32_OFFSET)
#define VRAM_B_U32_PTR    (vram_ABCD_store + VRAM_B_U32_OFFSET)
#define VRAM_C_U32_PTR    (vram_ABCD_store + VRAM_C_U32_OFFSET)
#define VRAM_D_U32_PTR    (vram_ABCD_store + VRAM_D_U32_OFFSET)
#define VRAM_AB_U32_PTR   (vram_ABCD_store + VRAM_AB_U32_OFFSET)
#define VRAM_CD_U32_PTR   (vram_ABCD_store + VRAM_CD_U32_OFFSET)
#define VRAM_ABCD_U32_PTR (vram_ABCD_store + VRAM_ABCD_U32_OFFSET)

////////////////////////////////////////////////////////////////
//
// VRAM EFGHI: 80kb of storage
// Best used for BG MAP storage


#define VRAM_E_U32_SIZE (128*128)
#define VRAM_F_U32_SIZE (32*32)
#define VRAM_G_U32_SIZE (32*32)
#define VRAM_H_U32_SIZE (32*32)
#define VRAM_I_U32_SIZE (32*32)

#define VRAM_E_U32_OFFSET (0)
#define VRAM_F_U32_OFFSET (VRAM_E_U32_OFFSET + VRAM_E_U32_SIZE)
#define VRAM_G_U32_OFFSET (VRAM_F_U32_OFFSET + VRAM_F_U32_SIZE)
#define VRAM_H_U32_OFFSET (VRAM_G_U32_OFFSET + VRAM_G_U32_SIZE)
#define VRAM_I_U32_OFFSET (VRAM_H_U32_OFFSET + VRAM_H_U32_SIZE)

uint32_t vram_EFGHI_store[VRAM_E_U32_SIZE + VRAM_F_U32_SIZE + VRAM_G_U32_SIZE + VRAM_H_U32_SIZE + VRAM_I_U32_SIZE] __attribute__ ((aligned (16)));

#define VRAM_E_U32_PTR (vram_EFGHI_store + VRAM_E_U32_OFFSET)
#define VRAM_F_U32_PTR (vram_EFGHI_store + VRAM_F_U32_OFFSET)
#define VRAM_G_U32_PTR (vram_EFGHI_store + VRAM_G_U32_OFFSET)
#define VRAM_H_U32_PTR (vram_EFGHI_store + VRAM_H_U32_OFFSET)
#define VRAM_I_U32_PTR (vram_EFGHI_store + VRAM_I_U32_OFFSET)

////////////////////////////////////////////////////////////////

/** Return the size, in 32-bit words, of a VRAM bank.
 *  @param [in] bank
 *  @return size of bank of VRAM in 32-bit words
 */
size_t vram_get_u32_size (vram_bank_t bank);

/** Return a pointer to the beginning of the VRAM bank.
 *  @param [in] bank index
 *  @return a pointer to the VRAM bank
 */
uint32_t *vram_get_u32_ptr (vram_bank_t bank);

/** Zero-fill the contents of a VRAM bank.
 *  @param [in] bank index
 */
void vram_zero_bank (vram_bank_t bank);

/** Register VRAM procedures with the script engine. */
void vram_init_guile_procedures (void);


////////////////////////////////////////////////////////////////


// WRAM - 64K + 32K, audio

// OAM - 2K object memory

// 4MB ISO data storage memory

// Total VRAM ABCD = 1024kB EFGHI = 80kB
//       WRAM = 96kB  (2.2 sec of audio)

// MAX ISO size 512 MB

#endif
