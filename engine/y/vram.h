#ifndef BURRO_VRAM_H
#define BURRO_VRAM_H

#include <stdint.h>
////////////////////////////////////////////////////////////////
//

// For reasons of efficienty and perversity, all the background and sprite memory
// is preallocated in a couple of blocks

enum vram_bank_tag {
    VRAM_0,
    VRAM_1,
    VRAM_A,
    VRAM_B,
    VRAM_C,
    VRAM_D,
    VRAM_E,
    VRAM_F,
    VRAM_G,
    VRAM_H,
    VRAM_I,
    VRAM_ABCD,
    VRAM_AB,
    VRAM_CD,
    VRAM_COUNT
};

typedef enum vram_bank_tag vram_bank_t;

////////////////////////////////////////////////////////////////
//

// VRAM 0 and 1: 2MB of storage.
// Best used as 512x512px BG Tilesheets

#define VRAM_0_U32_SIZE (BG_TILESHEET_HEIGHT * BG_TILESHEET_WIDTH)
#define VRAM_1_U32_SIZE (BG_TILESHEET_HEIGHT * BG_TILESHEET_WIDTH)

#define VRAM_0_U32_OFFSET 0
#define VRAM_1_U32_OFFSET (VRAM_0_U32_OFFSET + VRAM_0_U32_SIZE)

uint32_t vram_01[VRAM_0_U32_SIZE + VRAM_1_U32_SIZE] __attribute__ ((aligned (__BIGGEST_ALIGNMENT__)));

const uint32_t *vram_0 = vram_01 + VRAM_0_U32_OFFSET;
const uint32_t *vram_1 = vram_01 + VRAM_1_U32_OFFSET;

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

uint32_t vram_ABCD_store[VRAM_A_U32_SIZE + VRAM_B_U32_SIZE + VRAM_C_U32_SIZE + VRAM_D_U32_SIZE] __attribute__ ((aligned (__BIGGEST_ALIGNMENT__)));

const uint32_t *vram_A = vram_ABCD_store + VRAM_A_U32_OFFSET;
const uint32_t *vram_B = vram_ABCD_store + VRAM_B_U32_OFFSET;
const uint32_t *vram_C = vram_ABCD_store + VRAM_C_U32_OFFSET;
const uint32_t *vram_D = vram_ABCD_store + VRAM_D_U32_OFFSET;
const uint32_t *vram_ABCD = vram_ABCD_store;
const uint32_t *vram_AB = vram_ABCD_store;
const uint32_t *vram_CD = vram_ABCD_store + VRAM_C_U32_OFFSET;

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

uint32_t vram_EFGHI[VRAM_E_U32_SIZE + VRAM_F_U32_SIZE + VRAM_G_U32_SIZE + VRAM_H_U32_SIZE + VRAM_I_U32_SIZE] __attribute__ ((aligned (__BIGGEST_ALIGNMENT__)));

const uint32_t *vram_E = vram_EFGHI + VRAM_E_U32_OFFSET;
const uint32_t *vram_F = vram_EFGHI + VRAM_F_U32_OFFSET;
const uint32_t *vram_G = vram_EFGHI + VRAM_G_U32_OFFSET;
const uint32_t *vram_H = vram_EFGHI + VRAM_H_U32_OFFSET;
const uint32_t *vram_I = vram_EFGHI + VRAM_I_U32_OFFSET;

////////////////////////////////////////////////////////////////
size_t vram_get_u32_size (vram_bank_t bank);
uint32_t *vram_get_u32_ptr (vram_bank_t bank);
void vram_zero_bank (vram_bank_t bank);

// WRAM - 64K + 32K, audio

// OAM - 2K object memory

// 4MB ISO data storage memory

// Total VRAM ABCD = 1024kB EFGHI = 80kB
//       WRAM = 96kB  (2.2 sec of audio)

// MAX ISO size 512 MB
