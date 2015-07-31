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

#define VRAM_0_U32_SIZE (BG_TILESHEET_HEIGHT * BG_TILESHEET_WIDTH)
#define VRAM_1_U32_SIZE (BG_TILESHEET_HEIGHT * BG_TILESHEET_WIDTH)

#define VRAM_0_U32_OFFSET 0
#define VRAM_1_U32_OFFSET (VRAM_0_U32_OFFSET + VRAM_0_U32_SIZE)

uint32_t vram_01[VRAM_0_U32_SIZE + VRAM_1_U32_SIZE] __attribute__ ((aligned (__BIGGEST_ALIGNMENT__)));

const uint32_t *vram_0 = vram_01 + VRAM_0_U32_OFFSET;
const uint32_t *vram_1 = vram_01 + VRAM_1_U32_OFFSET;

////////////////////////////////////////////////////////////////
//
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
void vram_zero_bank (vram_bank_t bank);


// VRAM A - 256K
//   main bg (256*256 bmp, tilesheet or map)
//   main sprite
// VRAM B - 128K
//   main bg  (256*256 bmp, tilesheet or map)
//   main sprite
// VRAM C - 128K
//   main bg (256*256 bmp, tilesheet or map)
//   sub bg  (256*256 bmp, tilesheet or map)
//   audio
// VRAM D - 128K
//   main bg (256*256 bmp, tilesheet or map)
//  audio, sub sprite
// VRAM E - 64K
//   main bg (256*128 bmp, tilesheet, or map)
//  main sprite
// VRAM F - 16K
//   main bg (128*64 bmp, tilesheet, or map)
//   main sprite
// VRAM G - 16K
//   main bg, main sprite
// VRAM H - 32K
//   sub bg
// VRAM I - 16K
//   sub sprite

// VRAM A - 256*256*4 - main bg or main sprite
// VRAM B - 256*256*4 - main bg or main sprite
// VRAM C - 256*256*4 - main bg or sub bg
// VRAM D - 256*256*4 - main bg or sub sprite
// VRAM E - 64*64*4   - main bg or sub bg
// VRAM F - 64*64*4   - main bg or main sprite
// VRAM G - 64*64*4   - main bg or main sprite
// VRAM H - 64*64*4   - sub bg
// VRAM I - 64*64*4   - sub sprite

//                      | main bg | main sprite | sub bg | sub sprite | audio
// VRAM A - 256*256*4 -      x          x
// VRAM B - 256*256*4 -      x          x
// VRAM C - 256*256*4 -      C                      C                     C
// VRAM D - 256*256*4 -      D                                  D         D
// VRAM E - 64*64*4   -      E                      E
// VRAM F - 64*64*4   -      F          F
// VRAM G - 64*64*4   -      G          G
// VRAM H - 64*64*4   -                             H
// VRAM I - 64*64*4   -                                         I

// WRAM - 64K + 32K, audio

// OAM - 2K object memory

// 4MB ISO data storage memory

// Total VRAM ABCD = 1024kB EFGHI = 80kB
//       WRAM = 96kB  (2.2 sec of audio)

// MAX ISO size 512 MB


// ...

// Or if you just go by the DS Memory maps
// VRAM Engine A, BG
//   - 1234   512x512
//   - 12 34  512x256, 512x256
//   -        512x256, 256x512
//   - 12 3 4 512x256, 256x256, 256x256
//   - 256x512, 256x512
//   - 256x512, 256x256, 256x256
//   - 256x256, 256x256, 256x256, 256x256

// VRAM Engine B, BG 128kb
//   - (256x256, 256x128x2, 128x256x2, 128x128x4)
// VRAM Engine A, OBJ 256kb
//   -  (256x512, 512x256, 256x256x2)
// VRAM Engine B, OBJ 128kb
//   -   (256x256)
// OAM  Engine A, 1kb
// OAM  Engine B, 1 kb

// Main Memory, 4MB
// WRAM 64kb + 32kb

// ISO ROM - 512 MB


////////////////////////////////////////////////////////////////

#if 0
enum VRAM_A_TYPE {
    VRAM_A_MAIN_BG_0,
    VRAM_A_MAIN_BG_1,
    VRAM_A_MAIN_BG_2,
    VRAM_A_MAIN_BG_3,
    VRAM_A_MAIN_SPRITE_0,
    VRAM_A_MAIN_SPRITE_1
};

enum VRAM_B_TYPE {
    VRAM_B_MAIN_BG_0,
    VRAM_B_MAIN_BG_1,
    VRAM_B_MAIN_BG_2,
    VRAM_B_MAIN_BG_3,
    VRAM_B_MAIN_SPRITE_0,
    VRAM_B_MAIN_SPRITE_1,
};

enum VRAM_C_TYPE {
    VRAM_C_MAIN_BG_0,
    VRAM_C_MAIN_BG_1,
    VRAM_C_MAIN_BG_2,
    VRAM_C_MAIN_BG_3,
    VRAM_C_SUB_BG
};

enum VRAM_D_TYPE {
    VRAM_D_MAIN_BG_0,
    VRAM_D_MAIN_BG_1,
    VRAM_D_MAIN_BG_2,
    VRAM_D_MAIN_BG_3,
    VRAM_D_SUB_SPRITE,
};

enum VRAM_E_TYPE {
    VRAM_E_MAIN_BG_0_FIRST_HALF,
    VRAM_E_MAIN_SPRITE_0_FIRST_HALF
};

enum VRAM_F_TYPE {
    VRAM_F_MAIN_BG_0_FIRST_EIGHTH,
    VRAM_F_MAIN_BG_0_SECOND_EIGHTH,
    VRAM_F_MAIN_BG_0_FIFTH_EIGHTH,
    VRAM_F_MAIN_BG_0_SIXTH_EIGHTH,
    VRAM_F_MAIN_SPRITE_0_FIRST_EIGHTH,
    VRAM_F_MAIN_SPRITE_0_SECOND_EIGHTH,
    VRAM_F_MAIN_SPRITE_0_FIFTH_EIGHTH,
    VRAM_F_MAIN_SPRITE_0_SIXTH_EIGHTH,
};

enum VRAM_G_TYPE {
    VRAM_G_MAIN_BG_0_FIRST_EIGHTH,
    VRAM_G_MAIN_BG_0_SECOND_EIGHTH,
    VRAM_G_MAIN_BG_0_FIFTH_EIGHTH,
    VRAM_G_MAIN_BG_0_SIXTH_EIGHTH,
    VRAM_G_MAIN_SPRITE_0_FIRST_EIGHTH,
    VRAM_G_MAIN_SPRITE_0_SECOND_EIGHTH,
    VRAM_G_MAIN_SPRITE_0_FIFTH_EIGHTH,
    VRAM_G_MAIN_SPRITE_0_SIXTH_EIGHTH,
};

enum VRAM_H_TYPE {
    VRAM_H_SUB_BG_FIRST_QUARTER,
};

enum VRAM_I_TYPE {
    VRAM_I_SUB_BG_THIRD_EIGHTH,
    VRAM_I_SUB_SPRITE_FIRST_EIGHTH
}
#endif    
    
#endif
