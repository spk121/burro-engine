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
    
    
    
