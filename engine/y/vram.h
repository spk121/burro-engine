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


