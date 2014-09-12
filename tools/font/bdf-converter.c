/****************************************************************************
 * tools/bdf-converter.c
 *
 *   Copyright (C) 2011 NX Engineering, S.A., All rights reserved.
 *   Author: Jose Pablo Carballo Gomez <jcarballo@nx-engineering.com>
 *
 *   Copyright (C) 2013 Michael L. Gran <spk121@yahoo.com>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 * 3. Neither the name NuttX nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 ****************************************************************************/

/* 
 * Based one the "Glyph Bitmap Distribution Format (BDF) Specification",
 * Version 2.2, by Adobe Systems Incorporated.
 *
 */
 
/****************************************************************************
 * Included Files

 ****************************************************************************/

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

/****************************************************************************
 * Pre-Processor Definitions
 ****************************************************************************/

/*
#define VERBOSE
#define DBG
*/
 
// BDF Specification Version 2.2:
// This version lifts the restriction on line length. In this version, the new
// maximum length of a value of the type string is 65535 characters, and hence
// lines may now be at least this long.

#define BDF_MAX_LINE_LENGTH 65535

/* Ranges of interest to the Videotex repertoire */

#define ASCII_MIN 0x20
#define ASCII_MAX 0x7F

#define LATIN1_MIN 0xA0
#define LATIN1_MAX 0xFF

#define LATIN_EXTENDED_A_MIN 0x0100
#define LATIN_EXTENDED_A_MAX 0X017f

#define SPACING_MODIFIER_LETTERS_MIN 0X02B0
#define SPACING_MODIFIER_LETTERS_MAX 0X02FF

#define GENERAL_PUNCTUATION_MIN 0x2000
#define GENERAL_PUNCTUATION_MAX 0x206F

#define LETTERLIKE_SYMBOLS_MIN 0x2100
#define LETTERLIKE_SYMBOLS_MAX 0x214F

#define NUMBER_FORMS_MIN 0x2150
#define NUMBER_FORMS_MAX 0x218F

#define ARROWS_MIN 0x2190
#define ARROWS_MAX 0x21FF

#define BLOCK_ELEMENTS_MIN 0x2500
#define BLOCK_ELEMENTS_MAX 0x257f

#define MISCELLANEOUS_SYMBOLS_AND_ARROWS_MIN 0x2b00
#define MISCELLANEOUS_SYMBOLS_AND_ARROWS_MAX 0x2bff

#define GEOMETRIC_SHAPES_MIN 0x25a0
#define GEOMETRIC_SHAPES_MAX 0x25ff

#define MISCELLANEOUS_MIN 0x2600
#define MISCELLANEOUS_MAX 0x26ff

#define PUA_MIN 0xE000
#define PUA_MAX 0xF8FF

#define MAX16BIT 0xFFFC

/****************************************************************************
 * Public Types
 ****************************************************************************/

/* This structure holds information about a glyph */

typedef struct glyphinfo_s
{
  char *name;       /* Name for they glyph */
  int   encoding;   /* The Adobe Standard Encoding value */
  int   dw_x0;      /* Width in x of the vector indicating
                     * the position of the next glyph's origin
                     * relative to the origin of this glyph */
  int   dw_y0;      /* Width in y of the vector indicating
                     * the position of the next glyph's origin
                     * relative to the origin of this glyph */
  int   bb_w;       /* The width of the black pixels in x */
  int   bb_h;       /* The height of the black pixels in y */
  int   bb_x_off;   /* X displacement of the lower left corner
                     * of the bitmap from origin 0 */
  int   bb_y_off;   /* Y displacement of the lower left corner
                     * of the bitmap from origin 0 */
  uint64_t *bitmap; /* Hexadecimal data for the character bitmap */
} glyphinfo_t;

/* This structures provides the metrics for one glyph */

typedef struct nx_fontmetric_s
{
  uint32_t stride   : 3;    /* Width of one font row in bytes */
  uint32_t width    : 6;    /* Width of the font in bits */
  uint32_t height   : 6;    /* Height of the font in rows */
  uint32_t xoffset  : 6;    /* Top, left-hand corner X-offset in pixels */
  uint32_t yoffset  : 6;    /* Top, left-hand corner y-offset in pixels */
  uint32_t unused   : 5;
} nx_fontmetric_t;

/****************************************************************************
 * Private Functions
 ****************************************************************************/

/****************************************************************************
 * Name: trimLine
 *
 * Description:
 *   Trims the line removing space characters at the front and at the end
 *   of the line.
 *
 * Input Parameters:
 *   line  - The line to trim
 *
 ****************************************************************************/
static void trimLine(char *line)
{
  char *str;
  str = line;
  char *strEnd;
  for (strEnd = str + strlen(str) - 1;
       strEnd >= str && isspace((int)(*strEnd));
       strEnd--);
  *(strEnd + 1) = 0;
}

/****************************************************************************
 * Name: bdf_parseIntLine
 *
 * Description:
 *   Parses a line containing a BDF property followed by integers. It will
 *   ignore the first token that corresponds to the property name.
 *
 * Input Parameters:
 *   line  - A line with a BDF property followed by integers, i.e.:
 *                  "FONTBOUNDINGBOX 8 13 0 -2"
 *   count - How many integers are specified by the BDF property. In the
 *           example above, count = 4.
 *   info  - A pointer to memory provided by the caller in which to
 *           return the array of integers. For the example above:
 *                info[0] =  8
 *                info[1] = 13
 *                info[2] =  0
 *                info[3] = -2
 *
 ****************************************************************************/
static void bdf_parseintline(char *line, unsigned int count, int *info)
{
  char *str, *token, *saveptr1;
  str = line;
  
  /* Ignore the key */
  
  token = (char *)strtok_r(str, " ", &saveptr1);
  
  while ((token = (char *)strtok_r(NULL, " ", &saveptr1)) && count--)
    {
      *(info++) = atoi(token);
    }
}

/****************************************************************************
 * Name: bdf_printglyphinfo
 *
 * Description:
 *   Prints the information available for a glyph.
 *
 * Input Parameters:
 *   ginfo  - A glyphinfo_t struct with the glyph's information.
 *
 ****************************************************************************/
#ifdef DBG
static void bdf_printglyphinfo(const glyphinfo_t *ginfo)
{
  printf("NAME     = %s\n", ginfo->name);
  printf("ENCODING = %d\n", ginfo->encoding);
  printf("DW_X0    = %d\n", ginfo->dw_x0);
  printf("DW_Y0    = %d\n", ginfo->dw_y0);
  printf("BB_W     = %d\n", ginfo->bb_w);
  printf("BB_H     = %d\n", ginfo->bb_h);
  printf("BB_X_OFF = %d\n", ginfo->bb_x_off);
  printf("BB_Y_OFF = %d\n", ginfo->bb_y_off);
  int i;
  for (i = 0; i < ginfo->bb_h; i++)
    {
      printf("BITMAP[%d] = %x\n", i, ginfo->bitmap[i]);
    }
}
#endif /* DBG */

/****************************************************************************
 * Name: bdf_printnxmetricinfo
 *
 * Description:
 *   Prints the information available for a glyph's metric in the NX
 *   graphics system.
 *
 * Input Parameters:
 *   info  - A nx_fontmetric_t struct with the glyph's information.
 *
 ****************************************************************************/
#ifdef DBG
static void bdf_printnxmetricinfo(const nx_fontmetric_t *info)
{
  printf("STRIDE  = %d\n", info->stride);
  printf("WIDTH   = %d\n", info->width);
  printf("HEIGHT  = %d\n", info->height);
  printf("XOFFSET = %d\n", info->xoffset);
  printf("YOFFSET = %d\n", info->yoffset);
}
#endif /* DBG */

/****************************************************************************
 * Name: bdf_getglyphinfo
 *
 * Description:
 *   Obtains the information for an individual glyph. The BDF properties
 *   taken into account are:
 *     - ENCODING
 *     - DWIDTH
 *     - BBX
 *   BDF properties ignored:
 *     - SWIDTH
 *     - SWIDTH1
 *     - DWIDTH1
 *     - VVECTOR
 *
 * Input Parameters:
 *   file  - The input file stream pointing to the first line of the
 *           glyph's information (right after STARTCHAR). 
 *   ginfo - A glyphinfo_t struct to fill with the glyph's information.
 *
 ****************************************************************************/
static void bdf_getglyphinfo(FILE *file, glyphinfo_t *ginfo)
{
  char line[BDF_MAX_LINE_LENGTH];
  char lineCopy[BDF_MAX_LINE_LENGTH];
  char *str, *token, *saveptr1;
  bool done;
  
  done = false;
  
  while(fgets(line, BDF_MAX_LINE_LENGTH, file) != NULL && !done)
    {
      trimLine(line);
      strcpy(lineCopy, line);
      str = line;
      
      while ((token = (char *)strtok_r(str, " ", &saveptr1)))
        {

          /* ENCODING information */
          
          if(strcmp(token, "ENCODING") == 0)
            {
              token = (char *)strtok_r(NULL, " ", &saveptr1);
              ginfo->encoding = atoi(token);
            }
            
          /* DWIDTH information */
          
          if(strcmp(token, "DWIDTH") == 0)
            {
              token = (char *)strtok_r(NULL, " ", &saveptr1);
              ginfo->dw_x0 = atoi(token);
              token = (char *)strtok_r(NULL, " ", &saveptr1);
              ginfo->dw_y0 = atoi(token);
            }
            
          /* BBX information */
          
          else if(strcmp(token, "BBX") == 0)
            {
              int bbxinfo[4];
              bdf_parseintline(lineCopy, 4, bbxinfo);
              ginfo->bb_w     = bbxinfo[0];
              ginfo->bb_h     = bbxinfo[1];
              ginfo->bb_x_off = bbxinfo[2];
              ginfo->bb_y_off = bbxinfo[3];
              
              /* This is the last BDF property of interest*/
              
              done = true;
            }

          str = NULL;
        }
      
    }
}

/****************************************************************************
 * Name: bdf_getglyphbitmap
 *
 * Description:
 *   Obtains the character bitmap information for an individual glyph.
 *
 * Input Parameters:
 *   file  - The input file stream pointing to the first line of the
 *           glyph's bitmap (right after BITMAP). 
 *   ginfo - A glyphinfo_t struct to fill with the glyph's bitmap.
 *
 ****************************************************************************/
static void bdf_getglyphbitmap(FILE *file, glyphinfo_t *ginfo)
{
  char line[BDF_MAX_LINE_LENGTH];
  uint64_t *bitmap;
  bool readingbitmap;
  
  bitmap = ginfo->bitmap;
  readingbitmap = true;
  
  while (readingbitmap)
    {
      if (fgets(line, BDF_MAX_LINE_LENGTH, file) != NULL)
      {
        trimLine(line);
      
        if(strcmp(line, "ENDCHAR") == 0)
          {
            readingbitmap = false;
          }
        else
          {
            char *endptr;
            *bitmap = strtoul(line, &endptr, 16);
            bitmap++;
          }
          
      }
      else
      {
        /* error condition */
        
        readingbitmap = false;
      }
       
    }
}

/****************************************************************************
 * Name: bdf_getstride
 *
 * Description:
 *   Obtains the stride for an individual glyph. The stride is the width
 *   of one glyph's bitmap row in bytes.
 *
 * Input Parameters:
 *   ginfo  - A glyphinfo_t struct with the glyph's information.
 *   stride - A pointer to memory provided by the caller in which to
 *            return the stride.
 *
 ****************************************************************************/
static void bdf_getstride(glyphinfo_t *ginfo, uint32_t *stride)
{
  *stride = (ginfo->bb_w % 8 == 0) ? ginfo->bb_w / 8 : ginfo->bb_w / 8 + 1;
}

/****************************************************************************
 * Name: bdf_printoutput
 *
 * Description:
 *   Prints to the output stream the information of an individual glyph in
 *   the NuttX font format.
 *
 * Input Parameters:
 *   out       - The output stream.
 *   ginfo     - A glyphinfo_t struct with the glyph's information.
 *   nxmetric  - A nx_fontmetric_t struct with the glyph's information.
 *
 ****************************************************************************/
static int  bdf_printoutput(FILE *out, 
                            glyphinfo_t *ginfo,
                            nx_fontmetric_t *nxmetric,
                            int fbb_x, int fbb_y)
{

  /* Only interested in the 7 and 8 bit ranges */
  
  if ((ginfo->encoding >= ASCII_MIN  && ginfo->encoding <= ASCII_MAX)
      || (ginfo->encoding >= LATIN1_MIN && ginfo->encoding <= LATIN1_MAX)
      || (ginfo->encoding >= LATIN_EXTENDED_A_MIN && ginfo->encoding <= LATIN_EXTENDED_A_MAX)
      || (ginfo->encoding >= SPACING_MODIFIER_LETTERS_MIN && ginfo->encoding <= SPACING_MODIFIER_LETTERS_MAX)
      || (ginfo->encoding >= GENERAL_PUNCTUATION_MIN && ginfo->encoding <= GENERAL_PUNCTUATION_MAX)
      || (ginfo->encoding >= LETTERLIKE_SYMBOLS_MIN && ginfo->encoding <= LETTERLIKE_SYMBOLS_MAX)
      || (ginfo->encoding >= NUMBER_FORMS_MIN && ginfo->encoding <= NUMBER_FORMS_MAX)
      || (ginfo->encoding >= ARROWS_MIN && ginfo->encoding <= ARROWS_MAX)
      || (ginfo->encoding >= BLOCK_ELEMENTS_MIN && ginfo->encoding <= BLOCK_ELEMENTS_MAX)
      || (ginfo->encoding >= MISCELLANEOUS_SYMBOLS_AND_ARROWS_MIN && ginfo->encoding <= MISCELLANEOUS_SYMBOLS_AND_ARROWS_MAX)
      || (ginfo->encoding >= GEOMETRIC_SHAPES_MIN && ginfo->encoding <= GEOMETRIC_SHAPES_MAX)
      || (ginfo->encoding >= MISCELLANEOUS_MIN && ginfo->encoding <= MISCELLANEOUS_MAX)
      || (ginfo->encoding >= PUA_MIN && ginfo->encoding <= PUA_MAX))
    {
      
      /* Glyph general info */
      
      if (ginfo->bb_x_off < 0)
        {
          fprintf(out,
                  "/* %s (%d) -- NOTE: Xoffset should be %d, not 0. */\n",
                  ginfo->name,
                  ginfo->encoding,
                  ginfo->bb_x_off);
        }
      else
        {
          fprintf(out, "/* %s (U+%04X) */\n", ginfo->name, ginfo->encoding);
        }
      
      /* Glyph metrics */
      
      fprintf(out,
              "#define FIXED%dx%d_METRICS_%d {%d, %d, %d, %d, %d, 0}\n",
              fbb_x,
              fbb_y,
              ginfo->encoding,
              nxmetric->stride,
              nxmetric->width,
              nxmetric->height,
              nxmetric->xoffset,
              nxmetric->yoffset);
              
      /* Glyph bitmap */
      
      fprintf(out, "#define FIXED%dx%d_BITMAP_%d {", fbb_x, fbb_y, ginfo->encoding);
      int i;
      for (i = 0; i < ginfo->bb_h - 1; i++)
        {
          uint64_t tempbitmap = ginfo->bitmap[i];
          
          /* Get the next byte */
          
          if (nxmetric->stride == 1)
            fprintf(out, "0x%02llX, ", tempbitmap);
          else if (nxmetric->stride == 2)
            fprintf(out, "0x%04llX, ", tempbitmap);
          else if (nxmetric->stride == 3)
            fprintf(out, "0x%06llX, ", tempbitmap);
          else if (nxmetric->stride == 4)
            fprintf(out, "0x%08llX, ", tempbitmap);
          else
            fprintf(out, "0x%0llX, ", tempbitmap);
        }
      
      /* Different behavior for the last bitmap */
      
      uint64_t tempbitmap = ginfo->bitmap[i];
      
      if (nxmetric->stride == 1)
        fprintf(out, "0x%02llX}\n", tempbitmap);
      else if (nxmetric->stride == 2)
        fprintf(out, "0x%04llX}\n", tempbitmap);
      else if (nxmetric->stride == 3)
        fprintf(out, "0x%06llX}\n", tempbitmap);
      else if (nxmetric->stride == 4)
        fprintf(out, "0x%08llX}\n", tempbitmap);
      else
        fprintf(out, "0x%llX", tempbitmap);
      
      fprintf(out, "\n");
      return 1;
    }
  return 0;
}

/****************************************************************************
 * Main
 ****************************************************************************/

int main(int argc, char **argv)
{
  FILE *file, *out;
  char line[BDF_MAX_LINE_LENGTH];
  char lineCopy[BDF_MAX_LINE_LENGTH];
  char *str, *token, *saveptr1;
  char *input, *output;
  char glyph[MAX16BIT];
  int i = 0, n = 0;
  
  /* FONTBOUNDINGBOX properties*/
  
  int fbb_x     = 0;
  int fbb_y     = 0;
  // int fbb_x_off = 0;
  int fbb_y_off = 0;
  
  /* Input BDF file */
  memset(glyph, 0, MAX16BIT);
  
  input = argv[1];
  
  if (input == NULL)
    {
      printf("%s: no input file\n", argv[0]);
      exit(0);
    }
    
  file = fopen(input, "r");
  
  if (file == NULL)
    {
      printf("%s: error opening file %s\n", argv[0], input);
      exit(0);
    }
  else
    {
#ifdef VERBOSE
      printf("Opening \"%s\"\n", input);
#endif /* VERBOSE */
    }
  
  /* Output file */
  if (argv[2])
    {
      output = argv[2];
    }
  else
    {
      output = "nxfonts_myfont.h";
    }
  
  out  = fopen(output, "w");
  
  if (out == NULL)
    {
      printf("%s: error opening file %s\n", argv[0], output);
      fclose(file);
      exit(0);
    }
  else
    {
      fprintf(out, "#include <stdint.h>\n");
      while (fgets(line, BDF_MAX_LINE_LENGTH, file) != NULL)
        {
        
#ifdef DBG
          printf("--\n");
#endif /* DBG */
          
          // Save a copy of the line
          
          strcpy(lineCopy,line);
          
          // Clean it
          
          trimLine(line);
          str = line;

          while ((token = (char *)strtok_r(str, " ", &saveptr1)))
            {
            
              /* FONTBOUNDINGBOX - Global font information */
            
              if (strcmp(token, "FONTBOUNDINGBOX") == 0)
                {
                  int fbbinfo[4];
                  bdf_parseintline(lineCopy, 4, fbbinfo);
                  fbb_x     = fbbinfo[0];
                  fbb_y     = fbbinfo[1];
                  // fbb_x_off = fbbinfo[2];
                  fbb_y_off = fbbinfo[3];
                  
                  /* Print FONTBOUNDINGBOX information */
                  
                  fprintf(out, "/* Maximum height and width of any");
                  fprintf(out, " glyph in the set */\n\n");
                  fprintf(out, "#define FIXED%dx%d_MAXHEIGHT  %d\n", fbb_x, fbb_y, fbb_y);
                  fprintf(out, "#define FIXED%dx%d_MAXWIDTH   %d\n\n", fbb_x, fbb_y, fbb_x);
                }
                
              /* STARTCHAR - Individual glyph information */
                
              if (strcmp(token, "STARTCHAR") == 0)
                {
                  glyphinfo_t ginfo;
                  
                  /* Glyph name */
                  
                  ginfo.name = (char *)strtok_r(NULL, " ", &saveptr1);

#ifdef VERBOSE
                  printf("Processing glyph: %s\n", ginfo.name);
#endif /* VERBOSE */
                  
                  /* Glyph information:
                  *    ENCODING
                  *    DWIDTH
                  *    BBX
                  */
                  ginfo.encoding = 0;
                  ginfo.dw_x0    = 0;
                  ginfo.dw_y0    = 0;
                  ginfo.bb_w     = 0;
                  ginfo.bb_h     = 0;
                  ginfo.bb_x_off = 0;
                  ginfo.bb_y_off = 0;
                  bdf_getglyphinfo(file, &ginfo);
                  
                  /* Glyph bitmap */
                  
                  ginfo.bitmap = malloc(sizeof(uint64_t) * ginfo.bb_h);
                  bdf_getglyphbitmap(file, &ginfo);
                  
#ifdef DBG
                  bdf_printglyphinfo(&ginfo);
#endif /* DBG */
                  
                  /* Convert to nxfonts */
                  
                  nx_fontmetric_t nxmetric;
                  uint32_t stride;
                  bdf_getstride(&ginfo, &stride);
                  nxmetric.stride  = stride;
                  nxmetric.width   = ginfo.bb_w;
                  nxmetric.height  = ginfo.bb_h;

                  /* The NuttX font format does not support
                   * negative X offsets. */
                  
                  if (ginfo.bb_x_off < 0)
                    {
                      nxmetric.xoffset = 0;
                      printf("%s: ignoring negative x offset for "
                             "glyph '%s' (%d)\n",
                             argv[0],
                             ginfo.name,
                             ginfo.encoding);
                    }
                  else
                    {
                      nxmetric.xoffset = ginfo.bb_x_off;
                    }
                    
                  nxmetric.yoffset = fbb_y + fbb_y_off -
                                     ginfo.bb_y_off - ginfo.bb_h;
                                     
                  
#ifdef DBG                  
                  bdf_printnxmetricinfo(&nxmetric);
#endif /* DBG */

                  /* The space (32) character is treated differently */

                  if (ginfo.encoding == 32)
                    {
                      fprintf(out, "/* The width of a space */\n\n");
                      fprintf(out, "#define FIXED%dx%d_SPACEWIDTH %d\n\n", fbb_x, fbb_y, ginfo.dw_x0);
                      if (bdf_printoutput(out, &ginfo, &nxmetric, fbb_x, fbb_y))
                      {
                          glyph[ginfo.encoding] = 1;
                          n++;
                      }
                    }
                  else
                    {
                        if (bdf_printoutput(out, &ginfo, &nxmetric, fbb_x, fbb_y))
                        {
                          glyph[ginfo.encoding] = 1;
                          n++;
                        }
                    }
                  
                  /* Free memory */
                  
                  free(ginfo.bitmap);
                  
                }
              
              str = NULL;
            }
          
        }
      fclose(file);

      /* FIXME: should be getting this data from the glyph's stride? */
      if (fbb_x <= 8 * sizeof(uint8_t))
          fprintf (out, "typedef uint8_t glyph_fixed%dx%d_row_t;\n", fbb_x, fbb_y);
      else if (fbb_x <= 8 * sizeof(uint16_t))
          fprintf (out, "typedef uint16_t glyph_fixed%dx%d_row_t;\n", fbb_x, fbb_y);
      else if (fbb_x <= 8 * sizeof(uint32_t))
          fprintf (out, "typedef uint32_t glyph_fixed%dx%d_row_t;\n", fbb_x, fbb_y);
      else if (fbb_x <= 8 * sizeof(uint64_t))
          fprintf (out, "typedef uint64_t glyph_fixed%dx%d_row_t;\n", fbb_x, fbb_y);
      fprintf (out, "\n");

      fprintf (out, "#define FIXED%dx%d_COUNT (%d)\n", fbb_x, fbb_y, n);
      fprintf (out, "\n");

      fprintf (out, "typedef struct glyph_data_s\n");
      fprintf (out, "{\n");
      fprintf (out, "  uint32_t stride   : 3;    /* Width of one font row in bytes */\n");
      fprintf (out, "  uint32_t width    : 6;    /* Width of the font in bits */\n");
      fprintf (out, "  uint32_t height   : 6;    /* Height of the font in rows */\n");
      fprintf (out, "  uint32_t xoffset  : 6;    /* Top, left-hand corner X-offset in pixels */\n");
      fprintf (out, "  uint32_t yoffset  : 6;    /* Top, left-hand corner y-offset in pixels */\n");
      fprintf (out, "  uint32_t unused   : 5;\n");
      fprintf (out, "} glyph_data_t;\n");
      fprintf (out, "\n");
      fprintf (out, "typedef struct glyph_s\n");
      fprintf (out, "{\n");
      fprintf (out, "  int encoding;\n");
      fprintf (out, "  glyph_data_t data;\n");
      fprintf (out, "  glyph_fixed%dx%d_row_t bitmap[FIXED%dx%d_MAXHEIGHT];\n", fbb_x, fbb_y, fbb_x, fbb_y);
      fprintf (out, "} glyph_fixed%dx%d_t;\n", fbb_x, fbb_y);
      fprintf (out, "\n");
      fprintf (out, "glyph_fixed%dx%d_t fixed%dx%d_glyphs[%d] = {\n", fbb_x, fbb_y, fbb_x, fbb_y, n);
      for (i = 0; i < MAX16BIT; i ++)
        {
          if (glyph[i]) {
              fprintf (out, "  {0x%04x, FIXED%dx%d_METRICS_%d, FIXED%dx%d_BITMAP_%d},\n", i, fbb_x, fbb_y, i, 
                       fbb_x, fbb_y, i);
          }
        }
      fprintf (out, "};\n");
      fclose(out);
      
      /* The End */
      
      printf("Generated \"%s\"\n", output);
      
    }
    
  return EXIT_SUCCESS;
}
