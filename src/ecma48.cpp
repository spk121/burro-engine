
#line 1 "ecma48.rl"
// -*- coding: utf-8-unix -*-

// This is an ECMA-48-compliant parser.  It takes a string of
// text data with embedded escape commands and control characters
// and converts it into a set of terminal actions.

// ECMA-48 is a huge standard and is a mixture of printer-specific and
// terminal-specific actions.  Most of the commands are going to
// remain unimplemented.

// ECMA-48 draws a distinction between 'DATA' and 'PRESENTATION'.
// DATA is the order in which characters are received and stored.
// PRESENTATION is the coordinate system of the screen.  For English
// they are one and the same.

#include <stdint.h>
#include <SDL.h>
#include "const.h"
//#include "codes.h"
#include "console.hpp"

#define G0 0
#define G1 1
#define G2 2
#define G3 3
int GL = G0;
int GR = G1;

// SIMD - SELECT IMPLICIT MOVEMENT DIRECTION.
// 0 = movement in same direction of character progression.
// 1 = movement in opposite direction of character progression.
static int simd = 0;

// IRM - INSERTION REPLACEMENT MODE
// 0 = graphical characters replace.
// 1 = graphical characters insert
#define IRM_REPLACE 0
#define IRM_INSERT 1
static int irm = IRM_REPLACE;

// HEM - CHARACTER EDITING MODE
#define HEM_FOLLOWING 0
#define HEM_PRECEDING 1
static int hem = 0;
// VEM - VERTICAL CHARACTER EDITING MODE
static int vem = 0;

// GRCM - GRAPHIC RENDITION COMBINATION MODE
#define GRCM_REPLACING 1
#define GRCM_COMBINING 0
static int grcm = GRCM_COMBINING;

// The "home" column.  The left edge of the text area.
static int line_home = 1;

// The "line limit" column.  The right edge of the text area.
static int line_limit = CONSOLE_COLS;

// The "page_home" line.  The top of the text area.
static int page_home = 1;

// The "page_limit" line.  The bottom of text area.
static int page_limit = CONSOLE_ROWS;

// These are locations where parsed parameters from the data
// stream are stored.
#define PARAMS 4
static int P[PARAMS];
static int P_size = 0;

static int p1(int nominal)
{
  if (P_size >= 1) {
    return P[0];
  }
  else
    return nominal;
}

static int p2(int nominal)
{
  if (P_size >= 2)
    return P[1];
  else
    return nominal;
}

static int cs;

#if 0
static void reparse_P() {
    unsigned char *pos = fpc;
    int i, n;
    P_size = 0;
    
    pos --;
    while (*pos >= '0' && *pos <= '9' || *pos == ';') {
      pos --;
    }

  loop:
    n = 0;
    i = 1;
    while (*pos >= '0' && *pos <= '9') {
      n += (*pos - '0') * i;
      i *= 10;
      pos ++;
    }
    P[P_size] = n;
    P_size ++;
    SDL_assert(P_size < PARAMS);
    if (*pos == ';')
      goto loop;
    fpc = pos;
}
#endif

static int codepoint_tables[4][96] =
  {
    { // ISO-646.IRV
      // ISO-IR-2
      0x0020, // SPACE
      0x0021, //  EXCLAMATION MARK
      0x0022, //  QUOTATION MARK
      0x0023, //  NUMBER SIGN
      0x00A4, //  CURRENCY SIGN
      0x0025, //  PERCENT SIGN
      0x0026, //  AMPERSAND
      0x0027, //  APOSTROPHE
      0x0028, //  LEFT PARENTHESIS
      0x0029, //  RIGHT PARENTHESIS
      0x002A, //  ASTERISK
      0x002B, //  PLUS SIGN
      0x002C, //  COMMA
      0x002D, //  HYPHEN-MINUS
      0x002E, //  FULL STOP
      0x002F, //  SOLIDUS
      0x0030, //  DIGIT ZERO
      0x0031, //  DIGIT ONE
      0x0032, //  DIGIT TWO
      0x0033, //  DIGIT THREE
      0x0034, //  DIGIT FOUR
      0x0035, //  DIGIT FIVE
      0x0036, //  DIGIT SIX
      0x0037, //  DIGIT SEVEN
      0x0038, //  DIGIT EIGHT
      0x0039, //  DIGIT NINE
      0x003A, //  COLON
      0x003B, //  SEMICOLON
      0x003C, //  LESS-THAN SIGN
      0x003D, //  EQUALS SIGN
      0x003E, //  GREATER-THAN SIGN
      0x003F, //  QUESTION MARK
      0x0040, //  COMMERCIAL AT
      0x0041, //  LATIN CAPITAL LETTER A
      0x0042, //  LATIN CAPITAL LETTER B
      0x0043, //  LATIN CAPITAL LETTER C
      0x0044, //  LATIN CAPITAL LETTER D
      0x0045, //  LATIN CAPITAL LETTER E
      0x0046, //  LATIN CAPITAL LETTER F
      0x0047, //  LATIN CAPITAL LETTER G
      0x0048, //  LATIN CAPITAL LETTER H
      0x0049, //  LATIN CAPITAL LETTER I
      0x004A, //  LATIN CAPITAL LETTER J
      0x004B, //  LATIN CAPITAL LETTER K
      0x004C, //  LATIN CAPITAL LETTER L
      0x004D, //  LATIN CAPITAL LETTER M
      0x004E, //  LATIN CAPITAL LETTER N
      0x004F, //  LATIN CAPITAL LETTER O
      0x0050, //  LATIN CAPITAL LETTER P
      0x0051, //  LATIN CAPITAL LETTER Q
      0x0052, //  LATIN CAPITAL LETTER R
      0x0053, //  LATIN CAPITAL LETTER S
      0x0054, //  LATIN CAPITAL LETTER T
      0x0055, //  LATIN CAPITAL LETTER U
      0x0056, //  LATIN CAPITAL LETTER V
      0x0057, //  LATIN CAPITAL LETTER W
      0x0058, //  LATIN CAPITAL LETTER X
      0x0059, //  LATIN CAPITAL LETTER Y
      0x005A, //  LATIN CAPITAL LETTER Z
      0x005B, //  LEFT SQUARE BRACKET
      0x005C, //  REVERSE SOLIDUS
      0x005D, //  RIGHT SQUARE BRACKET
      0x005E, //  CIRCUMFLEX ACCENT
      0x005F, //  LOW LINE
      0x0060, //  GRAVE ACCENT
      0x0061, //  LATIN SMALL LETTER A
      0x0062, //  LATIN SMALL LETTER B
      0x0063, //  LATIN SMALL LETTER C
      0x0064, //  LATIN SMALL LETTER D
      0x0065, //  LATIN SMALL LETTER E
      0x0066, //  LATIN SMALL LETTER F
      0x0067, //  LATIN SMALL LETTER G
      0x0068, //  LATIN SMALL LETTER H
      0x0069, //  LATIN SMALL LETTER I
      0x006A, //  LATIN SMALL LETTER J
      0x006B, //  LATIN SMALL LETTER K
      0x006C, //  LATIN SMALL LETTER L
      0x006D, //  LATIN SMALL LETTER M
      0x006E, //  LATIN SMALL LETTER N
      0x006F, //  LATIN SMALL LETTER O
      0x0070, //  LATIN SMALL LETTER P
      0x0071, //  LATIN SMALL LETTER Q
      0x0072, //  LATIN SMALL LETTER R
      0x0073, //  LATIN SMALL LETTER S
      0x0074, //  LATIN SMALL LETTER T
      0x0075, //  LATIN SMALL LETTER U
      0x0076, //  LATIN SMALL LETTER V
      0x0077, //  LATIN SMALL LETTER W
      0x0078, //  LATIN SMALL LETTER X
      0x0079, //  LATIN SMALL LETTER Y
      0x007A, //  LATIN SMALL LETTER Z
      0x007B, //  LEFT CURLY BRACKET
      0x007C, //  VERTICAL LINE
      0x007D, //  RIGHT CURLY BRACKET
      0x203E, //  OVERLINE
      0x007F, //  <delete>
    },
    {
      // VIDEOTEX-SUPPL
      // ISO-IR-70
      0x0020, // SPACE
      0x00A1, //  INVERTED EXCLAMATION MARK
      0x00A2, //  CENT SIGN
      0x00A3, //  POUND SIGN
      0x0024, //  DOLLAR SIGN
      0x00A5, //  YEN SIGN
      0x0023, //  NUMBER SIGN
      0x00A7, //  SECTION SIGN
      0x00A4, //  CURRENCY SIGN
      0x2018, //  LEFT SINGLE QUOTATION MARK
      0x201C, //  LEFT DOUBLE QUOTATION MARK
      0x00AB, //  LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      0x2190, //  LEFTWARDS ARROW
      0x2191, //  UPWARDS ARROW
      0x2192, //  RIGHTWARDS ARROW
      0x2193, //  DOWNWARDS ARROW
      0x00B0, //  DEGREE SIGN
      0x00B1, //  PLUS-MINUS SIGN
      0x00B2, //  SUPERSCRIPT TWO
      0x00B3, //  SUPERSCRIPT THREE
      0x00D7, //  MULTIPLICATION SIGN
      0x00B5, //  MICRO SIGN
      0x00B6, //  PILCROW SIGN
      0x00B7, //  MIDDLE DOT
      0x00F7, //  DIVISION SIGN
      0x2019, //  RIGHT SINGLE QUOTATION MARK
      0x201D, //  RIGHT DOUBLE QUOTATION MARK
      0x00BB, //  RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      0x00BC, //  VULGAR FRACTION ONE QUARTER
      0x00BD, //  VULGAR FRACTION ONE HALF
      0x00BE, //  VULGAR FRACTION THREE QUARTERS
      0x00BF, //  INVERTED QUESTION MARK
      0x0344, //  COMBINING DIAERESIS WITH ACCENT (Greek dialytica tonos)
      0x0300, //  COMBINING GRAVE ACCENT
      0x0301, //  COMBINING ACUTE ACCENT
      0x0302, //  COMBINING CIRCUMFLEX ACCENT
      0x0303, //  COMBINING TILDE
      0x0304, //  COMBINING MACRON
      0x0306, //  COMBINING BREVE
      0x0307, //  COMBINING DOT ABOVE
      0x0308, //  COMBINING DIAERESIS
      0x030a, //  COMBINING RING ABOVE
      0x0327, //  COMBINING CEDILLA
      0x030b, //  COMBINING DOUBLE ACUTE ACCENT
      0x0328, //  COMBINING OGONEK
      0x030c, //  COMBINING CARON
      0x2014, //  EM DASH
      0x00B9, //  SUPERSCRIPT ONE
      0x00AE, //  REGISTERED SIGN
      0x00A9, //  COPYRIGHT SIGN
      0x2122, //  TRADE MARK SIGN
      0x266A, //  EIGHTH NOTE
      0x215B, //  VULGAR FRACTION ONE EIGHTH
      0x215C, //  VULGAR FRACTION THREE EIGHTHS
      0x215D, //  VULGAR FRACTION FIVE EIGHTHS
      0x215E, //  VULGAR FRACTION SEVEN EIGHTHS
      0x2126, //  OHM SIGN
      0x00C6, //  LATIN CAPITAL LETTER AE
      0x00D0, //  LATIN CAPITAL LETTER ETH (Icelandic)
      0x00AA, //  FEMININE ORDINAL INDICATOR
      0x0126, //  LATIN CAPITAL LETTER H WITH STROKE
      0x0132, //  LATIN CAPITAL LIGATURE IJ
      0x013F, //  LATIN CAPITAL LETTER L WITH MIDDLE DOT
      0x0141, //  LATIN CAPITAL LETTER L WITH STROKE
      0x00D8, //  LATIN CAPITAL LETTER O WITH STROKE
      0x0152, //  LATIN CAPITAL LIGATURE OE
      0x00BA, //  MASCULINE ORDINAL INDICATOR
      0x00DE, //  LATIN CAPITAL LETTER THORN (Icelandic)
      0x0166, //  LATIN CAPITAL LETTER T WITH STROKE
      0x014A, //  LATIN CAPITAL LETTER ENG (Sami)
      0x0149, //  LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
      0x0138, //  LATIN SMALL LETTER KRA (Greenlandic)
      0x00E6, //  LATIN SMALL LETTER AE
      0x0111, //  LATIN SMALL LETTER D WITH STROKE
      0x00F0, //  LATIN SMALL LETTER ETH (Icelandic)
      0x0127, //  LATIN SMALL LETTER H WITH STROKE
      0x0131, //  LATIN SMALL LETTER DOTLESS I
      0x0133, //  LATIN SMALL LIGATURE IJ
      0x0140, //  LATIN SMALL LETTER L WITH MIDDLE DOT
      0x0142, //  LATIN SMALL LETTER L WITH STROKE
      0x00F8, //  LATIN SMALL LETTER O WITH STROKE
      0x0153, //  LATIN SMALL LIGATURE OE
      0x00DF, //  LATIN SMALL LETTER SHARP S (German)
      0x00FE, //  LATIN SMALL LETTER THORN (Icelandic)
      0x0167, //  LATIN SMALL LETTER T WITH STROKE
      0x014B,  //  LATIN SMALL LETTER ENG (Sami)
      0x007F, // <delete>
    },
    {
      // VIDEOTEX-SUPPL
      // "ISO-IR-71"
      0x0020, // SPACE
      0xE101, //  MG01
      0xE102, //  MG02
      0xE103, //  MG03
      0xE104, //  MG04
      0xE105, //  MG05
      0xE106, //  MG06
      0xE107, //  MG07
      0xE108, //  MG08
      0xE109, //  MG09
      0xE10A, //  MG10
      0xE10B, //  MG11
      0xE10C, //  MG12
      0xE10D, //  MG13
      0xE10E, //  MG14
      0xE10F, //  MG15
      0xE110, //  MG16
      0xE111, //  MG17
      0xE112, //  MG18
      0xE113, //  MG19
      0xE114, //  MG20
      0xE115, //  MG21
      0xE116, //  MG22
      0xE117, //  MG23
      0xE118, //  MG24
      0xE119, //  MG25
      0xE11A, //  MG26
      0xE11B, //  MG27
      0xE11C, //  MG28
      0xE11D, //  MG29
      0xE11E, //  MG30
      0xE11F, //  MG31
      0xE201, //  SG01
      0xE202, //  SG02
      0xE203, //  SG03
      0xE204, //  SG04
      0xE205, //  SG05
      0xE206, //  SG06
      0xE207, //  SG07
      0xE208, //  SG08
      0xE209, //  SG09
      0xE20A, //  SG10
      0xE20B, //  SG11
      0xE20C, //  SG12
      0xE20D, //  SG13
      0xE20E, //  SG14
      0xE42D, //  SM45
      0xE320, //  DG32
      0xE21C, //  SG28
      0xE21b, //  SG27
      0xE21a, //  SG26
      0xE219, //  SG25
      0xE218, //  SG24
      0xE217, //  SG23
      0xE216, //  SG22
      0xE215, //  SG21
      0xE214, //  SG20
      0xE213, //  SG19
      0xE212, //  SG18
      0xE211, //  SG17
      0xE210, //  SG16
      0xE20f, //  SG15
      0xE32e, //  SM46
      0xE13f, //  MG63
      0xE120, //  MG32
      0xE121, //  MG33
      0xE122, //  MG34
      0xE123, //  MG35
      0xE124, //  MG36
      0xE125, //  MG37
      0xE126, //  MG38
      0xE127, //  MG39
      0xE128, //  MG40
      0xE129, //  MG41
      0xE12a, //  MG42
      0xE12b, //  MG43
      0xE12c, //  MG44
      0xE12d, //  MG45
      0xE12e, //  MG46
      0xE12f, //  MG47
      0xE130, //  MG48
      0xE131, //  MG49
      0xE132, //  MG50
      0xE133, //  MG51
      0xE134, //  MG52
      0xE135, //  MG53
      0xE136, //  MG54
      0xE137, //  MG55
      0xE138, //  MG56
      0xE139, //  MG57
      0xE13a, //  MG58
      0xE13b, //  MG59
      0xE13c, //  MG60
      0xE13d, //  MG61
      0xE13e, //  MG62
      0x007F, // <delete>
    },
    {
      // MOSAIC3
      // ISO-IR-72
      0x0020, // SPACE
      0xE220, //  SG32
      0xE221, //  SG33
      0xE222, //  SG34
      0xE223, //  SG35
      0xE224, //  SG36
      0xE225, //  SG37
      0xE226, //  SG38
      0xE227, //  SG39
      0xE228, //  SG40
      0xE229, //  SG41
      0xE22a, //  SG42
      0xE22b, //  SG43
      0xE22c, //  SG44
      0xE22d, //  SG45
      0xE22e, //  SG46
      0xE22f, //  SG47
      0xE230, //  SG48
      0xE231, //  SG49
      0xE232, //  SG50
      0xE233, //  SG51
      0xE234, //  SG52
      0xE235, //  SG53
      0xE236, //  SG54
      0xE237, //  SG55
      0xE238, //  SG56
      0xE239, //  SG57
      0xE23a, //  SG58
      0xE23b, //  SG59
      0xE23c, //  SG60
      0xE23d, //  SG61
      0xE23e, //  SG62
      0x007F, // <delete>
    }
  };




#line 1720 "ecma48.rl"



#line 459 "ecma48.cpp"
static const unsigned char _ECMA48_actions[] = {
	0, 1, 1, 1, 2, 1, 3, 1, 
	4, 1, 5, 1, 6, 1, 7, 1, 
	8, 1, 9, 1, 10, 1, 11, 1, 
	12, 1, 13, 1, 14, 1, 15, 1, 
	16, 1, 17, 1, 18, 1, 19, 1, 
	20, 1, 21, 1, 22, 1, 23, 1, 
	24, 1, 25, 1, 26, 1, 27, 1, 
	28, 1, 29, 1, 31, 1, 32, 1, 
	34, 1, 35, 1, 36, 1, 38, 1, 
	39, 1, 40, 1, 41, 1, 46, 1, 
	47, 1, 48, 1, 49, 1, 50, 1, 
	51, 1, 52, 1, 54, 1, 55, 1, 
	56, 1, 57, 1, 58, 1, 59, 1, 
	61, 1, 62, 1, 63, 1, 64, 1, 
	65, 1, 66, 1, 67, 1, 68, 1, 
	69, 1, 70, 1, 71, 1, 72, 1, 
	73, 1, 74, 1, 77, 1, 78, 1, 
	79, 1, 80, 1, 84, 1, 85, 1, 
	86, 1, 88, 1, 89, 1, 90, 1, 
	91, 1, 93, 1, 97, 1, 98, 1, 
	99, 1, 102, 1, 108, 1, 109, 1, 
	117, 1, 118, 1, 121, 1, 122, 1, 
	124, 1, 125, 1, 126, 1, 135, 1, 
	136, 1, 137, 1, 138, 1, 139, 1, 
	140, 1, 141, 2, 7, 140, 2, 9, 
	140, 2, 10, 140, 2, 11, 140, 2, 
	12, 140, 2, 13, 140, 2, 15, 140, 
	2, 16, 140, 2, 17, 140, 2, 18, 
	140, 2, 19, 140, 2, 20, 140, 2, 
	21, 140, 2, 22, 140, 2, 23, 140, 
	2, 24, 140, 2, 25, 108, 2, 30, 
	37, 2, 32, 140, 2, 33, 140, 2, 
	34, 140, 2, 35, 140, 2, 36, 140, 
	2, 38, 140, 2, 42, 140, 2, 43, 
	140, 2, 44, 140, 2, 45, 140, 2, 
	46, 140, 2, 47, 140, 2, 48, 140, 
	2, 52, 140, 2, 54, 140, 2, 60, 
	140, 2, 68, 140, 2, 72, 140, 2, 
	75, 140, 2, 76, 140, 2, 80, 140, 
	2, 81, 140, 2, 82, 140, 2, 83, 
	140, 2, 84, 140, 2, 87, 140, 2, 
	88, 140, 2, 91, 140, 2, 92, 140, 
	2, 94, 140, 2, 95, 140, 2, 96, 
	140, 2, 97, 140, 2, 98, 140, 2, 
	99, 140, 2, 100, 140, 2, 101, 140, 
	2, 102, 140, 2, 103, 140, 2, 104, 
	140, 2, 105, 140, 2, 106, 140, 2, 
	107, 131, 2, 107, 140, 2, 108, 2, 
	2, 108, 74, 2, 108, 79, 2, 108, 
	140, 2, 111, 140, 2, 112, 140, 2, 
	113, 140, 2, 114, 140, 2, 115, 140, 
	2, 116, 140, 2, 117, 140, 2, 119, 
	140, 2, 120, 140, 2, 123, 140, 2, 
	125, 140, 2, 128, 140, 2, 129, 140, 
	2, 130, 140, 2, 133, 140, 2, 134, 
	140, 2, 135, 140, 2, 136, 140, 2, 
	137, 140, 2, 140, 1, 2, 140, 3, 
	2, 140, 4, 2, 140, 5, 2, 140, 
	6, 2, 140, 8, 2, 140, 14, 2, 
	140, 26, 2, 140, 27, 2, 140, 28, 
	2, 140, 29, 2, 140, 31, 2, 140, 
	39, 2, 140, 40, 2, 140, 41, 2, 
	140, 49, 2, 140, 50, 2, 140, 51, 
	2, 140, 56, 2, 140, 57, 2, 140, 
	58, 2, 140, 59, 2, 140, 61, 2, 
	140, 62, 2, 140, 63, 2, 140, 69, 
	2, 140, 70, 2, 140, 71, 2, 140, 
	73, 2, 140, 77, 2, 140, 78, 2, 
	140, 85, 2, 140, 86, 2, 140, 89, 
	2, 140, 109, 2, 140, 118, 2, 140, 
	124, 2, 140, 126, 2, 140, 138, 2, 
	140, 139, 2, 141, 0, 2, 141, 140, 
	3, 30, 37, 140, 3, 53, 127, 140, 
	3, 101, 140, 1, 3, 101, 140, 3, 
	3, 101, 140, 4, 3, 101, 140, 5, 
	3, 101, 140, 6, 3, 101, 140, 8, 
	3, 101, 140, 14, 3, 101, 140, 26, 
	3, 101, 140, 27, 3, 101, 140, 28, 
	3, 101, 140, 29, 3, 101, 140, 31, 
	3, 101, 140, 39, 3, 101, 140, 40, 
	3, 101, 140, 41, 3, 101, 140, 49, 
	3, 101, 140, 50, 3, 101, 140, 51, 
	3, 101, 140, 56, 3, 101, 140, 57, 
	3, 101, 140, 58, 3, 101, 140, 59, 
	3, 101, 140, 61, 3, 101, 140, 62, 
	3, 101, 140, 63, 3, 101, 140, 69, 
	3, 101, 140, 70, 3, 101, 140, 71, 
	3, 101, 140, 73, 3, 101, 140, 77, 
	3, 101, 140, 78, 3, 101, 140, 85, 
	3, 101, 140, 86, 3, 101, 140, 89, 
	3, 101, 140, 109, 3, 101, 140, 118, 
	3, 101, 140, 124, 3, 101, 140, 126, 
	3, 101, 140, 138, 3, 101, 140, 139, 
	3, 107, 131, 140, 3, 108, 101, 140, 
	3, 110, 132, 140, 3, 141, 0, 140
	
};

static const short _ECMA48_eof_actions[] = {
	0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 
	191, 191, 191, 191, 191, 191, 191, 357, 
	0, 191, 191, 191, 191, 191, 191, 191, 
	357
};

static const int ECMA48_start = 37;
static const int ECMA48_first_final = 37;
static const int ECMA48_error = 0;

static const int ECMA48_en_main = 37;


#line 1723 "ecma48.rl"

static char *p;
static char *pe;
static char *eof;
// static int cs;

int ecma48_init(void)
{
    // Writing moves right
    simd = 0;
    // overwrite, not insert
    irm = IRM_REPLACE;
    // inserting moves write
    hem = 0;
    // inserting lines moves down
    vem = 0;
    // graphics rendition combining
    grcm = GRCM_COMBINING;
    // left edge of text is 1st column
    line_home = 1;
    // right endge of text is last column
    line_limit = CONSOLE_COLS;
    // top of page is 1st line
    page_home = 1;
    // bottom of page is last line
    page_limit = CONSOLE_ROWS;
    // parameter storage is cleared
    P_size = 0;
    
    
#line 608 "ecma48.cpp"
	{
	cs = ECMA48_start;
	}

#line 1753 "ecma48.rl"
    return 1;
}

int ecma48_execute(const char *data, int len)
{
    p = (char *) data;
    pe = (char *)data + len;
    eof = pe;

    
#line 624 "ecma48.cpp"
	{
	const unsigned char *_acts;
	unsigned int _nacts;

	if ( p == pe )
		goto _test_eof;
	if ( cs == 0 )
		goto _out;
_resume:
	switch ( cs ) {
case 37:
	switch( (*p) ) {
		case 0u: goto tr105;
		case 1u: goto tr6;
		case 2u: goto tr2;
		case 6u: goto tr106;
		case 7u: goto tr107;
		case 8u: goto tr108;
		case 9u: goto tr109;
		case 10u: goto tr110;
		case 11u: goto tr111;
		case 12u: goto tr112;
		case 13u: goto tr113;
		case 14u: goto tr114;
		case 15u: goto tr115;
		case 16u: goto tr116;
		case 17u: goto tr117;
		case 18u: goto tr118;
		case 19u: goto tr119;
		case 20u: goto tr120;
		case 24u: goto tr121;
		case 25u: goto tr122;
		case 26u: goto tr123;
		case 27u: goto tr124;
		case 28u: goto tr125;
		case 29u: goto tr126;
		case 30u: goto tr127;
		case 31u: goto tr128;
		case 130u: goto tr7;
		case 131u: goto tr8;
		case 133u: goto tr9;
		case 134u: goto tr10;
		case 135u: goto tr5;
		case 136u: goto tr11;
		case 137u: goto tr12;
		case 138u: goto tr13;
		case 139u: goto tr14;
		case 140u: goto tr15;
		case 141u: goto tr16;
		case 142u: goto tr17;
		case 143u: goto tr18;
		case 144u: goto tr19;
		case 145u: goto tr20;
		case 146u: goto tr21;
		case 147u: goto tr22;
		case 148u: goto tr23;
		case 149u: goto tr24;
		case 150u: goto tr25;
		case 151u: goto tr26;
		case 152u: goto tr27;
		case 154u: goto tr28;
		case 155u: goto tr29;
		case 156u: goto tr5;
		case 157u: goto tr30;
		case 158u: goto tr31;
		case 159u: goto tr32;
	}
	if ( 32u <= (*p) && (*p) <= 126u )
		goto tr129;
	goto tr1;
case 1:
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr0;
	} else if ( (*p) >= 9u )
		goto tr0;
	goto tr1;
case 0:
	goto _out;
case 2:
	switch( (*p) ) {
		case 2u: goto tr2;
		case 23u: goto tr3;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr0;
	} else if ( (*p) >= 9u )
		goto tr0;
	goto tr1;
case 3:
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr4;
	} else if ( (*p) >= 9u )
		goto tr4;
	goto tr1;
case 4:
	switch( (*p) ) {
		case 3u: goto tr5;
		case 23u: goto tr2;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr4;
	} else if ( (*p) >= 9u )
		goto tr4;
	goto tr1;
case 5:
	switch( (*p) ) {
		case 1u: goto tr6;
		case 2u: goto tr2;
	}
	goto tr1;
case 6:
	switch( (*p) ) {
		case 66u: goto tr7;
		case 67u: goto tr8;
		case 69u: goto tr9;
		case 70u: goto tr10;
		case 71u: goto tr5;
		case 72u: goto tr11;
		case 73u: goto tr12;
		case 74u: goto tr13;
		case 75u: goto tr14;
		case 76u: goto tr15;
		case 77u: goto tr16;
		case 78u: goto tr17;
		case 79u: goto tr18;
		case 80u: goto tr19;
		case 81u: goto tr20;
		case 82u: goto tr21;
		case 83u: goto tr22;
		case 84u: goto tr23;
		case 85u: goto tr24;
		case 86u: goto tr25;
		case 87u: goto tr26;
		case 88u: goto tr27;
		case 90u: goto tr28;
		case 91u: goto tr29;
		case 92u: goto tr5;
		case 93u: goto tr30;
		case 94u: goto tr31;
		case 95u: goto tr32;
		case 97u: goto tr33;
		case 99u: goto tr34;
		case 110u: goto tr35;
		case 111u: goto tr36;
		case 124u: goto tr37;
		case 126u: goto tr38;
	}
	goto tr1;
case 7:
	if ( 33u <= (*p) && (*p) <= 126u )
		goto tr39;
	goto tr1;
case 8:
	if ( 33u <= (*p) && (*p) <= 126u )
		goto tr40;
	goto tr1;
case 9:
	switch( (*p) ) {
		case 27u: goto tr41;
		case 156u: goto tr42;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr19;
	} else if ( (*p) >= 8u )
		goto tr19;
	goto tr1;
case 10:
	if ( (*p) == 92u )
		goto tr42;
	goto tr1;
case 11:
	switch( (*p) ) {
		case 27u: goto tr43;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	goto tr27;
case 12:
	switch( (*p) ) {
		case 27u: goto tr43;
		case 92u: goto tr45;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	goto tr27;
case 38:
	switch( (*p) ) {
		case 0u: goto tr130;
		case 1u: goto tr51;
		case 2u: goto tr47;
		case 6u: goto tr131;
		case 7u: goto tr132;
		case 8u: goto tr133;
		case 9u: goto tr134;
		case 10u: goto tr135;
		case 11u: goto tr136;
		case 12u: goto tr137;
		case 13u: goto tr138;
		case 14u: goto tr139;
		case 15u: goto tr140;
		case 16u: goto tr141;
		case 17u: goto tr142;
		case 18u: goto tr143;
		case 19u: goto tr144;
		case 20u: goto tr145;
		case 24u: goto tr146;
		case 25u: goto tr147;
		case 26u: goto tr148;
		case 27u: goto tr149;
		case 28u: goto tr150;
		case 29u: goto tr151;
		case 30u: goto tr152;
		case 31u: goto tr153;
		case 130u: goto tr52;
		case 131u: goto tr53;
		case 133u: goto tr54;
		case 134u: goto tr55;
		case 135u: goto tr50;
		case 136u: goto tr56;
		case 137u: goto tr57;
		case 138u: goto tr58;
		case 139u: goto tr59;
		case 140u: goto tr60;
		case 141u: goto tr61;
		case 142u: goto tr62;
		case 143u: goto tr63;
		case 144u: goto tr64;
		case 145u: goto tr65;
		case 146u: goto tr66;
		case 147u: goto tr67;
		case 148u: goto tr68;
		case 149u: goto tr69;
		case 150u: goto tr70;
		case 151u: goto tr71;
		case 154u: goto tr72;
		case 155u: goto tr73;
		case 156u: goto tr44;
		case 157u: goto tr74;
		case 158u: goto tr75;
		case 159u: goto tr76;
	}
	if ( 32u <= (*p) && (*p) <= 126u )
		goto tr154;
	goto tr27;
case 13:
	switch( (*p) ) {
		case 27u: goto tr43;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr46;
	} else if ( (*p) >= 9u )
		goto tr46;
	goto tr27;
case 14:
	switch( (*p) ) {
		case 2u: goto tr47;
		case 23u: goto tr48;
		case 27u: goto tr43;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr46;
	} else if ( (*p) >= 9u )
		goto tr46;
	goto tr27;
case 15:
	switch( (*p) ) {
		case 27u: goto tr43;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr49;
	} else if ( (*p) >= 9u )
		goto tr49;
	goto tr27;
case 16:
	switch( (*p) ) {
		case 3u: goto tr50;
		case 23u: goto tr47;
		case 27u: goto tr43;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr49;
	} else if ( (*p) >= 9u )
		goto tr49;
	goto tr27;
case 17:
	switch( (*p) ) {
		case 1u: goto tr51;
		case 2u: goto tr47;
		case 27u: goto tr43;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	goto tr27;
case 18:
	switch( (*p) ) {
		case 27u: goto tr43;
		case 66u: goto tr52;
		case 67u: goto tr53;
		case 69u: goto tr54;
		case 70u: goto tr55;
		case 71u: goto tr50;
		case 72u: goto tr56;
		case 73u: goto tr57;
		case 74u: goto tr58;
		case 75u: goto tr59;
		case 76u: goto tr60;
		case 77u: goto tr61;
		case 78u: goto tr62;
		case 79u: goto tr63;
		case 80u: goto tr64;
		case 81u: goto tr65;
		case 82u: goto tr66;
		case 83u: goto tr67;
		case 84u: goto tr68;
		case 85u: goto tr69;
		case 86u: goto tr70;
		case 87u: goto tr71;
		case 90u: goto tr72;
		case 91u: goto tr73;
		case 92u: goto tr45;
		case 93u: goto tr74;
		case 94u: goto tr75;
		case 95u: goto tr76;
		case 97u: goto tr77;
		case 99u: goto tr78;
		case 110u: goto tr79;
		case 111u: goto tr80;
		case 124u: goto tr81;
		case 126u: goto tr82;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	goto tr27;
case 19:
	switch( (*p) ) {
		case 27u: goto tr43;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	if ( 33u <= (*p) && (*p) <= 126u )
		goto tr83;
	goto tr27;
case 20:
	switch( (*p) ) {
		case 27u: goto tr43;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	if ( 33u <= (*p) && (*p) <= 126u )
		goto tr84;
	goto tr27;
case 21:
	switch( (*p) ) {
		case 27u: goto tr85;
		case 152u: goto tr1;
		case 156u: goto tr86;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr64;
	} else if ( (*p) >= 8u )
		goto tr64;
	goto tr27;
case 22:
	switch( (*p) ) {
		case 27u: goto tr43;
		case 92u: goto tr87;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	goto tr27;
case 23:
	switch( (*p) ) {
		case 27u: goto tr43;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr88;
	} else if ( (*p) >= 8u )
		goto tr88;
	goto tr27;
case 39:
	switch( (*p) ) {
		case 0u: goto tr130;
		case 1u: goto tr51;
		case 2u: goto tr47;
		case 6u: goto tr131;
		case 7u: goto tr132;
		case 8u: goto tr133;
		case 9u: goto tr134;
		case 10u: goto tr135;
		case 11u: goto tr136;
		case 12u: goto tr137;
		case 13u: goto tr138;
		case 14u: goto tr139;
		case 15u: goto tr140;
		case 16u: goto tr141;
		case 17u: goto tr142;
		case 18u: goto tr143;
		case 19u: goto tr144;
		case 20u: goto tr145;
		case 24u: goto tr146;
		case 25u: goto tr147;
		case 26u: goto tr148;
		case 27u: goto tr149;
		case 28u: goto tr150;
		case 29u: goto tr151;
		case 30u: goto tr152;
		case 31u: goto tr153;
		case 32u: goto tr155;
		case 59u: goto tr157;
		case 64u: goto tr158;
		case 65u: goto tr159;
		case 66u: goto tr160;
		case 67u: goto tr161;
		case 68u: goto tr162;
		case 69u: goto tr163;
		case 70u: goto tr164;
		case 71u: goto tr165;
		case 72u: goto tr166;
		case 73u: goto tr167;
		case 74u: goto tr168;
		case 75u: goto tr169;
		case 76u: goto tr170;
		case 77u: goto tr171;
		case 79u: goto tr172;
		case 80u: goto tr173;
		case 81u: goto tr174;
		case 82u: goto tr175;
		case 83u: goto tr176;
		case 84u: goto tr177;
		case 85u: goto tr178;
		case 86u: goto tr179;
		case 87u: goto tr180;
		case 88u: goto tr181;
		case 89u: goto tr182;
		case 90u: goto tr183;
		case 91u: goto tr184;
		case 92u: goto tr185;
		case 93u: goto tr186;
		case 94u: goto tr187;
		case 96u: goto tr188;
		case 97u: goto tr189;
		case 98u: goto tr190;
		case 99u: goto tr191;
		case 100u: goto tr192;
		case 101u: goto tr193;
		case 103u: goto tr194;
		case 105u: goto tr195;
		case 106u: goto tr196;
		case 107u: goto tr197;
		case 108u: goto tr198;
		case 109u: goto tr199;
		case 110u: goto tr200;
		case 111u: goto tr201;
		case 130u: goto tr52;
		case 131u: goto tr53;
		case 133u: goto tr54;
		case 134u: goto tr55;
		case 135u: goto tr50;
		case 136u: goto tr56;
		case 137u: goto tr57;
		case 138u: goto tr58;
		case 139u: goto tr59;
		case 140u: goto tr60;
		case 141u: goto tr61;
		case 142u: goto tr62;
		case 143u: goto tr63;
		case 144u: goto tr64;
		case 145u: goto tr65;
		case 146u: goto tr66;
		case 147u: goto tr67;
		case 148u: goto tr68;
		case 149u: goto tr69;
		case 150u: goto tr70;
		case 151u: goto tr71;
		case 154u: goto tr72;
		case 155u: goto tr73;
		case 156u: goto tr44;
		case 157u: goto tr74;
		case 158u: goto tr75;
		case 159u: goto tr76;
	}
	if ( (*p) < 48u ) {
		if ( 33u <= (*p) && (*p) <= 47u )
			goto tr154;
	} else if ( (*p) > 57u ) {
		if ( 58u <= (*p) && (*p) <= 126u )
			goto tr154;
	} else
		goto tr156;
	goto tr27;
case 40:
	switch( (*p) ) {
		case 0u: goto tr202;
		case 1u: goto tr203;
		case 2u: goto tr204;
		case 6u: goto tr205;
		case 7u: goto tr206;
		case 8u: goto tr207;
		case 9u: goto tr208;
		case 10u: goto tr209;
		case 11u: goto tr210;
		case 12u: goto tr211;
		case 13u: goto tr212;
		case 14u: goto tr213;
		case 15u: goto tr214;
		case 16u: goto tr215;
		case 17u: goto tr216;
		case 18u: goto tr217;
		case 19u: goto tr218;
		case 20u: goto tr219;
		case 24u: goto tr220;
		case 25u: goto tr221;
		case 26u: goto tr222;
		case 27u: goto tr223;
		case 28u: goto tr224;
		case 29u: goto tr225;
		case 30u: goto tr226;
		case 31u: goto tr227;
		case 64u: goto tr229;
		case 65u: goto tr230;
		case 66u: goto tr231;
		case 68u: goto tr232;
		case 69u: goto tr233;
		case 70u: goto tr234;
		case 71u: goto tr235;
		case 72u: goto tr236;
		case 73u: goto tr237;
		case 74u: goto tr238;
		case 76u: goto tr239;
		case 80u: goto tr240;
		case 81u: goto tr241;
		case 82u: goto tr242;
		case 84u: goto tr243;
		case 85u: goto tr244;
		case 86u: goto tr245;
		case 87u: goto tr246;
		case 88u: goto tr247;
		case 89u: goto tr248;
		case 90u: goto tr249;
		case 91u: goto tr250;
		case 92u: goto tr251;
		case 94u: goto tr252;
		case 95u: goto tr253;
		case 96u: goto tr254;
		case 97u: goto tr255;
		case 98u: goto tr256;
		case 99u: goto tr257;
		case 100u: goto tr258;
		case 101u: goto tr259;
		case 102u: goto tr260;
		case 103u: goto tr261;
		case 104u: goto tr262;
		case 105u: goto tr263;
		case 106u: goto tr264;
		case 107u: goto tr265;
		case 130u: goto tr266;
		case 131u: goto tr267;
		case 133u: goto tr268;
		case 134u: goto tr269;
		case 135u: goto tr270;
		case 136u: goto tr271;
		case 137u: goto tr272;
		case 138u: goto tr273;
		case 139u: goto tr274;
		case 140u: goto tr275;
		case 141u: goto tr276;
		case 142u: goto tr277;
		case 143u: goto tr278;
		case 144u: goto tr279;
		case 145u: goto tr280;
		case 146u: goto tr281;
		case 147u: goto tr282;
		case 148u: goto tr283;
		case 149u: goto tr284;
		case 150u: goto tr285;
		case 151u: goto tr286;
		case 152u: goto tr287;
		case 154u: goto tr288;
		case 155u: goto tr289;
		case 156u: goto tr290;
		case 157u: goto tr291;
		case 158u: goto tr292;
		case 159u: goto tr293;
	}
	if ( 32u <= (*p) && (*p) <= 126u )
		goto tr228;
	goto tr27;
case 41:
	switch( (*p) ) {
		case 0u: goto tr202;
		case 1u: goto tr203;
		case 2u: goto tr204;
		case 6u: goto tr205;
		case 7u: goto tr206;
		case 8u: goto tr207;
		case 9u: goto tr208;
		case 10u: goto tr209;
		case 11u: goto tr210;
		case 12u: goto tr211;
		case 13u: goto tr212;
		case 14u: goto tr213;
		case 15u: goto tr214;
		case 16u: goto tr215;
		case 17u: goto tr216;
		case 18u: goto tr217;
		case 19u: goto tr218;
		case 20u: goto tr219;
		case 24u: goto tr220;
		case 25u: goto tr221;
		case 26u: goto tr222;
		case 27u: goto tr223;
		case 28u: goto tr224;
		case 29u: goto tr225;
		case 30u: goto tr226;
		case 31u: goto tr227;
		case 130u: goto tr266;
		case 131u: goto tr267;
		case 133u: goto tr268;
		case 134u: goto tr269;
		case 135u: goto tr270;
		case 136u: goto tr271;
		case 137u: goto tr272;
		case 138u: goto tr273;
		case 139u: goto tr274;
		case 140u: goto tr275;
		case 141u: goto tr276;
		case 142u: goto tr277;
		case 143u: goto tr278;
		case 144u: goto tr279;
		case 145u: goto tr280;
		case 146u: goto tr281;
		case 147u: goto tr282;
		case 148u: goto tr283;
		case 149u: goto tr284;
		case 150u: goto tr285;
		case 151u: goto tr286;
		case 152u: goto tr287;
		case 154u: goto tr288;
		case 155u: goto tr289;
		case 156u: goto tr290;
		case 157u: goto tr291;
		case 158u: goto tr292;
		case 159u: goto tr293;
	}
	if ( 32u <= (*p) && (*p) <= 126u )
		goto tr228;
	goto tr27;
case 24:
	switch( (*p) ) {
		case 27u: goto tr89;
		case 152u: goto tr1;
		case 156u: goto tr90;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr74;
	} else if ( (*p) >= 8u )
		goto tr74;
	goto tr27;
case 25:
	switch( (*p) ) {
		case 27u: goto tr43;
		case 92u: goto tr91;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	goto tr27;
case 26:
	switch( (*p) ) {
		case 27u: goto tr92;
		case 152u: goto tr1;
		case 156u: goto tr93;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr75;
	} else if ( (*p) >= 8u )
		goto tr75;
	goto tr27;
case 27:
	switch( (*p) ) {
		case 27u: goto tr43;
		case 92u: goto tr94;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	goto tr27;
case 28:
	switch( (*p) ) {
		case 27u: goto tr95;
		case 152u: goto tr1;
		case 156u: goto tr96;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr76;
	} else if ( (*p) >= 8u )
		goto tr76;
	goto tr27;
case 29:
	switch( (*p) ) {
		case 27u: goto tr43;
		case 92u: goto tr97;
		case 152u: goto tr1;
		case 156u: goto tr44;
	}
	goto tr27;
case 42:
	switch( (*p) ) {
		case 0u: goto tr202;
		case 1u: goto tr203;
		case 2u: goto tr204;
		case 6u: goto tr205;
		case 7u: goto tr206;
		case 8u: goto tr207;
		case 9u: goto tr208;
		case 10u: goto tr209;
		case 11u: goto tr210;
		case 12u: goto tr211;
		case 13u: goto tr212;
		case 14u: goto tr213;
		case 15u: goto tr214;
		case 16u: goto tr215;
		case 17u: goto tr216;
		case 18u: goto tr217;
		case 19u: goto tr218;
		case 20u: goto tr219;
		case 24u: goto tr220;
		case 25u: goto tr221;
		case 26u: goto tr222;
		case 27u: goto tr223;
		case 28u: goto tr224;
		case 29u: goto tr225;
		case 30u: goto tr226;
		case 31u: goto tr227;
		case 32u: goto tr294;
		case 59u: goto tr296;
		case 64u: goto tr297;
		case 65u: goto tr298;
		case 66u: goto tr299;
		case 67u: goto tr300;
		case 68u: goto tr301;
		case 69u: goto tr302;
		case 70u: goto tr303;
		case 71u: goto tr304;
		case 72u: goto tr305;
		case 73u: goto tr306;
		case 74u: goto tr307;
		case 75u: goto tr308;
		case 76u: goto tr309;
		case 77u: goto tr310;
		case 79u: goto tr311;
		case 80u: goto tr312;
		case 81u: goto tr313;
		case 82u: goto tr314;
		case 83u: goto tr315;
		case 84u: goto tr316;
		case 85u: goto tr317;
		case 86u: goto tr318;
		case 87u: goto tr319;
		case 88u: goto tr320;
		case 89u: goto tr321;
		case 90u: goto tr322;
		case 91u: goto tr323;
		case 92u: goto tr324;
		case 93u: goto tr325;
		case 94u: goto tr326;
		case 96u: goto tr327;
		case 97u: goto tr328;
		case 98u: goto tr329;
		case 99u: goto tr330;
		case 100u: goto tr331;
		case 101u: goto tr332;
		case 103u: goto tr333;
		case 105u: goto tr334;
		case 106u: goto tr335;
		case 107u: goto tr336;
		case 108u: goto tr337;
		case 109u: goto tr338;
		case 110u: goto tr339;
		case 111u: goto tr340;
		case 130u: goto tr266;
		case 131u: goto tr267;
		case 133u: goto tr268;
		case 134u: goto tr269;
		case 135u: goto tr270;
		case 136u: goto tr271;
		case 137u: goto tr272;
		case 138u: goto tr273;
		case 139u: goto tr274;
		case 140u: goto tr275;
		case 141u: goto tr276;
		case 142u: goto tr277;
		case 143u: goto tr278;
		case 144u: goto tr279;
		case 145u: goto tr280;
		case 146u: goto tr281;
		case 147u: goto tr282;
		case 148u: goto tr283;
		case 149u: goto tr284;
		case 150u: goto tr285;
		case 151u: goto tr286;
		case 152u: goto tr287;
		case 154u: goto tr288;
		case 155u: goto tr289;
		case 156u: goto tr290;
		case 157u: goto tr291;
		case 158u: goto tr292;
		case 159u: goto tr293;
	}
	if ( (*p) < 48u ) {
		if ( 33u <= (*p) && (*p) <= 47u )
			goto tr228;
	} else if ( (*p) > 57u ) {
		if ( 58u <= (*p) && (*p) <= 126u )
			goto tr228;
	} else
		goto tr295;
	goto tr27;
case 43:
	switch( (*p) ) {
		case 0u: goto tr202;
		case 1u: goto tr203;
		case 2u: goto tr204;
		case 6u: goto tr205;
		case 7u: goto tr206;
		case 8u: goto tr207;
		case 9u: goto tr208;
		case 10u: goto tr209;
		case 11u: goto tr210;
		case 12u: goto tr211;
		case 13u: goto tr212;
		case 14u: goto tr213;
		case 15u: goto tr214;
		case 16u: goto tr215;
		case 17u: goto tr216;
		case 18u: goto tr217;
		case 19u: goto tr218;
		case 20u: goto tr219;
		case 24u: goto tr220;
		case 25u: goto tr221;
		case 26u: goto tr222;
		case 27u: goto tr223;
		case 28u: goto tr224;
		case 29u: goto tr225;
		case 30u: goto tr226;
		case 31u: goto tr227;
		case 32u: goto tr341;
		case 59u: goto tr342;
		case 72u: goto tr305;
		case 82u: goto tr314;
		case 87u: goto tr319;
		case 103u: goto tr343;
		case 108u: goto tr337;
		case 109u: goto tr338;
		case 111u: goto tr340;
		case 130u: goto tr266;
		case 131u: goto tr267;
		case 133u: goto tr268;
		case 134u: goto tr269;
		case 135u: goto tr270;
		case 136u: goto tr271;
		case 137u: goto tr272;
		case 138u: goto tr273;
		case 139u: goto tr274;
		case 140u: goto tr275;
		case 141u: goto tr276;
		case 142u: goto tr277;
		case 143u: goto tr278;
		case 144u: goto tr279;
		case 145u: goto tr280;
		case 146u: goto tr281;
		case 147u: goto tr282;
		case 148u: goto tr283;
		case 149u: goto tr284;
		case 150u: goto tr285;
		case 151u: goto tr286;
		case 152u: goto tr287;
		case 154u: goto tr288;
		case 155u: goto tr289;
		case 156u: goto tr290;
		case 157u: goto tr291;
		case 158u: goto tr292;
		case 159u: goto tr293;
	}
	if ( (*p) < 48u ) {
		if ( 33u <= (*p) && (*p) <= 47u )
			goto tr228;
	} else if ( (*p) > 57u ) {
		if ( 58u <= (*p) && (*p) <= 126u )
			goto tr228;
	} else
		goto tr296;
	goto tr27;
case 44:
	switch( (*p) ) {
		case 0u: goto tr202;
		case 1u: goto tr203;
		case 2u: goto tr204;
		case 6u: goto tr205;
		case 7u: goto tr206;
		case 8u: goto tr207;
		case 9u: goto tr208;
		case 10u: goto tr209;
		case 11u: goto tr210;
		case 12u: goto tr211;
		case 13u: goto tr212;
		case 14u: goto tr213;
		case 15u: goto tr214;
		case 16u: goto tr215;
		case 17u: goto tr216;
		case 18u: goto tr217;
		case 19u: goto tr218;
		case 20u: goto tr219;
		case 24u: goto tr220;
		case 25u: goto tr221;
		case 26u: goto tr222;
		case 27u: goto tr223;
		case 28u: goto tr224;
		case 29u: goto tr225;
		case 30u: goto tr226;
		case 31u: goto tr227;
		case 66u: goto tr231;
		case 68u: goto tr232;
		case 70u: goto tr234;
		case 71u: goto tr235;
		case 72u: goto tr236;
		case 84u: goto tr243;
		case 89u: goto tr248;
		case 99u: goto tr257;
		case 107u: goto tr265;
		case 130u: goto tr266;
		case 131u: goto tr267;
		case 133u: goto tr268;
		case 134u: goto tr269;
		case 135u: goto tr270;
		case 136u: goto tr271;
		case 137u: goto tr272;
		case 138u: goto tr273;
		case 139u: goto tr274;
		case 140u: goto tr275;
		case 141u: goto tr276;
		case 142u: goto tr277;
		case 143u: goto tr278;
		case 144u: goto tr279;
		case 145u: goto tr280;
		case 146u: goto tr281;
		case 147u: goto tr282;
		case 148u: goto tr283;
		case 149u: goto tr284;
		case 150u: goto tr285;
		case 151u: goto tr286;
		case 152u: goto tr287;
		case 154u: goto tr288;
		case 155u: goto tr289;
		case 156u: goto tr290;
		case 157u: goto tr291;
		case 158u: goto tr292;
		case 159u: goto tr293;
	}
	if ( 32u <= (*p) && (*p) <= 126u )
		goto tr228;
	goto tr27;
case 45:
	switch( (*p) ) {
		case 0u: goto tr202;
		case 1u: goto tr203;
		case 2u: goto tr204;
		case 6u: goto tr205;
		case 7u: goto tr206;
		case 8u: goto tr207;
		case 9u: goto tr208;
		case 10u: goto tr209;
		case 11u: goto tr210;
		case 12u: goto tr211;
		case 13u: goto tr212;
		case 14u: goto tr213;
		case 15u: goto tr214;
		case 16u: goto tr215;
		case 17u: goto tr216;
		case 18u: goto tr217;
		case 19u: goto tr218;
		case 20u: goto tr219;
		case 24u: goto tr220;
		case 25u: goto tr221;
		case 26u: goto tr222;
		case 27u: goto tr223;
		case 28u: goto tr224;
		case 29u: goto tr225;
		case 30u: goto tr226;
		case 31u: goto tr227;
		case 32u: goto tr344;
		case 58u: goto tr228;
		case 87u: goto tr319;
		case 103u: goto tr343;
		case 108u: goto tr337;
		case 109u: goto tr338;
		case 111u: goto tr340;
		case 130u: goto tr266;
		case 131u: goto tr267;
		case 133u: goto tr268;
		case 134u: goto tr269;
		case 135u: goto tr270;
		case 136u: goto tr271;
		case 137u: goto tr272;
		case 138u: goto tr273;
		case 139u: goto tr274;
		case 140u: goto tr275;
		case 141u: goto tr276;
		case 142u: goto tr277;
		case 143u: goto tr278;
		case 144u: goto tr279;
		case 145u: goto tr280;
		case 146u: goto tr281;
		case 147u: goto tr282;
		case 148u: goto tr283;
		case 149u: goto tr284;
		case 150u: goto tr285;
		case 151u: goto tr286;
		case 152u: goto tr287;
		case 154u: goto tr288;
		case 155u: goto tr289;
		case 156u: goto tr290;
		case 157u: goto tr291;
		case 158u: goto tr292;
		case 159u: goto tr293;
	}
	if ( (*p) < 48u ) {
		if ( 33u <= (*p) && (*p) <= 47u )
			goto tr228;
	} else if ( (*p) > 59u ) {
		if ( 60u <= (*p) && (*p) <= 126u )
			goto tr228;
	} else
		goto tr342;
	goto tr27;
case 46:
	switch( (*p) ) {
		case 0u: goto tr202;
		case 1u: goto tr203;
		case 2u: goto tr204;
		case 6u: goto tr205;
		case 7u: goto tr206;
		case 8u: goto tr207;
		case 9u: goto tr208;
		case 10u: goto tr209;
		case 11u: goto tr210;
		case 12u: goto tr211;
		case 13u: goto tr212;
		case 14u: goto tr213;
		case 15u: goto tr214;
		case 16u: goto tr215;
		case 17u: goto tr216;
		case 18u: goto tr217;
		case 19u: goto tr218;
		case 20u: goto tr219;
		case 24u: goto tr220;
		case 25u: goto tr221;
		case 26u: goto tr222;
		case 27u: goto tr223;
		case 28u: goto tr224;
		case 29u: goto tr225;
		case 30u: goto tr226;
		case 31u: goto tr227;
		case 70u: goto tr234;
		case 72u: goto tr236;
		case 130u: goto tr266;
		case 131u: goto tr267;
		case 133u: goto tr268;
		case 134u: goto tr269;
		case 135u: goto tr270;
		case 136u: goto tr271;
		case 137u: goto tr272;
		case 138u: goto tr273;
		case 139u: goto tr274;
		case 140u: goto tr275;
		case 141u: goto tr276;
		case 142u: goto tr277;
		case 143u: goto tr278;
		case 144u: goto tr279;
		case 145u: goto tr280;
		case 146u: goto tr281;
		case 147u: goto tr282;
		case 148u: goto tr283;
		case 149u: goto tr284;
		case 150u: goto tr285;
		case 151u: goto tr286;
		case 152u: goto tr287;
		case 154u: goto tr288;
		case 155u: goto tr289;
		case 156u: goto tr290;
		case 157u: goto tr291;
		case 158u: goto tr292;
		case 159u: goto tr293;
	}
	if ( 32u <= (*p) && (*p) <= 126u )
		goto tr228;
	goto tr27;
case 47:
	switch( (*p) ) {
		case 0u: goto tr345;
		case 1u: goto tr346;
		case 2u: goto tr347;
		case 6u: goto tr348;
		case 7u: goto tr349;
		case 8u: goto tr350;
		case 9u: goto tr351;
		case 10u: goto tr352;
		case 11u: goto tr353;
		case 12u: goto tr354;
		case 13u: goto tr355;
		case 14u: goto tr356;
		case 15u: goto tr357;
		case 16u: goto tr358;
		case 17u: goto tr359;
		case 18u: goto tr360;
		case 19u: goto tr361;
		case 20u: goto tr362;
		case 24u: goto tr363;
		case 25u: goto tr364;
		case 26u: goto tr365;
		case 27u: goto tr366;
		case 28u: goto tr367;
		case 29u: goto tr368;
		case 30u: goto tr369;
		case 31u: goto tr370;
		case 130u: goto tr372;
		case 131u: goto tr373;
		case 133u: goto tr374;
		case 134u: goto tr375;
		case 135u: goto tr376;
		case 136u: goto tr377;
		case 137u: goto tr378;
		case 138u: goto tr379;
		case 139u: goto tr380;
		case 140u: goto tr381;
		case 141u: goto tr382;
		case 142u: goto tr383;
		case 143u: goto tr384;
		case 144u: goto tr385;
		case 145u: goto tr386;
		case 146u: goto tr387;
		case 147u: goto tr388;
		case 148u: goto tr389;
		case 149u: goto tr390;
		case 150u: goto tr391;
		case 151u: goto tr392;
		case 152u: goto tr393;
		case 154u: goto tr394;
		case 155u: goto tr395;
		case 156u: goto tr396;
		case 157u: goto tr397;
		case 158u: goto tr398;
		case 159u: goto tr399;
	}
	if ( 32u <= (*p) && (*p) <= 126u )
		goto tr371;
	goto tr27;
case 30:
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr98;
	} else if ( (*p) >= 8u )
		goto tr98;
	goto tr1;
case 48:
	switch( (*p) ) {
		case 0u: goto tr105;
		case 1u: goto tr6;
		case 2u: goto tr2;
		case 6u: goto tr106;
		case 7u: goto tr107;
		case 8u: goto tr108;
		case 9u: goto tr109;
		case 10u: goto tr110;
		case 11u: goto tr111;
		case 12u: goto tr112;
		case 13u: goto tr113;
		case 14u: goto tr114;
		case 15u: goto tr115;
		case 16u: goto tr116;
		case 17u: goto tr117;
		case 18u: goto tr118;
		case 19u: goto tr119;
		case 20u: goto tr120;
		case 24u: goto tr121;
		case 25u: goto tr122;
		case 26u: goto tr123;
		case 27u: goto tr124;
		case 28u: goto tr125;
		case 29u: goto tr126;
		case 30u: goto tr127;
		case 31u: goto tr128;
		case 32u: goto tr400;
		case 59u: goto tr402;
		case 64u: goto tr403;
		case 65u: goto tr404;
		case 66u: goto tr405;
		case 67u: goto tr406;
		case 68u: goto tr407;
		case 69u: goto tr408;
		case 70u: goto tr409;
		case 71u: goto tr410;
		case 72u: goto tr411;
		case 73u: goto tr412;
		case 74u: goto tr413;
		case 75u: goto tr414;
		case 76u: goto tr415;
		case 77u: goto tr416;
		case 79u: goto tr417;
		case 80u: goto tr418;
		case 81u: goto tr419;
		case 82u: goto tr420;
		case 83u: goto tr421;
		case 84u: goto tr422;
		case 85u: goto tr423;
		case 86u: goto tr424;
		case 87u: goto tr425;
		case 88u: goto tr426;
		case 89u: goto tr427;
		case 90u: goto tr428;
		case 91u: goto tr429;
		case 92u: goto tr430;
		case 93u: goto tr431;
		case 94u: goto tr432;
		case 96u: goto tr433;
		case 97u: goto tr434;
		case 98u: goto tr435;
		case 99u: goto tr436;
		case 100u: goto tr437;
		case 101u: goto tr438;
		case 103u: goto tr439;
		case 105u: goto tr440;
		case 106u: goto tr441;
		case 107u: goto tr442;
		case 108u: goto tr443;
		case 109u: goto tr444;
		case 110u: goto tr445;
		case 111u: goto tr446;
		case 130u: goto tr7;
		case 131u: goto tr8;
		case 133u: goto tr9;
		case 134u: goto tr10;
		case 135u: goto tr5;
		case 136u: goto tr11;
		case 137u: goto tr12;
		case 138u: goto tr13;
		case 139u: goto tr14;
		case 140u: goto tr15;
		case 141u: goto tr16;
		case 142u: goto tr17;
		case 143u: goto tr18;
		case 144u: goto tr19;
		case 145u: goto tr20;
		case 146u: goto tr21;
		case 147u: goto tr22;
		case 148u: goto tr23;
		case 149u: goto tr24;
		case 150u: goto tr25;
		case 151u: goto tr26;
		case 152u: goto tr27;
		case 154u: goto tr28;
		case 155u: goto tr29;
		case 156u: goto tr5;
		case 157u: goto tr30;
		case 158u: goto tr31;
		case 159u: goto tr32;
	}
	if ( (*p) < 48u ) {
		if ( 33u <= (*p) && (*p) <= 47u )
			goto tr129;
	} else if ( (*p) > 57u ) {
		if ( 58u <= (*p) && (*p) <= 126u )
			goto tr129;
	} else
		goto tr401;
	goto tr1;
case 49:
	switch( (*p) ) {
		case 0u: goto tr447;
		case 1u: goto tr448;
		case 2u: goto tr449;
		case 6u: goto tr450;
		case 7u: goto tr451;
		case 8u: goto tr452;
		case 9u: goto tr453;
		case 10u: goto tr454;
		case 11u: goto tr455;
		case 12u: goto tr456;
		case 13u: goto tr457;
		case 14u: goto tr458;
		case 15u: goto tr459;
		case 16u: goto tr460;
		case 17u: goto tr461;
		case 18u: goto tr462;
		case 19u: goto tr463;
		case 20u: goto tr464;
		case 24u: goto tr465;
		case 25u: goto tr466;
		case 26u: goto tr467;
		case 27u: goto tr468;
		case 28u: goto tr469;
		case 29u: goto tr470;
		case 30u: goto tr471;
		case 31u: goto tr472;
		case 64u: goto tr474;
		case 65u: goto tr475;
		case 66u: goto tr476;
		case 68u: goto tr477;
		case 69u: goto tr478;
		case 70u: goto tr479;
		case 71u: goto tr480;
		case 72u: goto tr481;
		case 73u: goto tr482;
		case 74u: goto tr483;
		case 76u: goto tr484;
		case 80u: goto tr485;
		case 81u: goto tr486;
		case 82u: goto tr487;
		case 84u: goto tr488;
		case 85u: goto tr489;
		case 86u: goto tr490;
		case 87u: goto tr491;
		case 88u: goto tr492;
		case 89u: goto tr493;
		case 90u: goto tr494;
		case 91u: goto tr495;
		case 92u: goto tr496;
		case 94u: goto tr497;
		case 95u: goto tr498;
		case 96u: goto tr499;
		case 97u: goto tr500;
		case 98u: goto tr501;
		case 99u: goto tr502;
		case 100u: goto tr503;
		case 101u: goto tr504;
		case 102u: goto tr505;
		case 103u: goto tr506;
		case 104u: goto tr507;
		case 105u: goto tr508;
		case 106u: goto tr509;
		case 107u: goto tr510;
		case 130u: goto tr511;
		case 131u: goto tr512;
		case 133u: goto tr513;
		case 134u: goto tr514;
		case 135u: goto tr515;
		case 136u: goto tr516;
		case 137u: goto tr517;
		case 138u: goto tr518;
		case 139u: goto tr519;
		case 140u: goto tr520;
		case 141u: goto tr521;
		case 142u: goto tr522;
		case 143u: goto tr523;
		case 144u: goto tr524;
		case 145u: goto tr525;
		case 146u: goto tr526;
		case 147u: goto tr527;
		case 148u: goto tr528;
		case 149u: goto tr529;
		case 150u: goto tr530;
		case 151u: goto tr531;
		case 152u: goto tr287;
		case 154u: goto tr532;
		case 155u: goto tr533;
		case 156u: goto tr515;
		case 157u: goto tr534;
		case 158u: goto tr535;
		case 159u: goto tr536;
	}
	if ( 32u <= (*p) && (*p) <= 126u )
		goto tr473;
	goto tr1;
case 50:
	switch( (*p) ) {
		case 0u: goto tr447;
		case 1u: goto tr448;
		case 2u: goto tr449;
		case 6u: goto tr450;
		case 7u: goto tr451;
		case 8u: goto tr452;
		case 9u: goto tr453;
		case 10u: goto tr454;
		case 11u: goto tr455;
		case 12u: goto tr456;
		case 13u: goto tr457;
		case 14u: goto tr458;
		case 15u: goto tr459;
		case 16u: goto tr460;
		case 17u: goto tr461;
		case 18u: goto tr462;
		case 19u: goto tr463;
		case 20u: goto tr464;
		case 24u: goto tr465;
		case 25u: goto tr466;
		case 26u: goto tr467;
		case 27u: goto tr468;
		case 28u: goto tr469;
		case 29u: goto tr470;
		case 30u: goto tr471;
		case 31u: goto tr472;
		case 130u: goto tr511;
		case 131u: goto tr512;
		case 133u: goto tr513;
		case 134u: goto tr514;
		case 135u: goto tr515;
		case 136u: goto tr516;
		case 137u: goto tr517;
		case 138u: goto tr518;
		case 139u: goto tr519;
		case 140u: goto tr520;
		case 141u: goto tr521;
		case 142u: goto tr522;
		case 143u: goto tr523;
		case 144u: goto tr524;
		case 145u: goto tr525;
		case 146u: goto tr526;
		case 147u: goto tr527;
		case 148u: goto tr528;
		case 149u: goto tr529;
		case 150u: goto tr530;
		case 151u: goto tr531;
		case 152u: goto tr287;
		case 154u: goto tr532;
		case 155u: goto tr533;
		case 156u: goto tr515;
		case 157u: goto tr534;
		case 158u: goto tr535;
		case 159u: goto tr536;
	}
	if ( 32u <= (*p) && (*p) <= 126u )
		goto tr473;
	goto tr1;
case 31:
	switch( (*p) ) {
		case 27u: goto tr99;
		case 156u: goto tr100;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr30;
	} else if ( (*p) >= 8u )
		goto tr30;
	goto tr1;
case 32:
	if ( (*p) == 92u )
		goto tr100;
	goto tr1;
case 33:
	switch( (*p) ) {
		case 27u: goto tr101;
		case 156u: goto tr102;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr31;
	} else if ( (*p) >= 8u )
		goto tr31;
	goto tr1;
case 34:
	if ( (*p) == 92u )
		goto tr102;
	goto tr1;
case 35:
	switch( (*p) ) {
		case 27u: goto tr103;
		case 156u: goto tr104;
	}
	if ( (*p) > 13u ) {
		if ( 32u <= (*p) && (*p) <= 126u )
			goto tr32;
	} else if ( (*p) >= 8u )
		goto tr32;
	goto tr1;
case 36:
	if ( (*p) == 92u )
		goto tr104;
	goto tr1;
case 51:
	switch( (*p) ) {
		case 0u: goto tr447;
		case 1u: goto tr448;
		case 2u: goto tr449;
		case 6u: goto tr450;
		case 7u: goto tr451;
		case 8u: goto tr452;
		case 9u: goto tr453;
		case 10u: goto tr454;
		case 11u: goto tr455;
		case 12u: goto tr456;
		case 13u: goto tr457;
		case 14u: goto tr458;
		case 15u: goto tr459;
		case 16u: goto tr460;
		case 17u: goto tr461;
		case 18u: goto tr462;
		case 19u: goto tr463;
		case 20u: goto tr464;
		case 24u: goto tr465;
		case 25u: goto tr466;
		case 26u: goto tr467;
		case 27u: goto tr468;
		case 28u: goto tr469;
		case 29u: goto tr470;
		case 30u: goto tr471;
		case 31u: goto tr472;
		case 32u: goto tr537;
		case 59u: goto tr539;
		case 64u: goto tr540;
		case 65u: goto tr541;
		case 66u: goto tr542;
		case 67u: goto tr543;
		case 68u: goto tr544;
		case 69u: goto tr545;
		case 70u: goto tr546;
		case 71u: goto tr547;
		case 72u: goto tr548;
		case 73u: goto tr549;
		case 74u: goto tr550;
		case 75u: goto tr551;
		case 76u: goto tr552;
		case 77u: goto tr553;
		case 79u: goto tr554;
		case 80u: goto tr555;
		case 81u: goto tr556;
		case 82u: goto tr557;
		case 83u: goto tr558;
		case 84u: goto tr559;
		case 85u: goto tr560;
		case 86u: goto tr561;
		case 87u: goto tr562;
		case 88u: goto tr563;
		case 89u: goto tr564;
		case 90u: goto tr565;
		case 91u: goto tr566;
		case 92u: goto tr567;
		case 93u: goto tr568;
		case 94u: goto tr569;
		case 96u: goto tr570;
		case 97u: goto tr571;
		case 98u: goto tr572;
		case 99u: goto tr573;
		case 100u: goto tr574;
		case 101u: goto tr575;
		case 103u: goto tr576;
		case 105u: goto tr577;
		case 106u: goto tr578;
		case 107u: goto tr579;
		case 108u: goto tr580;
		case 109u: goto tr581;
		case 110u: goto tr582;
		case 111u: goto tr583;
		case 130u: goto tr511;
		case 131u: goto tr512;
		case 133u: goto tr513;
		case 134u: goto tr514;
		case 135u: goto tr515;
		case 136u: goto tr516;
		case 137u: goto tr517;
		case 138u: goto tr518;
		case 139u: goto tr519;
		case 140u: goto tr520;
		case 141u: goto tr521;
		case 142u: goto tr522;
		case 143u: goto tr523;
		case 144u: goto tr524;
		case 145u: goto tr525;
		case 146u: goto tr526;
		case 147u: goto tr527;
		case 148u: goto tr528;
		case 149u: goto tr529;
		case 150u: goto tr530;
		case 151u: goto tr531;
		case 152u: goto tr287;
		case 154u: goto tr532;
		case 155u: goto tr533;
		case 156u: goto tr515;
		case 157u: goto tr534;
		case 158u: goto tr535;
		case 159u: goto tr536;
	}
	if ( (*p) < 48u ) {
		if ( 33u <= (*p) && (*p) <= 47u )
			goto tr473;
	} else if ( (*p) > 57u ) {
		if ( 58u <= (*p) && (*p) <= 126u )
			goto tr473;
	} else
		goto tr538;
	goto tr1;
case 52:
	switch( (*p) ) {
		case 0u: goto tr447;
		case 1u: goto tr448;
		case 2u: goto tr449;
		case 6u: goto tr450;
		case 7u: goto tr451;
		case 8u: goto tr452;
		case 9u: goto tr453;
		case 10u: goto tr454;
		case 11u: goto tr455;
		case 12u: goto tr456;
		case 13u: goto tr457;
		case 14u: goto tr458;
		case 15u: goto tr459;
		case 16u: goto tr460;
		case 17u: goto tr461;
		case 18u: goto tr462;
		case 19u: goto tr463;
		case 20u: goto tr464;
		case 24u: goto tr465;
		case 25u: goto tr466;
		case 26u: goto tr467;
		case 27u: goto tr468;
		case 28u: goto tr469;
		case 29u: goto tr470;
		case 30u: goto tr471;
		case 31u: goto tr472;
		case 32u: goto tr584;
		case 59u: goto tr585;
		case 72u: goto tr548;
		case 82u: goto tr557;
		case 87u: goto tr562;
		case 103u: goto tr586;
		case 108u: goto tr580;
		case 109u: goto tr581;
		case 111u: goto tr583;
		case 130u: goto tr511;
		case 131u: goto tr512;
		case 133u: goto tr513;
		case 134u: goto tr514;
		case 135u: goto tr515;
		case 136u: goto tr516;
		case 137u: goto tr517;
		case 138u: goto tr518;
		case 139u: goto tr519;
		case 140u: goto tr520;
		case 141u: goto tr521;
		case 142u: goto tr522;
		case 143u: goto tr523;
		case 144u: goto tr524;
		case 145u: goto tr525;
		case 146u: goto tr526;
		case 147u: goto tr527;
		case 148u: goto tr528;
		case 149u: goto tr529;
		case 150u: goto tr530;
		case 151u: goto tr531;
		case 152u: goto tr287;
		case 154u: goto tr532;
		case 155u: goto tr533;
		case 156u: goto tr515;
		case 157u: goto tr534;
		case 158u: goto tr535;
		case 159u: goto tr536;
	}
	if ( (*p) < 48u ) {
		if ( 33u <= (*p) && (*p) <= 47u )
			goto tr473;
	} else if ( (*p) > 57u ) {
		if ( 58u <= (*p) && (*p) <= 126u )
			goto tr473;
	} else
		goto tr539;
	goto tr1;
case 53:
	switch( (*p) ) {
		case 0u: goto tr447;
		case 1u: goto tr448;
		case 2u: goto tr449;
		case 6u: goto tr450;
		case 7u: goto tr451;
		case 8u: goto tr452;
		case 9u: goto tr453;
		case 10u: goto tr454;
		case 11u: goto tr455;
		case 12u: goto tr456;
		case 13u: goto tr457;
		case 14u: goto tr458;
		case 15u: goto tr459;
		case 16u: goto tr460;
		case 17u: goto tr461;
		case 18u: goto tr462;
		case 19u: goto tr463;
		case 20u: goto tr464;
		case 24u: goto tr465;
		case 25u: goto tr466;
		case 26u: goto tr467;
		case 27u: goto tr468;
		case 28u: goto tr469;
		case 29u: goto tr470;
		case 30u: goto tr471;
		case 31u: goto tr472;
		case 66u: goto tr476;
		case 68u: goto tr477;
		case 70u: goto tr479;
		case 71u: goto tr480;
		case 72u: goto tr481;
		case 84u: goto tr488;
		case 89u: goto tr493;
		case 99u: goto tr502;
		case 107u: goto tr510;
		case 130u: goto tr511;
		case 131u: goto tr512;
		case 133u: goto tr513;
		case 134u: goto tr514;
		case 135u: goto tr515;
		case 136u: goto tr516;
		case 137u: goto tr517;
		case 138u: goto tr518;
		case 139u: goto tr519;
		case 140u: goto tr520;
		case 141u: goto tr521;
		case 142u: goto tr522;
		case 143u: goto tr523;
		case 144u: goto tr524;
		case 145u: goto tr525;
		case 146u: goto tr526;
		case 147u: goto tr527;
		case 148u: goto tr528;
		case 149u: goto tr529;
		case 150u: goto tr530;
		case 151u: goto tr531;
		case 152u: goto tr287;
		case 154u: goto tr532;
		case 155u: goto tr533;
		case 156u: goto tr515;
		case 157u: goto tr534;
		case 158u: goto tr535;
		case 159u: goto tr536;
	}
	if ( 32u <= (*p) && (*p) <= 126u )
		goto tr473;
	goto tr1;
case 54:
	switch( (*p) ) {
		case 0u: goto tr447;
		case 1u: goto tr448;
		case 2u: goto tr449;
		case 6u: goto tr450;
		case 7u: goto tr451;
		case 8u: goto tr452;
		case 9u: goto tr453;
		case 10u: goto tr454;
		case 11u: goto tr455;
		case 12u: goto tr456;
		case 13u: goto tr457;
		case 14u: goto tr458;
		case 15u: goto tr459;
		case 16u: goto tr460;
		case 17u: goto tr461;
		case 18u: goto tr462;
		case 19u: goto tr463;
		case 20u: goto tr464;
		case 24u: goto tr465;
		case 25u: goto tr466;
		case 26u: goto tr467;
		case 27u: goto tr468;
		case 28u: goto tr469;
		case 29u: goto tr470;
		case 30u: goto tr471;
		case 31u: goto tr472;
		case 32u: goto tr587;
		case 58u: goto tr473;
		case 87u: goto tr562;
		case 103u: goto tr586;
		case 108u: goto tr580;
		case 109u: goto tr581;
		case 111u: goto tr583;
		case 130u: goto tr511;
		case 131u: goto tr512;
		case 133u: goto tr513;
		case 134u: goto tr514;
		case 135u: goto tr515;
		case 136u: goto tr516;
		case 137u: goto tr517;
		case 138u: goto tr518;
		case 139u: goto tr519;
		case 140u: goto tr520;
		case 141u: goto tr521;
		case 142u: goto tr522;
		case 143u: goto tr523;
		case 144u: goto tr524;
		case 145u: goto tr525;
		case 146u: goto tr526;
		case 147u: goto tr527;
		case 148u: goto tr528;
		case 149u: goto tr529;
		case 150u: goto tr530;
		case 151u: goto tr531;
		case 152u: goto tr287;
		case 154u: goto tr532;
		case 155u: goto tr533;
		case 156u: goto tr515;
		case 157u: goto tr534;
		case 158u: goto tr535;
		case 159u: goto tr536;
	}
	if ( (*p) < 48u ) {
		if ( 33u <= (*p) && (*p) <= 47u )
			goto tr473;
	} else if ( (*p) > 59u ) {
		if ( 60u <= (*p) && (*p) <= 126u )
			goto tr473;
	} else
		goto tr585;
	goto tr1;
case 55:
	switch( (*p) ) {
		case 0u: goto tr447;
		case 1u: goto tr448;
		case 2u: goto tr449;
		case 6u: goto tr450;
		case 7u: goto tr451;
		case 8u: goto tr452;
		case 9u: goto tr453;
		case 10u: goto tr454;
		case 11u: goto tr455;
		case 12u: goto tr456;
		case 13u: goto tr457;
		case 14u: goto tr458;
		case 15u: goto tr459;
		case 16u: goto tr460;
		case 17u: goto tr461;
		case 18u: goto tr462;
		case 19u: goto tr463;
		case 20u: goto tr464;
		case 24u: goto tr465;
		case 25u: goto tr466;
		case 26u: goto tr467;
		case 27u: goto tr468;
		case 28u: goto tr469;
		case 29u: goto tr470;
		case 30u: goto tr471;
		case 31u: goto tr472;
		case 70u: goto tr479;
		case 72u: goto tr481;
		case 130u: goto tr511;
		case 131u: goto tr512;
		case 133u: goto tr513;
		case 134u: goto tr514;
		case 135u: goto tr515;
		case 136u: goto tr516;
		case 137u: goto tr517;
		case 138u: goto tr518;
		case 139u: goto tr519;
		case 140u: goto tr520;
		case 141u: goto tr521;
		case 142u: goto tr522;
		case 143u: goto tr523;
		case 144u: goto tr524;
		case 145u: goto tr525;
		case 146u: goto tr526;
		case 147u: goto tr527;
		case 148u: goto tr528;
		case 149u: goto tr529;
		case 150u: goto tr530;
		case 151u: goto tr531;
		case 152u: goto tr287;
		case 154u: goto tr532;
		case 155u: goto tr533;
		case 156u: goto tr515;
		case 157u: goto tr534;
		case 158u: goto tr535;
		case 159u: goto tr536;
	}
	if ( 32u <= (*p) && (*p) <= 126u )
		goto tr473;
	goto tr1;
case 56:
	switch( (*p) ) {
		case 0u: goto tr588;
		case 1u: goto tr589;
		case 2u: goto tr590;
		case 6u: goto tr591;
		case 7u: goto tr592;
		case 8u: goto tr593;
		case 9u: goto tr594;
		case 10u: goto tr595;
		case 11u: goto tr596;
		case 12u: goto tr597;
		case 13u: goto tr598;
		case 14u: goto tr599;
		case 15u: goto tr600;
		case 16u: goto tr601;
		case 17u: goto tr602;
		case 18u: goto tr603;
		case 19u: goto tr604;
		case 20u: goto tr605;
		case 24u: goto tr606;
		case 25u: goto tr607;
		case 26u: goto tr608;
		case 27u: goto tr609;
		case 28u: goto tr610;
		case 29u: goto tr611;
		case 30u: goto tr612;
		case 31u: goto tr613;
		case 130u: goto tr615;
		case 131u: goto tr616;
		case 133u: goto tr617;
		case 134u: goto tr618;
		case 135u: goto tr619;
		case 136u: goto tr620;
		case 137u: goto tr621;
		case 138u: goto tr622;
		case 139u: goto tr623;
		case 140u: goto tr624;
		case 141u: goto tr625;
		case 142u: goto tr626;
		case 143u: goto tr627;
		case 144u: goto tr628;
		case 145u: goto tr629;
		case 146u: goto tr630;
		case 147u: goto tr631;
		case 148u: goto tr632;
		case 149u: goto tr633;
		case 150u: goto tr634;
		case 151u: goto tr635;
		case 152u: goto tr393;
		case 154u: goto tr636;
		case 155u: goto tr637;
		case 156u: goto tr619;
		case 157u: goto tr638;
		case 158u: goto tr639;
		case 159u: goto tr640;
	}
	if ( 32u <= (*p) && (*p) <= 126u )
		goto tr614;
	goto tr1;
	}

	tr1: cs = 0; goto _again;
	tr6: cs = 1; goto _again;
	tr448: cs = 1; goto f103;
	tr589: cs = 1; goto f228;
	tr0: cs = 2; goto _again;
	tr2: cs = 3; goto _again;
	tr449: cs = 3; goto f103;
	tr590: cs = 3; goto f228;
	tr4: cs = 4; goto _again;
	tr3: cs = 5; goto _again;
	tr124: cs = 6; goto _again;
	tr468: cs = 6; goto f103;
	tr609: cs = 6; goto f228;
	tr17: cs = 7; goto _again;
	tr522: cs = 7; goto f103;
	tr626: cs = 7; goto f228;
	tr18: cs = 8; goto _again;
	tr523: cs = 8; goto f103;
	tr627: cs = 8; goto f228;
	tr19: cs = 9; goto _again;
	tr524: cs = 9; goto f103;
	tr628: cs = 9; goto f228;
	tr41: cs = 10; goto _again;
	tr27: cs = 11; goto _again;
	tr287: cs = 11; goto f103;
	tr393: cs = 11; goto f228;
	tr43: cs = 12; goto _again;
	tr51: cs = 13; goto _again;
	tr203: cs = 13; goto f103;
	tr346: cs = 13; goto f228;
	tr46: cs = 14; goto _again;
	tr47: cs = 15; goto _again;
	tr204: cs = 15; goto f103;
	tr347: cs = 15; goto f228;
	tr49: cs = 16; goto _again;
	tr48: cs = 17; goto _again;
	tr149: cs = 18; goto _again;
	tr223: cs = 18; goto f103;
	tr366: cs = 18; goto f228;
	tr62: cs = 19; goto _again;
	tr277: cs = 19; goto f103;
	tr383: cs = 19; goto f228;
	tr63: cs = 20; goto _again;
	tr278: cs = 20; goto f103;
	tr384: cs = 20; goto f228;
	tr64: cs = 21; goto _again;
	tr279: cs = 21; goto f103;
	tr385: cs = 21; goto f228;
	tr85: cs = 22; goto _again;
	tr72: cs = 23; goto _again;
	tr288: cs = 23; goto f103;
	tr394: cs = 23; goto f228;
	tr74: cs = 24; goto _again;
	tr291: cs = 24; goto f103;
	tr397: cs = 24; goto f228;
	tr89: cs = 25; goto _again;
	tr75: cs = 26; goto _again;
	tr292: cs = 26; goto f103;
	tr398: cs = 26; goto f228;
	tr92: cs = 27; goto _again;
	tr76: cs = 28; goto _again;
	tr293: cs = 28; goto f103;
	tr399: cs = 28; goto f228;
	tr95: cs = 29; goto _again;
	tr28: cs = 30; goto _again;
	tr532: cs = 30; goto f103;
	tr636: cs = 30; goto f228;
	tr30: cs = 31; goto _again;
	tr534: cs = 31; goto f103;
	tr638: cs = 31; goto f228;
	tr99: cs = 32; goto _again;
	tr31: cs = 33; goto _again;
	tr535: cs = 33; goto f103;
	tr639: cs = 33; goto f228;
	tr101: cs = 34; goto _again;
	tr32: cs = 35; goto _again;
	tr536: cs = 35; goto f103;
	tr640: cs = 35; goto f228;
	tr103: cs = 36; goto _again;
	tr5: cs = 37; goto _again;
	tr7: cs = 37; goto f0;
	tr8: cs = 37; goto f1;
	tr9: cs = 37; goto f2;
	tr10: cs = 37; goto f3;
	tr11: cs = 37; goto f4;
	tr12: cs = 37; goto f5;
	tr13: cs = 37; goto f6;
	tr14: cs = 37; goto f7;
	tr15: cs = 37; goto f8;
	tr16: cs = 37; goto f9;
	tr20: cs = 37; goto f10;
	tr21: cs = 37; goto f11;
	tr22: cs = 37; goto f12;
	tr23: cs = 37; goto f13;
	tr24: cs = 37; goto f14;
	tr25: cs = 37; goto f15;
	tr26: cs = 37; goto f16;
	tr33: cs = 37; goto f17;
	tr34: cs = 37; goto f18;
	tr35: cs = 37; goto f19;
	tr36: cs = 37; goto f20;
	tr37: cs = 37; goto f21;
	tr38: cs = 37; goto f22;
	tr39: cs = 37; goto f23;
	tr40: cs = 37; goto f24;
	tr42: cs = 37; goto f25;
	tr44: cs = 37; goto f26;
	tr86: cs = 37; goto f27;
	tr98: cs = 37; goto f28;
	tr90: cs = 37; goto f29;
	tr93: cs = 37; goto f30;
	tr96: cs = 37; goto f31;
	tr100: cs = 37; goto f32;
	tr102: cs = 37; goto f33;
	tr104: cs = 37; goto f34;
	tr105: cs = 37; goto f35;
	tr106: cs = 37; goto f36;
	tr107: cs = 37; goto f37;
	tr108: cs = 37; goto f38;
	tr109: cs = 37; goto f39;
	tr110: cs = 37; goto f40;
	tr111: cs = 37; goto f41;
	tr112: cs = 37; goto f42;
	tr113: cs = 37; goto f43;
	tr114: cs = 37; goto f44;
	tr115: cs = 37; goto f45;
	tr116: cs = 37; goto f46;
	tr117: cs = 37; goto f47;
	tr118: cs = 37; goto f48;
	tr119: cs = 37; goto f49;
	tr120: cs = 37; goto f50;
	tr121: cs = 37; goto f51;
	tr122: cs = 37; goto f52;
	tr123: cs = 37; goto f53;
	tr125: cs = 37; goto f54;
	tr126: cs = 37; goto f55;
	tr127: cs = 37; goto f56;
	tr128: cs = 37; goto f57;
	tr515: cs = 37; goto f103;
	tr447: cs = 37; goto f104;
	tr450: cs = 37; goto f105;
	tr451: cs = 37; goto f106;
	tr452: cs = 37; goto f107;
	tr453: cs = 37; goto f108;
	tr454: cs = 37; goto f109;
	tr455: cs = 37; goto f110;
	tr456: cs = 37; goto f111;
	tr457: cs = 37; goto f112;
	tr458: cs = 37; goto f113;
	tr459: cs = 37; goto f114;
	tr460: cs = 37; goto f115;
	tr461: cs = 37; goto f116;
	tr462: cs = 37; goto f117;
	tr463: cs = 37; goto f118;
	tr464: cs = 37; goto f119;
	tr465: cs = 37; goto f120;
	tr466: cs = 37; goto f121;
	tr467: cs = 37; goto f122;
	tr469: cs = 37; goto f123;
	tr470: cs = 37; goto f124;
	tr471: cs = 37; goto f125;
	tr472: cs = 37; goto f126;
	tr511: cs = 37; goto f164;
	tr512: cs = 37; goto f165;
	tr513: cs = 37; goto f166;
	tr514: cs = 37; goto f167;
	tr516: cs = 37; goto f168;
	tr517: cs = 37; goto f169;
	tr518: cs = 37; goto f170;
	tr519: cs = 37; goto f171;
	tr520: cs = 37; goto f172;
	tr521: cs = 37; goto f173;
	tr525: cs = 37; goto f174;
	tr526: cs = 37; goto f175;
	tr527: cs = 37; goto f176;
	tr528: cs = 37; goto f177;
	tr529: cs = 37; goto f178;
	tr530: cs = 37; goto f179;
	tr531: cs = 37; goto f180;
	tr290: cs = 37; goto f181;
	tr619: cs = 37; goto f228;
	tr588: cs = 37; goto f229;
	tr591: cs = 37; goto f230;
	tr592: cs = 37; goto f231;
	tr593: cs = 37; goto f232;
	tr594: cs = 37; goto f233;
	tr595: cs = 37; goto f234;
	tr596: cs = 37; goto f235;
	tr597: cs = 37; goto f236;
	tr598: cs = 37; goto f237;
	tr599: cs = 37; goto f238;
	tr600: cs = 37; goto f239;
	tr601: cs = 37; goto f240;
	tr602: cs = 37; goto f241;
	tr603: cs = 37; goto f242;
	tr604: cs = 37; goto f243;
	tr605: cs = 37; goto f244;
	tr606: cs = 37; goto f245;
	tr607: cs = 37; goto f246;
	tr608: cs = 37; goto f247;
	tr610: cs = 37; goto f248;
	tr611: cs = 37; goto f249;
	tr612: cs = 37; goto f250;
	tr613: cs = 37; goto f251;
	tr615: cs = 37; goto f252;
	tr616: cs = 37; goto f253;
	tr617: cs = 37; goto f254;
	tr618: cs = 37; goto f255;
	tr620: cs = 37; goto f256;
	tr621: cs = 37; goto f257;
	tr622: cs = 37; goto f258;
	tr623: cs = 37; goto f259;
	tr624: cs = 37; goto f260;
	tr625: cs = 37; goto f261;
	tr629: cs = 37; goto f262;
	tr630: cs = 37; goto f263;
	tr631: cs = 37; goto f264;
	tr632: cs = 37; goto f265;
	tr633: cs = 37; goto f266;
	tr634: cs = 37; goto f267;
	tr635: cs = 37; goto f268;
	tr396: cs = 37; goto f269;
	tr50: cs = 38; goto _again;
	tr52: cs = 38; goto f0;
	tr53: cs = 38; goto f1;
	tr54: cs = 38; goto f2;
	tr55: cs = 38; goto f3;
	tr56: cs = 38; goto f4;
	tr57: cs = 38; goto f5;
	tr58: cs = 38; goto f6;
	tr59: cs = 38; goto f7;
	tr60: cs = 38; goto f8;
	tr61: cs = 38; goto f9;
	tr65: cs = 38; goto f10;
	tr66: cs = 38; goto f11;
	tr67: cs = 38; goto f12;
	tr68: cs = 38; goto f13;
	tr69: cs = 38; goto f14;
	tr70: cs = 38; goto f15;
	tr71: cs = 38; goto f16;
	tr77: cs = 38; goto f17;
	tr78: cs = 38; goto f18;
	tr79: cs = 38; goto f19;
	tr80: cs = 38; goto f20;
	tr81: cs = 38; goto f21;
	tr82: cs = 38; goto f22;
	tr83: cs = 38; goto f23;
	tr84: cs = 38; goto f24;
	tr45: cs = 38; goto f26;
	tr87: cs = 38; goto f27;
	tr88: cs = 38; goto f28;
	tr91: cs = 38; goto f29;
	tr94: cs = 38; goto f30;
	tr97: cs = 38; goto f31;
	tr130: cs = 38; goto f35;
	tr131: cs = 38; goto f36;
	tr132: cs = 38; goto f37;
	tr133: cs = 38; goto f38;
	tr134: cs = 38; goto f39;
	tr135: cs = 38; goto f40;
	tr136: cs = 38; goto f41;
	tr137: cs = 38; goto f42;
	tr138: cs = 38; goto f43;
	tr139: cs = 38; goto f44;
	tr140: cs = 38; goto f45;
	tr141: cs = 38; goto f46;
	tr142: cs = 38; goto f47;
	tr143: cs = 38; goto f48;
	tr144: cs = 38; goto f49;
	tr145: cs = 38; goto f50;
	tr146: cs = 38; goto f51;
	tr147: cs = 38; goto f52;
	tr148: cs = 38; goto f53;
	tr150: cs = 38; goto f54;
	tr151: cs = 38; goto f55;
	tr152: cs = 38; goto f56;
	tr153: cs = 38; goto f57;
	tr270: cs = 38; goto f103;
	tr202: cs = 38; goto f104;
	tr205: cs = 38; goto f105;
	tr206: cs = 38; goto f106;
	tr207: cs = 38; goto f107;
	tr208: cs = 38; goto f108;
	tr209: cs = 38; goto f109;
	tr210: cs = 38; goto f110;
	tr211: cs = 38; goto f111;
	tr212: cs = 38; goto f112;
	tr213: cs = 38; goto f113;
	tr214: cs = 38; goto f114;
	tr215: cs = 38; goto f115;
	tr216: cs = 38; goto f116;
	tr217: cs = 38; goto f117;
	tr218: cs = 38; goto f118;
	tr219: cs = 38; goto f119;
	tr220: cs = 38; goto f120;
	tr221: cs = 38; goto f121;
	tr222: cs = 38; goto f122;
	tr224: cs = 38; goto f123;
	tr225: cs = 38; goto f124;
	tr226: cs = 38; goto f125;
	tr227: cs = 38; goto f126;
	tr266: cs = 38; goto f164;
	tr267: cs = 38; goto f165;
	tr268: cs = 38; goto f166;
	tr269: cs = 38; goto f167;
	tr271: cs = 38; goto f168;
	tr272: cs = 38; goto f169;
	tr273: cs = 38; goto f170;
	tr274: cs = 38; goto f171;
	tr275: cs = 38; goto f172;
	tr276: cs = 38; goto f173;
	tr280: cs = 38; goto f174;
	tr281: cs = 38; goto f175;
	tr282: cs = 38; goto f176;
	tr283: cs = 38; goto f177;
	tr284: cs = 38; goto f178;
	tr285: cs = 38; goto f179;
	tr286: cs = 38; goto f180;
	tr376: cs = 38; goto f228;
	tr345: cs = 38; goto f229;
	tr348: cs = 38; goto f230;
	tr349: cs = 38; goto f231;
	tr350: cs = 38; goto f232;
	tr351: cs = 38; goto f233;
	tr352: cs = 38; goto f234;
	tr353: cs = 38; goto f235;
	tr354: cs = 38; goto f236;
	tr355: cs = 38; goto f237;
	tr356: cs = 38; goto f238;
	tr357: cs = 38; goto f239;
	tr358: cs = 38; goto f240;
	tr359: cs = 38; goto f241;
	tr360: cs = 38; goto f242;
	tr361: cs = 38; goto f243;
	tr362: cs = 38; goto f244;
	tr363: cs = 38; goto f245;
	tr364: cs = 38; goto f246;
	tr365: cs = 38; goto f247;
	tr367: cs = 38; goto f248;
	tr368: cs = 38; goto f249;
	tr369: cs = 38; goto f250;
	tr370: cs = 38; goto f251;
	tr372: cs = 38; goto f252;
	tr373: cs = 38; goto f253;
	tr374: cs = 38; goto f254;
	tr375: cs = 38; goto f255;
	tr377: cs = 38; goto f256;
	tr378: cs = 38; goto f257;
	tr379: cs = 38; goto f258;
	tr380: cs = 38; goto f259;
	tr381: cs = 38; goto f260;
	tr382: cs = 38; goto f261;
	tr386: cs = 38; goto f262;
	tr387: cs = 38; goto f263;
	tr388: cs = 38; goto f264;
	tr389: cs = 38; goto f265;
	tr390: cs = 38; goto f266;
	tr391: cs = 38; goto f267;
	tr392: cs = 38; goto f268;
	tr73: cs = 39; goto _again;
	tr289: cs = 39; goto f103;
	tr395: cs = 39; goto f228;
	tr155: cs = 40; goto _again;
	tr294: cs = 40; goto f103;
	tr154: cs = 41; goto _again;
	tr158: cs = 41; goto f60;
	tr159: cs = 41; goto f61;
	tr160: cs = 41; goto f62;
	tr161: cs = 41; goto f63;
	tr162: cs = 41; goto f64;
	tr163: cs = 41; goto f65;
	tr164: cs = 41; goto f66;
	tr165: cs = 41; goto f67;
	tr166: cs = 41; goto f68;
	tr167: cs = 41; goto f69;
	tr168: cs = 41; goto f70;
	tr169: cs = 41; goto f71;
	tr170: cs = 41; goto f72;
	tr171: cs = 41; goto f73;
	tr172: cs = 41; goto f74;
	tr173: cs = 41; goto f75;
	tr174: cs = 41; goto f76;
	tr175: cs = 41; goto f77;
	tr176: cs = 41; goto f78;
	tr177: cs = 41; goto f79;
	tr178: cs = 41; goto f80;
	tr179: cs = 41; goto f81;
	tr180: cs = 41; goto f82;
	tr181: cs = 41; goto f83;
	tr182: cs = 41; goto f84;
	tr183: cs = 41; goto f85;
	tr184: cs = 41; goto f86;
	tr185: cs = 41; goto f87;
	tr186: cs = 41; goto f88;
	tr187: cs = 41; goto f89;
	tr188: cs = 41; goto f90;
	tr189: cs = 41; goto f91;
	tr190: cs = 41; goto f92;
	tr191: cs = 41; goto f93;
	tr192: cs = 41; goto f94;
	tr193: cs = 41; goto f95;
	tr194: cs = 41; goto f96;
	tr195: cs = 41; goto f97;
	tr196: cs = 41; goto f98;
	tr197: cs = 41; goto f99;
	tr198: cs = 41; goto f100;
	tr200: cs = 41; goto f101;
	tr201: cs = 41; goto f102;
	tr228: cs = 41; goto f103;
	tr229: cs = 41; goto f127;
	tr230: cs = 41; goto f128;
	tr231: cs = 41; goto f129;
	tr232: cs = 41; goto f130;
	tr233: cs = 41; goto f131;
	tr234: cs = 41; goto f132;
	tr235: cs = 41; goto f133;
	tr236: cs = 41; goto f134;
	tr237: cs = 41; goto f135;
	tr238: cs = 41; goto f136;
	tr239: cs = 41; goto f137;
	tr240: cs = 41; goto f138;
	tr241: cs = 41; goto f139;
	tr242: cs = 41; goto f140;
	tr243: cs = 41; goto f141;
	tr244: cs = 41; goto f142;
	tr245: cs = 41; goto f143;
	tr246: cs = 41; goto f144;
	tr247: cs = 41; goto f145;
	tr248: cs = 41; goto f146;
	tr249: cs = 41; goto f147;
	tr250: cs = 41; goto f148;
	tr251: cs = 41; goto f149;
	tr252: cs = 41; goto f150;
	tr253: cs = 41; goto f151;
	tr254: cs = 41; goto f152;
	tr255: cs = 41; goto f153;
	tr256: cs = 41; goto f154;
	tr257: cs = 41; goto f155;
	tr258: cs = 41; goto f156;
	tr259: cs = 41; goto f157;
	tr260: cs = 41; goto f158;
	tr261: cs = 41; goto f159;
	tr262: cs = 41; goto f160;
	tr263: cs = 41; goto f161;
	tr264: cs = 41; goto f162;
	tr265: cs = 41; goto f163;
	tr297: cs = 41; goto f184;
	tr298: cs = 41; goto f185;
	tr299: cs = 41; goto f186;
	tr300: cs = 41; goto f187;
	tr301: cs = 41; goto f188;
	tr302: cs = 41; goto f189;
	tr303: cs = 41; goto f190;
	tr304: cs = 41; goto f191;
	tr305: cs = 41; goto f192;
	tr306: cs = 41; goto f193;
	tr307: cs = 41; goto f194;
	tr308: cs = 41; goto f195;
	tr309: cs = 41; goto f196;
	tr310: cs = 41; goto f197;
	tr311: cs = 41; goto f198;
	tr312: cs = 41; goto f199;
	tr313: cs = 41; goto f200;
	tr314: cs = 41; goto f201;
	tr315: cs = 41; goto f202;
	tr316: cs = 41; goto f203;
	tr317: cs = 41; goto f204;
	tr318: cs = 41; goto f205;
	tr319: cs = 41; goto f206;
	tr320: cs = 41; goto f207;
	tr321: cs = 41; goto f208;
	tr322: cs = 41; goto f209;
	tr323: cs = 41; goto f210;
	tr324: cs = 41; goto f211;
	tr325: cs = 41; goto f212;
	tr326: cs = 41; goto f213;
	tr327: cs = 41; goto f214;
	tr328: cs = 41; goto f215;
	tr329: cs = 41; goto f216;
	tr330: cs = 41; goto f217;
	tr331: cs = 41; goto f218;
	tr332: cs = 41; goto f219;
	tr333: cs = 41; goto f220;
	tr334: cs = 41; goto f221;
	tr335: cs = 41; goto f222;
	tr336: cs = 41; goto f223;
	tr337: cs = 41; goto f224;
	tr339: cs = 41; goto f225;
	tr340: cs = 41; goto f226;
	tr343: cs = 41; goto f227;
	tr371: cs = 41; goto f228;
	tr156: cs = 42; goto f58;
	tr295: cs = 42; goto f182;
	tr157: cs = 43; goto f59;
	tr296: cs = 43; goto f183;
	tr341: cs = 44; goto f103;
	tr342: cs = 45; goto f103;
	tr344: cs = 46; goto f103;
	tr199: cs = 47; goto _again;
	tr338: cs = 47; goto f103;
	tr29: cs = 48; goto _again;
	tr533: cs = 48; goto f103;
	tr637: cs = 48; goto f228;
	tr400: cs = 49; goto _again;
	tr537: cs = 49; goto f103;
	tr129: cs = 50; goto _again;
	tr403: cs = 50; goto f60;
	tr404: cs = 50; goto f61;
	tr405: cs = 50; goto f62;
	tr406: cs = 50; goto f63;
	tr407: cs = 50; goto f64;
	tr408: cs = 50; goto f65;
	tr409: cs = 50; goto f66;
	tr410: cs = 50; goto f67;
	tr411: cs = 50; goto f68;
	tr412: cs = 50; goto f69;
	tr413: cs = 50; goto f70;
	tr414: cs = 50; goto f71;
	tr415: cs = 50; goto f72;
	tr416: cs = 50; goto f73;
	tr417: cs = 50; goto f74;
	tr418: cs = 50; goto f75;
	tr419: cs = 50; goto f76;
	tr420: cs = 50; goto f77;
	tr421: cs = 50; goto f78;
	tr422: cs = 50; goto f79;
	tr423: cs = 50; goto f80;
	tr424: cs = 50; goto f81;
	tr425: cs = 50; goto f82;
	tr426: cs = 50; goto f83;
	tr427: cs = 50; goto f84;
	tr428: cs = 50; goto f85;
	tr429: cs = 50; goto f86;
	tr430: cs = 50; goto f87;
	tr431: cs = 50; goto f88;
	tr432: cs = 50; goto f89;
	tr433: cs = 50; goto f90;
	tr434: cs = 50; goto f91;
	tr435: cs = 50; goto f92;
	tr436: cs = 50; goto f93;
	tr437: cs = 50; goto f94;
	tr438: cs = 50; goto f95;
	tr439: cs = 50; goto f96;
	tr440: cs = 50; goto f97;
	tr441: cs = 50; goto f98;
	tr442: cs = 50; goto f99;
	tr443: cs = 50; goto f100;
	tr445: cs = 50; goto f101;
	tr446: cs = 50; goto f102;
	tr473: cs = 50; goto f103;
	tr474: cs = 50; goto f127;
	tr475: cs = 50; goto f128;
	tr476: cs = 50; goto f129;
	tr477: cs = 50; goto f130;
	tr478: cs = 50; goto f131;
	tr479: cs = 50; goto f132;
	tr480: cs = 50; goto f133;
	tr481: cs = 50; goto f134;
	tr482: cs = 50; goto f135;
	tr483: cs = 50; goto f136;
	tr484: cs = 50; goto f137;
	tr485: cs = 50; goto f138;
	tr486: cs = 50; goto f139;
	tr487: cs = 50; goto f140;
	tr488: cs = 50; goto f141;
	tr489: cs = 50; goto f142;
	tr490: cs = 50; goto f143;
	tr491: cs = 50; goto f144;
	tr492: cs = 50; goto f145;
	tr493: cs = 50; goto f146;
	tr494: cs = 50; goto f147;
	tr495: cs = 50; goto f148;
	tr496: cs = 50; goto f149;
	tr497: cs = 50; goto f150;
	tr498: cs = 50; goto f151;
	tr499: cs = 50; goto f152;
	tr500: cs = 50; goto f153;
	tr501: cs = 50; goto f154;
	tr502: cs = 50; goto f155;
	tr503: cs = 50; goto f156;
	tr504: cs = 50; goto f157;
	tr505: cs = 50; goto f158;
	tr506: cs = 50; goto f159;
	tr507: cs = 50; goto f160;
	tr508: cs = 50; goto f161;
	tr509: cs = 50; goto f162;
	tr510: cs = 50; goto f163;
	tr540: cs = 50; goto f184;
	tr541: cs = 50; goto f185;
	tr542: cs = 50; goto f186;
	tr543: cs = 50; goto f187;
	tr544: cs = 50; goto f188;
	tr545: cs = 50; goto f189;
	tr546: cs = 50; goto f190;
	tr547: cs = 50; goto f191;
	tr548: cs = 50; goto f192;
	tr549: cs = 50; goto f193;
	tr550: cs = 50; goto f194;
	tr551: cs = 50; goto f195;
	tr552: cs = 50; goto f196;
	tr553: cs = 50; goto f197;
	tr554: cs = 50; goto f198;
	tr555: cs = 50; goto f199;
	tr556: cs = 50; goto f200;
	tr557: cs = 50; goto f201;
	tr558: cs = 50; goto f202;
	tr559: cs = 50; goto f203;
	tr560: cs = 50; goto f204;
	tr561: cs = 50; goto f205;
	tr562: cs = 50; goto f206;
	tr563: cs = 50; goto f207;
	tr564: cs = 50; goto f208;
	tr565: cs = 50; goto f209;
	tr566: cs = 50; goto f210;
	tr567: cs = 50; goto f211;
	tr568: cs = 50; goto f212;
	tr569: cs = 50; goto f213;
	tr570: cs = 50; goto f214;
	tr571: cs = 50; goto f215;
	tr572: cs = 50; goto f216;
	tr573: cs = 50; goto f217;
	tr574: cs = 50; goto f218;
	tr575: cs = 50; goto f219;
	tr576: cs = 50; goto f220;
	tr577: cs = 50; goto f221;
	tr578: cs = 50; goto f222;
	tr579: cs = 50; goto f223;
	tr580: cs = 50; goto f224;
	tr582: cs = 50; goto f225;
	tr583: cs = 50; goto f226;
	tr586: cs = 50; goto f227;
	tr614: cs = 50; goto f228;
	tr401: cs = 51; goto f58;
	tr538: cs = 51; goto f182;
	tr402: cs = 52; goto f59;
	tr539: cs = 52; goto f183;
	tr584: cs = 53; goto f103;
	tr585: cs = 54; goto f103;
	tr587: cs = 55; goto f103;
	tr444: cs = 56; goto _again;
	tr581: cs = 56; goto f103;

	f36: _acts = _ECMA48_actions + 1; goto execFuncs;
	f34: _acts = _ECMA48_actions + 3; goto execFuncs;
	f37: _acts = _ECMA48_actions + 5; goto execFuncs;
	f0: _acts = _ECMA48_actions + 7; goto execFuncs;
	f38: _acts = _ECMA48_actions + 9; goto execFuncs;
	f51: _acts = _ECMA48_actions + 11; goto execFuncs;
	f85: _acts = _ECMA48_actions + 13; goto execFuncs;
	f13: _acts = _ECMA48_actions + 15; goto execFuncs;
	f67: _acts = _ECMA48_actions + 17; goto execFuncs;
	f69: _acts = _ECMA48_actions + 19; goto execFuncs;
	f65: _acts = _ECMA48_actions + 21; goto execFuncs;
	f66: _acts = _ECMA48_actions + 23; goto execFuncs;
	f77: _acts = _ECMA48_actions + 25; goto execFuncs;
	f43: _acts = _ECMA48_actions + 27; goto execFuncs;
	f82: _acts = _ECMA48_actions + 29; goto execFuncs;
	f64: _acts = _ECMA48_actions + 31; goto execFuncs;
	f62: _acts = _ECMA48_actions + 33; goto execFuncs;
	f63: _acts = _ECMA48_actions + 35; goto execFuncs;
	f68: _acts = _ECMA48_actions + 37; goto execFuncs;
	f61: _acts = _ECMA48_actions + 39; goto execFuncs;
	f84: _acts = _ECMA48_actions + 41; goto execFuncs;
	f93: _acts = _ECMA48_actions + 43; goto execFuncs;
	f102: _acts = _ECMA48_actions + 45; goto execFuncs;
	f75: _acts = _ECMA48_actions + 47; goto execFuncs;
	f25: _acts = _ECMA48_actions + 49; goto execFuncs;
	f47: _acts = _ECMA48_actions + 51; goto execFuncs;
	f48: _acts = _ECMA48_actions + 53; goto execFuncs;
	f49: _acts = _ECMA48_actions + 55; goto execFuncs;
	f50: _acts = _ECMA48_actions + 57; goto execFuncs;
	f46: _acts = _ECMA48_actions + 59; goto execFuncs;
	f101: _acts = _ECMA48_actions + 61; goto execFuncs;
	f74: _acts = _ECMA48_actions + 63; goto execFuncs;
	f83: _acts = _ECMA48_actions + 65; goto execFuncs;
	f70: _acts = _ECMA48_actions + 67; goto execFuncs;
	f71: _acts = _ECMA48_actions + 69; goto execFuncs;
	f52: _acts = _ECMA48_actions + 71; goto execFuncs;
	f16: _acts = _ECMA48_actions + 73; goto execFuncs;
	f42: _acts = _ECMA48_actions + 75; goto execFuncs;
	f90: _acts = _ECMA48_actions + 77; goto execFuncs;
	f98: _acts = _ECMA48_actions + 79; goto execFuncs;
	f91: _acts = _ECMA48_actions + 81; goto execFuncs;
	f39: _acts = _ECMA48_actions + 83; goto execFuncs;
	f5: _acts = _ECMA48_actions + 85; goto execFuncs;
	f4: _acts = _ECMA48_actions + 87; goto execFuncs;
	f60: _acts = _ECMA48_actions + 89; goto execFuncs;
	f72: _acts = _ECMA48_actions + 91; goto execFuncs;
	f17: _acts = _ECMA48_actions + 93; goto execFuncs;
	f57: _acts = _ECMA48_actions + 95; goto execFuncs;
	f56: _acts = _ECMA48_actions + 97; goto execFuncs;
	f55: _acts = _ECMA48_actions + 99; goto execFuncs;
	f54: _acts = _ECMA48_actions + 101; goto execFuncs;
	f40: _acts = _ECMA48_actions + 103; goto execFuncs;
	f44: _acts = _ECMA48_actions + 105; goto execFuncs;
	f45: _acts = _ECMA48_actions + 107; goto execFuncs;
	f22: _acts = _ECMA48_actions + 109; goto execFuncs;
	f19: _acts = _ECMA48_actions + 111; goto execFuncs;
	f20: _acts = _ECMA48_actions + 113; goto execFuncs;
	f21: _acts = _ECMA48_actions + 115; goto execFuncs;
	f97: _acts = _ECMA48_actions + 117; goto execFuncs;
	f14: _acts = _ECMA48_actions + 119; goto execFuncs;
	f1: _acts = _ECMA48_actions + 121; goto execFuncs;
	f2: _acts = _ECMA48_actions + 123; goto execFuncs;
	f80: _acts = _ECMA48_actions + 125; goto execFuncs;
	f35: _acts = _ECMA48_actions + 127; goto execFuncs;
	f32: _acts = _ECMA48_actions + 129; goto execFuncs;
	f7: _acts = _ECMA48_actions + 131; goto execFuncs;
	f8: _acts = _ECMA48_actions + 133; goto execFuncs;
	f33: _acts = _ECMA48_actions + 135; goto execFuncs;
	f81: _acts = _ECMA48_actions + 137; goto execFuncs;
	f87: _acts = _ECMA48_actions + 139; goto execFuncs;
	f10: _acts = _ECMA48_actions + 141; goto execFuncs;
	f11: _acts = _ECMA48_actions + 143; goto execFuncs;
	f92: _acts = _ECMA48_actions + 145; goto execFuncs;
	f9: _acts = _ECMA48_actions + 147; goto execFuncs;
	f18: _acts = _ECMA48_actions + 149; goto execFuncs;
	f100: _acts = _ECMA48_actions + 151; goto execFuncs;
	f28: _acts = _ECMA48_actions + 153; goto execFuncs;
	f79: _acts = _ECMA48_actions + 155; goto execFuncs;
	f88: _acts = _ECMA48_actions + 157; goto execFuncs;
	f76: _acts = _ECMA48_actions + 159; goto execFuncs;
	f89: _acts = _ECMA48_actions + 161; goto execFuncs;
	f26: _acts = _ECMA48_actions + 163; goto execFuncs;
	f15: _acts = _ECMA48_actions + 165; goto execFuncs;
	f86: _acts = _ECMA48_actions + 167; goto execFuncs;
	f3: _acts = _ECMA48_actions + 169; goto execFuncs;
	f23: _acts = _ECMA48_actions + 171; goto execFuncs;
	f24: _acts = _ECMA48_actions + 173; goto execFuncs;
	f12: _acts = _ECMA48_actions + 175; goto execFuncs;
	f78: _acts = _ECMA48_actions + 177; goto execFuncs;
	f53: _acts = _ECMA48_actions + 179; goto execFuncs;
	f94: _acts = _ECMA48_actions + 181; goto execFuncs;
	f99: _acts = _ECMA48_actions + 183; goto execFuncs;
	f95: _acts = _ECMA48_actions + 185; goto execFuncs;
	f41: _acts = _ECMA48_actions + 187; goto execFuncs;
	f6: _acts = _ECMA48_actions + 189; goto execFuncs;
	f103: _acts = _ECMA48_actions + 191; goto execFuncs;
	f59: _acts = _ECMA48_actions + 193; goto execFuncs;
	f209: _acts = _ECMA48_actions + 195; goto execFuncs;
	f191: _acts = _ECMA48_actions + 198; goto execFuncs;
	f193: _acts = _ECMA48_actions + 201; goto execFuncs;
	f189: _acts = _ECMA48_actions + 204; goto execFuncs;
	f190: _acts = _ECMA48_actions + 207; goto execFuncs;
	f201: _acts = _ECMA48_actions + 210; goto execFuncs;
	f206: _acts = _ECMA48_actions + 213; goto execFuncs;
	f188: _acts = _ECMA48_actions + 216; goto execFuncs;
	f186: _acts = _ECMA48_actions + 219; goto execFuncs;
	f187: _acts = _ECMA48_actions + 222; goto execFuncs;
	f192: _acts = _ECMA48_actions + 225; goto execFuncs;
	f185: _acts = _ECMA48_actions + 228; goto execFuncs;
	f208: _acts = _ECMA48_actions + 231; goto execFuncs;
	f217: _acts = _ECMA48_actions + 234; goto execFuncs;
	f226: _acts = _ECMA48_actions + 237; goto execFuncs;
	f199: _acts = _ECMA48_actions + 240; goto execFuncs;
	f27: _acts = _ECMA48_actions + 243; goto execFuncs;
	f73: _acts = _ECMA48_actions + 246; goto execFuncs;
	f225: _acts = _ECMA48_actions + 249; goto execFuncs;
	f141: _acts = _ECMA48_actions + 252; goto execFuncs;
	f198: _acts = _ECMA48_actions + 255; goto execFuncs;
	f207: _acts = _ECMA48_actions + 258; goto execFuncs;
	f194: _acts = _ECMA48_actions + 261; goto execFuncs;
	f195: _acts = _ECMA48_actions + 264; goto execFuncs;
	f144: _acts = _ECMA48_actions + 267; goto execFuncs;
	f130: _acts = _ECMA48_actions + 270; goto execFuncs;
	f151: _acts = _ECMA48_actions + 273; goto execFuncs;
	f129: _acts = _ECMA48_actions + 276; goto execFuncs;
	f214: _acts = _ECMA48_actions + 279; goto execFuncs;
	f222: _acts = _ECMA48_actions + 282; goto execFuncs;
	f215: _acts = _ECMA48_actions + 285; goto execFuncs;
	f184: _acts = _ECMA48_actions + 288; goto execFuncs;
	f196: _acts = _ECMA48_actions + 291; goto execFuncs;
	f132: _acts = _ECMA48_actions + 294; goto execFuncs;
	f221: _acts = _ECMA48_actions + 297; goto execFuncs;
	f204: _acts = _ECMA48_actions + 300; goto execFuncs;
	f147: _acts = _ECMA48_actions + 303; goto execFuncs;
	f136: _acts = _ECMA48_actions + 306; goto execFuncs;
	f205: _acts = _ECMA48_actions + 309; goto execFuncs;
	f138: _acts = _ECMA48_actions + 312; goto execFuncs;
	f140: _acts = _ECMA48_actions + 315; goto execFuncs;
	f139: _acts = _ECMA48_actions + 318; goto execFuncs;
	f211: _acts = _ECMA48_actions + 321; goto execFuncs;
	f134: _acts = _ECMA48_actions + 324; goto execFuncs;
	f216: _acts = _ECMA48_actions + 327; goto execFuncs;
	f224: _acts = _ECMA48_actions + 330; goto execFuncs;
	f149: _acts = _ECMA48_actions + 333; goto execFuncs;
	f157: _acts = _ECMA48_actions + 336; goto execFuncs;
	f163: _acts = _ECMA48_actions + 339; goto execFuncs;
	f159: _acts = _ECMA48_actions + 342; goto execFuncs;
	f203: _acts = _ECMA48_actions + 345; goto execFuncs;
	f212: _acts = _ECMA48_actions + 348; goto execFuncs;
	f200: _acts = _ECMA48_actions + 351; goto execFuncs;
	f146: _acts = _ECMA48_actions + 354; goto execFuncs;
	f228: _acts = _ECMA48_actions + 357; goto execFuncs;
	f213: _acts = _ECMA48_actions + 360; goto execFuncs;
	f127: _acts = _ECMA48_actions + 363; goto execFuncs;
	f142: _acts = _ECMA48_actions + 366; goto execFuncs;
	f143: _acts = _ECMA48_actions + 369; goto execFuncs;
	f160: _acts = _ECMA48_actions + 372; goto execFuncs;
	f96: _acts = _ECMA48_actions + 375; goto execFuncs;
	f227: _acts = _ECMA48_actions + 378; goto execFuncs;
	f31: _acts = _ECMA48_actions + 381; goto execFuncs;
	f29: _acts = _ECMA48_actions + 384; goto execFuncs;
	f30: _acts = _ECMA48_actions + 387; goto execFuncs;
	f181: _acts = _ECMA48_actions + 390; goto execFuncs;
	f161: _acts = _ECMA48_actions + 393; goto execFuncs;
	f133: _acts = _ECMA48_actions + 396; goto execFuncs;
	f162: _acts = _ECMA48_actions + 399; goto execFuncs;
	f145: _acts = _ECMA48_actions + 402; goto execFuncs;
	f128: _acts = _ECMA48_actions + 405; goto execFuncs;
	f158: _acts = _ECMA48_actions + 408; goto execFuncs;
	f210: _acts = _ECMA48_actions + 411; goto execFuncs;
	f135: _acts = _ECMA48_actions + 414; goto execFuncs;
	f148: _acts = _ECMA48_actions + 417; goto execFuncs;
	f150: _acts = _ECMA48_actions + 420; goto execFuncs;
	f202: _acts = _ECMA48_actions + 423; goto execFuncs;
	f154: _acts = _ECMA48_actions + 426; goto execFuncs;
	f153: _acts = _ECMA48_actions + 429; goto execFuncs;
	f152: _acts = _ECMA48_actions + 432; goto execFuncs;
	f156: _acts = _ECMA48_actions + 435; goto execFuncs;
	f131: _acts = _ECMA48_actions + 438; goto execFuncs;
	f218: _acts = _ECMA48_actions + 441; goto execFuncs;
	f223: _acts = _ECMA48_actions + 444; goto execFuncs;
	f219: _acts = _ECMA48_actions + 447; goto execFuncs;
	f105: _acts = _ECMA48_actions + 450; goto execFuncs;
	f106: _acts = _ECMA48_actions + 453; goto execFuncs;
	f164: _acts = _ECMA48_actions + 456; goto execFuncs;
	f107: _acts = _ECMA48_actions + 459; goto execFuncs;
	f120: _acts = _ECMA48_actions + 462; goto execFuncs;
	f177: _acts = _ECMA48_actions + 465; goto execFuncs;
	f112: _acts = _ECMA48_actions + 468; goto execFuncs;
	f116: _acts = _ECMA48_actions + 471; goto execFuncs;
	f117: _acts = _ECMA48_actions + 474; goto execFuncs;
	f118: _acts = _ECMA48_actions + 477; goto execFuncs;
	f119: _acts = _ECMA48_actions + 480; goto execFuncs;
	f115: _acts = _ECMA48_actions + 483; goto execFuncs;
	f121: _acts = _ECMA48_actions + 486; goto execFuncs;
	f180: _acts = _ECMA48_actions + 489; goto execFuncs;
	f111: _acts = _ECMA48_actions + 492; goto execFuncs;
	f108: _acts = _ECMA48_actions + 495; goto execFuncs;
	f169: _acts = _ECMA48_actions + 498; goto execFuncs;
	f168: _acts = _ECMA48_actions + 501; goto execFuncs;
	f126: _acts = _ECMA48_actions + 504; goto execFuncs;
	f125: _acts = _ECMA48_actions + 507; goto execFuncs;
	f124: _acts = _ECMA48_actions + 510; goto execFuncs;
	f123: _acts = _ECMA48_actions + 513; goto execFuncs;
	f109: _acts = _ECMA48_actions + 516; goto execFuncs;
	f113: _acts = _ECMA48_actions + 519; goto execFuncs;
	f114: _acts = _ECMA48_actions + 522; goto execFuncs;
	f178: _acts = _ECMA48_actions + 525; goto execFuncs;
	f165: _acts = _ECMA48_actions + 528; goto execFuncs;
	f166: _acts = _ECMA48_actions + 531; goto execFuncs;
	f104: _acts = _ECMA48_actions + 534; goto execFuncs;
	f171: _acts = _ECMA48_actions + 537; goto execFuncs;
	f172: _acts = _ECMA48_actions + 540; goto execFuncs;
	f174: _acts = _ECMA48_actions + 543; goto execFuncs;
	f175: _acts = _ECMA48_actions + 546; goto execFuncs;
	f173: _acts = _ECMA48_actions + 549; goto execFuncs;
	f179: _acts = _ECMA48_actions + 552; goto execFuncs;
	f167: _acts = _ECMA48_actions + 555; goto execFuncs;
	f176: _acts = _ECMA48_actions + 558; goto execFuncs;
	f122: _acts = _ECMA48_actions + 561; goto execFuncs;
	f110: _acts = _ECMA48_actions + 564; goto execFuncs;
	f170: _acts = _ECMA48_actions + 567; goto execFuncs;
	f58: _acts = _ECMA48_actions + 570; goto execFuncs;
	f183: _acts = _ECMA48_actions + 573; goto execFuncs;
	f197: _acts = _ECMA48_actions + 576; goto execFuncs;
	f137: _acts = _ECMA48_actions + 580; goto execFuncs;
	f230: _acts = _ECMA48_actions + 584; goto execFuncs;
	f231: _acts = _ECMA48_actions + 588; goto execFuncs;
	f252: _acts = _ECMA48_actions + 592; goto execFuncs;
	f232: _acts = _ECMA48_actions + 596; goto execFuncs;
	f245: _acts = _ECMA48_actions + 600; goto execFuncs;
	f265: _acts = _ECMA48_actions + 604; goto execFuncs;
	f237: _acts = _ECMA48_actions + 608; goto execFuncs;
	f241: _acts = _ECMA48_actions + 612; goto execFuncs;
	f242: _acts = _ECMA48_actions + 616; goto execFuncs;
	f243: _acts = _ECMA48_actions + 620; goto execFuncs;
	f244: _acts = _ECMA48_actions + 624; goto execFuncs;
	f240: _acts = _ECMA48_actions + 628; goto execFuncs;
	f246: _acts = _ECMA48_actions + 632; goto execFuncs;
	f268: _acts = _ECMA48_actions + 636; goto execFuncs;
	f236: _acts = _ECMA48_actions + 640; goto execFuncs;
	f233: _acts = _ECMA48_actions + 644; goto execFuncs;
	f257: _acts = _ECMA48_actions + 648; goto execFuncs;
	f256: _acts = _ECMA48_actions + 652; goto execFuncs;
	f251: _acts = _ECMA48_actions + 656; goto execFuncs;
	f250: _acts = _ECMA48_actions + 660; goto execFuncs;
	f249: _acts = _ECMA48_actions + 664; goto execFuncs;
	f248: _acts = _ECMA48_actions + 668; goto execFuncs;
	f234: _acts = _ECMA48_actions + 672; goto execFuncs;
	f238: _acts = _ECMA48_actions + 676; goto execFuncs;
	f239: _acts = _ECMA48_actions + 680; goto execFuncs;
	f266: _acts = _ECMA48_actions + 684; goto execFuncs;
	f253: _acts = _ECMA48_actions + 688; goto execFuncs;
	f254: _acts = _ECMA48_actions + 692; goto execFuncs;
	f229: _acts = _ECMA48_actions + 696; goto execFuncs;
	f259: _acts = _ECMA48_actions + 700; goto execFuncs;
	f260: _acts = _ECMA48_actions + 704; goto execFuncs;
	f262: _acts = _ECMA48_actions + 708; goto execFuncs;
	f263: _acts = _ECMA48_actions + 712; goto execFuncs;
	f261: _acts = _ECMA48_actions + 716; goto execFuncs;
	f267: _acts = _ECMA48_actions + 720; goto execFuncs;
	f255: _acts = _ECMA48_actions + 724; goto execFuncs;
	f264: _acts = _ECMA48_actions + 728; goto execFuncs;
	f247: _acts = _ECMA48_actions + 732; goto execFuncs;
	f235: _acts = _ECMA48_actions + 736; goto execFuncs;
	f258: _acts = _ECMA48_actions + 740; goto execFuncs;
	f220: _acts = _ECMA48_actions + 744; goto execFuncs;
	f269: _acts = _ECMA48_actions + 748; goto execFuncs;
	f155: _acts = _ECMA48_actions + 752; goto execFuncs;
	f182: _acts = _ECMA48_actions + 756; goto execFuncs;

execFuncs:
	_nacts = *_acts++;
	while ( _nacts-- > 0 ) {
		switch ( *_acts++ ) {
	case 0:
#line 482 "ecma48.rl"
	{
    /* Store sole numeric parameter */
  }
	break;
	case 1:
#line 490 "ecma48.rl"
	{
    // ACK - ACKNOWLEDGE.  If the console receives ACK, it ignores it.
  }
	break;
	case 2:
#line 494 "ecma48.rl"
	{
    // APC - APPLICATION PROGRAM COMMAND.  For now, the console
    // doesn't have any program commands to which is responds.
  }
	break;
	case 3:
#line 499 "ecma48.rl"
	{
    // BEL - BELL.
    console_bell();
  }
	break;
	case 4:
#line 504 "ecma48.rl"
	{
    // BPH - BREAK PERMITTED HERE.  This console doesn't auto-wrap, so
    // this is ignored.
  }
	break;
	case 5:
#line 509 "ecma48.rl"
	{
    // BS - BACKSPACE.  Moves the active data position one character position
    // in the direction opposite of the implicit movement.
    if (simd == 0)
      console_move_left(1);
    else
      console_move_right(1);
  }
	break;
	case 6:
#line 518 "ecma48.rl"
	{
    // CAN - CANCEL.  Undo previous.
  }
	break;
	case 7:
#line 522 "ecma48.rl"
	{
    // CBT - CURSOR BACKWARD TABULATION.
    console_move_tab_left(p1(1));
  }
	break;
	case 8:
#line 527 "ecma48.rl"
	{
    // CCH - CANCEL CHARACTER.  Undo previous byte if it is a graphic
    // character.
  }
	break;
	case 9:
#line 532 "ecma48.rl"
	{
    // CHA - CURSOR CHARACTER ABSOLUTE.
    console_move_to_column(p1(1)-1);	// ECMA-48 coords start at 1.
  }
	break;
	case 10:
#line 537 "ecma48.rl"
	{
    // CHT - CURSOR FORWARD TABULATION
    console_move_tab_right(p1(1));
  }
	break;
	case 11:
#line 542 "ecma48.rl"
	{
    // CNL - CURSOR NEXT LINE
    console_move_to_column(0);
    console_move_down(p1(1));
  }
	break;
	case 12:
#line 548 "ecma48.rl"
	{
    // CPL - CURSOR PRECEDING LINE
    console_move_to_column(0);
    console_move_up(p1(1));
  }
	break;
	case 13:
#line 554 "ecma48.rl"
	{
    // CPR - ACTIVE POSITION REPORT.  The console ignores position
    // reports that it receives the input stream.
  }
	break;
	case 14:
#line 559 "ecma48.rl"
	{
    // CR - CARRIAGE RETURN.
    if (simd == 0)
      console_move_to_column(line_home - 1); // ECMA48 coords begin at 1
    else if (simd == 1)
      console_move_to_column(line_limit - 1); // ECMA48 coords begin at 1
  }
	break;
	case 15:
#line 567 "ecma48.rl"
	{
    // CTC - CURSOR TABULATION CONTROL.  For now, tabs are fixed, so these
    // commands are ignored.
  }
	break;
	case 16:
#line 572 "ecma48.rl"
	{
    // CUB - CURSOR LEFT.
    console_move_left(p1(1));
  }
	break;
	case 17:
#line 577 "ecma48.rl"
	{
    // CUD - CURSOR DOWN.
    console_move_down(p1(1));
  }
	break;
	case 18:
#line 582 "ecma48.rl"
	{
    // CUF - CURSOR RIGHT
    console_move_right(p1(1));
  }
	break;
	case 19:
#line 587 "ecma48.rl"
	{
    // CUP - CURSOR POSITION
    console_move_to(p1(1) - 1, p2(1) - 1); // ECMA-48 coords begin at 1
  }
	break;
	case 20:
#line 592 "ecma48.rl"
	{
    // CUU - CURSOR UP
    console_move_up(p1(1));
  }
	break;
	case 21:
#line 597 "ecma48.rl"
	{
    // CVT - CURSOR LINE TABULATION
    console_move_vertical_tab_down(p1(1));
  }
	break;
	case 22:
#line 602 "ecma48.rl"
	{
    // DA - DEVICE ATTRIBUTES.  The console ignores DA messages from the
    // data stream, and doesn't respond.
  }
	break;
	case 23:
#line 607 "ecma48.rl"
	{
    // DAQ - DEFINE AREA QUALIFICATION.  This is too complicated.
  }
	break;
	case 24:
#line 611 "ecma48.rl"
	{
    // DCH - DELETE CHARACTER.
    if (hem == 0)
      console_delete_right(p1(1));
    else if (hem == 1)
      console_delete_left(p1(1));
  }
	break;
	case 25:
#line 619 "ecma48.rl"
	{
    // DCS - DEVICE CONTROL STRING.  This console doesn't interpret
    // any device control strings.
  }
	break;
	case 26:
#line 624 "ecma48.rl"
	{
    // DC1 - DEVICE CONTROL ONE.  This console doesn't have any 
    // devices to control.
  }
	break;
	case 27:
#line 629 "ecma48.rl"
	{
    // DC2 - DEVICE CONTROL TWO.  This console doesn't have any
    // devices to control.
  }
	break;
	case 28:
#line 634 "ecma48.rl"
	{
    // DC2 - DEVICE CONTROL THREE.  This console doesn't have any
    // devices to control.
  }
	break;
	case 29:
#line 639 "ecma48.rl"
	{
    // DC2 - DEVICE CONTROL FOUR.  This console doesn't have any
    // devices to control.
  }
	break;
	case 30:
#line 644 "ecma48.rl"
	{
    // DL - DELETE LINE.
    if (vem == 0)
      console_delete_line_down(p1(1));
    else if (vem == 1)
      console_delete_line_up(p1(1));
    console_move_to_column(line_home - 1); // ECMA-48 coords begin a 1
  }
	break;
	case 31:
#line 653 "ecma48.rl"
	{
    // DLE - DATA LINK ESCAPE.  In ISO 1745, this would disconnect
    // communication.
  }
	break;
	case 32:
#line 663 "ecma48.rl"
	{
    // DSR - DEVICE STATUS REPORT.  This console doesn't respond to
    // status report requests from the input stream.
  }
	break;
	case 33:
#line 668 "ecma48.rl"
	{
    // DTA - DIMENSION TEXT AREA.  This establishes the dimensions
    // of the text area for the next page.  This console doesn't have
    // pages, so it is unimplemented.
  }
	break;
	case 34:
#line 674 "ecma48.rl"
	{
    // EA - ERASE IN AREA.  The concept of 'areas' is unimplemented.
  }
	break;
	case 35:
#line 678 "ecma48.rl"
	{
    // ECH - ERASE CHARACTER.
    console_erase_right(p1(1));
  }
	break;
	case 36:
#line 683 "ecma48.rl"
	{
    // ED - ERASE IN PAGE.
    int s = p1(0);
    if (s == 1)
      console_erase_to_beginning_of_page();
    else if (s == 0)
      console_erase_to_end_of_page();
    else if (s == 2)
      console_erase_page ();
  }
	break;
	case 37:
#line 694 "ecma48.rl"
	{
    // EF - ERASE IN FIELD.  This console has no notion of fields.
  }
	break;
	case 38:
#line 698 "ecma48.rl"
	{
    // EL - ERASE IN LINE
    int s = p1(0);
    if (s == 1)
      console_erase_to_beginning_of_line();
    else if (s == 0)
      console_erase_to_end_of_line();
    else if (s == 2)
      console_erase_line ();
  }
	break;
	case 39:
#line 709 "ecma48.rl"
	{
    // EM - END OF MEDIUM.  This console has no physical end, so this
    // is ignored.
  }
	break;
	case 40:
#line 723 "ecma48.rl"
	{
    // EPA - END OF GUARDED AREA.  This console doesn't implement areas.
  }
	break;
	case 41:
#line 731 "ecma48.rl"
	{
    // FF - FORM FEED.  This is traditionally used as reset screen for
    // consoles.
    console_erase_page();
    console_move_to_row(page_home - 1);	// ECMA-48 coordinates begin at 1
  }
	break;
	case 42:
#line 738 "ecma48.rl"
	{
    // FNK - FUNCTION KEY.  Sent by the input stream when function keys
    // are pressed.
  }
	break;
	case 43:
#line 743 "ecma48.rl"
	{
    // FNT - FONT SELECTION.  This is supposed to map a font number into
    // on of the 10 font slots available by SGR calls, but, this console
    // doesn't have remappable fonts.
  }
	break;
	case 44:
#line 749 "ecma48.rl"
	{
    // GCC - GRAPHIC CHARACTER COMBINATION.  Overstriking characters would
    // be sweet, but, this isn't implemented.
  }
	break;
	case 45:
#line 754 "ecma48.rl"
	{
    // GSM - GRAPHIC SIZE MODIFICATION.  This would scale the font height
    // or width.
  }
	break;
	case 46:
#line 763 "ecma48.rl"
	{
    // HPA - CHARACTER POSITION ABSOLUTE.
    console_move_to_column(p1(1)- 1); 	// ECMA-48 coordinates begin at 1
  }
	break;
	case 47:
#line 768 "ecma48.rl"
	{
    // HPB - CHARACTER POSITION BACKWARD.
    console_move_left(p1(1));
  }
	break;
	case 48:
#line 773 "ecma48.rl"
	{
    // HPR - CHARACTER POSITION FORWARD.
    console_move_right(p1(1));
  }
	break;
	case 49:
#line 778 "ecma48.rl"
	{
    // HT - CHARACTER TABULATION.  This is a single tab.
    console_move_tab_right(p1(1));
  }
	break;
	case 50:
#line 783 "ecma48.rl"
	{
    // HTJ - CHARACTER TABULATION WITH JUSTIFICATION. Unimplemented.
  }
	break;
	case 51:
#line 787 "ecma48.rl"
	{
    // HTS - CHARACTER TABULATION SET.  Unimplemented.
  }
	break;
	case 52:
#line 796 "ecma48.rl"
	{
    // ICH - INSERT CHARACTER.
    if (hem == 0) {
      console_insert_right(p1(1));
      console_move_to_column(line_home - 1); // ECMA-48 coords begin at 1
    }
    else if (hem == 1) {
      console_insert_left(p1(1));
      console_move_to_column(line_home - 1);
    }
  }
	break;
	case 53:
#line 812 "ecma48.rl"
	{
    // IGS - IDENTIFY GRAPHIC SUBREPERTOIRE.  Used to indicate that
    // a repertoire of ISO 10367 is used in the following text.
  }
	break;
	case 54:
#line 817 "ecma48.rl"
	{
    // IL - INSERT LINE.
    if (vem == 0)
      console_insert_line_down(p1(1));
    else if (vem == 1)
      console_insert_line_up(p1(1));
    console_move_to_column(line_home - 1); // ECMA-48 coordinates begin at 1
  }
	break;
	case 55:
#line 826 "ecma48.rl"
	{
    // INT - INTERRUPT. Data stream indicates to console that the current
    // process is to be interrupted and some agreed procedure is to be
    // initiated.
  }
	break;
	case 56:
#line 832 "ecma48.rl"
	{
    // IS1 - INFORMATION SEPARATOR ONE (UNIT SEPARATOR)
  }
	break;
	case 57:
#line 836 "ecma48.rl"
	{
    // IS2 - INFORMATION SEPARATOR TWO (RECORD SEPARATOR)
  }
	break;
	case 58:
#line 840 "ecma48.rl"
	{
    // IS3 - INFORMATION SEPARATOR THREE (GROUP SEPARATOR)
  }
	break;
	case 59:
#line 844 "ecma48.rl"
	{
    // IS4 - INFORMATION SEPARATOR FOUR (FILE SEPARATOR)
  }
	break;
	case 60:
#line 848 "ecma48.rl"
	{
    // JFY - JUSTIFY. Indicates the beginning or end of a string that
    // was to be justified on the screen.  Cool, but, unimplemented.
  }
	break;
	case 61:
#line 853 "ecma48.rl"
	{
    // LF - LINE FEED.
    console_move_down(1);
  }
	break;
	case 62:
#line 858 "ecma48.rl"
	{
    // LS0 - LOCKING-SHIFT ZERO
    // term_locking_shift (0);
    GL = G0;
  }
	break;
	case 63:
#line 864 "ecma48.rl"
	{
    // LS1 - LOCKING-SHIFT ONE
    // term_locking_shift (1);
    GL = G1;
  }
	break;
	case 64:
#line 870 "ecma48.rl"
	{
    // LS1R - LOCKING-SHIFT ONE RIGHT
    // term_locking_shift_right (1);
    GR = G1;
  }
	break;
	case 65:
#line 882 "ecma48.rl"
	{
    // LS2R - LOCKING-SHIFT TWO RIGHT
    // term_locking_shift_right (2);
    GR = G2;
  }
	break;
	case 66:
#line 888 "ecma48.rl"
	{
    // LS3 - LOCKING-SHIFT THREE
    // term_locking_shift (3);
    GL = G3;
  }
	break;
	case 67:
#line 894 "ecma48.rl"
	{
    // LS3R - LOCKING-SHIFT THREE RIGHT
    // term_locking_shift_right (3);
    GR = G3;
  }
	break;
	case 68:
#line 900 "ecma48.rl"
	{
    // MC - MEDIA COPY.  Unimplemented, because there is no
    // auxiliary devoce to which one would copy.
  }
	break;
	case 69:
#line 905 "ecma48.rl"
	{
    // MW - MESSAGE WAITING. Used to set a message-waiting indicator
    // on the console.
  }
	break;
	case 70:
#line 916 "ecma48.rl"
	{
    // NBH - NO BREAK HERE.  Clever breaking is not implemented.
  }
	break;
	case 71:
#line 920 "ecma48.rl"
	{
    // NEL - NEXT LINE.
    if (simd == 0) {
      console_move_down(1);
      console_move_to_column(line_home - 1);
    }
    else if (simd == 1) {
      console_move_down(1);
      console_move_to_column(line_limit - 1);
    }
  }
	break;
	case 72:
#line 932 "ecma48.rl"
	{
    // NP - NEXT PAGE.  Unimplemented since the console has a single
    // page.
  }
	break;
	case 73:
#line 937 "ecma48.rl"
	{
    // NUL - NULL.
  }
	break;
	case 74:
#line 941 "ecma48.rl"
	{
    // OSC - OPERATING SYSTEM COMMAND.  The data stream sends a string
    // that is denoted as an operating system command.  This console
    // doesn't interpret it.
  }
	break;
	case 75:
#line 947 "ecma48.rl"
	{
    // PEC - PRESENTATION EXPAND OR CONTRACT.  Unimplemented since
    // this console doesn't have variable spacing.
  }
	break;
	case 76:
#line 952 "ecma48.rl"
	{
    // PFS - PAGE FORMAT SELECTION.  Unimplemented since this console's
    // page size can't be modified by the data stream.
  }
	break;
	case 77:
#line 957 "ecma48.rl"
	{
    // PLD - PARTIAL LINE DOWN.  Unimplemented.  This would move the
    // cursor down by a half line.
  }
	break;
	case 78:
#line 962 "ecma48.rl"
	{
    // PLU - PARTIAL LINE UP.  Unimplemented.  This would move the cursor
    // up by a half line.
  }
	break;
	case 79:
#line 967 "ecma48.rl"
	{
    // PM - PRIVACY MESSAGE.  The data stream sends a string to the
    // console denoted as a privacy message.  The console ignores it.
  }
	break;
	case 80:
#line 972 "ecma48.rl"
	{
    // PP - PRECEDING PAGE.  Unimplemented since this console doesn't
    // have pages.
  }
	break;
	case 81:
#line 977 "ecma48.rl"
	{
    // PPA - PAGE POSITION ABSOLUTE.  Would move the active position
    // to a given page.  Unimplemented, since this console has only one page.
  }
	break;
	case 82:
#line 982 "ecma48.rl"
	{
    // PPB - PAGE POSITION BACKWARD.  Would move the active position
    // to a previous page.  Unimplemented, since this console has only
    // one page.
  }
	break;
	case 83:
#line 988 "ecma48.rl"
	{
    // PPB - PAGE POSITION FORWARD.  Would move the active position
    // to a following page.  Unimplemented, since this console has only
    // one page.
  }
	break;
	case 84:
#line 994 "ecma48.rl"
	{
    // PTX - PARALLEL TEXTS.  Denotes that the next couple of strings
    // are supposed to be drawn one over the other.  Unimplemented because
    // it is too complicated.
  }
	break;
	case 85:
#line 1000 "ecma48.rl"
	{
    // PU1 - PRIVATE USE ONE. Unimplemented.
  }
	break;
	case 86:
#line 1004 "ecma48.rl"
	{
    // PU1 - PRIVATE USE TWO. Unimplemented.
  }
	break;
	case 87:
#line 1008 "ecma48.rl"
	{
    // QUAD.  Would center or flush the previous line or string.
    // Unimplemented because it is too complicated.
  }
	break;
	case 88:
#line 1013 "ecma48.rl"
	{
    // REP - REPEAT.  Would repeat the preceding graphic character
    // multiple times.  Unimplemented.
  }
	break;
	case 89:
#line 1018 "ecma48.rl"
	{
    // RI - REVERSE LINE FEED.
    console_move_up(1);
  }
	break;
	case 90:
#line 1023 "ecma48.rl"
	{
    // RIS - RESET TO INITIAL STATE.
    console_reset();
  }
	break;
	case 91:
#line 1028 "ecma48.rl"
	{
    // RM - RESET MODE
    for (int i = 0; i < P_size; i ++) {
      if (P[i] == 4)
	irm = IRM_REPLACE;
      else if (P[i] == 7)
	vem = 0;
      else if (P[i] == 10)
	hem = 0;
      else if (P[i] == 21)
	grcm = GRCM_COMBINING;
    }
  }
	break;
	case 92:
#line 1042 "ecma48.rl"
	{
    // SACS - SET ADDITIONAL CHARACTER SEPARATION.  Would change the
    // spacing between characters on the console.  Unimplemented.
  }
	break;
	case 93:
#line 1052 "ecma48.rl"
	{
    // SCI - SINGLE CHARACTER INTRODUCER.  Not a command, but, a
    // family of possible future commands.  Unimplemented.
  }
	break;
	case 94:
#line 1057 "ecma48.rl"
	{
    // SCO - SELECT CHARACTER ORIENTATION.  Would allow one to choose
    // the rotation of the characters.  Sounds like fun.  Unimplemented.
  }
	break;
	case 95:
#line 1062 "ecma48.rl"
	{
    // SCP - SELECT CHARACTER PATH.  Would allow changing the character
    // path from right-to-left, top-to-bottom, or left-to-right.
    // Unimplemented.
  }
	break;
	case 96:
#line 1068 "ecma48.rl"
	{
    // SCS - SET CHARACTER SPACING.  Would allow modification of the
    // character spacing.  Unimplemented.
  }
	break;
	case 97:
#line 1073 "ecma48.rl"
	{
    // SD - SCROLL DOWN.
    console_scroll_down(p1(1));
  }
	break;
	case 98:
#line 1078 "ecma48.rl"
	{
    // SDS - START DIRECTED STRING. Would allow one to label the 
    // string as right-to-left or left-to-right.  Unimplemented.
  }
	break;
	case 99:
#line 1083 "ecma48.rl"
	{
    // SEE - SET EDITING EXTENT.  Would allow one to establish
    // a portion of the console as the extent that is modified
    // by the character insertion or deletion operations.
    // Possibly useful.  Unimplemented.
  }
	break;
	case 100:
#line 1090 "ecma48.rl"
	{
    // SEF - SHEET EJECT AND FEED.  Unimplemented.
  }
	break;
	case 101:
#line 1094 "ecma48.rl"
	{
    // SGR - SELECT GRAPHIC RENDITION
    //reparse_P ();
    if (grcm == GRCM_REPLACING)
      console_set_default();
    for (int i = 0; i < P_size; i ++) {
      switch (P[i]) {
	case 0:
	  console_set_default();
	  break;
	case 1:
	  console_set_intensity(INTENSITY_BOLD);
	  break;
	case 2:
	  console_set_intensity(INTENSITY_FAINT);
	  break;
	case 3:
	  // Italicized
	  break;
	case 4:
	  console_set_underline(UNDERLINE_SINGLY);
	  break;
	case 5:
	  console_set_blink(BLINK_SLOW);
	  break;
	case 6:
	  console_set_blink(BLINK_FAST);
	  break;
	case 7:
	  console_set_polarity(POLARITY_NEGATIVE);
	  break;
	case 8:
	  // Concealed.
	  break;
	case 9:
	  // Strike-through
	  break;
	case 10:
	case 11:
	case 12:
	case 13:
	case 14:
	case 15:
	case 16:
	case 17:
	case 18:
	case 19:
	  // Alternative fonts
	  break;
	case 20:
	  // Fraktur
	  break;
	case 21:
	  console_set_underline(UNDERLINE_DOUBLY);
	  break;
	case 22:
	  console_set_intensity(INTENSITY_NORMAL);
	  console_set_fgcolor(COLOR_FG_DEFAULT);
	  console_set_bgcolor(COLOR_BG_DEFAULT);
	  break;
	case 23:
	  // Turn off italicization and fraktur.
	  break;
	case 24:
	  console_set_underline(UNDERLINE_NONE);
	  break;
	case 25:
	  console_set_blink (BLINK_NONE);
	  break;
	case 26:
	  // Reserved
	  break;
	case 27:
	  console_set_polarity(POLARITY_POSITIVE);
	  break;
	case 28:
	  // Turn off concealed
	  break;
	case 29:
	  // Turn off strike-through
	  break;
	case 30:
	  console_set_fgcolor(COLOR_FG_BLACK);
	  break;
	case 31:
	  console_set_fgcolor(COLOR_FG_RED);
	  break;
	case 32:
	  console_set_fgcolor(COLOR_FG_GREEN);
	  break;
	case 33:
	  console_set_fgcolor(COLOR_FG_YELLOW);
	  break;
	case 34:
	  console_set_fgcolor(COLOR_FG_BLUE);
	  break;
	case 35:
	  console_set_fgcolor(COLOR_FG_MAGENTA);
	  break;
	case 36:
	  console_set_fgcolor(COLOR_FG_CYAN);
	  break;
	case 37:
	  console_set_fgcolor(COLOR_FG_WHITE);
	  break;
	case 38:
	  console_set_fgcolor(COLOR_FG_TRANSPARENT);
	  break;
	case 39:
	  // white is default
	  console_set_fgcolor(COLOR_FG_DEFAULT);
	  break;
	case 40:
	  console_set_bgcolor(COLOR_BG_BLACK);
	  break;
	case 41:
	  console_set_bgcolor(COLOR_BG_RED);
	  break;
	case 42:
	  console_set_bgcolor(COLOR_BG_GREEN);
	  break;
	case 43:
	  console_set_bgcolor(COLOR_BG_YELLOW);
	  break;
	case 44:
	  console_set_bgcolor(COLOR_BG_BLUE);
	  break;
	case 45:
	  console_set_bgcolor(COLOR_BG_MAGENTA);
	  break;
	case 46:
	  console_set_bgcolor(COLOR_BG_CYAN);
	  break;
	case 47:
	  console_set_bgcolor(COLOR_BG_WHITE);
	  break;
	case 48:
	  console_set_bgcolor(COLOR_BG_TRANSPARENT);
	  break;
	case 49:
	  // Default BG color
	  console_set_bgcolor(COLOR_BG_DEFAULT);
	  break;
	case 50:
	  // Reserved
	  break;
	case 51:
	case 52:
	case 53:
	case 54:
	case 55:
	  // Framing, encircline, and overlining
	  break;
	case 56:
	case 57:
	case 58:
	case 59:
	  // Unspecified
	case 60:
	case 61:
	case 62:
	case 63:
	case 64:
	case 65:
	  // Ideogram decorations
	default:
	  break;
	}
    }
  }
	break;
	case 102:
#line 1270 "ecma48.rl"
	{
    // SIMD - SELECT IMPLICIT MOVEMENT DIRECTION. Necessary for right-to-left
    // languages.
    simd = p1(0);
  }
	break;
	case 103:
#line 1276 "ecma48.rl"
	{
    // SL - SCROLL LEFT.
    console_scroll_left (p1(1));
  }
	break;
	case 104:
#line 1281 "ecma48.rl"
	{
    // SLH - SET LINE HOME. Determines the location of the cursor after,
    // carriage return, next line, insert line or delete line.
    line_home = p1(1);
  }
	break;
	case 105:
#line 1287 "ecma48.rl"
	{
    // SLL - SET LINE LIMIT
    line_limit = p1(CONSOLE_COLS);
  }
	break;
	case 106:
#line 1292 "ecma48.rl"
	{
    // SLS - SET LINE SPACING. Determine line spacing for following text.
    // Unimplemented.
  }
	break;
	case 107:
#line 1297 "ecma48.rl"
	{
    // SM - SET MODE.  Flip the boolean mode to the 
    // non-nominal state.
    for (int i = 0; i < P_size; i ++) {
      if (P[i] == 4)
	irm = IRM_INSERT;
      else if (P[i] == 7)
	vem = 1;
      else if (P[i] == 10)
	hem = 1;
      else if (P[i] == 21)
	grcm = GRCM_REPLACING;
    }
  }
	break;
	case 108:
#line 1312 "ecma48.rl"
	{
    // SOS - START OF STRING.  Allows the data stream to send a string
    // to the console.  This console doesn't interpret them.
  }
	break;
	case 109:
#line 1317 "ecma48.rl"
	{
    // SPA - START OF GUARDED AREA. Guarding is not implemented.
  }
	break;
	case 110:
#line 1321 "ecma48.rl"
	{
    // SPD - SELECT PRESENTATION DIRECTIONS.  Directionality is
    // unimplemented.
  }
	break;
	case 111:
#line 1326 "ecma48.rl"
	{
    // SPH - SET PAGE HOME.  Sets the line to which the active position
    // is set after a FORM FEED.
    page_home = p1(1);
  }
	break;
	case 112:
#line 1332 "ecma48.rl"
	{
    // SPI - SPACING INCREMENT.  Used to set the line and character
    // spacing.  Unimplemented.
  }
	break;
	case 113:
#line 1337 "ecma48.rl"
	{
    // SPL - SET PAGE LIMIT.  Sets the line on the page beyond which
    // the active position may not be moved.  Unimplemented, because
    // it is fixed for this console.
  }
	break;
	case 114:
#line 1343 "ecma48.rl"
	{
    // SPQR - SELECT PRINT QUALITY AND RAPIDITY.  Would let one
    // choose the DPI of the fonts.  Unimplemented.
  }
	break;
	case 115:
#line 1348 "ecma48.rl"
	{
    // SR _SCROLL RIGHT.
    console_scroll_right (p1(1));
  }
	break;
	case 116:
#line 1353 "ecma48.rl"
	{
    // SRCS - SET REDUCED CHARACTER SEPARATOR. Would reduce in
    // inter-character spacing.  Unimplemented.
  }
	break;
	case 117:
#line 1358 "ecma48.rl"
	{
    // SRS - START REVERSED STRING.  Would allow one to designate some
    // text to have the opposite directionality.  Unimplemented.
  }
	break;
	case 118:
#line 1363 "ecma48.rl"
	{
    // SSA - START OF SELECTED AREA.  Areas are unimplemented.
  }
	break;
	case 119:
#line 1367 "ecma48.rl"
	{
    // SSU - SELECT SIZE UNIT. Would allow one to establish the units used
    // in the various size-setting functions.  Unimplemented.
  }
	break;
	case 120:
#line 1372 "ecma48.rl"
	{
    // SSW - SET SPACE WIDTH.  Would allow one to set the width of a space.
    // Unimplemented.
  }
	break;
	case 121:
#line 1377 "ecma48.rl"
	{
    // SS2 - SINGLE-SHIFT TWO.  The next graphical character comes from
    // character set G2.
    // term_single_shift (2);
    console_write_char (codepoint_tables[G2][*p - 0x20], irm, hem, simd,
		  line_home - 1, line_limit - 1);
  }
	break;
	case 122:
#line 1385 "ecma48.rl"
	{
    // SS3 - SINGLE-SHIFT THREE.  The next graphical character will come
    // from the character set G3.
    // term_single_shift (3);
    console_write_char (codepoint_tables[G3][*p - 0x20], irm, hem, simd,
		  line_home - 1, line_limit - 1);
  }
	break;
	case 123:
#line 1393 "ecma48.rl"
	{
    // STAB - SELECTIVE TABULATION.  Something to do with jumping to
    // a specified tab.  Unimplemented.
  }
	break;
	case 124:
#line 1398 "ecma48.rl"
	{
    // STS - SET TRANSMIT STATE.  Used to set the transmit state of the
    // console.  Unimplemented.
  }
	break;
	case 125:
#line 1403 "ecma48.rl"
	{
    // SU - SCROLL UP.
    console_scroll_up(p1(1));
  }
	break;
	case 126:
#line 1408 "ecma48.rl"
	{
    // SUB - SUBSTITUTE.  If the data stream sent a SUB, it would be
    // indicating that it is a replacement for an invalid or erroneous
    // byte.  Ignored.
  }
	break;
	case 127:
#line 1414 "ecma48.rl"
	{
    // SVS - SELECT LINE SPACING.  Would set the lines-per-inch for the
    // following text.  Unimplemented.
  }
	break;
	case 128:
#line 1424 "ecma48.rl"
	{
    // TAC - TABULATION ALIGNED CENTRED.  Would allow one to center a field
    // on a tab stop.  Unimplemented.
  }
	break;
	case 129:
#line 1429 "ecma48.rl"
	{
    // TAC - TABULATION ALIGNED LEADING EDGE.  Would allow one to
    // center a field on a tab stop.  Unimplemented.
  }
	break;
	case 130:
#line 1434 "ecma48.rl"
	{
    // TAC - TABULATION ALIGNED TRAILING EDGE.  Would allow one to
    // center a field on a tab stop.  Unimplemented.
  }
	break;
	case 131:
#line 1439 "ecma48.rl"
	{
    // TBC - TABULATION CLEAR. Woudl allow one to clear tabstops.
    // Unimplemented.
  }
	break;
	case 132:
#line 1444 "ecma48.rl"
	{
    // TCC - TABULATION CENTRED ON CHARACTER.  Another fancy tab stop.
    // Unimplemented.
  }
	break;
	case 133:
#line 1449 "ecma48.rl"
	{
    // TSR - TABULATION STOP REMOVE.  Remove the tab stop at the current
    // location.  Unimplemented.
  }
	break;
	case 134:
#line 1454 "ecma48.rl"
	{
    // TSS - THIN SPACE SPECIFICATION.  Thin spaces are unimplemented.
  }
	break;
	case 135:
#line 1458 "ecma48.rl"
	{
    // VPA - LINE POSITION ABSOLUTE.
    console_move_to_row(p1(1));
  }
	break;
	case 136:
#line 1463 "ecma48.rl"
	{
    // VPB - LINE POSITION BACKWARD
    console_move_up(p1(1));
  }
	break;
	case 137:
#line 1468 "ecma48.rl"
	{
    // VPR - LINE POSITION FORWARD
    console_move_down(p1(1));
  }
	break;
	case 138:
#line 1473 "ecma48.rl"
	{
    // VT - LINE TABULATION
    console_move_vertical_tab_down(1);
  }
	break;
	case 139:
#line 1478 "ecma48.rl"
	{
    // VTS - LINE TABULATION SET.  Would set the vertical tab stops.
    // Unimplemented.
  }
	break;
	case 140:
#line 1483 "ecma48.rl"
	{
    int n = *p;
    if (n >= 0x20 && n <= 0x7F)
      console_write_char (codepoint_tables[GL][n - 0x20], irm, hem, simd,
		       line_home - 1, line_limit - 1);
    else if (n >= 0xA0 && n <= 0xFF)
      console_write_char (codepoint_tables[GR][n - 0xA0], irm, hem, simd,
		       line_home - 1, line_limit - 1);
  }
	break;
	case 141:
#line 1497 "ecma48.rl"
	{
  //   unsigned char *pos = fpc;
  //   int i, n;
  //   P_size = 0;
    
  // loop:
  //   n = 0;
  //   i = 1;
  //   while (*pos >= '0' && *pos <= '9') {
  //     n += (*pos - '0') * i;
  //     i *= 10;
  //     pos ++;
  //   }
  //   P[P_size] = n;
  //   P_size ++;
  //   SDL_assert(P_size < PARAMS);
  //   if (*pos == ';')
  //     goto loop;
  //   fpc = pos;
  }
	break;
#line 4749 "ecma48.cpp"
		}
	}
	goto _again;

_again:
	if ( cs == 0 )
		goto _out;
	if ( ++p != pe )
		goto _resume;
	_test_eof: {}
	if ( p == eof )
	{
	const unsigned char *__acts = _ECMA48_actions + _ECMA48_eof_actions[cs];
	unsigned int __nacts = (unsigned int) *__acts++;
	while ( __nacts-- > 0 ) {
		switch ( *__acts++ ) {
	case 101:
#line 1094 "ecma48.rl"
	{
    // SGR - SELECT GRAPHIC RENDITION
    //reparse_P ();
    if (grcm == GRCM_REPLACING)
      console_set_default();
    for (int i = 0; i < P_size; i ++) {
      switch (P[i]) {
	case 0:
	  console_set_default();
	  break;
	case 1:
	  console_set_intensity(INTENSITY_BOLD);
	  break;
	case 2:
	  console_set_intensity(INTENSITY_FAINT);
	  break;
	case 3:
	  // Italicized
	  break;
	case 4:
	  console_set_underline(UNDERLINE_SINGLY);
	  break;
	case 5:
	  console_set_blink(BLINK_SLOW);
	  break;
	case 6:
	  console_set_blink(BLINK_FAST);
	  break;
	case 7:
	  console_set_polarity(POLARITY_NEGATIVE);
	  break;
	case 8:
	  // Concealed.
	  break;
	case 9:
	  // Strike-through
	  break;
	case 10:
	case 11:
	case 12:
	case 13:
	case 14:
	case 15:
	case 16:
	case 17:
	case 18:
	case 19:
	  // Alternative fonts
	  break;
	case 20:
	  // Fraktur
	  break;
	case 21:
	  console_set_underline(UNDERLINE_DOUBLY);
	  break;
	case 22:
	  console_set_intensity(INTENSITY_NORMAL);
	  console_set_fgcolor(COLOR_FG_DEFAULT);
	  console_set_bgcolor(COLOR_BG_DEFAULT);
	  break;
	case 23:
	  // Turn off italicization and fraktur.
	  break;
	case 24:
	  console_set_underline(UNDERLINE_NONE);
	  break;
	case 25:
	  console_set_blink (BLINK_NONE);
	  break;
	case 26:
	  // Reserved
	  break;
	case 27:
	  console_set_polarity(POLARITY_POSITIVE);
	  break;
	case 28:
	  // Turn off concealed
	  break;
	case 29:
	  // Turn off strike-through
	  break;
	case 30:
	  console_set_fgcolor(COLOR_FG_BLACK);
	  break;
	case 31:
	  console_set_fgcolor(COLOR_FG_RED);
	  break;
	case 32:
	  console_set_fgcolor(COLOR_FG_GREEN);
	  break;
	case 33:
	  console_set_fgcolor(COLOR_FG_YELLOW);
	  break;
	case 34:
	  console_set_fgcolor(COLOR_FG_BLUE);
	  break;
	case 35:
	  console_set_fgcolor(COLOR_FG_MAGENTA);
	  break;
	case 36:
	  console_set_fgcolor(COLOR_FG_CYAN);
	  break;
	case 37:
	  console_set_fgcolor(COLOR_FG_WHITE);
	  break;
	case 38:
	  console_set_fgcolor(COLOR_FG_TRANSPARENT);
	  break;
	case 39:
	  // white is default
	  console_set_fgcolor(COLOR_FG_DEFAULT);
	  break;
	case 40:
	  console_set_bgcolor(COLOR_BG_BLACK);
	  break;
	case 41:
	  console_set_bgcolor(COLOR_BG_RED);
	  break;
	case 42:
	  console_set_bgcolor(COLOR_BG_GREEN);
	  break;
	case 43:
	  console_set_bgcolor(COLOR_BG_YELLOW);
	  break;
	case 44:
	  console_set_bgcolor(COLOR_BG_BLUE);
	  break;
	case 45:
	  console_set_bgcolor(COLOR_BG_MAGENTA);
	  break;
	case 46:
	  console_set_bgcolor(COLOR_BG_CYAN);
	  break;
	case 47:
	  console_set_bgcolor(COLOR_BG_WHITE);
	  break;
	case 48:
	  console_set_bgcolor(COLOR_BG_TRANSPARENT);
	  break;
	case 49:
	  // Default BG color
	  console_set_bgcolor(COLOR_BG_DEFAULT);
	  break;
	case 50:
	  // Reserved
	  break;
	case 51:
	case 52:
	case 53:
	case 54:
	case 55:
	  // Framing, encircline, and overlining
	  break;
	case 56:
	case 57:
	case 58:
	case 59:
	  // Unspecified
	case 60:
	case 61:
	case 62:
	case 63:
	case 64:
	case 65:
	  // Ideogram decorations
	default:
	  break;
	}
    }
  }
	break;
	case 140:
#line 1483 "ecma48.rl"
	{
    int n = *p;
    if (n >= 0x20 && n <= 0x7F)
      console_write_char (codepoint_tables[GL][n - 0x20], irm, hem, simd,
		       line_home - 1, line_limit - 1);
    else if (n >= 0xA0 && n <= 0xFF)
      console_write_char (codepoint_tables[GR][n - 0xA0], irm, hem, simd,
		       line_home - 1, line_limit - 1);
  }
	break;
#line 4951 "ecma48.cpp"
		}
	}
	}

	_out: {}
	}

#line 1763 "ecma48.rl"

    if ( cs == ECMA48_error )
        return -1;
    if ( cs >= ECMA48_first_final )
        return 1;
    return 0;
}


