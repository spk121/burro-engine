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
// #include <SDL.h>
#include "eng.h"
#include "console.h"

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



%%{
  machine ECMA48;
  
  alphtype unsigned char;
  action reparse_P {
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

  
  action store_param {
    /* Store sole numeric parameter */
  }
  
  action store_string {
    /* Store control string parameter */
  }
  
  action do_ack {
    // ACK - ACKNOWLEDGE.  If the console receives ACK, it ignores it.
  }

  action do_apc {
    // APC - APPLICATION PROGRAM COMMAND.  For now, the console
    // doesn't have any program commands to which is responds.
  }

  action do_bel {
    // BEL - BELL.
    console_bell();
  }
  
  action do_bph {
    // BPH - BREAK PERMITTED HERE.  This console doesn't auto-wrap, so
    // this is ignored.
  }

  action do_bs {
    // BS - BACKSPACE.  Moves the active data position one character position
    // in the direction opposite of the implicit movement.
    if (simd == 0)
      console_move_left(1);
    else
      console_move_right(1);
  }

  action do_can {
    // CAN - CANCEL.  Undo previous.
  }

  action do_cbt {
    // CBT - CURSOR BACKWARD TABULATION.
    console_move_tab_left(p1(1));
  }

  action do_cch {
    // CCH - CANCEL CHARACTER.  Undo previous byte if it is a graphic
    // character.
  }

  action do_cha {
    // CHA - CURSOR CHARACTER ABSOLUTE.
    console_move_to_column(p1(1)-1);	// ECMA-48 coords start at 1.
  }

  action do_cht {
    // CHT - CURSOR FORWARD TABULATION
    console_move_tab_right(p1(1));
  }

  action do_cnl {
    // CNL - CURSOR NEXT LINE
    console_move_to_column(0);
    console_move_down(p1(1));
  }

  action do_cpl {
    // CPL - CURSOR PRECEDING LINE
    console_move_to_column(0);
    console_move_up(p1(1));
  }

  action do_cpr {
    // CPR - ACTIVE POSITION REPORT.  The console ignores position
    // reports that it receives the input stream.
  }

  action do_cr {
    // CR - CARRIAGE RETURN.
    if (simd == 0)
      console_move_to_column(line_home - 1); // ECMA48 coords begin at 1
    else if (simd == 1)
      console_move_to_column(line_limit - 1); // ECMA48 coords begin at 1
  }

  action do_ctc {
    // CTC - CURSOR TABULATION CONTROL.  For now, tabs are fixed, so these
    // commands are ignored.
  }

  action do_cub {
    // CUB - CURSOR LEFT.
    console_move_left(p1(1));
  }

  action do_cud {
    // CUD - CURSOR DOWN.
    console_move_down(p1(1));
  }

  action do_cuf {
    // CUF - CURSOR RIGHT
    console_move_right(p1(1));
  }
  
  action do_cup {
    // CUP - CURSOR POSITION
    console_move_to(p1(1) - 1, p2(1) - 1); // ECMA-48 coords begin at 1
  }

  action do_cuu {
    // CUU - CURSOR UP
    console_move_up(p1(1));
  }

  action do_cvt {
    // CVT - CURSOR LINE TABULATION
    console_move_vertical_tab_down(p1(1));
  }

  action do_da {
    // DA - DEVICE ATTRIBUTES.  The console ignores DA messages from the
    // data stream, and doesn't respond.
  }

  action do_daq {
    // DAQ - DEFINE AREA QUALIFICATION.  This is too complicated.
  }

  action do_dch {
    // DCH - DELETE CHARACTER.
    if (hem == HEM_FOLLOWING)
      console_delete_right(p1(1));
    else if (hem == HEM_PRECEDING)
      console_delete_left(p1(1));
  }
    
  action do_dcs {
    // DCS - DEVICE CONTROL STRING.  This console doesn't interpret
    // any device control strings.
  }

  action do_dc1 {
    // DC1 - DEVICE CONTROL ONE.  This console doesn't have any 
    // devices to control.
  }

  action do_dc2 {
    // DC2 - DEVICE CONTROL TWO.  This console doesn't have any
    // devices to control.
  }

  action do_dc3 {
    // DC2 - DEVICE CONTROL THREE.  This console doesn't have any
    // devices to control.
  }

  action do_dc4 {
    // DC2 - DEVICE CONTROL FOUR.  This console doesn't have any
    // devices to control.
  }

  action do_dl {
    // DL - DELETE LINE.
    if (vem == 0)
      console_delete_line_down(p1(1));
    else if (vem == 1)
      console_delete_line_up(p1(1));
    console_move_to_column(line_home - 1); // ECMA-48 coords begin a 1
  }
  
  action do_dle {
    // DLE - DATA LINK ESCAPE.  In ISO 1745, this would disconnect
    // communication.
  }

  action do_dmi {
    // DMI - DISABLE MANUAL INPUT.  This might be useful, but, it is
    // unimplemented.
  }

  action do_dsr {
    // DSR - DEVICE STATUS REPORT.  This console doesn't respond to
    // status report requests from the input stream.
  }

  action do_dta {
    // DTA - DIMENSION TEXT AREA.  This establishes the dimensions
    // of the text area for the next page.  This console doesn't have
    // pages, so it is unimplemented.
  }

  action do_ea {
    // EA - ERASE IN AREA.  The concept of 'areas' is unimplemented.
  }
  
  action do_ech {
    // ECH - ERASE CHARACTER.
    console_erase_right(p1(1));
  }

  action do_ed {
    // ED - ERASE IN PAGE.
    int s = p1(0);
    if (s == 1)
      console_erase_to_beginning_of_page();
    else if (s == 0)
      console_erase_to_end_of_page();
    else if (s == 2)
      console_erase_page ();
  }
  
  action do_ef {
    // EF - ERASE IN FIELD.  This console has no notion of fields.
  }

  action do_el {
    // EL - ERASE IN LINE
    int s = p1(0);
    if (s == 1)
      console_erase_to_beginning_of_line();
    else if (s == 0)
      console_erase_to_end_of_line();
    else if (s == 2)
      console_erase_line ();
  }

  action do_em {
    // EM - END OF MEDIUM.  This console has no physical end, so this
    // is ignored.
  }

  action do_emi {
    // EMI - ENABLE MANUAL INPUT. Unimplemented, for now.
  }

  action do_enq {
    // ENQ - ENQUIRY.  This this console doesn't respond, it ignores this
    // request.
  }

  action do_epa {
    // EPA - END OF GUARDED AREA.  This console doesn't implement areas.
  }

  action do_esa {
    // ESA - END OF SELECTED AREA.
  }

  action do_ff {
    // FF - FORM FEED.  This is traditionally used as reset screen for
    // consoles.
    console_erase_page();
    console_move_to_row(page_home - 1);	// ECMA-48 coordinates begin at 1
  }

  action do_fnk {
    // FNK - FUNCTION KEY.  Sent by the input stream when function keys
    // are pressed.
  }

  action do_fnt {
    // FNT - FONT SELECTION.  This is supposed to map a font number into
    // on of the 10 font slots available by SGR calls, but, this console
    // doesn't have remappable fonts.
  }

  action do_gcc {
    // GCC - GRAPHIC CHARACTER COMBINATION.  Overstriking characters would
    // be sweet, but, this isn't implemented.
  }

  action do_gsm {
    // GSM - GRAPHIC SIZE MODIFICATION.  This would scale the font height
    // or width.
  }

  action do_gss {
    // GSS - GRAPHIC SIZE SELECTION.  This sets the font height or width.
  }

  action do_hpa {
    // HPA - CHARACTER POSITION ABSOLUTE.
    console_move_to_column(p1(1)- 1); 	// ECMA-48 coordinates begin at 1
  }

  action do_hpb {
    // HPB - CHARACTER POSITION BACKWARD.
    console_move_left(p1(1));
  }

  action do_hpr {
    // HPR - CHARACTER POSITION FORWARD.
    console_move_right(p1(1));
  }

  action do_ht {
    // HT - CHARACTER TABULATION.  This is a single tab.
    console_move_tab_right(p1(1));
  }

  action do_htj {
    // HTJ - CHARACTER TABULATION WITH JUSTIFICATION. Unimplemented.
  }

  action do_hts {
    // HTS - CHARACTER TABULATION SET.  Unimplemented.
  }
    
  action do_hvp {
    // HVP = CHARACTER AND LINE POSITION.
    console_move_to (p1(1) - 1, p2(1) - 1);
  }

  action do_ich {
    // ICH - INSERT CHARACTER.
    if (hem == HEM_FOLLOWING) {
      console_insert_right(p1(1));
      console_move_to_column(line_home - 1); // ECMA-48 coords begin at 1
    }
    else if (hem == HEM_PRECEDING) {
      console_insert_left(p1(1));
      console_move_to_column(line_home - 1);
    }
  }

  action do_idcs {
    // IDCS - IDENTIFY DEVICE CONTROL STRING.  Unimplemented.
  }

  action do_igs {
    // IGS - IDENTIFY GRAPHIC SUBREPERTOIRE.  Used to indicate that
    // a repertoire of ISO 10367 is used in the following text.
  }

  action do_il {
    // IL - INSERT LINE.
    if (vem == 0)
      console_insert_line_down(p1(1));
    else if (vem == 1)
      console_insert_line_up(p1(1));
    console_move_to_column(line_home - 1); // ECMA-48 coordinates begin at 1
  }
  
  action do_int {
    // INT - INTERRUPT. Data stream indicates to console that the current
    // process is to be interrupted and some agreed procedure is to be
    // initiated.
  }

  action do_is1 {
    // IS1 - INFORMATION SEPARATOR ONE (UNIT SEPARATOR)
  }

  action do_is2 {
    // IS2 - INFORMATION SEPARATOR TWO (RECORD SEPARATOR)
  }

  action do_is3 {
    // IS3 - INFORMATION SEPARATOR THREE (GROUP SEPARATOR)
  }

  action do_is4 {
    // IS4 - INFORMATION SEPARATOR FOUR (FILE SEPARATOR)
  }

  action do_jfy {
    // JFY - JUSTIFY. Indicates the beginning or end of a string that
    // was to be justified on the screen.  Cool, but, unimplemented.
  }

  action do_lf {
    // LF - LINE FEED.
    console_move_down(1);
  }

  action do_ls0 {
    // LS0 - LOCKING-SHIFT ZERO
    // term_locking_shift (0);
    GL = G0;
  }

  action do_ls1 {
    // LS1 - LOCKING-SHIFT ONE
    // term_locking_shift (1);
    GL = G1;
  }

  action do_ls1r {
    // LS1R - LOCKING-SHIFT ONE RIGHT
    // term_locking_shift_right (1);
    GR = G1;
  }

  action do_ls2 {
    // LS2 - LOCKING-SHIFT TWO
    // term_locking_shift (2);
    GL = G2;
  }

  action do_ls2r {
    // LS2R - LOCKING-SHIFT TWO RIGHT
    // term_locking_shift_right (2);
    GR = G2;
  }

  action do_ls3 {
    // LS3 - LOCKING-SHIFT THREE
    // term_locking_shift (3);
    GL = G3;
  }

  action do_ls3r {
    // LS3R - LOCKING-SHIFT THREE RIGHT
    // term_locking_shift_right (3);
    GR = G3;
  }

  action do_mc {
    // MC - MEDIA COPY.  Unimplemented, because there is no
    // auxiliary devoce to which one would copy.
  }

  action do_mw {
    // MW - MESSAGE WAITING. Used to set a message-waiting indicator
    // on the console.
  }

  action do_nak {
    // NAK - NEGATIVE ACKNOWLEDGE.  The data stream sends notifies the
    // console that it is negatively acknowledging to an ENQ.  But,
    // the console never ENQ's the data stream, thus Unimplemented.
  }

  action do_nbh {
    // NBH - NO BREAK HERE.  Clever breaking is not implemented.
  }

  action do_nel {
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

  action do_np {
    // NP - NEXT PAGE.  Unimplemented since the console has a single
    // page.
  }

  action do_nul {
    // NUL - NULL.
  }

  action do_osc {
    // OSC - OPERATING SYSTEM COMMAND.  The data stream sends a string
    // that is denoted as an operating system command.  This console
    // doesn't interpret it.
  }

  action do_pec {
    // PEC - PRESENTATION EXPAND OR CONTRACT.  Unimplemented since
    // this console doesn't have variable spacing.
  }

  action do_pfs {
    // PFS - PAGE FORMAT SELECTION.  Unimplemented since this console's
    // page size can't be modified by the data stream.
  }

  action do_pld {
    // PLD - PARTIAL LINE DOWN.  Unimplemented.  This would move the
    // cursor down by a half line.
  }

  action do_plu {
    // PLU - PARTIAL LINE UP.  Unimplemented.  This would move the cursor
    // up by a half line.
  }

  action do_pm {
    // PM - PRIVACY MESSAGE.  The data stream sends a string to the
    // console denoted as a privacy message.  The console ignores it.
  }

  action do_pp {
    // PP - PRECEDING PAGE.  Unimplemented since this console doesn't
    // have pages.
  }

  action do_ppa {
    // PPA - PAGE POSITION ABSOLUTE.  Would move the active position
    // to a given page.  Unimplemented, since this console has only one page.
  }

  action do_ppb {
    // PPB - PAGE POSITION BACKWARD.  Would move the active position
    // to a previous page.  Unimplemented, since this console has only
    // one page.
  }

  action do_ppr {
    // PPB - PAGE POSITION FORWARD.  Would move the active position
    // to a following page.  Unimplemented, since this console has only
    // one page.
  }

  action do_ptx {
    // PTX - PARALLEL TEXTS.  Denotes that the next couple of strings
    // are supposed to be drawn one over the other.  Unimplemented because
    // it is too complicated.
  }

  action do_pu1 {
    // PU1 - PRIVATE USE ONE. Unimplemented.
  }

  action do_pu2 {
    // PU1 - PRIVATE USE TWO. Unimplemented.
  }

  action do_quad {
    // QUAD.  Would center or flush the previous line or string.
    // Unimplemented because it is too complicated.
  }

  action do_rep {
    // REP - REPEAT.  Would repeat the preceding graphic character
    // multiple times.  Unimplemented.
  }

  action do_ri {
    // RI - REVERSE LINE FEED.
    console_move_up(1);
  }

  action do_ris {
    // RIS - RESET TO INITIAL STATE.
    console_reset();
  }

  action do_rm {
    // RM - RESET MODE
    for (int i = 0; i < P_size; i ++) {
      if (P[i] == 4)
	irm = IRM_REPLACE;
      else if (P[i] == 7)
	vem = 0;
      else if (P[i] == 10)
	hem = HEM_FOLLOWING;
      else if (P[i] == 21)
	grcm = GRCM_COMBINING;
    }
  }

  action do_sacs {
    // SACS - SET ADDITIONAL CHARACTER SEPARATION.  Would change the
    // spacing between characters on the console.  Unimplemented.
  }

  action do_sapv {
    // SAPV - SELECT ALTERNATIVE PRESENTATION.  Some useful functions
    // for Arabic languages.  Unimplemented.
  }

  action do_sci {
    // SCI - SINGLE CHARACTER INTRODUCER.  Not a command, but, a
    // family of possible future commands.  Unimplemented.
  }

  action do_sco {
    // SCO - SELECT CHARACTER ORIENTATION.  Would allow one to choose
    // the rotation of the characters.  Sounds like fun.  Unimplemented.
  }

  action do_scp {
    // SCP - SELECT CHARACTER PATH.  Would allow changing the character
    // path from right-to-left, top-to-bottom, or left-to-right.
    // Unimplemented.
  }

  action do_scs {
    // SCS - SET CHARACTER SPACING.  Would allow modification of the
    // character spacing.  Unimplemented.
  }

  action do_sd {
    // SD - SCROLL DOWN.
    console_scroll_down(p1(1));
  }

  action do_sds {
    // SDS - START DIRECTED STRING. Would allow one to label the 
    // string as right-to-left or left-to-right.  Unimplemented.
  }

  action do_see {
    // SEE - SET EDITING EXTENT.  Would allow one to establish
    // a portion of the console as the extent that is modified
    // by the character insertion or deletion operations.
    // Possibly useful.  Unimplemented.
  }

  action do_sef {
    // SEF - SHEET EJECT AND FEED.  Unimplemented.
  }

  action do_sgr {
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
	  console_set_intensity(CONSOLE_INTENSITY_BOLD);
	  break;
	case 2:
	  console_set_intensity(CONSOLE_INTENSITY_FAINT);
	  break;
	case 3:
	  // Italicized
	  break;
	case 4:
	  console_set_underline(CONSOLE_UNDERLINE_SINGLY);
	  break;
	case 5:
	  console_set_blink(CONSOLE_BLINK_SLOW);
	  break;
	case 6:
	  console_set_blink(CONSOLE_BLINK_FAST);
	  break;
	case 7:
	  console_set_polarity(CONSOLE_POLARITY_NEGATIVE);
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
	  console_set_underline(CONSOLE_UNDERLINE_DOUBLY);
	  break;
	case 22:
	  console_set_intensity(CONSOLE_INTENSITY_NORMAL);
	  console_set_fgcolor(CONSOLE_COLOR_DEFAULT);
	  console_set_bgcolor(CONSOLE_COLOR_DEFAULT);
	  break;
	case 23:
	  // Turn off italicization and fraktur.
	  break;
	case 24:
	  console_set_underline(CONSOLE_UNDERLINE_NONE);
	  break;
	case 25:
	  console_set_blink (CONSOLE_BLINK_NONE);
	  break;
	case 26:
	  // Reserved
	  break;
	case 27:
	  console_set_polarity(CONSOLE_POLARITY_POSITIVE);
	  break;
	case 28:
	  // Turn off concealed
	  break;
	case 29:
	  // Turn off strike-through
	  break;
	case 30:
	  console_set_fgcolor(CONSOLE_COLOR_BLACK);
	  break;
	case 31:
	  console_set_fgcolor(CONSOLE_COLOR_RED);
	  break;
	case 32:
	  console_set_fgcolor(CONSOLE_COLOR_GREEN);
	  break;
	case 33:
	  console_set_fgcolor(CONSOLE_COLOR_YELLOW);
	  break;
	case 34:
	  console_set_fgcolor(CONSOLE_COLOR_BLUE);
	  break;
	case 35:
	  console_set_fgcolor(CONSOLE_COLOR_MAGENTA);
	  break;
	case 36:
	  console_set_fgcolor(CONSOLE_COLOR_CYAN);
	  break;
	case 37:
	  console_set_fgcolor(CONSOLE_COLOR_WHITE);
	  break;
	case 38:
	  console_set_fgcolor(CONSOLE_COLOR_TRANSPARENT);
	  break;
	case 39:
	  // white is default
	  console_set_fgcolor(CONSOLE_COLOR_DEFAULT);
	  break;
	case 40:
	  console_set_bgcolor(CONSOLE_COLOR_BLACK);
	  break;
	case 41:
	  console_set_bgcolor(CONSOLE_COLOR_RED);
	  break;
	case 42:
	  console_set_bgcolor(CONSOLE_COLOR_GREEN);
	  break;
	case 43:
	  console_set_bgcolor(CONSOLE_COLOR_YELLOW);
	  break;
	case 44:
	  console_set_bgcolor(CONSOLE_COLOR_BLUE);
	  break;
	case 45:
	  console_set_bgcolor(CONSOLE_COLOR_MAGENTA);
	  break;
	case 46:
	  console_set_bgcolor(CONSOLE_COLOR_CYAN);
	  break;
	case 47:
	  console_set_bgcolor(CONSOLE_COLOR_WHITE);
	  break;
	case 48:
	  console_set_bgcolor(CONSOLE_COLOR_TRANSPARENT);
	  break;
	case 49:
	  // Default BG color
	  console_set_bgcolor(CONSOLE_COLOR_DEFAULT);
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

  action do_shs {
    // SHS - SELECT CHARACTER SPACING. Would change the number of
    // characters per inch.  Unimplemented.
  }

  action do_simd {
    // SIMD - SELECT IMPLICIT MOVEMENT DIRECTION. Necessary for right-to-left
    // languages.
    simd = p1(0);
  }

  action do_sl {
    // SL - SCROLL LEFT.
    console_scroll_left (p1(1));
  }

  action do_slh {
    // SLH - SET LINE HOME. Determines the location of the cursor after,
    // carriage return, next line, insert line or delete line.
    line_home = p1(1);
  }

  action do_sll {
    // SLL - SET LINE LIMIT
    line_limit = p1(CONSOLE_COLS);
  }

  action do_sls {
    // SLS - SET LINE SPACING. Determine line spacing for following text.
    // Unimplemented.
  }

  action do_sm {
    // SM - SET MODE.  Flip the boolean mode to the 
    // non-nominal state.
    for (int i = 0; i < P_size; i ++) {
      if (P[i] == 4)
	irm = IRM_INSERT;
      else if (P[i] == 7)
	vem = 1;
      else if (P[i] == 10)
	hem = HEM_PRECEDING;
      else if (P[i] == 21)
	grcm = GRCM_REPLACING;
    }
  }

  action do_sos {
    // SOS - START OF STRING.  Allows the data stream to send a string
    // to the console.  This console doesn't interpret them.
  }

  action do_spa {
    // SPA - START OF GUARDED AREA. Guarding is not implemented.
  }

  action do_spd {
    // SPD - SELECT PRESENTATION DIRECTIONS.  Directionality is
    // unimplemented.
  }
  
  action do_sph {
    // SPH - SET PAGE HOME.  Sets the line to which the active position
    // is set after a FORM FEED.
    page_home = p1(1);
  }

  action do_spi {
    // SPI - SPACING INCREMENT.  Used to set the line and character
    // spacing.  Unimplemented.
  }

  action do_spl {
    // SPL - SET PAGE LIMIT.  Sets the line on the page beyond which
    // the active position may not be moved.  Unimplemented, because
    // it is fixed for this console.
  }
  
  action do_spqr {
    // SPQR - SELECT PRINT QUALITY AND RAPIDITY.  Would let one
    // choose the DPI of the fonts.  Unimplemented.
  }

  action do_sr {
    // SR _SCROLL RIGHT.
    console_scroll_right (p1(1));
  }

  action do_srcs {
    // SRCS - SET REDUCED CHARACTER SEPARATOR. Would reduce in
    // inter-character spacing.  Unimplemented.
  }

  action do_srs {
    // SRS - START REVERSED STRING.  Would allow one to designate some
    // text to have the opposite directionality.  Unimplemented.
  }

  action do_ssa {
    // SSA - START OF SELECTED AREA.  Areas are unimplemented.
  }

  action do_ssu {
    // SSU - SELECT SIZE UNIT. Would allow one to establish the units used
    // in the various size-setting functions.  Unimplemented.
  }

  action do_ssw {
    // SSW - SET SPACE WIDTH.  Would allow one to set the width of a space.
    // Unimplemented.
  }

  action do_ss2 {
    // SS2 - SINGLE-SHIFT TWO.  The next graphical character comes from
    // character set G2.
    // term_single_shift (2);
    console_write_char (codepoint_tables[G2][*p - 0x20], irm, hem, simd,
		  line_home - 1, line_limit - 1);
  }
  
  action do_ss3 {
    // SS3 - SINGLE-SHIFT THREE.  The next graphical character will come
    // from the character set G3.
    // term_single_shift (3);
    console_write_char (codepoint_tables[G3][*p - 0x20], irm, hem, simd,
		  line_home - 1, line_limit - 1);
  }

  action do_stab {
    // STAB - SELECTIVE TABULATION.  Something to do with jumping to
    // a specified tab.  Unimplemented.
  }

  action do_sts {
    // STS - SET TRANSMIT STATE.  Used to set the transmit state of the
    // console.  Unimplemented.
  }

  action do_su {
    // SU - SCROLL UP.
    console_scroll_up(p1(1));
  }

  action do_sub {
    // SUB - SUBSTITUTE.  If the data stream sent a SUB, it would be
    // indicating that it is a replacement for an invalid or erroneous
    // byte.  Ignored.
  }

  action do_svs {
    // SVS - SELECT LINE SPACING.  Would set the lines-per-inch for the
    // following text.  Unimplemented.
  }

  action do_syn {
    // SYN - SYNCHRONOUS IDLE.  For really old protocols, this was sent out
    // regularly provide a synchronous signal.  Ignored.
  }

  action do_tac {
    // TAC - TABULATION ALIGNED CENTRED.  Would allow one to center a field
    // on a tab stop.  Unimplemented.
  }

  action do_tale {
    // TAC - TABULATION ALIGNED LEADING EDGE.  Would allow one to
    // center a field on a tab stop.  Unimplemented.
  }

  action do_tate {
    // TAC - TABULATION ALIGNED TRAILING EDGE.  Would allow one to
    // center a field on a tab stop.  Unimplemented.
  }
  
  action do_tbc {
    // TBC - TABULATION CLEAR. Woudl allow one to clear tabstops.
    // Unimplemented.
  }

  action do_tcc {
    // TCC - TABULATION CENTRED ON CHARACTER.  Another fancy tab stop.
    // Unimplemented.
  }

  action do_tsr {
    // TSR - TABULATION STOP REMOVE.  Remove the tab stop at the current
    // location.  Unimplemented.
  }

  action do_tss {
    // TSS - THIN SPACE SPECIFICATION.  Thin spaces are unimplemented.
  }

  action do_vpa {
    // VPA - LINE POSITION ABSOLUTE.
    console_move_to_row(p1(1));
  }

  action do_vpb {
    // VPB - LINE POSITION BACKWARD
    console_move_up(p1(1));
  }

  action do_vpr {
    // VPR - LINE POSITION FORWARD
    console_move_down(p1(1));
  }

  action do_vt {
    // VT - LINE TABULATION
    console_move_vertical_tab_down(1);
  }

  action do_vts {
    // VTS - LINE TABULATION SET.  Would set the vertical tab stops.
    // Unimplemented.
  }

  action do_print {
    int n = *p;
    if (n >= 0x20 && n <= 0x7F)
      console_write_char (codepoint_tables[GL][n - 0x20], irm, hem, simd,
		       line_home - 1, line_limit - 1);
    else if (n >= 0xA0 && n <= 0xFF)
      console_write_char (codepoint_tables[GR][n - 0xA0], irm, hem, simd,
		       line_home - 1, line_limit - 1);
  }
  
  action parse_P_void {
    P_size = 0;
  }
  
  action parse_P {
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
  
  
    command_string = ('\b' | space | graph)*;
    character_string = (any - (0x1B 0x58 | 0x98 | 0x1B 0x5C | 0x9C))*;
    Pn = '' @parse_P_void | digit+ @parse_P;
    Pn2 = '' @parse_P_void
      | digit+ @parse_P
      | (digit* ';' digit*) @parse_P;
    Ps = '' @parse_P_void | digit+ @parse_P;
    Ps2 = '' @parse_P_void
      | digit+ @parse_P
      | (digit* ';' digit*) @parse_P;
    Ps_ = ''
      | digit+
      | (digit* ';')+ digit*;

    # Codepoint
    GLYPH = ([\x20] | print | [\xA0-\xff]) %do_print;

    # C0 control functions from Section 5.2
    NUL = 0x00 @do_nul;
    SOH = 0x01;
    STX = 0x02;
    ETX = 0x03;
    EOT = 0x04;
    ENQ = 0x05;
    ACK = 0x06 @do_ack;
    BEL = 0x07 @do_bel;
    BS  = 0x08 @do_bs;
    HT  = 0x09 @do_ht;
    LF  = 0x0A @do_lf;
    VT  = 0x0B @do_vt;
    FF  = 0x0C @do_ff;
    CR  = 0x0D @do_cr;
    SO  = 0x0E @do_ls0;
    SI  = 0x0F @do_ls1;
    DLE = 0x10 @do_dle;
    DC1 = 0x11 @do_dc1;
    DC2 = 0x12 @do_dc2;
    DC3 = 0x13 @do_dc3;
    DC4 = 0x14 @do_dc4;
    NAK = 0x15 @do_nak;
    SYN = 0x16 @do_syn;
    ETB = 0x17;
    CAN = 0x18 @do_can;
    EM  = 0x19 @do_em;
    SUB = 0x1A @do_sub;
    ESC = 0x1B;
    IS4 = 0x1C @do_is4;
    IS3 = 0x1D @do_is3;
    IS2 = 0x1E @do_is2;
    IS1 = 0x1F @do_is1;

    # C1 controls from Table 2a 
    ST  = (ESC 0x5C | 0x9C);
    BPH = (ESC 0x42 | 0x82) @do_bph;
    NBH = (ESC 0x43 | 0x83) @do_nbh;
    NEL = (ESC 0x45 | 0x85) @do_nel;
    SSA = (ESC 0x46 | 0x86) @do_ssa;
    ESA = (ESC 0x47 | 0x87);
    HTS = (ESC 0x48 | 0x88) @do_hts;
    HTJ = (ESC 0x49 | 0x89) @do_htj;
    VTS = (ESC 0x4A | 0x8A) @do_vts;
    PLD = (ESC 0x4B | 0x8B) @do_pld;
    PLU = (ESC 0x4C | 0x8C) @do_plu;
    RI  = (ESC 0x4D | 0x8D) @do_ri;
    SS2 = (ESC 0x4E | 0x8E) graph @do_ss2;
    SS3 = (ESC 0x4F | 0x8F) graph @do_ss3;
    DCS = (ESC 0x50 | 0x90) command_string ST @do_dcs;
    PU1 = (ESC 0x51 | 0x91) @do_pu1;
    PU2 = (ESC 0x52 | 0x92) @do_pu2;
    STS = (ESC 0x53 | 0x93) @do_sts;
    CCH = (ESC 0x54 | 0x94) @do_cch;
    MW  = (ESC 0x55 | 0x95) @do_mw;
    SPA = (ESC 0x56 | 0x96) @do_spa;
    EPA = (ESC 0x57 | 0x97) @do_epa;
    SOS = (ESC 0x58 | 0x98) character_string ST @do_sos;
    SCI = (ESC 0x5A | 0x9A) ([\b] | space | graph) @do_sci;
    CSI = (ESC 0x5B | 0x9B);
    OSC = (ESC 0x5D | 0x9D) command_string ST @do_osc;
    PM  = (ESC 0x5E | 0x9E) command_string ST @do_pm;
    APC = (ESC 0x5F | 0x9F) command_string ST @do_apc;

    CBP = CSI Pn @store_param 0x5A @do_cbt;
    CHA = CSI Pn @store_param 0x47 @do_cha;
    CHT = CSI Pn @store_param 0x49 @do_cht;
    CNL = CSI Pn @store_param 0x45 @do_cnl;
    CPH = CSI Pn @store_param 0x46 @do_cpl;
    CPR = CSI Pn2 0x52 @do_cpr;
    CTC = CSI Ps_ 0x57 @do_ctc;
    CUB = CSI Pn 0x44 @do_cub;
    CUD = CSI Pn 0x42 @do_cud;
    CUF = CSI Pn 0x43 @do_cuf;
    CUP = CSI Pn2 0x48 @do_cup;
    CUU = CSI Pn 0x41 @do_cuu;
    CVT = CSI Pn 0x59 @do_cvt;
    DA  = CSI Ps 0x63 @do_da;
    DAQ = CSI Ps_ 0x6F @do_daq;
    DCH = CSI Pn 0x50 @do_dch;
    DL  = CSI Pn 0x4D @do_dl;
    DSR = CSI Ps 0x6E @do_dsr;
    DTA = CSI Pn2 0x20 0x54 @do_dta;
    EA  = CSI Ps 0x4F @do_ea;
    ECH = CSI Pn 0x58 @do_ech;
    ED  = CSI Ps 0x4A @do_ed;
    EF  = CSI Ps 0x4D @do_ef;
    EL  = CSI Ps 0x4B @do_el;
    FNK = CSI Pn 0x20 0x57 @do_fnk;
    FNT = CSI Ps2 0x20 0x44 @do_fnt;
    GCC = CSI Ps 0x20 0x5F @do_gcc;
    GSM = CSI Pn2 0x20 0x42 @do_gsm;
    GSS = CSI Pn 0x20 0x43 @do_gss;
    HPA = CSI Pn 0x60 @do_hpa;
    HPB = CSI Pn 0x6A @do_hpb;
    HPR = CSI Pn 0x61 @do_hpr;
    HVP = CSI Pn2 0x66 @do_hvp;
    ICH = CSI Pn 0x40 @do_ich;
    IDCS = CSI Ps 0x20 0x4E @do_idcs;
    IGS = CSI Ps 0x20 0x4C @do_igs;
    IL  = CSI Pn 0x4C @do_il;
    INT = ESC 0x61 @do_int;
    JFY = CSI Ps_ 0x20 0x46 @do_jfy;
    LS1R = ESC 0x7E @do_ls1r;
    LS2R = ESC 0x6E @do_ls2r;
    LS3 = ESC 0x6F @do_ls3;
    LS3R = ESC 0x7C @do_ls3r;
    MC  = CSI Ps 0x69 @do_mc;
    NP  = CSI Pn 0x55 @do_np;
    PEC = CSI Ps 0x20 0x5A @do_pec;
    PFS = CSI Ps 0x20 0x4A @do_pfs;
    PP  = CSI Pn 0x56 @do_pp;
    PPA = CSI Pn 0x20 0x50 @do_ppa;
    PPB = CSI Pn 0x20 0x52 @do_ppb;
    PPR = CSI Pn 0x20 0x51 @do_ppr;
    PTX = CSI Ps 0x5C @do_ptx;
    QUAD = CSI Ps_ 0x20 0x48 @do_quad;
    REP = CSI Pn 0x62 @do_rep;
    RIS = ESC 0x63 @do_ris;
    RM  = CSI Ps_ 0x6C @do_rm;
    SACS = CSI Pn 0x20 0x5C @do_sacs;
    SAPV = CSI Ps_ 0x20 0x5D @do_sapv;
    SCO = CSI Ps 0x20 0x65 @do_sco;
    SCP = CSI Ps2 0x20 0x6B @do_scp;
    SCS = CSI Pn 0x20 0x67 @do_scs;
    SD  = CSI Pn 0x54 @do_sd;
    SDS = CSI Ps 0x5D @do_sds;
    SEE = CSI Ps 0x51 @do_see;
    SEF = CSI Ps2 0x20 0x59 @do_sef;
    SGR = CSI Ps_ 0x6D %do_sgr;
    SHS = CSI Ps 0x20 0x4B @do_shs;
    SIMD = CSI Ps 0x5E @do_simd;
    SL  = CSI Pn 0x20 0x40 @do_sl;
    SLH = CSI Pn 0x20 0x55 @do_slh;
    SLL = CSI Pn 0x20 0x56 @do_sll;
    SLS = CSI Pn 0x20 0x68 @do_sls;
    SM  = CSI Ps_ 0x67 @do_sm;
    SPD = CSI Pn2 0x20 0x63 @do_spd;
    SPH = CSI Pn 0x20 0x69 @do_sph;
    SPI = CSI Pn2 0x20 0x47 @do_spi;
    SPL = CSI Pn 0x20 0x6A @do_spl;
    SPQR = CSI Ps 0x20 0x58 @do_spqr;
    SR  = CSI Pn 0x20 0x41 @do_sr;
    SRCS = CSI Pn 0x20 0x66 @do_srcs;
    SRS = CSI Ps 0x5B @do_srs;
    SSU = CSI Ps 0x20 0x49 @do_ssu;
    SSW = CSI Pn 0x20 0x5B @do_ssw;
    STAB = CSI Ps 0x20 0x5E @do_stab;
    SU  = CSI Pn 0x53 @do_su;    
    SVS = CSI Ps 0x20 0x4C @do_svs;
    TAC = CSI Pn 0x20 0x62 @do_tac;
    TALE = CSI Pn 0x20 0x61 @do_tale;
    TATE = CSI Pn 0x20 0x60 @do_tate;
    TBC = CSI Ps 0x67 @do_tbc;
    TCC = CSI Pn2 0x20 0x63 @do_tcc;
    TSR = CSI Pn 0x20 0x64 @do_tsr;
    TSS = CSI Pn 0x20 0x45 @do_tss;
    VPA = CSI Pn 0x64 @do_vpa;
    VPB = CSI Pn 0x6B @do_vpb;
    VPR = CSI Pn 0x65 @do_vpr;    
    
    # ISO 1745 Messages
    isoheader = (space | graph)+;
    isotext = (space | graph)+;
    ISO1745 = (SOH isoheader ETB)* (SOH isoheader)? STX (isotext ETB)* isotext ETX;

    main := (
      NUL | ACK | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE | DC1
      | DC2 | DC3 | DC4 | CAN | EM | SUB | IS4 | IS3 | IS2 | IS1
      | BPH | NBH | NEL | SSA | ESA | HTS | HTJ | VTS | PLD | PLU | RI
      | SS2 | SS3 | DCS | PU1 | PU2 | STS | CCH | MW | SPA | EPA | SOS
      | SCI | CSI | ST | OSC | PM 
      | APC | CBP | CHA | CHT | CNL | CPH | CPR | CTC | CUB | CUD | CUF
      | CUP | CUU | CVT | DA | DAQ | DCH | DL | DSR | DTA | EA | ECH | ED
      | EF | EL | FNK | FNT | GCC | GSM | HPA | HPB | HPR | ICH | IGS
      | IL | INT | JFY | LS1R | LS2R | LS3 | LS3R | MC | NP | PEC
      | PFS | PP | PPA | PPB | PPR | PTX | QUAD | REP | RIS | RM | SACS
      | SCO | SCP | SCS | SD | SDS | SEE | SEF | SGR | SIMD | SL | SLH
      |	SLL | SLS | SM | SPD | SPH | SPI | SPL | SPQR | SR | SRCS
      | SRS | SSU | SSW | STAB | SU | SVS | TAC | TALE | TATE | TBC
      | TCC | TSR | TSS | VPA | VPB | VPR
      | ISO1745
      | GLYPH
      )*;
}%%

%% write data;

static unsigned char *p;
static unsigned char *pe;
static unsigned char *eof;
// static int cs;

int ecma48_init(void)
{
    // Writing moves right
    simd = 0;
    // overwrite, not insert
    irm = IRM_REPLACE;
    // inserting moves write
    hem = HEM_FOLLOWING;
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
    
    %% write init;
    return 1;
}

int ecma48_execute(const unsigned char *data, int len)
{
    p = (unsigned char *) data;
    pe = (unsigned char *)data + len;
    eof = pe;

    %% write exec;

    if ( cs == ECMA48_error )
        return -1;
    if ( cs >= ECMA48_first_final )
        return 1;
    return 0;
}
