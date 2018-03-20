#include <stdint.h>
/* Maximum height and width of any glyph in the set */

#define FIXED6x9_MAXHEIGHT  9
#define FIXED6x9_MAXWIDTH   6

/* The width of a space */

#define FIXED6x9_SPACEWIDTH 6

/* space (U+0020) */
#define FIXED6x9_METRICS_32 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_32 {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* exclam (U+0021) */
#define FIXED6x9_METRICS_33 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_33 {0x00, 0x20, 0x20, 0x20, 0x20, 0x00, 0x20, 0x00, 0x00}

/* quotedbl (U+0022) */
#define FIXED6x9_METRICS_34 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_34 {0x00, 0x50, 0x50, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00}

/* numbersign (U+0023) */
#define FIXED6x9_METRICS_35 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_35 {0x00, 0x50, 0x50, 0xF8, 0x50, 0xF8, 0x50, 0x50, 0x00}

/* dollar (U+0024) */
#define FIXED6x9_METRICS_36 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_36 {0x20, 0x70, 0xA8, 0xA0, 0x70, 0x28, 0xA8, 0x70, 0x20}

/* percent (U+0025) */
#define FIXED6x9_METRICS_37 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_37 {0x40, 0xA8, 0x48, 0x10, 0x20, 0x48, 0x54, 0x08, 0x00}

/* ampersand (U+0026) */
#define FIXED6x9_METRICS_38 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_38 {0x00, 0x60, 0x90, 0x90, 0x60, 0x98, 0x90, 0x68, 0x00}

/* quotesingle (U+0027) */
#define FIXED6x9_METRICS_39 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_39 {0x00, 0x20, 0x20, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00}

/* parenleft (U+0028) */
#define FIXED6x9_METRICS_40 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_40 {0x00, 0x10, 0x20, 0x20, 0x20, 0x20, 0x20, 0x10, 0x00}

/* parenright (U+0029) */
#define FIXED6x9_METRICS_41 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_41 {0x00, 0x20, 0x10, 0x10, 0x10, 0x10, 0x10, 0x20, 0x00}

/* asterisk (U+002A) */
#define FIXED6x9_METRICS_42 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_42 {0x00, 0x00, 0x88, 0x50, 0xF8, 0x50, 0x88, 0x00, 0x00}

/* plus (U+002B) */
#define FIXED6x9_METRICS_43 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_43 {0x00, 0x00, 0x20, 0x20, 0xF8, 0x20, 0x20, 0x00, 0x00}

/* comma (U+002C) */
#define FIXED6x9_METRICS_44 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_44 {0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x10, 0x10, 0x20}

/* hyphen (U+002D) */
#define FIXED6x9_METRICS_45 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_45 {0x00, 0x00, 0x00, 0x00, 0xF8, 0x00, 0x00, 0x00, 0x00}

/* period (U+002E) */
#define FIXED6x9_METRICS_46 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_46 {0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x30, 0x00, 0x00}

/* slash (U+002F) */
#define FIXED6x9_METRICS_47 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_47 {0x00, 0x08, 0x08, 0x10, 0x20, 0x40, 0x40, 0x00, 0x00}

/* zero (U+0030) */
#define FIXED6x9_METRICS_48 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_48 {0x00, 0x30, 0x48, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* one (U+0031) */
#define FIXED6x9_METRICS_49 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_49 {0x00, 0x20, 0x60, 0x20, 0x20, 0x20, 0x70, 0x00, 0x00}

/* two (U+0032) */
#define FIXED6x9_METRICS_50 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_50 {0x00, 0x30, 0x48, 0x08, 0x10, 0x20, 0x78, 0x00, 0x00}

/* three (U+0033) */
#define FIXED6x9_METRICS_51 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_51 {0x00, 0x78, 0x10, 0x30, 0x08, 0x08, 0x70, 0x00, 0x00}

/* four (U+0034) */
#define FIXED6x9_METRICS_52 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_52 {0x00, 0x10, 0x30, 0x50, 0x90, 0xF8, 0x10, 0x00, 0x00}

/* five (U+0035) */
#define FIXED6x9_METRICS_53 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_53 {0x00, 0x78, 0x40, 0x70, 0x08, 0x08, 0x70, 0x00, 0x00}

/* six (U+0036) */
#define FIXED6x9_METRICS_54 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_54 {0x00, 0x30, 0x40, 0x70, 0x48, 0x48, 0x30, 0x00, 0x00}

/* seven (U+0037) */
#define FIXED6x9_METRICS_55 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_55 {0x00, 0x78, 0x08, 0x08, 0x10, 0x20, 0x20, 0x00, 0x00}

/* eight (U+0038) */
#define FIXED6x9_METRICS_56 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_56 {0x00, 0x30, 0x48, 0x30, 0x48, 0x48, 0x30, 0x00, 0x00}

/* nine (U+0039) */
#define FIXED6x9_METRICS_57 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_57 {0x00, 0x30, 0x48, 0x48, 0x38, 0x08, 0x30, 0x00, 0x00}

/* colon (U+003A) */
#define FIXED6x9_METRICS_58 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_58 {0x00, 0x00, 0x30, 0x30, 0x00, 0x30, 0x30, 0x00, 0x00}

/* semicolon (U+003B) */
#define FIXED6x9_METRICS_59 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_59 {0x00, 0x00, 0x30, 0x30, 0x00, 0x30, 0x10, 0x10, 0x20}

/* less (U+003C) */
#define FIXED6x9_METRICS_60 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_60 {0x00, 0x00, 0x18, 0x60, 0x80, 0x60, 0x18, 0x00, 0x00}

/* equal (U+003D) */
#define FIXED6x9_METRICS_61 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_61 {0x00, 0x00, 0x00, 0xF8, 0x00, 0xF8, 0x00, 0x00, 0x00}

/* greater (U+003E) */
#define FIXED6x9_METRICS_62 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_62 {0x00, 0x00, 0xC0, 0x30, 0x08, 0x30, 0xC0, 0x00, 0x00}

/* question (U+003F) */
#define FIXED6x9_METRICS_63 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_63 {0x30, 0x48, 0x08, 0x30, 0x20, 0x00, 0x20, 0x00, 0x00}

/* at (U+0040) */
#define FIXED6x9_METRICS_64 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_64 {0x00, 0x70, 0x90, 0xA8, 0xB0, 0x80, 0x70, 0x00, 0x00}

/* A (U+0041) */
#define FIXED6x9_METRICS_65 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_65 {0x00, 0x20, 0x50, 0x88, 0xF8, 0x88, 0x88, 0x00, 0x00}

/* B (U+0042) */
#define FIXED6x9_METRICS_66 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_66 {0x00, 0xF0, 0x88, 0xF0, 0x88, 0x88, 0xF0, 0x00, 0x00}

/* C (U+0043) */
#define FIXED6x9_METRICS_67 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_67 {0x00, 0x30, 0x48, 0x40, 0x40, 0x48, 0x30, 0x00, 0x00}

/* D (U+0044) */
#define FIXED6x9_METRICS_68 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_68 {0x00, 0x70, 0x48, 0x48, 0x48, 0x48, 0x70, 0x00, 0x00}

/* E (U+0045) */
#define FIXED6x9_METRICS_69 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_69 {0x00, 0x78, 0x40, 0x70, 0x40, 0x40, 0x78, 0x00, 0x00}

/* F (U+0046) */
#define FIXED6x9_METRICS_70 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_70 {0x00, 0x78, 0x40, 0x70, 0x40, 0x40, 0x40, 0x00, 0x00}

/* G (U+0047) */
#define FIXED6x9_METRICS_71 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_71 {0x00, 0x30, 0x48, 0x40, 0x58, 0x48, 0x30, 0x00, 0x00}

/* H (U+0048) */
#define FIXED6x9_METRICS_72 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_72 {0x00, 0x48, 0x48, 0x78, 0x48, 0x48, 0x48, 0x00, 0x00}

/* I (U+0049) */
#define FIXED6x9_METRICS_73 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_73 {0x00, 0x70, 0x20, 0x20, 0x20, 0x20, 0x70, 0x00, 0x00}

/* J (U+004A) */
#define FIXED6x9_METRICS_74 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_74 {0x00, 0x38, 0x10, 0x10, 0x10, 0x90, 0x60, 0x00, 0x00}

/* K (U+004B) */
#define FIXED6x9_METRICS_75 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_75 {0x00, 0x48, 0x50, 0x60, 0x50, 0x48, 0x48, 0x00, 0x00}

/* L (U+004C) */
#define FIXED6x9_METRICS_76 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_76 {0x00, 0x40, 0x40, 0x40, 0x40, 0x40, 0x78, 0x00, 0x00}

/* M (U+004D) */
#define FIXED6x9_METRICS_77 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_77 {0x00, 0x88, 0xD8, 0xA8, 0xA8, 0x88, 0x88, 0x00, 0x00}

/* N (U+004E) */
#define FIXED6x9_METRICS_78 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_78 {0x00, 0x48, 0x68, 0x58, 0x48, 0x48, 0x48, 0x00, 0x00}

/* O (U+004F) */
#define FIXED6x9_METRICS_79 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_79 {0x00, 0x70, 0x88, 0x88, 0x88, 0x88, 0x70, 0x00, 0x00}

/* P (U+0050) */
#define FIXED6x9_METRICS_80 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_80 {0x00, 0x70, 0x48, 0x48, 0x70, 0x40, 0x40, 0x00, 0x00}

/* Q (U+0051) */
#define FIXED6x9_METRICS_81 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_81 {0x00, 0x30, 0x48, 0x48, 0x68, 0x58, 0x30, 0x08, 0x00}

/* R (U+0052) */
#define FIXED6x9_METRICS_82 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_82 {0x00, 0x70, 0x48, 0x48, 0x70, 0x48, 0x48, 0x00, 0x00}

/* S (U+0053) */
#define FIXED6x9_METRICS_83 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_83 {0x00, 0x30, 0x48, 0x20, 0x10, 0x48, 0x30, 0x00, 0x00}

/* T (U+0054) */
#define FIXED6x9_METRICS_84 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_84 {0x00, 0xF8, 0x20, 0x20, 0x20, 0x20, 0x20, 0x00, 0x00}

/* U (U+0055) */
#define FIXED6x9_METRICS_85 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_85 {0x00, 0x48, 0x48, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* V (U+0056) */
#define FIXED6x9_METRICS_86 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_86 {0x00, 0x48, 0x48, 0x48, 0x78, 0x30, 0x30, 0x00, 0x00}

/* W (U+0057) */
#define FIXED6x9_METRICS_87 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_87 {0x00, 0x88, 0x88, 0xA8, 0xA8, 0xD8, 0x88, 0x00, 0x00}

/* X (U+0058) */
#define FIXED6x9_METRICS_88 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_88 {0x00, 0x88, 0x50, 0x20, 0x20, 0x50, 0x88, 0x00, 0x00}

/* Y (U+0059) */
#define FIXED6x9_METRICS_89 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_89 {0x00, 0x88, 0x88, 0x50, 0x20, 0x20, 0x20, 0x00, 0x00}

/* Z (U+005A) */
#define FIXED6x9_METRICS_90 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_90 {0x00, 0x78, 0x08, 0x10, 0x20, 0x40, 0x78, 0x00, 0x00}

/* bracketleft (U+005B) */
#define FIXED6x9_METRICS_91 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_91 {0x00, 0x70, 0x40, 0x40, 0x40, 0x40, 0x70, 0x00, 0x00}

/* backslash (U+005C) */
#define FIXED6x9_METRICS_92 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_92 {0x00, 0x40, 0x40, 0x20, 0x10, 0x08, 0x08, 0x00, 0x00}

/* bracketright (U+005D) */
#define FIXED6x9_METRICS_93 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_93 {0x00, 0x70, 0x10, 0x10, 0x10, 0x10, 0x70, 0x00, 0x00}

/* asciicircum (U+005E) */
#define FIXED6x9_METRICS_94 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_94 {0x00, 0x20, 0x50, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00}

/* underscore (U+005F) */
#define FIXED6x9_METRICS_95 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_95 {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF8}

/* grave (U+0060) */
#define FIXED6x9_METRICS_96 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_96 {0x00, 0x20, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* a (U+0061) */
#define FIXED6x9_METRICS_97 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_97 {0x00, 0x00, 0x00, 0x38, 0x48, 0x48, 0x38, 0x00, 0x00}

/* b (U+0062) */
#define FIXED6x9_METRICS_98 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_98 {0x00, 0x40, 0x40, 0x70, 0x48, 0x48, 0x70, 0x00, 0x00}

/* c (U+0063) */
#define FIXED6x9_METRICS_99 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_99 {0x00, 0x00, 0x00, 0x38, 0x40, 0x40, 0x38, 0x00, 0x00}

/* d (U+0064) */
#define FIXED6x9_METRICS_100 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_100 {0x00, 0x08, 0x08, 0x38, 0x48, 0x48, 0x38, 0x00, 0x00}

/* e (U+0065) */
#define FIXED6x9_METRICS_101 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_101 {0x00, 0x00, 0x00, 0x30, 0x58, 0x60, 0x38, 0x00, 0x00}

/* f (U+0066) */
#define FIXED6x9_METRICS_102 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_102 {0x00, 0x10, 0x28, 0x20, 0x70, 0x20, 0x20, 0x00, 0x00}

/* g (U+0067) */
#define FIXED6x9_METRICS_103 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_103 {0x00, 0x00, 0x00, 0x30, 0x48, 0x48, 0x38, 0x08, 0x30}

/* h (U+0068) */
#define FIXED6x9_METRICS_104 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_104 {0x00, 0x40, 0x40, 0x70, 0x48, 0x48, 0x48, 0x00, 0x00}

/* i (U+0069) */
#define FIXED6x9_METRICS_105 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_105 {0x00, 0x20, 0x00, 0x60, 0x20, 0x20, 0x70, 0x00, 0x00}

/* j (U+006A) */
#define FIXED6x9_METRICS_106 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_106 {0x00, 0x10, 0x00, 0x30, 0x10, 0x10, 0x10, 0x50, 0x20}

/* k (U+006B) */
#define FIXED6x9_METRICS_107 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_107 {0x00, 0x40, 0x40, 0x50, 0x60, 0x50, 0x48, 0x00, 0x00}

/* l (U+006C) */
#define FIXED6x9_METRICS_108 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_108 {0x00, 0x60, 0x20, 0x20, 0x20, 0x20, 0x70, 0x00, 0x00}

/* m (U+006D) */
#define FIXED6x9_METRICS_109 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_109 {0x00, 0x00, 0x00, 0xD0, 0xA8, 0xA8, 0x88, 0x00, 0x00}

/* n (U+006E) */
#define FIXED6x9_METRICS_110 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_110 {0x00, 0x00, 0x00, 0x70, 0x48, 0x48, 0x48, 0x00, 0x00}

/* o (U+006F) */
#define FIXED6x9_METRICS_111 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_111 {0x00, 0x00, 0x00, 0x30, 0x48, 0x48, 0x30, 0x00, 0x00}

/* p (U+0070) */
#define FIXED6x9_METRICS_112 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_112 {0x00, 0x00, 0x00, 0x70, 0x48, 0x48, 0x70, 0x40, 0x40}

/* q (U+0071) */
#define FIXED6x9_METRICS_113 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_113 {0x00, 0x00, 0x00, 0x38, 0x48, 0x48, 0x38, 0x08, 0x08}

/* r (U+0072) */
#define FIXED6x9_METRICS_114 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_114 {0x00, 0x00, 0x00, 0x50, 0x68, 0x40, 0x40, 0x00, 0x00}

/* s (U+0073) */
#define FIXED6x9_METRICS_115 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_115 {0x00, 0x00, 0x00, 0x38, 0x60, 0x18, 0x70, 0x00, 0x00}

/* t (U+0074) */
#define FIXED6x9_METRICS_116 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_116 {0x00, 0x20, 0x20, 0x70, 0x20, 0x28, 0x10, 0x00, 0x00}

/* u (U+0075) */
#define FIXED6x9_METRICS_117 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_117 {0x00, 0x00, 0x00, 0x48, 0x48, 0x48, 0x38, 0x00, 0x00}

/* v (U+0076) */
#define FIXED6x9_METRICS_118 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_118 {0x00, 0x00, 0x00, 0x48, 0x48, 0x30, 0x30, 0x00, 0x00}

/* w (U+0077) */
#define FIXED6x9_METRICS_119 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_119 {0x00, 0x00, 0x00, 0x88, 0xA8, 0xA8, 0x50, 0x00, 0x00}

/* x (U+0078) */
#define FIXED6x9_METRICS_120 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_120 {0x00, 0x00, 0x00, 0x48, 0x30, 0x30, 0x48, 0x00, 0x00}

/* y (U+0079) */
#define FIXED6x9_METRICS_121 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_121 {0x00, 0x00, 0x00, 0x48, 0x48, 0x48, 0x38, 0x48, 0x30}

/* z (U+007A) */
#define FIXED6x9_METRICS_122 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_122 {0x00, 0x00, 0x00, 0x78, 0x10, 0x20, 0x78, 0x00, 0x00}

/* braceleft (U+007B) */
#define FIXED6x9_METRICS_123 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_123 {0x10, 0x20, 0x20, 0x40, 0x20, 0x20, 0x10, 0x00, 0x00}

/* bar (U+007C) */
#define FIXED6x9_METRICS_124 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_124 {0x00, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x00}

/* braceright (U+007D) */
#define FIXED6x9_METRICS_125 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_125 {0x40, 0x20, 0x20, 0x10, 0x20, 0x20, 0x40, 0x00, 0x00}

/* asciitilde (U+007E) */
#define FIXED6x9_METRICS_126 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_126 {0x00, 0x00, 0x28, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00}

/* space (U+00A0) */
#define FIXED6x9_METRICS_160 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_160 {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* exclamdown (U+00A1) */
#define FIXED6x9_METRICS_161 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_161 {0x00, 0x20, 0x00, 0x20, 0x20, 0x20, 0x20, 0x00, 0x00}

/* cent (U+00A2) */
#define FIXED6x9_METRICS_162 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_162 {0x00, 0x00, 0x10, 0x38, 0x50, 0x50, 0x38, 0x10, 0x00}

/* sterling (U+00A3) */
#define FIXED6x9_METRICS_163 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_163 {0x00, 0x30, 0x48, 0x40, 0xF0, 0x40, 0x40, 0xF8, 0x00}

/* currency (U+00A4) */
#define FIXED6x9_METRICS_164 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_164 {0x00, 0x00, 0xA8, 0x50, 0x88, 0x50, 0xA8, 0x00, 0x00}

/* yen (U+00A5) */
#define FIXED6x9_METRICS_165 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_165 {0x00, 0x88, 0x50, 0xF8, 0x20, 0xF8, 0x20, 0x00, 0x00}

/* brokenbar (U+00A6) */
#define FIXED6x9_METRICS_166 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_166 {0x00, 0x20, 0x20, 0x20, 0x00, 0x20, 0x20, 0x20, 0x00}

/* section (U+00A7) */
#define FIXED6x9_METRICS_167 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_167 {0x00, 0x38, 0x40, 0x30, 0x48, 0x30, 0x08, 0x70, 0x00}

/* dieresis (U+00A8) */
#define FIXED6x9_METRICS_168 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_168 {0x00, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* copyright (U+00A9) */
#define FIXED6x9_METRICS_169 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_169 {0x78, 0x84, 0x94, 0xA4, 0x94, 0x84, 0x78, 0x00, 0x00}

/* ordfeminine (U+00AA) */
#define FIXED6x9_METRICS_170 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_170 {0x00, 0x30, 0x50, 0x30, 0x00, 0x70, 0x00, 0x00, 0x00}

/* guillemotleft (U+00AB) */
#define FIXED6x9_METRICS_171 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_171 {0x00, 0x00, 0x28, 0x50, 0xA0, 0x50, 0x28, 0x00, 0x00}

/* logicalnot (U+00AC) */
#define FIXED6x9_METRICS_172 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_172 {0x00, 0x00, 0x00, 0x00, 0x78, 0x08, 0x08, 0x00, 0x00}

/* hyphen (U+00AD) */
#define FIXED6x9_METRICS_173 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_173 {0x00, 0x00, 0x00, 0x00, 0x78, 0x00, 0x00, 0x00, 0x00}

/* registered (U+00AE) */
#define FIXED6x9_METRICS_174 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_174 {0x78, 0x84, 0xB4, 0xA4, 0xA4, 0x84, 0x78, 0x00, 0x00}

/* macron (U+00AF) */
#define FIXED6x9_METRICS_175 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_175 {0x00, 0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* degree (U+00B0) */
#define FIXED6x9_METRICS_176 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_176 {0x00, 0x00, 0x30, 0x48, 0x30, 0x00, 0x00, 0x00, 0x00}

/* plusminus (U+00B1) */
#define FIXED6x9_METRICS_177 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_177 {0x00, 0x20, 0x20, 0xF8, 0x20, 0x20, 0x00, 0xF8, 0x00}

/* twosuperior (U+00B2) */
#define FIXED6x9_METRICS_178 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_178 {0x00, 0x20, 0x50, 0x10, 0x20, 0x70, 0x00, 0x00, 0x00}

/* threesuperior (U+00B3) */
#define FIXED6x9_METRICS_179 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_179 {0x00, 0x60, 0x10, 0x20, 0x10, 0x60, 0x00, 0x00, 0x00}

/* acute (U+00B4) */
#define FIXED6x9_METRICS_180 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_180 {0x00, 0x10, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* mu (U+00B5) */
#define FIXED6x9_METRICS_181 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_181 {0x00, 0x00, 0x00, 0x48, 0x48, 0x58, 0x68, 0x40, 0x00}

/* paragraph (U+00B6) */
#define FIXED6x9_METRICS_182 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_182 {0x00, 0x78, 0xE8, 0xE8, 0x68, 0x28, 0x28, 0x00, 0x00}

/* periodcentered (U+00B7) */
#define FIXED6x9_METRICS_183 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_183 {0x00, 0x00, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00}

/* cedilla (U+00B8) */
#define FIXED6x9_METRICS_184 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_184 {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x20}

/* onesuperior (U+00B9) */
#define FIXED6x9_METRICS_185 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_185 {0x00, 0x20, 0x60, 0x20, 0x20, 0x70, 0x00, 0x00, 0x00}

/* ordmasculine (U+00BA) */
#define FIXED6x9_METRICS_186 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_186 {0x00, 0x20, 0x50, 0x20, 0x00, 0x70, 0x00, 0x00, 0x00}

/* guillemotright (U+00BB) */
#define FIXED6x9_METRICS_187 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_187 {0x00, 0x00, 0xA0, 0x50, 0x28, 0x50, 0xA0, 0x00, 0x00}

/* onequarter (U+00BC) */
#define FIXED6x9_METRICS_188 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_188 {0x40, 0xC0, 0x40, 0x50, 0x70, 0x30, 0x78, 0x10, 0x00}

/* onehalf (U+00BD) */
#define FIXED6x9_METRICS_189 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_189 {0x40, 0xC0, 0x40, 0x50, 0x68, 0x08, 0x10, 0x38, 0x00}

/* threequarters (U+00BE) */
#define FIXED6x9_METRICS_190 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_190 {0xC0, 0x20, 0x40, 0x30, 0xF0, 0x30, 0x78, 0x10, 0x00}

/* questiondown (U+00BF) */
#define FIXED6x9_METRICS_191 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_191 {0x10, 0x00, 0x10, 0x30, 0x40, 0x48, 0x30, 0x00, 0x00}

/* Agrave (U+00C0) */
#define FIXED6x9_METRICS_192 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_192 {0x40, 0x20, 0x20, 0x50, 0x70, 0x88, 0x88, 0x00, 0x00}

/* Aacute (U+00C1) */
#define FIXED6x9_METRICS_193 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_193 {0x10, 0x20, 0x20, 0x50, 0x70, 0x88, 0x88, 0x00, 0x00}

/* Acircumflex (U+00C2) */
#define FIXED6x9_METRICS_194 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_194 {0x20, 0x50, 0x20, 0x50, 0x70, 0x88, 0x88, 0x00, 0x00}

/* Atilde (U+00C3) */
#define FIXED6x9_METRICS_195 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_195 {0x28, 0x50, 0x20, 0x50, 0x70, 0x88, 0x88, 0x00, 0x00}

/* Adieresis (U+00C4) */
#define FIXED6x9_METRICS_196 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_196 {0x50, 0x00, 0x20, 0x50, 0x70, 0x88, 0x88, 0x00, 0x00}

/* Aring (U+00C5) */
#define FIXED6x9_METRICS_197 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_197 {0x20, 0x50, 0x20, 0x50, 0x70, 0x88, 0x88, 0x00, 0x00}

/* AE (U+00C6) */
#define FIXED6x9_METRICS_198 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_198 {0x00, 0x78, 0xA0, 0xF0, 0xA0, 0xA0, 0xB8, 0x00, 0x00}

/* Ccedilla (U+00C7) */
#define FIXED6x9_METRICS_199 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_199 {0x00, 0x30, 0x48, 0x40, 0x40, 0x48, 0x30, 0x10, 0x20}

/* Egrave (U+00C8) */
#define FIXED6x9_METRICS_200 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_200 {0x20, 0x10, 0x78, 0x40, 0x70, 0x40, 0x78, 0x00, 0x00}

/* Eacute (U+00C9) */
#define FIXED6x9_METRICS_201 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_201 {0x10, 0x20, 0x78, 0x40, 0x70, 0x40, 0x78, 0x00, 0x00}

/* Ecircumflex (U+00CA) */
#define FIXED6x9_METRICS_202 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_202 {0x10, 0x28, 0x78, 0x40, 0x70, 0x40, 0x78, 0x00, 0x00}

/* Edieresis (U+00CB) */
#define FIXED6x9_METRICS_203 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_203 {0x28, 0x00, 0x78, 0x40, 0x70, 0x40, 0x78, 0x00, 0x00}

/* Igrave (U+00CC) */
#define FIXED6x9_METRICS_204 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_204 {0x40, 0x20, 0x70, 0x20, 0x20, 0x20, 0x70, 0x00, 0x00}

/* Iacute (U+00CD) */
#define FIXED6x9_METRICS_205 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_205 {0x10, 0x20, 0x70, 0x20, 0x20, 0x20, 0x70, 0x00, 0x00}

/* Icircumflex (U+00CE) */
#define FIXED6x9_METRICS_206 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_206 {0x20, 0x50, 0x70, 0x20, 0x20, 0x20, 0x70, 0x00, 0x00}

/* Idieresis (U+00CF) */
#define FIXED6x9_METRICS_207 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_207 {0x50, 0x00, 0x70, 0x20, 0x20, 0x20, 0x70, 0x00, 0x00}

/* Eth (U+00D0) */
#define FIXED6x9_METRICS_208 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_208 {0x00, 0x70, 0x48, 0xE8, 0x48, 0x48, 0x70, 0x00, 0x00}

/* Ntilde (U+00D1) */
#define FIXED6x9_METRICS_209 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_209 {0x28, 0x50, 0x48, 0x68, 0x58, 0x48, 0x48, 0x00, 0x00}

/* Ograve (U+00D2) */
#define FIXED6x9_METRICS_210 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_210 {0x20, 0x10, 0x30, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* Oacute (U+00D3) */
#define FIXED6x9_METRICS_211 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_211 {0x10, 0x20, 0x30, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* Ocircumflex (U+00D4) */
#define FIXED6x9_METRICS_212 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_212 {0x10, 0x28, 0x30, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* Otilde (U+00D5) */
#define FIXED6x9_METRICS_213 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_213 {0x28, 0x50, 0x30, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* Odieresis (U+00D6) */
#define FIXED6x9_METRICS_214 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_214 {0x28, 0x00, 0x30, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* multiply (U+00D7) */
#define FIXED6x9_METRICS_215 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_215 {0x00, 0x00, 0x88, 0x50, 0x20, 0x50, 0x88, 0x00, 0x00}

/* Oslash (U+00D8) */
#define FIXED6x9_METRICS_216 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_216 {0x08, 0x38, 0x58, 0x58, 0x68, 0x68, 0x70, 0x40, 0x00}

/* Ugrave (U+00D9) */
#define FIXED6x9_METRICS_217 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_217 {0x20, 0x10, 0x48, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* Uacute (U+00DA) */
#define FIXED6x9_METRICS_218 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_218 {0x10, 0x20, 0x48, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* Ucircumflex (U+00DB) */
#define FIXED6x9_METRICS_219 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_219 {0x10, 0x28, 0x48, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* Udieresis (U+00DC) */
#define FIXED6x9_METRICS_220 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_220 {0x28, 0x00, 0x48, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* Yacute (U+00DD) */
#define FIXED6x9_METRICS_221 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_221 {0x10, 0x20, 0x88, 0x50, 0x20, 0x20, 0x20, 0x00, 0x00}

/* Thorn (U+00DE) */
#define FIXED6x9_METRICS_222 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_222 {0x00, 0x40, 0x70, 0x48, 0x48, 0x70, 0x40, 0x00, 0x00}

/* germandbls (U+00DF) */
#define FIXED6x9_METRICS_223 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_223 {0x00, 0x30, 0x48, 0x50, 0x50, 0x48, 0x50, 0x00, 0x00}

/* agrave (U+00E0) */
#define FIXED6x9_METRICS_224 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_224 {0x20, 0x10, 0x00, 0x38, 0x48, 0x48, 0x38, 0x00, 0x00}

/* aacute (U+00E1) */
#define FIXED6x9_METRICS_225 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_225 {0x10, 0x20, 0x00, 0x38, 0x48, 0x48, 0x38, 0x00, 0x00}

/* acircumflex (U+00E2) */
#define FIXED6x9_METRICS_226 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_226 {0x10, 0x28, 0x00, 0x38, 0x48, 0x48, 0x38, 0x00, 0x00}

/* atilde (U+00E3) */
#define FIXED6x9_METRICS_227 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_227 {0x28, 0x50, 0x00, 0x38, 0x48, 0x48, 0x38, 0x00, 0x00}

/* adieresis (U+00E4) */
#define FIXED6x9_METRICS_228 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_228 {0x00, 0x28, 0x00, 0x38, 0x48, 0x48, 0x38, 0x00, 0x00}

/* aring (U+00E5) */
#define FIXED6x9_METRICS_229 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_229 {0x10, 0x28, 0x10, 0x38, 0x48, 0x48, 0x38, 0x00, 0x00}

/* ae (U+00E6) */
#define FIXED6x9_METRICS_230 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_230 {0x00, 0x00, 0x00, 0x70, 0xA8, 0xB0, 0x78, 0x00, 0x00}

/* ccedilla (U+00E7) */
#define FIXED6x9_METRICS_231 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_231 {0x00, 0x00, 0x00, 0x38, 0x40, 0x40, 0x38, 0x10, 0x20}

/* egrave (U+00E8) */
#define FIXED6x9_METRICS_232 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_232 {0x20, 0x10, 0x00, 0x30, 0x58, 0x60, 0x38, 0x00, 0x00}

/* eacute (U+00E9) */
#define FIXED6x9_METRICS_233 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_233 {0x10, 0x20, 0x00, 0x30, 0x58, 0x60, 0x38, 0x00, 0x00}

/* ecircumflex (U+00EA) */
#define FIXED6x9_METRICS_234 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_234 {0x10, 0x28, 0x00, 0x30, 0x58, 0x60, 0x38, 0x00, 0x00}

/* edieresis (U+00EB) */
#define FIXED6x9_METRICS_235 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_235 {0x00, 0x28, 0x00, 0x30, 0x58, 0x60, 0x38, 0x00, 0x00}

/* igrave (U+00EC) */
#define FIXED6x9_METRICS_236 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_236 {0x40, 0x20, 0x00, 0x60, 0x20, 0x20, 0x70, 0x00, 0x00}

/* iacute (U+00ED) */
#define FIXED6x9_METRICS_237 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_237 {0x10, 0x20, 0x00, 0x60, 0x20, 0x20, 0x70, 0x00, 0x00}

/* icircumflex (U+00EE) */
#define FIXED6x9_METRICS_238 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_238 {0x20, 0x50, 0x00, 0x60, 0x20, 0x20, 0x70, 0x00, 0x00}

/* idieresis (U+00EF) */
#define FIXED6x9_METRICS_239 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_239 {0x00, 0x50, 0x00, 0x60, 0x20, 0x20, 0x70, 0x00, 0x00}

/* eth (U+00F0) */
#define FIXED6x9_METRICS_240 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_240 {0x18, 0x30, 0x08, 0x38, 0x48, 0x48, 0x30, 0x00, 0x00}

/* ntilde (U+00F1) */
#define FIXED6x9_METRICS_241 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_241 {0x28, 0x50, 0x00, 0x70, 0x48, 0x48, 0x48, 0x00, 0x00}

/* ograve (U+00F2) */
#define FIXED6x9_METRICS_242 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_242 {0x20, 0x10, 0x00, 0x30, 0x48, 0x48, 0x30, 0x00, 0x00}

/* oacute (U+00F3) */
#define FIXED6x9_METRICS_243 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_243 {0x10, 0x20, 0x00, 0x30, 0x48, 0x48, 0x30, 0x00, 0x00}

/* ocircumflex (U+00F4) */
#define FIXED6x9_METRICS_244 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_244 {0x10, 0x28, 0x00, 0x30, 0x48, 0x48, 0x30, 0x00, 0x00}

/* otilde (U+00F5) */
#define FIXED6x9_METRICS_245 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_245 {0x28, 0x50, 0x00, 0x30, 0x48, 0x48, 0x30, 0x00, 0x00}

/* odieresis (U+00F6) */
#define FIXED6x9_METRICS_246 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_246 {0x00, 0x28, 0x00, 0x30, 0x48, 0x48, 0x30, 0x00, 0x00}

/* divide (U+00F7) */
#define FIXED6x9_METRICS_247 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_247 {0x00, 0x00, 0x20, 0x00, 0xF8, 0x00, 0x20, 0x00, 0x00}

/* oslash (U+00F8) */
#define FIXED6x9_METRICS_248 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_248 {0x00, 0x00, 0x00, 0x38, 0x58, 0x68, 0x70, 0x00, 0x00}

/* ugrave (U+00F9) */
#define FIXED6x9_METRICS_249 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_249 {0x20, 0x10, 0x00, 0x48, 0x48, 0x48, 0x38, 0x00, 0x00}

/* uacute (U+00FA) */
#define FIXED6x9_METRICS_250 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_250 {0x10, 0x20, 0x00, 0x48, 0x48, 0x48, 0x38, 0x00, 0x00}

/* ucircumflex (U+00FB) */
#define FIXED6x9_METRICS_251 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_251 {0x10, 0x28, 0x00, 0x48, 0x48, 0x48, 0x38, 0x00, 0x00}

/* udieresis (U+00FC) */
#define FIXED6x9_METRICS_252 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_252 {0x00, 0x28, 0x00, 0x48, 0x48, 0x48, 0x38, 0x00, 0x00}

/* yacute (U+00FD) */
#define FIXED6x9_METRICS_253 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_253 {0x10, 0x20, 0x00, 0x48, 0x48, 0x48, 0x38, 0x48, 0x30}

/* thorn (U+00FE) */
#define FIXED6x9_METRICS_254 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_254 {0x00, 0x40, 0x40, 0x70, 0x48, 0x48, 0x70, 0x40, 0x40}

/* ydieresis (U+00FF) */
#define FIXED6x9_METRICS_255 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_255 {0x00, 0x28, 0x00, 0x48, 0x48, 0x48, 0x38, 0x48, 0x30}

/* Amacron (U+0100) */
#define FIXED6x9_METRICS_256 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_256 {0x70, 0x00, 0x20, 0x50, 0x70, 0x88, 0x88, 0x00, 0x00}

/* amacron (U+0101) */
#define FIXED6x9_METRICS_257 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_257 {0x00, 0x38, 0x00, 0x38, 0x48, 0x48, 0x38, 0x00, 0x00}

/* Abreve (U+0102) */
#define FIXED6x9_METRICS_258 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_258 {0x90, 0x60, 0x20, 0x50, 0x70, 0x88, 0x88, 0x00, 0x00}

/* abreve (U+0103) */
#define FIXED6x9_METRICS_259 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_259 {0x48, 0x30, 0x00, 0x38, 0x48, 0x48, 0x38, 0x00, 0x00}

/* Aogonek (U+0104) */
#define FIXED6x9_METRICS_260 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_260 {0x00, 0x20, 0x50, 0x88, 0xF8, 0x88, 0x88, 0x10, 0x08}

/* aogonek (U+0105) */
#define FIXED6x9_METRICS_261 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_261 {0x00, 0x00, 0x00, 0x38, 0x48, 0x48, 0x38, 0x10, 0x08}

/* Cacute (U+0106) */
#define FIXED6x9_METRICS_262 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_262 {0x10, 0x20, 0x30, 0x48, 0x40, 0x48, 0x30, 0x00, 0x00}

/* cacute (U+0107) */
#define FIXED6x9_METRICS_263 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_263 {0x10, 0x20, 0x00, 0x38, 0x40, 0x40, 0x38, 0x00, 0x00}

/* Ccircumflex (U+0108) */
#define FIXED6x9_METRICS_264 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_264 {0x10, 0x28, 0x30, 0x48, 0x40, 0x48, 0x30, 0x00, 0x00}

/* ccircumflex (U+0109) */
#define FIXED6x9_METRICS_265 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_265 {0x10, 0x28, 0x00, 0x38, 0x40, 0x40, 0x38, 0x00, 0x00}

/* Cdotaccent (U+010A) */
#define FIXED6x9_METRICS_266 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_266 {0x10, 0x00, 0x30, 0x48, 0x40, 0x48, 0x30, 0x00, 0x00}

/* cdotaccent (U+010B) */
#define FIXED6x9_METRICS_267 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_267 {0x00, 0x10, 0x00, 0x38, 0x40, 0x40, 0x38, 0x00, 0x00}

/* Ccaron (U+010C) */
#define FIXED6x9_METRICS_268 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_268 {0x28, 0x10, 0x30, 0x48, 0x40, 0x48, 0x30, 0x00, 0x00}

/* ccaron (U+010D) */
#define FIXED6x9_METRICS_269 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_269 {0x28, 0x10, 0x00, 0x38, 0x40, 0x40, 0x38, 0x00, 0x00}

/* Dcaron (U+010E) */
#define FIXED6x9_METRICS_270 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_270 {0x28, 0x10, 0x70, 0x48, 0x48, 0x48, 0x70, 0x00, 0x00}

/* dcaron (U+010F) */
#define FIXED6x9_METRICS_271 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_271 {0x50, 0x28, 0x08, 0x38, 0x48, 0x48, 0x38, 0x00, 0x00}

/* Dcroat (U+0110) */
#define FIXED6x9_METRICS_272 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_272 {0x00, 0x70, 0x48, 0xE8, 0x48, 0x48, 0x70, 0x00, 0x00}

/* dcroat (U+0111) */
#define FIXED6x9_METRICS_273 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_273 {0x00, 0x10, 0x38, 0x70, 0x90, 0x90, 0x70, 0x00, 0x00}

/* Emacron (U+0112) */
#define FIXED6x9_METRICS_274 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_274 {0x78, 0x00, 0x78, 0x40, 0x70, 0x40, 0x78, 0x00, 0x00}

/* emacron (U+0113) */
#define FIXED6x9_METRICS_275 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_275 {0x00, 0x78, 0x00, 0x30, 0x58, 0x60, 0x38, 0x00, 0x00}

/* Ebreve (U+0114) */
#define FIXED6x9_METRICS_276 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_276 {0x48, 0x30, 0x78, 0x40, 0x70, 0x40, 0x78, 0x00, 0x00}

/* ebreve (U+0115) */
#define FIXED6x9_METRICS_277 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_277 {0x48, 0x30, 0x00, 0x30, 0x58, 0x60, 0x38, 0x00, 0x00}

/* Edotaccent (U+0116) */
#define FIXED6x9_METRICS_278 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_278 {0x20, 0x00, 0x78, 0x40, 0x70, 0x40, 0x78, 0x00, 0x00}

/* edotaccent (U+0117) */
#define FIXED6x9_METRICS_279 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_279 {0x00, 0x20, 0x00, 0x30, 0x58, 0x60, 0x38, 0x00, 0x00}

/* Eogonek (U+0118) */
#define FIXED6x9_METRICS_280 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_280 {0x00, 0x78, 0x40, 0x70, 0x40, 0x40, 0x78, 0x20, 0x10}

/* eogonek (U+0119) */
#define FIXED6x9_METRICS_281 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_281 {0x00, 0x00, 0x00, 0x30, 0x58, 0x60, 0x38, 0x20, 0x10}

/* Ecaron (U+011A) */
#define FIXED6x9_METRICS_282 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_282 {0x28, 0x10, 0x78, 0x40, 0x70, 0x40, 0x78, 0x00, 0x00}

/* ecaron (U+011B) */
#define FIXED6x9_METRICS_283 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_283 {0x28, 0x10, 0x00, 0x30, 0x58, 0x60, 0x38, 0x00, 0x00}

/* Gcircumflex (U+011C) */
#define FIXED6x9_METRICS_284 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_284 {0x10, 0x28, 0x38, 0x40, 0x58, 0x48, 0x30, 0x00, 0x00}

/* gcircumflex (U+011D) */
#define FIXED6x9_METRICS_285 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_285 {0x10, 0x28, 0x00, 0x30, 0x48, 0x48, 0x38, 0x08, 0x30}

/* Gbreve (U+011E) */
#define FIXED6x9_METRICS_286 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_286 {0x48, 0x30, 0x38, 0x40, 0x58, 0x48, 0x30, 0x00, 0x00}

/* gbreve (U+011F) */
#define FIXED6x9_METRICS_287 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_287 {0x48, 0x30, 0x00, 0x30, 0x48, 0x48, 0x38, 0x08, 0x30}

/* Gdotaccent (U+0120) */
#define FIXED6x9_METRICS_288 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_288 {0x10, 0x00, 0x38, 0x40, 0x58, 0x48, 0x30, 0x00, 0x00}

/* gdotaccent (U+0121) */
#define FIXED6x9_METRICS_289 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_289 {0x00, 0x10, 0x00, 0x30, 0x48, 0x48, 0x38, 0x08, 0x30}

/* Gcommaaccent (U+0122) */
#define FIXED6x9_METRICS_290 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_290 {0x00, 0x30, 0x48, 0x40, 0x58, 0x48, 0x30, 0x10, 0x20}

/* gcommaaccent (U+0123) */
#define FIXED6x9_METRICS_291 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_291 {0x00, 0x10, 0x20, 0x30, 0x48, 0x48, 0x38, 0x08, 0x30}

/* Hcircumflex (U+0124) */
#define FIXED6x9_METRICS_292 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_292 {0x10, 0x28, 0x00, 0x48, 0x78, 0x48, 0x48, 0x00, 0x00}

/* hcircumflex (U+0125) */
#define FIXED6x9_METRICS_293 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_293 {0x10, 0xA8, 0x80, 0xE0, 0x90, 0x90, 0x90, 0x00, 0x00}

/* Hbar (U+0126) */
#define FIXED6x9_METRICS_294 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_294 {0x00, 0x50, 0xF8, 0x50, 0x70, 0x50, 0x50, 0x00, 0x00}

/* hbar (U+0127) */
#define FIXED6x9_METRICS_295 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_295 {0x00, 0x40, 0xE0, 0x70, 0x48, 0x48, 0x48, 0x00, 0x00}

/* Itilde (U+0128) */
#define FIXED6x9_METRICS_296 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_296 {0x28, 0x50, 0x38, 0x10, 0x10, 0x10, 0x38, 0x00, 0x00}

/* itilde (U+0129) */
#define FIXED6x9_METRICS_297 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_297 {0x28, 0x50, 0x00, 0x30, 0x10, 0x10, 0x38, 0x00, 0x00}

/* Imacron (U+012A) */
#define FIXED6x9_METRICS_298 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_298 {0x70, 0x00, 0x70, 0x20, 0x20, 0x20, 0x70, 0x00, 0x00}

/* imacron (U+012B) */
#define FIXED6x9_METRICS_299 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_299 {0x00, 0x70, 0x00, 0x60, 0x20, 0x20, 0x70, 0x00, 0x00}

/* Ibreve (U+012C) */
#define FIXED6x9_METRICS_300 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_300 {0x48, 0x30, 0x00, 0x38, 0x10, 0x10, 0x38, 0x00, 0x00}

/* ibreve (U+012D) */
#define FIXED6x9_METRICS_301 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_301 {0x48, 0x30, 0x00, 0x30, 0x10, 0x10, 0x38, 0x00, 0x00}

/* Iogonek (U+012E) */
#define FIXED6x9_METRICS_302 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_302 {0x00, 0x70, 0x20, 0x20, 0x20, 0x20, 0x70, 0x20, 0x10}

/* iogonek (U+012F) */
#define FIXED6x9_METRICS_303 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_303 {0x00, 0x20, 0x00, 0x60, 0x20, 0x20, 0x70, 0x20, 0x10}

/* Idotaccent (U+0130) */
#define FIXED6x9_METRICS_304 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_304 {0x20, 0x00, 0x70, 0x20, 0x20, 0x20, 0x70, 0x00, 0x00}

/* dotlessi (U+0131) */
#define FIXED6x9_METRICS_305 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_305 {0x00, 0x00, 0x00, 0x60, 0x20, 0x20, 0x70, 0x00, 0x00}

/* IJ (U+0132) */
#define FIXED6x9_METRICS_306 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_306 {0x00, 0xB8, 0x88, 0x88, 0x88, 0xA8, 0x90, 0x00, 0x00}

/* ij (U+0133) */
#define FIXED6x9_METRICS_307 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_307 {0x00, 0x88, 0x00, 0x98, 0x88, 0x88, 0x88, 0x28, 0x10}

/* Jcircumflex (U+0134) */
#define FIXED6x9_METRICS_308 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_308 {0x10, 0x28, 0x38, 0x10, 0x10, 0x90, 0x60, 0x00, 0x00}

/* jcircumflex (U+0135) */
#define FIXED6x9_METRICS_309 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_309 {0x10, 0x28, 0x00, 0x30, 0x10, 0x10, 0x10, 0x50, 0x20}

/* Kcommaaccent (U+0136) */
#define FIXED6x9_METRICS_310 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_310 {0x00, 0x48, 0x50, 0x60, 0x50, 0x48, 0x48, 0x20, 0x40}

/* kcommaaccent (U+0137) */
#define FIXED6x9_METRICS_311 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_311 {0x00, 0x40, 0x40, 0x50, 0x60, 0x50, 0x48, 0x20, 0x40}

/* kgreenlandic (U+0138) */
#define FIXED6x9_METRICS_312 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_312 {0x00, 0x00, 0x00, 0x58, 0x60, 0x50, 0x48, 0x00, 0x00}

/* Lacute (U+0139) */
#define FIXED6x9_METRICS_313 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_313 {0x08, 0x10, 0x40, 0x40, 0x40, 0x40, 0x78, 0x00, 0x00}

/* lacute (U+013A) */
#define FIXED6x9_METRICS_314 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_314 {0x20, 0x40, 0x60, 0x20, 0x20, 0x20, 0x70, 0x00, 0x00}

/* Lcommaaccent (U+013B) */
#define FIXED6x9_METRICS_315 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_315 {0x00, 0x40, 0x40, 0x40, 0x40, 0x40, 0x78, 0x10, 0x20}

/* lcommaaccent (U+013C) */
#define FIXED6x9_METRICS_316 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_316 {0x00, 0x60, 0x20, 0x20, 0x20, 0x20, 0x70, 0x20, 0x40}

/* Lcaron (U+013D) */
#define FIXED6x9_METRICS_317 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_317 {0x28, 0x10, 0x40, 0x40, 0x40, 0x40, 0x78, 0x00, 0x00}

/* lcaron (U+013E) */
#define FIXED6x9_METRICS_318 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_318 {0x50, 0x20, 0x60, 0x20, 0x20, 0x20, 0x70, 0x00, 0x00}

/* Ldot (U+013F) */
#define FIXED6x9_METRICS_319 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_319 {0x00, 0x40, 0x40, 0x50, 0x40, 0x40, 0x78, 0x00, 0x00}

/* ldot (U+0140) */
#define FIXED6x9_METRICS_320 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_320 {0x00, 0x60, 0x20, 0x28, 0x20, 0x20, 0x70, 0x00, 0x00}

/* Lslash (U+0141) */
#define FIXED6x9_METRICS_321 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_321 {0x00, 0x40, 0x40, 0x60, 0xC0, 0x40, 0x78, 0x00, 0x00}

/* lslash (U+0142) */
#define FIXED6x9_METRICS_322 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_322 {0x00, 0x60, 0x20, 0x30, 0x60, 0x20, 0x70, 0x00, 0x00}

/* Nacute (U+0143) */
#define FIXED6x9_METRICS_323 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_323 {0x10, 0x20, 0x48, 0x68, 0x58, 0x48, 0x48, 0x00, 0x00}

/* nacute (U+0144) */
#define FIXED6x9_METRICS_324 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_324 {0x10, 0x20, 0x00, 0x70, 0x48, 0x48, 0x48, 0x00, 0x00}

/* Ncommaaccent (U+0145) */
#define FIXED6x9_METRICS_325 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_325 {0x00, 0x48, 0x68, 0x58, 0x48, 0x48, 0x48, 0x20, 0x40}

/* ncommaaccent (U+0146) */
#define FIXED6x9_METRICS_326 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_326 {0x00, 0x00, 0x00, 0x70, 0x48, 0x48, 0x48, 0x20, 0x40}

/* Ncaron (U+0147) */
#define FIXED6x9_METRICS_327 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_327 {0x28, 0x10, 0x48, 0x68, 0x58, 0x48, 0x48, 0x00, 0x00}

/* ncaron (U+0148) */
#define FIXED6x9_METRICS_328 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_328 {0x50, 0x20, 0x00, 0x70, 0x48, 0x48, 0x48, 0x00, 0x00}

/* napostrophe (U+0149) */
#define FIXED6x9_METRICS_329 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_329 {0x00, 0xC0, 0x40, 0xB0, 0x28, 0x28, 0x28, 0x00, 0x00}

/* Eng (U+014A) */
#define FIXED6x9_METRICS_330 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_330 {0x00, 0x48, 0x68, 0x58, 0x48, 0x48, 0x48, 0x08, 0x10}

/* eng (U+014B) */
#define FIXED6x9_METRICS_331 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_331 {0x00, 0x00, 0x00, 0x70, 0x48, 0x48, 0x48, 0x08, 0x10}

/* Omacron (U+014C) */
#define FIXED6x9_METRICS_332 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_332 {0xF8, 0x00, 0x70, 0x88, 0x88, 0x88, 0x70, 0x00, 0x00}

/* omacron (U+014D) */
#define FIXED6x9_METRICS_333 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_333 {0x00, 0x78, 0x00, 0x30, 0x48, 0x48, 0x30, 0x00, 0x00}

/* Obreve (U+014E) */
#define FIXED6x9_METRICS_334 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_334 {0x88, 0x70, 0x70, 0x88, 0x88, 0x88, 0x70, 0x00, 0x00}

/* obreve (U+014F) */
#define FIXED6x9_METRICS_335 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_335 {0x48, 0x30, 0x00, 0x30, 0x48, 0x48, 0x30, 0x00, 0x00}

/* Ohungarumlaut (U+0150) */
#define FIXED6x9_METRICS_336 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_336 {0x48, 0x90, 0x70, 0x88, 0x88, 0x88, 0x70, 0x00, 0x00}

/* ohungarumlaut (U+0151) */
#define FIXED6x9_METRICS_337 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_337 {0x48, 0x90, 0x00, 0x60, 0x90, 0x90, 0x60, 0x00, 0x00}

/* OE (U+0152) */
#define FIXED6x9_METRICS_338 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_338 {0x00, 0x78, 0xA0, 0xB0, 0xA0, 0xA0, 0x78, 0x00, 0x00}

/* oe (U+0153) */
#define FIXED6x9_METRICS_339 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_339 {0x00, 0x00, 0x00, 0x50, 0xA8, 0xB0, 0x58, 0x00, 0x00}

/* Racute (U+0154) */
#define FIXED6x9_METRICS_340 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_340 {0x10, 0x20, 0x70, 0x48, 0x70, 0x50, 0x48, 0x00, 0x00}

/* racute (U+0155) */
#define FIXED6x9_METRICS_341 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_341 {0x10, 0x20, 0x00, 0x50, 0x68, 0x40, 0x40, 0x00, 0x00}

/* Rcommaaccent (U+0156) */
#define FIXED6x9_METRICS_342 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_342 {0x00, 0x70, 0x48, 0x48, 0x70, 0x48, 0x48, 0x20, 0x40}

/* rcommaaccent (U+0157) */
#define FIXED6x9_METRICS_343 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_343 {0x00, 0x00, 0x00, 0x50, 0x68, 0x40, 0x40, 0x20, 0x40}

/* Rcaron (U+0158) */
#define FIXED6x9_METRICS_344 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_344 {0x50, 0x20, 0x70, 0x48, 0x70, 0x50, 0x48, 0x00, 0x00}

/* rcaron (U+0159) */
#define FIXED6x9_METRICS_345 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_345 {0x50, 0x20, 0x00, 0x50, 0x68, 0x40, 0x40, 0x00, 0x00}

/* Sacute (U+015A) */
#define FIXED6x9_METRICS_346 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_346 {0x10, 0x20, 0x38, 0x40, 0x30, 0x08, 0x70, 0x00, 0x00}

/* sacute (U+015B) */
#define FIXED6x9_METRICS_347 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_347 {0x10, 0x20, 0x00, 0x38, 0x60, 0x18, 0x70, 0x00, 0x00}

/* Scircumflex (U+015C) */
#define FIXED6x9_METRICS_348 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_348 {0x10, 0x28, 0x38, 0x40, 0x30, 0x08, 0x70, 0x00, 0x00}

/* scircumflex (U+015D) */
#define FIXED6x9_METRICS_349 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_349 {0x10, 0x28, 0x00, 0x38, 0x60, 0x18, 0x70, 0x00, 0x00}

/* Scedilla (U+015E) */
#define FIXED6x9_METRICS_350 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_350 {0x00, 0x30, 0x48, 0x20, 0x10, 0x48, 0x30, 0x10, 0x20}

/* scedilla (U+015F) */
#define FIXED6x9_METRICS_351 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_351 {0x00, 0x00, 0x00, 0x38, 0x60, 0x18, 0x70, 0x10, 0x20}

/* Scaron (U+0160) */
#define FIXED6x9_METRICS_352 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_352 {0x28, 0x10, 0x38, 0x40, 0x30, 0x08, 0x70, 0x00, 0x00}

/* scaron (U+0161) */
#define FIXED6x9_METRICS_353 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_353 {0x28, 0x10, 0x00, 0x38, 0x60, 0x18, 0x70, 0x00, 0x00}

/* Tcommaaccent (U+0162) */
#define FIXED6x9_METRICS_354 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_354 {0x00, 0xF8, 0x20, 0x20, 0x20, 0x20, 0x20, 0x10, 0x20}

/* tcommaaccent (U+0163) */
#define FIXED6x9_METRICS_355 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_355 {0x00, 0x20, 0x20, 0x70, 0x20, 0x28, 0x10, 0x10, 0x20}

/* Tcaron (U+0164) */
#define FIXED6x9_METRICS_356 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_356 {0x50, 0x20, 0xF8, 0x20, 0x20, 0x20, 0x20, 0x00, 0x00}

/* tcaron (U+0165) */
#define FIXED6x9_METRICS_357 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_357 {0x28, 0x10, 0x20, 0x70, 0x20, 0x28, 0x10, 0x00, 0x00}

/* Tbar (U+0166) */
#define FIXED6x9_METRICS_358 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_358 {0x00, 0xF8, 0x20, 0x20, 0x70, 0x20, 0x20, 0x00, 0x00}

/* tbar (U+0167) */
#define FIXED6x9_METRICS_359 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_359 {0x00, 0x20, 0x20, 0x70, 0x70, 0x28, 0x10, 0x00, 0x00}

/* Utilde (U+0168) */
#define FIXED6x9_METRICS_360 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_360 {0x28, 0x50, 0x00, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* utilde (U+0169) */
#define FIXED6x9_METRICS_361 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_361 {0x28, 0x50, 0x00, 0x48, 0x48, 0x48, 0x38, 0x00, 0x00}

/* Umacron (U+016A) */
#define FIXED6x9_METRICS_362 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_362 {0x78, 0x00, 0x48, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* umacron (U+016B) */
#define FIXED6x9_METRICS_363 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_363 {0x00, 0x78, 0x00, 0x48, 0x48, 0x48, 0x38, 0x00, 0x00}

/* Ubreve (U+016C) */
#define FIXED6x9_METRICS_364 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_364 {0x48, 0x30, 0x00, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* ubreve (U+016D) */
#define FIXED6x9_METRICS_365 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_365 {0x48, 0x30, 0x00, 0x48, 0x48, 0x48, 0x38, 0x00, 0x00}

/* Uring (U+016E) */
#define FIXED6x9_METRICS_366 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_366 {0x30, 0x48, 0x30, 0x48, 0x48, 0x48, 0x30, 0x00, 0x00}

/* uring (U+016F) */
#define FIXED6x9_METRICS_367 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_367 {0x30, 0x48, 0x30, 0x48, 0x48, 0x48, 0x38, 0x00, 0x00}

/* Uhungarumlaut (U+0170) */
#define FIXED6x9_METRICS_368 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_368 {0x48, 0x90, 0x00, 0x90, 0x90, 0x90, 0x60, 0x00, 0x00}

/* uhungarumlaut (U+0171) */
#define FIXED6x9_METRICS_369 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_369 {0x48, 0x90, 0x00, 0x90, 0x90, 0x90, 0x70, 0x00, 0x00}

/* Uogonek (U+0172) */
#define FIXED6x9_METRICS_370 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_370 {0x00, 0x48, 0x48, 0x48, 0x48, 0x48, 0x30, 0x20, 0x10}

/* uogonek (U+0173) */
#define FIXED6x9_METRICS_371 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_371 {0x00, 0x00, 0x00, 0x48, 0x48, 0x48, 0x38, 0x20, 0x10}

/* Wcircumflex (U+0174) */
#define FIXED6x9_METRICS_372 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_372 {0x20, 0x50, 0x00, 0x88, 0xA8, 0xD8, 0x88, 0x00, 0x00}

/* wcircumflex (U+0175) */
#define FIXED6x9_METRICS_373 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_373 {0x20, 0x50, 0x00, 0x88, 0xA8, 0xA8, 0x50, 0x00, 0x00}

/* Ycircumflex (U+0176) */
#define FIXED6x9_METRICS_374 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_374 {0x20, 0x50, 0x00, 0x88, 0x50, 0x20, 0x20, 0x00, 0x00}

/* ycircumflex (U+0177) */
#define FIXED6x9_METRICS_375 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_375 {0x10, 0x28, 0x00, 0x48, 0x48, 0x48, 0x38, 0x48, 0x30}

/* Ydieresis (U+0178) */
#define FIXED6x9_METRICS_376 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_376 {0x50, 0x00, 0x88, 0x50, 0x20, 0x20, 0x20, 0x00, 0x00}

/* Zacute (U+0179) */
#define FIXED6x9_METRICS_377 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_377 {0x10, 0x20, 0x78, 0x10, 0x20, 0x40, 0x78, 0x00, 0x00}

/* zacute (U+017A) */
#define FIXED6x9_METRICS_378 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_378 {0x10, 0x20, 0x00, 0x78, 0x10, 0x20, 0x78, 0x00, 0x00}

/* Zdotaccent (U+017B) */
#define FIXED6x9_METRICS_379 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_379 {0x10, 0x00, 0x78, 0x10, 0x20, 0x40, 0x78, 0x00, 0x00}

/* zdotaccent (U+017C) */
#define FIXED6x9_METRICS_380 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_380 {0x00, 0x10, 0x00, 0x78, 0x10, 0x20, 0x78, 0x00, 0x00}

/* Zcaron (U+017D) */
#define FIXED6x9_METRICS_381 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_381 {0x28, 0x10, 0x78, 0x10, 0x20, 0x40, 0x78, 0x00, 0x00}

/* zcaron (U+017E) */
#define FIXED6x9_METRICS_382 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_382 {0x28, 0x10, 0x00, 0x78, 0x10, 0x20, 0x78, 0x00, 0x00}

/* longs (U+017F) */
#define FIXED6x9_METRICS_383 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_383 {0x00, 0x10, 0x28, 0x60, 0x20, 0x20, 0x20, 0x00, 0x00}

/* uni02BB (U+02BB) */
#define FIXED6x9_METRICS_699 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_699 {0x00, 0x10, 0x20, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00}

/* afii57929 (U+02BC) */
#define FIXED6x9_METRICS_700 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_700 {0x00, 0x30, 0x10, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00}

/* afii64937 (U+02BD) */
#define FIXED6x9_METRICS_701 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_701 {0x00, 0x30, 0x20, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00}

/* circumflex (U+02C6) */
#define FIXED6x9_METRICS_710 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_710 {0x20, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* caron (U+02C7) */
#define FIXED6x9_METRICS_711 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_711 {0x50, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* macron (U+02C9) */
#define FIXED6x9_METRICS_713 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_713 {0x00, 0x70, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* breve (U+02D8) */
#define FIXED6x9_METRICS_728 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_728 {0x48, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* dotaccent (U+02D9) */
#define FIXED6x9_METRICS_729 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_729 {0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* ring (U+02DA) */
#define FIXED6x9_METRICS_730 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_730 {0x20, 0x50, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* ogonek (U+02DB) */
#define FIXED6x9_METRICS_731 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_731 {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x10}

/* tilde (U+02DC) */
#define FIXED6x9_METRICS_732 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_732 {0x28, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* hungarumlaut (U+02DD) */
#define FIXED6x9_METRICS_733 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_733 {0x48, 0x90, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* uni2010 (U+2010) */
#define FIXED6x9_METRICS_8208 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8208 {0x00, 0x00, 0x00, 0x00, 0x78, 0x00, 0x00, 0x00, 0x00}

/* uni2011 (U+2011) */
#define FIXED6x9_METRICS_8209 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8209 {0x00, 0x00, 0x00, 0x00, 0x78, 0x00, 0x00, 0x00, 0x00}

/* figuredash (U+2012) */
#define FIXED6x9_METRICS_8210 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8210 {0x00, 0x00, 0x00, 0x00, 0xF8, 0x00, 0x00, 0x00, 0x00}

/* endash (U+2013) */
#define FIXED6x9_METRICS_8211 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8211 {0x00, 0x00, 0x00, 0x00, 0xF8, 0x00, 0x00, 0x00, 0x00}

/* emdash (U+2014) */
#define FIXED6x9_METRICS_8212 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8212 {0x00, 0x00, 0x00, 0x00, 0xFC, 0x00, 0x00, 0x00, 0x00}

/* afii00208 (U+2015) */
#define FIXED6x9_METRICS_8213 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8213 {0x00, 0x00, 0x00, 0x00, 0xFC, 0x00, 0x00, 0x00, 0x00}

/* uni2016 (U+2016) */
#define FIXED6x9_METRICS_8214 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8214 {0x00, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x00, 0x00}

/* underscoredbl (U+2017) */
#define FIXED6x9_METRICS_8215 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8215 {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFC, 0x00, 0xFC}

/* quoteleft (U+2018) */
#define FIXED6x9_METRICS_8216 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8216 {0x00, 0x10, 0x20, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00}

/* quoteright (U+2019) */
#define FIXED6x9_METRICS_8217 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8217 {0x00, 0x30, 0x10, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00}

/* quotesinglbase (U+201A) */
#define FIXED6x9_METRICS_8218 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8218 {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x10, 0x20}

/* quotereversed (U+201B) */
#define FIXED6x9_METRICS_8219 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8219 {0x00, 0x30, 0x20, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00}

/* quotedblleft (U+201C) */
#define FIXED6x9_METRICS_8220 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8220 {0x00, 0x48, 0x90, 0xD8, 0x00, 0x00, 0x00, 0x00, 0x00}

/* quotedblright (U+201D) */
#define FIXED6x9_METRICS_8221 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8221 {0x00, 0xD8, 0x48, 0x90, 0x00, 0x00, 0x00, 0x00, 0x00}

/* quotedblbase (U+201E) */
#define FIXED6x9_METRICS_8222 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8222 {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xD8, 0x48, 0x90}

/* uni201F (U+201F) */
#define FIXED6x9_METRICS_8223 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8223 {0x00, 0xD8, 0x90, 0x48, 0x00, 0x00, 0x00, 0x00, 0x00}

/* dagger (U+2020) */
#define FIXED6x9_METRICS_8224 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8224 {0x00, 0x20, 0x20, 0xF8, 0x20, 0x20, 0x20, 0x20, 0x00}

/* daggerdbl (U+2021) */
#define FIXED6x9_METRICS_8225 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8225 {0x00, 0x20, 0x20, 0xF8, 0x20, 0xF8, 0x20, 0x20, 0x00}

/* bullet (U+2022) */
#define FIXED6x9_METRICS_8226 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8226 {0x00, 0x00, 0x30, 0x78, 0x78, 0x30, 0x00, 0x00, 0x00}

/* uni2023 (U+2023) */
#define FIXED6x9_METRICS_8227 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8227 {0x00, 0x00, 0x40, 0x60, 0x70, 0x60, 0x40, 0x00, 0x00}

/* onedotenleader (U+2024) */
#define FIXED6x9_METRICS_8228 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8228 {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x00, 0x00}

/* twodotenleader (U+2025) */
#define FIXED6x9_METRICS_8229 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8229 {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50, 0x00, 0x00}

/* ellipsis (U+2026) */
#define FIXED6x9_METRICS_8230 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8230 {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA8, 0x00, 0x00}

/* uni2027 (U+2027) */
#define FIXED6x9_METRICS_8231 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8231 {0x00, 0x00, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x00}

/* perthousand (U+2030) */
#define FIXED6x9_METRICS_8240 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8240 {0x40, 0xA8, 0x48, 0x10, 0x20, 0x68, 0xD4, 0x28, 0x00}

/* minute (U+2032) */
#define FIXED6x9_METRICS_8242 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8242 {0x00, 0x10, 0x10, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00}

/* second (U+2033) */
#define FIXED6x9_METRICS_8243 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8243 {0x00, 0x48, 0x48, 0x90, 0x00, 0x00, 0x00, 0x00, 0x00}

/* uni2034 (U+2034) */
#define FIXED6x9_METRICS_8244 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8244 {0x00, 0x54, 0x54, 0xA8, 0x00, 0x00, 0x00, 0x00, 0x00}

/* uni2035 (U+2035) */
#define FIXED6x9_METRICS_8245 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8245 {0x00, 0x20, 0x20, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00}

/* uni2036 (U+2036) */
#define FIXED6x9_METRICS_8246 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8246 {0x00, 0x90, 0x90, 0x48, 0x00, 0x00, 0x00, 0x00, 0x00}

/* uni2037 (U+2037) */
#define FIXED6x9_METRICS_8247 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8247 {0x00, 0xA8, 0xA8, 0x54, 0x00, 0x00, 0x00, 0x00, 0x00}

/* guilsinglleft (U+2039) */
#define FIXED6x9_METRICS_8249 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8249 {0x00, 0x00, 0x10, 0x20, 0x40, 0x20, 0x10, 0x00, 0x00}

/* guilsinglright (U+203A) */
#define FIXED6x9_METRICS_8250 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8250 {0x00, 0x00, 0x40, 0x20, 0x10, 0x20, 0x40, 0x00, 0x00}

/* exclamdbl (U+203C) */
#define FIXED6x9_METRICS_8252 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8252 {0x00, 0x50, 0x50, 0x50, 0x50, 0x00, 0x50, 0x00, 0x00}

/* uni203E (U+203E) */
#define FIXED6x9_METRICS_8254 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8254 {0x00, 0xFC, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

/* fraction (U+2044) */
#define FIXED6x9_METRICS_8260 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8260 {0x00, 0x08, 0x08, 0x10, 0x20, 0x40, 0x40, 0x00, 0x00}

/* uni2102 (U+2102) */
#define FIXED6x9_METRICS_8450 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8450 {0x00, 0x70, 0xA8, 0xA0, 0xA0, 0xA8, 0x70, 0x00, 0x00}

/* afii61248 (U+2105) */
#define FIXED6x9_METRICS_8453 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8453 {0x00, 0x40, 0x80, 0x40, 0x10, 0x28, 0x10, 0x00, 0x00}

/* afii61289 (U+2113) */
#define FIXED6x9_METRICS_8467 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8467 {0x00, 0x10, 0x28, 0x28, 0x30, 0x20, 0x58, 0x00, 0x00}

/* uni2115 (U+2115) */
#define FIXED6x9_METRICS_8469 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8469 {0x00, 0xC8, 0xE8, 0xE8, 0xD8, 0xD8, 0xC8, 0x00, 0x00}

/* afii61352 (U+2116) */
#define FIXED6x9_METRICS_8470 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8470 {0x00, 0x90, 0xD0, 0xD0, 0xB8, 0xB4, 0x98, 0x00, 0x00}

/* uni211A (U+211A) */
#define FIXED6x9_METRICS_8474 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8474 {0x00, 0x70, 0xA8, 0xA8, 0xA8, 0xA8, 0x70, 0x18, 0x00}

/* uni211D (U+211D) */
#define FIXED6x9_METRICS_8477 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8477 {0x00, 0xF0, 0xA8, 0xA8, 0xB0, 0xA8, 0xA8, 0x00, 0x00}

/* trademark (U+2122) */
#define FIXED6x9_METRICS_8482 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8482 {0x00, 0xFC, 0x5C, 0x54, 0x00, 0x00, 0x00, 0x00, 0x00}

/* uni2124 (U+2124) */
#define FIXED6x9_METRICS_8484 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8484 {0x00, 0xF8, 0x28, 0x50, 0x50, 0xA0, 0xF8, 0x00, 0x00}

/* Omega (U+2126) */
#define FIXED6x9_METRICS_8486 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8486 {0x00, 0x70, 0x88, 0x88, 0x88, 0x50, 0xD8, 0x00, 0x00}

/* estimated (U+212E) */
#define FIXED6x9_METRICS_8494 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8494 {0x00, 0x00, 0x70, 0xD8, 0xF8, 0xC0, 0x70, 0x00, 0x00}

/* oneeighth (U+215B) */
#define FIXED6x9_METRICS_8539 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8539 {0x40, 0xC0, 0x40, 0x50, 0x68, 0x10, 0x28, 0x10, 0x00}

/* threeeighths (U+215C) */
#define FIXED6x9_METRICS_8540 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8540 {0xC0, 0x20, 0x40, 0x30, 0xE8, 0x10, 0x28, 0x10, 0x00}

/* fiveeighths (U+215D) */
#define FIXED6x9_METRICS_8541 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8541 {0xE0, 0x80, 0xC0, 0x30, 0xE8, 0x10, 0x28, 0x10, 0x00}

/* seveneighths (U+215E) */
#define FIXED6x9_METRICS_8542 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8542 {0xE0, 0x20, 0x40, 0x50, 0x68, 0x10, 0x28, 0x10, 0x00}

/* arrowleft (U+2190) */
#define FIXED6x9_METRICS_8592 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8592 {0x00, 0x00, 0x20, 0x40, 0xF8, 0x40, 0x20, 0x00, 0x00}

/* arrowup (U+2191) */
#define FIXED6x9_METRICS_8593 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8593 {0x00, 0x20, 0x70, 0xA8, 0x20, 0x20, 0x20, 0x00, 0x00}

/* arrowright (U+2192) */
#define FIXED6x9_METRICS_8594 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8594 {0x00, 0x00, 0x20, 0x10, 0xF8, 0x10, 0x20, 0x00, 0x00}

/* arrowdown (U+2193) */
#define FIXED6x9_METRICS_8595 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8595 {0x00, 0x20, 0x20, 0x20, 0xA8, 0x70, 0x20, 0x00, 0x00}

/* arrowboth (U+2194) */
#define FIXED6x9_METRICS_8596 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8596 {0x00, 0x00, 0x00, 0x48, 0xFC, 0x48, 0x00, 0x00, 0x00}

/* arrowupdn (U+2195) */
#define FIXED6x9_METRICS_8597 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8597 {0x00, 0x20, 0x70, 0xA8, 0x20, 0xA8, 0x70, 0x20, 0x00}

/* uni21A4 (U+21A4) */
#define FIXED6x9_METRICS_8612 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8612 {0x00, 0x00, 0x20, 0x48, 0xF8, 0x48, 0x20, 0x00, 0x00}

/* uni21A5 (U+21A5) */
#define FIXED6x9_METRICS_8613 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8613 {0x00, 0x20, 0x70, 0xA8, 0x20, 0x20, 0x70, 0x00, 0x00}

/* uni21A6 (U+21A6) */
#define FIXED6x9_METRICS_8614 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8614 {0x00, 0x00, 0x20, 0x90, 0xF8, 0x90, 0x20, 0x00, 0x00}

/* uni21A7 (U+21A7) */
#define FIXED6x9_METRICS_8615 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8615 {0x00, 0x70, 0x20, 0x20, 0xA8, 0x70, 0x20, 0x00, 0x00}

/* arrowupdnbse (U+21A8) */
#define FIXED6x9_METRICS_8616 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8616 {0x20, 0x70, 0xA8, 0x20, 0xA8, 0x70, 0x20, 0xF8, 0x00}

/* arrowdblleft (U+21D0) */
#define FIXED6x9_METRICS_8656 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8656 {0x00, 0x00, 0x20, 0x78, 0x80, 0x78, 0x20, 0x00, 0x00}

/* arrowdblup (U+21D1) */
#define FIXED6x9_METRICS_8657 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8657 {0x00, 0x20, 0x50, 0xD8, 0x50, 0x50, 0x50, 0x00, 0x00}

/* arrowdblright (U+21D2) */
#define FIXED6x9_METRICS_8658 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8658 {0x00, 0x00, 0x20, 0xF0, 0x08, 0xF0, 0x20, 0x00, 0x00}

/* arrowdbldown (U+21D3) */
#define FIXED6x9_METRICS_8659 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8659 {0x00, 0x50, 0x50, 0x50, 0xD8, 0x70, 0x20, 0x00, 0x00}

/* arrowdblboth (U+21D4) */
#define FIXED6x9_METRICS_8660 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8660 {0x00, 0x00, 0x30, 0x78, 0x84, 0x78, 0x30, 0x00, 0x00}

/* uni21D5 (U+21D5) */
#define FIXED6x9_METRICS_8661 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_8661 {0x00, 0x20, 0x70, 0xD8, 0x50, 0xD8, 0x70, 0x20, 0x00}

/* SF100000 (U+2500) */
#define FIXED6x9_METRICS_9472 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9472 {0x00, 0x00, 0x00, 0x00, 0xFC, 0x00, 0x00, 0x00, 0x00}

/* uni2501 (U+2501) */
#define FIXED6x9_METRICS_9473 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9473 {0x00, 0x00, 0x00, 0xFC, 0xFC, 0x00, 0x00, 0x00, 0x00}

/* SF110000 (U+2502) */
#define FIXED6x9_METRICS_9474 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9474 {0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10}

/* uni2503 (U+2503) */
#define FIXED6x9_METRICS_9475 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9475 {0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30}

/* SF010000 (U+250C) */
#define FIXED6x9_METRICS_9484 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9484 {0x00, 0x00, 0x00, 0x00, 0x1C, 0x10, 0x10, 0x10, 0x10}

/* SF030000 (U+2510) */
#define FIXED6x9_METRICS_9488 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9488 {0x00, 0x00, 0x00, 0x00, 0xF0, 0x10, 0x10, 0x10, 0x10}

/* SF020000 (U+2514) */
#define FIXED6x9_METRICS_9492 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9492 {0x10, 0x10, 0x10, 0x10, 0x1C, 0x00, 0x00, 0x00, 0x00}

/* SF040000 (U+2518) */
#define FIXED6x9_METRICS_9496 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9496 {0x10, 0x10, 0x10, 0x10, 0xF0, 0x00, 0x00, 0x00, 0x00}

/* SF080000 (U+251C) */
#define FIXED6x9_METRICS_9500 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9500 {0x10, 0x10, 0x10, 0x10, 0x1C, 0x10, 0x10, 0x10, 0x10}

/* SF090000 (U+2524) */
#define FIXED6x9_METRICS_9508 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9508 {0x10, 0x10, 0x10, 0x10, 0xF0, 0x10, 0x10, 0x10, 0x10}

/* SF060000 (U+252C) */
#define FIXED6x9_METRICS_9516 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9516 {0x00, 0x00, 0x00, 0x00, 0xFC, 0x10, 0x10, 0x10, 0x10}

/* SF070000 (U+2534) */
#define FIXED6x9_METRICS_9524 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9524 {0x10, 0x10, 0x10, 0x10, 0xFC, 0x00, 0x00, 0x00, 0x00}

/* SF050000 (U+253C) */
#define FIXED6x9_METRICS_9532 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9532 {0x10, 0x10, 0x10, 0x10, 0xFC, 0x10, 0x10, 0x10, 0x10}

/* uni254C (U+254C) */
#define FIXED6x9_METRICS_9548 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9548 {0x00, 0x00, 0x00, 0x00, 0xD8, 0x00, 0x00, 0x00, 0x00}

/* uni254D (U+254D) */
#define FIXED6x9_METRICS_9549 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9549 {0x00, 0x00, 0x00, 0xD8, 0xD8, 0x00, 0x00, 0x00, 0x00}

/* uni254E (U+254E) */
#define FIXED6x9_METRICS_9550 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9550 {0x10, 0x10, 0x10, 0x10, 0x00, 0x10, 0x10, 0x10, 0x00}

/* uni254F (U+254F) */
#define FIXED6x9_METRICS_9551 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9551 {0x30, 0x30, 0x30, 0x30, 0x00, 0x30, 0x30, 0x30, 0x00}

/* SF430000 (U+2550) */
#define FIXED6x9_METRICS_9552 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9552 {0x00, 0x00, 0x00, 0xFC, 0x00, 0xFC, 0x00, 0x00, 0x00}

/* SF240000 (U+2551) */
#define FIXED6x9_METRICS_9553 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9553 {0x28, 0x28, 0x28, 0x28, 0x28, 0x28, 0x28, 0x28, 0x28}

/* SF510000 (U+2552) */
#define FIXED6x9_METRICS_9554 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9554 {0x00, 0x00, 0x00, 0x1C, 0x10, 0x1C, 0x10, 0x10, 0x10}

/* SF520000 (U+2553) */
#define FIXED6x9_METRICS_9555 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9555 {0x00, 0x00, 0x00, 0x00, 0x3C, 0x28, 0x28, 0x28, 0x28}

/* SF390000 (U+2554) */
#define FIXED6x9_METRICS_9556 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9556 {0x00, 0x00, 0x00, 0x3C, 0x20, 0x2C, 0x28, 0x28, 0x28}

/* SF220000 (U+2555) */
#define FIXED6x9_METRICS_9557 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9557 {0x00, 0x00, 0x00, 0xF0, 0x10, 0xF0, 0x10, 0x10, 0x10}

/* SF210000 (U+2556) */
#define FIXED6x9_METRICS_9558 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9558 {0x00, 0x00, 0x00, 0x00, 0xF8, 0x28, 0x28, 0x28, 0x28}

/* SF250000 (U+2557) */
#define FIXED6x9_METRICS_9559 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9559 {0x00, 0x00, 0x00, 0xF8, 0x08, 0xE8, 0x28, 0x28, 0x28}

/* SF500000 (U+2558) */
#define FIXED6x9_METRICS_9560 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9560 {0x10, 0x10, 0x10, 0x1C, 0x10, 0x1C, 0x00, 0x00, 0x00}

/* SF490000 (U+2559) */
#define FIXED6x9_METRICS_9561 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9561 {0x28, 0x28, 0x28, 0x28, 0x3C, 0x00, 0x00, 0x00, 0x00}

/* SF380000 (U+255A) */
#define FIXED6x9_METRICS_9562 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9562 {0x28, 0x28, 0x28, 0x2C, 0x20, 0x3C, 0x00, 0x00, 0x00}

/* SF280000 (U+255B) */
#define FIXED6x9_METRICS_9563 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9563 {0x10, 0x10, 0x10, 0xF0, 0x10, 0xF0, 0x00, 0x00, 0x00}

/* SF270000 (U+255C) */
#define FIXED6x9_METRICS_9564 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9564 {0x28, 0x28, 0x28, 0x28, 0xF8, 0x00, 0x00, 0x00, 0x00}

/* SF260000 (U+255D) */
#define FIXED6x9_METRICS_9565 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9565 {0x28, 0x28, 0x28, 0xE8, 0x08, 0xF8, 0x00, 0x00, 0x00}

/* SF360000 (U+255E) */
#define FIXED6x9_METRICS_9566 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9566 {0x10, 0x10, 0x10, 0x1C, 0x10, 0x1C, 0x10, 0x10, 0x10}

/* SF370000 (U+255F) */
#define FIXED6x9_METRICS_9567 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9567 {0x28, 0x28, 0x28, 0x28, 0x2C, 0x28, 0x28, 0x28, 0x28}

/* SF420000 (U+2560) */
#define FIXED6x9_METRICS_9568 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9568 {0x28, 0x28, 0x28, 0x2C, 0x20, 0x2C, 0x28, 0x28, 0x28}

/* SF190000 (U+2561) */
#define FIXED6x9_METRICS_9569 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9569 {0x10, 0x10, 0x10, 0xF0, 0x10, 0xF0, 0x10, 0x10, 0x10}

/* SF200000 (U+2562) */
#define FIXED6x9_METRICS_9570 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9570 {0x28, 0x28, 0x28, 0x28, 0xE8, 0x28, 0x28, 0x28, 0x28}

/* SF230000 (U+2563) */
#define FIXED6x9_METRICS_9571 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9571 {0x28, 0x28, 0x28, 0xE8, 0x08, 0xE8, 0x28, 0x28, 0x28}

/* SF470000 (U+2564) */
#define FIXED6x9_METRICS_9572 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9572 {0x00, 0x00, 0x00, 0xFC, 0x00, 0xFC, 0x10, 0x10, 0x10}

/* SF480000 (U+2565) */
#define FIXED6x9_METRICS_9573 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9573 {0x00, 0x00, 0x00, 0x00, 0xFC, 0x28, 0x28, 0x28, 0x28}

/* SF410000 (U+2566) */
#define FIXED6x9_METRICS_9574 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9574 {0x00, 0x00, 0x00, 0xFC, 0x00, 0xEC, 0x28, 0x28, 0x28}

/* SF450000 (U+2567) */
#define FIXED6x9_METRICS_9575 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9575 {0x10, 0x10, 0x10, 0xFC, 0x00, 0xFC, 0x00, 0x00, 0x00}

/* SF460000 (U+2568) */
#define FIXED6x9_METRICS_9576 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9576 {0x28, 0x28, 0x28, 0x28, 0xFC, 0x00, 0x00, 0x00, 0x00}

/* SF400000 (U+2569) */
#define FIXED6x9_METRICS_9577 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9577 {0x28, 0x28, 0x28, 0xEC, 0x00, 0xFC, 0x00, 0x00, 0x00}

/* SF540000 (U+256A) */
#define FIXED6x9_METRICS_9578 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9578 {0x10, 0x10, 0x10, 0xFC, 0x10, 0xFC, 0x10, 0x10, 0x10}

/* SF530000 (U+256B) */
#define FIXED6x9_METRICS_9579 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9579 {0x28, 0x28, 0x28, 0x28, 0xFC, 0x28, 0x28, 0x28, 0x28}

/* SF440000 (U+256C) */
#define FIXED6x9_METRICS_9580 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9580 {0x28, 0x28, 0x28, 0xEC, 0x00, 0xEC, 0x28, 0x28, 0x28}

/* uni256D (U+256D) */
#define FIXED6x9_METRICS_9581 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9581 {0x00, 0x00, 0x00, 0x00, 0x04, 0x08, 0x10, 0x10, 0x10}

/* uni256E (U+256E) */
#define FIXED6x9_METRICS_9582 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9582 {0x00, 0x00, 0x00, 0x00, 0xC0, 0x20, 0x10, 0x10, 0x10}

/* uni256F (U+256F) */
#define FIXED6x9_METRICS_9583 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9583 {0x10, 0x10, 0x10, 0x20, 0xC0, 0x00, 0x00, 0x00, 0x00}

/* uni2570 (U+2570) */
#define FIXED6x9_METRICS_9584 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9584 {0x10, 0x10, 0x10, 0x08, 0x04, 0x00, 0x00, 0x00, 0x00}

/* uni2571 (U+2571) */
#define FIXED6x9_METRICS_9585 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9585 {0x04, 0x04, 0x08, 0x10, 0x10, 0x20, 0x40, 0x40, 0x80}

/* uni2572 (U+2572) */
#define FIXED6x9_METRICS_9586 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9586 {0x80, 0x80, 0x40, 0x20, 0x20, 0x10, 0x08, 0x08, 0x04}

/* uni2573 (U+2573) */
#define FIXED6x9_METRICS_9587 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9587 {0x84, 0x84, 0x48, 0x30, 0x30, 0x30, 0x48, 0x48, 0x84}

/* filledbox (U+25A0) */
#define FIXED6x9_METRICS_9632 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9632 {0x00, 0x00, 0xF8, 0xF8, 0xF8, 0xF8, 0xF8, 0x00, 0x00}

/* H22073 (U+25A1) */
#define FIXED6x9_METRICS_9633 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9633 {0x00, 0x00, 0xF8, 0x88, 0x88, 0x88, 0xF8, 0x00, 0x00}

/* uni25A2 (U+25A2) */
#define FIXED6x9_METRICS_9634 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9634 {0x00, 0x00, 0x70, 0x88, 0x88, 0x88, 0x70, 0x00, 0x00}

/* uni25A3 (U+25A3) */
#define FIXED6x9_METRICS_9635 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9635 {0x00, 0x00, 0xF8, 0x88, 0xA8, 0x88, 0xF8, 0x00, 0x00}

/* H18543 (U+25AA) */
#define FIXED6x9_METRICS_9642 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9642 {0x00, 0x00, 0x00, 0x70, 0x70, 0x70, 0x00, 0x00, 0x00}

/* H18551 (U+25AB) */
#define FIXED6x9_METRICS_9643 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9643 {0x00, 0x00, 0x00, 0x70, 0x50, 0x70, 0x00, 0x00, 0x00}

/* filledrect (U+25AC) */
#define FIXED6x9_METRICS_9644 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9644 {0x00, 0x00, 0x00, 0xF8, 0xF8, 0xF8, 0x00, 0x00, 0x00}

/* uni25AD (U+25AD) */
#define FIXED6x9_METRICS_9645 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9645 {0x00, 0x00, 0x00, 0xF8, 0x88, 0xF8, 0x00, 0x00, 0x00}

/* uni25AE (U+25AE) */
#define FIXED6x9_METRICS_9646 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9646 {0x00, 0x78, 0x78, 0x78, 0x78, 0x78, 0x78, 0x00, 0x00}

/* uni25AF (U+25AF) */
#define FIXED6x9_METRICS_9647 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9647 {0x00, 0x78, 0x48, 0x48, 0x48, 0x48, 0x78, 0x00, 0x00}

/* uni25B0 (U+25B0) */
#define FIXED6x9_METRICS_9648 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9648 {0x00, 0x00, 0x00, 0x78, 0xF8, 0xF0, 0x00, 0x00, 0x00}

/* uni25B1 (U+25B1) */
#define FIXED6x9_METRICS_9649 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9649 {0x00, 0x00, 0x00, 0x78, 0x88, 0xF0, 0x00, 0x00, 0x00}

/* triagup (U+25B2) */
#define FIXED6x9_METRICS_9650 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9650 {0x00, 0x20, 0x20, 0x70, 0x70, 0xF8, 0xF8, 0x00, 0x00}

/* uni25B3 (U+25B3) */
#define FIXED6x9_METRICS_9651 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9651 {0x00, 0x20, 0x20, 0x50, 0x50, 0x88, 0xF8, 0x00, 0x00}

/* uni25B4 (U+25B4) */
#define FIXED6x9_METRICS_9652 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9652 {0x00, 0x00, 0x20, 0x20, 0x70, 0x70, 0x00, 0x00, 0x00}

/* uni25B5 (U+25B5) */
#define FIXED6x9_METRICS_9653 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9653 {0x00, 0x00, 0x20, 0x20, 0x50, 0x70, 0x00, 0x00, 0x00}

/* uni25B6 (U+25B6) */
#define FIXED6x9_METRICS_9654 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9654 {0x00, 0x00, 0xC0, 0xF0, 0xF8, 0xF0, 0xC0, 0x00, 0x00}

/* uni25B7 (U+25B7) */
#define FIXED6x9_METRICS_9655 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9655 {0x00, 0x00, 0xC0, 0xB0, 0x88, 0xB0, 0xC0, 0x00, 0x00}

/* uni25B8 (U+25B8) */
#define FIXED6x9_METRICS_9656 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9656 {0x00, 0x00, 0x00, 0x60, 0x78, 0x60, 0x00, 0x00, 0x00}

/* uni25B9 (U+25B9) */
#define FIXED6x9_METRICS_9657 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9657 {0x00, 0x00, 0x00, 0x60, 0x58, 0x60, 0x00, 0x00, 0x00}

/* triagrt (U+25BA) */
#define FIXED6x9_METRICS_9658 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9658 {0x00, 0x00, 0x00, 0xE0, 0xF8, 0xE0, 0x00, 0x00, 0x00}

/* uni25BB (U+25BB) */
#define FIXED6x9_METRICS_9659 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9659 {0x00, 0x00, 0x00, 0xE0, 0x98, 0xE0, 0x00, 0x00, 0x00}

/* triagdn (U+25BC) */
#define FIXED6x9_METRICS_9660 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9660 {0x00, 0xF8, 0xF8, 0x70, 0x70, 0x20, 0x20, 0x00, 0x00}

/* uni25BD (U+25BD) */
#define FIXED6x9_METRICS_9661 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9661 {0x00, 0xF8, 0x88, 0x50, 0x50, 0x20, 0x20, 0x00, 0x00}

/* uni25BE (U+25BE) */
#define FIXED6x9_METRICS_9662 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9662 {0x00, 0x00, 0x70, 0x70, 0x20, 0x20, 0x00, 0x00, 0x00}

/* uni25BF (U+25BF) */
#define FIXED6x9_METRICS_9663 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9663 {0x00, 0x00, 0x70, 0x50, 0x20, 0x20, 0x00, 0x00, 0x00}

/* uni25C0 (U+25C0) */
#define FIXED6x9_METRICS_9664 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9664 {0x00, 0x00, 0x18, 0x78, 0xF8, 0x78, 0x18, 0x00, 0x00}

/* uni25C1 (U+25C1) */
#define FIXED6x9_METRICS_9665 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9665 {0x00, 0x00, 0x18, 0x68, 0x88, 0x68, 0x18, 0x00, 0x00}

/* uni25C2 (U+25C2) */
#define FIXED6x9_METRICS_9666 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9666 {0x00, 0x00, 0x00, 0x18, 0x78, 0x18, 0x00, 0x00, 0x00}

/* uni25C3 (U+25C3) */
#define FIXED6x9_METRICS_9667 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9667 {0x00, 0x00, 0x00, 0x18, 0x68, 0x18, 0x00, 0x00, 0x00}

/* triaglf (U+25C4) */
#define FIXED6x9_METRICS_9668 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9668 {0x00, 0x00, 0x00, 0x38, 0xF8, 0x38, 0x00, 0x00, 0x00}

/* uni25C5 (U+25C5) */
#define FIXED6x9_METRICS_9669 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9669 {0x00, 0x00, 0x00, 0x38, 0xC8, 0x38, 0x00, 0x00, 0x00}

/* uni25C6 (U+25C6) */
#define FIXED6x9_METRICS_9670 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9670 {0x00, 0x00, 0x30, 0x78, 0xFC, 0x78, 0x30, 0x00, 0x00}

/* lozenge (U+25CA) */
#define FIXED6x9_METRICS_9674 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9674 {0x00, 0x20, 0x50, 0x50, 0x88, 0x50, 0x50, 0x20, 0x00}

/* circle (U+25CB) */
#define FIXED6x9_METRICS_9675 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9675 {0x00, 0x00, 0x70, 0x88, 0x88, 0x88, 0x70, 0x00, 0x00}

/* H18533 (U+25CF) */
#define FIXED6x9_METRICS_9679 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9679 {0x00, 0x00, 0x70, 0xF8, 0xF8, 0xF8, 0x70, 0x00, 0x00}

/* invbullet (U+25D8) */
#define FIXED6x9_METRICS_9688 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9688 {0xFC, 0xFC, 0xCC, 0x84, 0x84, 0xCC, 0xFC, 0xFC, 0xFC}

/* invcircle (U+25D9) */
#define FIXED6x9_METRICS_9689 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9689 {0xFC, 0xFC, 0xCC, 0xB4, 0xB4, 0xCC, 0xFC, 0xFC, 0xFC}

/* openbullet (U+25E6) */
#define FIXED6x9_METRICS_9702 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9702 {0x00, 0x00, 0x30, 0x48, 0x48, 0x30, 0x00, 0x00, 0x00}

/* uni2600 (U+2600) */
#define FIXED6x9_METRICS_9728 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9728 {0x00, 0x20, 0xA8, 0x70, 0xF8, 0x70, 0xA8, 0x20, 0x00}

/* uni2639 (U+2639) */
#define FIXED6x9_METRICS_9785 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9785 {0x78, 0x84, 0xCC, 0x84, 0xB4, 0xCC, 0x84, 0x78, 0x00}

/* smileface (U+263A) */
#define FIXED6x9_METRICS_9786 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9786 {0x78, 0x84, 0xCC, 0x84, 0xCC, 0xB4, 0x84, 0x78, 0x00}

/* invsmileface (U+263B) */
#define FIXED6x9_METRICS_9787 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9787 {0x78, 0xFC, 0xB4, 0xFC, 0xB4, 0xCC, 0xFC, 0x78, 0x00}

/* sun (U+263C) */
#define FIXED6x9_METRICS_9788 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9788 {0x00, 0x20, 0xA8, 0x70, 0xD8, 0x70, 0xA8, 0x20, 0x00}

/* uni263F (U+263F) */
#define FIXED6x9_METRICS_9791 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9791 {0x88, 0x70, 0x88, 0x88, 0x70, 0x20, 0xF8, 0x20, 0x00}

/* female (U+2640) */
#define FIXED6x9_METRICS_9792 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9792 {0x00, 0x70, 0x88, 0x88, 0x70, 0x20, 0xF8, 0x20, 0x00}

/* uni2641 (U+2641) */
#define FIXED6x9_METRICS_9793 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9793 {0x00, 0x20, 0xF8, 0x20, 0x70, 0x88, 0x88, 0x70, 0x00}

/* male (U+2642) */
#define FIXED6x9_METRICS_9794 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9794 {0x00, 0x1C, 0x0C, 0x14, 0x70, 0x88, 0x88, 0x70, 0x00}

/* spade (U+2660) */
#define FIXED6x9_METRICS_9824 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9824 {0x00, 0x20, 0x70, 0xF8, 0xF8, 0x20, 0x70, 0x00, 0x00}

/* uni2661 (U+2661) */
#define FIXED6x9_METRICS_9825 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9825 {0x00, 0x50, 0xA8, 0x88, 0x88, 0x50, 0x20, 0x00, 0x00}

/* uni2662 (U+2662) */
#define FIXED6x9_METRICS_9826 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9826 {0x00, 0x00, 0x20, 0x50, 0x88, 0x50, 0x20, 0x00, 0x00}

/* club (U+2663) */
#define FIXED6x9_METRICS_9827 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9827 {0x20, 0x70, 0xA8, 0xF8, 0xA8, 0x20, 0x70, 0x00, 0x00}

/* uni2664 (U+2664) */
#define FIXED6x9_METRICS_9828 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9828 {0x00, 0x20, 0x50, 0x88, 0xF8, 0x20, 0x70, 0x00, 0x00}

/* heart (U+2665) */
#define FIXED6x9_METRICS_9829 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9829 {0x00, 0x50, 0xF8, 0xF8, 0xF8, 0x70, 0x20, 0x00, 0x00}

/* diamond (U+2666) */
#define FIXED6x9_METRICS_9830 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9830 {0x00, 0x00, 0x20, 0x70, 0xF8, 0x70, 0x20, 0x00, 0x00}

/* uni2669 (U+2669) */
#define FIXED6x9_METRICS_9833 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9833 {0x00, 0x10, 0x10, 0x10, 0x10, 0x70, 0x60, 0x00, 0x00}

/* musicalnote (U+266A) */
#define FIXED6x9_METRICS_9834 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9834 {0x00, 0x30, 0x28, 0x20, 0x20, 0xE0, 0xC0, 0x00, 0x00}

/* musicalnotedbl (U+266B) */
#define FIXED6x9_METRICS_9835 {1, 6, 9, 0, 0, 0}
#define FIXED6x9_BITMAP_9835 {0x00, 0x60, 0x58, 0x48, 0xC8, 0xD8, 0x18, 0x00, 0x00}

typedef uint8_t glyph_fixed6x9_row_t;

#define FIXED6x9_COUNT (510)

typedef struct glyph_data_s
{
  uint32_t stride   : 3;    /* Width of one font row in bytes */
  uint32_t width    : 6;    /* Width of the font in bits */
  uint32_t height   : 6;    /* Height of the font in rows */
  uint32_t xoffset  : 6;    /* Top, left-hand corner X-offset in pixels */
  uint32_t yoffset  : 6;    /* Top, left-hand corner y-offset in pixels */
  uint32_t unused   : 5;
} glyph_data_t;

typedef struct glyph_s
{
  int encoding;
  glyph_data_t data;
  glyph_fixed6x9_row_t bitmap[FIXED6x9_MAXHEIGHT];
} glyph_fixed6x9_t;

glyph_fixed6x9_t fixed6x9_glyphs[510] = {
  {0x0020, FIXED6x9_METRICS_32, FIXED6x9_BITMAP_32},
  {0x0021, FIXED6x9_METRICS_33, FIXED6x9_BITMAP_33},
  {0x0022, FIXED6x9_METRICS_34, FIXED6x9_BITMAP_34},
  {0x0023, FIXED6x9_METRICS_35, FIXED6x9_BITMAP_35},
  {0x0024, FIXED6x9_METRICS_36, FIXED6x9_BITMAP_36},
  {0x0025, FIXED6x9_METRICS_37, FIXED6x9_BITMAP_37},
  {0x0026, FIXED6x9_METRICS_38, FIXED6x9_BITMAP_38},
  {0x0027, FIXED6x9_METRICS_39, FIXED6x9_BITMAP_39},
  {0x0028, FIXED6x9_METRICS_40, FIXED6x9_BITMAP_40},
  {0x0029, FIXED6x9_METRICS_41, FIXED6x9_BITMAP_41},
  {0x002a, FIXED6x9_METRICS_42, FIXED6x9_BITMAP_42},
  {0x002b, FIXED6x9_METRICS_43, FIXED6x9_BITMAP_43},
  {0x002c, FIXED6x9_METRICS_44, FIXED6x9_BITMAP_44},
  {0x002d, FIXED6x9_METRICS_45, FIXED6x9_BITMAP_45},
  {0x002e, FIXED6x9_METRICS_46, FIXED6x9_BITMAP_46},
  {0x002f, FIXED6x9_METRICS_47, FIXED6x9_BITMAP_47},
  {0x0030, FIXED6x9_METRICS_48, FIXED6x9_BITMAP_48},
  {0x0031, FIXED6x9_METRICS_49, FIXED6x9_BITMAP_49},
  {0x0032, FIXED6x9_METRICS_50, FIXED6x9_BITMAP_50},
  {0x0033, FIXED6x9_METRICS_51, FIXED6x9_BITMAP_51},
  {0x0034, FIXED6x9_METRICS_52, FIXED6x9_BITMAP_52},
  {0x0035, FIXED6x9_METRICS_53, FIXED6x9_BITMAP_53},
  {0x0036, FIXED6x9_METRICS_54, FIXED6x9_BITMAP_54},
  {0x0037, FIXED6x9_METRICS_55, FIXED6x9_BITMAP_55},
  {0x0038, FIXED6x9_METRICS_56, FIXED6x9_BITMAP_56},
  {0x0039, FIXED6x9_METRICS_57, FIXED6x9_BITMAP_57},
  {0x003a, FIXED6x9_METRICS_58, FIXED6x9_BITMAP_58},
  {0x003b, FIXED6x9_METRICS_59, FIXED6x9_BITMAP_59},
  {0x003c, FIXED6x9_METRICS_60, FIXED6x9_BITMAP_60},
  {0x003d, FIXED6x9_METRICS_61, FIXED6x9_BITMAP_61},
  {0x003e, FIXED6x9_METRICS_62, FIXED6x9_BITMAP_62},
  {0x003f, FIXED6x9_METRICS_63, FIXED6x9_BITMAP_63},
  {0x0040, FIXED6x9_METRICS_64, FIXED6x9_BITMAP_64},
  {0x0041, FIXED6x9_METRICS_65, FIXED6x9_BITMAP_65},
  {0x0042, FIXED6x9_METRICS_66, FIXED6x9_BITMAP_66},
  {0x0043, FIXED6x9_METRICS_67, FIXED6x9_BITMAP_67},
  {0x0044, FIXED6x9_METRICS_68, FIXED6x9_BITMAP_68},
  {0x0045, FIXED6x9_METRICS_69, FIXED6x9_BITMAP_69},
  {0x0046, FIXED6x9_METRICS_70, FIXED6x9_BITMAP_70},
  {0x0047, FIXED6x9_METRICS_71, FIXED6x9_BITMAP_71},
  {0x0048, FIXED6x9_METRICS_72, FIXED6x9_BITMAP_72},
  {0x0049, FIXED6x9_METRICS_73, FIXED6x9_BITMAP_73},
  {0x004a, FIXED6x9_METRICS_74, FIXED6x9_BITMAP_74},
  {0x004b, FIXED6x9_METRICS_75, FIXED6x9_BITMAP_75},
  {0x004c, FIXED6x9_METRICS_76, FIXED6x9_BITMAP_76},
  {0x004d, FIXED6x9_METRICS_77, FIXED6x9_BITMAP_77},
  {0x004e, FIXED6x9_METRICS_78, FIXED6x9_BITMAP_78},
  {0x004f, FIXED6x9_METRICS_79, FIXED6x9_BITMAP_79},
  {0x0050, FIXED6x9_METRICS_80, FIXED6x9_BITMAP_80},
  {0x0051, FIXED6x9_METRICS_81, FIXED6x9_BITMAP_81},
  {0x0052, FIXED6x9_METRICS_82, FIXED6x9_BITMAP_82},
  {0x0053, FIXED6x9_METRICS_83, FIXED6x9_BITMAP_83},
  {0x0054, FIXED6x9_METRICS_84, FIXED6x9_BITMAP_84},
  {0x0055, FIXED6x9_METRICS_85, FIXED6x9_BITMAP_85},
  {0x0056, FIXED6x9_METRICS_86, FIXED6x9_BITMAP_86},
  {0x0057, FIXED6x9_METRICS_87, FIXED6x9_BITMAP_87},
  {0x0058, FIXED6x9_METRICS_88, FIXED6x9_BITMAP_88},
  {0x0059, FIXED6x9_METRICS_89, FIXED6x9_BITMAP_89},
  {0x005a, FIXED6x9_METRICS_90, FIXED6x9_BITMAP_90},
  {0x005b, FIXED6x9_METRICS_91, FIXED6x9_BITMAP_91},
  {0x005c, FIXED6x9_METRICS_92, FIXED6x9_BITMAP_92},
  {0x005d, FIXED6x9_METRICS_93, FIXED6x9_BITMAP_93},
  {0x005e, FIXED6x9_METRICS_94, FIXED6x9_BITMAP_94},
  {0x005f, FIXED6x9_METRICS_95, FIXED6x9_BITMAP_95},
  {0x0060, FIXED6x9_METRICS_96, FIXED6x9_BITMAP_96},
  {0x0061, FIXED6x9_METRICS_97, FIXED6x9_BITMAP_97},
  {0x0062, FIXED6x9_METRICS_98, FIXED6x9_BITMAP_98},
  {0x0063, FIXED6x9_METRICS_99, FIXED6x9_BITMAP_99},
  {0x0064, FIXED6x9_METRICS_100, FIXED6x9_BITMAP_100},
  {0x0065, FIXED6x9_METRICS_101, FIXED6x9_BITMAP_101},
  {0x0066, FIXED6x9_METRICS_102, FIXED6x9_BITMAP_102},
  {0x0067, FIXED6x9_METRICS_103, FIXED6x9_BITMAP_103},
  {0x0068, FIXED6x9_METRICS_104, FIXED6x9_BITMAP_104},
  {0x0069, FIXED6x9_METRICS_105, FIXED6x9_BITMAP_105},
  {0x006a, FIXED6x9_METRICS_106, FIXED6x9_BITMAP_106},
  {0x006b, FIXED6x9_METRICS_107, FIXED6x9_BITMAP_107},
  {0x006c, FIXED6x9_METRICS_108, FIXED6x9_BITMAP_108},
  {0x006d, FIXED6x9_METRICS_109, FIXED6x9_BITMAP_109},
  {0x006e, FIXED6x9_METRICS_110, FIXED6x9_BITMAP_110},
  {0x006f, FIXED6x9_METRICS_111, FIXED6x9_BITMAP_111},
  {0x0070, FIXED6x9_METRICS_112, FIXED6x9_BITMAP_112},
  {0x0071, FIXED6x9_METRICS_113, FIXED6x9_BITMAP_113},
  {0x0072, FIXED6x9_METRICS_114, FIXED6x9_BITMAP_114},
  {0x0073, FIXED6x9_METRICS_115, FIXED6x9_BITMAP_115},
  {0x0074, FIXED6x9_METRICS_116, FIXED6x9_BITMAP_116},
  {0x0075, FIXED6x9_METRICS_117, FIXED6x9_BITMAP_117},
  {0x0076, FIXED6x9_METRICS_118, FIXED6x9_BITMAP_118},
  {0x0077, FIXED6x9_METRICS_119, FIXED6x9_BITMAP_119},
  {0x0078, FIXED6x9_METRICS_120, FIXED6x9_BITMAP_120},
  {0x0079, FIXED6x9_METRICS_121, FIXED6x9_BITMAP_121},
  {0x007a, FIXED6x9_METRICS_122, FIXED6x9_BITMAP_122},
  {0x007b, FIXED6x9_METRICS_123, FIXED6x9_BITMAP_123},
  {0x007c, FIXED6x9_METRICS_124, FIXED6x9_BITMAP_124},
  {0x007d, FIXED6x9_METRICS_125, FIXED6x9_BITMAP_125},
  {0x007e, FIXED6x9_METRICS_126, FIXED6x9_BITMAP_126},
  {0x00a0, FIXED6x9_METRICS_160, FIXED6x9_BITMAP_160},
  {0x00a1, FIXED6x9_METRICS_161, FIXED6x9_BITMAP_161},
  {0x00a2, FIXED6x9_METRICS_162, FIXED6x9_BITMAP_162},
  {0x00a3, FIXED6x9_METRICS_163, FIXED6x9_BITMAP_163},
  {0x00a4, FIXED6x9_METRICS_164, FIXED6x9_BITMAP_164},
  {0x00a5, FIXED6x9_METRICS_165, FIXED6x9_BITMAP_165},
  {0x00a6, FIXED6x9_METRICS_166, FIXED6x9_BITMAP_166},
  {0x00a7, FIXED6x9_METRICS_167, FIXED6x9_BITMAP_167},
  {0x00a8, FIXED6x9_METRICS_168, FIXED6x9_BITMAP_168},
  {0x00a9, FIXED6x9_METRICS_169, FIXED6x9_BITMAP_169},
  {0x00aa, FIXED6x9_METRICS_170, FIXED6x9_BITMAP_170},
  {0x00ab, FIXED6x9_METRICS_171, FIXED6x9_BITMAP_171},
  {0x00ac, FIXED6x9_METRICS_172, FIXED6x9_BITMAP_172},
  {0x00ad, FIXED6x9_METRICS_173, FIXED6x9_BITMAP_173},
  {0x00ae, FIXED6x9_METRICS_174, FIXED6x9_BITMAP_174},
  {0x00af, FIXED6x9_METRICS_175, FIXED6x9_BITMAP_175},
  {0x00b0, FIXED6x9_METRICS_176, FIXED6x9_BITMAP_176},
  {0x00b1, FIXED6x9_METRICS_177, FIXED6x9_BITMAP_177},
  {0x00b2, FIXED6x9_METRICS_178, FIXED6x9_BITMAP_178},
  {0x00b3, FIXED6x9_METRICS_179, FIXED6x9_BITMAP_179},
  {0x00b4, FIXED6x9_METRICS_180, FIXED6x9_BITMAP_180},
  {0x00b5, FIXED6x9_METRICS_181, FIXED6x9_BITMAP_181},
  {0x00b6, FIXED6x9_METRICS_182, FIXED6x9_BITMAP_182},
  {0x00b7, FIXED6x9_METRICS_183, FIXED6x9_BITMAP_183},
  {0x00b8, FIXED6x9_METRICS_184, FIXED6x9_BITMAP_184},
  {0x00b9, FIXED6x9_METRICS_185, FIXED6x9_BITMAP_185},
  {0x00ba, FIXED6x9_METRICS_186, FIXED6x9_BITMAP_186},
  {0x00bb, FIXED6x9_METRICS_187, FIXED6x9_BITMAP_187},
  {0x00bc, FIXED6x9_METRICS_188, FIXED6x9_BITMAP_188},
  {0x00bd, FIXED6x9_METRICS_189, FIXED6x9_BITMAP_189},
  {0x00be, FIXED6x9_METRICS_190, FIXED6x9_BITMAP_190},
  {0x00bf, FIXED6x9_METRICS_191, FIXED6x9_BITMAP_191},
  {0x00c0, FIXED6x9_METRICS_192, FIXED6x9_BITMAP_192},
  {0x00c1, FIXED6x9_METRICS_193, FIXED6x9_BITMAP_193},
  {0x00c2, FIXED6x9_METRICS_194, FIXED6x9_BITMAP_194},
  {0x00c3, FIXED6x9_METRICS_195, FIXED6x9_BITMAP_195},
  {0x00c4, FIXED6x9_METRICS_196, FIXED6x9_BITMAP_196},
  {0x00c5, FIXED6x9_METRICS_197, FIXED6x9_BITMAP_197},
  {0x00c6, FIXED6x9_METRICS_198, FIXED6x9_BITMAP_198},
  {0x00c7, FIXED6x9_METRICS_199, FIXED6x9_BITMAP_199},
  {0x00c8, FIXED6x9_METRICS_200, FIXED6x9_BITMAP_200},
  {0x00c9, FIXED6x9_METRICS_201, FIXED6x9_BITMAP_201},
  {0x00ca, FIXED6x9_METRICS_202, FIXED6x9_BITMAP_202},
  {0x00cb, FIXED6x9_METRICS_203, FIXED6x9_BITMAP_203},
  {0x00cc, FIXED6x9_METRICS_204, FIXED6x9_BITMAP_204},
  {0x00cd, FIXED6x9_METRICS_205, FIXED6x9_BITMAP_205},
  {0x00ce, FIXED6x9_METRICS_206, FIXED6x9_BITMAP_206},
  {0x00cf, FIXED6x9_METRICS_207, FIXED6x9_BITMAP_207},
  {0x00d0, FIXED6x9_METRICS_208, FIXED6x9_BITMAP_208},
  {0x00d1, FIXED6x9_METRICS_209, FIXED6x9_BITMAP_209},
  {0x00d2, FIXED6x9_METRICS_210, FIXED6x9_BITMAP_210},
  {0x00d3, FIXED6x9_METRICS_211, FIXED6x9_BITMAP_211},
  {0x00d4, FIXED6x9_METRICS_212, FIXED6x9_BITMAP_212},
  {0x00d5, FIXED6x9_METRICS_213, FIXED6x9_BITMAP_213},
  {0x00d6, FIXED6x9_METRICS_214, FIXED6x9_BITMAP_214},
  {0x00d7, FIXED6x9_METRICS_215, FIXED6x9_BITMAP_215},
  {0x00d8, FIXED6x9_METRICS_216, FIXED6x9_BITMAP_216},
  {0x00d9, FIXED6x9_METRICS_217, FIXED6x9_BITMAP_217},
  {0x00da, FIXED6x9_METRICS_218, FIXED6x9_BITMAP_218},
  {0x00db, FIXED6x9_METRICS_219, FIXED6x9_BITMAP_219},
  {0x00dc, FIXED6x9_METRICS_220, FIXED6x9_BITMAP_220},
  {0x00dd, FIXED6x9_METRICS_221, FIXED6x9_BITMAP_221},
  {0x00de, FIXED6x9_METRICS_222, FIXED6x9_BITMAP_222},
  {0x00df, FIXED6x9_METRICS_223, FIXED6x9_BITMAP_223},
  {0x00e0, FIXED6x9_METRICS_224, FIXED6x9_BITMAP_224},
  {0x00e1, FIXED6x9_METRICS_225, FIXED6x9_BITMAP_225},
  {0x00e2, FIXED6x9_METRICS_226, FIXED6x9_BITMAP_226},
  {0x00e3, FIXED6x9_METRICS_227, FIXED6x9_BITMAP_227},
  {0x00e4, FIXED6x9_METRICS_228, FIXED6x9_BITMAP_228},
  {0x00e5, FIXED6x9_METRICS_229, FIXED6x9_BITMAP_229},
  {0x00e6, FIXED6x9_METRICS_230, FIXED6x9_BITMAP_230},
  {0x00e7, FIXED6x9_METRICS_231, FIXED6x9_BITMAP_231},
  {0x00e8, FIXED6x9_METRICS_232, FIXED6x9_BITMAP_232},
  {0x00e9, FIXED6x9_METRICS_233, FIXED6x9_BITMAP_233},
  {0x00ea, FIXED6x9_METRICS_234, FIXED6x9_BITMAP_234},
  {0x00eb, FIXED6x9_METRICS_235, FIXED6x9_BITMAP_235},
  {0x00ec, FIXED6x9_METRICS_236, FIXED6x9_BITMAP_236},
  {0x00ed, FIXED6x9_METRICS_237, FIXED6x9_BITMAP_237},
  {0x00ee, FIXED6x9_METRICS_238, FIXED6x9_BITMAP_238},
  {0x00ef, FIXED6x9_METRICS_239, FIXED6x9_BITMAP_239},
  {0x00f0, FIXED6x9_METRICS_240, FIXED6x9_BITMAP_240},
  {0x00f1, FIXED6x9_METRICS_241, FIXED6x9_BITMAP_241},
  {0x00f2, FIXED6x9_METRICS_242, FIXED6x9_BITMAP_242},
  {0x00f3, FIXED6x9_METRICS_243, FIXED6x9_BITMAP_243},
  {0x00f4, FIXED6x9_METRICS_244, FIXED6x9_BITMAP_244},
  {0x00f5, FIXED6x9_METRICS_245, FIXED6x9_BITMAP_245},
  {0x00f6, FIXED6x9_METRICS_246, FIXED6x9_BITMAP_246},
  {0x00f7, FIXED6x9_METRICS_247, FIXED6x9_BITMAP_247},
  {0x00f8, FIXED6x9_METRICS_248, FIXED6x9_BITMAP_248},
  {0x00f9, FIXED6x9_METRICS_249, FIXED6x9_BITMAP_249},
  {0x00fa, FIXED6x9_METRICS_250, FIXED6x9_BITMAP_250},
  {0x00fb, FIXED6x9_METRICS_251, FIXED6x9_BITMAP_251},
  {0x00fc, FIXED6x9_METRICS_252, FIXED6x9_BITMAP_252},
  {0x00fd, FIXED6x9_METRICS_253, FIXED6x9_BITMAP_253},
  {0x00fe, FIXED6x9_METRICS_254, FIXED6x9_BITMAP_254},
  {0x00ff, FIXED6x9_METRICS_255, FIXED6x9_BITMAP_255},
  {0x0100, FIXED6x9_METRICS_256, FIXED6x9_BITMAP_256},
  {0x0101, FIXED6x9_METRICS_257, FIXED6x9_BITMAP_257},
  {0x0102, FIXED6x9_METRICS_258, FIXED6x9_BITMAP_258},
  {0x0103, FIXED6x9_METRICS_259, FIXED6x9_BITMAP_259},
  {0x0104, FIXED6x9_METRICS_260, FIXED6x9_BITMAP_260},
  {0x0105, FIXED6x9_METRICS_261, FIXED6x9_BITMAP_261},
  {0x0106, FIXED6x9_METRICS_262, FIXED6x9_BITMAP_262},
  {0x0107, FIXED6x9_METRICS_263, FIXED6x9_BITMAP_263},
  {0x0108, FIXED6x9_METRICS_264, FIXED6x9_BITMAP_264},
  {0x0109, FIXED6x9_METRICS_265, FIXED6x9_BITMAP_265},
  {0x010a, FIXED6x9_METRICS_266, FIXED6x9_BITMAP_266},
  {0x010b, FIXED6x9_METRICS_267, FIXED6x9_BITMAP_267},
  {0x010c, FIXED6x9_METRICS_268, FIXED6x9_BITMAP_268},
  {0x010d, FIXED6x9_METRICS_269, FIXED6x9_BITMAP_269},
  {0x010e, FIXED6x9_METRICS_270, FIXED6x9_BITMAP_270},
  {0x010f, FIXED6x9_METRICS_271, FIXED6x9_BITMAP_271},
  {0x0110, FIXED6x9_METRICS_272, FIXED6x9_BITMAP_272},
  {0x0111, FIXED6x9_METRICS_273, FIXED6x9_BITMAP_273},
  {0x0112, FIXED6x9_METRICS_274, FIXED6x9_BITMAP_274},
  {0x0113, FIXED6x9_METRICS_275, FIXED6x9_BITMAP_275},
  {0x0114, FIXED6x9_METRICS_276, FIXED6x9_BITMAP_276},
  {0x0115, FIXED6x9_METRICS_277, FIXED6x9_BITMAP_277},
  {0x0116, FIXED6x9_METRICS_278, FIXED6x9_BITMAP_278},
  {0x0117, FIXED6x9_METRICS_279, FIXED6x9_BITMAP_279},
  {0x0118, FIXED6x9_METRICS_280, FIXED6x9_BITMAP_280},
  {0x0119, FIXED6x9_METRICS_281, FIXED6x9_BITMAP_281},
  {0x011a, FIXED6x9_METRICS_282, FIXED6x9_BITMAP_282},
  {0x011b, FIXED6x9_METRICS_283, FIXED6x9_BITMAP_283},
  {0x011c, FIXED6x9_METRICS_284, FIXED6x9_BITMAP_284},
  {0x011d, FIXED6x9_METRICS_285, FIXED6x9_BITMAP_285},
  {0x011e, FIXED6x9_METRICS_286, FIXED6x9_BITMAP_286},
  {0x011f, FIXED6x9_METRICS_287, FIXED6x9_BITMAP_287},
  {0x0120, FIXED6x9_METRICS_288, FIXED6x9_BITMAP_288},
  {0x0121, FIXED6x9_METRICS_289, FIXED6x9_BITMAP_289},
  {0x0122, FIXED6x9_METRICS_290, FIXED6x9_BITMAP_290},
  {0x0123, FIXED6x9_METRICS_291, FIXED6x9_BITMAP_291},
  {0x0124, FIXED6x9_METRICS_292, FIXED6x9_BITMAP_292},
  {0x0125, FIXED6x9_METRICS_293, FIXED6x9_BITMAP_293},
  {0x0126, FIXED6x9_METRICS_294, FIXED6x9_BITMAP_294},
  {0x0127, FIXED6x9_METRICS_295, FIXED6x9_BITMAP_295},
  {0x0128, FIXED6x9_METRICS_296, FIXED6x9_BITMAP_296},
  {0x0129, FIXED6x9_METRICS_297, FIXED6x9_BITMAP_297},
  {0x012a, FIXED6x9_METRICS_298, FIXED6x9_BITMAP_298},
  {0x012b, FIXED6x9_METRICS_299, FIXED6x9_BITMAP_299},
  {0x012c, FIXED6x9_METRICS_300, FIXED6x9_BITMAP_300},
  {0x012d, FIXED6x9_METRICS_301, FIXED6x9_BITMAP_301},
  {0x012e, FIXED6x9_METRICS_302, FIXED6x9_BITMAP_302},
  {0x012f, FIXED6x9_METRICS_303, FIXED6x9_BITMAP_303},
  {0x0130, FIXED6x9_METRICS_304, FIXED6x9_BITMAP_304},
  {0x0131, FIXED6x9_METRICS_305, FIXED6x9_BITMAP_305},
  {0x0132, FIXED6x9_METRICS_306, FIXED6x9_BITMAP_306},
  {0x0133, FIXED6x9_METRICS_307, FIXED6x9_BITMAP_307},
  {0x0134, FIXED6x9_METRICS_308, FIXED6x9_BITMAP_308},
  {0x0135, FIXED6x9_METRICS_309, FIXED6x9_BITMAP_309},
  {0x0136, FIXED6x9_METRICS_310, FIXED6x9_BITMAP_310},
  {0x0137, FIXED6x9_METRICS_311, FIXED6x9_BITMAP_311},
  {0x0138, FIXED6x9_METRICS_312, FIXED6x9_BITMAP_312},
  {0x0139, FIXED6x9_METRICS_313, FIXED6x9_BITMAP_313},
  {0x013a, FIXED6x9_METRICS_314, FIXED6x9_BITMAP_314},
  {0x013b, FIXED6x9_METRICS_315, FIXED6x9_BITMAP_315},
  {0x013c, FIXED6x9_METRICS_316, FIXED6x9_BITMAP_316},
  {0x013d, FIXED6x9_METRICS_317, FIXED6x9_BITMAP_317},
  {0x013e, FIXED6x9_METRICS_318, FIXED6x9_BITMAP_318},
  {0x013f, FIXED6x9_METRICS_319, FIXED6x9_BITMAP_319},
  {0x0140, FIXED6x9_METRICS_320, FIXED6x9_BITMAP_320},
  {0x0141, FIXED6x9_METRICS_321, FIXED6x9_BITMAP_321},
  {0x0142, FIXED6x9_METRICS_322, FIXED6x9_BITMAP_322},
  {0x0143, FIXED6x9_METRICS_323, FIXED6x9_BITMAP_323},
  {0x0144, FIXED6x9_METRICS_324, FIXED6x9_BITMAP_324},
  {0x0145, FIXED6x9_METRICS_325, FIXED6x9_BITMAP_325},
  {0x0146, FIXED6x9_METRICS_326, FIXED6x9_BITMAP_326},
  {0x0147, FIXED6x9_METRICS_327, FIXED6x9_BITMAP_327},
  {0x0148, FIXED6x9_METRICS_328, FIXED6x9_BITMAP_328},
  {0x0149, FIXED6x9_METRICS_329, FIXED6x9_BITMAP_329},
  {0x014a, FIXED6x9_METRICS_330, FIXED6x9_BITMAP_330},
  {0x014b, FIXED6x9_METRICS_331, FIXED6x9_BITMAP_331},
  {0x014c, FIXED6x9_METRICS_332, FIXED6x9_BITMAP_332},
  {0x014d, FIXED6x9_METRICS_333, FIXED6x9_BITMAP_333},
  {0x014e, FIXED6x9_METRICS_334, FIXED6x9_BITMAP_334},
  {0x014f, FIXED6x9_METRICS_335, FIXED6x9_BITMAP_335},
  {0x0150, FIXED6x9_METRICS_336, FIXED6x9_BITMAP_336},
  {0x0151, FIXED6x9_METRICS_337, FIXED6x9_BITMAP_337},
  {0x0152, FIXED6x9_METRICS_338, FIXED6x9_BITMAP_338},
  {0x0153, FIXED6x9_METRICS_339, FIXED6x9_BITMAP_339},
  {0x0154, FIXED6x9_METRICS_340, FIXED6x9_BITMAP_340},
  {0x0155, FIXED6x9_METRICS_341, FIXED6x9_BITMAP_341},
  {0x0156, FIXED6x9_METRICS_342, FIXED6x9_BITMAP_342},
  {0x0157, FIXED6x9_METRICS_343, FIXED6x9_BITMAP_343},
  {0x0158, FIXED6x9_METRICS_344, FIXED6x9_BITMAP_344},
  {0x0159, FIXED6x9_METRICS_345, FIXED6x9_BITMAP_345},
  {0x015a, FIXED6x9_METRICS_346, FIXED6x9_BITMAP_346},
  {0x015b, FIXED6x9_METRICS_347, FIXED6x9_BITMAP_347},
  {0x015c, FIXED6x9_METRICS_348, FIXED6x9_BITMAP_348},
  {0x015d, FIXED6x9_METRICS_349, FIXED6x9_BITMAP_349},
  {0x015e, FIXED6x9_METRICS_350, FIXED6x9_BITMAP_350},
  {0x015f, FIXED6x9_METRICS_351, FIXED6x9_BITMAP_351},
  {0x0160, FIXED6x9_METRICS_352, FIXED6x9_BITMAP_352},
  {0x0161, FIXED6x9_METRICS_353, FIXED6x9_BITMAP_353},
  {0x0162, FIXED6x9_METRICS_354, FIXED6x9_BITMAP_354},
  {0x0163, FIXED6x9_METRICS_355, FIXED6x9_BITMAP_355},
  {0x0164, FIXED6x9_METRICS_356, FIXED6x9_BITMAP_356},
  {0x0165, FIXED6x9_METRICS_357, FIXED6x9_BITMAP_357},
  {0x0166, FIXED6x9_METRICS_358, FIXED6x9_BITMAP_358},
  {0x0167, FIXED6x9_METRICS_359, FIXED6x9_BITMAP_359},
  {0x0168, FIXED6x9_METRICS_360, FIXED6x9_BITMAP_360},
  {0x0169, FIXED6x9_METRICS_361, FIXED6x9_BITMAP_361},
  {0x016a, FIXED6x9_METRICS_362, FIXED6x9_BITMAP_362},
  {0x016b, FIXED6x9_METRICS_363, FIXED6x9_BITMAP_363},
  {0x016c, FIXED6x9_METRICS_364, FIXED6x9_BITMAP_364},
  {0x016d, FIXED6x9_METRICS_365, FIXED6x9_BITMAP_365},
  {0x016e, FIXED6x9_METRICS_366, FIXED6x9_BITMAP_366},
  {0x016f, FIXED6x9_METRICS_367, FIXED6x9_BITMAP_367},
  {0x0170, FIXED6x9_METRICS_368, FIXED6x9_BITMAP_368},
  {0x0171, FIXED6x9_METRICS_369, FIXED6x9_BITMAP_369},
  {0x0172, FIXED6x9_METRICS_370, FIXED6x9_BITMAP_370},
  {0x0173, FIXED6x9_METRICS_371, FIXED6x9_BITMAP_371},
  {0x0174, FIXED6x9_METRICS_372, FIXED6x9_BITMAP_372},
  {0x0175, FIXED6x9_METRICS_373, FIXED6x9_BITMAP_373},
  {0x0176, FIXED6x9_METRICS_374, FIXED6x9_BITMAP_374},
  {0x0177, FIXED6x9_METRICS_375, FIXED6x9_BITMAP_375},
  {0x0178, FIXED6x9_METRICS_376, FIXED6x9_BITMAP_376},
  {0x0179, FIXED6x9_METRICS_377, FIXED6x9_BITMAP_377},
  {0x017a, FIXED6x9_METRICS_378, FIXED6x9_BITMAP_378},
  {0x017b, FIXED6x9_METRICS_379, FIXED6x9_BITMAP_379},
  {0x017c, FIXED6x9_METRICS_380, FIXED6x9_BITMAP_380},
  {0x017d, FIXED6x9_METRICS_381, FIXED6x9_BITMAP_381},
  {0x017e, FIXED6x9_METRICS_382, FIXED6x9_BITMAP_382},
  {0x017f, FIXED6x9_METRICS_383, FIXED6x9_BITMAP_383},
  {0x02bb, FIXED6x9_METRICS_699, FIXED6x9_BITMAP_699},
  {0x02bc, FIXED6x9_METRICS_700, FIXED6x9_BITMAP_700},
  {0x02bd, FIXED6x9_METRICS_701, FIXED6x9_BITMAP_701},
  {0x02c6, FIXED6x9_METRICS_710, FIXED6x9_BITMAP_710},
  {0x02c7, FIXED6x9_METRICS_711, FIXED6x9_BITMAP_711},
  {0x02c9, FIXED6x9_METRICS_713, FIXED6x9_BITMAP_713},
  {0x02d8, FIXED6x9_METRICS_728, FIXED6x9_BITMAP_728},
  {0x02d9, FIXED6x9_METRICS_729, FIXED6x9_BITMAP_729},
  {0x02da, FIXED6x9_METRICS_730, FIXED6x9_BITMAP_730},
  {0x02db, FIXED6x9_METRICS_731, FIXED6x9_BITMAP_731},
  {0x02dc, FIXED6x9_METRICS_732, FIXED6x9_BITMAP_732},
  {0x02dd, FIXED6x9_METRICS_733, FIXED6x9_BITMAP_733},
  {0x2010, FIXED6x9_METRICS_8208, FIXED6x9_BITMAP_8208},
  {0x2011, FIXED6x9_METRICS_8209, FIXED6x9_BITMAP_8209},
  {0x2012, FIXED6x9_METRICS_8210, FIXED6x9_BITMAP_8210},
  {0x2013, FIXED6x9_METRICS_8211, FIXED6x9_BITMAP_8211},
  {0x2014, FIXED6x9_METRICS_8212, FIXED6x9_BITMAP_8212},
  {0x2015, FIXED6x9_METRICS_8213, FIXED6x9_BITMAP_8213},
  {0x2016, FIXED6x9_METRICS_8214, FIXED6x9_BITMAP_8214},
  {0x2017, FIXED6x9_METRICS_8215, FIXED6x9_BITMAP_8215},
  {0x2018, FIXED6x9_METRICS_8216, FIXED6x9_BITMAP_8216},
  {0x2019, FIXED6x9_METRICS_8217, FIXED6x9_BITMAP_8217},
  {0x201a, FIXED6x9_METRICS_8218, FIXED6x9_BITMAP_8218},
  {0x201b, FIXED6x9_METRICS_8219, FIXED6x9_BITMAP_8219},
  {0x201c, FIXED6x9_METRICS_8220, FIXED6x9_BITMAP_8220},
  {0x201d, FIXED6x9_METRICS_8221, FIXED6x9_BITMAP_8221},
  {0x201e, FIXED6x9_METRICS_8222, FIXED6x9_BITMAP_8222},
  {0x201f, FIXED6x9_METRICS_8223, FIXED6x9_BITMAP_8223},
  {0x2020, FIXED6x9_METRICS_8224, FIXED6x9_BITMAP_8224},
  {0x2021, FIXED6x9_METRICS_8225, FIXED6x9_BITMAP_8225},
  {0x2022, FIXED6x9_METRICS_8226, FIXED6x9_BITMAP_8226},
  {0x2023, FIXED6x9_METRICS_8227, FIXED6x9_BITMAP_8227},
  {0x2024, FIXED6x9_METRICS_8228, FIXED6x9_BITMAP_8228},
  {0x2025, FIXED6x9_METRICS_8229, FIXED6x9_BITMAP_8229},
  {0x2026, FIXED6x9_METRICS_8230, FIXED6x9_BITMAP_8230},
  {0x2027, FIXED6x9_METRICS_8231, FIXED6x9_BITMAP_8231},
  {0x2030, FIXED6x9_METRICS_8240, FIXED6x9_BITMAP_8240},
  {0x2032, FIXED6x9_METRICS_8242, FIXED6x9_BITMAP_8242},
  {0x2033, FIXED6x9_METRICS_8243, FIXED6x9_BITMAP_8243},
  {0x2034, FIXED6x9_METRICS_8244, FIXED6x9_BITMAP_8244},
  {0x2035, FIXED6x9_METRICS_8245, FIXED6x9_BITMAP_8245},
  {0x2036, FIXED6x9_METRICS_8246, FIXED6x9_BITMAP_8246},
  {0x2037, FIXED6x9_METRICS_8247, FIXED6x9_BITMAP_8247},
  {0x2039, FIXED6x9_METRICS_8249, FIXED6x9_BITMAP_8249},
  {0x203a, FIXED6x9_METRICS_8250, FIXED6x9_BITMAP_8250},
  {0x203c, FIXED6x9_METRICS_8252, FIXED6x9_BITMAP_8252},
  {0x203e, FIXED6x9_METRICS_8254, FIXED6x9_BITMAP_8254},
  {0x2044, FIXED6x9_METRICS_8260, FIXED6x9_BITMAP_8260},
  {0x2102, FIXED6x9_METRICS_8450, FIXED6x9_BITMAP_8450},
  {0x2105, FIXED6x9_METRICS_8453, FIXED6x9_BITMAP_8453},
  {0x2113, FIXED6x9_METRICS_8467, FIXED6x9_BITMAP_8467},
  {0x2115, FIXED6x9_METRICS_8469, FIXED6x9_BITMAP_8469},
  {0x2116, FIXED6x9_METRICS_8470, FIXED6x9_BITMAP_8470},
  {0x211a, FIXED6x9_METRICS_8474, FIXED6x9_BITMAP_8474},
  {0x211d, FIXED6x9_METRICS_8477, FIXED6x9_BITMAP_8477},
  {0x2122, FIXED6x9_METRICS_8482, FIXED6x9_BITMAP_8482},
  {0x2124, FIXED6x9_METRICS_8484, FIXED6x9_BITMAP_8484},
  {0x2126, FIXED6x9_METRICS_8486, FIXED6x9_BITMAP_8486},
  {0x212e, FIXED6x9_METRICS_8494, FIXED6x9_BITMAP_8494},
  {0x215b, FIXED6x9_METRICS_8539, FIXED6x9_BITMAP_8539},
  {0x215c, FIXED6x9_METRICS_8540, FIXED6x9_BITMAP_8540},
  {0x215d, FIXED6x9_METRICS_8541, FIXED6x9_BITMAP_8541},
  {0x215e, FIXED6x9_METRICS_8542, FIXED6x9_BITMAP_8542},
  {0x2190, FIXED6x9_METRICS_8592, FIXED6x9_BITMAP_8592},
  {0x2191, FIXED6x9_METRICS_8593, FIXED6x9_BITMAP_8593},
  {0x2192, FIXED6x9_METRICS_8594, FIXED6x9_BITMAP_8594},
  {0x2193, FIXED6x9_METRICS_8595, FIXED6x9_BITMAP_8595},
  {0x2194, FIXED6x9_METRICS_8596, FIXED6x9_BITMAP_8596},
  {0x2195, FIXED6x9_METRICS_8597, FIXED6x9_BITMAP_8597},
  {0x21a4, FIXED6x9_METRICS_8612, FIXED6x9_BITMAP_8612},
  {0x21a5, FIXED6x9_METRICS_8613, FIXED6x9_BITMAP_8613},
  {0x21a6, FIXED6x9_METRICS_8614, FIXED6x9_BITMAP_8614},
  {0x21a7, FIXED6x9_METRICS_8615, FIXED6x9_BITMAP_8615},
  {0x21a8, FIXED6x9_METRICS_8616, FIXED6x9_BITMAP_8616},
  {0x21d0, FIXED6x9_METRICS_8656, FIXED6x9_BITMAP_8656},
  {0x21d1, FIXED6x9_METRICS_8657, FIXED6x9_BITMAP_8657},
  {0x21d2, FIXED6x9_METRICS_8658, FIXED6x9_BITMAP_8658},
  {0x21d3, FIXED6x9_METRICS_8659, FIXED6x9_BITMAP_8659},
  {0x21d4, FIXED6x9_METRICS_8660, FIXED6x9_BITMAP_8660},
  {0x21d5, FIXED6x9_METRICS_8661, FIXED6x9_BITMAP_8661},
  {0x2500, FIXED6x9_METRICS_9472, FIXED6x9_BITMAP_9472},
  {0x2501, FIXED6x9_METRICS_9473, FIXED6x9_BITMAP_9473},
  {0x2502, FIXED6x9_METRICS_9474, FIXED6x9_BITMAP_9474},
  {0x2503, FIXED6x9_METRICS_9475, FIXED6x9_BITMAP_9475},
  {0x250c, FIXED6x9_METRICS_9484, FIXED6x9_BITMAP_9484},
  {0x2510, FIXED6x9_METRICS_9488, FIXED6x9_BITMAP_9488},
  {0x2514, FIXED6x9_METRICS_9492, FIXED6x9_BITMAP_9492},
  {0x2518, FIXED6x9_METRICS_9496, FIXED6x9_BITMAP_9496},
  {0x251c, FIXED6x9_METRICS_9500, FIXED6x9_BITMAP_9500},
  {0x2524, FIXED6x9_METRICS_9508, FIXED6x9_BITMAP_9508},
  {0x252c, FIXED6x9_METRICS_9516, FIXED6x9_BITMAP_9516},
  {0x2534, FIXED6x9_METRICS_9524, FIXED6x9_BITMAP_9524},
  {0x253c, FIXED6x9_METRICS_9532, FIXED6x9_BITMAP_9532},
  {0x254c, FIXED6x9_METRICS_9548, FIXED6x9_BITMAP_9548},
  {0x254d, FIXED6x9_METRICS_9549, FIXED6x9_BITMAP_9549},
  {0x254e, FIXED6x9_METRICS_9550, FIXED6x9_BITMAP_9550},
  {0x254f, FIXED6x9_METRICS_9551, FIXED6x9_BITMAP_9551},
  {0x2550, FIXED6x9_METRICS_9552, FIXED6x9_BITMAP_9552},
  {0x2551, FIXED6x9_METRICS_9553, FIXED6x9_BITMAP_9553},
  {0x2552, FIXED6x9_METRICS_9554, FIXED6x9_BITMAP_9554},
  {0x2553, FIXED6x9_METRICS_9555, FIXED6x9_BITMAP_9555},
  {0x2554, FIXED6x9_METRICS_9556, FIXED6x9_BITMAP_9556},
  {0x2555, FIXED6x9_METRICS_9557, FIXED6x9_BITMAP_9557},
  {0x2556, FIXED6x9_METRICS_9558, FIXED6x9_BITMAP_9558},
  {0x2557, FIXED6x9_METRICS_9559, FIXED6x9_BITMAP_9559},
  {0x2558, FIXED6x9_METRICS_9560, FIXED6x9_BITMAP_9560},
  {0x2559, FIXED6x9_METRICS_9561, FIXED6x9_BITMAP_9561},
  {0x255a, FIXED6x9_METRICS_9562, FIXED6x9_BITMAP_9562},
  {0x255b, FIXED6x9_METRICS_9563, FIXED6x9_BITMAP_9563},
  {0x255c, FIXED6x9_METRICS_9564, FIXED6x9_BITMAP_9564},
  {0x255d, FIXED6x9_METRICS_9565, FIXED6x9_BITMAP_9565},
  {0x255e, FIXED6x9_METRICS_9566, FIXED6x9_BITMAP_9566},
  {0x255f, FIXED6x9_METRICS_9567, FIXED6x9_BITMAP_9567},
  {0x2560, FIXED6x9_METRICS_9568, FIXED6x9_BITMAP_9568},
  {0x2561, FIXED6x9_METRICS_9569, FIXED6x9_BITMAP_9569},
  {0x2562, FIXED6x9_METRICS_9570, FIXED6x9_BITMAP_9570},
  {0x2563, FIXED6x9_METRICS_9571, FIXED6x9_BITMAP_9571},
  {0x2564, FIXED6x9_METRICS_9572, FIXED6x9_BITMAP_9572},
  {0x2565, FIXED6x9_METRICS_9573, FIXED6x9_BITMAP_9573},
  {0x2566, FIXED6x9_METRICS_9574, FIXED6x9_BITMAP_9574},
  {0x2567, FIXED6x9_METRICS_9575, FIXED6x9_BITMAP_9575},
  {0x2568, FIXED6x9_METRICS_9576, FIXED6x9_BITMAP_9576},
  {0x2569, FIXED6x9_METRICS_9577, FIXED6x9_BITMAP_9577},
  {0x256a, FIXED6x9_METRICS_9578, FIXED6x9_BITMAP_9578},
  {0x256b, FIXED6x9_METRICS_9579, FIXED6x9_BITMAP_9579},
  {0x256c, FIXED6x9_METRICS_9580, FIXED6x9_BITMAP_9580},
  {0x256d, FIXED6x9_METRICS_9581, FIXED6x9_BITMAP_9581},
  {0x256e, FIXED6x9_METRICS_9582, FIXED6x9_BITMAP_9582},
  {0x256f, FIXED6x9_METRICS_9583, FIXED6x9_BITMAP_9583},
  {0x2570, FIXED6x9_METRICS_9584, FIXED6x9_BITMAP_9584},
  {0x2571, FIXED6x9_METRICS_9585, FIXED6x9_BITMAP_9585},
  {0x2572, FIXED6x9_METRICS_9586, FIXED6x9_BITMAP_9586},
  {0x2573, FIXED6x9_METRICS_9587, FIXED6x9_BITMAP_9587},
  {0x25a0, FIXED6x9_METRICS_9632, FIXED6x9_BITMAP_9632},
  {0x25a1, FIXED6x9_METRICS_9633, FIXED6x9_BITMAP_9633},
  {0x25a2, FIXED6x9_METRICS_9634, FIXED6x9_BITMAP_9634},
  {0x25a3, FIXED6x9_METRICS_9635, FIXED6x9_BITMAP_9635},
  {0x25aa, FIXED6x9_METRICS_9642, FIXED6x9_BITMAP_9642},
  {0x25ab, FIXED6x9_METRICS_9643, FIXED6x9_BITMAP_9643},
  {0x25ac, FIXED6x9_METRICS_9644, FIXED6x9_BITMAP_9644},
  {0x25ad, FIXED6x9_METRICS_9645, FIXED6x9_BITMAP_9645},
  {0x25ae, FIXED6x9_METRICS_9646, FIXED6x9_BITMAP_9646},
  {0x25af, FIXED6x9_METRICS_9647, FIXED6x9_BITMAP_9647},
  {0x25b0, FIXED6x9_METRICS_9648, FIXED6x9_BITMAP_9648},
  {0x25b1, FIXED6x9_METRICS_9649, FIXED6x9_BITMAP_9649},
  {0x25b2, FIXED6x9_METRICS_9650, FIXED6x9_BITMAP_9650},
  {0x25b3, FIXED6x9_METRICS_9651, FIXED6x9_BITMAP_9651},
  {0x25b4, FIXED6x9_METRICS_9652, FIXED6x9_BITMAP_9652},
  {0x25b5, FIXED6x9_METRICS_9653, FIXED6x9_BITMAP_9653},
  {0x25b6, FIXED6x9_METRICS_9654, FIXED6x9_BITMAP_9654},
  {0x25b7, FIXED6x9_METRICS_9655, FIXED6x9_BITMAP_9655},
  {0x25b8, FIXED6x9_METRICS_9656, FIXED6x9_BITMAP_9656},
  {0x25b9, FIXED6x9_METRICS_9657, FIXED6x9_BITMAP_9657},
  {0x25ba, FIXED6x9_METRICS_9658, FIXED6x9_BITMAP_9658},
  {0x25bb, FIXED6x9_METRICS_9659, FIXED6x9_BITMAP_9659},
  {0x25bc, FIXED6x9_METRICS_9660, FIXED6x9_BITMAP_9660},
  {0x25bd, FIXED6x9_METRICS_9661, FIXED6x9_BITMAP_9661},
  {0x25be, FIXED6x9_METRICS_9662, FIXED6x9_BITMAP_9662},
  {0x25bf, FIXED6x9_METRICS_9663, FIXED6x9_BITMAP_9663},
  {0x25c0, FIXED6x9_METRICS_9664, FIXED6x9_BITMAP_9664},
  {0x25c1, FIXED6x9_METRICS_9665, FIXED6x9_BITMAP_9665},
  {0x25c2, FIXED6x9_METRICS_9666, FIXED6x9_BITMAP_9666},
  {0x25c3, FIXED6x9_METRICS_9667, FIXED6x9_BITMAP_9667},
  {0x25c4, FIXED6x9_METRICS_9668, FIXED6x9_BITMAP_9668},
  {0x25c5, FIXED6x9_METRICS_9669, FIXED6x9_BITMAP_9669},
  {0x25c6, FIXED6x9_METRICS_9670, FIXED6x9_BITMAP_9670},
  {0x25ca, FIXED6x9_METRICS_9674, FIXED6x9_BITMAP_9674},
  {0x25cb, FIXED6x9_METRICS_9675, FIXED6x9_BITMAP_9675},
  {0x25cf, FIXED6x9_METRICS_9679, FIXED6x9_BITMAP_9679},
  {0x25d8, FIXED6x9_METRICS_9688, FIXED6x9_BITMAP_9688},
  {0x25d9, FIXED6x9_METRICS_9689, FIXED6x9_BITMAP_9689},
  {0x25e6, FIXED6x9_METRICS_9702, FIXED6x9_BITMAP_9702},
  {0x2600, FIXED6x9_METRICS_9728, FIXED6x9_BITMAP_9728},
  {0x2639, FIXED6x9_METRICS_9785, FIXED6x9_BITMAP_9785},
  {0x263a, FIXED6x9_METRICS_9786, FIXED6x9_BITMAP_9786},
  {0x263b, FIXED6x9_METRICS_9787, FIXED6x9_BITMAP_9787},
  {0x263c, FIXED6x9_METRICS_9788, FIXED6x9_BITMAP_9788},
  {0x263f, FIXED6x9_METRICS_9791, FIXED6x9_BITMAP_9791},
  {0x2640, FIXED6x9_METRICS_9792, FIXED6x9_BITMAP_9792},
  {0x2641, FIXED6x9_METRICS_9793, FIXED6x9_BITMAP_9793},
  {0x2642, FIXED6x9_METRICS_9794, FIXED6x9_BITMAP_9794},
  {0x2660, FIXED6x9_METRICS_9824, FIXED6x9_BITMAP_9824},
  {0x2661, FIXED6x9_METRICS_9825, FIXED6x9_BITMAP_9825},
  {0x2662, FIXED6x9_METRICS_9826, FIXED6x9_BITMAP_9826},
  {0x2663, FIXED6x9_METRICS_9827, FIXED6x9_BITMAP_9827},
  {0x2664, FIXED6x9_METRICS_9828, FIXED6x9_BITMAP_9828},
  {0x2665, FIXED6x9_METRICS_9829, FIXED6x9_BITMAP_9829},
  {0x2666, FIXED6x9_METRICS_9830, FIXED6x9_BITMAP_9830},
  {0x2669, FIXED6x9_METRICS_9833, FIXED6x9_BITMAP_9833},
  {0x266a, FIXED6x9_METRICS_9834, FIXED6x9_BITMAP_9834},
  {0x266b, FIXED6x9_METRICS_9835, FIXED6x9_BITMAP_9835},
};
