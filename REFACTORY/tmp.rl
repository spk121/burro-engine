%%{
  machine ECMA48;
  
  alphtype unsigned char;


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
    term_scroll_left (p1(1));
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
    foreach (x : P) {
      if (x == 4)
	irm = 1;
      else if (x == 7)
	vem = 1;
      else if (x == 10)
	hem = 1;
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
    term_scroll_right (p1(1));
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
    console_move_to_line(p1(1));
  }

  action do_vpb {
    // VPB - LINE POSITION BACKWARD
    console_move_up(p1(n));
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
      term_write_char (codepoint_tables[GL][n - 0x20], irm, hem, simd,
		       line_home - 1, line_limit - 1);
    else if (n >= 0xA0 && n <= 0xFF)
      term_write_char (codepoint_tables[GR][n - 0xA0], irm, hem, simd,
		       line_home - 1, line_limit - 1);
  }
  
  action parse_P_void {
    P.clear();
  }
  
  action parse_P {
    unsigned char *pos;
    int i, n;
    P.clear();
    
  loop:
    n = 0;
    i = 1;
    while (*pos >= '0' && *pos <= '9') {
      n += (*pos - '0') * i;
      i *= 10;
    }
    P.insert(n, P.begin());
    if (*pos == ';')
      goto loop;
  }
  
    command_string = ('\b' | space | graph)*;
    character_string = (any - (0x1B 0x58 | 0x98 | 0x1B 0x5C | 0x9C))*;
    Pn = '' @parse_P_void | digit+ @parse_P;
    Pn2 = '' @parse_2_void
      | digit+ @parse_P
      | (digit* ';' digit*) @parse_P;
    Ps = '' @parse_P_void | digit+ @parse_P;
    Ps2 = '' @parse_P_void
      | digit+ @parse_P
      | (digit* ';' digit*) @parse_P;
    Ps_ = '' @parse_P_void
      | digit+ @parse_P
      | (digit* ';')+ digit* @parse_P;


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
    SGR = CSI Ps_ 0x6D @do_sgr;
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
      GLYPH
      | NUL | ACK | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE | DC1
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
      )*;
}%%

%% write data;

int ecma48_init(ConsoleModel& terminal)
{
    T = terminal;
    // Writing moves right
    simd = 0;
    // overwrite, not insert
    irm = 0;
    // inserting moves write
    hem = 0;
    // inserting lines moves down
    vem = 0;
    // left edge of text is 1st column
    line_home = 1;
    // right endge of text is last column
    line_limit = CONSOLE_COLS;
    // top of page is 1st line
    page_home = 1;
    // bottom of page is last line
    page_limit = CONSOLE_ROWS;
    // parameter storage is cleared
    P.clear();
    
    %% write init;
    return 1;
}

int ecma48_execute(const char *data, int len)
{
    const char *p = data;
    const char *pe = data + len;

    %% write exec;

    if ( cs == StateChart_error )
        return -1;
    if ( cs >= StateChart_first_final )
        return 1;
    return 0;
}

int StateChart::finish( )
{
    if ( cs == StateChart_error )
        return -1;
    if ( cs >= StateChart_first_final )
        return 1;
    return 0;
}
