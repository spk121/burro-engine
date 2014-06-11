/* This file was automatically generated.  Do not edit! */
void loop();
void console_scroll_up(int n);
void console_write_char(uint16_t codepoint,int irm,int hem,int simd,int home,int limit);
void console_scroll_right(int n);
void console_scroll_left(int n);
#define COLOR_BG_TRANSPARENT    0b000000010010000
#define COLOR_BG_WHITE          0b000000010000000
#define COLOR_BG_CYAN           0b000000001110000
#define COLOR_BG_MAGENTA        0b000000001100000
#define COLOR_BG_BLUE           0b000000001010000
#define COLOR_BG_YELLOW         0b000000001000000
#define COLOR_BG_GREEN          0b000000000110000
#define COLOR_BG_RED            0b000000000100000
#define COLOR_BG_BLACK          0b000000000010000
#define COLOR_FG_TRANSPARENT    0b000000000001001
#define COLOR_FG_WHITE          0b000000000001000
#define COLOR_FG_CYAN           0b000000000000111
#define COLOR_FG_MAGENTA        0b000000000000110
#define COLOR_FG_BLUE           0b000000000000101
#define COLOR_FG_YELLOW         0b000000000000100
#define COLOR_FG_GREEN          0b000000000000011
#define COLOR_FG_RED            0b000000000000010
#define COLOR_FG_BLACK          0b000000000000001
#define POLARITY_POSITIVE       0b000000000000000
#define BLINK_NONE              0b000000000000000
#define UNDERLINE_NONE          0b000000000000000
#define COLOR_BG_DEFAULT        0b000000000000000
void console_set_bgcolor(uint32_t c);
#define COLOR_FG_DEFAULT        0b000000000000000
void console_set_fgcolor(uint32_t c);
#define INTENSITY_NORMAL        0b000000000000000
#define UNDERLINE_DOUBLY        0b100000000000000
#define POLARITY_NEGATIVE       0b000010000000000
void console_set_polarity(uint32_t c);
#define BLINK_FAST              0b001000000000000
#define BLINK_SLOW              0b000100000000000
void console_set_blink(uint32_t c);
#define UNDERLINE_SINGLY        0b010000000000000
void console_set_underline(uint32_t c);
#define INTENSITY_FAINT         0b000000100000000
#define INTENSITY_BOLD          0b000001000000000
void console_set_intensity(uint32_t c);
void console_set_default(void);
void console_scroll_down(int n);
void console_reset(void);
void console_insert_line_up(int n);
void console_insert_line_down(int n);
void console_insert_left(int n);
void console_insert_right(int n);
void console_move_to_row(int n);
void console_erase_line(void);
void console_erase_to_end_of_line(void);
void console_erase_to_beginning_of_line(void);
void console_erase_page(void);
void console_erase_to_end_of_page(void);
void console_erase_to_beginning_of_page(void);
void console_erase_right(int n);
void console_delete_line_up(int n);
void console_delete_line_down(int n);
void console_delete_left(int n);
void console_delete_right(int n);
void console_move_vertical_tab_down(int n);
void console_move_to(int r,int c);
void console_move_up(int n);
void console_move_down(int n);
void console_move_tab_right(int n);
void console_move_to_column(int n);
void console_move_tab_left(int n);
void console_move_right(int n);
void console_move_left(int n);
void console_bell(void);
int ecma48_execute(const char *data,int len);
int ecma48_init(void);
#define CONSOLE_ROWS 24
#define CONSOLE_COLS 60
extern int GR;
extern int GL;
