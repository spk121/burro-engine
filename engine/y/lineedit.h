/* lineedit.hpp -- the lowest layer of the screen background.

   Copyright 2014, Michael L. Gran

   This file is part of the Project Burro game engine.

   Project Burro is free software: you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   Project Burro is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Project Burro.  If not, see
   <http://www.gnu.org/licenses/>. */

#ifndef BURRO_LINEEDIT_H
#define BURRO_LINEEDIT_H

struct linenoiseCompletions_tag {
  size_t len;
  wchar_t **cvec;
};
typedef struct linenoiseCompletions_tag linenoiseCompletions;

/* The linenoiseState structure represents the state during line editing.
 * We pass this state to functions implementing specific editing
 * functionalities. */
struct linenoiseState {
    wchar_t *buf;       /* Edited line buffer. */
    size_t buflen;      /* Count of characters in buffer. */
    const wchar_t *prompt; /* Prompt to display. */
    size_t plen;        /* Count of characters in prompt. */
    size_t pos;         /* Current cursor position. */
    size_t oldpos;      /* Previous refresh cursor position. */
    size_t len;         /* Current edited line length. */
    size_t cols;        /* Number of columns in terminal. */
    size_t maxrows;     /* Maximum num of rows used so far (multiline mode) */
    int history_index;  /* The history index we are currently editing. */
};

typedef void(linenoiseCompletionCallback)(const wchar_t *, linenoiseCompletions *);

#define LINENOISE_MAX_LINE 4096
extern wchar_t linenoiseLineBuf[LINENOISE_MAX_LINE+1];

void  lineedit_autocomplete();
void  lineedit_autocomplete();
void  lineedit_backspace();
void  lineedit_backspace(void);
void  lineedit_clear_screen();
void  lineedit_ctrl_c();
void  lineedit_delete();
void  lineedit_delete_line();
void  lineedit_delete_or_eof();
void  lineedit_delete_to_end();
void  lineedit_delete_word_prev();
void  lineedit_history_next();
char* lineedit_get_text(void);
void  lineedit_history_prev();
void  lineedit_toggle_insert_mode();
void  lineedit_move_end();
void  lineedit_move_end();
void  lineedit_move_home();
void  lineedit_move_home();
void  lineedit_move_left();
void  lineedit_move_left();
void  lineedit_move_right();
void  lineedit_move_right();
//void  lineedit_start(wchar_t *buf, size_t buflen, const wchar_t *prompt);
void lineedit_start();
void  lineedit_stop();
void  lineedit_swap_chars();
void  lineedit_text_input(wchar_t *str);
void  lineedit_return(int retval);
void  lineedit_autocomplete_text_input(wchar_t *str);
void lineedit_initialize();
void lineedit_finalize();



#endif
