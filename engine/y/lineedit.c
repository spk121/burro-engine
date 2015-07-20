/* lineedit.c -- command line editor
 *
 * Copyright (c) 2014, Michael L. Gran <spk121@yahoo.com>
 *
 * derived from 
 * linenoise.c -- guerrilla line editing library against the idea that a
 * line editing lib needs to be 20,000 lines of C code.
 *
 * You can find the latest source code for linenoise at:
 * 
 *   http://github.com/antirez/linenoise
 *
 * Copyright (c) 2010-2013, Salvatore Sanfilippo <antirez at gmail dot com>
 * Copyright (c) 2010-2013, Pieter Noordhuis <pcnoordhuis at gmail dot com>
 *
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 *  *  Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  *  Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * ------------------------------------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <wchar.h>
#include "lineedit.h"
#include "console.h"
#include "const.h"

#define LINENOISE_DEFAULT_HISTORY_MAX_LEN 100

wchar_t linenoiseLineBuf[LINENOISE_MAX_LINE+1];
static linenoiseCompletionCallback *completionCallback = NULL;

// Global state 
struct linenoiseState l;

static int mlmode = 0;  /* Multi line mode. Default is single line. */
static int history_max_len = LINENOISE_DEFAULT_HISTORY_MAX_LEN;
static int history_len = 0;
static wchar_t **history = NULL;

static void refreshLine(struct linenoiseState *l);
static int linenoiseHistoryAdd(const wchar_t *line);
static int linenoiseHistoryLoad(const char *filename);
static int linenoiseHistorySave(const char *filename);

/* ======================= Low level terminal handling ====================== */

/* Set if to use or not the multi line mode. */
void linenoiseSetMultiLine(int ml) {
    mlmode = ml;
}

/* Clear the screen. Used to handle ctrl+l */
void linenoiseClearScreen(void) {
    console_move_to(0, 0);
    console_erase_page();
}

/* Beep, used for completion when there is nothing to complete or when all
 * the choices were already shown. */
#if 0
static void linenoiseBeep(void) {
    console_bell();
}
#endif

/* ============================== Completion ================================ */

/* Free a list of completion option populated by linenoiseAddCompletion(). */
#if 0
static void freeCompletions(linenoiseCompletions *lc) {
    size_t i;
    for (i = 0; i < lc->len; i++)
        free(lc->cvec[i]);
    if (lc->cvec != NULL)
        free(lc->cvec);
}
#endif

/* This is an helper function for linenoiseEdit() and is called when the
 * user types the <tab> key in order to complete the string currently in the
 * input.
 * 
 * The state of the editing is encapsulated into the pointed linenoiseState
 * structure as described in the structure definition. */
static int completeLine(struct linenoiseState *ls) {
#if 0
    linenoiseCompletions lc = { 0, NULL };
    int nread, nwritten;
    char c = 0;

    completionCallback(ls->buf,&lc);
    if (lc.len == 0) {
        linenoiseBeep();
    } else {
        size_t stop = 0, i = 0;

        while(!stop) {
            /* Show completion or original buffer */
            if (i < lc.len) {
                struct linenoiseState saved = *ls;

                ls->len = ls->pos = wcslen(lc.cvec[i]);
                ls->buf = lc.cvec[i];
                refreshLine(ls);
                ls->len = saved.len;
                ls->pos = saved.pos;
                ls->buf = saved.buf;
            } else {
                refreshLine(ls);
            }

            nread = read(ls->ifd,&c,1);
            if (nread <= 0) {
                freeCompletions(&lc);
                return -1;
            }

            switch(c) {
                case 9: /* tab */
                    i = (i+1) % (lc.len+1);
                    if (i == lc.len) linenoiseBeep();
                    break;
                case 27: /* escape */
                    /* Re-show original buffer */
                    if (i < lc.len) refreshLine(ls);
                    stop = 1;
                    break;
                default:
                    /* Update buffer and return */
                    if (i < lc.len) {
                        nwritten = swprintf(ls->buf, ls->buflen, L"%s", lc.cvec[i]);
                        ls->len = ls->pos = nwritten;
                    }
                    stop = 1;
                    break;
            }
        }
    }

    freeCompletions(&lc);
    return c; /* Return last read character */
#else
    return ' ';
#endif
}


/* Register a callback function to be called for tab-completion. */
void linenoiseSetCompletionCallback(linenoiseCompletionCallback *fn) {
    completionCallback = fn;
}

/* This function is used by the callback function registered by the user
 * in order to add completion options given the input string when the
 * user typed <tab>. See the example.c source code for a very easy to
 * understand example. */
void linenoiseAddCompletion(linenoiseCompletions *lc, const wchar_t *str) {
    size_t len = wcslen(str);
    wchar_t *copy, **cvec;

    copy = (wchar_t *) calloc(len+1, sizeof(wchar_t));
    if (copy == NULL)
        return;
    memcpy(copy, str, sizeof(wchar_t) * (len+1));
    cvec = (wchar_t **) realloc(lc->cvec, sizeof(wchar_t *)*(lc->len+1));
    if (cvec == NULL) {
        free(copy);
        return;
    }
    lc->cvec = cvec;
    lc->cvec[lc->len++] = copy;
}

/* =========================== Line editing ================================= */
/* Single line low level line refresh.
 *
 * Rewrite the currently edited line accordingly to the buffer content,
 * cursor position, and number of columns of the terminal. */
static void refreshSingleLine(struct linenoiseState *l) {
    size_t plen = wcslen(l->prompt);
    wchar_t *buf = l->buf;
    size_t len = l->len;
    size_t pos = l->pos;
    
    while((plen+pos) >= l->cols) {
        buf++;
        len--;
        pos--;
    }
    while (plen+len > l->cols) {
        len--;
    }

    /* Cursor to left edge */
    console_move_to_column(0);
    /* Write the prompt and the current buffer content */
    console_write_wchar_string(l->prompt, plen);
    console_write_wchar_string(buf, len);
    /* Erase to right */
    console_erase_to_end_of_line();
    /* Move cursor to original position. */
    console_move_to_column(pos+plen);
}

/* Multi line low level line refresh.
 *
 * Rewrite the currently edited line accordingly to the buffer content,
 * cursor position, and number of columns of the terminal. */
static void refreshMultiLine(struct linenoiseState *l) {
    int plen = wcslen(l->prompt);
    int rows = (plen+l->len+l->cols-1)/l->cols; /* rows used by current buf. */
    int rpos = (plen+l->oldpos+l->cols)/l->cols; /* cursor relative row. */
    int rpos2; /* rpos after refresh. */
    int old_rows = l->maxrows;
    int j;

    /* Update maxrows if needed. */
    if (rows > (int)l->maxrows) l->maxrows = rows;

    /* First step: clear all the lines used before. To do so start by
     * going to the last row. */
    if (old_rows-rpos > 0) {
        console_move_down(old_rows - rpos);
    }

    /* Now for every row clear it, go up. */
    for (j = 0; j < old_rows-1; j++) {
        console_move_to_column(0);
        console_erase_to_end_of_line();
        console_move_up(1);
    }

    /* Clean the top line. */
    console_move_to_column(0);
    console_erase_to_end_of_line();
    
    /* Write the prompt and the current buffer content */
    console_write_wchar_string(l->prompt, plen);
    console_write_wchar_string(l->buf, l->len);

    /* If we are at the very end of the screen with our prompt, we need to
     * emit a newline and move the prompt to the first column. */
    if (l->pos &&
        l->pos == l->len &&
        (l->pos+plen) % l->cols == 0)
    {
        console_scroll_up(1);
        console_move_to_column(0);
        rows++;
        if (rows > (int)l->maxrows)
            l->maxrows = rows;
    }

    /* Move cursor to right position. */
    rpos2 = (plen+l->pos+l->cols)/l->cols; /* current cursor relative row. */

    /* Go up till we reach the expected positon. */
    if (rows-rpos2 > 0) {
        console_move_up(rows - rpos2);
    }

    /* Set column. */
    console_move_to_column(((plen+(int)l->pos) % (int)l->cols));

    // console_move_down(1);
    l->oldpos = l->pos;
}

/* Calls the two low level functions refreshSingleLine() or
 * refreshMultiLine() according to the selected mode. */
static void refreshLine(struct linenoiseState *l)
{
    if (mlmode)
        refreshMultiLine(l);
    else
        refreshSingleLine(l);
}

/* Insert the character 'c' at cursor current position.
 *
 * On error writing to the terminal -1 is returned, otherwise 0. */
int linenoiseEditInsert(struct linenoiseState *l, wchar_t c) {
    if (l->len < l->buflen) {
        if (l->len == l->pos) {
            l->buf[l->pos] = c;
            l->pos++;
            l->len++;
            l->buf[l->len] = L'\0';
            if ((!mlmode && l->plen+l->len < l->cols) /* || mlmode */) {
                /* Avoid a full update of the line in the
                 * trivial case. */
                console_write_wchar_string(&c, 1);
            } else {
                refreshLine(l);
            }
        } else {
            memmove(l->buf+l->pos+1, l->buf+l->pos, sizeof(wchar_t) * (l->len-l->pos));
            l->buf[l->pos] = c;
            l->len++;
            l->pos++;
            l->buf[l->len] = L'\0';
            refreshLine(l);
        }
    }
    return 0;
}

int linenoiseEditOverwrite(struct linenoiseState *l, wchar_t c) {
    if (l->len < l->buflen) {
        if (l->len == l->pos) {
            l->buf[l->pos] = c;
            l->pos++;
            l->len++;
            l->buf[l->len] = L'\0';
            if (!mlmode && (l->plen + l->len < l->cols)) {
                console_write_wchar_string(&c, 1);
            } else {
                refreshLine(l);
            }
        } else {
            l->buf[l->pos] = c;
            l->pos ++;
            refreshLine(l);
        }
    }
    return 0;
}

/* Move cursor on the left. */
void linenoiseEditMoveLeft(struct linenoiseState *l) {
    if (l->pos > 0) {
        l->pos--;
        refreshLine(l);
    }
}

/* Move cursor on the right. */
void linenoiseEditMoveRight(struct linenoiseState *l) {
    if (l->pos != l->len) {
        l->pos++;
        refreshLine(l);
    }
}

/* Move cursor to the start of the line. */
void linenoiseEditMoveHome(struct linenoiseState *l) {
    if (l->pos != 0) {
        l->pos = 0;
        refreshLine(l);
    }
}

/* Move cursor to the end of the line. */
void linenoiseEditMoveEnd(struct linenoiseState *l) {
    if (l->pos != l->len) {
        l->pos = l->len;
        refreshLine(l);
    }
}

/* Substitute the currently edited line with the next or previous history
 * entry as specified by 'dir'. */
#define LINENOISE_HISTORY_NEXT 0
#define LINENOISE_HISTORY_PREV 1
void linenoiseEditHistoryNext(struct linenoiseState *l, int dir) {
    if (history_len > 1) {
        /* Update the current history entry before to
         * overwrite it with the next one. */
        free(history[history_len - 1 - l->history_index]);
        history[history_len - 1 - l->history_index] = wcsdup(l->buf);
        /* Show the new entry */
        l->history_index += (dir == LINENOISE_HISTORY_PREV) ? 1 : -1;
        if (l->history_index < 0) {
            l->history_index = 0;
            return;
        } else if (l->history_index >= history_len) {
            l->history_index = history_len-1;
            return;
        }
        wcsncpy(l->buf, history[history_len - 1 - l->history_index], l->buflen);
        l->buf[l->buflen-1] = L'\0';
        l->len = l->pos = wcslen(l->buf);
        refreshLine(l);
    }
}

/* Delete the character at the right of the cursor without altering the cursor
 * position. Basically this is what happens with the "Delete" keyboard key. */
void linenoiseEditDelete(struct linenoiseState *l) {
    if (l->len > 0 && l->pos < l->len) {
        memmove(l->buf+l->pos, l->buf+l->pos+1, sizeof(wchar_t) * (l->len-l->pos-1));
        l->len--;
        l->buf[l->len] = L'\0';
        refreshLine(l);
    }
}

/* Backspace implementation. */
void linenoiseEditBackspace(struct linenoiseState *l) {
    if (l->pos > 0 && l->len > 0) {
        memmove(l->buf+l->pos-1, l->buf+l->pos, sizeof(wchar_t) * (l->len-l->pos));
        l->pos--;
        l->len--;
        l->buf[l->len] = L'\0';
        refreshLine(l);
    }
}

/* Delete the previosu word, maintaining the cursor at the start of the
 * current word. */
void linenoiseEditDeletePrevWord(struct linenoiseState *l) {
    size_t old_pos = l->pos;
    size_t diff;

    while (l->pos > 0 && l->buf[l->pos-1] == L' ')
        l->pos--;
    while (l->pos > 0 && l->buf[l->pos-1] != L' ')
        l->pos--;
    diff = old_pos - l->pos;
    memmove(l->buf+l->pos, l->buf+old_pos, sizeof(wchar_t) * (l->len-old_pos+1));
    l->len -= diff;
    refreshLine(l);
}

void lineedit_initialize()
{
    memset(&l, 0, sizeof(l));
    linenoiseSetMultiLine(1);
    /* Set the completion callback. This will be called every time the
     * user uses the <tab> key. */
    // linenoiseSetCompletionCallback(completion);

    /* Load history from file. The history file is just a plain text file
     * where entries are separated by newlines. */
    linenoiseHistoryLoad("history.txt");
    lineedit_start (linenoiseLineBuf, LINENOISE_MAX_LINE, L"->");
}

void lineedit_start(wchar_t *buf, size_t buflen, const wchar_t *prompt)
{
    memset(&l, 0, sizeof(l));

    /* Populate the linenoise state that we pass to functions implementing
     * specific editing functionalities. */
    l.buf = buf;
    l.buflen = buflen;
    l.prompt = prompt;
    l.plen = wcslen(prompt);
    l.oldpos = l.pos = 0;
    l.len = 0;
    l.cols = CONSOLE_COLS;
    l.maxrows = 0;
    l.history_index = 0;

    /* Buffer starts empty. */
    l.buf[0] = L'\0';
    l.buflen--; /* Make sure there is always space for the nulterm */

    /* The latest history entry is always our current buffer, that
     * initially is just an empty string. */
    linenoiseHistoryAdd(L"");
    refreshLine(&l);
}

void lineedit_stop()
{
    if (l.buflen > 0) {
        linenoiseHistoryAdd(l.buf); /* Add to the history. */
        linenoiseHistorySave("history.txt"); /* Save the history on disk. */
    }
}

void lineedit_finalize()
{
}

/* ================================ History ================================= */

/* Free the history, but does not reset it. Only used when we have to
* exit() to avoid memory leaks are reported by valgrind & co. */
#if 0
static void freeHistory(void)
{
    if (history) {
        int j;
        
        for (j = 0; j < history_len; j++)
            free(history[j]);
        free(history);
    }
}
#endif

/* At exit we'll try to fix the terminal to the initial conditions. */
#if 0
static void linenoiseAtExit(void)
{
    freeHistory();
}
#endif

/* This is the API call to add a new entry in the linenoise history.
* It uses a fixed array of char pointers that are shifted (memmoved)
* when the history max length is reached in order to remove the older
* entry and make room for the new one, so it is not exactly suitable for huge
* histories, but will work well for a few hundred of entries.
*
* Using a circular buffer is smarter, but a bit more complex to handle. */
static int linenoiseHistoryAdd(const wchar_t *line)
{
    wchar_t *linecopy;

    if (history_max_len == 0)
        return 0;

    /* Initialization on first call. */
    if (history == NULL) {
        history = (wchar_t **) calloc(history_max_len, sizeof(wchar_t *));
        if (history == NULL)
            return 0;
    }

    /* Don't add duplicated lines. */
    if (history_len && !wcscmp(history[history_len-1], line))
        return 0;

    /* Add an heap allocated copy of the line in the history.
     * If we reached the max length, remove the older line. */
    linecopy = wcsdup(line);
    if (!linecopy)
        return 0;
    if (history_len == history_max_len) {
        free(history[0]);
        memmove(history, history+1, sizeof(wchar_t *) * (history_max_len-1));
        history_len--;
    }
    history[history_len] = linecopy;
    history_len++;
    return 1;
}

/* Set the maximum length for the history. This function can be called even
* if there is already some history, the function will make sure to retain
* just the latest 'len' elements if the new history length value is smaller
* than the amount of items already inside the history. */
int linenoiseHistorySetMaxLen(int len)
{
    wchar_t **_new;

    if (len < 1)
        return 0;
    if (history) {
        int tocopy = history_len;

        _new = (wchar_t **) calloc(len, sizeof(wchar_t *));
        if (_new == NULL)
            return 0;

        /* If we can't copy everything, free the elements we'll not use. */
        if (len < tocopy) {
            int j;
            
            for (j = 0; j < tocopy-len; j++)
                free(history[j]);
            tocopy = len;
        }
        memset(_new, 0 ,sizeof(wchar_t *) * len);
        memcpy(_new,history+(history_len-tocopy), sizeof(wchar_t *)*tocopy);
        free(history);
        history = _new;
    }
    history_max_len = len;
    if (history_len > history_max_len)
        history_len = history_max_len;
    return 1;
}

/* Save the history in the specified file. On success 0 is returned
 * otherwise -1 is returned. */
static int linenoiseHistorySave(const char *filename)
{
    FILE *fp = fopen(filename,"w");
    int j;
    
    if (fp == NULL) return -1;
    for (j = 0; j < history_len; j++)
        fwprintf(fp, L"%s\n", history[j]);
    fclose(fp);
    return 0;
}

/* Load the history from the specified file. If the file does not exist
* zero is returned and no operation is performed.
*
* If the file exists and the operation succeeded 0 is returned, otherwise
* on error -1 is returned. */
static int linenoiseHistoryLoad(const char *filename)
{
    FILE *fp = fopen(filename,"r");
    wchar_t buf[LINENOISE_MAX_LINE];
    
    if (fp == NULL)
        return -1;

    while (fgetws(buf,LINENOISE_MAX_LINE,fp) != NULL) {
        //char *p;
        
        //p = strchr(buf,'\r');
        //if (!p) p = strchr(buf,'\n');
        //if (p) *p = '\0';
        linenoiseHistoryAdd(buf);
    }
    fclose(fp);
    return 0;
}

/*****************************************************************/

void lineedit_autocomplete()
{
    char c;
    if (completionCallback != NULL) {
        c = completeLine(&l);
        if (c < 0)
            lineedit_return(l.len);
    }
}

void lineedit_backspace()
{
    linenoiseEditBackspace(&l);
}

void lineedit_clear_screen()
{
    linenoiseClearScreen();
    refreshLine(&l);
}

void lineedit_ctrl_c()
{
    lineedit_return(-1);
}

void lineedit_delete()
{
    linenoiseEditDelete(&l);
}

void lineedit_delete_line()
{
    l.buf[0] = L'\0';
    l.pos = l.len = 0;
    refreshLine(&l);
}

void lineedit_delete_or_eof()
{
    if (l.len > 0) {
        linenoiseEditDelete(&l);
    } else {
        history_len--;
        free(history[history_len]);
        lineedit_return(-1);
    }
}

void lineedit_delete_to_end()
{
    l.buf[l.pos] = L'\0';
    l.len = l.pos;
    refreshLine(&l);
}

void lineedit_delete_word_prev()
{
    linenoiseEditDeletePrevWord(&l);
}

void lineedit_enter()
{
    history_len --;
    free(history[history_len]);
    lineedit_return((int)l.len);
}

void lineedit_history_next()
{
    linenoiseEditHistoryNext(&l, LINENOISE_HISTORY_NEXT);
}

void lineedit_history_prev()
{
    linenoiseEditHistoryNext(&l, LINENOISE_HISTORY_PREV);
}

void lineedit_move_end()
{
    linenoiseEditMoveEnd(&l);
}

void lineedit_move_home()
{
    linenoiseEditMoveHome(&l);
}

void lineedit_move_left()
{
    linenoiseEditMoveLeft(&l);
}

void lineedit_move_right()
{
    linenoiseEditMoveRight(&l);
}

void lineedit_swap_chars()
{
    if (l.pos > 0 && l.pos < l.len) {
        wchar_t aux = l.buf[l.pos-1];
        l.buf[l.pos-1] = l.buf[l.pos];
        l.buf[l.pos] = aux;
        if (l.pos != l.len-1)
            l.pos ++;
        refreshLine(&l);
    }
}

void lineedit_return(int retval)
{
}

void lineedit_autocomplete_text_input(wchar_t *str)
{
}

void lineedit_text_input(wchar_t *str)
{
    for (unsigned int i = 0; i < wcslen(str); i ++) {
        linenoiseEditOverwrite(&l, str[i]);
    }
}    

char *lineedit_get_text(void)
{
    size_t wide_len, multibyte_max_len;
    mbstate_t state;
    wchar_t **p_wide_string = &(l.buf);
    char *padded_multibyte_string = NULL;
    char *multibyte_string = NULL;

    memset(&state, '\0', sizeof (state));

    wide_len = wcslen(l.buf);
    multibyte_max_len = wide_len * 4 + 1;
    padded_multibyte_string = (char *) calloc(multibyte_max_len, sizeof(char));
    wcsrtombs (padded_multibyte_string, (const wchar_t **) p_wide_string, multibyte_max_len, &state);
    multibyte_string = strdup(padded_multibyte_string);
    free(padded_multibyte_string);
    return multibyte_string;
}
