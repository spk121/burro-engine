/* utf.cpp -- Unicode functions

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

// Am I really writing yet another UTF function?  I am so tired of C
// and Unicode.

// For UTF−8 string literals, the array elements have type char, and
// are initialized with the characters of the multibyte character
// sequence, as encoded in UTF−8.

// For wide string literals prefixed by the letter L, the array
// elements have type wchar_t and are initialized with the sequence of
// wide characters corresponding to the multibyte character sequence,
// as defined by the mbstowcs function with an implementation-defined
// current locale. For wide string literals prefixed by the letter u
// or U, the array elements have type char16_t or char32_t,
// respectively, and are initialized with the sequence of wide
// characters corresponding to the multibyte character sequence, as
// defined by successive calls to the mbrtoc16, or mbrtoc32 function
// as appropriate for its type, with an implementation-defined current
// locale. The value of a string literal containing a multibyte
// character or escape sequence not represented in the execution
// character set is implementation-defined.

// A character string literal is a sequence of zero or more multibyte
// characters enclosed in double-quotes, as in "xyz". A UTF−8 string
// literal is the same, except prefixed by u8.  A wide string literal
// is the same, except prefixed by the letter L, u, or U.

// _ _STDC_UTF_16_ _ The integer constant 1, intended to indicate that
// values of type char16_t are UTF −16 encoded. If some other encoding
// is used, the macro shall not be defined and the actual encoding
// used is implementation- defined.

// _ _STDC_UTF_32_ _ The integer constant 1, intended to indicate that
// values of type char32_t are UTF −32 encoded. If some other encoding
// is used, the macro shall not be defined and the actual encoding
// used is implementation- defined.

// The encoding of any of wchar_t, char16_t, and char32_t where the
// corresponding standard encoding macro (_ _STDC_ISO_10646_ _, _
// _STDC_UTF_16_ _, or _ _STDC_UTF_32_ _) is not defined (6.10.8.2).

// _ _STDC_ISO_10646_ _ An integer constant of the form yyyymmL (for
// example, 199712L). If this symbol is defined, then every character
// in the Unicode required set, when stored in an object of type
// wchar_t, has the same value as the short identifier of that
// character. The Unicode required set consists of all the characters
// that are defined by ISO/IEC 10646, along with all amendments and
// technical corrigenda, as of the specified year and month. If some
// other encoding is used, the macro shall not be defined and the
// actual encoding used is implementation-defined.

// The behavior of the multibyte character functions is affected by
// the LC_CTYPE category of the current locale.

// MB_CUR_MAX which expands to a positive integer expression with type
// size_t that is the maximum number of bytes in a multibyte character
// for the extended character set specified by the current locale
// (category LC_CTYPE), which is never greater than MB_LEN_MAX.

#include <stdint.h>
#include <wchar.h>
#include <memory.h>
#include <SDL.h>
#include <locale.h>
#include <limits.h>
#include "utf.hpp"

// Count the number of multi-byte characters in a multi-byte string,
// not including the final NULL.
size_t
mbs_len (const char *s)
{
    mbstate_t state;
    size_t result = 0;
    size_t nbytes;

    SDL_assert(s != NULL);

    memset (&state, '\0', sizeof (state));
    while ((nbytes = mbrlen (s, MB_LEN_MAX, &state)) > 0)
    {
        if (nbytes == (size_t) -2 || nbytes == (size_t) -1)
            /* Something is wrong.  */
            return (size_t) -1;
        s += nbytes;
        ++result;
    }
    return result;
}

// Converts multi-byte string S to a newly allocated WCHAR_T string,
// or NULL on any allocation or encoding error.
wchar_t *mbs_to_wcs_alloc(const char *s)
{
    mbstate_t state;
    size_t wclen;
    wchar_t *wcs;
    
    SDL_assert(s != NULL);

    wclen = mbs_len (s);

    // If string is empty or has errors, give up.
    if (wclen <= 0)
        return NULL;

    memset (&state, '\0', sizeof (state));
    wcs = (wchar_t *) calloc(wclen + 1, sizeof(wchar_t));
    if (wcs == NULL)
        return NULL;

    // Since mbslen has already done error validation, 
    // this better succeed.
    size_t thislen = mbsrtowcs (wcs, &s, wclen, &state);

    // If this fails but mbslen succeeded, then something
    // very strange has happened.
    SDL_assert (thislen == wclen);

    return wcs;
}

wchar_t *utf8_to_wcs_alloc(const char *s)
{    
#if !WINDOWS
    char *old_locale, *saved_locale, *new_locale;
    wchar_t *wcs;

    /* Get the name of the current locale.  */
    old_locale = setlocale (LC_ALL, NULL);

    /* Copy the name so it won't be clobbered by 'setlocale'. */
    saved_locale = strdup (old_locale);
    if (saved_locale == NULL)
        SDL_assert ("Out of memory");

    /* Now change to common UTF-8 locale. */
    new_locale = setlocale (LC_ALL, "en_US.utf8");
    
    SDL_assert (new_locale != NULL);

    wcs = mbs_to_wcs_alloc (s);

    /* Restore the original locale. */
    setlocale (LC_ALL, saved_locale);
    free (saved_locale);
    return wcs;
#else
#error "Untested implementation"
    // FIXME: something like this
    int result;

    // get length of converted string in characters
    result = MultiByteToWideChar (CP_UTF8, MB_ERR_INVALID_CHARS, (char *)bytes, 
                                  sizeof (bytes), NULL, 0);
    
    wchar_t * name = new wchar_t [result];
    
    // convert string
    result = MultiByteToWideChar (CP_UTF8, MB_ERR_INVALID_CHARS, (char *)bytes, 
                                  sizeof (bytes), name, result);
    return result;
#endif
}
