/* js_func.hpp -- the SpiderMonkey utility functions

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

#ifndef BURRO_JS_UTIL_H
#define BURRO_JS_UTIL_H

#include "jsapi.hpp"

#define DEFINE_BOOL_FUNC_INT(NAME1, NAME2)                  \
    static JSBool                                           \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)         \
    {                                                       \
        jsval* argv = JS_ARGV(cx, vp);                      \
        int n;                                              \
        if(!JS_ConvertArguments(cx, argc, argv, "i", &n))   \
            return JS_FALSE;                                \
        bool r = NAME2 (n);                                 \
        JS::Value ret;                                      \
        ret.setBoolean(r);                                  \
        JS_SET_RVAL(cx, vp, ret);                           \
        return JS_TRUE;                                     \
    }

#define DECLARE_BOOL_FUNC_INT(NAME1, NAME2)     \
    JS_FS(#NAME1, NAME1, 1, 0),

#define DEFINE_BOOL_FUNC_VOID(NAME1, NAME2)         \
    static JSBool                                   \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp) \
    {                                               \
        bool r = NAME2 ();                          \
        JS::Value ret;                              \
        ret.setBoolean(r);                          \
        JS_SET_RVAL(cx, vp, ret);                   \
        return JS_TRUE;                             \
    }

#define DECLARE_BOOL_FUNC_VOID(NAME1, NAME2)    \
    JS_FS(#NAME1, NAME1, 0, 0),

#define DEFINE_DOUBLE_FUNC_INT(NAME1, NAME2)                  \
    static JSBool                                             \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)           \
    {                                                         \
        jsval* argv = JS_ARGV(cx, vp);                        \
        int n;                                                \
        if(!JS_ConvertArguments(cx, argc, argv, "i", &n))     \
            return JS_FALSE;                                  \
        double r = NAME2 (n);                                 \
        JS::Value ret;                                        \
        ret.setNumber(r);                                     \
        JS_SET_RVAL(cx, vp, ret);                             \
        return JS_TRUE;                                       \
    }

#define DECLARE_DOUBLE_FUNC_INT(NAME1, NAME2)   \
    JS_FS(#NAME1, NAME1, 1, 0),

#define DEFINE_DOUBLE_FUNC_VOID(NAME1, NAME2)       \
    static JSBool                                   \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp) \
    {                                               \
        double r = NAME2 ();                        \
        JS::Value ret;                              \
        ret.setNumber(r);                           \
        JS_SET_RVAL(cx, vp, ret);                   \
        return JS_TRUE;                             \
    }

#define DECLARE_DOUBLE_FUNC_VOID(NAME1, NAME2)  \
    JS_FS(#NAME1, NAME1, 0, 0),

#define DEFINE_INT_FUNC_INT(NAME1, NAME2)                   \
    static JSBool                                           \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)         \
    {                                                       \
        jsval* argv = JS_ARGV(cx, vp);                      \
        int n;                                              \
        if(!JS_ConvertArguments(cx, argc, argv, "i", &n))   \
            return JS_FALSE;                                \
        int r = NAME2 (n);                                  \
        JS::Value ret;                                      \
        ret.setInt32(r);                                    \
        JS_SET_RVAL(cx, vp, ret);                           \
        return JS_TRUE;                                     \
    }

#define DECLARE_INT_FUNC_INT(NAME1, NAME2)      \
    JS_FS(#NAME1, NAME1, 1, 0),

#define DEFINE_VOID_FUNC_DOUBLE(NAME1, NAME2)               \
    static JSBool                                           \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)         \
    {                                                       \
        jsval* argv = JS_ARGV(cx, vp);                      \
        double n;                                           \
        if(!JS_ConvertArguments(cx, argc, argv, "d", &n))   \
            return JS_FALSE;                                \
        NAME2 (n);                                          \
        JS_SET_RVAL(cx, vp, JSVAL_VOID);                    \
        return JS_TRUE;                                     \
    }                                                       \

#define DECLARE_VOID_FUNC_DOUBLE(NAME1, NAME2)  \
    JS_FS(#NAME1, NAME1, 1, 0),

#define DEFINE_VOID_FUNC_INT(NAME1, NAME2)                  \
    static JSBool                                           \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)         \
    {                                                       \
        jsval* argv = JS_ARGV(cx, vp);                      \
        int n;                                              \
        if(!JS_ConvertArguments(cx, argc, argv, "i", &n))   \
            return JS_FALSE;                                \
        NAME2 (n);                                          \
        JS_SET_RVAL(cx, vp, JSVAL_VOID);                    \
        return JS_TRUE;                                     \
    }                                                       \

#define DECLARE_VOID_FUNC_INT(NAME1, NAME2)     \
    JS_FS(#NAME1, NAME1, 1, 0),


#define DEFINE_VOID_FUNC_INT_DOUBLE(NAME1, NAME2)                       \
    static JSBool                                                       \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)                     \
    {                                                                   \
        jsval* argv = JS_ARGV(cx, vp);                                  \
        int i1;                                                         \
        double d1;                                                      \
        if(!JS_ConvertArguments(cx, argc, argv, "id", &i1, &d1))        \
            return JS_FALSE;                                            \
        NAME2 (i1, d1);                                                 \
        JS_SET_RVAL(cx, vp, JSVAL_VOID);                                \
        return JS_TRUE;                                                 \
    }                                                                   \
    
#define DECLARE_VOID_FUNC_INT_DOUBLE(NAME1, NAME2)  \
    JS_FS(#NAME1, NAME1, 2, 0),

#define DEFINE_VOID_FUNC_INT_DOUBLEx2(NAME1, NAME2)                     \
    static JSBool                                                       \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)                     \
    {                                                                   \
        jsval* argv = JS_ARGV(cx, vp);                                  \
        int i1;                                                         \
        double d1, d2;                                                  \
        if(!JS_ConvertArguments(cx, argc, argv, "idd", &i1, &d1, &d2))  \
            return JS_FALSE;                                            \
        NAME2 (i1, d1, d2);                                             \
        JS_SET_RVAL(cx, vp, JSVAL_VOID);                                \
        return JS_TRUE;                                                 \
    }                                                                   \

#define DECLARE_VOID_FUNC_INT_DOUBLEx2(NAME1, NAME2)    \
    JS_FS(#NAME1, NAME1, 3, 0),

#define DEFINE_VOID_FUNC_INT_DOUBLEx7(NAME1, NAME2)                     \
    static JSBool                                                       \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)                     \
    {                                                                   \
        jsval* argv = JS_ARGV(cx, vp);                                  \
        int i1;                                                         \
        double d1, d2, d3, d4, d5, d6, d7;                                 \
        if(!JS_ConvertArguments(cx, argc, argv, "iddddddd", &i1, &d1, &d2, &d3, &d4, &d5, &d6, &d7)) \
            return JS_FALSE;                                            \
        NAME2 (i1, d1, d2, d3, d4, d5, d6, d7);                            \
        JS_SET_RVAL(cx, vp, JSVAL_VOID);                                \
        return JS_TRUE;                                                 \
    }                                                                   \

#define DECLARE_VOID_FUNC_INT_DOUBLEx7(NAME1, NAME2)    \
    JS_FS(#NAME1, NAME1, 8, 0),

#define DEFINE_VOID_FUNC_INTx2(NAME1, NAME2)                    \
    static JSBool                                               \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)             \
    {                                                           \
        jsval* argv = JS_ARGV(cx, vp);                          \
        int a, b;                                               \
        if(!JS_ConvertArguments(cx, argc, argv, "ii", &a, &b))  \
            return JS_FALSE;                                    \
        NAME2 (a, b);                                           \
        JS_SET_RVAL(cx, vp, JSVAL_VOID);                        \
        return JS_TRUE;                                         \
    }                                                           \

#define DECLARE_VOID_FUNC_INTx2(NAME1, NAME2)   \
    JS_FS(#NAME1, NAME1, 2, 0),

#define DEFINE_VOID_FUNC_INTx2_DOUBLEx4_INT(NAME1, NAME2)               \
    static JSBool                                                       \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)                     \
    {                                                                   \
        jsval* argv = JS_ARGV(cx, vp);                                  \
        int i1, i2, i3;                                                 \
        double d1, d2, d3, d4;                                          \
        if(!JS_ConvertArguments(cx, argc, argv, "iiddddi", &i1, &i2, &d1, &d2, &d3, &d4, &i3)) \
            return JS_FALSE;                                            \
        NAME2 (i1, i2, d1, d2, d3, d4, i3);                             \
        JS_SET_RVAL(cx, vp, JSVAL_VOID);                                \
        return JS_TRUE;                                                 \
    }                                                                   \

#define DECLARE_VOID_FUNC_INTx2_DOUBLEx4_INT(NAME1, NAME2)  \
    JS_FS(#NAME1, NAME1, 7, 0),

#define DEFINE_VOID_FUNC_INTx3(NAME1, NAME2)                        \
    static JSBool                                                   \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)                 \
    {                                                               \
        jsval* argv = JS_ARGV(cx, vp);                              \
        int a, b,c ;                                                \
        if(!JS_ConvertArguments(cx, argc, argv, "iii", &a, &b, &c)) \
            return JS_FALSE;                                        \
        NAME2 (a, b, c);                                            \
        JS_SET_RVAL(cx, vp, JSVAL_VOID);                            \
        return JS_TRUE;                                             \
    }                                                               \

#define DECLARE_VOID_FUNC_INTx3(NAME1, NAME2)   \
    JS_FS(#NAME1, NAME1, 3, 0),

#define DEFINE_VOID_FUNC_INTx5_DOUBLEx2_BOOLx2_INT(NAME1, NAME2)        \
    static JSBool                                                       \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)                     \
    {                                                                   \
        jsval* argv = JS_ARGV(cx, vp);                                  \
        int i1, i2, i3, i4, i5, i6;                                     \
        double d1, d2;                                                  \
        JSBool b1, b2;                                                  \
        if(!JS_ConvertArguments(cx, argc, argv, "iiiiiddbbi", &i1, &i2, &i3, &i4, &i5, &d1, &d2, &b1, &b2, &i6)) \
            return JS_FALSE;                                            \
        NAME2 (i1, i2, i3, i4, i5, d1, d1, b1, b2, i6);                 \
        JS_SET_RVAL(cx, vp, JSVAL_VOID);                                \
        return JS_TRUE;                                                 \
    }                                                                   \

#define DECLARE_VOID_FUNC_INTx5_DOUBLEx2_BOOLx2_INT(NAME1, NAME2)   \
    JS_FS(#NAME1, NAME1, 10, 0),

#define DEFINE_VOID_FUNC_STRING(NAME1, NAME2)                       \
    static JSBool                                                   \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)                 \
    {                                                               \
        jsval* argv = JS_ARGV(cx, vp);                              \
        JSString *js_string = NULL;                                 \
        if(!JS_ConvertArguments(cx, argc, argv, "S", &js_string))   \
            return JS_FALSE;                                        \
        if (js_string) {                                            \
            char *str;                                              \
            str = JS_EncodeString(cx, js_string);                   \
            NAME2(str);                                             \
            free(str);                                              \
        }                                                           \
        JS_SET_RVAL(cx, vp, JSVAL_VOID);                            \
        return JS_TRUE;                                             \
    }

#define DECLARE_VOID_FUNC_STRING(NAME1, NAME2)  \
    JS_FS(#NAME1, NAME1, 1, 0),

#define DEFINE_VOID_FUNC_UINT32(NAME1, NAME2)               \
    static JSBool                                           \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp)         \
    {                                                       \
        jsval* argv = JS_ARGV(cx, vp);                      \
        uint32_t n;                                         \
        if(!JS_ConvertArguments(cx, argc, argv, "u", &n))   \
            return JS_FALSE;                                \
        NAME2 (n);                                          \
        JS_SET_RVAL(cx, vp, JSVAL_VOID);                    \
        return JS_TRUE;                                     \
    }                                                       \

#define DECLARE_VOID_FUNC_UINT32(NAME1, NAME2)  \
    JS_FS(#NAME1, NAME1, 1, 0),

#define DEFINE_VOID_FUNC_VOID(NAME1, NAME2)         \
    static JSBool                                   \
    NAME1 (JSContext *cx, unsigned argc, jsval *vp) \
    {                                               \
        NAME2 ();                                   \
        JS_SET_RVAL(cx, vp, JSVAL_VOID);            \
        return JS_TRUE;                             \
    }                                               \

#define DECLARE_VOID_FUNC_VOID(NAME1, NAME2)    \
    JS_FS(#NAME1, NAME1, 0, 0),



#endif
