# Makefile -- the makefile for the libx

# Copyright 2015, Michael L. Gran

# This file is part of the Project Burro game engine.

# Project Burro is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# Project Burro is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Project Burro.  If not, see
# <http://www.gnu.org/licenses/>. */

SHELL = /bin/sh

srcdir = .

CC       = gcc --std=c11 -fdiagnostics-color=auto -fmessage-length=78
CXX      = g++ --std=c++11 -fdiagnostics-color=auto -fmessage-length=78

################################################################
# FLAGS

CPPFLAGS = \
 -Wall \
 -Wunused-macros \
 -Wendif-labels \
 -Wunknown-pragmas \
 `pkg-config cairo --cflags` \
 `pkg-config gtk+-3.0 --cflags` \
 `pkg-config guile-2.0 --cflags` \
 `pkg-config libpulse --cflags` \
 -I .

DEFS     = -DSDL_ASSERT_LEVEL=2

CDEBUG   = -g3 -O0

CWARN = \
 -Wall \
 -Wpointer-arith \
 -Wformat-nonliteral \
 -Winit-self \
 -Wignored-qualifiers \
 -Wwrite-strings \
 -Wconversion \

    # -Wcast-align \
    # -Wmissing-declarations \
    # -Wpointer-arith \
    # -Warray-bounds -ftree-vrp \
    # -Wstrict-aliasing -fstrict-aliasing \
    # -Wstrict-overflow -fstrict-overflow \
    # -Wunreachable-code \
    # -Winvalid-pch \
	#    -Wundef \

CXXWARN = $(CWARN) -Wsign-promo -Weffc++

CFLAGS   = $(CPPFLAGS) $(CWARN) $(CDEBUG) $(DEFS) -fvisibility=hidden

CXXFLAGS = $(CFLAGS) $(CXXWARN) $(CDEBUG) $(DEFS)

LDFLAGS  = -Xlinker --Map=burro.map \
	-Wl,--gc-sections -Wl,--as-needed -Wl,--demangle

################################################################
# TOP-LEVEL SOURCES

SRCS_H = \
 xcairo.h \
 xgdk.h \
 xgdk-pixbuf.h \
 xgio.h \
 xgtk.h \
 xguile.h \
 xpulseaudio.h

SRCS_C   = \
 xcairo.c \
 xgdk.c \
 xgdk-pixbuf.c \
 xgio.c \
 xglib.c \
 xgtk.c \
 xguile.c \
 xpulseaudio.c

################################################################
# ALL

.PHONY:  all
all: libx.a x.pch

################################################################
# GENERATED SOURCES


################################################################
# OBJECTS

OBJS     = \
	$(SRCS_C:.c=.o) \
	$(SRCS_CPP:.cpp=.o)

DEPENDS = ${OBJS:.o=.d}
-include ${DEPENDS}

%.o : %.c
	$(CC) $(CFLAGS) -c $*.c -o $*.o
	$(CC) $(CPPFLAGS) -M $*.c > $*.d

%.o : %.cpp
	$(CXX) $(CXXFLAGS) -c $*.cpp -o $*.o
	$(CXX) $(CPPFLAGS) -M  $*.cpp > $*.d

################################################################
# MAJOR TARGETS

libx.a: $(OBJS)
	$(AR) rcs $@ $(OBJS)

x.pch: x.h $(SRCS_H)
	$(CC) -o $@ $(CFLAGS) $<

################################################################
# MINOR TARGETS

.PHONY:  clean
clean:
	-rm -f $(OBJS) $(DEPENDS) bdf-convert burro burro.iso core

.PHONY: check-syntax
check-syntax:
	$(CC) $(CFLAGS) -fsyntax-only $(SRCS_C)

TAGS: $(SRCS_C) $(SRCS_CPP)
	etags $(SRCS_C) $(SRCS_CPP)

