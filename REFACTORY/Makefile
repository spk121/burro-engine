# Makefile -- the makefile for the gui

# Copyright 2014, Michael L. Gran

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

CC       = gcc --std=c11
CXX      = g++ --std=c++11

DEFS     = -DSDL_ASSERT_LEVEL=2
CDEBUG   = -g3 -O0
WARNINGS = -Wall -Wextra -Weffc++ -Wsign-promo
CFLAGS   = $(WARNINGS) $(CDEBUG) $(DEFS) -I$(srcdir) \
	   `sdl2-config --cflags` \
	   `js/bin/js24-config --cflags` \
	   `pkg-config cairo --cflags` \
           -fvisibility=hidden \
           -ffunction-sections \
           -fdata-sections
CXXFLAGS = $(CFLAGS)

LDFLAGS  = -Xlinker --Map=burro.map \
	-Wl,--gc-sections -Wl,--as-needed -Wl,--demangle
#	-Wl,--print-gc-sections
LIBS     = `sdl2-config --libs` \
           `pkg-config cairo --libs` \
	    js/lib/libmozjs-24.a -ldl -lm -lz -lpthread


SRCS_C   = 
SRCS_CPP = \
	backdrop.cpp \
	bg.cpp \
	console.cpp \
	draw.cpp \
	eng.cpp \
	js.cpp \
	lineedit.cpp \
	loop.cpp \
	main.cpp \
	obj.cpp \
	tga.cpp \
	utf.cpp \
	xcairo.cpp \
	xsdl.cpp
SRCS_JS  = \
	script.js
SRCS     = $(SRCS_C) $(SRCS_CPP)
IMAGES   =
OBJS     = $(SRCS_C:.c=.o) $(SRCS_CPP:.cpp=.o) $(SRCS_JS:.js=.o) $(IMAGES:.tga=.o)
AUX      = const.h

.PHONY:  all
all:     burro
burro:   $(OBJS)
	$(CXX) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

%.o : %.js
	ld -r -b binary -o $@ $^
	objcopy --rename-section .data=.rodata,alloc,load,readonly,data,contents $@ $@

%.o : %.tga
	ld -r -b binary -o $@ $^
	objcopy --rename-section .data=.rodata,alloc,load,readonly,data,contents $@ $@

.PHONY:  clean
clean:
	rm -f *.o burro core
