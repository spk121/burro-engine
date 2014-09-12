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

CPPFLAGS = \
	-Wall \
	-Wunused-macros \
	-Wendif-labels \
	-pedantic \
	`sdl2-config --cflags` \
	`pkg-config cairo --cflags` \
	`pkg-config tinyxml --cflags` \
	`pkg-config libiso9660++ --cflags` \
	`pkg-config mozjs-24 --cflags `\
	-I$(srcdir) \
	-I$(srcdir)/tmxparser/src

DEFS     = -DSDL_ASSERT_LEVEL=2

CDEBUG   = -g3 -O0

CWARN = \
    -Wall \
    -Warray-bounds \
    -Wcast-align \
    -Wno-cast-qual \
    -Wextra \
    -Wmissing-declarations \
    -Wpointer-arith \
    -Wstrict-aliasing \
    -Wstrict-overflow=5 \
    -Wundef \
    -Wunreachable-code \
    -Winvalid-pch

CXXWARN = $(CWARN) -Weffc++ -Wsign-promo

CFLAGS   = $(CPPFLAGS) $(CWARN) $(CDEBUG) $(DEFS) \
    -fstrict-aliasing \
    -fstrict-overflow \
    -ftree-vrp \
    -fvisibility=hidden \
    -ffunction-sections \
    -fdata-sections

CXXFLAGS = $(CFLAGS) $(CXXWARN)

LDFLAGS  = -Xlinker --Map=burro.map \
	-Wl,--gc-sections -Wl,--as-needed -Wl,--demangle

LIBS     = `sdl2-config --libs` \
	`pkg-config cairo --libs` \
	`pkg-config tinyxml --libs` \
	`pkg-config libiso9660++ --libs` \
	`pkg-config mozjs-24 --cflags `\
	-L$(srcdir)/tmxparser/build -ltmxparser

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
	xiso9660.cpp \
	xsdl.cpp

SRCS_JS  = \
	script.js

SRCS     = $(SRCS_C) $(SRCS_CPP)
OBJS     = $(SRCS_C:.c=.o) \
	$(SRCS_CPP:.cpp=.o) \

AUX      = const.h

DATA = data/map1.tmx

.PHONY:  all
all:     burro burro.iso ../tools/font/bdf-converter

burro:   $(OBJS) burro.iso
	$(CXX) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

burro.iso: $(DATA)
	genisoimage -o $@ data

console.cpp : fixed8x13.h

nxfonts_myfont.h: ../tools/font/bdf-converter ../tools/font/8x13.bdf
	../tools/font/bdf-converter ../tools/font/8x13.bdf

fixed8x13.h: nxfonts_myfont.h
	cp nxfonts_myfont.h fixed8x13.h

../tools/font/bdf-converter: ../tools/font/bdf-converter.o
	$(CC) -o $@ ../tools/font/bdf-converter.o

# %.o : %.js
# 	ld -r -b binary -o $@ $^
# 	objcopy --rename-section .data=.rodata,alloc,load,readonly,data,contents \
# 		$@ $@

# %.o : %.tga
# 	ld -r -b binary -o $@ $^
# 	objcopy --rename-section .data=.rodata,alloc,load,readonly,data,contents \
# 		$@ $@

.PHONY:  clean
clean:
	rm -f *.o burro core

check-syntax:
	$(CC) $(CFLAGS) -fsyntax-only $(SRCS_C)
	$(CXX) -Wall -Wextra -pedantic -fsyntax-only $(SRCS_CPP)
