# Makefile -- the makefile for the gui

# Copyright 2014, Michael L. Gran

# This file is part of the Project Burro game engine.

# Project Burro is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# Project Burro is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Project Burro.	 If not, see
# <http://www.gnu.org/licenses/>. */

SHELL = /bin/sh

srcdir = .

################################################################
# FLAGS


################################################################
# TOP-LEVEL SOURCES

SRCS_JS	 = \
	main.js

MAPS_TMX = \
	map1.tmx

TILESHEETS_PNG = \
	cave.png

SPRITES_ASE = \
	man.ase face.ase

CONSOLE_FONTS = \
	tools/font/8x13.bdf
################################################################
# ALL

.PHONY:	 all
all:	 burro.iso

################################################################
# GENERATED SOURCES

sprites.png sprites.jsn: $(SPRITES_ASE)
	 aseprite --batch --sheet sprites.png --data sprites.jsn $(SPRITES_ASE)

################################################################
# OBJECTS

burro.iso: $(SRCS_JS) $(MAPS_TMX) $(TILESHEETS_PNG) sprites.png sprites.jsn
	genisoimage -o $@ $^


################################################################
# EXECUTABLES

################################################################
# MINOR TARGETS

.PHONY:	 clean
clean:
	-rm -f burro.iso sprites.png sprites.jsn

.PHONY: check-syntax

TAGS:
	etags $(SRCS_JS)


