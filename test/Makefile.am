# Makefile.am for burro-engine/test
# Derived from guile-ncurses
#
# Copyright 2009, 2010, 2011, 2014, 2016 Free Software Foundation, Inc.
# Copyright 2018 Michael L. Gran
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

TESTLIB = automake-test-lib.scm

AM_TESTS_ENVIRONMENT = \
	GUILE_AUTO_COMPILE=0; export GUILE_AUTO_COMPILE; \
	GUILE_LOAD_PATH=$(abs_top_srcdir)/test
TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = \
	$(top_builddir)/src/burro

TESTS = \
 window_width.scm \
 window_height.scm


DEMOS = 

EXTRA_DIST = $(TESTLIB) $(TESTS) $(DEMOS)
