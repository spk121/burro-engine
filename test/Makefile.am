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
	GUILE_LOAD_PATH=$(abs_top_srcdir)/engine
TEST_EXTENSIONS = .scm .burro
SCM_LOG_COMPILER = \
	guile
AM_SCM_LOG_FLAGS = -L $(abs_top_srcdir)/src -L $(abs_top_srcdir) -s
BURRO_LOG_COMPILER = \
	$(top_builddir)/engine/burro
AM_BURRO_LOG_FLAGS = --load

SCMTESTS = \
 scm_test_setup.scm

BURROTESTS = \
 burro_test_setup.burro \
 burro_window_width.burro \
 burro_window_height.burro \
 burro_window_size.burro

TESTS = $(SCMTESTS) $(BURROTESTS)

DEMOS = 

EXTRA_DIST = $(TESTLIB) $(SCMTESTS) $(BURROTESTS) $(DEMOS)
