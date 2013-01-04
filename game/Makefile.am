pkgdata_DATA = main.scm
CLEANFILES = main.scm

EXTRA_DIST = main.scm.m4

main.scm: main.scm.m4 Makefile
	m4 \
	--prefix-builtins \
	--debug \
	--define=GUILE_VERSION=${GUILEVER} \
	main.scm.m4 > main.scm

