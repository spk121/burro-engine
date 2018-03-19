all:
	 gcc --std=gnu11 -Wall -Wextra -O0 -g3 -gdwarf-2 `pkg-config gtk+-3.0 --cflags --libs` -I.. \
	io.c \
	common.c \
	targa.c \
	targatool.c \
	palettize.c -o targatool
