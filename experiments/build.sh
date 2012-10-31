#!/bin/sh

 gcc -g -Wall -Wextra `pkg-config glib-2.0 gio-2.0 gstreamer-0.10 --cflags --libs` gstreamer-beep.c -o gstreamer-beep

