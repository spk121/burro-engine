# DEFS
#  GTK3 - using GTK3 and Pango for video and GStreamer for audio
GTK_VERSION=2
GSTREAMER_VERSION=0.10

DEFS = -I. -DBURRO
LIBS = 	`pkg-config gstreamer-0.10 --libs` \
	-ldl
CFLAGS = $(DEFS) \
	`pkg-config gstreamer-0.10 --cflags`

ifeq ($(GTK_VERSION), 3)
CFLAGS += `pkg-config gtk+-3.0 --cflags`
DEFS += -DGTK3
LIBS += `pkg-config gtk+-3.0 --libs`
endif

ifeq ($(GTK_VERSION), 2)
CFLAGS += `pkg-config gtk+-2.0 --cflags`
DEFS += -DGTK2
LIBS += `pkg-config gtk+-2.0 --libs`
endif

ifeq ($(GSTREAMER_VERSION), 0.10)
CFLAGS += `pkg-config gstreamer-0.10 --cflags`
endif

DEFS += -DGSTREAMER_010
CFLAGSD = $(CFLAGS) -g3 -gdwarf-2 -O0 -Wall -DDEBUG -UNDEBUG
CFLAGS += $(DEFS) -O2 -DNDEBUG -UDEBUG


# The source code files, relative to this directory
CFILES = main.c dl.c engine.c
CPPFILES = 

# The debug versions of the object files
COBJD = $(addprefix obj/debug/,$(patsubst %.c,%.o,$(CFILES)))
CPPOBJD = $(addprefix obj/debug/,$(patsubst %.cpp,%.o,$(CPPFILES)))

# The release versions of the object files
COBJ = $(addprefix obj/release/,$(patsubst %.c,%.o,$(CFILES)))
CPPOBJ = $(addprefix obj/release/,$(patsubst %.cpp,%.o,$(CPPFILES)))

.PHONY: debug release
all: debug release

debug: test/burrod

release: bin/burro

# This is the debug version of the FauxDS Executable
test/burrod: $(COBJD) $(CPPOBJD)
	g++ $^ $(LIBS) -o $@ 

# This is the release version of the FauxDX executable
bin/burro: $(COBJ) $(CPPOBJ)
	g++ $^ $(LIBS) -o $@ 

$(COBJD): $(CFILES)
	gcc $(CFLAGSD) -c \
	$(patsubst obj/debug/%,./%,$(patsubst %.o,%.c,$@)) -o $@

$(COBJ): $(CFILES)
	gcc $(CFLAGS) -c \
	$(patsubst obj/release/%,./%,$(patsubst %.o,%.c,$@)) -o $@

$(CPPOBJD): $(CPPFILES)
	g++ $(CFLAGSD) -c \
	$(patsubst obj/debug/%,./%,$(patsubst %.o,%.cpp,$@)) -o $@

$(CPPOBJ): $(CPPFILES)
	g++ $(CFLAGS) -c \
	$(patsubst obj/release/%,./%,$(patsubst %.o,%.cpp,$@)) -o $@


clean:
	rm $(COBJ) $(COBJD) $(CPPOBJ) $(CPPOBJD) bin/burro 2test/burrod
