IDRIS = idris
RANLIB      ?=ranlib
AR          ?=ar

MACHINE         := $(shell $(CC) -dumpmachine)
ifneq (, $(findstring darwin, $(MACHINE)))
	OS      :=darwin
	SHLIB_SUFFIX    :=.dylib
else ifneq (, $(findstring cygwin, $(MACHINE)))
	OS      :=windows
	SHLIB_SUFFIX    :=.dll
else ifneq (, $(findstring mingw, $(MACHINE)))
	OS      :=windows
	SHLIB_SUFFIX    :=.dll
else ifneq (, $(findstring windows, $(MACHINE)))
	OS      :=windows
	SHLIB_SUFFIX    :=.dll
else
	OS      :=unix
	SHLIB_SUFFIX    :=.so
endif


LIBNAME=image
OBJS = $(LIBNAME).o
HDRS = $(LIBNAME).h
CFLAGS := $(CFLAGS)
IDRIS_SRCS = Day8.idr

ifneq ($(OS), windows)
	CFLAGS += -fPIC
endif

DYLIBTARGET = $(LIBNAME)$(SHLIB_SUFFIX)
LIBTARGET = $(LIBNAME).a
TARGET=`${IDRIS2} --libdir`

build: $(DYLIBTARGET)

$(DYLIBTARGET) : $(OBJS)
	$(CC) -o $(DYLIBTARGET) $(LIBFLAGS) -shared $(OBJS)

install:
	@if ! [ -d $(TARGET)/lib ]; then mkdir $(TARGET)/lib; fi
	install $(DYLIBTARGET) $(HDRS) $(TARGET)/lib

clean :
	rm -rf $(OBJS) $(LIBTARGET) $(DYLIBTARGET) build

$(OBJS): $(HDRS)

all: $(DYLIBTARGET)

.PHONY: install clean
