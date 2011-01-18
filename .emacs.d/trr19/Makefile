# Makefile - for the TRR19 distribution.
# Last modified on Sun Jun 30 03:10:31 1996

# Edit the Makefile, type `make', and follow the instructions.

##----------------------------------------------------------------------
##  You MUST edit the following lines
##----------------------------------------------------------------------

# Your Full name or E-mail address
installer = yamagus@kw.netlaputa.or.jp

# Default message type (t or nil)
japanese = t

# Where TRR directory is found
trrdir = /usr/local/share/emacs/site-lisp/trr

# Where info files go.
infodir = /usr/local/info
#infodir = /usr/local/lib/emacs/info

# Where TRR binary files go.
bindir = /usr/local/bin

# Name of your emacs binary
#EMACS = emacs
EMACS = mule

# Which C Compiler will you use?
CC = gcc

# What Options will you use?
OPTIONS = -O2 -DHAVE_STRING_H -DHAVE_SYS_TIME_H -DHAVE_FCNTL_H
# -DHAVE_SYS_FCNTL_H

# Name of your sed
SED = /usr/bin/sed
#SED = /bin/sed

# Name of your grep
GREP = /usr/bin/grep
#GREP = /bin/grep

##----------------------------------------------------------------------
## You MAY need to edit these
##----------------------------------------------------------------------

# BSD compatible install
INSTALL = ./install-sh
#INSTALL = /usr/bin/install
#INSTALL = /usr/ucb/install

# Using emacs in batch mode.
BATCH = $(EMACS) -batch -q -no-site-file

# Specify the byte-compiler for compiling TRR files
ELC= $(BATCH) -f batch-byte-compile

# Specify makeinfo program
MAKEINFO = $(EMACS) -batch -l texinfmt -funcall batch-texinfo-format 
#MAKEINFO = makeinfo

LDFLAGS =

##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

.SUFFIXES:
.SUFFIXES: .el .elc .c .o .texi .info

TEXT_DIR = $(trrdir)/text/
RECORD_DIR = $(trrdir)/record/

CFLAGS = $(OPTIONS) -DTEXT_DIR=\"$(TEXT_DIR)\" -DRECORD_DIR=\"$(RECORD_DIR)\" \
	-DSED=\"$(SED)\" -DGREP=\"$(GREP)\"

SHELL = /bin/sh

TRRSRC = trr.el trr-mesg.el trr-files.el trr-menus.el \
	 trr-graphs.el trr-sess.el

TRRELC = trr.elc trr-mesg.elc trr-files.elc trr-menus.elc \
	 trr-graphs.elc trr-sess.elc

SOURCES = trr_update.c trr_format.c

SUBPROGS = trr_update trr_format

CONTENTS = CONTENTS

TEXIFILES = trr.texi

INFO = trr.info

TEXTS = The_Constitution_Of_JAPAN Constitution_of_the_USA

EXTRAFILES = Makefile README.euc ChangeLog

first:
	@echo ""
	@echo "	 First of all, edit the Makefile to suit your needs."
	@echo "	 Then run:"
	@echo
	@echo "	  make all"
	@echo
	@echo "	 and follow the instructions."
	@echo

all: main

main: elc $(SUBPROGS)

install: main install-dir
	for i in $(SUBPROGS); do $(INSTALL) -c -m 6755 $$i $(bindir); done
	for i in $(TRRELC) CONTENTS; \
		do $(INSTALL) -c -m 644 $$i $(trrdir); done
	(cd text; for i in $(TEXTS); \
			do $(INSTALL) -c -m 644 $$i $(trrdir)/text; done)
	$(INSTALL) -c -m 644 $(INFO) $(infodir)
	@echo 
	@echo "** TRR installation is almost completed."
	@echo "**"
	@echo "** Now edit \`CONTENTS' in the directory where you put"
	@echo "** TRR files ($(trrdir)/), and insert"
	@echo "** (autoload 'trr \"$(trrdir)/trr\" nil t)"
	@echo "** in your \`.emacs' or \`site-start.el' file."
	@echo "** then edit $(infodir)/dir to add TRR entry."
	@echo "**"
	@echo "** You may want to add some texts to TRR text directory."
	@echo "** Put them into $(trrdir)/text and edit \`CONTENTS'"
	@echo

install-dir:
	if [ ! -d $(bindir) ]; \
	then rm -f $(bindir); \
	     mkdir $(bindir); \
	     chmod 755 $(bindir); \
	fi
	if [ ! -d $(trrdir) ]; \
	then rm -f $(trrdir); \
	     mkdir $(trrdir); \
	     chmod 755 $(trrdir); \
	fi
	if [ ! -d $(trrdir)/text ]; \
	then rm -f $(trrdir)/text; \
	     mkdir $(trrdir)/text; \
	     chmod 755 $(trrdir)/text; \
	fi
	if [ ! -d $(trrdir)/record ]; \
	then rm -f $(trrdir)/record; \
	     mkdir $(trrdir)/record; \
	     chmod 755 $(trrdir)/record; \
	fi
	if [ ! -d $(infodir) ]; \
	then rm -f $(infodir); \
	     mkdir $(infodir); \
	     chmod 755 $(infodir); \
	fi

.texi.info:
	$(MAKEINFO) $<

info: $(TEXIFILES)

trr.el: trr.el.base
	rm -f $@
	$(SED)  -e 's,TRRELCINSTALLDIR,$(trrdir),' \
		-e 's,TRRBININSTALLDIR,$(bindir),' \
		-e 's,TRRINSTALLER,$(installer),' \
		-e 's,TRRDEFAULTJAPANESE,$(japanese),' trr.el.base > $@

.el.elc:
	$(ELC) $<

elc: $(TRRELC)

.c.o:
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $<

trr_format: trr_format.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ trr_format.o

trr_update: trr_update.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ trr_update.o

clean:
	rm -f *.o *~ $(TRRELC) $(SUBPROGS) trr.el

distclean: clean
	rm -f *.info
