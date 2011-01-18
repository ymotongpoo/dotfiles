#
# Makefile for YaTeX
#

# Edit these variables to be suitable for your site
PREFIX	= /usr/local

## mule2
EMACS	= mule
EMACSDIR= ${PREFIX}/lib/${EMACS}
## emacs20
#EMACS	= emacs
#EMACSDIR= ${PREFIX}/share/${EMACS}
## XEmacs
#EMACS	= xemacs
#EMACSDIR= ${PREFIX}/lib/${EMACS}
## Meadow (Sample)
#EMACS	= meadow
#EMACSDIR = c:/usr/local/meadow

LISPDIR	= ${EMACSDIR}/site-lisp/yatex
# LISPDIR	= ${EMACSDIR}/site-packages/lisp/yatex
DOCDIR	= ${LISPDIR}/docs
HELPDIR	= ${EMACSDIR}/site-lisp
INFODIR	= ${PREFIX}/info

TAR	= tar
INSTALL	= install -c -m 444
MKDIR	= mkdir -p

# Comment out below if you are using Emacs Windows(meadow, etc)
GEO	= -geometry 80x20+0+0

###################
# Do not edit below
###################
# make install		to install YaTeX into public space
# make install-nw	same as above, but -nw mode, or Emacs18(Nemacs)
# make ajimi		to feel taste
# make ajimi-nw		same as above, but -nw mode
# make package		to create package for relase
# make yahtmlpack	to create package for relase
# make clean		to delete all producted files
# make ci		to check in all
# make co		to check out all
MVER	= 1.72
LISP	= ${LISP18} ${LISP19}
YAHTML	= yahtml.el
COMMON	= yatexlib.el yatexprc.el
LISP18	= comment.el yatex.el yatexadd.el yatexgen.el yatexenv.el \
	  ${COMMON} \
	  yatexmth.el yatexhks.el yatexhlp.el \
	  yatexm-o.el yatexsec.el  yatexhie.el yatexpkg.el ${YAHTML}
LISP19	= yatex19.el
DOCS	= ${DOCSRC} ${DOCOBJ} ${NEWS}
NEWS	= yatex.new
DOCHTML	= docs/htmlqa docs/htmlqa.eng docs/yahtmlj.tex docs/yahtmle.tex
DOCSRC	= docs/yatexj.tex docs/yatexe.tex \
	  docs/yatex.ref docs/yatexref.eng \
	  docs/yatexadd.doc docs/yatexgen.doc \
	  docs/qanda docs/qanda.eng ${DOCHTML}
DOCOBJ	= docs/yatexj docs/yatexe docs/yahtmlj docs/yahtmle
HELP	= help/YATEXHLP.jp help/YATEXHLP.eng
MANIFEST= manifest
EXTRA	= dir install 00readme makefile readme.meadow.j
DISTRIB = ${EXTRA} ${LISP} ${DOCS} ${MANIFEST} ${HELP}
RCSFILE	= ${LISP} ${NEWS} ${DOCSRC} ${HELP}
YAHTMLLISP = ${YAHTML} ${COMMON}
YAHTMLDIST = ${YAHTMLLISP} install 00readme makefile
PACK	= `ls ${DISTRIB}`
TMPDIR	= /tmp
VERSION = `head yatex.el|awk '/rev\./{print $$4}'`
PACKDIR	= ${TMPDIR}/yatex${VERSION}

all:
	@echo "Edit this makefile first."
	@echo 'Type "make install" to install YaTeX.'
	@echo 'Type "make install-yahtml" to install yahtml.'
	@echo 'If you cling to elc files. type "make elc" before make install'
#	@echo "If you don't use X-clinet of Emacs,"
#	@echo 'type "make install-nw" instead.'

install: install-real
#install-yahtml: bytecompile-yahtml
install-yahtml:
	[ -d ${LISPDIR} ] || mkdir ${LISPDIR}
	for f in *.el; do \
	 rm -f ${LISPDIR}/$${f}c; \
	done

	${INSTALL} *.el* ${LISPDIR}

install-real:
	if [ ! -d ${LISPDIR} ]; then ${MKDIR} ${LISPDIR}; fi
	if [ ! -d ${DOCDIR} ]; then ${MKDIR} ${DOCDIR}; fi
	if [ ! -d ${INFODIR} ]; then ${MKDIR} ${INFODIR}; fi
	for f in *.el; do \
	 rm -f ${LISPDIR}/$${f}c; \
	done
	${INSTALL} *.el* ${NEWS} ${LISPDIR}
	${INSTALL} ${DOCSRC} ${DOCDIR}
	${INSTALL} ${DOCOBJ} ${INFODIR}
	${INSTALL} ${HELP} ${HELPDIR}
	@echo "Add next few lines into your site's info dir manually please!"
	@cat dir

install-nw: bytecompile-nw install-real

elc:	bytecompile

bytecompile: lp
	if [ "$$DISPLAY"x = ""x ]; then \
		echo "Set DISPLAY environment variable!!"; exit 1; fi
	${EMACS} -q ${GEO} -l ./yatexlib.el -e bcf-and-exit ${LISP}

bytecompile-nw: lp1
	${EMACS} -batch -l ./yatexlib.el -e batch-byte-compile ${LISP18}

bytecompile-yahtml:
	if [ "$$DISPLAY"x = ""x ]; then \
		echo "Set DISPLAY environment variable!!"; exit 1; fi
	${EMACS} -q -g 80x20+0+0 -l ./yatexlib.el -e bcf-and-exit ${YAHTMLLISP}

lp:
	echo '(setq load-path (cons "." load-path))'	> lp.el
	echo '(load-file "./yatexlib.el")'		>>lp.el

lp1:	lp
	echo '(load-file "./yatex.el")'			>>lp.el
	echo '(load-file "./comment.el")'		>>lp.el

lp2:
	echo '(setq load-path (cons "'`pwd`'" load-path))'		>>lp.el
	echo '(setq auto-mode-alist'					>>lp.el
	echo '(cons (cons "\\.tex" '"'yatex-mode) auto-mode-alist))"	>>lp.el
	echo '(load-library "yatex")'					>>lp.el

ajimi: lp lp2
	${EMACS} -l ./lp.el -e yatex-mode

ajimi-nw: lp lp2
	${EMACS} -nw -l ./lp.el -e yatex-mode

clean:
	rm -f *.elc *~ lp.el

info: docs/yatexj docs/yatexe docs/yahtmlj docs/yahtmle

docs/yatexj: docs/yatexj.tex
	(cd docs; ${EMACS} -batch yatexj.tex -e texinfo-format-buffer \
	 -e basic-save-buffer)

docs/yatexe: docs/yatexe.tex
	(cd docs; ${EMACS} -batch yatexe.tex -e texinfo-format-buffer \
	 -e basic-save-buffer)

docs/yahtmlj: docs/yahtmlj.tex
	(cd docs; ${EMACS} -batch yahtmlj.tex -e texinfo-format-buffer \
	 -e basic-save-buffer)

docs/yahtmle: docs/yahtmle.tex
	(cd docs; ${EMACS} -batch yahtmle.tex -e texinfo-format-buffer \
	 -e basic-save-buffer)

package: info
	@-mkdir ${PACKDIR}
	@tar cf - ${PACK} | (cd ${PACKDIR}; tar xf -)
	( version=${VERSION}; cd ${TMPDIR}; \
	     ${TAR} vzcf ${TMPDIR}/yatex$$version.tar.gz yatex$$version)

yahtmlpack:
	@-mkdir ${PACKDIR}
	@tar cf - ${YAHTMLDIST} | (cd ${PACKDIR}; tar xf -)
	( version=${VERSION}; cd ${TMPDIR}; \
	     ${TAR} vzcf ${TMPDIR}/yahtml$$version.tar.gz yatex$$version)

ci:
	ci -r${VERSION} -sRel -f ${RCSFILE}
	ci -u${VERSION} makefile 00readme

co:
	co ${RCSFILE}

co-l:
	co -l ${RCSFILE}

tci:
	ci -l${VERSION}.0 -Ncurrent ${RCSFILE} makefile

dostci:
	ci -l${MVER}.0 -Ncurrent @rcsfile

gohome:
	zip -u -r /com/okoma/yuuji/tmp/dosconv/yatex.zip . \
	-x '*RCS/*' -x 'texinfo/*'

RSYNCDIR	= ${HOME}/http/yatex/rsync/yatex
sync:	
	@-mkdir ${PACKDIR}
	@tar cf - ${PACK} | (cd ${PACKDIR}; tar xf -)
	syncdir -A ${PACKDIR} ${RSYNCDIR}
	rm -rf ${PACKDIR} 
