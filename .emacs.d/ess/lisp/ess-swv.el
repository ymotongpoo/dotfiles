;; Copyright (C) 2005 David Whiting, A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: David Whiting <david.whiting@ncl.ac.uk>
;; Created: 15 April 2005
;; Maintainers: ESS-core <ESS-core@stat.math.ethz.ch>

;; Keywords: Noweb, Literate Statistical Practice, Sweave

;; This file is part of ESS.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;
;; Some simple functions for ESS and Sweave
;; david.whiting at ncl.ac.uk
;; Wed Sep 1 14:55:52 CEST 2004

;; I have written some very simple elisp functions that I have found
;; useful and thought others might like to see. I dabble with elisp
;; occasionally at best so there are probably better ways to do what I
;; have done, but so far this seems to work for me. There are several
;; things that are hard-coded, I use Linux and I think that this would
;; probably not work in Windows (not as it is now anyway).

;; With these functions and key bindings all I need to do is open a .Rnw
;; file and, assuming R is running, I press:

;; M-n s to Sweave the file, then
;; M-n l to run latex on the results of Sweave, then
;; M-n p to make and display a postscript file , or
;; M-n P to make and display a PDF version.

;; David  Whiting to Anthony Rossini,  Mar 30
;; On Wed, Mar 30, 2005 at 11:51:26AM +0200, A.J. Rossini wrote:
;; > I'm going to go ahead and add this stuff to the distribution tonight
;; > if you don't mind.  I'd forgotten about it!
;; It would make me very happy indeed.
;; > (however, need permission to do so).
;; Permission granted!

;; Dave
;; --
;; David Whiting
;; School of Clinical Medical Sciences, The Medical School
;; University of Newcastle upon Tyne, NE2 4HH, UK.


;;; TODO:
;;;
;;; 1. I want to be able to send ess-swv-latex a parameter to tell it
;;; the number of times to run LaTeX (to get references updated
;;; correctly).
;;;
;;; 2. Also need to add ess-swv-Bibtex.
;;;
;;; 3. Might be good to have a way to chain commands.
;;
;; 4. ADD to the ../doc/ess.texi !!
;;
;; 5. add a sub-menu [Sweave] to the [Noweb] menu to teach users about
;;     the possibilities

;;; Autoloads and Requires

(require 'ess-noweb)

;; MM: I think we should *not* require 'cl, but it's needed for
;;     (search .) below ... -> ``please'' replace the (search ...) parts
(require 'cl)

(defun ess-swv-run-in-R (cmd)
   "Run \\[cmd] on the current .Rnw file.  Utility function not called by user."
   (ess-force-buffer-current "Process to load into: ")
   (save-excursion
     (ess-execute (format "require(tools)"));; Make sure tools is loaded.
     (let* ((this-buf (current-buffer))
	    (sprocess (get-ess-process ess-current-process-name))
	    (sbuffer (process-buffer sprocess))
	    (this-file (buffer-file-name))
	    (Rnw-dir (file-name-directory this-file))
	    (Sw-cmd
	     (format
	      "local({..od <- getwd(); setwd(%S); %s(%S); setwd(..od) })"
	      Rnw-dir cmd this-file))
	    )
       (message "%s()ing %S" this-file)
       (ess-execute Sw-cmd 'buffer nil nil)
       (ess-show-buffer (buffer-name sbuffer) nil))))

(defun ess-swv-tangle ()
   "Run Stangle on the current .Rnw file."
   (interactive)
   (ess-swv-run-in-R "Stangle"))

(defun ess-swv-weave ()
   "Run Sweave on the current .Rnw file."
   (interactive)
   (ess-swv-run-in-R "Sweave"))

(defun ess-swv-weave ()
   "Run Sweave on the current .Rnw file."
   (interactive)
   (ess-force-buffer-current "Process to load into: ")
   (save-excursion
     (ess-execute (format "require(tools)"));; Make sure tools is loaded.
     (let* ((this-buf (current-buffer))
	    (sprocess (get-ess-process ess-current-process-name))
	    (sbuffer (process-buffer sprocess))
	    (this-file (buffer-file-name))
	    (Rnw-dir (file-name-directory this-file))
	    (Sw-cmd
	     (format
	      "local({..od <- getwd(); setwd(%S); Sweave(%S); setwd(..od) })"
	      Rnw-dir this-file))
	    )
       (message "Sweaving %S" this-file)
       (ess-execute Sw-cmd 'buffer nil nil)
       (ess-show-buffer (buffer-name sbuffer) nil))))


(defun ess-swv-latex ()
   "Run LaTeX on the product of Sweave()ing the current file."
   (interactive)
   (save-excursion
     (let* ((thisbuffer (buffer-name))
	    (namestem (substring (buffer-name) 0 (search ".Rnw" (buffer-name))))
	    (latex-filename (concat namestem ".tex"))
	    (tex-buf (get-buffer-create " *ESS-tex-output*")))
       (message "Running LaTeX on '%s' ..." latex-filename)
       (switch-to-buffer tex-buf)
       (call-process "latex" nil tex-buf 1 latex-filename)
       (switch-to-buffer thisbuffer)
       (display-buffer tex-buf)
       (message "Finished running LaTeX" ))))


;;-- trying different viewers; thanks to a patch from Leo <sdl@web.de> ---

(defun ess-swv-PS ()
   "Create a postscript file from a dvi file (name based on the current
Sweave file buffer name) and display it."
   (interactive)
   (let* ((namestem (substring (buffer-name) 0 (search ".Rnw" (buffer-name))))
	 (dvi-filename (concat namestem ".dvi"))
	 (psviewer (file-name-nondirectory
		    (or (executable-find "gv")
			(executable-find "evince")
			(executable-find "kghostview")))))
     (shell-command (concat "dvips -o temp.ps " dvi-filename))
     (shell-command (concat psviewer " temp.ps & "))))


(defun ess-swv-PDF ()
   "Create a PDF file ('pdflatex') and display it."
   (interactive)
   (let* ((namestem (substring (buffer-name) 0 (search ".Rnw" (buffer-name))))
	 (tex-filename (concat namestem ".tex"))
	 (pdfviewer (file-name-nondirectory
		     (or (executable-find "evince")
			 (executable-find "kpdf")
			 (executable-find "xpdf")
			 (executable-find "acroread")))))
     (shell-command (concat "pdflatex " tex-filename))
     (shell-command (concat pdfviewer " " namestem ".pdf &"))))

(defun ess-insert-Sexpr ()
 "Insert Sexpr{} into the buffer at point."
 (interactive)
 (insert "\\Sexpr{}")
 (backward-char))


;;; back-compatible wrappers:
(defun ess-makeSweave () "old *DEPRECATED* version of \\[ess-swv-weave]."
 (interactive) (ding)
 (message
  "** warning: ess-makeSweave is deprecated. Do use (ess-swv-weave) instead!")
 (ess-swv-weave))

(defun ess-makeLatex () "old *DEPRECATED* version of \\[ess-swv-latex]."
 (interactive) (ding)
 (message
  "** warning: ess-makeLatex is deprecated. Do use (ess-swv-latex) instead!")
 (ess-swv-latex))

(defun ess-makePS () "old *DEPRECATED* version of \\[ess-swv-PS]."
 (interactive) (ding)
 (message
  "** warning: ess-makePS is deprecated. Do use (ess-swv-PS) instead!")
 (ess-swv-PS))

(defun ess-makePDF () "old *DEPRECATED* version of \\[ess-swv-PDF]."
 (interactive) (ding)
 (message
  "** warning: ess-makePDF is deprecated. Do use (ess-swv-PDF) instead!")
 (ess-swv-PDF))


;;; Now bind some keys.
(define-key noweb-minor-mode-map "\M-ns" 'ess-swv-weave)
(define-key noweb-minor-mode-map "\M-nT" 'ess-swv-tangle)
(define-key noweb-minor-mode-map "\M-nl" 'ess-swv-latex)
(define-key noweb-minor-mode-map "\M-np" 'ess-swv-PS)
(define-key noweb-minor-mode-map "\M-nP" 'ess-swv-PDF)

(define-key noweb-minor-mode-map "\M-nx" 'ess-insert-Sexpr)



 ; provides

(provide 'ess-swv)

 ; Local variables section

;;; This file is automatically placed in Outline minor mode.
;;; The file is structured as follows:
;;; Chapters:     ^L ;
;;; Sections:    ;;*;;
;;; Subsections: ;;;*;;;
;;; Components:  defuns, defvars, defconsts
;;;              Random code beginning with a ;;;;* comment

;;; Local variables:
;;; mode: emacs-lisp
;;; outline-minor-mode: nil
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-swv.el ends here
