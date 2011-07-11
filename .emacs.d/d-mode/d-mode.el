;;; d-mode.el --- d-mode

;; Copyright (C) 2003

;; Authors: 2003 Ben Hinkle bhinkle4@juno.com
;; Version: 1.1

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Loosely based on version 1.12 of csharp-mode.el by Dennis Haney
;; http://davh.dk/script/
;; and on the font-lock.el definitions for C and C++.

;; To activate put this in your .emacs:

;; (autoload 'd-mode "d-mode" 
;;   "Major mode for editing D code." t)
;; (setq auto-mode-alist (cons '( "\\.d\\'" . d-mode ) auto-mode-alist ))

;; Put this file in your site-lisp emacs directory.
;; Load it into emacs and run
;;  M-x byte-compile-file RET RET 
;; to compile it into a .elc file

;; TODO: 
;;  nested comments /+ +/
;;  ` ` strings
;;  cc-mode wants "else" clause of "version" to have matching "if"
;;  import c.stdio doesn't highlight stdio
;;  enum with storage class 
;;  probably lots of other stuff

(provide 'd-mode)

(require 'cc-mode)
(require 'font-lock)

(eval-when-compile
  (require 'cc-mode)
  (require 'font-lock)
  (defmacro c-paren-re (re)
    `(concat "\\(" ,re "\\)"))
  (defmacro c-identifier-re (re)
    `(concat "\\[^_]"))
  )

;;xemacs fix -- from c-sharp mode
(if (not (facep 'font-lock-constant-face))
    (copy-face 'font-lock-reference-face 'font-lock-constant-face))
(if (not (facep 'font-lock-builtin-face))
    (copy-face 'font-lock-keyword-face 'font-lock-builtin-face))

;; Primitive type keywords.
(defconst c-D-primitive-type-kwds
  (concat "bit\\|byte\\|ubyte\\|char\\|"
	  "double\\|float\\|int\\|long\\|"
	  "ubyte\\|short\\|uint\\|ulong\\|ushort\\|cent\\|"
	  "ucent\\|real\\|ireal\\|ifloat\\|"
	  "creal\\|cfloat\\|cdouble\\|" 
	  "wchar\\|dchar\\|void"
	  ))

;; Declaration specifier keywords.
(defconst c-D-specifier-kwds
  (concat "const\\|extern\\|static\\|volatile\\|"
	  "final\\|synchronized\\|deprecated\\|"
	  "private\\|auto\\|final\\|override\\|abstract"
	  ))

;; Class/struct declaration keywords.
(defconst c-D-class-kwds "class\\|struct\\|union\\|interface")

;; Keywords introducing other declaration-level blocks.
(defconst c-D-extra-toplevel-kwds "extern\\|version\\|debug")

;; Keywords introducing other declaration-level constructs.
(defconst c-D-other-decl-kwds 
  "enum\\|typedef\\|alias\\|template\\|import\\|module\\|instance")

;; Protection label keywords in classes.
(defconst c-D-protection-kwds
  "public\\|protected\\|private\\|export\\|in\\|out")

;; Statement keywords followed directly by a block.
(defconst c-D-block-stmt-1-kwds
  "do\\|else\\|try\\|finally\\|asm\\|unittest\\|override\\|body")

;; Statement keywords followed by a paren sexp and then by a block.
(defconst c-D-block-stmt-2-kwds
  (concat "foreach\\|for\\|if\\|while\\|switch\\|catch\\|"
	  "with\\|synchronized\\|invariant\\|version"))

;; Statement keywords followed by an expression or nothing.
(defconst c-D-simple-stmt-kwds
  (concat "break\\|continue\\|goto\\|return\\|throw\\|"
	  "synchronized\\|assert\\|cast"))

;; Keywords introducing labels in blocks.
(defconst c-D-label-kwds  "case\\|default")

;; Keywords that can occur anywhere in expressions.
(defconst c-D-expr-kwds
  "new\\|this\\|throw\\|super\\|in\\|delete")

;; All keywords.
(defconst c-D-keywords
  (concat c-D-primitive-type-kwds "\\|" c-D-specifier-kwds
	  "\\|" c-D-class-kwds "\\|" c-D-extra-toplevel-kwds
	  "\\|" c-D-other-decl-kwds
	  ;; "\\|" c-D-decl-level-kwds
	  "\\|" c-D-protection-kwds
	  "\\|" c-D-block-stmt-1-kwds "\\|" c-D-block-stmt-2-kwds
	  "\\|" c-D-simple-stmt-kwds "\\|" c-D-label-kwds
	  "\\|" c-D-expr-kwds))

(defconst c-D-attrib-key (concat "\["
				 "\\s *" c-symbol-key "\\(\\s *([^)]*)\\)?\\s *"
				 "\\(?:,"
				 "\\s *" c-symbol-key "\\(\\s *([^)]*)\\)?\\s *"
				 "\\)*\]"))
(defconst c-D-protection-key
  (concat "\\<" (c-paren-re c-D-protection-kwds) "\\>"))

;; Regexps introducing class definitions.
(defconst c-D-class-key
  (concat
   "\\(" c-D-attrib-key "\\s +\\)*"
   "\\(" c-D-protection-key "\\s +\\)*"
   "\\(" c-D-class-kwds "\\)\\s +"
   c-symbol-key                       ;name of the class
   "\\(\\s *:\\s *" c-symbol-key	;maybe followed by parent(s)
   "\\(\\s *,\\s *" c-symbol-key "\\)*"
   "\\)?"
   ))

(defconst c-D-extra-toplevel-key (c-paren-re c-D-extra-toplevel-kwds))

;; regexp describing access protection clauses.  language specific
(defconst c-D-access-key 
  (concat "\\<\\(" c-D-protection-kwds "\\)\\>[ \t]*:"
          ))

;; keywords introducing conditional blocks
(defconst c-D-conditional-key 
  (c-identifier-re
   (concat c-D-block-stmt-1-kwds "\\|" c-D-block-stmt-2-kwds)
   ))

;; regular expression for // and /* comments
(defconst c-D-comment-start-regexp "/[/*]")

(defconst c-D-inexpr-class-key "\\<new\\>")

(defvar d-mode-abbrev-table nil
  "Abbreviation table used in d-mode buffers.")
(define-abbrev-table 'd-mode-abbrev-table ())

(c-add-style "D"
 '("Java"
   (c-basic-offset . 4)
   (c-comment-only-line-offset . (0 . 0))
   ))

;;;###autoload
(defvar d-mode-syntax-table nil
  "Syntax table used in d-mode buffers.")
(if d-mode-syntax-table
    nil
  (setq d-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table d-mode-syntax-table))

(defcustom d-mode-hook nil
  "*Hook called by `d-mode'."
  :type 'hook
  :group 'c)

(defcustom d-font-lock-extra-types
  '()
  "*List of extra types to fontify in D mode.
Each list item should be a regexp not containing word-delimiters.
For example, a value of (\"System\") means the word string is treated as a type
name.

The value of this variable is used when Font Lock mode is turned on."
  :type 'font-lock-extra-types-widget
  :group 'font-lock-extra-types)

;;; D font-lock support (see font-lock.el settings for C and C++)

(defconst d-font-lock-keywords-1 nil
  "Subdued level highlighting for D mode.")

(defconst d-font-lock-keywords-2 nil
  "Medium level highlighting for D mode.
See also `d-font-lock-extra-types'.")

(defconst d-font-lock-keywords-3 nil
  "Gaudy level highlighting for D mode.
See also `d-font-lock-extra-types'.")

(let* ((d-type-specs
	(eval-when-compile
	  (regexp-opt '("enum" "struct" "union" "class" "interface"))))
       (d-type-specs-depth
	(regexp-opt-depth d-type-specs))
       (d-type-names
	`(mapconcat 'identity
	  (cons
	   ,c-D-primitive-type-kwds
	   d-font-lock-extra-types)
	  "\\|"))
       (d-type-names-depth
	`(regexp-opt-depth ,d-type-names))
       (d-preprocessor-directives "line")
       (d-preprocessor-directives-depth
	(regexp-opt-depth d-preprocessor-directives)))
 (setq d-font-lock-keywords-1
  (list
   ;;
   ;; Fontify class names.
   '("\\<\\(class\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-type-face nil t))
   ;;
   ;; These are all anchored at the beginning of line for speed.
   ;; Note that `c++-font-lock-keywords-1' depends on `c-font-lock-keywords-1'.
   ;;
   ;; Fontify otherwise as symbol names, and the preprocessor directive names.
   (list
    "#[ \t]*\\(line\\)\\>[ \t!]*\\(\\sw+\\)?"
    '(1 font-lock-builtin-face)
    (list (+ 2 d-preprocessor-directives-depth)
	  'font-lock-variable-name-face nil t))))

 (setq d-font-lock-keywords-2
  (append d-font-lock-keywords-1
   (list
    ;;
    ;; Simple regexps for speed.
    ;;
    ;; Fontify all type names.
    `(eval .
      (cons (concat "\\<\\(" ,d-type-names "\\)\\>") 'font-lock-type-face))
    ;;
    ;; Fontify all builtin keywords (except case, default and goto; see below).
    (concat "\\<\\(" c-D-keywords "\\|" d-type-specs "\\)\\>")
    ;;
    ;; Fontify case/goto keywords and targets, and case default/goto tags.
    '("\\<\\(case\\|goto\\)\\>"
      (1 font-lock-keyword-face)
      ("\\(-[0-9]+\\|\\sw+\\)"
       ;; Return limit of search.
       (save-excursion (skip-chars-forward "^:\n") (point))
       nil
       (1 font-lock-constant-face nil t)))
    ;; Anders Lindgren <andersl@andersl.com> points out that it is quicker to
    ;; use MATCH-ANCHORED to effectively anchor the regexp on the left.
    ;; This must come after the one for keywords and targets.
    '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*$"
	   (beginning-of-line) (end-of-line)
	   (1 font-lock-constant-face)))
    )))

 (setq d-font-lock-keywords-3
  (append d-font-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   ;; We still have to fontify type specifiers individually, as C is so hairy.
   (list
    ;;
    ;; Fontify all storage types, plus their items.
    `(eval .
      (list (concat "\\<\\(" ,d-type-names "\\|import\\)\\>"
		    "\\([ \t*&]+\\sw+\\>\\)*")
	    ;; Fontify each declaration item.
	    (list 'font-lock-match-c++-style-declaration-item-and-skip-to-next
		  ;; Start with point after all type specifiers.
		  (list 'goto-char (list 'or
					 (list 'match-beginning
					       (+ ,d-type-names-depth 2))
					 '(match-end 1)))
		  ;; Finish with point after first type specifier.
		  '(goto-char (match-end 1))
		  ;; Fontify as a variable or function name.
		  '(1 (if (match-beginning 2)
			  font-lock-function-name-face
			font-lock-variable-name-face)))))
    ;;
    ;; Fontify all storage specs and types, plus their items.
    `(eval .
      (list (concat "\\<\\(" ,d-type-specs "\\)\\>"
		    "[ \t]*\\(\\sw+\\)?")
	  (list 1 'font-lock-keyword-face)
	  (list ,(+ d-type-specs-depth 2) 'font-lock-type-face nil t)
	  (list 'font-lock-match-c++-style-declaration-item-and-skip-to-next
		  nil 
		  ;; Finish with point after the variable name if
		  ;; there is one.
		  `(if (match-end 2) 
		       (goto-char (match-end 2)))
		  ;; Fontify as a variable or function name.
		  '(1 (if (match-beginning 2)
			  font-lock-function-name-face
			font-lock-variable-name-face) nil t))))
    ;;
    ;; Fontify structures, or typedef names, plus their items.
    '("\\(}\\)[ \t*]*\\sw"
      (font-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (match-end 1)) nil
       (1 font-lock-type-face)))
    ;;
    ;; Fontify anything at beginning of line as a declaration or definition.
    '("^\\(\\sw+\\)\\>\\([ \t*]+\\sw+\\>\\)*"
      (1 font-lock-type-face)
      (font-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (or (match-beginning 2) (match-end 1))) nil
       (1 (if (match-beginning 2)
	      font-lock-function-name-face
	    font-lock-variable-name-face))))
    )))
 )

(defvar d-font-lock-keywords d-font-lock-keywords-1
  "Default expressions to highlight in D mode.
See also `d-font-lock-extra-types'.")

;;;###autoload
(defun d-mode ()
  "Major mode for editing D code.
To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `d-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the common hook
`c-mode-common-hook' is run first.  Note that this mode automatically
sets the \"D\" style before calling any hooks so be careful if you
set styles in `c-mode-common-hook'."
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table d-mode-syntax-table)
  (setq major-mode 'd-mode
 	mode-name "D"
 	local-abbrev-table d-mode-abbrev-table)
  (use-local-map c-mode-map)
  (c-common-init)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start "// "
 	comment-end   ""
 	c-conditional-key c-D-conditional-key
 	c-comment-start-regexp c-D-comment-start-regexp
  	c-class-key c-D-class-key
	c-extra-toplevel-key c-D-extra-toplevel-key
 	c-baseclass-key nil
	c-recognize-knr-p nil
 	c-access-key  c-D-access-key
	c-inexpr-class-key nil
	imenu-case-fold-search nil
	)
  ;; Font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'((d-font-lock-keywords d-font-lock-keywords-1
				     d-font-lock-keywords-2 d-font-lock-keywords-3)
	  nil nil ((?_ . "w") (?$ . "w")) nil
	  (font-lock-mark-block-function . mark-defun)))
  ;; hooks
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'd-mode-hook)
  (c-update-modeline))

;; ChangeLog
;; 1.1 added body to block keywords.

;;; d-mode.el ends here

