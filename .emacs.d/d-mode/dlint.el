;;; dlint.el --- run dlint in a D buffer

;; Modified by Ben Hinkle from mlint.el by Eric Ludlam

;; Author: Eric M. Ludlam <eludlam@mathworks.com>
;; Maintainer: Eric M. Ludlam <eludlam@mathworks.com>
;; Created: June 25, 2002
;;
;; Copyright (C) 2002 Eric M. Ludlam
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; 
;; Run dlint, and highlight the problems in the buffer.
;;
;; This requires Dlint executable to be on the system path
;; This requires from d-mode.el
;; This requires eieio version 0.17beta5 or later.
;;     (Not yet released at the time of this writing.)

;;; Install
;;
;; (autoload 'dlint-minor-mode "dlint" nil t)
;; (add-hook 'd-mode-hook (lambda () (dlint-minor-mode 1)))

(require 'linemark)

;;; Code:
(defvar dlint-program "dlint" "Program to run for Dlint.")

(defvar dlint-output-regex
  "^L \\([0-9]+\\) (C \\([-0-9]+\\)): \\([^\n]+\\)"
  "Regular expression for collecting mlink output.")

(defun dlint-run (&optional buffer)
  "Run Dlint on BUFFER and return a list of issues.
If BUFFER is nil, use the current buffer."
  (let ((fn (buffer-file-name (current-buffer)))
	(dd default-directory)
	(errors nil))
    (save-excursion
      (set-buffer (get-buffer-create "*Dlint*"))
      (erase-buffer)
      (message "Running dlint...")
      (call-process dlint-program nil (current-buffer) nil
		    "-w" fn)
      (message "Running dlint...done")
      (goto-char (point-min))
      (while (re-search-forward dlint-output-regex nil t)
	(setq errors (cons
		      (cons (string-to-int (match-string 1))
			    (cons (string-to-int (match-string 2))
				  (match-string 3)))
		      errors))))
    errors
    ))

(defclass dlint-lm-group (linemark-group)
  ()
  "Group of linemarks for dlint.")

(defclass dlint-lm-entry (linemark-entry)
  ((column :initarg :column
	   :type integer
	   :documentation
	   "The column on which the warning occurs.")
   (coverlay :type overlay
	     :documantation
	     "Overlay used for the specific part of the line at issue.")
   (warning :initarg :warning
	   :type string
	   :documentation
	   "The error message created by dlint on this line."))
  "A linemark entry.")

(defun dlint-linemark-create-group ()
  "Create a group object for tracking linemark entries.
Do not permit multiple groups with the same name."
  (let* ((name "dlint")
	 (newgroup (dlint-lm-group name :face 'linemark-go-face))
	 (foundgroup nil)
	 (lmg linemark-groups))
    (while (and (not foundgroup) lmg)
      (if (string= name (object-name-string (car lmg)))
	  (setq foundgroup (car lmg)))
      (setq lmg (cdr lmg)))
    (if foundgroup
	(setq newgroup foundgroup)
      (setq linemark-groups (cons newgroup linemark-groups))
      newgroup)))

(defvar dlint-mark-group (dlint-linemark-create-group)
  "Group of marked lines for dlint.")

(defmethod linemark-new-entry ((g dlint-lm-group) &rest args)
  "Add a `linemark-entry' to G.
It will be at location FILE and LINE, and use optional FACE.
Call the new entrie's activate method."
  (let ((f (plist-get args :file))
	(l (plist-get args :line)))
    (apply 'dlint-lm-entry (format "%s %d" f l) args)))

(defun dlint-end-of-something ()
  "Move cursor to the end of whatever the cursor is on."
  (cond ((looking-at "\\w\\|\\s(")
	 (forward-sexp 1))
	((looking-at "\\s.")
	 (skip-syntax-forward "."))
	(t (error nil))))

(defmethod linemark-display ((e dlint-lm-entry) active-p)
  "Set object E to be active."
  (call-next-method)
  (if active-p
      (when (and (not (slot-boundp e 'coverlay))
		 (slot-boundp e 'overlay)
		 (oref e overlay))
	(with-slots (overlay column warning) e
	  ;; We called super first, so this should be an active overlay.
	  (linemark-overlay-put overlay 'help-echo warning)
	  (linemark-overlay-put overlay 'local-map dlint-overlay-map)
	  ;; Now, if we have some column data, lets put more highlighting on.
	  (save-excursion
	    (set-buffer (linemark-overlay-buffer overlay))
	    (goto-char (linemark-overlay-start overlay))
	    (move-to-column (1- column))
	    (oset e coverlay (linemark-make-overlay
			      (point)
			      (progn
				(condition-case nil
				    (dlint-end-of-something)
				  (error (forward-char 1)))
				(point))
			      (current-buffer)))
	    (with-slots (coverlay) e
	      (linemark-overlay-put coverlay 'face 'linemark-caution-face)
	      (linemark-overlay-put coverlay 'obj e)
	      (linemark-overlay-put coverlay 'tag 'dlint))
	    )))
    ;; Delete our spare overlay
    (when (slot-boundp e 'coverlay)
      (with-slots (coverlay) e
	(when coverlay
	  (linemark-delete-overlay coverlay)
	  (slot-makeunbound e 'coverlay)))
      )))

(defun dlint-highlight (err)
  "Setup ERR, an dlint message to be marked."
  (linemark-add-entry dlint-mark-group
		      :line (car err)
		      :column (car (cdr err))
		      :warning (cdr (cdr err))))

(defun dlint-unhighlight-all ()
  "Unhighlight all existing dlint messages."
  (interactive)
  (mapcar (lambda (e)
	    (if (string= (oref e filename) (buffer-file-name))
		(linemark-delete e)))
	  (oref dlint-mark-group marks)))

;;    {print errs}
(defun dlint-buffer ()
  "Run dlint on the current buffer, and highlight problems."
  (interactive)
  (let ((errs (dlint-run))
	)
    (dlint-unhighlight-all)
    (while errs
      (dlint-highlight (car errs))
      (setq errs (cdr errs)))))

(defun dlint-next-buffer ()
  "Move to the next warning in this buffer."
  (interactive)
  (let ((n (linemark-next-in-buffer dlint-mark-group 1 t)))
    (if n
	(progn (goto-line (oref n line))
	       (message (oref n warning)))
      (ding))))

(defun dlint-prev-buffer ()
  "Move to the prev warning in this buffer."
  (interactive)
  (let ((n (linemark-next-in-buffer dlint-mark-group -1 t)))
    (if n
	(progn (goto-line (oref n line))
	       (message (oref n warning)))
      (ding))))

(defun dlint-next-buffer-new ()
  "Move to the next new warning in this buffer."
  (interactive)
  (let ((p (linemark-at-point (point) dlint-mark-group))
	(n (linemark-next-in-buffer dlint-mark-group 1 t)))
    ;; Skip over messages that are the same as the one under point.
    (save-excursion
      (while (and n p (not (eq n p))
		  (string= (oref p warning) (oref n warning)))
	(goto-line (oref n line))
	(setq n (linemark-next-in-buffer dlint-mark-group 1 t))))
    (if n
	(progn (goto-line (oref n line))
	       (message (oref n warning)))
      (ding))))

(defun dlint-prev-buffer-new ()
  "Move to the prev new warning in this buffer."
  (interactive)
  (let ((p (linemark-at-point (point) dlint-mark-group))
	(n (linemark-next-in-buffer dlint-mark-group -1 t)))
    ;; Skip over messages that are the same as the one under point.
    (save-excursion
      (while (and n p (not (eq n p))
		  (string= (oref p warning) (oref n warning)))
	(goto-line (oref n line))
	(setq n (linemark-next-in-buffer dlint-mark-group -1 t))))
    (if n
	(progn (goto-line (oref n line))
	       (message (oref n warning)))
      (ding))))

(defun dlint-show-warning ()
  "Show the warning for the current mark."
  (interactive)
  (let ((n (linemark-at-point (point) dlint-mark-group)))
    (if (not n)
	(message "No warning at point.")
      (message (oref n warning)))))

;;; Define an dlinting minor mode
(defvar dlint-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c,n" 'dlint-next-buffer)
    (define-key map "\C-c,p" 'dlint-prev-buffer)
    (define-key map "\C-c,N" 'dlint-next-buffer-new)
    (define-key map "\C-c,P" 'dlint-prev-buffer-new)
    (define-key map "\C-c,g" 'dlint-buffer)
    (define-key map "\C-c,c" 'dlint-unhighlight-all)
    (define-key map "\C-c, " 'dlint-show-warning)
    map)
  "Minor mode keymap used when dlinting a buffer.")

(easy-menu-define
  dlint-minor-menu dlint-minor-mode-map "Dlint Minor Mode Menu"
 '("Dlint"
   ["Get Dlint Warnings" dlint-buffer t]
   ["Clear Dlint Warnings" dlint-unhighlight-all t]
   ["Show Warning" dlint-show-warning (linemark-at-point (point) dlint-mark-group)]
   "--"
   ["Next Warning" dlint-next-buffer t]
   ["Previous Warning" dlint-prev-buffer t]
   ["Next New Warning" dlint-next-buffer-new t]
   ["Previous New Warning" dlint-prev-buffer-new t]
   ))

(defvar dlint-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-3] 'dlint-emacs-popup-kludge)
    map)
  "Map used in overlays marking dlint warnings.")

(easy-menu-define
  dlint-overlay-menu dlint-overlay-map "Dlint"
  '("Dlint"
    ["Show Warning" dlint-show-warning t]
    "--"
    ["Next Warning" dlint-next-buffer t]
    ["Previous Warning" dlint-prev-buffer t]
    ["Next New Warning" dlint-next-buffer-new t]
    ["Previous New Warning" dlint-prev-buffer-new t]    
   ))

(defun dlint-emacs-popup-kludge (e)
  "Pop up a menu related to the clicked on item.
Must be bound to event E."
  (interactive "e")
  (let ((repos nil)
	(ipos nil)
	(startpos (point))
	)
    (save-excursion
      (mouse-set-point e)
      (setq ipos (point))
      (popup-menu dlint-overlay-menu)
      (if (/= (point) ipos) (setq repos (point)))
      )
    (when repos (goto-char repos))))

(easy-mmode-define-minor-mode dlint-minor-mode
  "Toggle dlint minor mode, a mode for showing dlint errors.
With prefix ARG, turn Checkdoc minor mode on iff ARG is positive.
\\{dlint-minor-mode-map\\}"
  nil " lint" dlint-minor-mode-map
  (if (and dlint-minor-mode (not (eq major-mode 'd-mode)))
      (progn
	(dlint-minor-mode -1)
	(error "Dlint minor mode is only for D Major mode.")))
  (if (not dlint-minor-mode)
      (progn
	;; We are linting, so don't verify on save.
	(dlint-unhighlight-all)
	(remove-hook 'after-save-hook 'dlint-buffer t)
	)
    (add-hook 'after-save-hook 'dlint-buffer nil t)
    (dlint-buffer))
  )

(provide 'dlint)

;;; dlint.el ends here
