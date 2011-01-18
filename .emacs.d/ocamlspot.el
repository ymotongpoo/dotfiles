; (***********************************************************************)
; (*                                                                     *)
; (*                            OCamlSpotter                             *)
; (*                                                                     *)
; (*                             Jun FURUSE                              *)
; (*                                                                     *)
; (*   Copyright 2008, 2009 Jun Furuse. All rights reserved.             *)
; (*   This file is distributed under the terms of the GNU Library       *)
; (*   General Public License, with the special exception on linking     *)
; (*   described in file LICENSE.                                        *)
; (*                                                                     *)
; (***********************************************************************)

; How-to-use
;
; Write the following to your .emacs
;
;; load-path
; (setq load-path (cons "WHERE-YOU-HAVE-INSTALLED-THE-ELISP" load-path))
;
;; set the path of the ocamlspot binary
;; this can be a shell command, e.g., "ocamlfind ocamlspot"
; (setq ocamlspot-command "WHERE-YOU-HAVE-INSTALLED-THE-BINARIES/ocamlspot")
;
;; autoload
; (autoload 'ocamlspot-query "ocamlspot" "OCamlSpot")
;
;; tuareg mode hook (use caml-mode-hook instead if you use caml-mode)
;   (add-hook 'tuareg-mode-hook
;         '(lambda ()
;            (local-set-key "\C-c;" 'ocamlspot-query)
; 	     (local-set-key "\C-c:" 'ocamlspot-query-interface)
;            (local-set-key "\C-c\C-t" 'ocamlspot-type)
;            (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
;            (local-set-key "\C-c\C-u" 'ocamlspot-use)
;            (local-set-key "\C-ct" 'caml-types-show-type)))
;
;; You can also change overlay colors as follows:
; (set-face-background 'ocamlspot-spot-face "#660000")
; (set-face-background 'ocamlspot-tree-face "#006600")
;
; ocamlspot-query
;   Show the type of the inner-most subexpression under the cursor.
;   If there is an identifier under the cursor, browse and show its definition
;
; ocamlspot-query-interface
;   Same as ocamlspot-query but browse identifier's interface rather than its defintion
;   This is currently under construction and does not work properly.
;
; ocamlspot-type
;   Show the type of the inner-most subexpression under the cursor.
;
; ocamlspot-type-and-copy
;   Same as ocamlspot-type but it also copies the type expression to the kill buffer.
;
; ocamlspot-use
;   Show the use information of the identifier under the cursor.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Configurable variables

(eval-when-compile (require 'cl)) ; for `destructuring-bind'

(defgroup ocamlspot ()
  "OCamlSpotter: find the definition and type of variables."
  :group 'languages)

(defcustom ocamlspot-command "OCAML-SOURCE-TREE/ocamlspot/ocamlspot"
  "*The command which invokes ocamlspot."
  :type 'string :group 'ocamlspot)

(defcustom ocamlspot-debug nil
  "*Turn on ocamlspot debug output."
  :type 'boolean :group 'ocamlspot)

(defcustom ocamlspot-support-older-version nil
  "*If t, support an older command line interface for the first versions of ocamlspot, with an overhead of version detection. Useful with multiple versions of ocamlspot."
  :type 'boolean :group 'ocamlspot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constants

(defconst ocamlspot-buffer "*ocamlspot*"
  "The name of ocamlspot communication buffer")

(defconst ocamlspot-debug-buffer "*ocamlspot-debug*"
  "The name of ocamlspot debugging buffer")

(defconst ocamlspot-message-buffer "*ocamlspot-message*"
  "The name of ocamlspot message buffer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ocamlspot-path

; ocamlspot-path is superceded by ocamlspot-command, but if it exists,
; it overrides ocamlspot-command

(defun ocamlspot-get-command ()
  (if (boundp 'ocamlspot-path) ocamlspot-path ocamlspot-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Debugging

(defun ocamlspot-debug-message (s)
  (with-current-buffer (get-buffer-create ocamlspot-debug-buffer)
    (insert s)
    (insert "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; column chars => column bytes

; This looks complicated, but we need this conversion for multi-byte characters

(defun ocamlspot-string-of-line-to-point ()
  (buffer-substring-no-properties
   (line-beginning-position) (point)))

(defun ocamlspot-bytes-of-line-to-point ()
  (length
   (encode-coding-string
    (ocamlspot-string-of-line-to-point) buffer-file-coding-system)))

; It count one line less when the cursor is at (point-max) 
; and it is at the top of the line.
(defun ocamlspot-lines-of-point ()
  (count-lines (point-min) (min (1+ (point)) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; column bytes => column chars

; This looks complicated, but we need this conversion for multi-byte characters

; goto-line set mark and we see the result in the minibuffer
(defun ocamlspot-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;; get the string at line
(defun ocamlspot-buffer-substring-at-line (line)
  ; no need of save-excursion
  (ocamlspot-goto-line line)
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun ocamlspot-chars-of-bytes-of-string (str bytes)
  (length
   (decode-coding-string
    (substring (encode-coding-string str buffer-file-coding-system)
               0 bytes)
    buffer-file-coding-system)))

(defun ocamlspot-pos-of-bytes-at-line (line bytes)
  ; no need of save-excursion
  (ocamlspot-goto-line line)
  (let ((pos-at-beginning-of-line (line-beginning-position))
        (chars-from-beginning-of-line
         (ocamlspot-chars-of-bytes-of-string
          (ocamlspot-buffer-substring-at-line line) bytes)))
    (+ pos-at-beginning-of-line chars-from-beginning-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; location parser

; parses lxxxcxxxbxxx and returns the triplet
;; CR match-begin/match-end => matach-string x s
(defun ocamlspot-parse-location (s)
  (if (string-match "^l\\([\-0-9]+\\)c\\([\-0-9]+\\)b\\([\-0-9]+\\)$" s)
      (let ((line (string-to-number
                   (substring s (match-beginning 1) (match-end 1))))
            (colbytes (string-to-number
                       (substring s (match-beginning 2) (match-end 2))))
            (bytes (string-to-number
                       (substring s (match-beginning 3) (match-end 3)))))
        (list line colbytes bytes))
    ; older version
    (if (string-match "^\\([\-0-9]+\\)$" s)
        (let ((line -1)
              (colbytes -1)
              (bytes (string-to-number
                      (substring s (match-beginning 1) (match-end 1)))))
          (list line colbytes (+ bytes 1)))
      nil)))

(defun ocamlspot-pos-of-location (buffer s)
  (destructuring-bind (line colbytes bytes) (ocamlspot-parse-location s)
    (if (= line -1) bytes
      (with-current-buffer buffer
        (ocamlspot-pos-of-bytes-at-line line colbytes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Messaging

(setq ocamlspot-message-file-name nil)

(defun ocamlspot-message-init ()
  (setq ocamlspot-message-file-name (buffer-file-name))
  (setq ocamlspot-message-errors nil)
  (with-current-buffer (get-buffer-create ocamlspot-message-buffer)
    (erase-buffer)))

(defun ocamlspot-message-add (mes)
  (with-current-buffer (get-buffer-create ocamlspot-message-buffer)
    (if (/= 0 (current-column))
        (insert "\n"))
    (insert mes)))

; display message in the echo area if it is enough short, then return the string
; otherwise, pop a buffer of the message if may-pop is t and return the buffer 
; otherwise, returns nil
(defun ocamlspot-message-display (&optional may-pop)
  (with-current-buffer (get-buffer-create ocamlspot-message-buffer)
    (let ((lines ; how many lines in minibuffer-window ? 
           (count-screen-lines nil nil nil (minibuffer-window)))
          (max-echo-height 
           (if resize-mini-windows
               (cond ((floatp max-mini-window-height)
                      (* (frame-height) max-mini-window-height))
                     ((integerp max-mini-window-height)
                      max-mini-window-height)
                     (t 1)))))

      (if (or (<= lines  1)
              (<= lines max-echo-height))
          (progn
            (let ((mes (buffer-string)))
              (message mes)
              mes))
        (if may-pop ; buffer layout may change... no way to recover ?
            (progn
              (display-buffer ocamlspot-message-buffer)
              ocamlspot-message-buffer)
          ;; display the first max-echo-height lines
          (let ((lines (max 1 (1- max-echo-height))))
            (goto-char (point-min))
            (forward-visible-line (max 1 (- max-echo-height 2)))
            (message (concat (buffer-substring (point-min) (point)) "... Result is too long. Truncated."))
            nil))))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Overlays

;; the spot overlay
(defvar ocamlspot-spot-overlay (make-overlay 1 1))
(defface ocamlspot-spot-face
    '((t (:foreground "#88FF44")))
  "Face for ocamlspot spot highlight"
  :group 'ocamlspot)
(overlay-put ocamlspot-spot-overlay 'face 'ocamlspot-spot-face)

;; the tree overlay
(defvar ocamlspot-tree-overlay (make-overlay 1 1))
(defface ocamlspot-tree-face
    '((t (:foreground "#FF88FF")))
  "Face for ocamlspot tree highlight"
  :group 'ocamlspot)
(overlay-put ocamlspot-tree-overlay 'face 'ocamlspot-tree-face)

(defun ocamlspot-delete-overlays-now ()
  (interactive)
  (delete-overlay ocamlspot-tree-overlay)
  (delete-overlay ocamlspot-spot-overlay))

(defun ocamlspot-delete-overlays ()
  (unwind-protect
      (sit-for 10)
    (ocamlspot-delete-overlays-now)))

; obsolete code, but probably useful in future
; (defun ocamlspot-display-overlay (filename position overlay)
;   (if (string-match "\.cm[ioxa]$" filename)
;       ;; It is not an .ml or .mli. Packed module.
;       (progn
;         (message "Packed module: %s" filename)
;         ;; CR jfuruse: opening a binary file is not good
;         (setq target-buffer (ocamlspot-find-file-existing filename)))
;     (progn
;       (setq target-buffer (ocamlspot-find-file-existing filename))
;       (if (string-match "^\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[\-0-9]+\\):\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[\-0-9]+\\)$" position)
;           (let ((start (substring position (match-beginning 1) (match-end 1)))
;                 (end   (substring position (match-beginning 2) (match-end 2))))
;             (let ((start (ocamlspot-pos-of-location target-buffer start))
;                   (end   (ocamlspot-pos-of-location target-buffer end)))
;               ;; display the result
;               (save-excursion
;                 (set-buffer target-buffer)
;                 (goto-char start)
;                 (move-overlay overlay start end target-buffer))))))))

(defun ocamlspot-display-overlay (buffer position overlay)
  (if (string-match "^\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[0-9]+\\):\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[0-9]+\\)$" position)
      (let ((start (substring position (match-beginning 1) (match-end 1)))
            (end   (substring position (match-beginning 2) (match-end 2))))
        (let ((start (ocamlspot-pos-of-location buffer start))
              (end   (ocamlspot-pos-of-location buffer end)))
          ;; display the result
          (set-buffer buffer)
          (goto-char start)
          (move-overlay overlay start end buffer)))
    ; this should be all
    (display-buffer buffer)
    (move-overlay overlay (point-min) (point-max) buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Warnings

(defun ocamlspot-warning ()
  (and (re-search-forward "^\\(Warning: .*\\)$" nil t)
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

(defun ocamlspot-warnings-rev (lst)
  (let ((warning (ocamlspot-warning)))
    (if warning (ocamlspot-warnings-rev (concat lst warning "\n"))
      lst)))

(defun ocamlspot-warnings ()
  (goto-char (point-min))
  (ocamlspot-warnings-rev ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; File access

(defun ocamlspot-find-file-existing (path)
  (if (file-exists-p path)
      (find-file-other-window path)
    (ocamlspot-message-add (format "ERROR: source file %s was not found" path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Queries

; It is all my failure, but the first versions of ocamlspot lacks clear notion
; of versions. This function infers the command option interface of the command
; from the help string. If t, ocamlspot cannot take line-bytes specifications.
(defun ocamlspot-older-version ()
  (if ocamlspot-support-older-version
      (with-current-buffer (get-buffer-create ocamlspot-buffer)
        (erase-buffer)
        (call-process shell-file-name nil t nil shell-command-switch
                      (concat (ocamlspot-get-command) " -help"))
        (goto-char (point-min))
        (re-search-forward "^ocamlspot path:charpos" nil t))
    nil))

; launch ocamlspot
; result is stored in the buffer "ocamlspot-buffer"
; the current buffer is stored in source_buffer
(defun ocamlspot-gen-query (extra_args)
  (interactive)
  (ocamlspot-message-init)
  (save-excursion
    (ocamlspot-delete-overlays-now)
    ;; arguments
    (let ((file-name (buffer-file-name))
          (arg (if (ocamlspot-older-version)
                   ; older pos spec
                   (format "%s:%d"
                           (buffer-file-name)
                           (1- (point)))
                   ; newer pos spec
                   (format "%s:l%dc%d"
                           (buffer-file-name)
                           (ocamlspot-lines-of-point)
                           (ocamlspot-bytes-of-line-to-point))))
          (source-buffer (current-buffer))) ; ocamlspot buffer
      (with-current-buffer (get-buffer-create ocamlspot-buffer)
        (erase-buffer)
        ;; chdir is required
        (cd (file-name-directory file-name))
        (call-process shell-file-name nil t nil shell-command-switch
                      (concat (ocamlspot-get-command) " " arg
                              (if ocamlspot-debug " -debug " " ")
                              extra_args))
        ;; search the found tree element
        (goto-char (point-min))
        (if (re-search-forward "^Tree: \\(l[\-0-9]+c[\-0-9]+b[\-0-9]+:l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[0-9]+:[0-9]+\\)$"
                               nil t)
            (let ((pos (buffer-substring (match-beginning 1) (match-end 1))))
              ;; display the result
              (save-current-buffer
                (ocamlspot-display-overlay source-buffer pos ocamlspot-tree-overlay))
              (ocamlspot-message-add (ocamlspot-warnings))
              t)
          (if (re-search-forward "^\\(Error: .*\\)" nil t)
              (ocamlspot-message-add (buffer-substring (match-beginning 1) (match-end 1)))
              ;; display debug info
            (ocamlspot-message-add "ERROR: no tree node found there"))
          nil)))))

(defun ocamlspot-jump-to-spot (filename position)
  (if (string-match "\.cm[ioxa]$" filename)
      ;; It is not an .ml or .mli. Packed module.
      ;; CR jfuruse: opening a binary file is not good
      (ocamlspot-message-add "Packed module: %s" filename)
    (ocamlspot-display-overlay
     (ocamlspot-find-file-existing filename)
     position ocamlspot-spot-overlay)))

(defun ocamlspot-find-type-in-buffer (&optional to-kill)
  (set-buffer (get-buffer-create ocamlspot-buffer))
  (goto-char (point-min))
  (if (re-search-forward "^Type: \\(.*\\(\n +.*\\)*\\)" nil t)
      (let ((type (buffer-substring (match-beginning 1) (match-end 1))))
        (if to-kill (kill-new type))
        (ocamlspot-message-add (format "Type: %s" type))
        type)
    (ocamlspot-message-add "no type found here")
    nil))

;; same as type-in-buffer but for XType
(defun ocamlspot-find-xtype-in-buffer ()
  (set-buffer (get-buffer-create ocamlspot-buffer))
  (goto-char (point-min))
  (if (re-search-forward "^XType: \\(.*\\(\n +.*\\)*\\)" nil t)
      (let ((type (buffer-substring (match-beginning 1) (match-end 1))))
        (ocamlspot-message-add (format "%s" type))
	type)
    (ocamlspot-message-add "no type found here")
    nil))

(defun ocamlspot-find-val-or-type-in-buffer (&optional to-kill)
  (set-buffer (get-buffer-create ocamlspot-buffer))
  (goto-char (point-min))
  (if (re-search-forward "^Val: \\(.*\\(\n +.*\\)*\\)" nil t)
      (let ((type (buffer-substring (match-beginning 1) (match-end 1))))
        (if to-kill (kill-new type))
        (ocamlspot-message-add (format "Val: %s" type))
        type)
    (ocamlspot-find-type-in-buffer to-kill)))

(defun ocamlspot-find-spot-in-buffer ()
  (set-buffer (get-buffer-create ocamlspot-buffer))
  (goto-char (point-min))
  ;; all and -1:-1 mean the whole file
  (if (re-search-forward "^Spot: \\(.*\\):\\(l[\-0-9]+c[\-0-9]+b[\-0-9]+:l[\-0-9]+c[\-0-9]+b[\-0-9]+\\|[0-9]+:[0-9]+\\|all\\|-1:-1\\)$"
                         nil t)
      (let ((filename (buffer-substring (match-beginning 1)
                                        (match-end 1)))
            (position (buffer-substring (match-beginning 2)
                                        (match-end 2))))
        ;; display the result
        (let ((type (ocamlspot-find-val-or-type-in-buffer)))
          (ocamlspot-jump-to-spot filename position)
          ; (if type (ocamlspot-message-add (format "Type: %s" type)))
	  ))
    (if (re-search-forward "^Spot: \\(.*\\)" nil t)
        (ocamlspot-message-add (buffer-substring (match-beginning 1) (match-end 1)))
      (if (re-search-forward "^\\(Error: .*\\)" nil t)
          (ocamlspot-message-add (buffer-substring (match-beginning 1) (match-end 1)))
        ;; display debug info
        (ocamlspot-message-add "No spot found there")
        (ocamlspot-find-val-or-type-in-buffer)
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactives

(defun ocamlspot-wait (&optional may-pop)
  (ocamlspot-message-display may-pop)
  (ocamlspot-delete-overlays))

(defun ocamlspot-query ()
  (interactive)
  (let ((sel-window (selected-window)))
  (save-selected-window
    (when (ocamlspot-gen-query nil)
      ;; search the result
      (ocamlspot-find-spot-in-buffer))
    (ocamlspot-wait))
  ; I dunno why but we need the following line to list-buffers work nicely
  (select-window sel-window)))

; CR dup code
(defun ocamlspot-query-interface ()
  (interactive)
  (let ((sel-window (selected-window)))
  (save-selected-window
    (if (ocamlspot-gen-query "--interface")
        (progn ;save-excursion
          ;; search the result
          (ocamlspot-find-spot-in-buffer)))
    (ocamlspot-wait))
  ; I dunno why but we need the following line to list-buffers work nicely
  (select-window sel-window)))

(defun ocamlspot-type (&optional to-kill)
  (interactive)
  (if (ocamlspot-gen-query "-n")
      (save-current-buffer
        (ocamlspot-find-val-or-type-in-buffer to-kill)))
  (ocamlspot-wait t))

;; CR dup code
(defun ocamlspot-xtype (&optional to-kill)
  (interactive)
  (if (ocamlspot-gen-query "-n")
      (save-current-buffer
        (ocamlspot-find-xtype-in-buffer)))
  (display-buffer ocamlspot-message-buffer))

(defun ocamlspot-type-and-copy ()
  (interactive)
  (ocamlspot-type t))

; CR can be shared with ocamlspot-type
(defun ocamlspot-use ()
  (interactive)
  (if (ocamlspot-gen-query "-n")
      (save-current-buffer
        (set-buffer (get-buffer-create ocamlspot-buffer))
        (goto-char (point-min))
        (if (re-search-forward "^Use: \\(.*\\(\n +.*\\)*\\)" nil t)
            (let ((type (buffer-substring (match-beginning 1) (match-end 1))))
              (ocamlspot-message-add type))
          (ocamlspot-message-add "no use information found here"))))
  (ocamlspot-wait t))

(defun ocamlspot-display-ocamlspot-buffer ()
  (interactive)
  (display-buffer ocamlspot-buffer))

(provide 'ocamlspot)
