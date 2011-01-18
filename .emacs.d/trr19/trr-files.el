;;; trr-files - (C) 1996 Yamamoto Hirotaka <ymmt@is.s.u-tokyo.ac.jp>
;;; Last modified on Tue Mar 18 00:37:07 1997
;;;               by SHUHEI, Yamaguchi <yamagus@kw.netlaputa.or.jp>

;; This file is a part of TRR19, a type training package for Emacs19.
;; See the copyright notice in trr.el.base

(eval-when-compile
  ;; Shut Emacs' byte-compiler up
  (setq byte-compile-warnings '(redefine callargs)))

;; files for a session.
(defvar   TRR:text-file nil "* text file")
(defvar   TRR:record-dir
  (let ((drt (or (getenv "TRRSCORES")
		 (expand-file-name "~/.trrscores"))))
    (if (string= "/"
		 (substring drt
			    (1- (length drt))
			    (length drt)))
	(substring drt 0 (1- (length drt)))
      drt))
  "* directory which your score files will reside.\n\
you can override this by setting the environment variable\n\
$TRRSCORES or (setq TRR:record-dir \"...\")")
(defvar   TRR:record-file nil "* result file for each text.")
(defvar   TRR:score-file nil "* score file")
(defvar   TRR:score-file-name nil "* score file name")
(defvar   TRR:text-name "" "* text name from CONTENTS")


(defun TRR:debug (file)
  (interactive)
  (save-excursion
    (find-file (expand-file-name (concat "~/src/trr19/" file)))
    (erase-buffer)
    (insert (format "TRR:flags\nTRR:start-flag\t%s\nTRR:quit-flag\t%s\nTRR:update-flag\t%s\nTRR:pass-flag\t%s\nTRR:cheat-flag\t%s\nTRR:beginner-flag\t%s\nTRR:random-flag\t%s\nTRR:secret-flag\t%s\nTRR:typist-flag\t%s\nTRR:small-window-flag\t%s\nTRR:skip-session-flag\t%s\n\n" TRR:start-flag TRR:quit-flag TRR:update-flag TRR:pass-flag TRR:cheat-flag TRR:beginner-flag TRR:random-flag TRR:secret-flag TRR:typist-flag TRR:small-window-flag TRR:skip-session-flag))
    (insert (format "Variables for Session\nTRR:eval\t%s\nTRR:whole-char-count\t%s\nTRR:correct-char-count\t%s\nTRR:start-time\t%s\nTRR:end-time\t%s\nTRR:miss-type-ratio\t%s\nTRR:type-speed\t%s\n\n" TRR:eval TRR:whole-char-count TRR:correct-char-count TRR:start-time TRR:end-time TRR:miss-type-ratio TRR:type-speed))
    (insert (format "Variables for STEP\nTRR:steps\t%s\nTRR:times-of-current-step\t%s\nTRR:time-of-current-step\t%s\nTRR:whole-chars-of-current-step\t%s\nTRR:whole-miss-of-current-step\t%s\n\n" TRR:steps TRR:times-of-current-step TRR:time-of-current-step TRR:whole-chars-of-current-step TRR:whole-miss-of-current-step))
    (insert (format "Others\nTRR:total-times\t%s\nTRR:total-time\t%s\nTRR:high-score\t%s\nTRR:high-score-old\t%s\nTRR:elapsed-time\t%s\n" TRR:total-times TRR:total-time TRR:high-score TRR:high-score-old TRR:elapsed-time))
    (save-buffer)
    (kill-buffer (current-buffer))))


(defun TRR:initiate-files ()
  (save-excursion
    (set-buffer (get-buffer-create TRR:text-file-buffer))
    (erase-buffer)
    (insert-file-contents TRR:text-file)
    (or (file-exists-p TRR:score-file)
	(call-process  TRR:update-program nil 0 nil TRR:score-file-name))
    (find-file-read-only TRR:score-file)
    (find-file TRR:record-file)
    (set-buffer (get-buffer-create (TRR:record-buffer)))
    (erase-buffer)
    (if (file-exists-p TRR:record-file)
	(insert-file-contents TRR:record-file)
      (if TRR:japanese
	  (insert "  0    0      0      0     0 �դ�����!\n")
	(insert "  0    0      0      0     0 cheers!  \n")))))


(defun TRR:decide-trr-text (num)
  (switch-to-buffer 
   (get-buffer-create (TRR:trainer-menu-buffer)))
  (erase-buffer)
  (insert-file-contents TRR:select-text-file)
  (untabify (point-min) (point-max))
  (goto-char (point-min))
  (let ((kill-whole-line t))
    (while (not (eobp))
      (if (or (= (char-after (point)) ?#) ; comment line begins with #
	       (= (char-after (point)) ?\n))
	  (kill-line)
	(forward-line))))
  (goto-line num)
  (let (temp temp-file temp-id)
    (setq temp (point))
    (while (not (= (char-after (point)) 32)) (forward-char))
    (setq TRR:text-name (buffer-substring temp (point)))
    (save-excursion
      (set-buffer (get-buffer-create (TRR:trainer-menu-buffer)))
      (setq mode-line-format (list "   "
				   (TRR:trainer-menu-buffer)
				   "                "
				   TRR:text-name))
      (force-mode-line-update))
    (while      (= (char-after (point)) 32)  (forward-char))
    (while (not (= (char-after (point)) 32)) (forward-char))
    (while      (= (char-after (point)) 32)  (forward-char))
    (while (not (= (char-after (point)) 32)) (forward-char)) ; need comment
    (while      (= (char-after (point)) 32)  (forward-char))
    (setq temp (point))
    (while (not (= (char-after (point)) 32)) (forward-char))
    (setq temp-file (buffer-substring temp (point)))
    (while      (= (char-after (point)) 32)  (forward-char))
    (setq temp (point))
    (while (not (or (eobp)
		    (= (char-after (point)) 32)
		    (= (char-after (point)) ?\n))) (forward-char))
    (setq temp-id (buffer-substring temp (point)))
    (setq TRR:text-file (concat TRR:directory
				"text/"
				temp-file
				".formed"))
    (setq TRR:text-file-buffer (file-name-nondirectory TRR:text-file))
    (and (not (and (file-exists-p (expand-file-name TRR:text-file))
		   (file-newer-than-file-p (expand-file-name TRR:text-file)
					   (expand-file-name
					    (concat TRR:directory
						    "text/" temp-file)))))
	 (or (file-exists-p (expand-file-name (concat TRR:directory
						    "text/"
						    temp-file)))
	     (error "%s does not exists!" temp-file))
	 (message (if TRR:japanese "������ե�������äƤ�Ρ�����äȤޤäƤ͡�"
		    "Making preformatted file. please wait..."))
	 (call-process TRR:format-program nil nil nil temp-file))
    (or (file-directory-p TRR:record-dir)
	(progn
;;;	  (call-process "/bin/rm" nil 0 nil "-f" TRR:record-dir)
	  (make-directory TRR:record-dir)))
    (setq TRR:record-file (concat TRR:record-dir
				  "/"
				  (user-login-name)
				  "-"
				  temp-id
				  (TRR:file-string)))
    (setq TRR:score-file-name (concat "SCORES-" temp-id (TRR:file-string)))
    (setq TRR:score-file (concat TRR:directory
				 "record/"
				 TRR:score-file-name))))


(defun TRR:file-string ()
  (cond
   (TRR:secret-flag "-secret")
   (TRR:typist-flag "-typist")
   ((not TRR:random-flag) "-easy")
   (t "")))


(defun TRR:kill-file (file)
  (if file
      (let ((tfile (get-file-buffer (expand-file-name file))))
	(if tfile (kill-buffer tfile)))))


(defun TRR:save-file (buffer file)
  (let ((tb (get-buffer buffer))
	tfile)
    (if tb
	(save-excursion
	  (set-buffer (get-buffer-create buffer))
	  (and (buffer-modified-p)
	       (progn (write-file file)
		      (setq tfile (concat file "~"))
		      (if (file-exists-p tfile) (delete-file tfile))))))))


(defun TRR:read-file ()
  (save-excursion
    (set-buffer TRR:text-file-buffer)
    (if TRR:random-flag
	(goto-line (TRR:random-num))
      (goto-line
       (* TRR:steps (/ TRR:number-of-text-lines 64))))
    (let (fill-column text start ch)
      (setq ch (char-after (point)))
      (while (not (or (= ch ?!)
		      (= ch ?.)
		      (= ch ?\;)
		      (= ch ?\?)
		      (= ch ?\)) ; )
		      (= ch ?\}))) ; }
	(forward-char)
	(setq ch (char-after (point))))
      (forward-char)
      (setq ch (char-after (point)))
      (while (or
	      (= ch ?\n) ; newline
	      (= ch ?\ ) ; space
	      (= ch ?!)
	      (= ch ?\))
	      (= ch ?.)
	      (= ch ?,)
	      (= ch ?\;)
	      (= ch ?\?)
	      (= ch ?\})
	      (and (= ch ?\")
		   (or (= (char-after (1+ (point))) ?\n)
		       (= (char-after (1+ (point))) ?\ )))
	      (and (= ch ?\')
		   (or (= (char-after (1+ (point))) ?\n)
		       (= (char-after (1+ (point))) ?\ ))))
	(forward-char)
	(setq ch (char-after (point))))
      (setq start (point))
      (forward-line 18)
      (setq text (buffer-substring start (point)))
      (switch-to-buffer (get-buffer-create (TRR:display-buffer)))
      (erase-buffer)
      (insert text)
      (untabify (point-min) (point-max))
      (and TRR:un-hyphenate
	   (progn
	     (goto-char (point-min))
	     (while (re-search-forward "\\([a-zA-z]\\)-\n *\\([a-zA-Z]\\)"
				       nil t)
	       (replace-match "\\1\\2" t))))
      (setq fill-column (- (window-width) 5))
      (fill-region (point-min) (point-max))
      (goto-char (point-min))
      (while (re-search-forward "\\([^ ]\\) *$" nil t)
	(replace-match "\\1" t))
      (goto-char (point-min))
      (while (not (eobp))
	(end-of-line)
	(insert "\n\n")
	(forward-line 1))
      (and window-system
	   TRR:text-color-name
	   (put-text-property (point-min) (point-max) 'face
			      TRR:text-face-name))
      (goto-char (point-min)))))

(defun TRR:update-score-file (score)
  (call-process TRR:update-program nil 0 nil
		TRR:score-file-name
		(user-login-name)
		(int-to-string score) 
		(int-to-string TRR:steps)
		(int-to-string TRR:total-times)
		(int-to-string (/ TRR:total-time 60))))


(defun TRR:get-high-score (&optional file)
  (if (not (file-exists-p (expand-file-name TRR:record-file)))
      -1
    (find-file (expand-file-name TRR:record-file))
    (let ((maxp -1))
      (while (not (eobp))
	(or (zerop (string-to-int (buffer-substring (+ (point) 4)
						    (+ (point) 8))))
	    (setq maxp
		  (max maxp (string-to-int (buffer-substring (point)
							     (+ (point) 3))))))
	(forward-line 1))
      (kill-buffer (current-buffer))
      maxp)))
