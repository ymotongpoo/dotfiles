;;;;;;;;;; package loading
(when (> emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d"))

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "elisp" "conf" "public_repos")

(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-user-dir "~/.emacs.d/elpa")
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)
  (message "loading init-loader")
  (unless (package-installed-p 'init-loader)
    (package-refresh-contents)
    (package-install 'init-loader))
  (require 'init-loader)
  (init-loader-load))

;;;;;;;;;; env path
(add-to-list 'exec-path "/opt/local/bin")
(add-to-list 'exec-path "/usr/local/bin")

;;;;;;;;;; character encoding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

(when (eq system-type 'w32)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;;;;;;;;;; frame
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time-mode t)
(setq frame-title-format "%f")

;;;; count number of lines and chars
;;;; http://d.hatena.ne.jp/sonota88/20110224/1298557375
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))

(add-to-list 'mode-line-format
             '(:eval (count-lines-and-chars)))


;;;;;;;;;; indent
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "stroustrup")))


;;;;;;;;;; theme
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-hober))


;;;;;;;;;; highlight
(setq hl-line-face 'underline)
(global-hl-line-mode)
(setq show-paren-delay 0)
(show-paren-mode t)


;;;;;;;;;; multi-term
(when (require 'multi-term nil t)
  (setq multi-term-program shell-file-name))


;;;;;;;;;; misc
(setq vc-follow-syslinks t)
