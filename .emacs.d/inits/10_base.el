;; neotree
(use-package neotree
  :ensure t
  :defer t
  :config
  (global-set-key [f8] 'neotree-toggle))

;;;;; frame
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)
(display-time-mode t)
(menu-bar-mode -1)
(tool-bar-mode 0)
(scroll-bar-mode -1)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq frame-title-format "%f")
(setq scroll-conservatively 1)
(setq scroll-margin 5)
(setq next-screen-context-lines 5)

;;;;; theme
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

;;;;; highlight
;(global-hl-line-mode)
(show-paren-mode t)
(setq hl-line-face 'underline)
(setq show-paren-delay 0)

;;;;; edit
;; completion (company-mode)
(use-package company
  :ensure t
  :defer t
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq x-select-enable-clipboard t)
  (setq save-interprogram-paste-before-kill t)
  (setq yank-pop-change-selection t)
  (setq x-select-enable-clipboard-manager t)
  (setq x-select-enable-primary t)
  (setq mouse-drag-copy-region t))

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; mozc
;; http://ain0204.hatenablog.com/entry/2016/08/12/235206
(use-package mozc
  :ensure t
  :defer t
  :config
  (set-language-environment "Japanese")
  (setq default-input-method "japanese-mozc"))

;;;;; misc
;; support for symblic links
(setq vc-follow-syslinks t)

;; auto revert buffer, useful for update of VCS repo
(global-auto-revert-mode 1)

;; flexible match using IDO
(use-package ido-completing-read+
  :ensure t
  :defer t)
(use-package ido-vertical-mode
  :ensure t
  :defer t
  :config
  (ido-vertical-mode 1))
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(setq ido-enable-flex-matching t)

;; Changebackup file location
(setq backup-directory-alist
      (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
        backup-directory-alist))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
