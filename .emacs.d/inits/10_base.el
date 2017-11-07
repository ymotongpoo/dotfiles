;; neotree
(global-set-key [f8] 'neotree-toggle)

;;;;; frame
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time-mode t)
(setq frame-title-format "%f")
(menu-bar-mode -1)
(tool-bar-mode -1)

;;;;; theme
(load-theme 'monokai t)

;;;;; highlight
(setq hl-line-face 'underline)
(global-hl-line-mode)
(setq show-paren-delay 0)
(show-paren-mode t)

;;;;; edit
;; completion
(require 'company)
(global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;;;; misc
;; support for symblic links
(setq vc-follow-syslinks t)

;; auto revert buffer, useful for update of VCS repo
(global-auto-revert-mode 1)

;; change backup file location
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
