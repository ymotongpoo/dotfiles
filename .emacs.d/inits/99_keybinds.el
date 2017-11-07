;;;;; modifier keys
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;;;;; key binds
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "C-h") 'delete-backward-char)
(define-key global-map (kbd "C-x M-f") 'recentf-open-files)

;;;;; smex
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
