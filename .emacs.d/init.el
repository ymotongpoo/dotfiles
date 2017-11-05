;;;;; package initialization
(package-initialize)
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;;;;; start Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;; env path
(add-to-list 'exec-path "/opt/local/bin")
(add-to-list 'exec-path "/usr/local/bin")

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

;;;;; misc
;; support for symblic links
(setq vc-follow-syslinks t)

;; auto revert buffer, useful for update of VCS repo
(global-auto-revert-mode 1)

;; change backup file location
(setq backup-directory-alist
      (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
        backup-directory-alist))

;;;;;;;;;; init-loader
(require 'init-loader)
(init-loader-load "~/.emacs.d/inits")
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-go company-web yasnippet web-mode use-package tuareg tide smex smartparens recentf-ext projectile prodigy popwin pallet ocp-indent nyan-mode neotree multiple-cursors monokai-theme merlin magit js2-mode init-loader idle-highlight-mode htmlize go-mode flycheck-cask expand-region exec-path-from-shell emmet-mode drag-stuff company color-theme-sanityinc-tomorrow auto-async-byte-compile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
