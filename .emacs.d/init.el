;; installed packages.  Don't delete this line.  If you don't want it,
;; Added by Package.el.  This must come before configurations of
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;;;;; start Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")))

(require 'package)

;;;;; install required pacakges
(package-initialize)
(unless package-archive-contents
    (package-refresh-contents))
(package-install-selected-packages) ;; call M-x package-install-selected-packages for first launch

;;;;; init-loader
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'init-loader)
(init-loader-load "~/.emacs.d/inits")

;;;;; change the place for Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
