;;;;; start Emacs server
(use-package server
  :ensure t
  :config
  (unless (server-running-p)
    (server-start))
  (when (display-graphic-p)
    (add-to-list 'default-frame-alist '(font . "Source Han Code JP 12"))
    (set-face-attribute 'default nil :font "Source Han Code JP 12")))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(select-frame frame)
		  (when (display-graphic-p)
		    (add-to-list 'default-frame-alist '(font . "Source Han Code JP 12"))
		    (set-face-attribute 'default nil :font "Source Han Code JP 12"))
		  (use-package monokai-theme
		    :ensure t
		    :config
		    (load-theme 'monokai t))))
    (use-package monokai-theme
      :ensure t
      :config
      (load-theme 'monokai t)))
		  
