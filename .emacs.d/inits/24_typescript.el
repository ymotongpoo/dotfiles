(use-package typescript-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(use-package tide :ensure t)
(add-hook 'typescript-mode-hook
	  (lambda ()
	    (tide-setup)
	    (flycheck-mode t)
	    (setq flycheck-check-syntax-automatically '(save mode-enabled))
	    (eldoc-mode t)
	    (company-mode-on)))
