(use-package typescript-mode :ensure t :defer t) 
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(use-package tide :ensure t :defer t)
(add-hook 'typescript-mode-hook
	  (lambda ()
	    (tide-setup)
	    (flycheck-mode t)
	    (eldoc-mode t)
	    (company-mode-on)
	    (setq flycheck-check-syntax-automatically '(save mode-enabled))))
