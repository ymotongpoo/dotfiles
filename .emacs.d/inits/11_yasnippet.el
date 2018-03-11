(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (("C-c y i" . yas-insert-snippet)
	 ("C-c y n" . yas-new-snippet)
	 ("C-c y v" . yas-visit-snippet-file))
  :init
  (add-hook 'after-init-hook 'yas-global-mode)
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets/"))
  (setq yas-prompt-functions '(yas-ido-prompt))
  :commands
  (yas-minor-mode yas-global-mode))

