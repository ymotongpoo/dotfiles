(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/"))

;; editing snippets themselves
(define-key yas-minor-mode-map (kbd "C-c y i") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-c y n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-c y v") 'yas-visit-snippet-file)

