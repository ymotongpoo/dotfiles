(require 'yasnippet)
(yas-global-mode 1)

(add-to-list 'load-path
             "~/.emacs.d/snippets/")

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/"
        "~/.emacs.d/snippets/web-mode"))

;; editing snippets themselves
(define-key yas-minor-mode-map (kbd "C-c y i") 'yas-insert-snippet)
(define-key yas-minor-mode-map (kbd "C-c y n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-c y v") 'yas-visit-snippet-file)

