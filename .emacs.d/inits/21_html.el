;;;;;;;;;; emmet-mode
(use-package emmet-mode :ensure t :defer t)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'php-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook 
          (lambda () 
            (setq emmet-indentation 2)
            (define-key emmet-mode-keymap (kbd "C-c w") 'web-mode)
            ))
(setq emmet-move-cursor-between-quotes t)

;;;;;;;;;; web-mode
(setq auto-mode-alist
      (append '(
                ("\\.\\(html\\|xhtml\\|shtml\\|tpl\\)\\'" . web-mode))
              auto-mode-alist))

(use-package web-mode :ensure t :defer t)
(use-package company :ensure t :defer t)

(custom-set-faces
 '(web-mode-doctype-face
   ((t (:foreground "#82AE46")))) 
 '(web-mode-html-tag-face
   ((t (:foreground "#E6B422" :weight bold))))
 '(web-mode-html-tag-custom-face
   ((t (:foreground "#FF3366" :weight bold))))
 '(web-mode-html-tag-bracket-face
   ((t (:foreground "#aaaaaa"))))
 '(web-mode-html-attr-name-face
   ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face
   ((t (:foreground "#82AE46"))))
 '(web-mode-comment-face
   ((t (:foreground "#D9333F"))))
 '(web-mode-server-comment-face
   ((t (:foreground "#D9333F"))))
 '(web-mode-css-rule-face
   ((t (:foreground "#A0D8EF"))))
 '(web-mode-css-pseudo-class-face
   ((t (:foreground "#FF7F00"))))
 '(web-mode-css-at-rule-face
   ((t (:foreground "#FF7F00"))))
)
