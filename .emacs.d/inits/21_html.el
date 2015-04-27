(setq auto-mode-alist
      (append '(
                ("\\.\\(html\\|xhtml\\|shtml\\|tpl\\)\\'" . web-mode))
              auto-mode-alist))

(require 'web-mode)
(defun web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-auto-close-style 2)
)

(add-hook 'web-mode-hook 'web-mode-hook)

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
