(require 'js2-mode)

(setq auto-mode-alist
      (append '(
                ("\\.js$" . js2-mode))
              auto-mode-alist))

(add-hook 'js2-mode-hook 'ac-js2-mode)
