(use-package js2-mode :ensure t :defer t)

(setq auto-mode-alist
      (append '(
                ("\\.js$" . js2-mode))
              auto-mode-alist))

(add-hook 'js2-mode-hook 'ac-js2-mode)
