(use-package tuareg :ensure t)
(use-package ocp-indent :ensure t)
(add-hook 'tuareg-mode-hook
          (lambda ()
            (setq tuareg-in-indent 0)
            (setq electric-indent-mode nil)
            (setq tuareg-leading-star-in-doc t)
            (setq tuareg-with-indent 0)))


(use-package merlin :ensure t)
; Make company aware of merlin
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))
; Enable company on merlin managed buffers
(add-hook 'merlin-mode-hook 'company-mode)
