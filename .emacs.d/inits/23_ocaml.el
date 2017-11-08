(use-package tuareg :ensure t :defer t)
(use-package ocp-indent :ensure t :defer t)
(add-hook 'tuareg-mode-hook
          (lambda ()
            (setq tuareg-in-indent 0)
            (setq electric-indent-mode nil)
            (setq tuareg-leading-star-in-doc t)
            (setq tuareg-with-indent 0)))

(use-package merlin :ensure t :defer t)
; Make company aware of merlin
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))
; Enable company on merlin managed buffers
(add-hook 'merlin-mode-hook 'company-mode)
