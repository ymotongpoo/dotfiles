(message "20. Go programming language settings")
(add-hook 'go-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq indent-tabs-mode t)
            (local-set-key (kbd "M-.") 'godef-jump)
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
            (local-set-key (kbd "C-c i") 'go-goto-imports)
            (local-set-key (kbd "C-c d") 'godoc)))
(add-hook 'before-save-hook 'gofmt-before-save)

;;(define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer)
;;(push '(direx:direx-mode :position left :width 0.4 :dedicated t :stick t)
;;      popwin:special-display-config)
