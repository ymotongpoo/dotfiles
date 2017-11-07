(require 'go-mode)
(require 'company)
(require 'company-go)

(exec-path-from-shell-copy-env "GOPATH")

(add-hook 'go-mode-hook
          (lambda ()
	    (setq tab-width 4)
            (setq c-basic-offset 4)
            (setq indent-tabs-mode t)
            (setq gofmt-command "gofmt")
            (add-hook 'before-save-hook 'gofmt-before-save)
            (add-hook 'go-mode-hook 'company-mode)
            (if (not (string-match "go" compile-command))
                (set (make-local-variable 'compile-command)
                     "go-generate && go build -v && go test -v && go vet"))
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)
	    (flycheck-mode)
            (local-set-key (kbd "M-.") 'godef-jump)
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
            (local-set-key (kbd "C-c i") 'go-goto-imports)
            (local-set-key (kbd "C-c d") 'godoc)))

;;(define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer)
;;(push '(direx:direx-mode :position left :width 0.4 :dedicated t :stick t)
;;      popwin:special-display-config)
