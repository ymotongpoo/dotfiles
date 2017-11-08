(use-package go-mode :ensure t)
(use-package company :ensure t)
(use-package company-go :ensure t)
(use-package go-eldoc :ensure t)
(use-package golint :ensure t)
(use-package go-guru :ensure t)
(use-package exec-path-from-shell :ensure t)

;; Go settings require the following tools
;; go get -u github.com/nsf/gocode
;; go get =u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru

(exec-path-from-shell-copy-env "GOPATH")
(add-to-list 'exec-path (expand-file-name "~/src/go/workspace/bin"))

(add-hook 'go-mode-hook
          (lambda ()
	    (setq tab-width 4)
            (setq c-basic-offset 4)
            (setq indent-tabs-mode t)
            (setq gofmt-command "gofmt")
            (add-hook 'before-save-hook 'gofmt-before-save)
            (if (not (string-match "go" compile-command))
                (set (make-local-variable 'compile-command)
                     "go-generate && go build -v && go test -v && go vet"))
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)
	    (flycheck-mode)
	    (go-eldoc-setup)
	    (go-guru-hl-identifier-mode)
            (local-set-key (kbd "M-.")     'godef-jump)
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
            (local-set-key (kbd "C-c i")   'go-goto-imports)
            (local-set-key (kbd "C-c d")   'godoc)
	    (local-set-key (kbd "C-c l")   'golint)))

;;(define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer)
;;(push '(direx:direx-mode :position left :width 0.4 :dedicated t :stick t)
;;      popwin:special-display-config)
