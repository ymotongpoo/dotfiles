(use-package go-mode
  :ensure t
  :defer t
  :mode
  ("\\.go$" . go-mode)
  :commands
  (go-mode)
  :bind
  (:map go-mode-map
	("M-." . godef-jump)
	("C-c C-r" . go-remove-unused-imports)
	("C-c i" . go-goto-imports)
	("C-c d" . godoc)
	("C-c l" . golint))
  :init
  (add-hook 'go-mode-hook
	    (lambda ()
	      (setq tab-width 4)
	      (setq c-basic-offset 4)
	      (setq indent-tabs-mode t)
	      (if (not (string-match "go" compile-command))
		  (set (make-local-variable 'compile-command)
		       "go-generate && go build -v && go test -v && go vet"))
	      (company-mode)
	      (go-eldoc-setup)
	      (go-guru-hl-identifier-mode)))
  :config
  (use-package company
    :ensure t)
  (use-package company-go
    :ensure t)
  (use-package go-eldoc
    :ensure t)
  (use-package golint
    :ensure t)
  (use-package go-guru
    :ensure t)
  (use-package exec-path-from-shell
    :ensure t)
  (setq godef-command "godef")
  (setq gofmt-command "gofmt")
  (setq company-go-show-annotation t)
  (flycheck-mode t)
  (add-to-list 'company-backends '(company-go :with company-dabbrev-code))
  (setq company-transformers '(company-sort-by-backend-importance))
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

;; Go settings require the following tools
;; go get -u github.com/nsf/gocode
;; go get =u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru

(exec-path-from-shell-copy-env "GOPATH")
(add-to-list 'exec-path (expand-file-name "~/src/go/workspace/bin"))


;;(define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer)
;;(push '(direx:direx-mode :position left :width 0.4 :dedicated t :stick t)
;;      popwin:special-display-config)
