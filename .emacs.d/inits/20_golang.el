(message "20. Go programming language settings")

(require 'go-mode)
(require 'company)

(defvar my-gopath)
(setq my-gopath (concat (getenv "HOME") "/src/go/workspace"))
(setenv "GOPATH" my-gopath)
(add-to-list 'exec-path (concat my-gopath "/bin"))

;; setting for godoc
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

(add-hook 'go-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq indent-tabs-mode t)
            (setq gofmt-command "goimports") ;; use goimports instead of go fmt
            (add-hook 'before-save-hook 'gofmt-before-save)
            (add-hook 'go-mode-hook 'company-mode)
            (if (not (string-match "go" compile-command))
                (set (make-local-variable 'compile-command)
                     "go-generate && go build -v && go test -v && go vet"))
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)
            (local-set-key (kbd "M-.") 'godef-jump)
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
            (local-set-key (kbd "C-c i") 'go-goto-imports)
            (local-set-key (kbd "C-c d") 'godoc)
            (load-file (concat my-gopath "/src/golang.org/x/tools/cmd/oracle/oracle.el"))
            (local-set-key (kbd "C-c o o") 'go-oracle-set-scope)
            (local-set-key (kbd "C-c o c") 'go-oracle-callers)))

;;(define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer)
;;(push '(direx:direx-mode :position left :width 0.4 :dedicated t :stick t)
;;      popwin:special-display-config)
