(message "20. Go programming language settings")

(defvar my-gopath)
(setq my-gopath (concat (getenv "HOME") "src/go/workspace"))
(setenv "GOPATH" my-gopath)
(add-to-list 'exec-path (concat my-gopath "/bin"))

(add-hook 'go-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq indent-tabs-mode t)))

(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go-generate && go build -v && go test -v && go vet"))
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c i") 'go-goto-imports)
  (local-set-key (kbd "C-c d") 'godoc))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'go-autocomplete)
(require 'auto-complete-config)

;;(define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer)
;;(push '(direx:direx-mode :position left :width 0.4 :dedicated t :stick t)
;;      popwin:special-display-config)
