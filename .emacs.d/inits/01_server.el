;;;;; start Emacs server
(use-package server
  :ensure t
  :config
  (unless (server-running-p)
    (server-start)))
