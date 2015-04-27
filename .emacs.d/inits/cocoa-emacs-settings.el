(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; no tool bar and menu bar
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq inhibit-startup-message t)

;; miximize window at launch time.
;; If full screen is preferred, change maximized -> fullboth
;;(set-frame-parameter nil 'fullscreen 'fullboth)

(if window-system
    (progn
      (set-frame-parameter nil 'alpha 95)))

;; font settings
(set-face-attribute 'default nil
                    :family "Menlo" ;; font
                    :size 18)       ;; size

;; Jaanese font settings
(set-fontset-font nil 'japanese-jisx0208
                  (font-spec :family "Hiragino Kaku Gothic Pro"))
(setq face-font-rescale-alist
      '((".*Hiragino Kaku Gothic Pro.*" . 1.2)))

