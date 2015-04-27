(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; no tool bar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-startup-message t)

;; miximize window at launch time.
;; If full screen is preferred, change maximized -> fullboth
(set-frame-parameter nil 'fullscreen 'fullboth)

(if window-system
    (progn
      (set-frame-parameter nil 'alpha 95)))

;; font settings
(set-face-attribute 'default nil
                    :family "Menlo" ;; font
                    :height 140)    ;; size

;; Jaanese font settings
(set-fontset-font nil 'japanese-jisx0208
                  (font-spec :family "Hiragino Maru Gothic Pro"))
(setq face-font-rescale-alist
      '((".*Hiragino Maru Gothic Pro.*" . 1.2)))

