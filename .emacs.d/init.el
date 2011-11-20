;;;環境の判別
(defvar run-unix
  (or (equal system-type 'gnu/linux)
      (equal system-type 'usg-unix-v)))
(defvar run-xemacs (featurep 'xemacs))
(defvar run-xemacs-no-mule
  (and run-xemacs (not (featurep 'mule))))

;;; タブ幅
(setq-default tab-width 4)

;;; 日本語環境設定
(set-language-environment "Japanese")

;;; C-hでBackSpace
(keyboard-translate ?\C-h ?\C-?)

;;; CmdをMetaキーに
(when (>= emacs-major-version 23)
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super)))

;;; 列数の表示
(column-number-mode 1)

;;; シンボリックリンクの読み込みを許可
(setq vc-follow-symlinks t)
;;; シンボリックリンク先のVCS内で更新が入った場合にバッファを自動更新
(setq auto-revert-check-vc-info t)

;;; スタートアップスクリーンを表示しない
(setq inhibit-splash-screen t)

;;; バックアップファイルの設定
(setq make-backup-files t)
(setq backup-directory-alist
	  (cons (cons "\\.*$" (expand-file-name "~/.emacs_backup"))
			backup-directory-alist))
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq delete-old-versions t)

;;;load-pathに~/.emacs.dを追加
(setq load-path (cons "~/.emacs.d" load-path))
(setq load-path (cons "~/.emacs.d/tuareg-mode-1.45.6" load-path))
(setq load-path (cons "~/.emacs.d/python" load-path))
(setq load-path (cons "~/.emacs.d/scel" load-path))
(setq load-path (cons "~/.emacs.d/ocaml-mode" load-path))

;; 常にホームディレクトリから
(cd "~")

;; バッファはバッファで
(setq one-buffer-one-frame-mode nil)
 
;;リージョンに色をつける
(transient-mark-mode t)

;;リージョンの色の変更
(set-face-background 'region "DeepSkyBlue4")

;;リージョンを[delete][BS]で削除
(delete-selection-mode 1)

;;color-theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

;;フリンジの色の変更
(set-face-background 'fringe "gray20")

(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "blue4")

;;対応する括弧に色をつける
(require 'paren)
(show-paren-mode 1)

;;対応する括弧に@で移動
(global-set-key "@" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
		((looking-at "\\s\)") (forward-char 1) (backward-list 1))
		(t (self-insert-command (or arg 1)))))

;;;タイトルバーの表示
(setq frame-title-format "%b")
(menu-bar-mode -1)

;;;右端で折り返さない
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;;;1行丸ごとカット
(setq kill-whole-line t)

;;; カーソルキーで新しい行を作らない
(setq next-line-add-newlines nil)

;;;警告音をフラッシュに
(setq visible-bell t)

;;;スクロール設定
(setq scroll-conservatively 35
	  scroll-margin 5
	  scroll-step 1)

;;;M-g で goto-line
(global-set-key [M-g] 'goto-line)

;;;日本語動的補完
(load "dabbrev-ja")

;;;動的補完をMeta+右に割り当て
(global-set-key [M-right] 'dabbrev-expand)

;;;コメントアウトをMeta+↑、Meta+↓
(global-set-key [M-up] 'comment-region)
(global-set-key [M-down] 'uncomment-region)

;;;goto-lineをMeta+←
(global-set-key [M-left] 'goto-line)

;;;ヘッダファイルはc++-modeで開く
(setq auto-mode-alist
      (cons (cons "\\.c$" 'c++-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons (cons "\\.h$" 'c++-mode) auto-mode-alist))


;; バッファの切り替え
(setq *next-buffer-in-tab-order* t)
(global-set-key [\C-2] 'previous-buffer)
(global-set-key [\C-3] 'next-buffer)

 
;;; font-lockの設定
(global-font-lock-mode 1)
;(setq font-lock-support-mode 'fast-lock-mode)
;(setq font-lock-support-mode 'lazy-lock-mode)

;;; フォントをInconsolataに設定
;;; Mac OS X -- http://www.levien.com/type/myfonts/inconsolata.html
;;; Ubuntu -- http://yamashita.dyndns.org/blog/inconsolata-as-a-programming-font/
(when (>= emacs-major-version 23)
  (set-face-attribute 'default nil
					  :family "monaco"
					  :height 140)
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0212
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'mule-unicode-0100-24ff
   '("monaco" . "iso10646-1"))
  (setq face-font-rescale-alist
		'(("^-apple-hiragino.*" . 1.2)
		  (".*osaka-bold.*" . 1.2)
		  (".*osaka-medium.*" . 1.2)
		  (".*courier-bold-.*-mac-roman" . 1.0)
		  (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
		  (".*monaco-bold-.*-mac-roman" . 0.9)
		  ("-cdac$" . 1.3))))

(when (< emacs-major-version 23)
  (when (eq system-type 'darwin)
	(create-fontset-from-mac-roman-font
	 "-apple-inconsolata-medium-r-normal--14-0-72-72-m-0-iso10646-1"
	 nil "myfont")

	(set-fontset-font "fontset-myfont"
					  'japanese-jisx0208
					  '("ヒラギノ丸ゴ pro w4*" . "jisx0208.*"))

	(set-fontset-font "fontset-myfont"
					  'katakana-jisx0201
					  '("ヒラギノ丸ゴ pro w4*" . "jisx0201.*"))

	(add-to-list 'default-frame-alist '(font . "fontset-myfont")))

  (when (eq system-type 'gnu/linux)
	(set-default-font "Inconsolata-11")
	(set-face-font 'variable-pitch "Inconsolata-11")
	(set-fontset-font (frame-parameter nil 'font)
					  'japanese-jisx0208
					  '("Takaoゴシック" . "unicode-bmp"))))
	

  
;;; 初期フレームの設定
(setq default-frame-alist
  (append (list '(foreground-color . "white")
    '(background-color . "black")
    '(background-color . "gray")
    '(border-color . "white")
    '(mouse-color . "white")
    '(width . 100)
    '(height . 50)
    '(top . 30)
    '(left . 50)
    '(alpha . (80 50)))
  default-frame-alist))

;;; mini-buffer 
(setq resize-mini-windows nil)

;;;tool-bar
(setq mac-tool-bar-display-mode nil)
(tool-bar-mode 0)
;(setq tool-bar-button-margin 0)

;;;HOMEとENDの設定
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

;;;カーソルの非選択画面での表示
(setq cursor-in-non-selected-windows nil)

;; 文字数カウント関数
(defun count-char-region (start end)
  (interactive "r")
  (save-excursion	  ;;これと
    (save-restriction ;;これは オマジナイ。 (ちゃんと調べましょう (爆))
      (let ((lf-num 0))	  ;;改行文字の個数用、初期化している。
        (goto-char start) ;;指定領域の先頭に行く。
        (while (re-search-forward "[\n\C-m]" end t) ;;改行文字のカウント
          (setq lf-num (+ 1 lf-num))) ;;(つまり、 search できる度に 1 足す)
        (message "%d 文字 (除改行文字) : %d 行 : %d 文字 (含改行文字)"
                 (- end start lf-num) (count-lines start end) (- end start))))))

;;trr
(setenv "TRRDIR" "~/.emacs.d/trr19")
(setenv "TRRBINDIR" "~/.emacs.d/trr19")
(autoload 'trr "~/.emacs.d/trr19/trr" nil t)

;;c++ namespace no indent
(add-hook 'c++-mode-hook
          '(lambda()
			 (c-set-style "stroustrup")
             (c-set-offset 'innamespace 0) ; namespace {}の中はインデントしない
			 (c-set-offset 'c-basic-offset 2)
			 (setq indent-tabs-mode nil)
             ))

;;;;*************** Major mode ***************
;;;;; python mode
(progn (cd "~/.emacs.d/vendor")
       (normal-top-level-add-subdirs-to-load-path))

(require 'python)

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-hook 'python-mode-hook
		  (function (lambda ()
					  (setq indent-tabs-mode nil)
					  (setq indent-level 2)
					  (setq python-indent 2)
					  (setq tab-width 2)
					  )))

;; paren complete
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\"" 'electric-pair)
            (define-key python-mode-map "\'" 'electric-pair)
            (define-key python-mode-map "(" 'electric-pair)
            (define-key python-mode-map "[" 'electric-pair)
            (define-key python-mode-map "{" 'electric-pair)))

(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

;;;;; Erlang mode
(require 'erlang-start)
(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil))) 

;;;;; Tuareg mode (for OCaml)
;;;;; OCamlSpotter
; set the path of the ocamlspot binary
(setq ocamlspot-path "/opt/ocaml/3.12.0\+1.2.0/bin/ocamlspot")
 
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code." t)
(autoload 'camldebug "cameldeb" "Run the Caml debugger." t)
(autoload 'ocamlspot-query "ocamlspot" "OCamlSpot")
(add-hook 
 'tuareg-mode-hook
 '(lambda ()
    ;; indentation rules
    (setq tuareg-lazy-= t)
    (setq tuareg-lazy-paren t)
    (setq tuareg-in-indent 0)
    (setq tuareg-electric-indent nil)
    (setq tuareg-leading-star-in-doc t)
    (setq tuareg-with-indent 0)

    (setq tuareg-library-path "/opt/ocaml/3.12.0\+1.2.0/lib/ocaml/")

    ;; Sym-Lock customization only
    ;; turn off special under face mouse
    (if (featurep 'sym-lock)   
	(setq sym-lock-mouse-face-enabled nil))

    ;; ocamlspot and other keys
    (local-set-key "\C-c;" 'ocamlspot-query)
    (local-set-key "\C-c\C-t" 'ocamlspot-type)
    (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
    (local-set-key "\C-c\C-u" 'ocamlspot-use)
    (local-set-key "\C-ct" 'caml-types-show-type)
	
	(setq indent-tabs-mode nil)
    ))

;(setq tuareg-lazy-paren t)


;;;;; D mode
(setq load-path (cons "~/.emacs.d/d-mode" load-path))
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(setq auto-mode-alist 
	  (cons '( "\\.d\\'" . d-mode ) auto-mode-alist))
(autoload 'dlint-minor-mode "dlint" nil t)
(add-hook 'd-mode-hook 
		  (lambda () (dlint-minor-mode 1)))


;;;;; Scala mode
;(require 'scala-mode-auto)
;(add-hook 
; 'scala-mode-hook
; '(lambda ()
;    (setq indent-tabs-mode nil)
;    ))

;;;;; rst mode
(require 'rst)
(setq frame-background-mode 'dark)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

(add-hook 
 'rst-mode-hook
 '(lambda ()
    (setq indent-tabs-mode nil)
    ))


;;;;; JavaScript mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;;;;; FUEL (Factor) mode
(load-file "~/.emacs.d/fuel/fu.el")


;;;;; Haskell mode
(setq load-path (cons "~/.emacs.d/haskell" load-path))
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hi$"     . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
  "Major mode for editing literate Haskell scripts." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

(setq haskell-literate-default 'latex)
(setq haskell-doc-idle-delay 0)

;;;;; ChucK mode
;(setq auto-mode-alist
;	  (cons '("\.ck" . chuck-mode)
;			auto-mode-alist))
;(autoload 'chuck-mode "chuck-mode" "ChucK editing mode" t)
;(defvar chuck-exec "~/bin/chuck")

;;;;; Yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;;;; php mode
(autoload 'php-mode "php-mode" "PHP mode" t)
(add-hook 'php-mode-user-hook
		  '(lambda ()
			 (setq-default tab-width 4)
			 (setq indent-tabs-mode nil)
			 (setq c-basic-offset 4)))

(defcustom php-file-patterns (list "\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'")
  "*List of file patterns for which to automatically invoke php-mode."
  :type '(repeat (regexp :tag "Pattern"))
  :group 'php)

(let ((php-file-patterns-temp php-file-patterns))
  (while php-file-patterns-temp
    (add-to-list 'auto-mode-alist
                 (cons (car php-file-patterns-temp) 'php-mode))
    (setq php-file-patterns-temp (cdr php-file-patterns-temp))))


;;;;; Super Collider from Emacs
;(require 'sclang)

;;; end of file

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(vc-follow-symlinks t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
