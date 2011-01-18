;;; -*- mode: emacs-lisp; coding: euc-japan; -*-

;; Copyright (C) 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: dabbrev

;;; Commentary:

;; 漢字・平仮名を含む単語を対象とする動的略称展開を使いやすくするため
;; の設定です．
;;
;; Emacs20 / Emacs21 の動的略称展開は，そのままでは，漢字や平仮名を含
;; む単語を適切に動的略称展開できません．英語と異なり，日本語には単語
;; 間の空白が存在しないため，文を単語単位に簡単に分割することが出来な
;; いことが，その原因です．
;;
;; この設定は，dabbrev-abbrev-char-regexp を動的に変更することによって，
;; 少しでも動的略称展開を使いやすくしようとするものです．

;;; History:

;; この拡張は，http://www.sodan.org/~knagano/emacs/dabbrev-20.html に
;; て記述されている技法を reimplement したものです．

;;; Code:

(or (boundp 'MULE)			; Mule2 と
    (featurep 'xemacs)			; XEmacs は設定不要
    (let (current-load-list)
      (defadvice dabbrev-expand
	(around modify-regexp-for-japanese activate compile)
	"Modify `dabbrev-abbrev-char-regexp' dynamically for Japanese words."
	(if (bobp)
	    ad-do-it
	  (let ((dabbrev-abbrev-char-regexp
		 (let ((c (char-category-set (char-before))))
		   (cond
		    ((aref c ?a) "[-_A-Za-z0-9]") ; ASCII
		    ((aref c ?j)	; Japanese
		     (cond
		      ((aref c ?K) "\\cK") ; katakana
		      ((aref c ?A) "\\cA") ; 2byte alphanumeric
		      ((aref c ?H) "\\cH") ; hiragana
		      ((aref c ?C) "\\cC") ; kanji
		      (t "\\cj")))
		    ((aref c ?k) "\\ck") ; hankaku-kana
		    ((aref c ?r) "\\cr") ; Japanese roman ?
		    (t dabbrev-abbrev-char-regexp)))))
	    ad-do-it)))))
