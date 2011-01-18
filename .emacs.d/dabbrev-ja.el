;;; -*- mode: emacs-lisp; coding: euc-japan; -*-

;; Copyright (C) 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: dabbrev

;;; Commentary:

;; ������ʿ��̾��ޤ�ñ����оݤȤ���ưŪά��Ÿ����Ȥ��䤹�����뤿��
;; ������Ǥ���
;;
;; Emacs20 / Emacs21 ��ưŪά��Ÿ���ϡ����ΤޤޤǤϡ�������ʿ��̾���
;; ��ñ���Ŭ�ڤ�ưŪά��Ÿ���Ǥ��ޤ��󡥱Ѹ�Ȱۤʤꡤ���ܸ�ˤ�ñ��
;; �֤ζ���¸�ߤ��ʤ����ᡤʸ��ñ��ñ�̤˴�ñ��ʬ�䤹�뤳�Ȥ������
;; �����Ȥ������θ����Ǥ���
;;
;; ��������ϡ�dabbrev-abbrev-char-regexp ��ưŪ���ѹ����뤳�Ȥˤ�äơ�
;; �����Ǥ�ưŪά��Ÿ����Ȥ��䤹�����褦�Ȥ����ΤǤ���

;;; History:

;; ���γ�ĥ�ϡ�http://www.sodan.org/~knagano/emacs/dabbrev-20.html ��
;; �Ƶ��Ҥ���Ƥ��뵻ˡ�� reimplement ������ΤǤ���

;;; Code:

(or (boundp 'MULE)			; Mule2 ��
    (featurep 'xemacs)			; XEmacs ����������
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
