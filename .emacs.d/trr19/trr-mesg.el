;;; trr-message.el - (C) 1996 Yamamoto Hirotaka <ymmt@is.s.u-tokyo.ac.jp>
;;; Last modified on Sun Jun 30 03:11:31 1996

;; This file is a part of TRR19, a type training package for Emacs19.
;; See the copyright notice in trr.el.base

(eval-when-compile
  ;; Shut Emacs' byte-compiler up
  (setq byte-compile-warnings '(redefine callargs)))

;; ��å������ϰʲ����ѿ����ͤˤ�äƷ����롣
;; �ˤʿͤ�����Ƥ�ä�Ŭ�ڤʥ�å������ηϤ��ۤ��Ƥ���뤳�Ȥ�˾�ࡣ
;; TRR decide its messages according to the following variables.
;; I hope you build more proper messaging system.

;; TRR:beginner-flag         ���� TRR �򤷤����ɤ���
;;			     whether this is the first play or not
;; TRR:random-flag           �ƥ����Ȥ������फ�ɤ���
;;			     whether random selecting is enabled or not
;; TRR:update-flag           ��Ͽ�����������ɤ���
;;			     whether there's necessity of updating high scores
;; TRR:pass-flag             ���ƥåפ�ѥ��������ɤ���
;;			     whether the player achieved the mark point
;; TRR:secret-flag           ��̩����Ԥ��ɤ���
;;			     whether TRR won't record the player's score or not
;; TRR:cheat-flag            ���餫�ε��路���԰٤򤷤����ɤ���
;;			     whether there's something doubtful or not
;; TRR:typist-flag           �����ԥ��Ȥ��ܻؤ����ɤ���
;;			     whether TRR runs in typist mode or not
;; TRR:steps                 ���ߤΥ��ƥå�
;;			     the player's current step
;; TRR:eval                  ����Ф�������
;;			     the player's current mark
;; TRR:whole-char-count      �ƥ����Ȥ�ʸ����
;;			     the number of characters in the text
;; TRR:high-score-old        ����ޤǤκǹ�����
;;			     the previous high score record
;; TRR:high-score            ����ޤǤκǹ�����
;;			     high score record
;; TRR:miss-type-ratio       �ߥ�Ψ (��ʬΨ)
;;			     miss type ratio
;; TRR:type-speed            �����ԥ�®�١�ʸ������ʬ��
;;			     the number of characters typed per minute
;; TRR:total-times           ���ޤǤ����Ѽ¹Բ��
;;			     the total number of TRR trials
;; TRR:total-time            ���ޤǤ����Ѽ¹Ի���
;;			     the total time spends in TRR trials
;; TRR:times-for-message     ���Υ��ƥåפǤ����Ѽ¹Բ��
;;			     the total number of TRR trials in the current step


(defun TRR:print-first-message-as-result ()
  (insert  (if TRR:japanese
	       " �褦����TRR�������ء�\n\
 ��٤ߤ�Ż��θ�ˡ�\n\
 �����ϻŻ��κ���ˤ�\n\
 ��ĥ�ä�TRR����⤦��"
	     " Welcome to TRR world! \n\
 Let's play TRR\n\
 After lunch, class,\n\
 Even during works!")))


(defun TRR:print-message ()
  (let ((fill-column (- (window-width) 3)))
    (delete-region (point-min) (point-max))
    (insert "  ")
    (TRR:print-message-main)
    (fill-region (point-min) (point-max))
    (goto-char (point-min))))


(defun TRR:print-message-main ()
  (let ((diff (- (* (1+ TRR:steps) 10) TRR:eval)))
    (cond
     (TRR:cheat-flag
      (TRR:message-for-cheater))
     (TRR:secret-flag
      (TRR:message-for-secret-player))
     (TRR:typist-flag
      (TRR:message-for-typist))
     (TRR:beginner-flag
      (TRR:message-for-beginner))
     ((and TRR:update-flag TRR:pass-flag)
      (insert
       (format (if TRR:japanese
		   "���ƥå�%d���ˤ����Ƶ�Ͽ��������ǤȤ���"
		 "Great. You've cleared the step %d with the new record!")
	       TRR:steps))
      (if (< (% TRR:high-score 100) (% TRR:high-score-old 100))
	  (progn
	    (TRR:message-specially-for-record-breaker))
	(TRR:message-for-record-breaker))
      (setq TRR:high-score-old TRR:high-score))
     (TRR:update-flag
      (insert (if TRR:japanese
		  "��Ͽ��������ǤȤ���"
		"Congratulations! You've marked the new record!"))
      (TRR:message-for-record-breaker)
      (setq TRR:high-score-old TRR:high-score))
     (TRR:pass-flag
      (insert
       (format (if TRR:japanese
		   "���ƥå�%d���ˤ���ǤȤ���"
		 "Nice! You've cleared the step %d.")
	       TRR:steps))
      (TRR:message-for-success))
     ((= TRR:eval 0)
      (insert (if TRR:japanese
		  "�����ʤ���Ѥ����������ä����Ϥ��ʤ�����"
		"Arn't you ashmed of having marked such an amazing score 0!")))
     ((< diff  60)
      (TRR:message-for-failed-one-1 diff))
     ((or (< diff 100) (> TRR:miss-type-ratio 30))
      (TRR:message-for-failed-one-2 diff))
     (t
      (TRR:message-for-failed-one-3 diff)))))


(defun TRR:message-for-cheater ()
  (cond 
   ((> TRR:eval 750)
    (insert (if TRR:japanese
		"����ʤ��ȤǤ����Ρ��Ѥ��Τ�ʤ�����"
	      "Aren't you ashamed of having done such a thing?")))
   ((< TRR:whole-char-count 270)
    (insert (if TRR:japanese
		"�ܶ��衣�ƥ����Ȥ����ʲ᤮������Ǥ��줷����"
	      "That's not fair! Too few letters in the text!")))
   ((and (< TRR:whole-char-count 520) TRR:typist-flag)
    (insert (if TRR:japanese
		"�ܶ��衣�ƥ����Ȥ����ʲ᤮������Ǥ��줷����"
	      "That's not fair! Too few letters in the text!")))))


(defun TRR:message-for-secret-player ()
  (cond
   (TRR:pass-flag
    (setq TRR:secret-flag nil)
    (setq TRR:update-flag nil)
    (setq TRR:beginner-flag nil)
    (TRR:print-message-main)
    (setq TRR:secret-flag t))
   ((> TRR:eval 300)
    (insert (if TRR:japanese
		"����ʹ⤤������Ф������ɤ�������̩�ˤ��Ƥ����Ρ�"
	      "What a good typist you are! You'd unveil your score.")))
   ((> TRR:eval 200)
    (insert (if TRR:japanese
		"�ȳ�ɸ���ۤ��Ƥ���̩�ˤ���ɬ�פ������ʤ���衣"
	      "Your score now reaches to the World standard. Go out public TRR world!")))
   ((> TRR:eval 120)
    (insert (if TRR:japanese
		"�Ѥ��������ʤ��������̩�ˤ���ΤϤ⤦���ޤ��礦��"
	      "Good score! Put an end to play in this secret world.")))
   (t
    (insert (if TRR:japanese
		"��������Ȥ���ä��Ѥ���������������Ф餯��̩��³���ޤ��礦��"
	      "Keep your score secret for a while.")))))


(defun TRR:message-for-beginner ()
  (cond
   ((= TRR:eval 0)
    (insert (if TRR:japanese
		"�����Ȥ����Τ����������줫�餫�ʤ�����Ϥ�ɬ�פ衣ƻ�Τ��Ĺ�����ɴ�ĥ��ޤ��礦��"
	      "0point... hopeless it is! You have to do much effort to step your level up.")))
   ((< TRR:eval 40)
    (insert (if TRR:japanese
		"���ʤ��Ȥ��ʸ�����餤�����гФ��뤳�ȡ��ȳ�ɬ�ܤ�100���˸����Ƥ��줫���ĥ��ޤ��礦��"
	      "You need to learn at least the position of the alphabet keys. Set your sights on 100pt: the World indispensable point.")))
   ((< TRR:eval 80)
    (insert (if TRR:japanese
		"�������֤���ʬ�Ф����褦�����ɤޤ��ޤ�����ȳ�ɬ�ܤ�100���˸����Ƥ��줫���ĥ��ޤ��礦��"
	      "Yes, you've learned the positions of keys; but still more! Set your sights on 100pt: the World indispensable point.")))
   ((< TRR:eval 130)
    (insert (if TRR:japanese
		"����Ū�ʵ��ѤϿȤ��դ��Ƥ���褦�����ɤޤ��ޤ�����ȳ�ɸ���200���˸����Ƥ��줫���ĥ��ޤ��礦��"
	      "You've learned some basic techniques; but still more! Go forward to 200pt: the World standard point.")))
   ((< TRR:eval 180)
    (insert (if TRR:japanese
		"�ʤ��ʤ��μ��Ϥ͡��Ǥ⥹�ԡ��ɤ����Τ�������­��ʤ���ȳ�ɸ���200���˸����Ƥ⤦������ĥ��ޤ��礦��"
	      "Your typing skill is rather high. More speedy & exactly! Go forward to 200pt: the World standard point.")))
   ((< TRR:eval 280)
    (insert (if TRR:japanese
		"�ʤ��ʤ�����͡��⤦������ĥ��жȳ���ɸ��300���򤭤ä����ˤǤ���"
	      "Nice. With some effort, you will surely reach 300pt: the World highly standard.")))
   ((< TRR:eval 380)
    (insert (if TRR:japanese
		"��������͡����ƤǤ��줰�餤�Ф���н�ʬ����Ǥ�ȳ���ή��400���˸����Ƥ⤦������ĥ��ޤ��礦��"
	      "Great. You have had sufficient skill. But push yourself to 400pt: the World firstclass.")))
   ((< TRR:eval 480)
    (insert (if TRR:japanese
		"���ä��������������Ф��ͤ���¿�ˤ��ʤ���衣�Ҥ�äȤ��ƥץ�ǤϤʤ������顩"
	      "Wonderful score! You may be a proffesional typist?")))
   (t
    (insert (if TRR:japanese
		"���ޤ�ˤ�Ķ��Ū������äȥ��ͥ��֥å��˺ܤ��衣"
	      "Too high score. You are sure to get a entry of the Guiness Book.")))))


(defun TRR:message-for-success ()
  (cond
   ((>= (- TRR:eval (* 10 (1+ TRR:steps))) 100)
    (insert (if TRR:japanese
		"���ʤ��ˤϴ�ñ�᤮���褦�͡�"
	      "This step must have been quite easy for you.")))
   ((<= TRR:times-for-message 2)
    (insert (if TRR:japanese
		"�ڤ����ˤ�����͡�"
	      "You made it effortlessly.")))
   ((<= TRR:times-for-message 4)
    (insert (if TRR:japanese
		"���ȴ�ñ�����ˤ�����͡�"
	      "You made it!")))
   ((<= TRR:times-for-message 8)
    (insert (if TRR:japanese
		"����äȤƤ����ä��褦�͡�"
	      "You carried out with a little trouble.")))
   ((<= TRR:times-for-message 16)
    (insert (if TRR:japanese
		"�����֤Ƥ����ä��褦�͡�"
	      "With much trouble, you accomplished this step's mark!")))
   ((<= TRR:times-for-message 32)
    (insert (if TRR:japanese
		"�褯��ĥ�ä���͡�"
	      "You've sweat it out. Nice record.")))
   ((<= TRR:times-for-message 64)
    (insert (if TRR:japanese
		"��ʬ��ϫ�����褦�͡�"
	      "You've had a very hard time.")))
   ((<= TRR:times-for-message 128)
    (insert (if TRR:japanese
		"�줷�ߤ̤�����͡�"
	      "You've gone through all sorts of hardships. ")))
   (t
    (insert 
     (format (if TRR:japanese
		 "%d���ĩ�魯��ʤ�Ƥ��������ǰ�Ǥ��Ȥ�����͡�"
	       "You've challenged this step %d times. Great efforts! ")
	     TRR:times-for-message)))))


(defun TRR:message-for-failed-one-1 (diff)
  (cond 
   ((< diff 10)
    (insert (if TRR:japanese
		"���Ȥۤ�ξ������ä��Τ�....�������ˤ����ä���͡�"
	      "Your score is slightly lower than the mark... How maddening!")))
   ((< diff 20)
    (insert (if TRR:japanese
		"�ˤ����ä���͡�"
	      "Disappointing!")))
   ((< diff 30)
    (insert (if TRR:japanese
		"����Ĵ�Ҥ衣"
	      "That's it!")))
   ((< diff 40)
    (insert (if TRR:japanese
		"�⤦��©����Ǥ�©ȴ���Ϥ���衣"
	      "Just one more effort. Don't goof off!")))
   ((< diff 50)
    (insert (if TRR:japanese
		"��ĥ��Ф��äȤǤ���"
	      "With much effort, and you will make it.")))
   (t
    (insert (if TRR:japanese
		"���Ϥ���Τߤ衣"
	      "What you have to do is nothing but making all possible effort.")))))


(defun TRR:message-for-failed-one-2 (diff)
  (cond 
   ((> TRR:miss-type-ratio 60)
    (insert (if TRR:japanese
		"�ߥ������ޤ�ˤ�¿�᤮�뤫�����ʤΤ衣�Ȥˤ������Τ��Ǥ���������ߤʤ������⤦���줷����ˡ�Ϥʤ��"
	      "Your hopeless point is based on your enormous misses! Practice the typing paying attention to correctness of typing keys.")))
   ((> TRR:miss-type-ratio 40)
    (insert (if TRR:japanese
		"�ߥ���¿�᤮���鿴�˵��äư�İ�Ŀ��Ť��Ǥ������򤷤ʤ�����"
	      "Too many wrong types! Remember your original purpose.")))
   ((> TRR:miss-type-ratio 24)
    (insert (if TRR:japanese
		"�ߥ���¿������Τ��Ǥ������򤷤ʤ�����"
	      "You failed frequently. Type accurate!")))
   ((> TRR:miss-type-ratio  8)
    (insert (if TRR:japanese
		"������������Ťͤʤ�����"
	      "Keep in practice.")))
   (t
    (insert (if TRR:japanese
		"���Τ��ǤäƤ�褦�����ɥ��ԡ��ɤ��٤�����®���Ǥ���������ߤʤ�����"
	      "You typed accurately, but too slow! Type more quickly.")))))


(defun TRR:message-for-failed-one-3 (diff)
  (cond 
   ((< diff 110)
    (insert (if TRR:japanese
		"��TRR��ƻ�ϰ����ˤ��Ƥʤ餺��"
	      "\"TRR was not built in a day.\"")))
   ((< diff 120)
    (insert (if TRR:japanese
		"��TRR�˲�ƻ�ʤ���"
	      "\"There is no royal road to TRRing.\"")))
   ((< diff 130)
    (insert
     (format (if TRR:japanese
		 "����ޤ���%d����Ф����ͤ����ä���%d���ʤ�Ƥ��ä����ɤ������Τ衣"
	       "Oh, no! Your best is %d, however marked %d point this time! What on earth be with you?")
	     TRR:high-score TRR:eval)))
   ((< diff 140)
    (insert
     (format (if TRR:japanese
		 "%d���Ϥޤ�����ä��Ρ�"
	       "Is the fact once you marked %d point an illusion?")
	     TRR:high-score)))
   (t
    (insert (if TRR:japanese
		"���ʤ��μ��ϤäƤ������٤��ä��Τ͡�"
	      "Your real ability is no more than this point. isn't it?")))))


(defun TRR:message-specially-for-record-breaker ()
  (cond 
   ((< TRR:high-score-old 100)
    (insert (if TRR:japanese
		"�Ĥ��˶ȳ�ɬ�ܤ�100�����ˤ͡����줫��϶ȳ�ɸ���200�����ܻؤ��ƴ�ĥ��ޤ��礦��"
	      "Congratulations! You reaches 100pt: the World indispensable. Next your target is 200pt: the World standard.")))
   ((< TRR:high-score-old 200)
    (insert (if TRR:japanese
		"�Ĥ��˶ȳ�ɸ���200�����ˤ͡����줫��϶ȳ���ɸ��300�����ܻؤ��ƴ�ĥ��ޤ��礦��"
	      "Congratulations! You reaches 200pt: the World standard. Next your target is 300pt: the World highly standard.")))
   ((< TRR:high-score-old 300)
    (insert (if TRR:japanese
		"�Ĥ��˶ȳ���ɸ��300�����ˤ͡����줫��϶ȳ���ή��400�����ܻؤ��ƴ�ĥ��ޤ��礦��"
	      "Congratulations! You reaches 300pt: the World highly standard. Next your target is 400pt: the World firstclass.")))
   ((< TRR:high-score-old 400)
    (insert (if TRR:japanese
		"�Ĥ��˶ȳ���ή��400�����ˤ͡����줫��϶ȳ�Ķ��ή��500�����ܻؤ��ƴ�ĥ��ޤ��礦��"
	      "Congratulations! You reaches 400pt: the World firstclass. Next your target is 500pt: the world superclass.")))
   ((< TRR:high-score-old 500)
    (insert (if TRR:japanese
		"�Ĥ��˶ȳ�Ķ��ή��500�����ˤ͡����줫��϶ȳ�ĺ����600�����ܻؤ��ƴ�ĥ��ޤ��礦��"
	      "Congratulations! You reaches 500pt: the world superclass. Next your target is 600pt: the World supreme.")))
   (t
    (insert (if TRR:japanese
		"���ʤ��Τ褦�ʤ������ͤϽ��Ƥ衣�ץ�ˤʤ��"
	      "You are the most marvelous typist I've ever met. The title \"TRRer\" suits you well!")))))


(defun TRR:message-for-record-breaker ()
  (cond
   ((< TRR:high-score  67)
    (insert (if TRR:japanese
		"�ȳ�ɬ�ܤ�100���ؤ��ƴ�ĥ�äơ�"
	      "Keep aiming at 100pt: the World indispensable.")))
   ((< TRR:high-score 100)
    (insert (if TRR:japanese
		"�ȳ�ɬ�ܤ�100���ޤǤ⤦�����衣"
	      "You are close to 100pt: the World indispensable.")))
   ((< TRR:high-score 167)
    (insert (if TRR:japanese
		"�ȳ�ɸ���200���ܻؤ��ƴ�ĥ�äơ�"
	      "Keep aiming at 200pt: the World standard.")))
   ((< TRR:high-score 200)
    (insert (if TRR:japanese
		"�ȳ�ɸ���200���ޤǤ⤦�����衣"
	      "You are close to 200pt: the World standard.")))
   ((< TRR:high-score 267)
    (insert (if TRR:japanese
		"�ȳ���ɸ��300���ܻؤ��ƴ�ĥ�äơ�"
	      "Keep aiming at 300pt: the World highly standard.")))
   ((< TRR:high-score 300)
    (insert (if TRR:japanese
		"�ȳ���ɸ��300���ޤǤ⤦�����衣"
	      "You are close to 300pt: the World highly standard.")))
   ((< TRR:high-score 367)
    (insert (if TRR:japanese
		"�ȳ���ή��400���ܻؤ��ƴ�ĥ�äơ�"
	      "Keep aiming at 400pt: the World firstclass.")))
   ((< TRR:high-score 400)
    (insert (if TRR:japanese
		"�ȳ���ή��400���ޤǤ⤦�����衣"
	      "You are close to 400pt: the World firstclass.")))
   ((< TRR:high-score 467)
    (insert (if TRR:japanese
		"�ȳ�Ķ��ή��500���ܻؤ��ƴ�ĥ�äơ�"
	      "Keep aiming at 500pt: the world superclass.")))
   ((< TRR:high-score 500)
    (insert (if TRR:japanese
		"�ȳ�Ķ��ή��500���ޤǤ⤦�����衣"
	      "You are close to 500pt: the world superclass.")))
   ((< TRR:high-score 567)
    (insert (if TRR:japanese
		"�ȳ�ĺ����600���ޤ��ܻؤ��ƴ�ĥ�äơ�"
	      "Keep aiming at 600pt: the World supreme.")))
   ((< TRR:high-score 600)
    (insert (if TRR:japanese
		"�ȳ�ĺ����600���ޤǤ⤦�����衣"
	      "You are close to 600pt: the World supreme.")))
   (t
    (insert (if TRR:japanese
		"�褯�����ޤǤ���͡����ʤ�����ɸ�ϰ��β��ʤΡ�"
	      "What is interesting to you? What you are aiming at?")))))


(defun TRR:message-for-typist ()
  (cond
   (TRR:beginner-flag
    (insert (if TRR:japanese
		"�����ԥ��Ȥؤ�ƻ�ϸ�������衣���ʤ��Ȥ�300���򥳥󥹥���Ȥ˽Ф��褦�˴�ĥ�äơ�"
	      "The way to the typist is severe. Keep makeing 300pt every time."))
    (setq TRR:beginner-flag nil))
   ((and TRR:pass-flag (not TRR:update-flag))
    (setq TRR:typist-flag nil)
    (TRR:print-message-main)
    (setq TRR:typist-flag t))
   ((and TRR:update-flag TRR:pass-flag)
    (insert (if TRR:japanese
		"��Ͽ����������"
	      "You've marked a new record. And "))
    (setq TRR:typist-flag nil)
    (setq TRR:update-flag nil)
    (TRR:print-message-main)
    (setq TRR:typist-flag t))
   (TRR:update-flag (insert (if TRR:japanese
				"��Ͽ��������ǤȤ���"
			      "Nice! You've marked a new record.")))
   ((> TRR:miss-type-ratio 30)
    (insert (if TRR:japanese
		"���ʤ��ˤ�̵���衣�����ԥ��Ȥˤʤ��ʤ����ʬ�ͤ��ʤ����Ȥ͡�"
	      "You are not up to Typist mode. Leave here for a while.")))
   ((> TRR:miss-type-ratio 20)
    (insert (if TRR:japanese
		"�����ʤ���Ѥ���������͡����ζ����򶻤˿�����߹��ߤʤ�����"
	      "0pt! Aren't you ashamed?  Engrave this humiliation deeply engraved on my mind.")))
   ((> TRR:miss-type-ratio 15)
    (insert (if TRR:japanese
		"�ߥ������ޤ�ˤ�¿�᤮���ж���á�����Ϥ�褦�˥����פ��ʤ�����"
	      "Excessively many miss types! Make assurance double sure.")))
   ((> TRR:miss-type-ratio 10)
    (insert (if TRR:japanese
		"�ߥ���¿�᤮����äȿ��Ť˥����פ��ʤ�����"
	      "Too many typos. Type more carefully.")))
   ((> TRR:miss-type-ratio 6)
    (insert (if TRR:japanese
		"�ߥ���¿�����äȿ��Ť˥����פ�������������衣"
	      "Many typos. Take more care of typing.")))
   (t
    (setq TRR:typist-flag nil)
    (TRR:print-message-main)
    (setq TRR:typist-flag t))))
