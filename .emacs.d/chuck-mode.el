;;; ChucK major mode for editing ChucK code and hopefully in the
;;; future also updating a running ChucK engine
;;;
;;; (c) 2004 Mikael Johansson

;; mode hook for user defined actions
(defvar chuck-mode-hook nil)

;;; CHANGE THIS to something that fits your system!!!
(defvar chuck-exec "c:/Chuckproj/chuck.exe")

;; ChucK as an internal listener does not work well. Run it externally
;; and control it internally.
;(defun run-chuck ()
;  "Start a ChucK listener"
;  (interactive)
;  (start-process "ChucK" "*ChucK*" chuck-exec "--loop"))
;(defun kill-chuck ()
;  "Kills a running ChucK listener"
;  (interactive)
;  (call-process chuck-exec nil 0 "--kill"))
(defun chuck-add-code () 
  "Add buffer to running ChucK"
  (interactive)
  (if (buffer-modified-p)
      (message "You need to save first")
    (call-process chuck-exec nil 0 "--add" buffer-file-name)))
(defun chuck-remove-code ()
  "Remove code snippet from running ChucK"
  (interactive)
  (progn
    (setq snippet (read-string "Which snippet would you remove? " nil nil nil))
    (call-process chuck-exec nil 0 "--remove" snippet)))
(defun chuck-replace-code ()
  "Replace code snippet in running ChucK with buffer"
  (interactive)
  (if (buffer-modified-p)
      (message "You need to save first")
    (progn
      (let (snippet (read-string "Which snippet would you replace? " nil nil nil))
	(call-process chuck-exec nil 0 "--replace" snippet buffer-file-name)))))
(defun chuck-status ()
  "Tell ChucK to report status"
  (interactive)
  (call-process chuck-exec nil 0 "--status"))

;; keymap for ChucK mode
(defvar chuck-mode-map
  (let ((chuck-mode-map (make-keymap)))
    (define-key chuck-mode-map (kbd "<RET>") 'newline-and-indent)    
                                                 
    (define-key chuck-mode-map [menu-bar chuck]  
      (cons "ChucK" (make-sparse-keymap "ChucK")))    
                                                 
    (define-key chuck-mode-map "\M-s" 'chuck-status)  
    ;; M-s is normally undefined                 
    (define-key chuck-mode-map [menu-bar chuck chuck-status]    
      '("Query ChucK status" . chuck-status))    
                                                 
    (define-key chuck-mode-map "\M-r" 'chuck-replace-code) 
    ;; M-r normally move-to-window-line          
    (define-key chuck-mode-map [menu-bar chuck chuck-replace-code]   
      '("Replace code in running ChucK with buffer" . chuck-replace-code))
                                                 
    (define-key chuck-mode-map "\M-e" 'chuck-remove-code)  
    ;; M-e is normally sentence-end              
    (define-key chuck-mode-map [menu-bar chuck chuck-remove-code]    
      '("Remove code from running ChucK" . chuck-remove-code))  
                                                 
    (define-key chuck-mode-map "\M-a" 'chuck-add-code)
    ;; M-e is normally forward-sentence          
    (define-key chuck-mode-map [menu-bar chuck chuck-add-code]  
      '("Add buffer to running ChucK" . chuck-add-code))   
                                                 
    chuck-mode-map)
  "Keymap for ChucK major mode")


;; Filename binding
(add-to-list 'auto-mode-alist '("\\.ck\\'" . chuck-mode))

;; Syntax highlighting
(defconst chuck-font-lock-keywords-1
  (list
   (cons (concat "\\<for\\>\\|" "\\<while\\>\\|" "\\<break\\>\\|" "\\<if\\>\\|"
	    "\\<else\\>\\|" "\\<then\\>\\|" "\\<NULL\\>\\|" "\\<null\\>\\|"
	    "\\<return\\>") 'font-lock-keyword-face)
   (cons (concat "\\<until\\>\\|" "\\<before\\>\\|" "\\<after\\>\\|"
	    "\\<at\\>\\|" "\\<function\\>\\|" "\\<fun\\>\\|" "\\<new\\>\\|"
	    "\\<class\\>\\|" "\\<extends\\>\\|" "\\<implements\\>\\|"
	    "\\<until\\>\\|" "\\<before\\>\\|" "\\<after\\>\\|" "\\<at\\>\\|"
	    "\\<function\\>\\|" "\\<fun\\>\\|" "\\<new\\>\\|" "\\<class\\>\\|"
	    "\\<extends\\>\\|" "\\<implements\\>\\|" "\\<public\\>\\|"
	    "\\<protected\\>\\|" "\\<private\\>\\|" "\\<static\\>\\|"
	    "\\<const\\>\\|" "\\<spork\\>") 'font-lock-keyword-face)
   (cons (concat "\\<=>\\>\\|" "\\<=<\\>\\|" "\\<!=>\\>\\|" "\\<->\\>\\|"
	    "\\<<-\\>\\|" "\\<+->\\>\\|" "\\<-->\\>\\|" "\\<*->\\>\\|"
	    "\\</->\\>\\|" "\\<&->\\>\\|" "\\<|->\\>\\|" "\\<^->\\>\\|"
	    "\\<>>->\\>\\|" "\\<<<->\\>\\|" "\\<%->\\>\\|" "\\<@=>\\>\\|"
	    "\\<+=>\\>\\|" "\\<-=>\\>\\|" "\\<*=>\\>\\|" "\\</=>\\>\\|"
	    "\\<&=>\\>\\|" "\\<|=>\\>\\|" "\\<^=>\\>\\|" "\\<>>=>\\>\\|"
	    "\\<<<=>\\>\\|" "\\<%=>\\>") 'font-lock-builtin-face)
   (cons (concat "\\<std\\>\\|" "\\<abs\\>\\|" "\\<fabs\\>\\|"
	    "\\<rand\\>\\|" "\\<randf\\>\\|" "\\<rand2f\\>\\|" "\\<randi\\>\\|" "\\<rand2\\>\\|"
	    "\\<sgn\\>\\|" "\\<system\\>\\|" "\\<aoti\\>\\|" "\\<atof\\>\\|"
	    "\\<getenv\\>\\|" "\\<setenv\\>\\|" "\\<math\\>\\|" "\\<sin\\>\\|"
	    "\\<cos\\>\\|" "\\<tan\\>\\|" "\\<asin\\>\\|" "\\<acos\\>\\|"
	    "\\<atan\\>\\|" "\\<atan2\\>\\|" "\\<sinh\\>\\|" "\\<cosh\\>\\|"
	    "\\<tanh\\>\\|" "\\<hypot\\>\\|" "\\<pow\\>\\|"
	    "\\<sqrt\\>\\|" "\\<exp\\>\\|" "\\<log\\>\\|" "\\<log2\\>\\|"
	    "\\<log10\\>\\|" "\\<floor\\>\\|" "\\<ceil\\>\\|" "\\<round\\>\\|"
	    "\\<trunc\\>\\|" "\\<fmod\\>\\|" "\\<remainder\\>\\|" "\\<min\\>\\|"
	    "\\<max\\>\\|" "\\<nextpow2\\>\\|" "\\<pi\\>\\|" "\\<twopi\\>\\|"
	    "\\<math.e\\>\\|" "\\<sin\\>\\|" "\\<cos\\>\\|"
	    "\\<tan\\>\\|" "\\<asin\\>\\|" "\\<acos\\>\\|" "\\<atan\\>\\|"
	    "\\<atan2\\>\\|" "\\<sinh\\>\\|" "\\<cosh\\>\\|" "\\<tanh\\>\\|"
	    "\\<hypot\\>\\|" "\\<pow\\>\\|" "\\<sqrt\\>\\|" "\\<exp\\>\\|"
	    "\\<log\\>\\|" "\\<log2\\>\\|" "\\<log10\\>\\|" "\\<floor\\>\\|"
	    "\\<ceil\\>\\|" "\\<round\\>\\|"
	    "\\<trunc\\>\\|" "\\<fmod\\>\\|" "\\<remainder\\>\\|" "\\<min\\>\\|"
	    "\\<max\\>\\|" "\\<nextpow2\\>\\|" "\\<machine\\>\\|" "\\<add\\>\\|"
	    "\\<remove\\>\\|" "\\<replace\\>\\|" "\\<status\\>\\|" "\\<spork\\>\\|"
	    "\\<net\\>\\|" "\\<init\\>\\|" "\\<data\\>\\|" "\\<data\\>\\|"
	    "\\<send\\>\\|" "\\<bind\\>\\|" "\\<connect\\>") 
	 'font-lock-function-name-face)
   (cons (concat "\\<dac\\>\\|" "\\<adc\\>\\|"
	    "\\<blackhole\\>\\|" "\\<gain\\>\\|" "\\<noise\\>\\|"
	    "\\<impulse\\>\\|" "\\<step\\>\\|" "\\<halfrect\\>\\|"
	    "\\<fullrect\\>\\|" "\\<zerox\\>\\|" "\\<delayp\\>\\|"
	    "\\<sndbuf\\>\\|" "\\<phasor\\>\\|"
	    "\\<sinosc\\>\\|" "\\<pulseosc\\>\\|" "\\<sqrosc\\>\\|"
	    "\\<triosc\\>\\|" "\\<sawosc\\>\\|" "\\<netout\\>\\|"
	    "\\<netin\\>\\|" "\\<BandedWG\\>\\|" "\\<BlowBotl\\>\\|"
	    "\\<BlowHole\\>\\|" "\\<Bowed\\>\\|" "\\<Brass\\>\\|"
	    "\\<Clarinet\\>\\|" "\\<Flute\\>\\|"
	    "\\<Mandolin\\>\\|" "\\<ModalBar\\>\\|" "\\<Moog\\>\\|"
	    "\\<Saxofony\\>\\|" "\\<Shakers\\>\\|" "\\<Sitar\\>\\|"
	    "\\<StifKarp\\>\\|" "\\<VoicForm\\>\\|" "\\<FM\\>\\|"
	    "\\<BeeThree\\>\\|" "\\<FMVoices\\>\\|" "\\<HevyMetl\\>\\|"
	    "\\<PercFlut\\>\\|" "\\<Rhodey\\>\\|"
	    "\\<TubeBell\\>\\|" "\\<Wurley\\>\\|" "\\<Delay\\>\\|"
	    "\\<DelayA\\>\\|" "\\<DelayL\\>\\|" "\\<Echo\\>\\|" "\\<Envelope\\>\\|"
	    "\\<ADSR\\>\\|" "\\<BiQuad\\>\\|" "\\<Filter\\>\\|" "\\<OnePole\\>\\|"
	    "\\<TwoPole\\>\\|" "\\<OneZero\\>\\|" "\\<TwoZero\\>\\|"
	    "\\<PoleZero\\>\\|" "\\<JCRev\\>\\|" "\\<NRev\\>\\|" "\\<PRCRev\\>\\|"
	    "\\<Chorus\\>\\|" "\\<Modulate\\>\\|" "\\<PitShift\\>\\|"
	    "\\<SubNoise\\>\\|" "\\<WvIn\\>\\|" "\\<WaveLoop\\>\\|" "\\<WvOut\\>") 
    'font-lock-constant-face)
   (cons (concat "\\<NULL\\>\\|" "\\<adc\\>\\|"
	    "\\<array\\>\\|" "\\<blackhole\\>\\|" "\\<bunghole\\>\\|"
	    "\\<cherr\\>\\|" "\\<chout\\>\\|" "\\<class\\>\\|" "\\<code\\>\\|"
	    "\\<compiler\\>\\|" "\\<dac\\>\\|" "\\<day\\>\\|" "\\<double\\>\\|"
	    "\\<dur\\>\\|" "\\<endl\\>\\|" "\\<false\\>\\|" "\\<float\\>\\|"
	    "\\<function\\>\\|" "\\<global\\>\\|" "\\<host\\>\\|" "\\<hour\\>\\|"
	    "\\<int\\>\\|" "\\<language\\>\\|" "\\<machine\\>\\|" "\\<midiin\\>\\|"
	    "\\<midiout\\>\\|" "\\<minute\\>\\|" "\\<ms\\>\\|" "\\<now\\>\\|"
	    "\\<null\\>\\|" "\\<object\\>\\|"
	    "\\<pattern\\>\\|" "\\<math.pi\\>\\|" "\\<samp\\>\\|" "\\<second\\>\\|"
	    "\\<shred\\>\\|" "\\<single\\>\\|" "\\<start\\>\\|" "\\<stderr\\>\\|"
	    "\\<stdout\\>\\|" "\\<string\\>\\|" "\\<thread\\>\\|" "\\<time\\>\\|"
	    "\\<transport\\>\\|" "\\<true\\>\\|" "\\<tuple\\>\\|" "\\<ugen\\>\\|"
	    "\\<uint\\>\\|" "\\<void\\>\\|" "\\<week\\>") 'font-lock-type-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Highlighting for ChucK mode")
(defvar chuck-font-lock-keywords chuck-font-lock-keywords-1
  "Default highlighting for ChucK mode")

;; Indenting for ChucK mode
(defun chuck-indent-line () 
  "Indent current line as ChucK code"
  (interactive)
  (beginning-of-line)
  (if (bobp)  ;; Start of buffer starts out unindented
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at ".*}") ; Closing a block
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) default-tab-width)))
	    (if (< cur-indent 0)
		(setq cur-indent 0)))
	(save-excursion
	  (while not-indented
	    (forward-line -1)
	    (if (looking-at ".*}") ; Earlier block closed
		(progn
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil))
	      (if (looking-at ".*{") ; In open block
		  (progn
		    (setq cur-indent (+ (current-indentation) default-tab-width))
		    (setq not-indented nil))
		(if (bobp)
		    (setq not-indented nil)))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0)))))

;; Syntax table
(defvar chuck-mode-syntax-table nil "Syntax table for ChucK mode")
(setq chuck-mode-syntax-table
  (let ((chuck-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "_" chuck-mode-syntax-table)
    (modify-syntax-entry ?/ ". 12" chuck-mode-syntax-table)
    (modify-syntax-entry ?\n ">" chuck-mode-syntax-table)
    chuck-mode-syntax-table))

;; Entry point
(defun chuck-mode ()
  "Major mode for editing ChucK music/audio scripts"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table chuck-mode-syntax-table)
  (use-local-map chuck-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(chuck-font-lock-keywords))
  (set (make-local-variable 'indent-line-function)
       'chuck-indent-line)
  (setq major-mode 'chuck-mode)
  (setq mode-name "ChucK")
  (setq default-tab-width 4)
  (run-hooks 'chuck-mode-hook))

(provide 'chuck-mode)


; (setq foo (regexp-opt '("dac" "adc" "blackhole" "gain" "noise" "impulse" "step" "halfrect" "fullrect" "zerox" "delayp" "sndbuf" "phasor" "sinosc" "pulseosc" "sqrosc" "triosc" "sawosc" "netout" "netin" "BandedWG" "BlowBotl" "BlowHole" "Bowed" "Brass" "Clarinet" "Flute" "Mandolin" "ModalBar" "Moog" "Saxofony" "Shakers" "Sitar" "StifKarp" "VoicForm" "FM" "BeeThree" "FMVoices" "HevyMetl" "PercFlut" "Rhodey" "TubeBell" "Wurley" "Delay" "DelayA" "DelayL" "Echo" "Envelope" "ADSR" "BiQuad" "Filter" "OnePole" "TwoPole" "OneZero" "TwoZero" "PoleZero" "JCRev" "NRev" "PRCRev" "Chorus" "Modulate" "PitShift" "SubNoise" "WvIn" "WaveLoop" "WvOut")))
