;;; asmibm-mode --- A major mode to handle Assembler/IBM for MVS or Z/OS JCL.
;;;; -*- Mode: Emacs-Lisp -*-

;;; asmibm-mode.el
;;
;; See the file COPYING for license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: December 2nd, 2020.
;;
;; Version: 20201206.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; A major mode to handle IBM Assemblers, mostly for MVS or z/OS.


;;; Code:

;;;; ASM IBM Mode Setup.

(defgroup asmibm nil
  "The major mode to handle Assemblers for IBM 360/370 and z Series machines.

This mode is part of the IRON MAIN package."
  :group 'languages)

(defcustom asmibm-os-flavor "MVS 3.8j"
  "The current flavor of MVS used.

The values of this variable are strings starting either with 'MVS' or
'z/OS'.  Other variants are acceptable as long as the 'main' OS name
comes first.

The value 'MVS 3.8' is the default one, being the version of MVS
that IBM release in the public domain."
  :group 'asmibm
  :type 'string)


;;; Things to highlight.

;;; We inherit from standard asm-mode, so we just need things for
;;; comments at the end of a line.

(defvar asmibm-strings
  "'.*'"
  "ASM IBM strings.")


(defvar asmibm-names
  "^\\([[:alpha:]][[:alnum:]]*\\)"
  "ASM IBM names (or labels).

These are the 'names'of instructions.")


;; (defvar asmibm-names
;;   "^\\([^* ][[:graph:]]+\\)"
;;   "ASM IBM names.
;;
;; These are the 'names'of instructions.")

(defvar asmibm-instructions
  "^\\([[:alpha:]][[:alnum:]]*\\)?[[:blank:]]+\\([[:alpha:]][[:alnum:]]*\\)"
  "ASM IBM instructions.

These are the instructions mnemonics.
The second regexp group contains the istruction name.")

;; (defvar asmibm-instructions
;;   "^\\([^* ][[:graph:]]+\\| +\\) +\\([[:graph:]]+\\)"
;;   "ASM IBM instructions.

;; These are the instructions mnemonics.")


(defvar asmibm-registers
  "R[0-9][0-5]?"
  "ASM IBM register names.")


(defvar asmibm-card-end-comments-0
  "^\\*.*$"
  "ASM IBM 'comment' card.

A '*' at the beginning of the card (line) marks a comment.")


(defvar asmibm-card-end-comments-1
  "^\\([[:alpha:]][[:alnum:]]*\\)?[[:blank:]]+[[:alpha:]][[:alnum:]]*[[:blank:]]+[-[:alnum:],*='()+]?\\([[:graph:]]*\\)$
     "
  "ASM IBM 'end of card' comments for 'full' cards.

Anything after the 'operands' in a card is a comment; this regexp
selects them.")


;; (defvar asmibm-card-end-comments-1
;;   "^[^* ]+ +[[:graph:]]+ +[[:graph:]]+ +\\([[:graph:]].*\\)"
;;   "ASM IBM 'end of card' comments for 'full' cards..

;; Anything after the 'operands' in a card is a comment; this regexp
;; selects them.")


(defvar asmibm-card-end-comments-2
  "^ +[[:graph:]]+ +\\([[:graph:]].*\\)"
  "ASM IBM 'end of card' comments for 'continuation' cards.

Anything after the 'operands' in a card is a comment; this regexp
selects them in case of 'continuation' cards that do not have the
'name' and 'operation'.")


(defvar asmibm-card-end-comments-3
  "^ +[[:graph:]]+ +[[:graph:]]+ +\\([[:graph:]].*\\)"
  "ASM IBM 'end of card' comments for 'no operands' cards.

Anything after the 'operands' in a card is a comment; this regexp
selects them in case of 'no operands' cards that do not have the
'name' and 'operands'; however, the 'operands' must be at least one
non blank character.")


;; asmibm-attribute-symbol
;; Note that the following does only attributed symbols and not
;; symbols and expressions as per IBM specs.

(defvar asmibm-attributed-symbol
  "\\([LKDINOST]'[[:alnum:]]+\\|L'\\*\\)"

  "ASM IBM 'attributed terms.

This is typically the 'length' of something, e.g.,

    L'FOOBAR

indicating the length of FOOBAR.
Other 'attributes' are introduced by the other letters.

See, e.g.: https://www.ibm.com/docs/en/zos/2.1.0?topic=terms-other-attribute-references
")


(defvar asmibm-jcl
  "^//.*$"
  "Lines starting with '//' are assumed to be JCL which wraps the Assembler.")


;; asmibm-card-remarks
;;
;; IBM assembly is old, and it uses some messed up syntax, especially
;; when it comes to 'end of lined (or card) remarks' (i.e., comments)
;; and 'strings'.
;;
;; Hence the hairball below, to be used for font-lock.
;;
;; Notes:
;; This is the first attempt.  But it looks like font-lock-mode (and
;; font-lock-fontifiy-buffer, alongside font-lock-defaults) keep
;; calling it expecting it to work several times in a row.  In other
;; words it should always return t even is the match-data were empty:
;; this seems incostistent, although there could always be a match for
;; "^.*$".
;; This is the reason why we terminate a search with
;;
;;     (re-search-forward ".*$" nil nil)
;;
;; at the end of line.


(cl-defun asmibm-card-remarks (&optional (limit (line-end-position)))
  ;; When called programmatically `limit' is end of buffer.
  ;; (interactive)

  (message "\nASMIBM CARD REMARK: entered with limit = %s @%s" limit (point))

  (condition-case nil
      (let* ((current-point (point))
	     (post-instr-point
	      (re-search-forward asmibm-instructions nil nil))
	     
	     ;; Now point should be at the end of the instruction.
	     ;; I can start skipping over the operands characters,
	     ;; including strings and attributed symbols.
	     
	     (skipped-ws (skip-chars-forward "[:blank:]" limit))
	     (first-operand-point (point))
	     (operand-pos first-operand-point)
	     (first-remark-point nil)
	     )
        (message "ASMIBM CARD REMARK: asserting")
	(cl-assert (not (char-equal (following-char) ?\ ))) ; Being paranoid.
	(cl-assert (not (bolp)))        ; Yep, very paranoid.
	(message "ASMIBM CARD REMARK: asserted")

	;; Now I am at the first character of the 'operands' of the
	;; instruction.  Note that, as per IBM documentation, to
	;; have "remarks" that are comments for instructions that
	;; use zero arguments, there should be a lone comma on the
	;; line, surrounded my spaces, signifying "I am the operand
	;; list".

	(cl-labels ((finish-remark-parsing
		     ()
		     (message "ASMIBM CARD REMARK: finishing")
		     
		     ;; We get here in one case.
		     ;; We have a blank character at hand and we need
		     ;; to see whether there is 'end of line/card'
		     ;; comment.
		     ;; If so we need to fix the match and return t,
		     ;; otherwise we return nil.
		     (skip-chars-forward "[:blank:]" limit)

		     (message "ASMIBM CARD REMARK: char after blanks `%c'" (char-after))

		     (if (< (point) (line-end-position))
			 ;; Something non blank found before end of line.
			 ;; Fix match
			 (cl-return-from asmibm-card-remarks
			   (let ((result (re-search-forward ".*$" limit t)))
			     (message "ASMIBM CARD REMARK: finishing match data \"%s\" %s"
			      	      (match-string 0)
			     	      (match-data))
			     result))
		       (cl-return-from asmibm-card-remarks
			 (progn
			   (message "ASMIBM CARD REMARK: finishing with no match")
			   ;; nil
			   (finish-eol-success)
			   )
			 ))
		     )

		    (finish-eol-success
		     ()
		     (progn
		       (message "ASMIBM CARD REMARK: finishing at eol %s %s"
				(point)
				(line-end-position))
		       (cl-return-from asmibm-card-remarks
			 (re-search-forward ".*$" limit t))
		       )
		     )
		    		    
		    (advance
		     ()
		     (forward-char)
		     (setq operand-pos (point))
		     )
		    
		    (in-string
		     ()
		     ;; We are just before a "'"

		     (message "ASMIBM CARD REMARK: in-string")
		     ;; Just to be paranoid...
		     (cl-assert (char-equal ?' (char-after))
			        t
			        "in-string cl-label")
			       
		     (forward-char)
		     (let ((string-end-pos
			    (search-forward "'"
					    (line-end-position)
					    t))
			   )
		       ;; We should have skipped the string
		       ;; now (in a dumb way for the time
		       ;; being; there are probably some
		       ;; corner cases not considered here.)

		       ;; (if string-end-pos
		       ;; 	   (message "ASMIBM CARD REMARK: in-string after `%c'" (char-after))
		       ;; 	 (message "ASMIBM CARD REMARK: in-string ooooops."))

		       (if string-end-pos
			   (setq operand-pos string-end-pos) ; We are cool.
			 (setq operand-pos (point)) ; We pray.
			 ))
		     )
		    )
	  (message "ASMIBM CARD REMARK: in loop with operand-pos = %s, eol = %s"
	   	   operand-pos
	   	   (line-end-position))
	  
	  (while (and operand-pos
		      (< operand-pos limit)
		      (< operand-pos (line-end-position)))
	    
	    (let ((c (following-char)))
	      (message "ASMIBM CARD REMARK: in loop with c = `%c'" c)
	      
	      (cond ((equal (char-syntax c) ?\ ) ; Found a space; skip to first non blank.
		     (skip-chars-forward "[:blank:]" limit)
		     (finish-remark-parsing)
		     )

		    ((char-equal c ?') ; Tricky part: string or attribute?
		     (let ((prev-char (preceding-char)))

		       (message "ASMIBM CARD REMARK: attribute with prev c = `%c'"
		                prev-char)
		       
		       ;; Is this an attributed expression
		       (if (cl-find prev-char "LKDINOST") ; Possible attribute.
			   (cl-case (char-before (1- (point)))
			     ((?+ ?- ?/ ?* ?\( ?\) ?, ?\ ?\t)
			      ;; Possible 'operators'. Last is space!
			      ;; We have (almost surely) an attributed
			      ;; expression.
			      ;; Keep going without bothering to deal
			      ;; with strings.

			      ;; (message "ASMIBM CARD REMARK: attributed")
			      (advance)
			      )
			     (t
			      ;; Something else? We are probably in
			      ;; the middle of an operand and
			      ;; starting a proper string.
			     
			      (in-string)
			      )
			     )	; cl-case
			 ;; else, we are in a string (we hope).
			 (in-string)
			 ))
		     )
		    (t			; Any other character
		     (advance)
		     )
		    ))
	    )				; while...

	  (message "ASMIBM CARD REMARK: no match")
	  (if (>= operand-pos limit)
	      nil ; We are out of the loop past the limit, we finish with NIL.
	    (finish-eol-success)
	    )
	  )				; cl-labels...
	)				; let*...
    ;; condition-case catchers...
    (search-failed (message "ASMIBM CARD REMARK: search failed.") nil)
    ))


;;; ASM IBM faces.

(defcustom asmibm-string-face 'font-lock-string-face
  "The face used to fontify strings (single-quoted) in ASM IBM mode."
  :group 'asmibm
  :type 'symbol
  )

(defcustom asmibm-names-face 'font-lock-function-name-face
  "The face used to fontify 'names' in ASM IBM mode."
  :group 'asmibm
  :type 'symbol
  )

(defcustom asmibm-operations-face 'font-lock-keyword-face
  "The face used to fontify 'operations' in ASM IBM mode."
  :group 'asmibm
  :type 'symbol
  )

(defcustom asmibm-operands-face 'font-lock-type-face
  "The face used to fontify 'operands' in ASM IBM mode."
  :group 'asmibm
  :type 'symbol
  )

(defcustom asmibm-operators-face 'font-lock-builtin-face
  "The face used to fontify 'operators' in ASM IBM mode."
  :group 'asmibm
  :type 'symbol
  )

(defface asmibm-comment-face-red
  '((t :foreground "red" :weight bold))
  "Face to colorize comments in ASM IBM mode."
  :group 'asmibm
  )


(defcustom asmibm-comment-face 'font-lock-comment-face ; 'asmibm-comment-face-red
  "The face used to fontify 'comments' in ASM IBM mode."
  :group 'asmibm
  :type 'symbol
  )

;; (defcustom asmibm-comment-face 'font-lock-comment-face
;;   "The face used to fontify 'comments' in ASM IBM mode."
;;   :group 'asmibm
;;   :type 'symbol
;;   )

(defcustom asmibm-grey-face 'shadow
  "The face used to 'fontify out' possible JCL when assembler is embedded."
  :group 'asmibm
  :type 'symbol
  )


(defvar asmibm-font-lock-keywords
  `(
    (,asmibm-names . (1 asmibm-names-face))
    (,asmibm-instructions . (2 asmibm-operations-face))
    ;; (,asmibm-registers . ,asmibm-operators-face)

    
    (,asmibm-card-end-comments-0 . ,asmibm-comment-face)
    (asmibm-card-remarks . ,asmibm-comment-face)
    ;; (,asmibm-card-end-comments-1 . (2 ,asmibm-comment-face))
    ;; (,asmibm-card-end-comments-2 . (1 ,asmibm-comment-face))
    (,asmibm-card-end-comments-3 . (1 ,asmibm-comment-face))

    (,asmibm-attributed-symbol . (1 ,asmibm-operands-face))

    ;; (,asmibm-strings . (0 ,asmibm-string-face t))
    (,asmibm-strings . (0 ,asmibm-string-face))


    (,asmibm-jcl . (0 asmibm-grey-face t))
    )
  "The ASM IBM mode 'font-lock' 'keyword' specification."
  )


(defvar asmibm-font-lock-defaults
  (list 'asmibm-font-lock-keywords
	;; nil ; Do syntax based processing.
	t   ; Do not do syntax based processing.
	)
  "The ASM IBM mode 'font-lock' defaults specification."
  )


;;; asmibm-mode-syntax-table

(defvar asmibm-mode-syntax-table
  (let ((asmst (make-syntax-table)))
    ;; (modify-syntax-entry ?* ". 1" asmst)
    ;; (modify-syntax-entry ?\n "> " asmst)
    asmst
    )
  "The ASM IBM mode syntax table."
  )


;;; asmibm-keymap
;;; Not necessary.  define-derived-mode sets it up automatically.

(defvar asmibm-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km prog-mode-map) ; Inherit from prog-mode-map!
    
    km)
  "The ASM IBM mode key map.")



;;; asmibm-imenu-generic-expression

(defvar asmibm-imenu-generic-expression
  '((nil "^\\([^* /]+\\) +[[:graph:]]+$" 1) ; Also avoid JCL
    (nil "^\\([^* /]+\\) +[[:graph:]]+ .*$" 1)
    )
  "The ASM IBM Imenu regular expressions.")


;;; asmibm-mode

(define-derived-mode asmibm-mode asm-mode "ASM IBM"
  "ASM IBM mode is a major mode to edit IBM Assembler code.

IBM mainframes have a number of assemblers available; the latest
incarnation being \"High Level Assembler\" (HLASM).  This mode helps
in editing assembly code mostly for MVS or z/OS.

Notes:

Not all features of HLASM are currently supported."

  :syntax-table asmibm-mode-syntax-table

  (setq-local font-lock-defaults asmibm-font-lock-defaults)

  (face-remap-add-relative asmibm-operations-face  :weight 'bold)

  (face-remap-add-relative asmibm-operators-face
			   :weight 'bold
			   :foreground "Forest Green") ; May be too much.

  ;; Comments.
  ;; (setq-local comment-start "//\*")
  ;; (setq-local comment-end "")
  ;; (setq-local comment-start-skip
  ;;             "^//[[:graph:]]*[[:blank:]]+[[:graph:]]+[[:blank:]]+")

  ;; Set up the mode keymap.

  (use-local-map asmibm-mode-map)
  (local-set-key (kbd "RET") 'electric-indent-just-newline)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)


  ;; Set up tab stops.

  (setq-local tab-stop-list '(9 17 40))
  
  
  ;; Set up the menus.

  (easy-menu-define asmibm-mainframe-os-menu asmibm-mode-map
    "ASM IBM commands"
    '("ASM IBM OS"
      ["Submit Compilation" asmibm-compile]
      ["Submit Compilation and Go" asmibm-compile-and-go])
    )

  (setq-local imenu-generic-expression
	      (reverse asmibm-imenu-generic-expression))
  (imenu-add-to-menubar "ASM IBM Code")

  ;; Start the IRON MAIN minor mode, which sets up the ruler and the
  ;; "card" editing limits, plus the fill-column indicator.

  (when (fboundp 'iron-main-mode) (iron-main-mode))

  'asmibm-mode
  )


;;; Functions and Commands.

(defvar asmibm-not-implemented-flag t
  "Non-nil means the context where used is still unimplemented.")


;;; asmibm-compile-jcl

(defvar asmibm-compile-jcl
  "//EASMICJ  JOB (%s),'%s',
//            USER=%s,PASSWORD=%s,
//            CLASS=A,
//            MSGCLASS=%s,MSGLEVEL=(1,1)
//EASMICEX EXEC PGM=ASMFC,PARM.ASM='LIST,RENT,NODECK'
//EASMICEX.DD *
"
  "JCL job to compile a ASM IBM code.

This variable is acually a format string.")


(defun asmibm-prepare-job (&optional
			   acct
			   name
			   user
			   password
			   msgclass)
  (format asmibm-compile-jcl
	  (or acct "1")
	  (or name "EMACS COMPILES IRON ASSEMBLER")
	  (or user "HERC01")		; Sensible TK4- default.
	  (or password "CUL8TR")	; Ditto.
	  (or msgclass "H"))
  )


;;; asmibm-compile

(defun asmibm-compile (&optional port)
  "Compiles the buffer by submitting a job to the card reader on PORT."
  (interactive
   (if asmibm-not-implemented-flag
       (list nil)
     (let ((p (read-number "Card Reader number/port: " 3505))
	   )
       (list p)))
   )

  (unless port
    (setq port 3505))

  (message "Assembler compile not yet implemented; sorry...")
  )


;;; asmibm-compile-and-go

(defun asmibm-compile-and-go (&optional port)
  "Compile and execute the buffer on the mainframe.

This is obtained by submitting a job to the card reader on PORT."
  (interactive
   (if asmibm-not-implemented-flag
       (list nil)
     (let ((p (read-number "Card Reader number/port: " 3505))
	   )
       (list p)))
   )

  (unless port
    (setq port 3505))

  (message "Assembler compile-n-go not yet implemented; sorry...")
  )


;;;; Epilogue
;;;; ========

(add-to-list 'auto-mode-alist '("\\.asma\\'" . asmibm-mode))
(add-to-list 'auto-mode-alist '("\\.bal\\'" . asmibm-mode))

(provide 'asmibm-mode)

;;; asmibm-mode.el ends here
