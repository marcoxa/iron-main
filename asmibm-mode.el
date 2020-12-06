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
  "^\\([^* ][[:graph:]]+\\)"
  "ASM IBM names.

These are the 'names'of instructions.")


(defvar asmibm-instructions
  "^\\([^* ][[:graph:]]+\\| +\\) +\\([[:graph:]]+\\)"
  "ASM IBM instructions.

These are the instructions mnemonics.")


(defvar asmibm-registers
  "R[0-9][1-6]?"
  "ASM IBM register names.")


(defvar asmibm-card-end-comments-0
  "^\\*.*$"
  "ASM IBM 'comment' card.

A '*' at the beginning of the card (line) marks a comment.")


(defvar asmibm-card-end-comments-1
  "^[^* ]+ +[[:graph:]]+ +[[:graph:]]+ +\\([[:graph:]].*\\)"
  "ASM IBM 'end of card' comments for 'full' cards..

Anything after the 'operands' in a card is a comment; this regexp
selects them.")


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


(defvar asmibm-jcl
  "^//.*$"
  "Lines starting with '//' are assumed to be JCL which wraps the Assembler.")


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

(defcustom asmibm-comment-face 'font-lock-comment-face
  "The face used to fontify 'comments' in ASM IBM mode."
  :group 'asmibm
  :type 'symbol
  )

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
    (,asmibm-card-end-comments-1 . (1 ,asmibm-comment-face))
    ;; (,asmibm-card-end-comments-2 . (1 ,asmibm-comment-face))
    (,asmibm-card-end-comments-3 . (1 ,asmibm-comment-face))

    (,asmibm-strings . (0 ,asmibm-string-face t))

    (,asmibm-jcl . (0 asmibm-grey-face t))
    )
  "The ASM IBM mode 'font-lock' 'keyword' specification."
  )


(defvar asmibm-font-lock-defaults
  (list 'asmibm-font-lock-keywords
	nil ; Do syntax based processing.
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

  (iron-main-mode)

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

(provide 'asmibm-mode)

;;; asmibm-mode.el ends here
