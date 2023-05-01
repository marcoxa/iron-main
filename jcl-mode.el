;;; jcl-mode --- A major mode to handle MVS or Z/OS JCL.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; jcl-mode.el
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: December 2nd, 2020.
;;
;; Version: 20230501.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; A major mode to handle IBM MVS or z/OS JCL.
;;
;; The basic "grammar" of JCL is the following, "line-oriented" one:
;;
;;   //<name>? <operation> <operands> <comments>
;;
;; or
;;
;;   //*<comment>
;;
;; everything starting at the // or //* and ending at the end of
;; line, or better at the 72nd column with whitespaces meaningful.
;; Actually, we are not even talking about "lines", but we are
;; talking about "cards" (as it should be).
;;
;; In the following the naming just follows the above convention.
;;
;; The mode, for the time being, allows you to write JCL files,
;; pardon, "data sets", using Emacs editing facilities, plus two
;; useful facilities.
;;
;; The mode defines also two useful functions to "submit" your JCL
;; to a running instance of MVS or Z/OS, provided that
;;
;; 1. Your MVS or z/OS has a "card reader" listening on a port
;;    (default 3505).
;; 2. You have the necessary credentials to run the job on the OS.
;;
;; The two functions (which are available on the Emacs menubar) are
;; 'jcl-submit' (alias 'submit'; Emacs and ELisp do not define this
;; as a function) and 'jcl-submit-file' (a misnomer; it should be
;; "jcl-submit-dataset").  The two functions submit either the
;; current buffer or a file of your choice to the "card reader".
;;
;; Files with '.jcl' extensions are opend in 'jcl-mode'.
;;
;; Most behavior is customizable in the "jcl" group.
;;
;; If you manually downloaded the package, you can open the
;; 'testsubmit.jcl' file, which just executes 'IEFBR14' and
;; allocates a dataset and submit it to your MVS or z/OS
;; instance.
;;
;; The code has been tested with MVS TK4- and MVS "Jay Moseley"
;; build, running on a Hercules (SDL/Hyperion 4.2.x).
;; Useful links follow.
;;
;; Hercules: <http://www.hercules-390.eu/>
;; SDL/Hyperion: <https://github.com/SDL-Hercules-390>
;; TK4-: http://wotho.ethz.ch/tk4-/
;; Jay Moseley's site: http://www.jaymoseley.com/hercules/ (and
;;                     other useful things).
;;
;; To Do:
;;
;; Not all JCL keywords have been added to the font-lock machinery.
;; Packaging still a bit "in fieri"
;; Polymode would be very nice to have.
;; Some extra fixes to the font-lock machinery are a must.

;;; Code:

;;;; JCL Mode Setup.

(defgroup jcl nil
  "The major mode to manipulate IBM MVS and z/OS Job Control Language.

This mode is part of the IRON MAIN package."
  :group 'languages)

(defcustom jcl-mode-os-flavor "MVS 3.8j"
  "The current flavor of MVS used.

The values of this variable are strings starting either with 'MVS' or
'z/OS'.  Other variants are acceptable as long as the 'main' OS name
comes first.

The value 'MVS 3.8j' is the default one, being the version of MVS
that IBM release in the public domain."
  :group 'jcl
  :type 'string)


;;; Things to highlight.

;;; jcl-constants
;;; UNUSED.

(defvar jcl-mode--constants
  '("//" "/*" "//*")
  "JCL card starters.

These are not really 'constants', as JCL does not really have them.")


(defvar jcl-mode--strings
  "'.*'"
  "JCL strings regular expression.")


(defvar jcl-mode--operations
  '("JOB" "EXEC" "DD" "PROC" "PEND"
    "COMMAND" "CNTL" "ENDCNTL"
    "IF" "THEN" "ELSE" "ENDIF"
    "INCLUDE"
    "JCLLIB"
    "OUTPUT"
    "SET"
    "XMIT"
    )
  "JCL operations.

List build from the IBM publication \"MVS JCL User's Guide\" (z/OS Version 2 Relase3).")


;; jcl-operands
;; The list is incomplete.

(defvar jcl-mode--operands
  '("CLASS" "MSGCLASS" "MSGLEVEL" "USER" "PASSWORD" "NOTIFY" "TYPRUN"
    "PGM" "COND"
    "DISP" "NEW" "OLD" "KEEP" "CATLG" "SHARED" "SHR" "DELETE" "DEL"
    "VOL" "VOLUME" "SER" "SERIAL"
    "DSNAME" "DSN"
    "DSORG"
    "DDNAME"
    "UNIT"
    "SPACE" "CYL" "TRK"
    "DCB" "RECFM" "LRECL" "BLKSIZE"
    "SYSOUT"
    "DATA" "DLM"
    )
  "JCL operands.

The JCL operands are both positional and 'noun=adjective' ones, both
left and right of the '=' sign.  The identifiers used on the right are
quite varied.

In other languages, they would be the 'keyword' arguments.")


(defvar jcl-mode--operators
  '("=" "&" "&&" "*")
  "JCL 'operators'.  A really minimal set.")

(defvar jcl-mode--names
  "^//\\([^*][[:graph:]]+\\)"
  "JCL names.

These are the 'names' of jobs and steps.")


(defvar jcl-mode--comments
  "^//\\*.*$"
  "JCL 'full card' comments.")


(defvar jcl-mode--card-end-comments-1
  "^//[^* ]+ +[[:graph:]]+ +[[:graph:]]+ +\\([[:graph:]].*\\)"
  "JCL 'end of card' comments for 'full' cards.

Anything after the 'operands' in a card is a comment; this regexp
selects them.")


(defvar jcl-mode--card-end-comments-1b
  (concatenate 'string
	       "// +"
	       (regexp-opt jcl-mode--operations 'words)
	       " +[[:graph:]]+"
	       " +\\([[:graph:]].*\\)")
  "JCL 'end of card' comments for 'unnamed' cards.

Anything after the 'operands' in a card is a comment; this regexp
selects them in case of cards that do not have a 'name'.")


(defvar jcl-mode--card-end-comments-2
  "// +[[:graph:]]+ +\\([[:graph:]].*\\)"
  "JCL 'end of card' comments for 'continuation' cards.

Anything after the 'operands' in a card is a comment; this regexp
selects them in case of 'continuation' cards that do not have the
'name' and 'operation'.")


(defvar jcl-mode--card-not-interpretable
  "// \{14\}.*"

  "JCL 'card with nothing before column 16'; i.e., not interpretable.")


;;; JCL faces.

(defcustom jcl-mode-string-face 'font-lock-string-face
  "The face used to fontify strings (single-quoted) in JCL mode."
  :group 'jcl
  :type 'symbol
  )

(defcustom jcl-mode-names-face 'font-lock-function-name-face
  "The face used to fontify 'names' in JCL mode."
  :group 'jcl
  :type 'symbol
  )

(defcustom jcl-mode-operations-face 'font-lock-keyword-face
  "The face used to fontify 'operations' in JCL mode."
  :group 'jcl
  :type 'symbol
  )

(defcustom jcl-mode-operands-face 'font-lock-type-face
  "The face used to fontify 'operands' in JCL mode."
  :group 'jcl
  :type 'symbol
  )

(defcustom jcl-mode-operators-face 'font-lock-builtin-face
  "The face used to fontify 'operators' in JCL mode."
  :group 'jcl
  :type 'symbol
  )

(defcustom jcl-mode-comment-face 'font-lock-comment-face
  "The face used to fontify 'comments' in JCL mode."
  :group 'jcl
  :type 'symbol
  )


(defface jcl-mode-invalid-card
  '((t :underline (:color "red" :style wave)))
  "Face to colorize 'invalid' cards; mostly those with too much leading spaces."
  :group 'hlasm
  )

(defvar jcl-mode-invalid-card 'jcl-mode-invalid-card) ; Why we need this?  Who knows?


(defvar jcl-mode--operations-re
  (concatenate 'string
	       " "
	       (regexp-opt jcl-mode--operations 'words)
	       " "))

	       
(defvar jcl-mode--font-lock-keywords
  `(
    (,jcl-mode--strings . ,jcl-mode-string-face)

    (,jcl-mode--names . (1 ,jcl-mode-names-face))

    (,jcl-mode--operations-re . ,jcl-mode-operations-face)

    (,(regexp-opt jcl-mode--operands 'words) . ,jcl-mode-operands-face)

    (,(regexp-opt jcl-mode--operators nil) . ,jcl-mode-operators-face)

    ;; These must be last.
    (,jcl-mode--card-end-comments-1 . (1 ,jcl-mode-comment-face t))

    (,jcl-mode--card-end-comments-1b . (2 ,jcl-mode-comment-face t))
    
    (,jcl-mode--card-end-comments-2 . (1 ,jcl-mode-comment-face))
    (,jcl-mode--comments . (0 ,jcl-mode-comment-face t))
    )
  "The JCL mode 'font-lock' 'keyword' specification."
  )


(defvar jcl-mode--font-lock-defaults
  (list 'jcl-mode--font-lock-keywords
	nil ; Do syntax based processing.
	)
  "The JCL mode 'font-lock' defaults specification."
  )


;;; jcl-mode-syntax-table

(defvar jcl-mode-syntax-table
  (let ((jclst (make-syntax-table)))
    ;; (modify-syntax-entry ?/ ". 1" jclst)
    ;; (modify-syntax-entry ?* ". 2" jclst)
    ;; (modify-syntax-entry ?\n "> " jclst)
    jclst
    )
  "The JCL mode syntax table."
  )


;;; jcl-keymap

(defvar jcl-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km prog-mode-map) ; Inherit from prog-mode-map!
    km)
  "The JCL mode key map.")


;;; jcl-imenu-generic-expression

(defvar jcl-mode-imenu-generic-expression
  '(("Job" "//\\([^* ]*\\) +JOB" 1)		; The JOB is always first.
    ("Steps" "//\\([^* ]+\\) +EXEC" 1)
    ("SYSIN" "//\\([^* ]*SYSIN\\) +DD" 1)
    ("DD" "//\\([^* ]+\\) +DD" 1)
    ("Procedures" "//\\([^* ]+\\) +PROC" 1)
    )
  "The JCL Imenu regular expressions.")


;;; jcl-mode

(define-derived-mode jcl-mode prog-mode "JCL"
  "JCL mode is a major mode to edit IBM MVS or z/OS Job Control Language."

  :syntax-table jcl-mode-syntax-table

  (setq-local font-lock-defaults jcl-mode--font-lock-defaults)

  (face-remap-add-relative jcl-mode-comment-face  :weight 'bold)
  (face-remap-add-relative jcl-mode-operators-face  :weight 'bold
			   :foreground "Forest Green") ; This may be too much.
  (face-remap-add-relative jcl-mode-operations-face  :weight 'bold)

  ;; Comments.
  ;; (setq-local comment-start "//\\*")
  ;; (setq-local comment-end "")
  ;; (setq-local comment-start-skip
  ;;             "^//[[:graph:]]*[[:blank:]]+[[:graph:]]+[[:blank:]]+")

  ;; Set up the mode keymap.

  (use-local-map jcl-mode-map)

  ;; Set up the menus.

  (easy-menu-define jcl-mode-mainframe-os-menu jcl-mode-map
    "JCL commands"
    '("JCL OS"
      ["Submit Job" jcl-mode-submit
       :help "Submit the job contained in the current buffer." ]
      ["Submit a JCL File" jcl-mode-submit-file
       :help "Submit a JCL file to a card reader; a port will be asked for."])
    )

  (setq-local imenu-generic-expression
	      (reverse jcl-mode-imenu-generic-expression))
  (imenu-add-to-menubar "JCL Code")

  ;; If defined, start the IRON MAIN minor mode, which sets up the
  ;; ruler and the "card" editing limits, plus the fill-column
  ;; indicator.

  (when (fboundp 'iron-main-mode) (iron-main-mode))

  'jcl-mode
  )


;;;; Commands
;;;; ========

(defvar *jcl-mode-default-os-reader-port* 3505
  "The default OS reader port.

This is the port number where JCL-MODE assumes the OS is listening for
reader requests.")


(defun jcl-mode-submit (&optional port)
  "Submits the buffer's content to the 'card reader' at PORT.

The buffer contains 'JCL cards' (i.e., lines) which are submitted to a
'card reader'  listening on PORT.  PORT is an integer; its default is
3505."
  
  (interactive
   (let ((p (read-number "JCL: card reader number/port: "
			 *jcl-mode-default-os-reader-port*))
	 )
     (list p)))
  
  (unless port
    (setq port *jcl-mode-default-os-reader-port*))
  
  (message "JCL: submitting to card reader number/port %s." port)

  (let ((card-reader-stream
	 (open-network-stream "JCL OS CARD READER"
			      nil
			      "127.0.0.1"
			      port
			      :type 'plain
			      ))
	)
    (unwind-protect
	(progn
	  (process-send-region card-reader-stream (point-min) (point-max))
	  (message "JCL: submitted."))
      (delete-process card-reader-stream))
    ))


(defalias 'submit 'jcl-mode-submit)


(defun jcl-mode-submit-file (jcl-file &optional port)
  "Submits the file JCL-FILE to the 'card reader' at PORT.

The file JCL-FILE contains 'JCL cards' (i.e., lines) which are
submitted to a 'card reader' listening on PORT.  PORT is an
integer; its default is 3505."
    
  (interactive
   (let ((f (read-file-name "JCL: card file: " nil nil 'confirm))
	 (p (read-number "JCL: card reader number/port: "
			 *jcl-mode-default-os-reader-port*))
	 )
     (list f p)))

  (unless port
    (setq port *jcl-mode-default-os-reader-port*))
    
  (message "JCL: submitting '%s' to card reader number/port %s."
	   jcl-file port)
  (let ((card-reader-stream
	 (open-network-stream "JCL OS CARD READER"
			      nil
			      "127.0.0.1"
			      port
			      :type 'plain
			      ))
	)
    (unwind-protect
	(with-temp-buffer
	  (insert-file-contents jcl-file)
	  (process-send-region card-reader-stream (point-min) (point-max))
	  (message "JCL: submitted.")
	  )
      (delete-process card-reader-stream))
    )
)


;;;; Epilogue
;;;; ========

(add-to-list 'auto-mode-alist '("\\.jcl\\'" . jcl-mode))

(provide 'jcl-mode)

;;; jcl-mode.el ends here
