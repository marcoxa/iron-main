;;; pl1-mode --- A major mode to handle (IBM) PL/I code.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; pl1-mode.el
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: April 2nd, 2021
;;
;; Version: 20210404.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; A major mode to handle PL/I code.
;;
;; The code is very simple for the time being; mostly an excavation of
;; the IBM PLI/I Language Reference publication and a revisitation of
;; one of the two PL/I modes found online: the one by Stephen Merrony
;; at http://www.stephenmerrony.co.uk:8080 (which he kindly put in the
;; Public Domain).  The second PL/I mode floating around is the
;; Multics Emacs one, whch is about 40 years old.  It is interesting
;; per se, because it contains indentation code, however, it seems like
;; the Multics Emacs Lisp library seems "oldish", albeit a possible
;; subject of hermeneutical inquire.

;;; Code:

;;;; pl1 Mode Setup.

(defgroup pl1 nil
  "The major mode to manipulate PL/I code.

This mode is part of the IRON MAIN package."
  :group 'languages)


;;; The definitions of the syntax elements follow the "Language
;;; Reference" IBM document SC14-7285-00, First Edition (September
;;; 2010); in the following it is referred to as 'PL/I LR'.
;;, The reference 'Chapter X.' refer to PL/I LR.

;;; Things to highlight.

;; pl1-strings
;; The following is incomplete as the latest PL/I LR; strings bounded
;; by double quotes should be accepted.
;;
;; In any case, strings should be handled at this point by syntax
;; table entries (see below).

(defvar pl1-strings
  "'.*'"
  "PL/I strings regular expression.")


;; pl1-labels
;; Tricky one as now we have conditions to deal with.

(defvar pl1-labels
  ;; "^ *\\([[:alnum:]]* +\\)*\\([[:alnum:]]+\\):"
  ;; "^ *\\([A-Za-z_][A-Za-z0-9_]* +\\)*\\([A-Za-z_][A-Za-z0-9_]*\\):"
  ;; As above, but with optional spaces before colon.
  "^ *\\([A-Za-z_][A-Za-z0-9_]* +\\)*\\([A-Za-z_][A-Za-z0-9_]*\\) *:"
  )


;; pl1-statements
;; The list is incomplete.
;; The list is given as
;;
;;    FULL-NAME [ABBR-NAME]
;;
;; The list relies on Emacs default case insensitive matching.

(defvar pl1-statements
  '(
    ;; Chapter 6.
    "PROCEDURE" "PROC"
    "ENTRY"
    "CALL"
    "RETURN"
    "PACKAGE"
    "BEGIN"
    "END"

    ;; Chapter 7.
    "DEFINE"

    ;; Chapter 8.
    "DECLARE" "DCL"

    ;; Chapter 9.
    "ALLOCATE"
    "BY"
    "NAME"
    "CLOSE"
    "DELAY"
    "DELETE"
    "DETACH"
    "DISPLAY"
    "DO"
    "EXIT"
    "FETCH"
    "FLUSH"
    "FORMAT"
    "FREE"
    "GET"
    "GO TO" "GO" "TO" "GOTO"		; Special case.
    "IF"
    "THEN"
    "ELSE"
    "ITERATE"
    "LEAVE"
    "LOCATE"
    "ON"
    "OPEN"
    "OTHERWISE"
    "PUT"
    "READ"
    "RELEASE"
    "RESIGNAL"
    "REVERT"
    "REWRITE"
    "SELECT"
    "SIGNAL"
    "STOP"
    "WHEN"
    "WRITE"

    "WHILE"
    "UNTIL"
    "UPTHRU"
    "DOWNTHRU"
    "REPEAT"

    ;; Chapter 11.
    "OPEN"
    "CLOSE"
    "FLUSH"

    ;; Chapter 12.
    "READ"
    "WRITE"
    "REWRITE"
    "LOCATE"
    "DELETE"

    ;; Chapter 13.
    "GET"
    "PUT"

    ;; Chapter 16.
    "ON"
    "REVERT"
    "SIGNAL"
    "RESIGNAL"

    ;; Chapter 18.
    "ATTACH"
    "WAIT"
    "DETACH"
    
    
    )
  "PL/I statements.

The set of PL/I 'statements'."
  )



;; pl1-data-attributes
;; The list is incomplete.
;; The list is given as
;;
;;    FULL-NAME [ABBR-NAME]
;;
;; The list relies on Emacs default case insensitive matching.

(defvar pl1-data-attributes
  '(
    ;; Chapter 3.
    ;; Data attributes
    "AREA"
    "BINARY" "BIN"
    "BIT"
    "CHARACTER" "CHAR"
    "COMPLEX" "CPLX"
    "DECIMAL" "DEC"
    "DIMENSION" "DIM"
    "ENTRY"
    "FILE"
    "FIXED" "FIX"
    "FLOAT"
    "FORMAT"
    "GRAPHIC" "G"
    "HANDLE"
    "LABEL"
    "NONVARYING" "NONVAR"
    "OFFSET"
    "ORDINAL" "ORD"
    "PICTURE" "PIC"
    "POINTER" "PTR"
    "PRECISION" "PREC"
    "REAL"
    "RETURNS"
    "SIGNED"
    "STRUCTURE" "STRUCT"
    "TASK"
    "TYPE"
    "UNSIGNED"
    "UNION"
    "VARYING" "VAR"
    "VARYINGZ" "VARZ"
    "WIDECHAR"

    ;; Non data attributes.
    "ABNORMAL"
    "ALIGNED"
    "ASSIGNABLE"
    "AUTOMATIC" "AUTO"
    "BASED"
    "BIGENDIAN"
    "BUFFERED"
    "BUILTIN"
    "BYADDR"
    "BYVALUE"
    "CONDITION"
    "CONNECTED"
    "CONTROLLED"

    "DEFINED"
    "DIRECT"
    "ENVIRONMENT" "ENV"
    "EXCLUSIVE"
    "EXTERNAL"
    "GENERIC"
    "HEXADEC"
    "IEEE"
    "INITIAL" "INIT"
    "INONLY"
    "INOUT"
    "INPUT"

    ;; "INTERMAL" This is a mistake in IBM docs.
    "INTERNAL"
    "KEYED"
    "LIKE"
    "LIST"
    "LITTLEENDIAN"
    "NONASSIGNABLE"
    "NONCONNECTED"
    "NORMAL"
    "OPTIONAL"
    "OPTIONS"
    "OUTONLY"
    "OUTPUT"

    "PARAMETER" "PARAM"
    "POSITION" "POS"
    "PRINT"
    "RECORD"
    "SEQUENTIAL" "SEQ"
    "STATIC"
    "STREAM"
    "UNALIGNED"
    "UNBUFFERED"
    "UPDATE"
    "VALUE" "VAL"
    "VARIABLE"

    ;; Chapter 6.
    "EXPORTS"
    "RESERVES"
    "RECURSIVE"
    "LIMITED"

    ;; Chapter 7.

    ;; Chapter 8.
    "DEFAULT"
    "DIMACROSS"
    "NOINIT"

    ;; Chapter 9.
    "FORMAT"

    ;; Chapter 10.

    ;; Chapter 11.
    

    ;; Chapter 12.

    ;; Chapter 13.
    "FILE"
    "LINE"
    "PAGE"
    "SKIP"
    "STRING"
    "EDIT"
    "DATA"

    ;; Chapter 16.

    ;; Chapter 18.
    )
  "PL/I data attributes.

The set of names that can be used to qualify a PL/I piece of data.")


(defvar pl1-operators
  '(
    "->" "=>"
    "+" "-" "*" "/" "**"
    "&" "|" ; "^" ; not operator is not ASCII
    "<" "<=" "=" ">=" ">"
    ;; Missing "^<" "^=" "^>" ; the '^' being the EBCDIC neg sign.
    "||" ; Concatenation.
    )
  "PL/I 'operators'.  A really minimal set.")


(defvar pl1-directives                  ; Not really that nice, but it
                                        ; is a first approximation.
  '("%"
    "%ACTIVATE" "% ACTIVATE"
    "%DEACTIVATE" "% DEACTIVATE"
    "%DECLARE" "% DECLARE"
    "%DO" "% DO"
    "%END" "% END"
    "%GO TO" "% GO TO"
    "%IF" "% IF"
    "%THEN" "% THEN"
    "%ELSE" "% ELSE"
    "%INCLUDE" "% INCLUDE"
    "%INSCAN" "% INSCAN"
    "%ITERATE" "% ITERATE"
    "%LEAVE" "% LEAVE"
    "%NOTE" "% NOTE"
    "%PROCEDURE" "% PROCEDURE"
    "%PROCESS" "% PROCESS" "*PROCESS"   ; Not really preprocessor.
    "%REPLACE" "% REPLACE"
    "%SELECT" "% SELECT"
    "%XINCLUDE" "% XINCLUDE"
    "%XINSCAN" "% XINSCAN"
    )
  "PL/I preprocessor 'directives' and macro statements.  A minimal set.")


;;; PL/I faces.

(defcustom pl1-string-face 'font-lock-string-face
  "The face used to fontify strings (single-quoted) in PL/I mode."
  :group 'pl1
  :type 'symbol
  )

(defcustom pl1-labels-face 'font-lock-function-name-face
  "The face used to fontify 'labels' (i.e., 'names') in PL/I mode."
  :group 'pl1
  :type 'symbol
  )

(defcustom pl1-attributes-face 'font-lock-constant-face
  "The face used to fontify 'attributes' in PL/I mode."
  :group 'pl1
  :type 'symbol
  )


(defcustom pl1-operators-face 'font-lock-builtin-face
  "The face used to fontify 'operators' in PL/I mode."
  :group 'pl1
  :type 'symbol
  )


(defcustom pl1-statements-face 'font-lock-keyword-face ; 'font-lock-builtin-face
  "The face used to fontify 'statements' in PL/I mode.

Statements are actually the 'commands' or 'verbs' defined by PL/I."
  :group 'pl1
  :type 'symbol
  )


(defcustom pl1-directives-face 'font-lock-preprocessor-face
  "The face used to fontify 'directives' in PL/I mode.

Directives are actually akin to the 'commands' or 'verbs' defined by PL/I."
  :group 'pl1
  :type 'symbol
  )

(defcustom pl1-comment-face 'font-lock-comment-face
  "The face used to fontify 'comments' in PL/I mode."
  :group 'pl1
  :type 'symbol
  )


(defvar pl1-font-lock-keywords
  `(
    (,pl1-strings . ,pl1-string-face)

    (,pl1-labels . (2 ,pl1-labels-face))

    (,(regexp-opt pl1-statements 'symbols) . ,pl1-statements-face)

    (,(regexp-opt pl1-operators nil) . ,pl1-operators-face)

    (,(regexp-opt pl1-data-attributes 'symbols) . ,pl1-attributes-face)

    (,(regexp-opt pl1-directives 'symbols) . ,pl1-directives-face)

    ;; These must be last.
    ;; (,pl1-card-end-comments-1 . (1 ,pl1-comment-face))
    ;; (,pl1-card-end-comments-2 . (1 ,pl1-comment-face))
    ;; (,pl1-comments . (0 ,pl1-comment-face t))
    )
  "The PL/I mode 'font-lock' 'keyword' specification."
  )


(defvar pl1-font-lock-defaults
  (list 'pl1-font-lock-keywords
	nil ; Do syntax based processing.
	t   ; Be case insensitive.
	)
  "The PL/I mode 'font-lock' defaults specification."
  )


;;; pl1-mode-syntax-table

(defvar pl1-mode-syntax-table
  (let ((pl1-st (make-syntax-table)))
    (modify-syntax-entry ?' "\"" pl1-st)
    (modify-syntax-entry ?/ ". 14" pl1-st)
    (modify-syntax-entry ?* ". 23" pl1-st)
    ;; (modify-syntax-entry ?\n "> " pl1-st)
    pl1-st
    )
  "The PL/I mode syntax table."
  )


;;; pl1-keymap

(defvar pl1-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km prog-mode-map) ; Inherit from prog-mode-map!
    km)
  "The PL/I mode key map.")


;;; pl1-imenu-generic-expression
;;;
;;; 20210404: MA: ok, I lost an afternoon figuring out why the
;;; following was not working.  Bottom line, you MUST match from the
;;; beginning of line.

(defvar pl1-imenu-generic-expression
  '(("Packages"
     "^ *\\([A-Za-z_][A-Za-z0-9_]* +\\)*\\([A-Za-z_][A-Za-z0-9_]*\\): +PACKAGE" 2)
    ;; The above may be incorrect, as PLI/I LR seems to require a
    ;; colon after the condition.
    
    ("Procedures"
     "^ *\\([A-Za-z_][A-Za-z0-9_]* +\\)*\\([A-Za-z_][A-Za-z0-9_]*\\): +PROC" 2)
    ("Entries"
     "^ *\\([A-Za-z_][A-Za-z0-9_]* +\\)*\\([A-Za-z_][A-Za-z0-9_]*\\): +ENTRY" 2)
    ("Formats"
     "^ *\\([A-Za-z_][A-Za-z0-9_]* +\\)*\\([A-Za-z_][A-Za-z0-9_]*\\): +FORMAT" 2)
    ("Blocks"
     "^ *\\([A-Za-z_][A-Za-z0-9_]* +\\)*\\([A-Za-z_][A-Za-z0-9_]*\\): +BEGIN" 2)
    )
  "The PL/I Imenu regular expressions.")


;;; pl1-mode

(define-derived-mode pl1-mode prog-mode "PL/I"
  "PL/I mode is a major mode to edit PL/I code."

  :syntax-table pl1-mode-syntax-table

  (setq-local font-lock-defaults pl1-font-lock-defaults)

  ;; (face-remap-add-relative pl1-comment-face :weight 'bold)
  (face-remap-add-relative pl1-operators-face :weight 'bold
			   :foreground "Forest Green") ; This may be too much.
  (face-remap-add-relative pl1-labels-face :weight 'bold)

  ;; Comments.
  (setq-local comment-start "/\*")
  (setq-local comment-start-skip "/\*+[ \t]*")
  (setq-local comment-end "\*/")
  (setq-local comment-end-skip "[ \t]*\*+/")

  
  ;; Set up the mode keymap.

  (use-local-map pl1-mode-map)

  ;; Set up the menus.

  ;; (easy-menu-define pl1-mainframe-os-menu pl1-mode-map
  ;;   "PL/I commands"
  ;;   '("PL/I OS"
  ;;     ["Submit" pl1-submit]
  ;;     ["Submit PL/I File" pl1-submit-file])
  ;;   )

  (setq-local imenu-generic-expression
	      (reverse pl1-imenu-generic-expression))
  ;; (debug "About to enter ...")
  (imenu-add-to-menubar "PL/I Code")

  ;; If defined, start the IRON MAIN minor mode, which sets up the
  ;; ruler and the "card" editing limits, plus the fill-column
  ;; indicator.

  (when (fboundp 'iron-main-mode) (iron-main-mode))

  'pl1-mode
  )


;;;; Commands
;;;; ========



;;;; Epilogue
;;;; ========

(add-to-list 'auto-mode-alist '("\\.pl1\\'" . pl1-mode))
(add-to-list 'auto-mode-alist '("\\.pli\\'" . pl1-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . pl1-mode))
(add-to-list 'auto-mode-alist '("\\.cpy\\'" . pl1-mode))

(provide 'pl1-mode)

;;; pl1-mode.el ends here
