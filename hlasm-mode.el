;;; hlasm-mode --- A major mode to handle Assembler/IBM for MVS or Z/OS JCL.
;;; -*- Mode: Emacs-Lisp -*-

;;; hlasm-mode.el
;;
;; See the file COPYING for license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: October 21, 2022.
;;
;; Version: 2022-11-26.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; A major mode to handle IBM Assemblers (including HLASM), mostly for
;; MVS or z/OS.


;;; Code:

;;;; HLASM IBM Mode Setup.

(defgroup hlasm nil
  "The major mode to handle Assemblers for IBM 360/370 and z Series machines.

This mode is part of the IRON MAIN package."
  :group 'languages)


;; hlasm-mode-os-flavor
;;
;; Notes:
;; Maybe add VM, MTS, MUSIC, etc...

(defcustom hlasm-mode-os-flavor "MVS 3.8j"
  "The current flavor of MVS used.

The values of this variable are strings starting either with 'MVS' or
'z/OS'.  Other variants are acceptable as long as the 'main' OS name
comes first.

The value 'MVS 3.8j' is the default one, being the version of MVS
that IBM release in the public domain."
  :group 'hlasm
  :type 'string)


;; Things to highlight.
;; ====================

;; We inherit from standard asm-mode, so we just need things for
;; comments at the end of a line.

(defvar hlasm-mode--non-blank-card
  ;; "^[[:blank:]]*[[:alnum:]@$#&._,()]+"
  "^[[:blank:]]*\\S-"
  "Matching a non blank card.")


(defvar hlasm-mode--continued-card
  "^.\\{71\\}\\S-"
  "Matching a card which continues on the next one.")


(defvar hlasm-mode--strings
  "'.*'"
  "HLASM IBM strings.")


(defvar hlasm-mode--names
  "^\\(\\(?:\\.?[[:alpha:]]\\|[&[:alpha:]]\\)[#$&.@_[:alnum:]]*\\)"
  ;; "^\\([[:alpha:]&][[:alnum:]@$#_&.]*\\|\\.[:alpha:][[:alnum:]@$#_&.]*\\)"
  "HLASM IBM names (or labels).

These are the 'names' of instructions.")


;; (defvar hlasm-names
;;   "^\\([^* ][[:graph:]]+\\)"
;;   "HLASM IBM names.
;;
;; These are the 'names'of instructions.")


(defvar hlasm-mode--instructions
  (concat hlasm-mode--names
	  "?[[:blank:]]+\\([[:alpha:]@$#&._][[:alnum:]@$#&._]*\\)")
  "HLASM IBM instructions.

These are the instructions mnemonics.
The second regexp group contains the istruction name.")


;; (defvar hlasm-mode--instructions
;;   "^\\([[:alpha:]&.][[:alnum:]@$#_&.]*\\)?[[:blank:]]+\\([[:alpha:]@$#&._][[:alnum:]@$#&._]*\\)"
;;   "HLASM IBM instructions.

;; These are the instructions mnemonics.
;; The second regexp group contains the istruction name.")

;; (defvar hlasm-instructions
;;   "^\\([^* ][[:graph:]]+\\| +\\) +\\([[:graph:]]+\\)"
;;   "HLASM IBM instructions.

;; These are the instructions mnemonics.")


(defvar hlasm-mode--16-white-columns
  "^ \\{16\\}\\S-"

  "HLASM IBM continuation line prefix.

Continuation lines must start at column 16.  This regexp matches 16
spaces (columns) at the beginning of line.  Note that only spaces are
matched.")


(defvar hlasm-mode--registers
  "R[0-9][0-5]?"
  "HLASM IBM register names.")


(defvar hlasm-mode--card-end-comments-0
  "^\\.?\\*.*$"
  "HLASM IBM 'comment' card.

A '*' or a '.*' (for macros) at the beginning of the card (line) marks
a comment.")


(defvar hlasm-mode--card-end-comments-1
  "^\\([[:alpha:]&.][[:alnum:]@$#_&.]*\\)?[[:blank:]]+[[:alpha:]@$#&._][[:alnum:]@$#&._]*[[:blank:]]+[-[:alnum:],*='()+]?\\([[:graph:]]*\\)$
     "
  "HLASM IBM 'end of card' comments for 'full' cards.

Anything after the 'operands' in a card is a comment; this regexp
selects them.")


;; (defvar hlasm-card-end-comments-1
;;   "^[^* ]+ +[[:graph:]]+ +[[:graph:]]+ +\\([[:graph:]].*\\)"
;;   "HLASM IBM 'end of card' comments for 'full' cards..

;; Anything after the 'operands' in a card is a comment; this regexp
;; selects them.")


(defvar hlasm-mode-card-end-comments-2
  "^ +[[:graph:]]+ +\\([[:graph:]].*\\)"
  "HLASM IBM 'end of card' comments for 'continuation' cards.

Anything after the 'operands' in a card is a comment; this regexp
selects them in case of 'continuation' cards that do not have the
'name' and 'operation'.")


(defvar hlasm-mode--card-end-comments-3
  "^ +[[:graph:]]+ +[[:graph:]]+ +\\([[:graph:]].*\\)"
  "HLASM IBM 'end of card' comments for 'no operands' cards.

Anything after the 'operands' in a card is a comment; this regexp
selects them in case of 'no operands' cards that do not have the
'name' and 'operands'; however, the 'operands' must be at least one
non blank character.")


;; hlasm-attribute-symbol
;; Note that the following does only attributed symbols and not
;; symbols and expressions as per IBM specs.

(defvar hlasm-mode--attributed-symbol
  "\\([LKDINOST]'[[:alnum:]]+\\|L'\\*\\)"

  "HLASM IBM 'attributed terms.

This is typically the 'length' of something, e.g.,

    L'FOOBAR

indicating the length of FOOBAR.
Other 'attributes' are introduced by the other letters.

See, e.g.: https://www.ibm.com/docs/en/zos/2.1.0?topic=terms-other-attribute-references
")


;; Useless because of polymode.
;;
;; (defvar hlasm-mode--jcl
;;   "^//.*$"
;;   "Lines starting with '//' are assumed to be JCL which wraps the Assembler.")


;; Matching functions.
;; ===================

;; Macro utilities for debugging.
;; ------------------------------
;;
;; I know.  This is very kludgy.

(defvar *hlasm-mode--debug* t)


;; hlasm-mode-messaging
;;
;; Macro NOT to be used outside a function.
;; Creates a local 'msg' function that uses a given 'tag' and relies
;; on the value of '*hlasm-mode--debug*' (which can be captured). The
;; local funcion 'msg' returns NIL for convenience, instead of the
;; string that `message' would return.

(cl-defmacro hlasm-mode--messaging (tag &body code)
  `(cl-flet ((msg
              (m &rest args)
              (when *hlasm-mode--debug*
                (apply 'message (concat ,tag m) args))
              nil)
             )
     (message "\n")
     (msg "entering at line %s, column %s, point %s with limit %s"
          (line-number-at-pos)
          (current-column)
          (point)
          limit)
     
     ,@code))


;; Functions.
;; ----------

;; hlasm-card-remarks
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
;; this seems inconstistent, although there could always be a match for
;; "^.*$".
;; This is the reason why we terminate a search with
;;
;;     (re-search-forward ".*$" nil nil)
;;
;; at the end of line.

(cl-defun hlasm-mode--card-remarks-saved (&optional (limit (line-end-position)))
  ;; When called programmatically `limit' is end of buffer.
  ;; (interactive)

  (message "\nHLASM CARD REMARK: entered with limit = %s @%s" limit (point))

  (condition-case nil
      (let* ((current-point (point))
	     (post-instr-point
	      (re-search-forward hlasm-mode--instructions nil nil))
	     
	     ;; Now point should be at the end of the instruction.
	     ;; I can start skipping over the operands characters,
	     ;; including strings and attributed symbols.
	     
	     (skipped-ws (skip-chars-forward "[:blank:]" limit))
	     (first-operand-point (point))
	     (operand-pos first-operand-point)
	     (first-remark-point nil)
	     )
        (message "HLASM CARD REMARK: asserting")
	(cl-assert (not (char-equal (following-char) ?\ ))) ; Being paranoid.
	(cl-assert (not (bolp)))        ; Yep, very paranoid.
	(message "HLASM CARD REMARK: asserted")

	;; Now I am at the first character of the 'operands' of the
	;; instruction.  Note that, as per IBM documentation, to
	;; have "remarks" that are comments for instructions that
	;; use zero arguments, there should be a lone comma on the
	;; line, surrounded my spaces, signifying "I am the operand
	;; list".

	(cl-labels ((finish-remark-parsing
		     ()
		     (message "HLASM CARD REMARK: finishing")
		     
		     ;; We get here in one case.
		     ;; We have a blank character at hand and we need
		     ;; to see whether there is 'end of line/card'
		     ;; comment.
		     ;; If so we need to fix the match and return t,
		     ;; otherwise we return nil.
		     (skip-chars-forward "[:blank:]" limit)

		     (message "HLASM CARD REMARK: char after blanks `%c'" (char-after))

		     (if (< (point) (line-end-position))
			 ;; Something non blank found before end of line.
			 ;; Fix match
			 (cl-return-from hlasm-mode--card-remarks
			   (let ((result (re-search-forward ".*$" limit t)))
			     (message "HLASM CARD REMARK: finishing match data \"%s\" %s"
			      	      (match-string 0)
			     	      (match-data))
			     result))
		       (cl-return-from hlasm-mode--card-remarks
			 (progn
			   (message "HLASM CARD REMARK: finishing with no match")
			   ;; nil
			   (finish-eol-success)
			   )
			 ))
		     )

		    (finish-eol-success
		     ()
		     (progn
		       (message "HLASM CARD REMARK: finishing at eol %s %s"
				(point)
				(line-end-position))
		       (cl-return-from hlasm-mode--card-remarks
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

		     (message "HLASM CARD REMARK: in-string")
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
		       ;; 	   (message "HLASM CARD REMARK: in-string after `%c'" (char-after))
		       ;; 	 (message "HLASM CARD REMARK: in-string ooooops."))

		       (if string-end-pos
			   (setq operand-pos string-end-pos) ; We are cool.
			 (setq operand-pos (point)) ; We pray.
			 ))
		     )
		    )			; labels functions.
	  
	  (message "HLASM CARD REMARK: in loop with operand-pos = %s, eol = %s"
	   	   operand-pos
	   	   (line-end-position))
	  
	  (while (and operand-pos
		      (< operand-pos limit)
		      (< operand-pos (line-end-position)))
	    
	    (let ((c (following-char)))
	      (message "HLASM CARD REMARK: in loop with c = `%c'" c)
	      
	      (cond ((equal (char-syntax c) ?\ ) ; Found a space; skip to first non blank.
		     (skip-chars-forward "[:blank:]" limit)
		     (finish-remark-parsing)
		     )

		    ((char-equal c ?\') ; Tricky part: string or attribute?
		     (let ((prev-char (preceding-char)))

		       (message "HLASM CARD REMARK: attribute with prev c = `%c'"
		                prev-char)
		       
		       ;; Is this an attributed expression
		       (if (cl-find prev-char "LKDINOST") ; Possible attribute.
			   (cl-case (char-before (1- (point)))
			     ((?+ ?- ?/ ?* ?\( ?\) ?, ?\t ?\ )
			      ;; Possible 'operators'. Last is space!
			      ;; We have (almost surely) an attributed
			      ;; expression.
			      ;; Keep going without bothering to deal
			      ;; with strings.

			      ;; (message "HLASM CARD REMARK: attributed")
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

	  (message "HLASM CARD REMARK: no match")
	  (if (>= operand-pos limit)
	      nil ; We are out of the loop past the limit, we finish with NIL.
	    (finish-eol-success)
	    )
	  )				; cl-labels...
	)				; let*...
    ;; condition-case catchers...
    (search-failed (message "HLASM CARD REMARK: search failed.") nil)
    ))


;; hlasm-mode--parse-operands
;;
;; Trying to parse the lovely "expression".
;;
;; Notes:
;;
;; The parser is stupid and does not span lines.  That is, if you open
;; a parenthesis you must close it on the same line.

(cl-defun hlasm-mode--parse-operands (&optional
                                       (limit (line-end-position))
                                       (*hlasm-mode--debug* t)
                                       )
  ;; This function must be called with (point) at the start of an operand.
  ;; At the end of the call, the point is at the first space after the
  ;; operand (which must be a full expression; cfr., Assembler reference).
  ;;
  ;; When called programmatically `limit' may be end of buffer.
  ;;
  ;; Notes:
  ;;
  ;; TODO: Come back to munge the 'match'.
  
  (interactive)

  (cl-flet ((msg
             (m &rest args)
             (when *hlasm-mode--debug*
               (apply 'message
                      (concat "HLASM PARSE OPERANDS: " m)
                      args)))
            )
    (message "\n")
    (msg "entered with limit = %s @%s" limit (point))

    (condition-case nil
        (let ((operand-pos (point)))
	
          (msg "asserting")
          (cl-assert (not (char-equal (following-char) ?\ ))) ; Being paranoid.
          (cl-assert (not (bolp)))      ; Yep, very paranoid.
          (msg "asserted")

          ;; Now I am at the first character of the 'operands' of the
          ;; instruction.

          ;; The CL-LABELS here are a hand-crafted recursive descent
          ;; parser.
          ;;
          ;; There are three different functions (plus a few utility
          ;; ones). (a) functions for "terminals" and functions for
          ;; "more terminals", as the parser assumes a somewhat
          ;; right-recursive structure.
          ;; Each "terminal" function takes as argument the "starting
          ;; character" of the thing being parsed and finishes with
          ;; (point) right after the parsing.  Functions implementing
          ;; "more terminal" may check the first thing they see and do
          ;; something appropriate; they also finish at the end of the
          ;; last proper character.

          ;; Do not expect anything fancy, the parser is buit to skip
          ;; over the operands of an instruction.  That's it.

          ;; Note that, as per IBM documentation, to have "remarks" that
          ;; are comments for instructions that use zero arguments,
          ;; there should be a lone comma on the line, surrounded my
          ;; spaces, signifying "I am the operand list".

          (cl-labels
              ((advance
                ()
                (forward-char)
                (setq operand-pos (point))
                )                       ; advance

               (retreat
                ()
                (backward-char)
                (setq operand-pos (point))
                )                       ; retreat

               (is-space
                (c)
                (equal (char-syntax c) ?\ )
                )                       ; is-space

               (is-newline
                (c)
                (equal (char-syntax c) ?\n)
                )                       ; is-newline

               (is-comma
                (c)
                (char-equal c ?\,)
                )                       ; is-comma

	       (is-term-character
		(c)
                (or (equal (char-syntax c) ?\W) ; Word constituent
                    (equal (char-syntax c) ?\_) ; Symbol constituent;
                                        ; check later that it contains
                                        ; all characters needed.
                    (char-equal c ?\.)
                    (char-equal c ?\#))
		)			; is-term-character


               (finish-operands
                (c)
                ;; (message "XXXXXXXXXXXXXXXXXXXXXXXXXXXX")
                (msg "finishing operands.")
		     
                ;; We get here in one case.
                ;; We have a blank character terminating the operands
                ;; field or we are at the end of a line.

                (cl-assert (or (eolp)
                               (equal (char-syntax c) ?\ )))

                (msg "returning; point is @%d." (point))

                (cl-return-from hlasm-mode--parse-operands (point))
                )                       ; finish-operands

               (finish-operand
                (c)
                (msg "finishing one operand; c = ?%c" c)
                (cl-assert (char-equal c ?\,) t)
                ;; (advance)
                (msg "finished one operand.")
                t
                )                       ; finish-operand
		    
               (parse-string
                (c)
                ;; We are just before a "'"

                (msg "parse-string")
                ;; Just to be paranoid...
                (cl-assert (char-equal ?' c) t "in-string cl-label")
			       
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

                  (if string-end-pos
                      (setq operand-pos string-end-pos) ; We are cool.

                    (setq operand-pos (point)) ; We pray.
                    ))
                )                       ; parse-string
		    
               (parse-operands
                (c)
                (msg "parse-operands; c = ?%c" c)
                (parse-operand c)
                (msg "parse-operands; called parse-operand fc = ?%c"
                     (following-char))
                (parse-more-operands (following-char))
                )                       ; parse-operands

               (parse-operand
                (c)
                (msg "parse-operand; c = ?%c" c)
                (cond ((is-comma c)
                       (msg "parse-operand; calling finish-operand pc = ?%c fc = ?%c"
                            (char-before)
                            (following-char))
                       (msg "parse-operand; if I get here, there may be a problem.")
                       ;; Should not get here now that there is parse-more-operands.
                       ;; (retreat)
                       (finish-operand (following-char)) ; Essentially a no-op.
                       )
                      ((or (is-space c) (eolp))
                       (msg "parse-operand; if I get here, there's a problem.")
                       (finish-operands c)
                       )
                      (t
                       (parse-expressions c)
                       )
                      )
                )                       ; parse-operands
	     
               (parse-more-operands
                (c)
                (msg "parse-more-operands; c = ?%c" c)
                (cond ((or (eolp) (is-space c))
                       ;; Found a space or a newline, end of operand(s).
                       (finish-operands c)
                       )
                      ((is-comma c)
                       ;; Proceed to parse next operand, unless the
                       ;; next character is a blank or we are at the
                       ;; end of the line, as in the case before.
                       (advance)
                       (if (or (eolp) (is-space (following-char)))
                           ;; (finish-operands c)
                           (finish-operands (following-char))
                         (parse-operands (following-char)))
                       )
                      )
                )                       ; parse-operand

               (parse-expressions
                (c)
                (msg "parse-expressions; c = ?%c" c)
                (parse-expression c)
                (msg "parse-expressions; called parse-expressions fc = ?%c"
                     (following-char))
                (parse-more-expressions (following-char))
                )                       ; parse-operands

               (parse-expression
                (c)
                (msg "parse-expression; c = ?%c" c)
                (parse-term c)
                (parse-more-expressions (following-char))
                )                       ; parse-expression
		    
               (parse-more-expressions
                (c)
                (msg "parse-more-expressions; c = ?%c" c)
                (cond ((char-equal c ?\))
                       ;; Closing of expression
                       ;; Should not get here.
                       (msg "parse-more-expressions; trouble with c = ?%c" c)
                       ;; BUt, if we get here, we are really in a
                       ;; parenthesized expression.  Leave the closing
                       ;; parenthesis to be handled by
                       ;; parse-parenthesized-expression.

                       ;; (advance)
                       t)
                    
                      ((char-equal c ?\()
                       (parse-parenthesized-expression c))
                    
                      ((cl-find c "=+-*/")
                       (advance)
                       (parse-expressions (following-char)))
                    
                      ((char-equal c ?\')
                       (parse-string c)
                       (parse-more-expressions (following-char)))
                    
                      ((is-comma c)
                       ;; (parse-expression)
                       (msg "parse-more-expressions; am I here; c = ?%c" c)
                       ;; (retreat)
                       t)

		      ((is-term-character c)
                       (msg "parse-more-expressions; parse-expressions; c = ?%c" c)
		       (parse-expressions c))
                    
                      ((is-space c)
                       ;; (retreat)
                       t)
                    
                      ((equal (char-syntax c) ?\n)
                       t)
                      )
                )                       ; parse-more-expression
	     
               (parse-term
                (c)
                (msg "parse-term; c = ?%c" c)
                (cond ((char-equal c ?\()
                       ;; Beginning of expression.
                       (parse-parenthesized-expression c))
                    
                      ((and (cl-find c "DONSKILT")
                            (char-equal (char-after (1+ (point))) ?\'))
                       ;; We have an attribute.
                       (msg "attribute with c = ?%c" c)
                       (advance)
                       (advance) ; Now we are on the first character after the ?'.
                       (msg "attribute first c = ?%c" (following-char))
                       (parse-term (following-char)))
                    
                      ((and (cl-find c "XBCG")
                            (char-equal (char-after (1+ (point))) ?\'))
                       ;; We have a "self-defining term".
                       (msg "self-def with c = ?%c" c)
                       (advance)
                       ;; Now we parse a string.
                       (parse-string (following-char)))
                    
                      ((char-equal c ?\*) ; Special case for location
                                        ; counter.
                       (advance)
                       t)
                    
                      ((char-equal c ?\=)
                       ;; A literal
                       (msg "literal with c = ?%c" c)
                       (advance)
                       (parse-literal-or-symbol (following-char))
                       (parse-expression (following-char)))
                     
                      ((char-equal c ?\')
                       (parse-string c))
                    
                      (t
                       (msg "calling parse-literal-or-symbol")
                       (parse-literal-or-symbol c))
                      )
                )                       ; parse-term

               (parse-literal-or-symbol
                (c)
                (msg "parse-literal-or-symbol; c = ?%c" c)
                (cond ((is-literal-or-symbol-following-char c)
                       (msg "parse-literal-or-symbol; terminating ?%c"
                            (following-char))
                       t)
                      (t
                       (advance)
                       (parse-literal-or-symbol (following-char))
                       )
                      )
                )                       ; parse-literal-or-symbol

               (parse-parenthesized-expression
                (c)
                (msg "parse-parenthesized-expression c = ?%c" c)
                (cl-assert (char-equal c ?\())

                (advance)
                (parse-expression (following-char))
                (prog1 (parse-more-parenthesized-expression (following-char))

                  (cl-assert (char-equal (char-before) ?\))
                             ;; t
                             ;; "char before is %c." (following-char)
                             )
                  )
                )

               (parse-more-parenthesized-expression
                (c)
                (msg "parse-more-parenthesized-expression c = ?%c" c)
                (cond ((char-equal c ?\))
                       ;; Closing of epression
                       (msg "parse-more-parenthesized-expression; closing c = ?%c"
                            c)
                       (advance)
                       t)
                    
                      ((char-equal c ?\()
                       (parse-parenthesized-expression c))
                    
                      ((cl-find c "=+-*/")
                       (advance)
                       (parse-expression c))
                    
                      ((char-equal c ?\')
                       (parse-string c)
                       (parse-more-parenthesized-expression (following-char)))
                    
                      ((is-comma c)
                       ;; Contrary to parse-more-expression here we are
                       ;; in an "argument list", we must advance.
                       ;; parse-more-expression would return and let the
                       ;; operands parsing take over.
                       (advance)
                       (parse-expression (following-char))                     
                       (parse-more-parenthesized-expression (following-char))
                       t)
                    
                      ((is-space c)
                       ;; Here we differ as we can have the 'A..'
                       ;; macros.
                     
                       (skip-syntax-forward " " limit)
                       (parse-literal-or-symbol (following-char))
                       ;; Really a AND, OR, XOR...

                       (parse-more-parenthesized-expression (following-char))
                       )
                    
                      ((equal (char-syntax c) ?\n)
                       t)

                      (t                ; Other char
                       (msg "parse-more-parenthesized-expression; ooops!")
                       ;; (advance)
                       (parse-expression (following-char))
                       )
                      )
                )                ; parse-more-parenthesized-expression
		    
               (is-literal-or-symbol-following-char
                (c)
                (cond ((char-equal c ?\))
                       ;; Closing parenthesized expression.
                       t)
                      ((char-equal c ?\()
                       ;; Opening of parenthesized expression,
                       ;; possibly an argument list.
                       t)
                      ((cl-find c "+-*/")
                       ;; Operator in expression.
                       t)
                      ((char-equal c ?\')
                       ;; Beginning of string?
                       t
                       )
                      ((is-comma c)
                       ;; End of expression/operand?
                       t)
                      ((is-space c)
                       t)
                      ((equal (char-syntax c) ?\n)
                       t)
                      (t nil)
                      )
                )                ; is-literal-or-symbol-following-char

               (start-parsing
                ()
                (message "\n\nHLASM PARSE OPERANDS: start-parsing (pos = %d lep = %d)."
                         operand-pos
                         (line-end-position))
                (when (and operand-pos
                           (< operand-pos limit)
                           (< operand-pos (line-end-position)))
                  (let ((c (following-char)))
                    (parse-operands c)))
                )
               )			; labels functions.
	  
            (msg "in loop with operand-pos = %s, eol = %s"
                 operand-pos
                 (line-end-position))

            (start-parsing)
            )				; cl-labels...
          )				; let*...
      ;; condition-case catchers...
      (search-failed (msg "search failed.") nil)
      (error (msg "parsing failed.") nil)
      )))


;; hlasm-mode--parse-statement
;;
;; Let's bite the bullet.  Parse an entire statement and set the match
;; accordingly.

;; (cl-defun hlasm-mode--parse-statement (&optional (limit (line-end-position)))
;;   ;; When called programmatically `limit' is end of buffer.
;;   (interactive)

;;   (message "\nHLASM PARSE STMT: entered with limit = %s @%s" limit (point))

;;   (if (hlasm-mode--continuation-line-p)
;;       (hlasm-mode--parse-continuation-card limit)
;;     (hlasm-mode--parse-statement-card limit)
;;     ))


;; hlasm-mode--continuation-card-p

;; Old version
;; (cl-defun hlasm-mode--continuation-card-p (&optional (*hlasm-mode--debug* t))
;;   "Returns T if the previous line has a continuation mark in column 72.

;; This function serves as an indication that the current line must be
;; parsed as a continuation line.  It returns NIL if the previous line
;; does not have a continuation mark in column 72."
  
;;   (interactive)
;;   (cl-flet ((msg
;;              (m &rest args)
;;              (when *hlasm-mode--debug*
;; 	       (apply 'message
;;                       (concat "HLASM IS-CONT: " m)
;;                       args)))
;;             )
;;     (save-excursion
;;       (msg "line-number-at-pos = %s." (line-number-at-pos))
;;       (let ((n-line (forward-line -1)))
;;         (cl-case n-line
;;           (-1 (msg "n-line = %s on first line." n-line)) ; Do nothing.
;;           (0 (msg "n-line = %s, current line = %s, (%s %s)."
;;                   n-line
;;                   (line-number-at-pos)
;;                   (line-beginning-position)
;;                   (line-end-position)))
;;           )

;;         (when (zerop n-line)
;;           (let ((c71 (move-to-column 71)))
;;             (msg "moved to column %s." c71)
;;             (cond ((< c71 71)		; Line shortes than 72.
;;                    (msg "card is only %s long." c71)
;;                    (cl-return-from hlasm-mode--continuation-card-p
;;                      (progn (msg "returning nil.") nil))
;;                    )
;;                   ((and (= c71 71)
;;                         (= (char-syntax (char-after)) ?\ ))
;;                    ;; Nothing on column 72.
;;                    ;; Do nothing.
;;                    (msg "card is 71 or more long, but C72 is a blank.")
;;                    (cl-return-from hlasm-mode--continuation-card-p
;;                      (progn (msg "returning nil.") nil))
;;                    )
;;                   ((and (= c71 71)      ; Paranoid.
;;                         (not (= (char-syntax (char-after)) ?\ ))) ; Paranoid.
;;                    ;;
;;                    (msg "card is 71 or more long, and C72 is ?\%c." (char-after))
;;                    (cl-return-from hlasm-mode--continuation-card-p
;;                      (progn (msg "returning t.") t))
;;                    )
;;                   ))
;;           )
;;         (msg "returning nil.")
;;         nil
;;         ))))


(cl-defun hlasm-mode--continuation-card-p (&optional limit (*hlasm-mode--debug* t))
  "Returns T if the previous line has a continuation mark in column 72.

This function serves as an indication that the current line must be
parsed as a continuation line.  It returns NIL if the previous line
does not have a continuation mark in column 72."
  
  (interactive)
  (hlasm-mode--messaging
   "HLASM IS-CONT: "
   (save-excursion
     (msg "line-number-at-pos = %s." (line-number-at-pos))
     (let ((n-line (forward-line -1)))
       (cl-case n-line
         (-1 (msg "n-line = %s on first line." n-line)) ; Do nothing.
         (0 (msg "n-line = %s, current line = %s, (%s %s)."
                 n-line
                 (line-number-at-pos)
                 (line-beginning-position)
                 (line-end-position)))
         )

       (if (zerop n-line)
           (let ((c71 (move-to-column 71)))
             (msg "moved to column %s." c71)
             (cond ((< c71 71)		; Line shortes than 72.
                    (msg "card is only %s long." c71)
                    (msg "returning nil.")
                    nil
                    )
                   ((and (= c71 71)
                         (= (char-syntax (char-after)) ?\ ))
                    ;; Nothing on column 72.
                    ;; Do nothing.
                    (msg "card is 71 or more long, but C72 is a blank.")
                    (msg "returning nil.")
                    nil
                    )
                   ((and (= c71 71)     ; Paranoid.
                         (not (= (char-syntax (char-after)) ?\ ))) ; Paranoid.
                    ;;
                    (msg "card is 71 or more long, and C72 is ?\%c."
                         (char-after))
                    (msg "returning t.")
                    t
                    ))
             )
         (progn (msg "returning nil.") nil)
         ))
     )))


(cl-defun hlasm-mode--card-continues-p (&optional (limit (point-max)) (*hlasm-mode--debug* t))
  "Returns T if the card has a continuation mark in column 72."
  
  (interactive)
  (hlasm-mode--messaging
   "HLASM CARD CONT: "
   
   (save-excursion
     (let ((c71 (move-to-column 71)))
       (msg "moved to column %s." c71)
       (cond ((< c71 71)		; Card shorter than 72.
	      (msg "card is only %s long." c71)
	      (cl-return-from hlasm-mode--card-continues-p
		(progn (msg "point = %s, returning nil." (point)) nil))
	      )
	     ((and (= c71 71)
		   (= (char-syntax (char-after)) ?\ ))
	      ;; Nothing on column 72.
	      ;; Do nothing.
	      (msg "card is 71 or more long, but C72 is a blank.")
	      (cl-return-from hlasm-mode--card-continues-p
		(progn (msg "point = %s, returning nil." (point)) nil))
	      )
	     ((and (= c71 71)		; Paranoid.
		   (not (= (char-syntax (char-after)) ?\ ))) ; Paranoid.
	      ;;
	      (msg "card is 71 or more long, and C72 is ?\%c." (char-after))
	      (cl-return-from hlasm-mode--card-continues-p
		(progn (msg "point = %s, returning t." (point)) t))
	      )
	     ))
     )
   (msg "something is wrong; returning nil.")
   nil
   ))


(cl-defun hlasm-mode--good-continuation-card-p (&optional
                                                (limit (point-max))
                                                (*hlasm-mode--debug* t))
  (interactive)
  (hlasm-mode--messaging
   "HLASM GOOD CONT CARD: "

   (save-excursion
     (beginning-of-line)                ; Unnecessary and paranoid.
    
     (= 15 (skip-chars-forward "[:space:]" limit))
     )
   ))


;; hlasm-mode--good-continuation-card-remark-free
;; Non-anchored version.

(cl-defun hlasm-mode--good-continuation-card-remark-free (&optional
							  (limit (point-max))
							  (*hlasm-mode--debug* t))
  (hlasm-mode--messaging
   "HLASM GOOD CONT CARD REMARK: "
   
   (when (<= (point) limit)
     (let ((continuing-card
            (re-search-forward hlasm-mode--continued-card limit t))
           )
       (when continuing-card
         (forward-line)
         (let ((current-card-cont-p
                (hlasm-mode--card-continues-p *hlasm-mode--debug*))
               )
           (cond ((hlasm-mode--good-continuation-card-p limit
                                                        *hlasm-mode--debug*)
                  (skip-chars-forward "[:space:]" limit)
                  (hlasm-mode--parse-operands limit *hlasm-mode--debug*)
                  (prog1 (re-search-forward ".*$")
                    (when current-card-cont-p
                      ;; Move back to BOL to ensure that the next round
                      ;; of Font Lock has a chance to handle the next
                      ;; good cont card.
                      (beginning-of-line)))
                  )
                 (t
                  (msg "not a good cont card, but let's keep going")
                  (set-match-data (match-data) t)
                  (end-of-line)
                  )
                 ))
         ))
     )))

;; Anchored version.

(cl-defun hlasm-mode--good-continuation-card-remark (&optional
						     (limit (point-max))
						     (*hlasm-mode--debug* t))
  (hlasm-mode--messaging
   "HLASM GOOD CONT CARD REMARK: "
   
   (when (and (<= (point) limit)		; I.e., the current line.
	      (hlasm-mode--good-continuation-card-p limit
						  *hlasm-mode--debug*))
     (skip-chars-forward "[:space:]" limit)
     (hlasm-mode--parse-operands limit *hlasm-mode--debug*)
     (re-search-forward ".*$")
     )))


;; hlasm-mode--bad-continuation-card-flag-free
;;
;; Non anchored version.

(cl-defun hlasm-mode--bad-continuation-card-flag-free (&optional
                                                       (limit (point-max))
                                                       (*hlasm-mode--debug* t))
  (hlasm-mode--messaging
   "HLASM BAD CONT CARD FLAG: "
   
   (when (<= (point) limit)
     (let ((continuing-card
            (re-search-forward hlasm-mode--continued-card limit t))
           )
       (when continuing-card
         (forward-line)
         (let ((current-card-cont-p
                (hlasm-mode--card-continues-p *hlasm-mode--debug*))
               )
           (cond ((hlasm-mode--good-continuation-card-p limit
                                                        *hlasm-mode--debug*)
                  (msg "not a bad cont card, but let's keep going")
                  (set-match-data (match-data) t)
                  (end-of-line)
                  )
                 (t
                  (skip-chars-forward "[:space:]" limit)
                  (prog1 (re-search-forward ".*$")
                    (when current-card-cont-p
                      ;; Move back to BOL to ensure that the next round
                      ;; of Font Lock has a chance to handle the next
                      ;; good cont card.
                      (beginning-of-line)))
                  )
                 ))
         ))
     )))


;; Anchored version.

(cl-defun hlasm-mode--bad-continuation-card-flag (&optional
                                                  (limit (point-max))
                                                  (*hlasm-mode--debug* t))
  (hlasm-mode--messaging
   "HLASM BAD CONT CARD FLAG: "
   
   (when (and (<= (point) limit)
	      (not (hlasm-mode--good-continuation-card-p limit
							 *hlasm-mode--debug*)))

     (skip-chars-forward "[:space:]" limit)
     (re-search-forward ".*$")
     )))


;; (cl-defun hlasm-mode--parse-continuation-card (limit)
;;   (beginning-of-line)
;;   (let ((c16 (skip-chars-forward "[:space:]" limit)))
;;     (cond ((/= c16 16)
;; 	   ;; Something wrong.
;; 	   (message "\nHLASM PARSE CONT: line is %d long, c16 is %s, char is ?\%c."
;; 		    (- (line-end-position) (line-beginning-position))
;; 		    c16
;; 		    (char-after))
;; 	   (cl-return-from hlasm-mode--parse-continuation-card
;; 	     (re-search-forward hlasm-mode--bad-cont-line-regexp limit t))
;; 	   )
	  
;; 	  ((= c16 16)
;; 	   (hlasm-mode--parse-operands limit)
;; 	   )
	  
;; 	  (t
;; 	   (message "\nHLASM PARSE CONT: c16 is %s; something wrong is happening." c16)
;; 	   (cl-return-from hlasm-mode--parse-continuation-card
;; 	     nil)
;; 	   )
;; 	  )))


(cl-defun hlasm-mode--continuation-card-remark (&optional
                                                (limit (point-max))
                                                should-be-cont-line
                                                (*hlasm-mode--debug* t))
  (hlasm-mode--messaging
   "HLASM CONT CARD REMARK: "
    
   (when (or should-be-cont-line (hlasm-mode--continuation-card-p limit))
     (msg "line %s is a continuation line" (line-number-at-pos))
     (beginning-of-line)
     (let ((c16 (skip-chars-forward "[:space:]" limit)))
       (cond ((/= c16 15)
	      ;; Something wrong.
	      (msg "line is %d long, c16 is %s, char is ?\%c."
		   (- (line-end-position) (line-beginning-position))
		   c16
		   (char-after))
              (msg "returning nil.")
	      (cl-return-from hlasm-mode--continuation-card-remark
	        nil)
	      )
	  
	     ((= c16 15)
	      (hlasm-mode--parse-operands limit)
	      (skip-chars-forward "[:space:]" limit)
              (msg "returning match.")
	      (cl-return-from hlasm-mode--continuation-card-remark
	        (re-search-forward ".*$" limit t))
	      )
	  
	     (t
	      (msg "c16 is %s; something wrong is happening." c16)
	      (cl-return-from hlasm-mode--parse-continuation-card
	        nil)
	      )
	     )))
   ))


;; (cl-defun hlasm-mode--bad-continuation-card-new (&optional
;;                                                  (limit (point-max))
;; 					         should-be-cont-line
;; 					         (*hlasm-mode--debug* t))
;;   (interactive)
;;   ;; (beginning-of-line)
;;   (cl-flet ((msg
;;              (m &rest args)
;;              (when *hlasm-mode--debug*
;;                (apply 'message
;;                       (concat "HLASM BAD CONT2: " m)
;;                       args)))
;;             )
;;     (message "\n")
;;     (msg "entering: point %d limit %d line %d"
;;          (point)
;;          limit
;;          (line-number-at-pos))
    
;;     (if (< (point) limit)
;;         (when (re-search-forward "^.\\{71\\}\\S-" limit t)
;;           (forward-line)
;;           (beginning-of-line)
;;           (msg "on a cont line %s %s" (point) (line-number-at-pos))
;;           (let ((c16 (skip-chars-forward "[:space:]" limit)))
;;             (when (/= c16 16)
;;                   ;; Not a kosher continuation line.
;;                   ;; This is our bad continuation line.
;;                   ;;
;;                   ;; Come back later to fix comments and empty
;;                   ;; lines if needed.
                       
;;                   (msg "line is %d long, c16 is %s, char is ?\%c."
;;                        (- (line-end-position) (line-beginning-position))
;;                        c16
;;                        (char-after))
;;                   (cl-return-from hlasm-mode--bad-continuation-card-new
;;                     (re-search-forward ".*$" limit t))
;;                   )))
;;	  
;;       (progn (msg "beyond limit %d." limit) nil))
;;     ))


;; hlasm-mode--bad-continuation-card-new
;;
;; New version relying on 'hlasm-mode--ensure-card-position'.

(cl-defun hlasm-mode--bad-continuation-card-new (&optional
                                                 (limit (point-max))
					         should-be-cont-line
					         (*hlasm-mode--debug* t))
  (interactive)
  ;; (beginning-of-line)

  (hlasm-mode--messaging
   "HLASM BAD CONT2: "
    
   (if (<= (point) limit)
       (if (hlasm-mode--ensure-card-position limit
                                             *hlasm-mode--debug*)

           ;; Now we are at the beginning of the card we want to
           ;; check.
           
           (progn
             (msg "on a cont line %s %s %s %s"
                  (point)
                  (line-number-at-pos)
                  (current-column)
                  (match-data))
              
             (cond ((hlasm-mode--good-continuation-card-p limit
                                                          *hlasm-mode--debug*)
                    ;; Not a bad continuation line
                    ;; Move to the end of line for the next call,
                    ;; ensuring that the match is empty.

                    (msg "not a bad continuation line %s"
                         (line-number-at-pos))
                    (msg "match data %s" (match-data))
                 
                    (set-match-data (match-data) t)
                    (end-of-line)
                    (point))

                   (t
                    ;; Not a kosher continuation line.
                    ;; This is our bad continuation line.
                    ;;
                    ;; Come back later to fix comments and empty
                    ;; lines if needed.
                    (beginning-of-line)	; Paranoid.
                    (let ((c16 (skip-chars-forward "[:space:]" limit)))
                      (msg "line is %d long, c16 is %s, char is ?\%c md %s."
                           (- (line-end-position) (line-beginning-position))
                           c16
                           (char-after))
                      (msg "match data %s" (match-data))
              
                      (re-search-forward ".*$" limit t))
                    )
                   )
             )
         (msg "couldn't ensure card position"))
     (msg "beyond limit %d." limit)
     )))


;; Old version
;; (cl-defun hlasm-mode--card-instruction (&optional
;;                                         (limit (point-max))
;;                                         should-be-cont-line
;;                                         (*hlasm-mode--debug* t))
;;   (interactive)
;;   ;; (beginning-of-line)

;;   (hlasm-mode--messaging
;;    "HLASM CARD INSTR: "

;;    (if (<= (point) limit)
;;        (let ((after-instr-pt
;;               (re-search-forward hlasm-mode--instructions limit t)))
;;          ;; Now lets check if we are on a continuation line.
;;          (when (hlasm-mode--continuation-card-p limit *hlasm-mode--debug*)
;;            (set-match-data ())
;; 	   (forward-line)
;; 	   (beginning-of-line)
;; 	   (let ((c16 (skip-chars-forward "[:space:]" limit)))
;; 	     (when (/= c16 16)
;; 	       ;; Not a kosher continuation line.
;; 	       ;; This is our bad continuation line.
;; 	       ;;
;; 	       ;; Come back later to fix comments and empty
;; 	       ;; lines if needed.
                       
;; 	       (msg "line is %d long, c16 is %s, char is ?\%c."
;; 		    (- (line-end-position) (line-beginning-position))
;; 		    c16
;; 		    (char-after))
;; 	       (cl-return-from hlasm-mode--bad-continuation-card-new
;; 		 (re-search-forward ".*$" limit t))
;; 	       ))))
	  
;;      (progn (msg "beyond limit %d." limit) nil))
;;    )
;;   )

(cl-defun hlasm-mode--card-instruction (&optional
                                        (limit (point-max))
                                        (*hlasm-mode--debug* t))
  (interactive)
  ;; (beginning-of-line)

  (hlasm-mode--messaging
   "HLASM CARD INSTR: "

   (if (<= (point) limit)
       (if (hlasm-mode--ensure-card-position limit *hlasm-mode--debug*)
           (if (hlasm-mode--continuation-card-p limit *hlasm-mode--debug*)
               ;; Do nothing
               ;; (progn
               ;;   ;; ...but zero the match-data just in case.
               ;;   (set-match-data (match-data) t)
               ;;   (end-of-line)
               ;;   (point))
             
               ;; ... but, just in case, look for next instruction;
               ;; recursively.  Of course, this would mean that we are
               ;; checking for boieng a continuation line some times too
               ;; many, but at least the intent of the code is clear.
	     
               (when (hlasm-mode--card-continues-p limit *hlasm-mode--debug*)
                 (forward-line)
                 (hlasm-mode--card-instruction limit *hlasm-mode--debug*)
                 )
           
             ;; Otherwise, try to match.
             (re-search-forward hlasm-mode--instructions limit t))
         (msg "couldn't ensure card position")
         )
     (msg "beyond limit %d." limit))
   ))



;; hlasm-mode--card-remarks
;; New version handling continuation lines and complex operands.

;; Old versione
;; (cl-defun hlasm-mode--card-remarks (&optional
;; 				    (limit (line-end-position))
;; 				    (*hlasm-mode--debug* t))
;;   (interactive)
;;   (cl-flet ((msg
;; 	     (m &rest args)
;; 	     (when *hlasm-mode--debug*
;; 	       (apply 'message
;;                       (concat "HLASM CARD REMARKS: " m)
;;                       args)))
;;             )
;;     (msg "entered with limit = %s @%s" limit (point))

;;     (condition-case nil
;; 	(if (hlasm-mode--continuation-card-p)
;; 	    ;; Find a remark on a continuation line; we assume things
;; 	    ;; are kosher here; that is, we are not on a bad
;; 	    ;; continuation line.
;;             (progn
;;               (msg "card %s is a continuation card."
;;                    (line-number-at-pos))
;; 	      (hlasm-mode--continuation-card-remark limit t))

;; 	  ;; Not on a continuation line.
;; 	  (let ((post-instr-point
;; 		 (re-search-forward hlasm-mode--instructions limit t))
;; 		)
;;             (when post-instr-point
;;               (msg "\"%s\"" (match-string 0))
;;               (msg "after instruction.")
;;               (skip-chars-forward "[:space:]" limit)
;;               (hlasm-mode--parse-operands limit)
;;               (cl-return-from hlasm-mode--card-remarks
;;                 (re-search-forward ".*$" limit t)))
;; 	    ))
;;       ;; condition-case catchers...
;;       (search-failed (msg "search failed.") nil)
;;       )
;;     ))


(cl-defun hlasm-mode--ensure-card-position (&optional
				            (limit (point-max))
				            (*hlasm-mode--debug* t))
  (interactive)
  (hlasm-mode--messaging
   "HLASM ENSURE CARD: "
       
   (let ((next-card-pos
          (re-search-forward hlasm-mode--non-blank-card limit t))
         )
     (msg "next-card-pos %s" next-card-pos)
     (if next-card-pos
         (prog1
            
             (beginning-of-line)

           ;; Here we need to start the actual processing of the
           ;; line/card.
           (msg "now at line %s column %s point %s (from point %s)"
                (line-number-at-pos)
                (current-column)
                (point)
                next-card-pos
                )
           )
       ;; No non-empty line found
       (progn
         (msg "no non-empty line found.")
         nil))
     )))
  

(cl-defun hlasm-mode--card-remarks (&optional
				    (limit (point-max))
				    (*hlasm-mode--debug* t))

  
  (interactive)                         ; Just for debugging.
  (hlasm-mode--messaging
   "HLASM CARD REMARKS: "

   ;; Font Lock does not work line-by-line even if it makes some
   ;; shenanigans in this regard.
   ;;
   ;; As things are written, this function is usually called at the
   ;; end of line, after matching something like ".$*", but it
   ;; actually must start matching on a proper line.  Hence the
   ;; forward movement on a "new" line (i.e., "card") done by the
   ;; call to 'hlasm-mode--ensure-card-position'.

   (condition-case nil
       (if (hlasm-mode--ensure-card-position limit
					     *hlasm-mode--debug*)

	   ;; Now we are at the beginning of the card we want to check.
	   (progn
	     (msg "on line %s" (line-number-at-pos))
          
	     (if (hlasm-mode--continuation-card-p limit *hlasm-mode--debug*)
		 ;; Find a remark on a continuation line; we assume things
		 ;; are kosher here; that is, we are not on a bad
		 ;; continuation line.
		 (progn
		   (msg "card %s is a continuation card."
			(line-number-at-pos))
		   (hlasm-mode--continuation-card-remark limit t))

	       ;; Not on a continuation line.
	       (let ((post-instr-point
		      (re-search-forward hlasm-mode--instructions limit t))
		     )
		 (when post-instr-point
		   (msg "\"%s\"" (match-string 0))
		   (msg "after instruction.")
		   (skip-chars-forward "[:blank:]" limit)
		   (hlasm-mode--parse-operands limit)
		   (cl-return-from hlasm-mode--card-remarks
		     (prog1 (re-search-forward ".*$" limit t)
		       (msg "exiting with line = %s"
			    (line-number-at-pos))))
		   ))
	       ))
	 (msg "couldn't ensure card pos"))
     
     ;; condition-case catchers...
     (search-failed (msg "search failed.") nil)
     )
   ))


;; HLASM IBM faces.
;; ================

(defcustom hlasm-mode-string-face 'font-lock-string-face
  "The face used to fontify strings (single-quoted) in HLASM IBM mode."
  :group 'hlasm
  :type 'symbol
  )

(defcustom hlasm-mode-names-face 'font-lock-function-name-face
  "The face used to fontify 'names' in HLASM IBM mode."
  :group 'hlasm
  :type 'symbol
  )

(defcustom hlasm-mode-operations-face 'font-lock-keyword-face
  "The face used to fontify 'operations' in HLASM IBM mode."
  :group 'hlasm
  :type 'symbol
  )

(defcustom hlasm-mode-operands-face 'font-lock-type-face
  "The face used to fontify 'operands' in HLASM IBM mode."
  :group 'hlasm
  :type 'symbol
  )

(defcustom hlasm-mode-operators-face 'font-lock-builtin-face
  "The face used to fontify 'operators' in HLASM IBM mode."
  :group 'hlasm
  :type 'symbol
  )


;; Just a try...

(defface hlasm-mode-comment-face-red
  '((t :foreground "red" :weight bold))
  "Face to colorize comments in HLASM IBM mode."
  :group 'hlasm
  )

(defface hlasm-mode-invalid-card
  '((t :underline (:color "red" :style wave)))
  "Face to colorize 'invalid' cards, e.g., bad continuation cards."
  :group 'hlasm
  )

(defvar hlasm-mode-invalid-card 'hlasm-mode-invalid-card) ; Why we need this?  Who knows?


(defcustom hlasm-mode-comment-face 'font-lock-comment-face ; 'hlasm-comment-face-red
  "The face used to fontify 'comments' in HLASM IBM mode."
  :group 'hlasm
  :type 'symbol
  )

;; (defcustom hlasm-comment-face 'font-lock-comment-face
;;   "The face used to fontify 'comments' in HLASM IBM mode."
;;   :group 'hlasm
;;   :type 'symbol
;;   )

(defcustom hlasm-mode-grey-face 'shadow
  "The face used to 'fontify out' possible JCL when assembler is embedded."
  :group 'hlasm
  :type 'symbol
  )


(defcustom hlasm-mode-invalid-face 'hlasm-mode-invalid-card
  "The face used to indicate invalid code.

Now used only for invalid continuation lines."
  :group 'hlasm
  :type 'symbol
  )


;; Font Lock Definitions.
;; ======================

;; Old version attempt without anchored mathchers.

;; (defvar hlasm-mode--font-lock-keywords
;;   `(
;;     ;; (hlasm-mode--bad-continuation-card-new . ',hlasm-mode-invalid-face) ; This has to be first.
    
;;     (,hlasm-mode--names . (1 hlasm-mode-names-face))
;;     (,hlasm-mode--instructions . (2 hlasm-mode-operations-face))
;;     ;; (,hlasm-registers . ,hlasm-operators-face)

    
;;     (,hlasm-mode--card-end-comments-0 . ,hlasm-mode-comment-face)

;;     (hlasm-mode--card-remarks . ,hlasm-mode-comment-face)
;;     ;; (,hlasm-card-end-comments-1 . (2 ,hlasm-comment-face))
;;     ;; (,hlasm-card-end-comments-2 . (1 ,hlasm-comment-face))
;;     ;; (,hlasm-card-end-comments-3 . (1 ,hlasm-comment-face))

;;     (,hlasm-mode--attributed-symbol . (1 ,hlasm-mode-operands-face))

;;     ;; (,hlasm-strings . (0 ,hlasm-string-face t))
;;     (,hlasm-mode--strings . (0 ,hlasm-mode-string-face))


;;     ;; (,hlasm-mode--jcl . (0 hlasm-mode-grey-face t))
;;     )
;;   "The HLASM IBM mode 'font-lock' 'keyword' specification."
;;   )

(defvar hlasm-mode--font-lock-keywords
  `(
    (,hlasm-mode--continued-card        ; Anchor.

     ;; Good continuation line after anchor.

     (hlasm-mode--good-continuation-card-remark
      (forward-line)
      nil
      (0 ,hlasm-mode-comment-face))

     ;; Bad continuation line after anchor.

     (hlasm-mode--bad-continuation-card-flag
      (beginning-of-line) ; (forward-line) ; Previous anchored match moved the line.
      nil
      (0  ,hlasm-mode-invalid-face))
     )
    
    (,hlasm-mode--names . (1 hlasm-mode-names-face))
    (hlasm-mode--card-instruction . (2 hlasm-mode-operations-face))
    ;; (,hlasm-registers . ,hlasm-operators-face)

    
    (,hlasm-mode--card-end-comments-0 . ,hlasm-mode-comment-face)

    (hlasm-mode--card-remarks . ,hlasm-mode-comment-face)
    ;; (,hlasm-card-end-comments-1 . (2 ,hlasm-comment-face))
    ;; (,hlasm-card-end-comments-2 . (1 ,hlasm-comment-face))
    ;; (,hlasm-card-end-comments-3 . (1 ,hlasm-comment-face))

    (,hlasm-mode--attributed-symbol . (1 ,hlasm-mode-operands-face))

    ;; (,hlasm-strings . (0 ,hlasm-string-face t))
    (,hlasm-mode--strings . (0 ,hlasm-mode-string-face))


    ;; (,hlasm-mode--jcl . (0 hlasm-mode-grey-face t))
    )
  "The HLASM IBM mode 'font-lock' 'keyword' specification."
  )


(defvar hlasm-mode--font-lock-defaults
  (list 'hlasm-mode--font-lock-keywords
	;; nil ; Do syntax based processing.
	t   ; Do not do syntax based processing.
	)
  "The HLASM IBM mode 'font-lock' defaults specification."
  )


;;; hlasm-mode-syntax-table

(defvar hlasm-mode--syntax-table
  (let ((asmst (make-syntax-table)))
    ;; (modify-syntax-entry ?* ". 1" asmst)
    ;; (modify-syntax-entry ?\n "> " asmst)
    asmst
    )
  "The HLASM IBM mode syntax table."
  )


;;; hlasm-keymap
;;; Not necessary.  define-derived-mode sets it up automatically.

(defvar hlasm-mode--map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km prog-mode-map) ; Inherit from prog-mode-map!
    
    km)
  "The HLASM IBM mode key map.")



;;; hlasm-imenu-generic-expression

;; (defvar hlasm-imenu-generic-expression
;;   '((nil "^\\([^* /]+\\) +[[:graph:]]+$" 1) ; Also avoid JCL
;;     (nil "^\\([^* /]+\\) +[[:graph:]]+ .*$" 1)
;;     )
;;   "The HLASM IBM Imenu regular expressions.")

(defvar hlasm-mode--imenu-generic-expression
  '((nil "^\\([[:alpha:]][[:alnum:]@$#_]*\\)" 1) ; Also avoid JCL
    )
  "The HLASM IBM Imenu regular expressions.")



;;; hlasm-mode

(define-derived-mode hlasm-mode asm-mode "HLASM IBM"
  "HLASM IBM mode is a major mode to edit IBM Assembler code.

IBM mainframes have a number of assemblers available; the latest
incarnation being \"High Level Assembler\" (HLASM).  This mode helps
in editing assembly code mostly for MVS or z/OS.

Notes:

Not all features of HLASM are currently supported."

  :syntax-table hlasm-mode--syntax-table

  (setq-local font-lock-defaults hlasm-mode--font-lock-defaults)

  (setq-default indent-tabs-mode nil)   ; Avoid spurious tabs.

  (face-remap-add-relative hlasm-mode-operations-face
                           :weight 'bold
                           :foreground "red" ; REVEDIT/RPF color.
                           )

  (face-remap-add-relative hlasm-mode-operators-face
			   :weight 'bold
			   :foreground "Forest Green") ; May be too much.

  (face-remap-add-relative hlasm-mode-comment-face
                           :foreground "cyan")
  
  ;; Comments.
  ;; (setq-local comment-start "//\*")
  ;; (setq-local comment-end "")
  ;; (setq-local comment-start-skip
  ;;             "^//[[:graph:]]*[[:blank:]]+[[:graph:]]+[[:blank:]]+")

  ;; Set up the mode keymap.

  (use-local-map hlasm-mode--map)
  (local-set-key (kbd "RET") 'electric-indent-just-newline)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)


  ;; Set up tab stops.

  (setq-local tab-stop-list '(9 15 40))
  
  
  ;; Set up the menus.

  (easy-menu-define hlasm-mode--mainframe-os-menu hlasm-mode--map
    "HLASM IBM commands"
    '("HLASM IBM OS"
      ["Submit Compilation" hlasm-mode-compile]
      ["Submit Compilation and Go" hlasm-mode-compile-and-go])
    )

  (setq-local imenu-generic-expression
	      (reverse hlasm-mode--imenu-generic-expression))
  (imenu-add-to-menubar "HLASM IBM Code")

  ;; Start the IRON MAIN minor mode, which sets up the ruler and the
  ;; "card" editing limits, plus the fill-column indicator.

  (when (fboundp 'iron-main-mode) (iron-main-mode))

  'hlasm-mode
  )


;;; Functions and Commands.

(defvar hlasm-mode--not-implemented-flag t
  "Non-nil means the context where used is still unimplemented.")


;;; hlasm-compile-jcl

(defvar hlasm-mode--compile-jcl
  "//EASMICJ  JOB (%s),'%s',
//            USER=%s,PASSWORD=%s,
//            CLASS=A,
//            MSGCLASS=%s,MSGLEVEL=(1,1)
//EASMICEX EXEC PGM=ASMFC,PARM.ASM='LIST,RENT,NODECK'
//EASMICEX.DD *
"
  "JCL job to compile a HLASM IBM code.

This variable is acually a format string.")


(defun hlasm-mode-prepare-job (&optional
			   acct
			   name
			   user
			   password
			   msgclass)
  (format hlasm-mode--compile-jcl
	  (or acct "1")
	  (or name "EMACS COMPILES IRON ASSEMBLER")
	  (or user "HERC01")		; Sensible TK4- default.
	  (or password "CUL8TR")	; Ditto.
	  (or msgclass "H"))
  )


;;; hlasm-mode-compile

(defun hlasm-mode-compile (&optional port)
  "Compiles the buffer by submitting a job to the card reader on PORT."
  (interactive
   (if hlasm-mode--not-implemented-flag
       (list nil)
     (let ((p (read-number "Card Reader number/port: " 3505))
	   )
       (list p)))
   )

  (unless port
    (setq port 3505))

  (message "Assembler compile not yet implemented; sorry...")
  )


;;; hlasm-compile-and-go

(defun hlasm-mode-compile-and-go (&optional port)
  "Compile and execute the buffer on the mainframe.

This is obtained by submitting a job to the card reader on PORT."
  (interactive
   (if hlasm-mode--not-implemented-flag
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

(add-to-list 'auto-mode-alist '("\\.asma\\'" . hlasm-mode))
(add-to-list 'auto-mode-alist '("\\.bal\\'" . hlasm-mode))
(add-to-list 'auto-mode-alist '("\\.mac\\'" . hlasm-mode))

(provide 'hlasm-mode)

;;; hlasm-mode.el ends here
