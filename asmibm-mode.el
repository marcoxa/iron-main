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
;; Version: 2022-10-05.1
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

(defcustom asmibm-mode-os-flavor "MVS 3.8j"
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

(defvar asmibm-mode--strings
  "'.*'"
  "ASM IBM strings.")


(defvar asmibm-mode--names
  "^\\([[:alpha:]&.][[:alnum:]@$#_&.]*\\|\\.[:alpha:][[:alnum:]@$#_&.]*\\)"
  "ASM IBM names (or labels).

These are the 'names' of instructions.")


;; (defvar asmibm-names
;;   "^\\([^* ][[:graph:]]+\\)"
;;   "ASM IBM names.
;;
;; These are the 'names'of instructions.")

(defvar asmibm-mode--instructions
  "^\\([[:alpha:]&.][[:alnum:]@$#_&.]*\\)?[[:blank:]]+\\([[:alpha:]@$#&._][[:alnum:]@$#&._]*\\)"
  "ASM IBM instructions.

These are the instructions mnemonics.
The second regexp group contains the istruction name.")

;; (defvar asmibm-instructions
;;   "^\\([^* ][[:graph:]]+\\| +\\) +\\([[:graph:]]+\\)"
;;   "ASM IBM instructions.

;; These are the instructions mnemonics.")


(defvar asmibm-mode--16-white-columns
  "^ \{16\}"

  "ASM IBM continuation line prefix.

Continuation lines must start at column 16.  This regexp matches 16
spaces (columns) at the beginning of line.")


(defvar asmibm-mode--registers
  "R[0-9][0-5]?"
  "ASM IBM register names.")


(defvar asmibm-mode--card-end-comments-0
  "^\\.?\\*.*$"
  "ASM IBM 'comment' card (single '*' is for regular assembler, '.*'
is for macros.

A '*' or a '.*' (for macros) at the beginning of the card (line) marks
a comment.")


(defvar asmibm-mode--card-end-comments-1
  "^\\([[:alpha:]&.][[:alnum:]@$#_&.]*\\)?[[:blank:]]+[[:alpha:]@$#&._][[:alnum:]@$#&._]*[[:blank:]]+[-[:alnum:],*='()+]?\\([[:graph:]]*\\)$
     "
  "ASM IBM 'end of card' comments for 'full' cards.

Anything after the 'operands' in a card is a comment; this regexp
selects them.")


;; (defvar asmibm-card-end-comments-1
;;   "^[^* ]+ +[[:graph:]]+ +[[:graph:]]+ +\\([[:graph:]].*\\)"
;;   "ASM IBM 'end of card' comments for 'full' cards..

;; Anything after the 'operands' in a card is a comment; this regexp
;; selects them.")


(defvar asmibm-mode-card-end-comments-2
  "^ +[[:graph:]]+ +\\([[:graph:]].*\\)"
  "ASM IBM 'end of card' comments for 'continuation' cards.

Anything after the 'operands' in a card is a comment; this regexp
selects them in case of 'continuation' cards that do not have the
'name' and 'operation'.")


(defvar asmibm-mode--card-end-comments-3
  "^ +[[:graph:]]+ +[[:graph:]]+ +\\([[:graph:]].*\\)"
  "ASM IBM 'end of card' comments for 'no operands' cards.

Anything after the 'operands' in a card is a comment; this regexp
selects them in case of 'no operands' cards that do not have the
'name' and 'operands'; however, the 'operands' must be at least one
non blank character.")


;; asmibm-attribute-symbol
;; Note that the following does only attributed symbols and not
;; symbols and expressions as per IBM specs.

(defvar asmibm-mode--attributed-symbol
  "\\([LKDINOST]'[[:alnum:]]+\\|L'\\*\\)"

  "ASM IBM 'attributed terms.

This is typically the 'length' of something, e.g.,

    L'FOOBAR

indicating the length of FOOBAR.
Other 'attributes' are introduced by the other letters.

See, e.g.: https://www.ibm.com/docs/en/zos/2.1.0?topic=terms-other-attribute-references
")


(defvar asmibm-mode--jcl
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


(cl-defun asmibm-mode--card-remarks (&optional (limit (line-end-position)))
  ;; When called programmatically `limit' is end of buffer.
  ;; (interactive)

  (message "\nASMIBM CARD REMARK: entered with limit = %s @%s" limit (point))

  (condition-case nil
      (let* ((current-point (point))
	     (post-instr-point
	      (re-search-forward asmibm-mode--instructions nil nil))
	     
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
			 (cl-return-from asmibm-mode--card-remarks
			   (let ((result (re-search-forward ".*$" limit t)))
			     (message "ASMIBM CARD REMARK: finishing match data \"%s\" %s"
			      	      (match-string 0)
			     	      (match-data))
			     result))
		       (cl-return-from asmibm-mode--card-remarks
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
		       (cl-return-from asmibm-mode--card-remarks
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
		    )			; labels functions.
	  
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

		    ((char-equal c ?\') ; Tricky part: string or attribute?
		     (let ((prev-char (preceding-char)))

		       (message "ASMIBM CARD REMARK: attribute with prev c = `%c'"
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


;; asmibm-mode--parse-operand
;;
;; Trying to parse the lovely "expression".
;;
;; Notes:
;;
;; The parser is stupid and does not span lines.  That is, if you open
;; a parenthesis you must close it on the same line.

(cl-defun asmibm-mode--parse-operands (&optional
                                       (limit (line-end-position))
                                       (*debug-parse-operands* t)
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
             (when *debug-parse-operands*
               (apply 'message
                      (concat "ASMIBM PARSE OPERANDS: " m)
                      args)))
            )

    (message "\nASMIBM PARSE OPERANDS: entered with limit = %s @%s" limit (point))

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

                (cl-return-from asmibm-mode--parse-operands (point))
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
                (message "\n\nASMIBM PARSE OPERANDS: start-parsing (pos = %d lep = %d)."
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


;; asmibm-mode--parse-statement
;;
;; Let's bite the bullet.  Parse an entire statement and set the match
;; accordingly.

(cl-defun asmibm-mode--parse-statement (&optional (limit (line-end-position)))
  ;; When called programmatically `limit' is end of buffer.
  (interactive)

  (message "\nASMIBM PARSE STMT: entered with limit = %s @%s" limit (point))

  (if (asmibm-mode--continuation-line-p)
      (asmibm-mode--parse-continuation-card limit)
    (asmibm-mode--parse-statement-card limit)
    ))


(cl-defun asmibm-mode--continuation-line-p ()
  (interactive)
  (save-excursion
    (message "\nASMIBM IS-CONT: line-number-at-pos = %s."
             (line-number-at-pos))
    (let ((n-line (forward-line -1)))
      (cl-case n-line
	(-1 (message "\nASMIBM IS-CONT: n-line = %s on first line." n-line)) ; Do nothing.
	(0 (message "\nASMIBM IS-CONT: n-line = %s, current line = %s, (%s %s)."
		    n-line
		    (line-number-at-pos)
		    (line-beginning-position)
		    (line-end-position)))
	)

      (when (zerop n-line)
	(let ((c71 (move-to-column 71)))
          (message "ASMIBM IS-CONT: moved to column %s." c71)
	  (cond ((< c71 71)		; Line shortes than 72.
		 (message "\nASMIBM IS-CONT: card is only %s long." c71)
		 nil
		 )
		((and (= c71 71)
		      (= (char-syntax (char-after)) ?\ ))
		 ;; Nothing on column 72.
		 ;; Do nothing.
		 (message "\nASMIBM IS-CONT: card is 71 or more long, but C72 is a blank.")
		 nil
		 )
		((and (= c71 71)                                ; Paranoid.
		      (not (= (char-syntax (char-after)) ?\ ))) ; Paranoid.
		 ;;
		 (message "\nASMIBM IS-CONT: card is 71 or more long, and C72 is ?\%c."
                          (char-after))
		 t
		 )
		))
	))))


(defvar asmibm-mode--bad-cont-line-regexp
  (rx (group)				; Dummy name/label
      (group)				; Dummy instruction
      (group)				; Dummy operands
      (group)				; Dummy remark
      (group (zero-or-more any))
      eol
      ))


(cl-defun asmibm-mode--parse-continuation-card (limit)
  (beginning-of-line)
  (let ((c16 (skip-chars-forward "[:space:]" limit)))
    (cond ((/= c16 16)
	   ;; Something wrong.
	   (message "\nASMIBM PARSE CONT: line is %d long, c16 is %s, char is ?\%c."
		    (- (line-end-position) (line-beginning-position))
		    c16
		    (char-after))
	   (cl-return-from asmibm-mode--parse-continuation-card
	     (re-search-forward asmibm-mode--bad-cont-line-regexp limit t))
	   )
	  
	  ((= c16 16)
	   (asmibm-mode--parse-operands limit)
	   )
	  
	  (t
	   (message "\nASMIBM PARSE CONT: c16 is %s; something wrong is happening." c16)
	   (cl-return-from asmibm-mode--parse-continuation-card
	     nil)
	   )
	  )))


;;; ASM IBM faces.

(defcustom asmibm-mode-string-face 'font-lock-string-face
  "The face used to fontify strings (single-quoted) in ASM IBM mode."
  :group 'asmibm
  :type 'symbol
  )

(defcustom asmibm-mode-names-face 'font-lock-function-name-face
  "The face used to fontify 'names' in ASM IBM mode."
  :group 'asmibm
  :type 'symbol
  )

(defcustom asmibm-mode-operations-face 'font-lock-keyword-face
  "The face used to fontify 'operations' in ASM IBM mode."
  :group 'asmibm
  :type 'symbol
  )

(defcustom asmibm-mode-operands-face 'font-lock-type-face
  "The face used to fontify 'operands' in ASM IBM mode."
  :group 'asmibm
  :type 'symbol
  )

(defcustom asmibm-mode-operators-face 'font-lock-builtin-face
  "The face used to fontify 'operators' in ASM IBM mode."
  :group 'asmibm
  :type 'symbol
  )


;; Just a try...

(defface asmibm-mode-comment-face-red
  '((t :foreground "red" :weight bold))
  "Face to colorize comments in ASM IBM mode."
  :group 'asmibm
  )


(defcustom asmibm-mode-comment-face 'font-lock-comment-face ; 'asmibm-comment-face-red
  "The face used to fontify 'comments' in ASM IBM mode."
  :group 'asmibm
  :type 'symbol
  )

;; (defcustom asmibm-comment-face 'font-lock-comment-face
;;   "The face used to fontify 'comments' in ASM IBM mode."
;;   :group 'asmibm
;;   :type 'symbol
;;   )

(defcustom asmibm-mode-grey-face 'shadow
  "The face used to 'fontify out' possible JCL when assembler is embedded."
  :group 'asmibm
  :type 'symbol
  )


(defvar asmibm-mode--font-lock-keywords
  `(
    (,asmibm-mode--names . (1 asmibm-mode-names-face))
    (,asmibm-mode--instructions . (2 asmibm-mode-operations-face))
    ;; (,asmibm-registers . ,asmibm-operators-face)

    
    (,asmibm-mode--card-end-comments-0 . ,asmibm-mode-comment-face)
    (asmibm-mode--card-remarks . ,asmibm-mode-comment-face)
    ;; (,asmibm-card-end-comments-1 . (2 ,asmibm-comment-face))
    ;; (,asmibm-card-end-comments-2 . (1 ,asmibm-comment-face))
    ;; (,asmibm-card-end-comments-3 . (1 ,asmibm-comment-face))

    (,asmibm-mode--attributed-symbol . (1 ,asmibm-mode-operands-face))

    ;; (,asmibm-strings . (0 ,asmibm-string-face t))
    (,asmibm-mode--strings . (0 ,asmibm-mode-string-face))


    (,asmibm-mode--jcl . (0 asmibm-mode-grey-face t))
    )
  "The ASM IBM mode 'font-lock' 'keyword' specification."
  )


(defvar asmibm-mode--font-lock-defaults
  (list 'asmibm-mode--font-lock-keywords
	;; nil ; Do syntax based processing.
	t   ; Do not do syntax based processing.
	)
  "The ASM IBM mode 'font-lock' defaults specification."
  )


;;; asmibm-mode-syntax-table

(defvar asmibm-mode--syntax-table
  (let ((asmst (make-syntax-table)))
    ;; (modify-syntax-entry ?* ". 1" asmst)
    ;; (modify-syntax-entry ?\n "> " asmst)
    asmst
    )
  "The ASM IBM mode syntax table."
  )


;;; asmibm-keymap
;;; Not necessary.  define-derived-mode sets it up automatically.

(defvar asmibm-mode--map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km prog-mode-map) ; Inherit from prog-mode-map!
    
    km)
  "The ASM IBM mode key map.")



;;; asmibm-imenu-generic-expression

;; (defvar asmibm-imenu-generic-expression
;;   '((nil "^\\([^* /]+\\) +[[:graph:]]+$" 1) ; Also avoid JCL
;;     (nil "^\\([^* /]+\\) +[[:graph:]]+ .*$" 1)
;;     )
;;   "The ASM IBM Imenu regular expressions.")

(defvar asmibm-mode--imenu-generic-expression
  '((nil "^\\([[:alpha:]][[:alnum:]@$#_]*\\)" 1) ; Also avoid JCL
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

  :syntax-table asmibm-mode--syntax-table

  (setq-local font-lock-defaults asmibm-mode--font-lock-defaults)

  (setq-default indent-tabs-mode nil)   ; Avoid spurious tabs.

  (face-remap-add-relative asmibm-mode-operations-face
                           :weight 'bold
                           :foreground "red" ; REVEDIT/RPF color.
                           )

  (face-remap-add-relative asmibm-mode-operators-face
			   :weight 'bold
			   :foreground "Forest Green") ; May be too much.

  (face-remap-add-relative asmibm-mode-comment-face
                           :foreground "cyan")
  
  ;; Comments.
  ;; (setq-local comment-start "//\*")
  ;; (setq-local comment-end "")
  ;; (setq-local comment-start-skip
  ;;             "^//[[:graph:]]*[[:blank:]]+[[:graph:]]+[[:blank:]]+")

  ;; Set up the mode keymap.

  (use-local-map asmibm-mode--map)
  (local-set-key (kbd "RET") 'electric-indent-just-newline)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)


  ;; Set up tab stops.

  (setq-local tab-stop-list '(9 15 40))
  
  
  ;; Set up the menus.

  (easy-menu-define asmibm-mode--mainframe-os-menu asmibm-mode--map
    "ASM IBM commands"
    '("ASM IBM OS"
      ["Submit Compilation" asmibm-mode-compile]
      ["Submit Compilation and Go" asmibm-mode-compile-and-go])
    )

  (setq-local imenu-generic-expression
	      (reverse asmibm-mode--imenu-generic-expression))
  (imenu-add-to-menubar "ASM IBM Code")

  ;; Start the IRON MAIN minor mode, which sets up the ruler and the
  ;; "card" editing limits, plus the fill-column indicator.

  (when (fboundp 'iron-main-mode) (iron-main-mode))

  'asmibm-mode
  )


;;; Functions and Commands.

(defvar asmibm-mode--not-implemented-flag t
  "Non-nil means the context where used is still unimplemented.")


;;; asmibm-compile-jcl

(defvar asmibm-mode--compile-jcl
  "//EASMICJ  JOB (%s),'%s',
//            USER=%s,PASSWORD=%s,
//            CLASS=A,
//            MSGCLASS=%s,MSGLEVEL=(1,1)
//EASMICEX EXEC PGM=ASMFC,PARM.ASM='LIST,RENT,NODECK'
//EASMICEX.DD *
"
  "JCL job to compile a ASM IBM code.

This variable is acually a format string.")


(defun asmibm-mode-prepare-job (&optional
			   acct
			   name
			   user
			   password
			   msgclass)
  (format asmibm-mode--compile-jcl
	  (or acct "1")
	  (or name "EMACS COMPILES IRON ASSEMBLER")
	  (or user "HERC01")		; Sensible TK4- default.
	  (or password "CUL8TR")	; Ditto.
	  (or msgclass "H"))
  )


;;; asmibm-mode-compile

(defun asmibm-mode-compile (&optional port)
  "Compiles the buffer by submitting a job to the card reader on PORT."
  (interactive
   (if asmibm-mode--not-implemented-flag
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

(defun asmibm-mode-compile-and-go (&optional port)
  "Compile and execute the buffer on the mainframe.

This is obtained by submitting a job to the card reader on PORT."
  (interactive
   (if asmibm-mode--not-implemented-flag
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
