;;; iron-main-widgets --- Session handling for the IRON MAIN package.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t -*-

;;; Commentary:
;;
;; The IRON MAIN package interacts with a mainframe -- for the time
;; being, mostly a Hercules emulator -- by using a number of "panels",
;; each of which uses the Emacs `widget' library.  This file contains
;; some specialized widgets that "fix" some bugs still present in
;; Emacs 28.x.


;;; Code:

(require 'cl-lib)
(require 'widget)
(require 'wid-edit)


(define-widget 'iron-main-natnum-widget
  'integer ; 'natnum only in recent emacs.
  "A non negative integer"
  
  ;; This is a widget that fixes the wrong functions
  ;; `:value-to-external' in `sexp' and `restricted-sexp' widgets (the
  ;; `numeber' widget must also be fixed).
  ;;
  ;; The library `:value-to-external' functions eventually call
  ;; '(read value)' where `value' is the widget value; alas, `value'
  ;; can be "", causing the traditional minor demons to fly out of the
  ;; nose, as a 'End of file during parsing' error gets signalled.

  :default-value 42			; Used in a limited way FTTB;
					; `:value' should be
					; initialized to it if missing.
  :value-to-external 'iron-main--natnum-value-to-ext
  :help-echo "Enter a non negative integer..."
  :validate-regexp "[0-9]*"
  )


(cl-defun iron-main--natnum-value-to-ext (widget value)
  ;; Lifted, and fixed, from `:value-to-external' in
  ;; `restricted-sexp'.

  (unless (stringp value)
    (display-warning
     'widget-bad-default-value
     (format-message
      "\nA widget of type %S has a bad default value.
value: %S
match function: %S
match-alternatives: %S"
      (widget-type widget)
      value
      (widget-get widget :match)
      (widget-get widget :match-alternatives))
     :warning))
  (if (and (stringp value) (string-equal "" value))
      ;; Oooops, we cannot just `read'.
      (widget-get widget :default-value)
    (read value))
  )


;;;; Epilogue.

(provide 'iron-main-widgets)

;;; iron-main-widgets.el ends here
