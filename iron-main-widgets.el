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
(require 'iron-main-vars)
(require 'widget)
(require 'wid-edit)


(define-widget 'widget-non-negative-integer 'integer ; 'natnum only in recent emacs.
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
  :value-to-external 'widget--non-negative-integer-value-to-ext
  :help-echo "Enter a non negative integer..."
  :validate-regexp "[0-9]*"
  )


;;;; Epilogue.

(provide 'iron-main-widgets)

;;; iron-main-widgets.el ends here
