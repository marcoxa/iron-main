;;; iron-main-mode --- A major mode to handle MVS or Z/OS JCL.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; iron-main-mode.el
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: December 2nd, 2020.
;;
;; Version: 20221014.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; A minor mode to edit files and interact with IBM MVS or z/OS.
;;
;; This is a minor mode just because it is a good thing to have a
;; shared location where to centralize definitions for the main modes
;; and functionalities that make up this package.  E.g., see the major
;; modes `jcl-mode' and `asmibm-mode'
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
;; Packaging still a bit "in fiery"
;; Polymode would be very nice to have.
;; Some extra fixes to the font-lock machinery are a must.

;;; Code:

;;;; IRON MAIN Mode Setup.

(require 'cl-lib)
(require 'ruler-mode)
(require 'iron-main-vars)
(require 'iron-main-ruler-function)
(require 'iron-main-utils)
(require 'iron-main-panels)

;; (load (expand-file-name "iron-main-ruler-function" iron-main-path))


;;; iron-main-mode

(define-minor-mode iron-main-mode
  "A minor mode to edit files and interact with IBM MVS or z/OS."

  :init-value nil
  :lighter "//IRON-MAIN"

  :group 'iron-main
  :keymap (let ((km (make-sparse-keymap)))
	    (set-keymap-parent km prog-mode-map) ; Inherit from prog-mode-map!
	    km)

  ;; Columns and Vertical line at column 72.
  ;; JCL cards start at column 1.
  (column-number-mode)
  (setq-local column-number-indicator-zero-based nil)
  
  ;; (fci-mode 42)
  ;; (setq-local fci-rule-column 72)
  ;; (setq fci-rule-width 24)

  (when (fboundp 'display-fill-column-indicator-mode)
    ;; May not be loaded in older versions of Emacs.
    
    (display-fill-column-indicator-mode)
    (setq-local display-fill-column-indicator t)
    (setq-local display-fill-column-indicator-column 72))

  ;; Always start ruler.
  ;; Unless we are in an older EMacs which may break because it is
  ;; missing the function ruler-mode-text-scaled-window-width.
  
  (when (fboundp 'ruler-mode-text-scaled-window-width)
    (ruler-mode)
    (if (fboundp 'iron-main-ruler-function)
        (setq-local ruler-mode-ruler-function
                    'iron-main-ruler-function)
      (warn "IRON MAIN: specialized ruler builder undefined.")
      ))
  )


;;;; Epilogue
;;;; ========

(provide 'iron-main-mode)

;;; iron-main-mode.el ends here
