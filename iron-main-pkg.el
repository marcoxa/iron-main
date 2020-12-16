;;; iron-main --- EMACS works the Iron on the (IBM) Mainframe.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; iron-mode.el
;;
;; See the file COPYING license and copyright information.
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
;; The package definition of the "IRON MAIN" collection of modes and
;; tools.

;;; Code:

(define-package "IRON MAIN" "20201206.1"
  "The Iron Main package.

The Iron Main package is a collection of modes and code to allow
Emacs to edit and interface (minimally) with a IBM MVS or z/OS
instance; mostly assuming that they are on the free Hercules
emulator."
  )


(provide 'iron-main)

;;; iron-main-pkg.el ends here.
