;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; iron-main --- Facilities to handle "Iron" (IBM MVS or z/OS).

;;; iron-main.el
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: December 5th, 2020.
;;
;; Version: 20210405.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; A set of minor and major modes to edit files and interact with
;; "Iron", i.e. IBM MVS or z/OS.
;;
;; This is the main file of the package which taked care of loading
;; all the other bits and pieces, minor and major modes necessary for
;; its functioning.  E.g., see the major modes `jcl-mode' and
;; `asmibm-mode'.
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


;;; Code:

;;;; IRON MAIN Mode Setup.

(defvar iron-main-path (file-name-directory load-file-name)
  "The location the IRON MAIN library is loaded from.")

(defvar iron-main-path-tests
  (expand-file-name "tests/" iron-main-path)
  "The location of the IRON MAIN library \"tests\" folder.")


;; The order of the require and load calls is relevant.

(require 'ruler-mode)


;; Mainframe interaction files.

(load (expand-file-name "iron-main-vars" iron-main-path))
(load (expand-file-name "iron-main-ruler-function" iron-main-path))
(load (expand-file-name "iron-main-utils" iron-main-path))
(load (expand-file-name "iron-main-session" iron-main-path))
(load (expand-file-name "iron-main-panels" iron-main-path))
(load (expand-file-name "iron-main-hercules-cmds" iron-main-path))
(load (expand-file-name "iron-main-mode" iron-main-path))


;; Language mode files.

(load (expand-file-name "asmibm-mode" iron-main-path))
(load (expand-file-name "pl1-mode" iron-main-path))
(load (expand-file-name "jcl-mode" iron-main-path))
(load (expand-file-name "jcl-poly-mode" iron-main-path))


;;;; Epilogue
;;;; ========

(byte-recompile-directory iron-main-path 0)

(provide 'iron-main)

;;; iron-main.el ends here
