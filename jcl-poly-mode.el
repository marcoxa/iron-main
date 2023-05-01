;;; jcl-poly-mode --- A major polymode mode to handle MVS or Z/OS JCL.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; jcl-poly-mode.el
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: April, 4th, 2021
;;
;; Version: 20230501.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; A major polymode mode to handle IBM MVS or z/OS JCL.
;;
;; For the time being only the following languages are supported.
;; * PL/I
;; * Fortran (H)
;; * COBOL
;; * Assembler
;; * C (JCC and GCC)
;;
;; The list is taken from TK4- SYS2.JCLLIB listing. The list is
;; missing Simula and Pascal.  In the future Jay Moseley's language
;; pack may be fully supported, provided that proper modes are
;; available in Emacs for those languages.

;;; Code:

(require 'polymode)

;;;; JCL Poly Mode Setup.

(use-package polymode
  :ensure t
  :mode ("\\.jcl$" . jcl-poly-mode)
  :config

  (define-hostmode jcl-poly-hostmode :mode 'jcl-mode)

  (define-innermode jcl-poly-pl1-innermode
    :mode 'pl1-mode
    :head-matcher "^//PL1L\\.SYSIN +DD +\\*"
    :tail-matcher "^\\(/\\*\\|//\\)"
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode jcl-poly-fortran-innermode
    :mode 'fortran-mode
    :head-matcher "^//FORT\\.SYSIN +DD +\\*"
    :tail-matcher "^\\(/\\*\\|//\\)"
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode jcl-poly-cobol-innermode
    :mode 'cobol-mode
    :head-matcher "^//COB\\.SYSIN +DD +\\*"
    :tail-matcher "^\\(/\\*\\|//\\)"
    :head-mode 'host
    :tail-mode 'host)

  ;; (define-innermode jcl-poly-asmibm-innermode
  ;;   :mode 'asmibm-mode
  ;;   :head-matcher "^//ASM\\.SYSIN +DD +\\*"
  ;;   :tail-matcher "^\\(/\\*\\|//\\)"
  ;;   :head-mode 'host
  ;;   :tail-mode 'host)

  (define-innermode jcl-poly-hlasm-innermode
    :mode 'hlasm-mode
    :head-matcher "^//ASM\\.SYSIN +DD +\\*"
    :tail-matcher "^\\(/\\*\\|//\\)"
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode jcl-poly-c-innermode
    :mode 'c-mode
    ;; The next should accomodate both JCC and GCC.
    :head-matcher "^//COMP\\(ILE\\))*\\.SYSIN +DD +\\*"
    :tail-matcher "^\\(/\\*\\|//\\)"
    :head-mode 'host
    :tail-mode 'host)

  (define-polymode jcl-poly-mode
    :hostmode 'jcl-poly-hostmode
    :innermodes '(jcl-poly-pl1-innermode
		  jcl-poly-fortran-innermode
		  jcl-poly-cobol-innermode
		  ;; jcl-poly-asmibm-innermode
		  jcl-poly-hlasm-innermode
		  jcl-poly-c-innermode
		  )
    ))


;;; Epilogue.

(provide 'jcl-poly-mode)

;;; jcl-poly-mode.el ends here
