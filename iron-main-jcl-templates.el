;;; iron-main-jcl-templates --- JCL templates used by IRON MAIN.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; iron-main-jcl-templates.el
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: December 17th, 2020.
;;
;; Version: 20201206.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; JCL templates used by `iron-main-mode'.
;; A minor mode to edit files and interact with IBM MVS or z/OS.
;;
;; The following are functions and data structures to handle the
;; simple JCL templates used to allocate and save files in datasets the
;; mainframe.
;;
;; To Do:
;;
;; JCL generation.

;;; Code:

;;;; IRON MAIN Mode JCL Templates.

(require 'cl-lib)

;; Allocation template.

(defvar iron-main-jcl-job-template
  "//* -*- Mode: JCL -*-
//* IRON MAIN Emacs generated job.
//EMJ%d    JOB (EMACS),'%s'
//            CLASS=A,
//            MSGCLASS=%s,
//            MSGLEVEL=(1,1)
//*
"
  "Template for Emacs Job."
  )

(defun iron-main-jcl-job (job-number programmer &optional msgclass)
  "Generate the Emacs Job Header.

The values for JOB-NUMBER and PROGRAMMER are spliced in as is the
MSGCLASS (this is optional and defaults to 'H'."
  (unless msgclass
    (setq msgclass "H"))
  
  (format iron-main-jcl-job-template job-number programmer msgclass))


(defvar iron-main-jcl-exist-step-template
  "//* IRON MAIN Emacs generated step.
//* Check if dataset exists.
//*
//EMDSEXIS EXEC PGM=IDCAMS
//SYSPRINT   DD DUMMY
//SYSIN      DD *
  LISTCAT ENTRIES('%s')
/*
"
  "Template for Emacs dataset existence check job step."
  )


(defun iron-main-jcl-exists-step (ds-rep)
  "Generate the Emacs 'exists'step given a dataset rep DS-REP."
  (format iron-main-jcl-exist-step-template
	  (string-trim (iron-main-ds-rep-name ds-rep) "'" "'")))
  


(defvar iron-main-jcl-allocate-step-template
  "//* IRON MAIN Emacs generated step.
//* Create dataset.
//*
//EMDSCREA EXEC PGM=IEFBR14%s
//SYSPRINT   DD SYSOUT=*
//NEW        DD DSN=%s,DISP=(NEW,CATLG),
//           LRECL=%d,RECFM=%s,
//           SPACE=(%s,(%d,%d,%d)),
//           DSORG=%s
"
  "Template for Emacs dataset allocation job step."
  )


(defun iron-main-jcl-allocate-step (ds-rep &optional cond)
  "Generate the Emacs 'allocation' step given a dataset rep DS-REP.

The optional COND defaults to the empty string, otherwise it should be
a condition that could prevent the step from running, possibly
referring to previous steps."

  (unless cond
    (setq cond ""))
  
  (format iron-main-jcl-allocate-step-template
	  cond
	  (iron-main-ds-rep-name ds-rep)
	  (iron-main-ds-rep-lrecl ds-rep)
	  (iron-main-ds-rep-recfm ds-rep)
	  (iron-main-ds-rep-space-unit ds-rep)
	  (iron-main-ds-rep-primary ds-rep)
	  (iron-main-ds-rep-secondary ds-rep)
	  (iron-main-ds-rep-directory ds-rep)
	  (iron-main-ds-rep-dsorg ds-rep)
	  ))


(defvar iron-main-jcl-delete-step-template
  "//* IRON MAIN Emacs generated step.
//* Delete dataset.
//*
//EMDSDEL EXEC PGM=IEFBR14%s
//SYSPRINT  DD SYSOUT=*
//DEL       DD DSN=%s,DISP=(OLD,DELETE,DELETE)
"
  "Template for Emacs dataset deletion job step."
  )


(defun iron-main-jcl-delete-step (ds-rep &optional cond)
  "Generate the Emacs 'delet' step given a dataset rep DS-REP.

The optional COND defaults to the empty string, otherwise it should be
a condition that could prevent the step from running, possibly
referring to previous steps."

  (unless cond
    (setq cond ""))
  
  (format iron-main-jcl-delete-step-template
	  cond
	  (iron-main-ds-rep-name ds-rep)
	  ))


;;;; Epilogue
;;;; ========

(provide 'iron-main-jcl-templates)

;;; iron-main-jcl-templates.el ends here
