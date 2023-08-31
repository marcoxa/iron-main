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
;; Version: 20230830.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; JCL templates used by `iron-main-mode'.
;;
;; The following are functions and data structures to handle the
;; simple JCL templates used to allocate and save files in datasets the
;; mainframe.
;;
;; Functions and variables are prefixed with "iron-main-jcl-tmpl--".
;;
;; To Do:
;;
;; JCL generation.

;;; Code:

;;;; IRON MAIN Mode JCL Templates.


(require 'cl-lib)

;; Allocation template.

(defvar iron-main-jcl-tmpl--job
  "//* -*- Mode: JCL -*-
//* IRON MAIN Emacs generated job - %s.
//* %s
//*
//EMIM%d   JOB (EMACS),'%s'
//            CLASS=A,
//            MSGCLASS=%s,
//            MSGLEVEL=(1,1)
//*
"
  "Template for Emacs Job."
  )

(cl-defun iron-main-jcl-tmpl--make-job (&key
					 (job-number 42)
					 (programmer "MYSELF")
					 (msgclass "H")
					 (header-line "")
					 )
						    
  "Generate the Emacs Job Header.

The values for JOB-NUMBER and PROGRAMMER are spliced in as is the
MSGCLASS (this is optional and defaults to 'H'."
  
  (format iron-main-jcl-tmpl--job
	  (current-time-string)
	  header-line
	  job-number
	  programmer
	  msgclass))


(defvar iron-main-jcl-tmpl--exist-step
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


(cl-defun iron-main-jcl-tmpl--make-exists-step (ds-rep)
  "Generate the Emacs `exists' step given a dataset rep DS-REP."
  (format iron-main-jcl-tmpl--exist-step
	  (string-trim (iron-main-ds-rep-name ds-rep) "'" "'")))
  


(defvar iron-main-jcl-tmpl--allocate-step
  "//* IRON MAIN Emacs generated step.
//* Create dataset.
//*
//EMDSCREA EXEC PGM=IEFBR14%s
//SYSPRINT   DD SYSOUT=*
//NEWDS      DD DSN=%s,DISP=(NEW,CATLG),
//           LRECL=%d,RECFM=%s,
//           SPACE=(%s,(%d,%d,%d)),
//           DSORG=%s
"
  "Template for Emacs dataset allocation job step."
  )



(defun iron-main-jcl-tmpl--make-allocate-step (ds-rep &optional cond)
  "Generate the Emacs `allocation' step given a dataset rep DS-REP.

The optional COND defaults to the empty string, otherwise it should be
a condition that could prevent the step from running, possibly
referring to previous steps."

  (unless cond
    (setq cond ""))
  
  (format iron-main-jcl-tmpl--allocate-step
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


(defvar iron-main-jcl-tmpl--delete-step
  "//* IRON MAIN Emacs generated step.
//* Delete dataset.
//*
//EMDSDEL EXEC PGM=IEFBR14%s
//SYSPRINT  DD SYSOUT=*
//DELDS     DD DSN=%s,DISP=(OLD,DELETE,DELETE)
"
  "Template for Emacs dataset deletion job step."
  )


(defun iron-main-jcl-delete-step (ds-rep &optional cond)
  "Generate the Emacs `delete' step given a dataset rep DS-REP.

The optional COND defaults to the empty string, otherwise it should be
a condition that could prevent the step from running, possibly
referring to previous steps."

  (unless cond
    (setq cond ""))
  
  (format iron-main-jcl-tmpl--delete-step
	  cond
	  (iron-main-ds-rep-name ds-rep)
	  ))


;; Buffer generation.

;; Just temporary...

(cl-defun iron-main-jcl-tmpl--instantiate (buffer-or-name
					   jcl-template-fun
					   &rest
					   args)
  
  (switch-to-buffer buffer-or-name)

  (kill-all-local-variables)

  (erase-buffer)

  (insert
   (apply jcl-template-fun args))

  (jcl-poly-mode)
  )


;; Allocation JCL

(cl-defun iron-main-jcl-tmpl--allocation-job
    (buffer-or-name
     &aux
     (dsrep
      (make-iron-main-ds-rep :name "FOOO.BAR.BAZ")
      ))
    
  (switch-to-buffer buffer-or-name)

  (kill-all-local-variables)

  (erase-buffer)

  (insert
   (iron-main-jcl-tmpl--make-job
    :header-line "Dataset Allocation Job.")
   (iron-main-jcl-tmpl--make-exists-step dsrep)
   (iron-main-jcl-tmpl--make-allocate-step dsrep ",COND(8)")
   "//\n"
   "//* End IRON MAIN Emacs generated job.\n"
   )

  (jcl-poly-mode)
  )



;;;; Epilogue
;;;; ========

(provide 'iron-main-jcl-templates)

;;; iron-main-jcl-templates.el ends here
