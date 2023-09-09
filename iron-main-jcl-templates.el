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
(require 'iron-main-session)

;; Allocation template.

(defvar iron-main-jcl-tmpl--job
  "//* -*- Mode: JCL -*-
//* IRON MAIN Emacs generated job - %s.
//* %s
//*
//EMIM%d   JOB (EMACS),'%s'
//            CLASS=A,
//            MSGCLASS=%s,
//            MSGLEVEL=(1,1),
//            USER='%s',
//            PASSWORD='%s'
//*
"
  "Template for Emacs Job."
  )

(cl-defun iron-main-jcl-tmpl--make-job (&key
					 (job-number 42)
					 (programmer "MYSELF")
					 (user "LUSER")
					 (password "PASSWORD")
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
	  msgclass
	  user
	  password
	  ))


(defvar iron-main-jcl-tmpl--exist-step
  "//* IRON MAIN Emacs generated step.
//* Check if dataset exists.
//*
//EXISTCHK EXEC PGM=IEFBR14
//EXISTDSN   DD DSN='%s',
//              DISP=(OLD,KEEP)
//* CC should be non zero if DSN does not exist.
//*
"
  "Template for Emacs dataset existence check job step."
  )


(cl-defun iron-main-jcl-tmpl--make-exists-step (ds-rep)
  "Generate the Emacs `exists' step given a dataset rep DS-REP."
  (format iron-main-jcl-tmpl--exist-step
	  (upcase
	   (string-trim (iron-main-ds-rep-name ds-rep) "'" "'"))))
  


(defvar iron-main-jcl-tmpl--allocate-step
  "//* IRON MAIN Emacs generated step.
//* Create dataset.
//*
//EMDSCREA EXEC PGM=IEFBR14%s
//SYSPRINT   DD SYSOUT=*
//NEWDS      DD DSN='%s',DISP=(NEW,CATLG),
//              LRECL=%d,RECFM=%s,
//              SPACE=(%s,(%d,%d,%d)),
//              DSORG=%s,
//              VOL=%s,
//              UNIT=%s
"
  "Template for Emacs dataset allocation job step."
  )



(cl-defun iron-main-jcl-tmpl--make-allocate-step (ds-rep
						  &optional (cond ""))
  "Generate the Emacs `allocation' step given a dataset rep DS-REP.

The optional COND defaults to the empty string, otherwise it should be
a condition that could prevent the step from running, possibly
referring to previous steps."

  (let ((dsname (upcase (iron-main-ds-rep-name ds-rep)))
	(lrecl (iron-main-ds-rep-lrecl ds-rep))
	(recfm (iron-main-ds-rep-recfm ds-rep))
	(space-unit (iron-main-ds-rep-space-unit ds-rep))
	(primary (iron-main-ds-rep-primary ds-rep))
	(secondary (iron-main-ds-rep-secondary ds-rep))
	(directory (iron-main-ds-rep-directory ds-rep))
	(dsorg (iron-main-ds-rep-dsorg ds-rep))
	(vol (iron-main-ds-rep-vol ds-rep))
	(unit (iron-main-ds-rep-unit ds-rep))
	)

    
    (when (string= "" dsname)
      (error "IMJT1E: empty dsname in allocation step"))
    
    (when (string= "" vol)
      (error "IMJT2E: empty volume in allocation step"))
  
    (format iron-main-jcl-tmpl--allocate-step
	    cond
	    dsname
	    lrecl
	    recfm
	    space-unit
	    primary
	    secondary
	    directory
	    dsorg
	    vol
	    unit
	    )))


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


(cl-defun iron-main-jcl-delete-step (ds-rep &optional (cond ""))
  "Generate the Emacs `delete' step given a dataset rep DS-REP.

The optional COND defaults to the empty string, otherwise it should be
a condition that could prevent the step from running, possibly
referring to previous steps."
  
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


(cl-defun iron-main-jcl-tmpl--show (buffer-or-name
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
;;
;; Notes:
;; I could, actually should, be smarter about reusing the buffer (and
;; getting rid of it).

(cl-defun iron-main-jcl-tmpl--make-alloc-job-buffer
    (&key
     (user "LUSER")
     (password "PASSWORD")
     (dsrep
      (make-iron-main-ds-rep :name "FOOO.BAR.BAZ")
      )
     &aux
     (alloc-job-buffer
      (get-buffer-create
       (format "*IRON MAIN Alloc Job for %s"
	       (iron-main-ds-rep-name dsrep))))
     )

  (with-current-buffer alloc-job-buffer
    (kill-all-local-variables)

    (erase-buffer)

    (insert
     (iron-main-jcl-tmpl--make-job
      :user user
      :password password
      :header-line "Dataset Allocation Job.")
     (iron-main-jcl-tmpl--make-exists-step dsrep)
     (iron-main-jcl-tmpl--make-allocate-step dsrep
					     ",COND(NE,0,EXISTCHK)")
     "//\n"
     "//* End IRON MAIN Emacs generated job.\n"
     )

    (jcl-poly-mode)
    )
  alloc-job-buffer
  )


(cl-defun iron-main-jcl-tmpl--allocation-job
    (session
     &key
     (dsrep
      (make-iron-main-ds-rep :name "FOOO.BAR.BAZ"))
     (user (iron-main-session-user session))
     (pw (iron-main-session-passwd session))
     
     &aux
     (dsname (iron-main-ds-rep-name dsrep))
     (alloc-job-buffer nil)
     )

  (when (string= "" dsname)
    (error "IMJT01E: empty dataset name for allocation job."))
    
  (setq alloc-job-buffer
	(iron-main-jcl-tmpl--make-alloc-job-buffer
	 :user user
	 :password pw
	 :dsrep dsrep))

  (if alloc-job-buffer
      (switch-to-buffer alloc-job-buffer)
    (error "IMJT02E: could not create buffer for allocation job."))
  )


(cl-defun iron-main-jcl-tmpl--run-allocation-job
    (session
     &key
     (dsrep
      (make-iron-main-ds-rep :name "FOOO.BAR.BAZ"))
     (user (iron-main-session-user session))
     (pw (iron-main-session-passwd session))
     
     &aux
     (dsname (iron-main-ds-rep-name dsrep))
     (alloc-job-buffer nil)
     )

  (when (string= "" dsname)
    (error "IMJT01E: empty dataset name for allocation job."))
    
  (setq alloc-job-buffer
	(iron-main-jcl-tmpl--make-alloc-job-buffer
	 :user user
	 :password pw
	 :dsrep dsrep))

  (if alloc-job-buffer
      (progn
	(message "IMJT01I: we should run %s, but let's just show it."
		 alloc-job-buffer)
	(switch-to-buffer alloc-job-buffer)
	)
    (error "IMJT02E: could not create buffer for allocation job."))
  )



;;;; Epilogue
;;;; ========

(provide 'iron-main-jcl-templates)

;;; iron-main-jcl-templates.el ends here
