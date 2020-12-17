;;; iron-main-utils --- A major mode to handle MVS or Z/OS JCL.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; iron-main-utils.el
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
;; Utilities in support of `iron-main-mode'.
;; A minor mode to edit files and interact with IBM MVS or z/OS.
;;
;; These are mostly data structures and functions to handle dataset
;; representations.
;;
;; To Do:
;;
;; JCL generation.

;;; Code:

;;;; IRON MAIN Mode utilities functions.

(require 'cl-lib)

(cl-defstruct (iron-main-dataset-representation
	       (:constructor make-iron-main-ds-rep)
	       (:conc-name iron-main-ds-rep-))
  "The IRON MAIN dataset representation."
  (name "")
  (recfm "FB" :type string)		; From Rob Prin's RPE.
  (lrecl 80 :type integer)		; From Rob Prin's RPE.
  (blksize 3120 :type integer)		; From Rob Prin's RPE.
  (dsorg "PS" :type string)

  
  (unit "SYSDA" :type string)
  (vol "" :type string)
  (space-unit "TRK" :type string)
  (primary 1 :type integer)
  (secondary 0 :type string)
  (directory 0 :type string)
  ;; ..
  )


(defun iron-main-ds-to-string (ds)
  "Convert a `iron-main-dataset-representation' object DS to a string."
  (format
   "DSN=%s,RECFM=%s,LRECL=%s,BLKSIZE=%s,DSORG=%s,UNIT=%s,VOL=%s,SPACE=(%s,(%s,%s,%s))"
   (iron-main-ds-rep-name ds)

   (iron-main-ds-rep-recfm ds)
   (iron-main-ds-rep-lrecl ds)
   (iron-main-ds-rep-blksize ds)
   (iron-main-ds-rep-dsorg ds)

   (iron-main-ds-rep-unit ds)
   (iron-main-ds-rep-vol ds)
   
   (iron-main-ds-rep-space-unit ds)
   
   (iron-main-ds-rep-primary ds)
   (iron-main-ds-rep-secondary ds)
   (iron-main-ds-rep-directory ds)
   ))


(defun iron-main-check-ds-rep (ds)
  "Check consistency of dataset representation DS.

The function runs a number of checks ensuring that a dataset
representation is properly set up.  At a minimum it checks the lenght
of the dsname and its breakdown in high, intermediate and low
qualifiers; it also checks that a sequential (PS) dataset
organization (DSORG) has 0 directory blocks (DIR) or a positive number
if DSORG denotes a partioned dataset (PDS).

Notes:

More tests will be added in the future."
  (and (iron-main-check-dsname (iron-main-ds-rep-name ds))
       (iron-main-check-dsorg-and-dir (iron-main-ds-rep-dsorg ds)
				      (iron-main-ds-rep-directory ds))
       ))


(defun iron-main-check-dsname (ds)
  "Check the constraints of the name of DS (the 'DSNAME')."
  (declare (type string ds))
  (let ((dsname (cl-first (split-string ds "'" t " +")))
	;; The above is a kludge to remove the quotes.
	)
    
    (when (> (length dsname) 44)	; Make this a constant.
      (error "DSNAME '%s' is too long" dsname))

    (let* ((quals-member (split-string dsname "[()]" nil " +")) ; Ok. Kludge.
	   (quals-string (cl-first quals-member))
	   (member-name (cl-second quals-member))
	   (quals (and quals-string
		       (split-string (cl-first quals-member) "\\." nil)))
	   )
      (when (zerop (length quals-string))
	(error "DSNAME is empty"))

      (when (equal "." quals-string)
	(error "DSNAME '%'s is invalid" quals-string))

      (when (member "" quals)
	;; Found a null string in quals; this means that we had two
	;; dots `.' next to each other, which is an error.
	(error "DSNAME '%s' cannot contain an empty qualifier"
	       dsname))

      (when (cl-some (lambda (q) (> (length q) 8)) quals)
	(error "DSNAME '%s' cannot contain a qualifier longer than 8 characters"
	       dsname))

      (when (and member-name (zerop (length member-name)))
	(error "DSNAME '%s' cannot have the member long 0 characters"
	       dsname))
      
      (when (and member-name (> (length member-name) 8))
	(error "DSNAME '%s' cannot have the member longer that 8 characters"
	       dsname))

      ;; All is fine.
      t
      )))


(defun iron-main-check-dsorg-and-dir (dsorg dir-blocks)
  "Check compatibility of DSORG and DIR-BLOCKS."
  (when (and (equal dsorg "PO") (zerop dir-blocks))
    (error "DS REP cannot have \"PO\" DSORG and 0 directory blocks"))
  ;; More checks later.
  t
  )


;;;; Epilogue
;;;; ========

(provide 'iron-main-utils)

;;; iron-main-utils.el ends here
