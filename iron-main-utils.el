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
;; Version: 20230831.1
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
(require 'url)
(require 'iron-main-vars)


;;;; Mainframe setup.

(defun iron-main-running-os (os-flavor)
  "Check whether IRON MAIN is running OS-FLAVOR."
  (string-prefix-p os-flavor iron-main-os-flavor))


(defun iron-main-running-machine (machine)
  "Check whether IRON MAIN is running MACHINE."
  (string-prefix-p machine iron-main-machine))


(defun iron-main-running-on (os-flavor machine)
  "Check whether IRON MAIN is running OS-FLAVOR on MACHINE."
  (and (string-prefix-p os-flavor iron-main-os-flavor)
       (string-prefix-p machine iron-main-machine)))


;;;; Datasets

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
  ;; (declare (type string ds))
  (let ((dsname (cl-first (split-string ds "'" t " +")))
	;; The above is a kludge to remove the quotes.
	)
    
    (when (> (length dsname) 44)	; Make this a constant.
      (error "IMU01E: DSNAME '%s' is too long" dsname))

    (let* ((quals-member (split-string dsname "[()]" nil " +")) ; Ok. Kludge.
	   (quals-string (cl-first quals-member))
	   (member-name (cl-second quals-member))
	   (quals (and quals-string
		       (split-string (cl-first quals-member) "\\." nil)))
	   )
      (when (zerop (length quals-string))
	(error "IMU02E: DSNAME is empty"))

      (when (equal "." quals-string)
	(error "IMU03E: DSNAME '%'s is invalid" quals-string))

      (when (member "" quals)
	;; Found a null string in quals; this means that we had two
	;; dots `.' next to each other, which is an error.
	(error "IMU04E: DSNAME '%s' cannot contain an empty qualifier"
	       dsname))

      (when (cl-some (lambda (q) (> (length q) 8)) quals)
	(error "IMU05E: DSNAME '%s' cannot contain a qualifier longer than 8 characters"
	       dsname))

      (when (and member-name (zerop (length member-name)))
	(error "IMU06E: DSNAME '%s' cannot have the member long 0 characters"
	       dsname))
      
      (when (and member-name (> (length member-name) 8))
	(error "IMU07E: DSNAME '%s' cannot have the member longer that 8 characters"
	       dsname))

      ;; All is fine.
      t
      )))


(defun iron-main-check-dsorg-and-dir (dsorg dir-blocks)
  "Check compatibility of DSORG and DIR-BLOCKS."
  (when (and (equal dsorg "PO") (zerop dir-blocks))
    (error "IMU08E: DS REP cannot have \"PO\" DSORG and 0 directory blocks"))
  ;; More checks later.
  t
  )


;;;; Host machine pathnames.

(defun iron-main--split-pathname (pathname)
  "Splits a pathname (Un*x or Windows) on the directory separators."
  (if (stringp pathname)
      (split-string pathname "[\\/]" t)
    (error "IMU09E: PATHNAME %s is not a string" pathname)))


(cl-defun iron-main--join-by (sl &optional (sep "") enclose)
  "Joins strings.

The function takes a list of objects (SL) and PRINCs them to a string
that is eventually returned.  The separator SEP (a string) is used
between each element of the list SL.

ENCLOSE can be T, the keyword :LEFT, :RIGHT, or :BOTH.  If T or :BOTH,
SEP is prepended and postpended (always by PRINC) to the result; if
:RIGHT only at the end, and if :LEFT only at the beginning.

Notes:

This function can be used to build strings that represent pathnames
starting from a list of strings.
"
  (with-output-to-string
    (when (and enclose (member enclose '(t :left :both)))
      (princ sep))
    (princ (cl-first sl))
    (dolist (s (cl-rest sl))
      (princ (format "%s%s" sep s)))
    (when (and enclose (member enclose '(t :right :both)))
      (princ sep))
    ))


(cl-defun iron-main--shorten-pathname (pathname-string
				       &optional (last-n 3))
  (let* ((pname-strings (iron-main--split-pathname pathname-string))
	 (last-pname-strings (last pname-strings last-n))
	 (short-pname
	  (iron-main--join-by last-pname-strings "/" :left))
	 )
    (if (> (length pname-strings) (length last-pname-strings))
	(cl-concatenate 'string "..." short-pname)
      short-pname)))


;;;; Hercules interface.
;;;; This may end up in a different file.

(defun iron-main-hercules-set-dasd-dir (dasd-dir)
  "Set the `iron-main-dasd-dir' to DASD-DIR."
  (if (file-directory-p dasd-dir)
      (setq-local iron-main-hercules-dasd-dir dasd-dir)
    (error "IMU10E: Cannot set the DASD folder to %s; not a directory"
	   dasd-dir)))


;; (defun iron-main-hercules-call-utility (utility output &rest args)
;;   "Call the Hercules utility program UTILITY with ARGS.

;; If OUTPUT is NIL the output of the utility is collected into a
;; string.  If OUTPUT is the atom `list' then the output of the utility
;; is collected into a list of lines."

;;   (let ((output-lines (apply 'process-lines utility args)))
;;     (cl-case output
;;       ((nil) (apply 'concat output-lines))
;;       (list output-lines)
;;       ;; ...
;;       )))


(cl-defun iron-main-hercules-is-listening (&optional
					   (herc-host
					    iron-main-hercules-http-host)
					   (herc-port
					    iron-main-hercules-http-port))
  "Check whether there is a HTTP-enabled Hercules instance running.

The Hercules instance should be running on HERC-HOST listening on
HERC-PORT.  HERC-HOST defaults to `iron-main-hercules-http-host',
while HERC-PORT defaults to `iron-main-hercules-http-port'.

The function returns a non NIL value if there is such an instance
running.

Notes:

The current value returned in the case a Hercules instance is
listening is a list with the network process as first element.  The
`process-status' of this process will turn to \\='closed\\='; therefore it
cannot be relied upon for anthing other than this check."

  (let* ((herc-string-url
	  (concat "http://" herc-host ":" (format "%s" herc-port)))
	 (herc-url
	  (url-generic-parse-url herc-string-url)) ; Maybe useless.
	 (url-current-object herc-url)	; Being paranoid, given the
					; stupidity of this variable.
	 )
    (message "IMHE000I: trying to connect to %S" herc-string-url)
    (condition-case e
	(let ((c (make-network-process
		  :name (format "IMHEHTTP test %S:%S"
				herc-host
				herc-port)
		  :host (url-host herc-url)
		  :service (url-port herc-url)
		  :server nil
		  ))
	      )
	  (when c
	    (prog1
		(list c (process-status c))
	      (delete-process c))))
      (file-error
       (message "IMHE000W: failed with %S %S" (cl-first e) (cl-second e))
       nil)
      (error
       (message "IMHE001W: %S %S" (cl-first e) (cl-second e))
       nil)
      )))


(cl-defun iron-main-hercules-cmd (cmd
				  &key
				  (host
				   iron-main-hercules-http-host)
				  (port
				   iron-main-hercules-http-port)
				  (timeout 15)
				  (check-listening nil)
				  )
  "Issue a command CMD to a running Hercules instance.

The command is issued to the HTTP-enabled Hercules instance running on
HOST and listening on PORT.  If the the Hercules instance responds,
then the buffer containing the response is returned, NIL otherwise.

If CHECK-LISTENING is non null, then the
test `iron-main-hercules-is-listening' is run; if its result is NIL
then the command is not issued.

The command is issued synchronously with a TIMEOUT (cfr.,
`url-retrieve-synchronously')."

  (when (and check-listening
	     (not (iron-main-hercules-is-listening host port)))
    (message "IMHE002W: no Hercules connection; command %S not issued." cmd)
    (cl-return-from iron-main-hercules-cmd nil))
  
  ;; The following CGI url was desumed from Hercules HTTP
  ;; implementation.
  
  (let* ((cmd-urlified
	  (replace-regexp-in-string " " "%20" cmd))
	 (cmd-url
	  (format "http://%s:%s/cgi-bin/tasks/cmd?cmd=%s"
		  host
		  port
		  cmd-urlified))
	 )
    (ignore-errors
      (url-retrieve-synchronously cmd-url nil nil timeout)))
  )


;;;; Epilogue
;;;; ========

(provide 'iron-main-utils)

;;; iron-main-utils.el ends here
