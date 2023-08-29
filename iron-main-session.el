;;; iron-main-session --- Session handling for the IRON MAIN package.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t -*-

;;; Commentary:
;;
;; The IRON MAIN package interacts with a mainframe -- for the time
;; being, mostly a Hercules emulator -- by using a number of "panels",
;; i.e., a number of buffers that must share some "session"
;; information.
;;
;; This file contans code to handle such sessions.

;;; Code:

(require 'cl-lib)
(require 'iron-main-vars)


;;; iron-main-session

(cl-defstruct (iron-main-session)
  
  "The structure that contains the session data for IRON MAIN."
  
  (machine iron-main-machine)
  (os-flavor iron-main-os-flavor)
  (id (gensym "iron-main-session#") :read-only t)
  )


;;; iron-main-hercules-session

(cl-defstruct (iron-main-hercules-session
	       (:conc-name iron-main-hs-) ; Let's shorten the names.
	       (:include iron-main-session
			 (machine "Hercules")
			 (os-flavor "MVS 3.8j"))
	       )
  "IRON MAIN session data for Hercules connection."
  
  (version iron-main-hercules-version :read-only t)
  (http-host iron-main-hercules-http-host)
  (port iron-main-hercules-http-port)
  (pid nil)
  (os-dir iron-main-hercules-os-dir)
  (dasd-dir iron-main-hercules-dasd-dir)
  (card-reader-port iron-main-hercules-card-reader-port)
  (user "HERC01" :type string)
  (passwd "CUL8ER")
  )


;;; iron-main-ibm-session
;; Just a placeholder for the time being.

(cl-defstruct (iron-main-ibm-session
	       (:conc-name iron-main-is-)
	       (:include iron-main-session
			 (machine "IBM z")
			 (os-flavor "z/OS"))
	       )
  "IRON MAIN session data for connections to IBM mainframes."
  )

;;; iron-main-mts-session
;; Just a placeholder for the time being.

(cl-defstruct (iron-main-mts-session
	       (:conc-name iron-main-mts-)
	       (:include iron-main-hercules-session
			 (machine "Hercules")
			 (os-flavor "MTS"))
	       )
  "IRON MAIN session data for connections to MTS running on Hercules."
  )


;;; iron-main-sessions
;; Global variable.

(defvar iron-main-sessions ()
  "The data structure containing the IRON MAIN sessions.")


(defun iron-main-session-add (session)
  "Add an IRON MAIN SESSION to the data structure `iron-main-sessions'."
  (push session iron-main-sessions))


(defun iron-main-session-find (session-id)
  "Find an IRON MAIN session in the data structure `iron-main-sessions'.

The search is done by means od the symbol SESSION-ID."
  (cl-find session-id iron-main-sessions
	   :key 'iron-main-session-id))


(defun iron-main-session-delete (session)
  "Deletes a SESSION from the data structure `iron-main-sessions'.

The argument SESSION can be either a `iron-main-session' or a symbol,
in which case it is compared with the slot `id' of the stored
sessions."
  
  (cl-etypecase session
    (iron-main-session
     (setf iron-main-sessions
	   (delete session iron-main-sessions)))
    (symbol
     (setf iron-main-sessions
	   (cl-delete session iron-main-sessions
		      :key 'iron-main-session-id)))
    ))


(cl-defun iron-main-session-start
    (&optional
     (session-type 'iron-main-hercules-session))
  "Start a new IRON MAIN session.

The new session type is determined by the parameter SESSION-TYPE.  The
new session is added to the data structure `ireon-main-sessions'."

  (let ((new-session
	 (cl-ecase session-type
	   (iron-main-hercules-session
	    (make-iron-main-hercules-session))
	   (iron-main-ibm-session
	    (make-iron-main-ibm-session))
	   ))
	)
    (iron-main-session-add new-session)
    new-session))


;;;; Epilogue.

(provide 'iron-main-session)

;;; iron-main-session.el ends here
