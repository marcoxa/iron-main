;;; iron-main-hercules-cmds --- A major mode to handle MVS or Z/OS JCL.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; iron-main-hercules-cmds.el
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: December 17th, 2020.
;;
;; Version: 20201229.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; Commands that can be issued to a running Hercules instance.
;; Cfr., `iron-main-hercules-cmd' and
;; `iron-main-hercules-is-listening'.


;;; Code:

;;;; IRON MAIN Hercules commands.

(require 'cl-lib)
(require 'iron-main-vars)
(require 'iron-main-utils)
;; (require 'iron-main-panels)
(require 'iron-main-epf)


;;;; Hercules commands.
;; The following functions implement a subset of the commands that can
;; be issued to a HTTP-enabled Hercules instance.  Note that the
;; commands are essentially "query" and "display" commands that do not
;; modify the state of the Hercules instance.
;;
;; In general "query" commands return strings or numbers, while
;; "display" commands return buffers that can then be used later on.
;;
;; Not all commands available are implemented.  Please refer to the
;; "Hercules – User Reference Guide" and to the
;; "Hercules – Operations and Utilities Guide", either the (a bit old)
;; PDFs dowloadable from the "http://hercules-390.eu" site or the more
;; up-to-date versions contained in the
;; "https://sdl-hercules-390.github.io/html/" site.
;;
;; Notes:
;;
;; 20201228
;; Hercules HTTP returns HTML, with a <pre> element that just contains
;; the printout produced by the command (which is intended to be
;; issued at the console).  Of course, it's be much nicer if Hercules
;; contained a REST and JSON (or better, S-expr) server, alas, su such
;; luxury at the end of 2020.


(cl-defmacro define-hercules-cmd (cmd args &body forms)
  "A useful macro to avoid repeating too much boilerplate.

Expands in a `cl-defun' that is named 'iron-main-hercules-xxx',
where xxx is CMD, with (extented) arguments ARGS, calling FORMS.
The variables HOST, PORT, TIMEOUT and CHECK-LISTENING are
declared '&key' in the extended arguments list and available in
FORMS (as in `iron-main-hercules-cmd'); they are also collected
in the KEYS &rest variable."
  
  (let ((cmd-fun-name (intern (format "iron-main-hercules-%s" cmd))))
    `(cl-defun ,cmd-fun-name
	 ,(append args
		  '(&rest
		    keys
		    &key
		    (host
		     iron-main-hercules-http-host)
		    (port
		     iron-main-hercules-http-port)
		    (timeout 15)
		    (check-listening nil)
		    ))
       ,@forms)))


;;;; Hercules console commands.
;;
;; There are two categories of commands (sorry, you have to read the
;; documentation strings): those that return a value -- usually a
;; string or a list of strings -- and those that switch to another
;; buffer/panel showing the output (the "help" command is one of
;; these).
;;
;; Notes:
;; 20210406: MA: implementation of -help command still incomplete.


(defun iron-main--cmd (cmd-string &rest string-args)
  (with-output-to-string
    (princ cmd-string)
    (dolist (sa string-args)		; Maybe remove empty string?
      (princ " ")
      (princ sa))
    ))


(defun iron-main--get-html-pre-output (buffer)
  (with-current-buffer buffer
    (message ">>> CMD result buffer %S" buffer)
    (goto-char (point-min))
    (let* ((pre-start "<PRE>")
	   (pre-end   "</PRE>")
	   (pre-start-point (search-forward pre-start nil t))
	   (pre-end-point   (search-forward pre-end nil t))
	   )
      (if (and pre-start-point pre-end-point)
	  (buffer-substring pre-start-point (- pre-end-point 6))
	"")
      )))


(define-hercules-cmd "help" (&optional (cmd ""))
  "Return result of running the Hercules 'help' command."
  (let* ((helpcmd (iron-main--cmd "help" cmd))
	 (cmdbuf (apply 'iron-main-hercules-cmd helpcmd keys))
	 )
    (when cmdbuf
      (iron-main--get-html-pre-output cmdbuf))))


(define-hercules-cmd "qpid" ()
  "Return the host process id of the running Hercules instance.

The value returned is a string containing the host system process id
or NIL if the command could not be issued (most likely because there
is no running Hercules instance)."
  (let ((cmdbuf (apply 'iron-main-hercules-cmd "qpid" keys)))
    (when cmdbuf
      (with-current-buffer cmdbuf
	(goto-char (point-min))
	(let ((pid-re "HHC17013I Process ID = \\([0-9]+\\)"))
	  (re-search-forward pid-re)
	  (match-string 1)
	  )))
    ))


(define-hercules-cmd "devlist" (&optional (class-or-id ""))
  "Return result of running the Hercules 'devlist' command."
  (let* ((devlistcmd (iron-main--cmd "devlist" class-or-id))
	 (cmdbuf (apply 'iron-main-hercules-cmd devlistcmd keys))
	 )
    (when cmdbuf
      (iron-main--get-html-pre-output cmdbuf))))
  

;;;; Epilogue
;;;; ========

(provide 'iron-main-hercules-cmds)

;;; iron-main-hercules-cmds.el ends here
