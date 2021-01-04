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
(require 'iron-main-panels)


;;;; Hercules commands.
;; The following functions implement a subset of the commands that can
;; be issued to a HTTP-enabled Hercules instance.  Note that the
;; commands are essentially "query" and "display" commands that do not
;; modify the state of the Hercules instance.
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

(define-hercules-cmd "help" ()
  "Return result of running the Hercules 'help' command.

The value returned is a string containing the host system process id
or NIL if the command could not be issued (most likely because there
is no running Hercules instance)."
  (let ((cmdbuf (apply 'iron-main-hercules-cmd "help" keys)))
    (when cmdbuf
      (with-current-buffer cmdbuf
	(goto-char (point-min))
	(let ((pid-re "HHC17013I Process ID = \\([0-9]+\\)"))
	  (re-search-forward pid-re)
	  (match-string 1)
	  )))
    ))


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


	 
    
  

;;;; Epilogue
;;;; ========

(provide 'iron-main-hercules-cmds)

;;; iron-main-hercules-cmds.el ends here
