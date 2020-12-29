;;; iron-main-vars --- A major mode to handle MVS or Z/OS JCL.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; iron-main-vars.el
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: December 28, 2020.
;;
;; Version: 20201228.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; The common variables and customizations for the IRON MAIN package.


;;; Code:

;;;; IRON MAIN variable and customizations

;;; Customizations:

(defgroup iron-main ()
  "A minor mode to edit files and interact with IBM MVS or z/OS .

This is a minor mode just because it is a good thing to have a
shared location where to centralize definitions for the main modes
and functionalities that make up this package.  E.g., see the major
modes `jcl-mode' and `asmibm-mode'."
  :group 'languages)


(defcustom iron-main-os-flavor "MVS 3.8j"
  "The current flavor of MVS used.

The values of this variable are strings starting either with 'MVS' or
'z/OS'.  Other variants are acceptable as long as the 'main' OS name
comes first.

The value \"MVS 3.8j\" is the default one, being the version of MVS
that IBM released in the public domain."
  :group 'iron-main
  :type 'string)


(defcustom iron-main-machine "Hercules 4.x"
  "The machine IRON MAIN connects to.

The default is \"Hercules\", meaning the Hercules emulator.  Another
possible values is \"IBM machine\", in which case we mean real heavy
iron hardware.

The values for this variable are strings starting with the type of
machine to which a 'connection' can be made.  For the time being the
only meaningful value is a string starting with 'Hercules', intended
as a variant of the Hercules emulator.  In the future it may be
possible to have also strings starting with 'IBM' (or, possibly,
'Hitachi', etc.).

If this variable has a 'Hercules' value, the Hercules emulator is
running and it has the internal HTTP server active, then IRON MAIN
will be able to extract some information about it.

If the Hercules emulator is running and it has the 'card reader'
active on port 3505 (or another suitable one), then it is possible for
IRON MAIN to at least submit jobs to it, especially if it has a
version of 'MVS' (or 'z/OS') running."
  :group 'iron-main
  :type 'string)


(defgroup iron-main-hercules ()
  "The IRON MAIN subgroup for local Hercules customization."
  :group 'iron-main)


(defcustom iron-main-hercules-version
  (when (string= (substring iron-main-machine 0 (length "Hercules"))
		 "Hercules")
    "SDL/Hyperion 4.3.x")
  "The Hercules version we are running."
  ;; Hardcoded FTTB; make it more intelligent later.
  :group 'iron-main-hercules
  :type  'string
  )


(defcustom iron-main-hercules-http-host "127.0.0.1"
  "The host Hercules uses to listen for HTTP requests."
  :group 'iron-main-hercules
  :type  'string)


(defcustom iron-main-hercules-http-port 8038
  "The more or less standard port Hercules uses to listen for HTTP requests."
  :group 'iron-main-hercules
  :type  'integer)


(defcustom iron-main-hercules-dasd-dir "dasd"
  "The folder containing the installation DASDs.

This variable makes sense only for Hercules installations, i.e., for
setups where the value of `iron-main-machine' is \"Hercules ...\"."
  :group 'iron-main-hercules
  :type  'string
  )


;;;; Epilogue
;;;; ========

(provide 'iron-main-vars)

;;; iron-main-vars.el ends here
