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
;; Version: 2023-09-06.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; The common variables and customizations for the IRON MAIN package.


;;; Code:

;;;; IRON MAIN variable and customizations

;;; Customizations:

(defgroup iron-main ()
  "Emacs interaction with Mainframes OSes (e.g., IBM MVS or z/OS).

IRON MAIN is a collection of applications and modes (major and minor)
that can be used to interact with IBM MVS or z/OS.  Most shared
options regarding the collection of application and modes are
collected under this heading.

See also, e.g., see the major modes `jcl-mode' and `hlasm-mode'."
  :group 'Applications)


(defcustom iron-main-os-flavor "MVS 3.8j"
  "The current flavor of MVS used.

The values of this variable are strings starting either with
\\='MVS\\=' or \\='z/OS\\='.  Other variants are acceptable as long as
the \\='main\\=' OS name comes first.

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
machine to which a \\='connection\\=' can be made.  For the time being
the only meaningful value is a string starting with \\='Hercules\\=',
intended as a variant of the Hercules emulator.  In the future it may
be possible to have also strings starting with \\='IBM\\=' (or,
possibly, '\\=Hitachi\\=', etc.).

If this variable has a \\='Hercules\\=' value, the Hercules emulator
is running and it has the internal HTTP server active, then IRON MAIN
will be able to extract some information about it.

If the Hercules emulator is running and it has the \\='card reader\\='
active on port 3505 (or another suitable one), then it is possible for
IRON MAIN to at least submit jobs to it, especially if it has a
version of \\='MVS\\=' (or \\='z/OS\\=') running."
  :group 'iron-main
  :type 'string)


(defgroup iron-main-hercules ()
  "The IRON MAIN subgroup for local Hercules customization."
  :group 'iron-main)


(defcustom iron-main-hercules-version
  (when (string= (substring iron-main-machine 0 (length "Hercules"))
		 "Hercules")
    "SDL/Hyperion 4.x.x")
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
  "The standard-ish port Hercules uses to listen for HTTP requests."
  :group 'iron-main-hercules
  :type  'integer)


(defcustom iron-main-hercules-os-dir "."
  "The folder where the OS running on Hercules resides..

This variable makes sense only for Hercules installations, i.e., for
setups where the value of `iron-main-machine' is \"Hercules ...\"."
  :group 'iron-main-hercules
  :type  'string
  )


(defcustom iron-main-hercules-dasd-dir "dasd"
  "The folder containing the installation DASDs.

This variable makes sense only for Hercules installations, i.e., for
setups where the value of `iron-main-machine' is \"Hercules ...\"."
  :group 'iron-main-hercules
  :type  'string
  )


(defcustom iron-main-hercules-card-reader-port 3505
  "The port where the Hercules \\='card reader\\=' listens to.

Hercules usually has a \\='card reader\\=' configured to listen
on a socket with default port number 3505 (the IBM card reader
devide model number). This reader, if configured, can be used
submit JCL directly, which is what IRON MAIN exploits in
`jcl-mode'."
  :group 'iron-main-hercules
  :type  'integer
  )


;;;; Epilogue
;;;; ========

(provide 'iron-main-vars)

;;; iron-main-vars.el ends here
