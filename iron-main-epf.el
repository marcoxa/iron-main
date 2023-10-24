;;; -*- Mode: Emacs-Lisp; lexical-binding: t -*-
;;; iron-main-epf.el --- IRON MAIN "panels" for mainframe interaction.

;;; iron-main-epf.el
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: July 20th, 2023.
;;
;; Version: 2023-10-24.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; This file contains the "SPF" application (Emacs PF) and panels that
;; are used to interact with a "mainframe" (again, mostly a MVS 3.8j
;; running on Hercules).
;;
;; The implementation is an interesting use (or, one should say,
;; "abuse") of the Emacs `widget.el' library to give the "look and feel
;; of using a "mainframe" application in... Emacs (I know: I am a pervert).
;;
;; To use, just issue
;;
;; M-x iron-main-frame
;;
;; The functions and variables in this file have 'iron-main-epf-' as
;; prefix.
;;
;; This file was cloned from the previous `iron-main-panels.el' file.
;; This was necessary as the wew implementation uses much more
;; sophisticated Emacs buffer and windows handling.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'widget)
  (require 'wid-edit))

(require 'iron-main-vars)
(require 'iron-main-utils)
(require 'iron-main-jcl-templates)


;;; IRON MAIN SPF panels.
;;
;; "Panel" is mainframe-speak for "page", or in the context of Emacs,
;; "window/buffer".
;;
;; The code below uses several examples of the "widget" library found
;; in Emacs.


;; Global constants.

(defconst iron-main-epf--overline (make-string 72 175))
(defconst iron-main-epf--underline (make-string 72 ?_))


;; IRON MAIN EPF panels are mapped to buffers and windows.  A panel
;; may have "sub" panels used for a variety of purposes, but they are
;; mapped on windows split from the top one.
;;
;; Since we are mapping windows and buffers to panels (and viceversa),
;; the state of a panel is encoded in buffer local variables.
;; Some of these are panel spcific (see the `widget' variables for,
;; e.g., the dataset allocation).


;; Common variables.

(defvar-local iron-main-epf--tag nil
  "A tag that identifies the type of IRON MAIN panel.

The value can be either a string or a symbol.")


;; Navigation.
;; These variables encode the "panels stack"; they could probably done
;; away with by using the buffer tracking builtin facilities.
;;
;; Pretty crude for the time being.
;; This just acts as a stack.  Whever a panel is "closed", the buffer
;; is killed and the content of this variable, a buffer, if not nil
;; and live, is restored.


(defvar-local iron-main-epf--back nil
  "Local panel variable set by the `invoking' panel."
  )


(defvar-local iron-main-epf--in-panel nil
  "When non NIL, the buffer is an IRON MAIN panel.")


;; Variables local to dataset handling panels.

(defvar-local iron-main-epf--current-ds (make-iron-main-ds-rep))
(defvar-local iron-main-epf--dsname-widget nil)


;; Variables local to dataset allocation and viewing panels.

(defvar-local iron-main-epf--recfm-widget nil)
(defvar-local iron-main-epf--lrecl-widget nil)
(defvar-local iron-main-epf--blksize-widget nil)
(defvar-local iron-main-epf--dsorg-widget nil)

(defvar-local iron-main-epf--vol-widget nil)
(defvar-local iron-main-epf--unit-widget nil)

(defvar-local iron-main-epf--space-unit-widget nil)
(defvar-local iron-main-epf--primary-widget nil)
(defvar-local iron-main-epf--secondary-widget nil)
(defvar-local iron-main-epf--dir-widget nil)


;;; IRON MAIN panel keymaps.

(defvar iron-main-epf-mode-keymap
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km widget-keymap)
    (define-key km (kbd "<f3>") 'iron-main-epf--exit-panel)
    (define-key km (kbd "q") 'iron-main-epf--exit-panel)
    (define-key km (kbd "Q") 'iron-main-epf--exit-panel)
    km
    )
  "The IRON MAIN Panel mode key map.

The key map inherits from `widget-keymap'.
The keys \\='<f3>\\=' (that is, \\='PF3\\='), \\='q\\=' and \\='Q\\='
exit the current panel or the whole IRON MAIN panel system.")


(defvar iron-main-epf-mode-top-panel-keymap iron-main-epf-mode-keymap
  "The IRON MAIN Panel mode key map.

The key map inherits from `widget-keymap'.
The keys \\='<f3>\\=' (that is, \\='PF3\\='), \\='q\\=' and \\='Q\\='
exit the current panel."
  )


(defvar iron-main-epf-mode-sub-panel-keymap
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km iron-main-epf-mode-keymap)

    ;; Redefine "exit" keys...
    (define-key km (kbd "<f3>") 'iron-main-epf--exit-subpanel)
    (define-key km (kbd "q") 'iron-main-epf--exit-subpanel)
    (define-key km (kbd "Q") 'iron-main-epf--exit-subpanel)
    km
    )
  "The IRON MAIN Panel mode key map.

The key map inherits from `widget-keymap'.
The keys \\='<f3>\\=' (that is, \\='PF3\\='), \\='q\\=' and \\='Q\\='
exit the current panel."
  )


(defvar iron-main-epf-editable-field-keymap
  (let ((km (make-sparse-keymap "Iron Main Editable Field Keymap")))
    (set-keymap-parent km widget-field-keymap)

    ;; Redefine F3; the original is
    ;; `kmacro-start-macro-or-insert-counter' from `global-map'.
    
    (define-key km (kbd "<f3>") 'iron-main-epf--exit-panel)
    km
    )
  "The IRON MAIN Editable Field key map.

The key map inherits from `widget-field-keymap'.
The key \\='<f3>\\=' (that is, \\='PF3\\=') exits the current panel.")


;;; Other buffer local variables.

(defvar-local iron-main-epf--cmds ()
  "The panel command alist.

This list contains a \"command alist\" which is a specification for
link widgets to insert in a panel.  Each IRON MAIN panel (which is
eventually an Emacs buffer) can initialize this variable as it wishes.

The format is of a  \"command alist\" is the following:

    (cmd function &rest keys)

Where CMD is a string, FUNCTION a \"panel invocation\" function and
KEYS a list of key-value pairs to be used for `widget-create'.

See Also:

`iron-main-epf--hercules-top-commands',
`iron-main-epf--hercules-dsfs-commands'
")


(defvar-local iron-main-epf--cmds-links ()
  "List of \"link\" widgets for the commands available in the panel.")


;;; Widget related functions.
;; -------------------------

;; Maybe use the widget "property" functions instead in the next
;; functions: i.e., `widget-get'.

(cl-defun iron-main-epf--widget-tag (w)
  "Get the :tag of widget W.

Notes:

This function is necessary because it is inexplicably absent from the
`widget.el' library."
  (plist-get (cdr w) :tag))


(cl-defun iron-main-epf--widget-notify (w)
  "Get the :notify function of widget W.

Notes:

This function is necessary because it is inexplicably absent from the
`widget.el' library."
  (plist-get (cdr w) :notify))


;; In-panel navigation.
;;
;; The following functions *assume* that the first and last character
;; in a panel are NOT part of a widget.

(cl-defun iron-main-epf--goto-first-widget ()
  (goto-char (point-min))
  (widget-forward 1))


(cl-defun iron-main-epf--goto-last-widget ()
  (goto-char (point-max))
  (widget-backward 1))


;;; Commands alists.
;; ----------------
;;
;; Commands alists are lists of "specifications" for link widgets to
;; insert in a panel.
;; Their format is:
;;
;;    (cmd function &rest keys)
;;
;; Where CMD is a string, FUNCTION a "panel invocation" function and
;; KEYS a list of key-value pairs to be used for `widget-create'.


;; Top commands.

(defconst iron-main-epf--hercules-top-commands
  `(("System" iron-main-epf--hercules-system
     :header "Inspect Hercules system/machine"
     )
    ("Datasets" iron-main-epf--hercules-dsfs-utilities
     :header "Handle files and datasets across systems"
     )
    ("Emacs PF" iron-main-epf--emacs-pf-system
     :header "Inspect Emacs SP configuration"
     )
    ("Help" iron-main-epf--hercules-help
     :header "Hercules help"
     )
    ("Exit"   iron-main-epf--exit-panel
     :header "Exit the IRON MAIN current panel or top-level"
     ;; We just want to exit, no questions asked...
     ;; This is necessary because `iron-main-epf--exit-panel' just
     ;; takes the buffer and not the session.
     :notify ,(lambda (w &rest args)
      		(ignore w args)
      		(iron-main-epf--exit-panel))
     )
    ("Test..." iron-main-epf--test-subpanels
     :header "Iron Main Test handle..."
     )
    )
  "A \\='commands alist\\=' for the IRON MAIN top SPF panel."
  )


;; DSFS commands

(defvar iron-main-epf--hercules-dsfs-commands
  `(("Allocate" iron-main-epf--dataset-allocation
     :header "Allocate a dataset on the mainframe"
     )
    ("Upload" iron-main-epf--dataset-save
     :header "Upload a local file on the mainframe"
     )
    ("Edit"  iron-main-epf--dataset-edit
     :header "Edit a dataset member from the mainframe (if connected)"
     )
    ("Edit local"  iron-main-epf--dataset-edit-local
     :header "Edit a local file"
     )
    ("Exit"   iron-main-epf--exit-panel
     :header "Exit the IRON MAIN current panel or top-level"
     :notify ,(lambda (w &rest args)
		(ignore w args)
		(iron-main-epf--exit-panel)))
    )
  "A \\='commands alist\\=' for the IRON MAIN dataset and filesystem panel."
  )


;; Commands lists functions.

(cl-defun iron-main-epf--find-command (cmd commands-alist)
  ;; (assoc cmd commands-alist 'string=)
  (assoc cmd commands-alist) ; Older Emacsen do not accept the third arg.
  )


(cl-defun iron-main-epf--command-field ()
  (widget-insert "Command")
  (widget-create 'iron-main-natnum-widget
                 :size 3
                 :tag ""
                 :value ""
                 
		 :validate
		 (lambda (cmd)
		   (<= 1
		       (widget-value cmd)
		       (length iron-main-epf--cmds)))
		 
		 :action
		 (lambda (cmd &optional event)
		   (ignore event)
		   ;; (message ">>> notified")
		   ;; (sleep-for 3)
		   (let* ((cmd-widget
			   (nth (1- (widget-value cmd))
				iron-main-epf--cmds-links))
			  (cmd-notify
			   (iron-main-epf--widget-notify cmd-widget))
			  )
		     ;; (message ">>> calling %s on %s"
		     ;;  	      cmd-notify
		     ;; 	      cmd-widget)
		     (when cmd-notify
		       (apply cmd-notify cmd-widget ()))
		     ))
                 
		 :keymap
		 iron-main-epf-editable-field-keymap
		 )
  
  (widget-insert "\n\n")
  )


(cl-defun iron-main-epf--insert-command-widgets (cmd-alist)
  (cl-loop for option in cmd-alist
	   for opt-i from 1
	   for header = (plist-get option :header)
	   for notify = (plist-get option :notify)
	   do
	   (widget-insert (format "%3d. " opt-i))
	   collect
	   (widget-create 'link
			  :format "%[%t%]\t: %v"
			  :tag (cl-first option)
			  :value header
			  :button-prefix ""
			  :button-suffix ""
			  :notify
			  (if notify
			      notify
			    (lambda (w &rest args)
			      (ignore args)
			      (let ((panel-function
				     (cl-second
				      (iron-main-epf--find-command
				       (iron-main-epf--widget-tag w)
				       cmd-alist)))
				    )
				(iron-main-epf--invoke-panel
				 (current-buffer)
				 panel-function)
				))))
	   do
	   (widget-insert "\n")
	   ))


;; IRON MAIN EPF modeline.
;; -----------------------

;;; iron-main-epf--make-modeline

(cl-defun iron-main-epf--make-mode-line
    (&optional
     (extra-info " Use 'Q', 'q', or '<F3>' to quit")
     )
  "Creates the (default) mode line for the IRON MAIN EPF windows."
  
  (identity
   ;; format-mode-line ; just return the structure as is.
   `(
     ;; "ESP "
     " "
     mode-line-buffer-identification
     "  "
     mode-line-modes
     ,extra-info
     )))


(cl-defun iron-main-epf--make-header-line (&optional (title ""))
  "Creates the (default) header line for the IRON MAIN EPF windows."

  (identity
   ;; format-mode-line ; just return the structure as is.
   `(:propertize ,(format " IRON MAIN %s" title)
		 ;; 'face 'fixed-pitch-serif
		 face (
		       ;; fixed-pitch
		       :foreground "red"
		       :weight bold
		       )
		 )))


;; IRON MAIN EPF modes.
;; --------------------

;;; iron-main-epf-mode
;; The main mode (major) for the panels.

(define-derived-mode iron-main-epf-mode nil "//IRON-MAIN"
  "IRON MAIN Panel Mode.

Major mode for IRON MAIN Panels.  Mostly a container for variables
and a specialized keymap.

You an use the function key `F3' (i.e., `PF3') or the `[Qq]' keys to
exit a panel.  Exiting the top panel will exit the IRON MAIN
interface.

Note that `overwrite-mode' is turned on in the panels."

  (setq-local iron-main-epf--in-panel t)
  (setq-local iron-main-epf--back nil)

  (overwrite-mode 42)                   ; Turn on `overwrite-mode'.
  (use-local-map iron-main-epf-mode-keymap)

  ;; Minimal header and mode lines; panels may change these.

  ;; Newer version that abuses the tab line for panel titles.
  ;; (setq-local header-line-format (iron-main-epf--make-header-line))
  (setq-local tab-line-format (iron-main-epf--make-header-line))
  (setq-local mode-line-format (iron-main-epf--make-mode-line))
  )


;; IRON MAIN Emacs Panels.
;; -----------------------
;;
;; The actual panels available.
;; The "fields" (or "widgets") of each panel have (function) names
;; that end in "-field" or "-widget".

;; title-field

(cl-defun iron-main-epf--title-field (&optional title)
  "Create the the IRON MAIN panel title using argument TITLE."
  
  (unless title
    (setq title "Top"))
  ;; (widget-insert "\n")
  (widget-insert (propertize (format "IRON MAIN %s\n" title)
			     ;; 'face 'fixed-pitch-serif
			     'face '(fixed-pitch-serif
				     :foreground "red"
				     :weight bold
				     )
			     ))
  (widget-insert iron-main-epf--overline)	; 175 is the "overline"
  (widget-insert "\n")
  )


;; help-field
;; Unused.

(cl-defun iron-main-epf--help-field ()
  "Create the \"help\" field at the bottom of the window."
  ;; Call last before `widget-setup'.

  ;; The next one is a kludge to position the message on the "last"
  ;; window without resorting (as I probably should) to more
  ;; sophisticated Emacs techniques involving minibuffer-less windows
  ;; etc.

  (let ((wh (window-total-height))
	(lc (count-lines (window-start) (window-end)))
	(lp-current (line-number-at-pos))
	(lp-window-end (line-number-at-pos (window-end)))
	)
    (cond ((>= lc wh)
	   (move-to-window-line -1)	; Last visible line.
	   )
	  ((< lc wh)
	   ;; Kludgy part: pad the buffer with newlines.
	   (forward-line (- lp-window-end lp-current))
	   (widget-insert (make-string (- wh lc) ?\n))
	   (move-to-window-line -1)))
    )

  (widget-insert
   (propertize
    (format "Use `[Qq]' or `PF3' to go back to previous panel.")
    'face '(fixed-pitch-serif :weight bold)
    ))
  )


;; Datasets and file system handling panel.
;; ----------------------------------------

(cl-defun iron-main-epf--dsname-item-field ()
  "Create the `dsname' editable-field widget in the IRON MAIN panel."
  
  (setq iron-main-epf--dsname-widget
	(widget-create
	 'editable-field
         :size 46		   ; A name is at most 44 plus quotes.
                       
         :format "Data set name: %v "	; Text after the field!
                       
	 :value
         (iron-main-ds-rep-name iron-main-epf--current-ds)
	 ;; (iron-main-ds-rep-name iron-main-epf--current-ds)
                       
	 :notify
	 (lambda (w &rest ignore)
	   (ignore ignore)
	   (message "DSN: <%s>."
		    (widget-value w))
	   (setf (iron-main-ds-rep-name
		  iron-main-epf--current-ds)
		 (widget-value w)))
                       
	 :keymap
	 iron-main-epf-editable-field-keymap
	 ))
  (widget-insert "\n")
  
  (setf iron-main-epf--vol-widget
	(widget-create
	 'editable-field
         :size 6
	 :format "Volume serial: %v "
                       
	 :value
         (iron-main-ds-rep-vol iron-main-epf--current-ds)
                       
	 :notify
	 (lambda (w &rest ignore)
	   (ignore ignore)
	   (message "VOL: <%s>."
		    (widget-value w))
	   (setf (iron-main-ds-rep-vol
		  iron-main-epf--current-ds)
		 (widget-value w)))
                       
	 :keymap
	 iron-main-epf-editable-field-keymap

	 ))
    )


(defvar iron-main-ds-allocation-dsorg "PDS"
  "The default data set organization (DSORG).")


(defvar-local iron-main-epf--hercules-dsfs-cmds-links ()
  "List of link widgets created for DSFS panel commands.")


(cl-defun iron-main-epf--hercules-dsfs-utilities (session &rest args)
  "Create the IRON MAIN datasets and file system utilities.

The panel presents the options regarding the handling of datasets
(files) on the connected, possibly hosted, operating system and the
file system(s) that Emacs has direct access to; most notably, the
\"host\" file system, say, Linux, Mac OS X or Windows."

  (interactive)

  (ignore args)
  
  (cl-assert (iron-main-session-p session) nil
	     "SESSION %S is not a `iron-main-session'"
	     session)

  (switch-to-buffer "*IRON MAIN dataset and file system handling.*")

  (kill-all-local-variables)

  (let ((inhibit-read-only t))
    (erase-buffer))

  (iron-main-epf-mode)

  ;; Now that we have the session, some of these are repetitions.

  (setq-local iron-main-epf--session session)
  
  (setq-local iron-main-machine
	      (iron-main-session-machine session))
  (setq-local iron-main-os-flavor
	      (iron-main-session-os-flavor session))

  (setq-local iron-main-epf--tag "Hercules datasets")
  (setq-local iron-main-epf--cmds
	      iron-main-epf--hercules-dsfs-commands)
  
  ;; (iron-main-epf--title-field "Dataset and file system handling panel")
  (setq-local header-line-format
	      (iron-main-epf--make-header-line
	       "Dataset and file system handling panel"))

  (widget-insert "\n")

  ;; Let's start!

  (iron-main-epf--command-field)
  
  (setq-local iron-main-epf--cmds-links
	      (iron-main-epf--insert-command-widgets
	       iron-main-epf--cmds))

  (widget-insert "\n\n")
  (widget-insert iron-main-epf--underline)
  (widget-insert "\n")
  (widget-insert (iron-main-epf--instance-banner session))

  (message "IMHS00I: Hercules datasets and file system utilities.")
  (prog1 (widget-setup)
    ;; (widget-forward 1)
    (iron-main-epf--goto-first-widget)
    )
  )


;; Dataset allocation.
;; -------------------

(defvar-local iron-main-epf--jcl-alloc-job-buffer nil
  "The buffer containing the JCL to allocate a dataset.")


(cl-defun iron-main-epf--dataset-allocation (session &rest args)
  "Create the IRON MAIN dataset allocation panel."
  
  (interactive)

  (ignore session args)

  ;; (cl-assert (iron-main-session-p session) t
  ;; 	     "SESSION %S is not a `iron-main-session'"
  ;; 	     session)
  
  (switch-to-buffer "*IRON MAIN dataset allocation*")

  (kill-all-local-variables)

  (let ((inhibit-read-only t))
    (erase-buffer))

  (iron-main-epf-mode)

  ;; Init buffer local variables.
  
  (setq-local iron-main-epf--tag "Allocation panel")
  (setq-local iron-main-epf--cmds ())
  (setq-local iron-main-epf--session session)

  
  ;; Let's start!
  
  ;; (iron-main-epf--title-field "Dataset allocation panel")
  (setq-local header-line-format
	      (iron-main-epf--make-header-line
	       "Dataset allocation panel"))
  
  (iron-main-epf--dsname-item-field)
			 
  (widget-insert "\n\n")

  (setf iron-main-epf--recfm-widget
	(widget-create 'editable-field
		       :format "Record format (RECFM):         %v "
		       :value (iron-main-ds-rep-recfm
			       iron-main-epf--current-ds)
		       :size 3
		       :notify
		       (lambda (w &rest ignore)
			 (ignore ignore)
			 (message "RECF: <%s>."
				  (widget-value w))
			 (setf (iron-main-ds-rep-recfm
				iron-main-epf--current-ds)
			       (widget-value w)))
		       :keymap
		       iron-main-epf-editable-field-keymap
		       :help-echo
		       "Please enter the record format: F, FB, V..."
		       ))
  (widget-insert "\n")
  (message "IMPDSA1: RECFM widget created.")

  (setf iron-main-epf--lrecl-widget
	(widget-create  'iron-main-natnum-widget
		       :format "Logical record length (LRECL): %v "
		       :value (iron-main-ds-rep-lrecl
			       iron-main-epf--current-ds)
		       :size 4
		       
		       ;; :value-regexp "[0-9]+"
		       :notify
		       (lambda (w &rest ignore)
			 (ignore ignore)
			 (message "LRECL: <%s>."
				  (widget-value w))
			 (setf (iron-main-ds-rep-lrecl
				iron-main-epf--current-ds)
			       (widget-value w)))

		       :keymap
		       iron-main-epf-editable-field-keymap
		       :help-echo
		       "Please enter the record length..."
		       ))
  (widget-insert "\n")
  (message "IMPDSA2: LRECL widget created.")

  (setf iron-main-epf--blksize-widget
	(widget-create  'iron-main-natnum-widget
		       :format "Block size (BLKSIZE):          %v "
		       :value (iron-main-ds-rep-blksize
			       iron-main-epf--current-ds)
		       :size 6
		       ;; :value-regexp "[0-9]+"
		       :notify
		       (lambda (w &rest ignore)
			 (ignore ignore)
			 (message "BLKSIZE: <%s>."
				  (widget-value w))
			 (setf (iron-main-ds-rep-blksize
				iron-main-epf--current-ds)
			       (widget-value w)))

		       :keymap
		       iron-main-epf-editable-field-keymap
		       :help-echo
		       "Please enter the block size..."
		       ))
  (widget-insert "\n\n")
  (message "IMPDSA3: BLKSIZE widget created.")

  (setf iron-main-epf--vol-widget
	(widget-create 'editable-field
		       :format "Unit:                          %v "
		       :value (iron-main-ds-rep-unit
			       iron-main-epf--current-ds)
		       :size 6
		       :notify
                       (lambda (w &rest ignore)
			 (ignore ignore)
			 (message "UNIT: <%s>."
				  (widget-value w))
			 (setf (iron-main-ds-rep-unit
				iron-main-epf--current-ds)
			       (widget-value w)))
                       
		       :keymap
		       iron-main-epf-editable-field-keymap
		       :help-echo
		       "Please enter the unit info..."
		       ))
  (widget-insert "\n\n")
  (message "IMPDSA4: UNIT widget created.")

  (widget-insert "Dataset organization (DSORG): \n")
  (setf iron-main-epf--dsorg-widget
	(widget-create 'radio-button-choice
		       :tag "Dataset organization (DSORG)"
		       :doc "Dataset organization (DSORG)"
		       ;; :value "PO"
		       :void  "PO"
		       :indent 4
		       :help-echo
                       "Please choose the dataset organization: PO, PS..."
                       
		       :notify
                       (lambda (w &rest ignore)
			 (ignore ignore)
			 (message "Dataset organization: <%s>."
				  (widget-value w))
			 (setf (iron-main-ds-rep-dsorg
				iron-main-epf--current-ds)
			       (widget-value w)))
                       
		       '(item :tag "Partitioned Data Set (PO)"
			      :value "PO")
		       ;; '(item :tag "Partitioned Data Set Extended (PDSE)"
		       ;;        :value "PDSE")
		       '(item :tag "Sequential (PS)"
			      :value "PS")
		       
		       ;; Add other ones later...		       
		       ))
  (widget-insert "\n\n")
  (message "IMPDSA5: DSORG widget created.")

  (widget-insert "Space allocation:\n")
  (setf iron-main-epf--space-unit-widget
	(widget-create 'radio-button-choice
		       :tag "Dataset space unit (SPACE)"
		       ;; :value "TRK"
		       :void  "TRK"
		       :indent 4
		       :help-echo
		       "Please choose the dataset space unit..."
		       :notify
		       (lambda (w &rest ignore)
			 (ignore ignore)
			 (message "Dataset space unit: <%s>."
				  (widget-value w))
			 (setf (iron-main-ds-rep-space-unit
				iron-main-epf--current-ds)
			       (widget-value w)))
		       
		       '(item "TRK")
		       '(item "CYL")
		       '(item "BLK")
		       ;; Add other ones.
		       ))
  (widget-insert "\n")
  (message "IMPDSA6: SPACE widget created.")
  
  (setf iron-main-epf--primary-widget
	(widget-create  'iron-main-natnum-widget
		       :format "Primary: %v "
		       :value (iron-main-ds-rep-primary
			       iron-main-epf--current-ds)
		       :size 8
		       ;; :value-regexp "[0-9]+"
		       :notify
		       (lambda (w &rest ignore)
			 (ignore ignore)
			 (message "Primary: <%s>."
				  (widget-value w))
			 (setf (iron-main-ds-rep-primary
				iron-main-epf--current-ds)
			       (widget-value w)))

		       :keymap
		       iron-main-epf-editable-field-keymap
		       :help-echo
		       "Please the primary amount of space..."
		       ))
  (message "IMPDSA7: Primary widget created.")
  
  (widget-insert "    ")
  (setf iron-main-epf--secondary-widget
	(widget-create  'iron-main-natnum-widget
		       :format "Secondary: %v "
		       :value (iron-main-ds-rep-secondary
			       iron-main-epf--current-ds)
		       :size 8
		       ;; :value-regexp "[0-9]+"
		       :notify
		       (lambda (w &rest ignore)
			 (ignore ignore)
			 (message "Secondary: <%s>."
				  (widget-value w))
			 (setf (iron-main-ds-rep-secondary
				iron-main-epf--current-ds)
			       (widget-value w)))
		       :keymap
		       iron-main-epf-editable-field-keymap
                       
		       :help-echo
                       "Please the secondary amount of space..."
		       ))
  (message "IMPDSA8: Secondary widget created.")
  
  (widget-insert "    ")
  (setf iron-main-epf--dir-widget
	(widget-create  'iron-main-natnum-widget
		       :format "Directory blocks: %v "
		       :value (iron-main-ds-rep-directory
			       iron-main-epf--current-ds)
		       :size 4		       ;; :value-regexp "[0-9]+"
		       :notify
		       (lambda (w &rest ignore)
			 (ignore ignore)
			 (message "Directory blocks: <%s>."
				  (widget-value w))
			 (setf (iron-main-ds-rep-directory
				iron-main-epf--current-ds)
			       (widget-value w)))

		       :keymap
		       iron-main-epf-editable-field-keymap
                       
		       :help-echo
                       "Please enter the number of directory blocks..."
		       ))
  (message "IMPDSA9: Directory blocks widget created.")

  ;; (widget-insert "\n\n\n")
  
  (widget-insert "\n")
  (widget-insert iron-main-epf--underline)
  (widget-insert "\n")
  
  (widget-create 'push-button
                 :value "Allocate"
                 
                 :notify
                 (lambda (&rest ignore)
		   (ignore ignore)

		   
		   ;; (setf (iron-main-ds-rep-name
		   ;; 	  iron-main-epf--current-ds)
		   ;; 	 (widget-value iron-main-epf--dsname-widget)
				 
		   ;; 	 (iron-main-ds-rep-dsorg
		   ;; 	  iron-main-epf--current-ds)
		   ;; 	 (widget-value iron-main-epf--dsorg-widget)

		   ;; 	 ;; (iron-main-ds-rep-space-unit
		   ;; 	 ;;  iron-main-epf--current-ds)
		   ;; 	 ;; (widget-value iron-main-epf--space-unit-widget)
		   ;; 	 )
		   
		   (message "DD %s"
			    (iron-main-ds-to-string
			     iron-main-epf--current-ds)
			    )
		   (iron-main-jcl-tmpl--run-allocation-job
		    iron-main-epf--session
		    :dsrep iron-main-epf--current-ds)
                   ))
  (widget-insert "    ")
  (widget-create 'push-button
                 :value "View job buffer"
                 
                 :notify
                 (lambda (&rest ignore)
		   (ignore ignore)
		   (let ((dsname
			  (iron-main-ds-rep-name
			   iron-main-epf--current-ds))
			 )
		     (message "JCL buffer for '%s': ...."
			      dsname
			      )
		     (iron-main-jcl-tmpl--allocation-job
		      ;; (format "%s.jcl" dsname)
		      iron-main-epf--session
		      :dsrep iron-main-epf--current-ds)
		     ))
		 )
  
  ;; (widget-insert "    ")
  ;; (widget-create 'push-button
  ;;                :value "Allocate and Save"
                 
  ;;                :notify
  ;;                (lambda (&rest ignore)
  ;; 		   (ignore ignore)
  ;; 		   (message "Data set name: %s."
  ;; 			    (iron-main-ds-rep-name
  ;; 			     iron-main-epf--current-ds)
  ;; 			    )
  ;; 		   (iron-main-epf--dataset-save session)
  ;; 		   )
  ;;               )
  
  (widget-insert "    ")
  (widget-create 'push-button
                 :value "Cancel"
                 
                 :notify
		 (lambda (&rest ignore)
		   (ignore ignore)
		   (iron-main-epf--exit-panel)
		   (message
		    "Cancelled dataset '%s' mainframe allocation."
		    (iron-main-ds-rep-name
		     iron-main-epf--current-ds)
		    )
		   )
               )

  (widget-insert "\n")

  (message "IMDS00I: Dataset allocation panel set up.")
  (prog1 (widget-setup)
    ;; (widget-forward 1)
    (iron-main-epf--goto-first-widget)
    )
  )


;; Dataset save panel.
;; -------------------

(defvar iron-main-epf--filename-widget "")

(cl-defun iron-main-epf--dataset-save (session &rest args)
  "Create the IRON MAIN dataset save panel."
  
  (interactive)

  (ignore args session)

  ;; (cl-assert (iron-main-session-p session) t
  ;; 	     "SESSION %S is not a `iron-main-session'"
  ;; 	     session)
  
  (switch-to-buffer "*IRON MAIN dataset save*")
  
  (kill-all-local-variables)
  
  (let ((inhibit-read-only t))
    (erase-buffer))

  (iron-main-epf-mode)
  
  ;; (iron-main-epf--title-field "Dataset member save")
  (setq-local header-line-format
	      (iron-main-epf--make-header-line "Dataset member save"))
  
  (iron-main-epf--dsname-item-field)
  
  (widget-insert "\n\n")
  
  (setq iron-main-epf--filename-widget
	(widget-create 'file
		       :format "File: %v\n"
		       :value (iron-main-ds-rep-name
			       iron-main-epf--current-ds)
		       :size (- 72 (length "File: "))
		       :keymap
		       iron-main-epf-editable-field-keymap))

  (widget-insert iron-main-epf--underline)
  (widget-insert "\n")
  
  (widget-create 'push-button
                 :notify
		 (lambda (&rest ignore)
		   (ignore ignore)
		   (message "DD: <%s>."
			    (iron-main-ds-to-string
			     iron-main-epf--current-ds)
			    ))		 
                 "Save to mainframe")
  (widget-insert "    ")
  (widget-create 'push-button
		 :value "View job buffer"

                 :notify
		 (lambda (&rest ignore)
		   (ignore ignore)
		   (let ((dsname
			  (iron-main-ds-rep-name
			   iron-main-epf--current-ds))
			 )
		     (message "JCL buffer for '%s': ...."
			      dsname
			      )
		     (iron-main-jcl-tmpl--allocation-job
		      (format "%s.jcl" dsname))
		     ))
		 
		 ;; (lambda (&rest ignore)
		 ;;   (ignore ignore)
		 ;;   (message "JCL buffer for '%s' and '%s': ...."
		 ;; 	    (iron-main-ds-rep-name
		 ;; 	     iron-main-epf--current-ds)
		 ;; 	    (widget-value
		 ;; 	     iron-main-epf--filename-widget)

		 )
  (widget-insert "    ")
  (widget-create 'push-button
		 :value "Cancel"

                 :notify
		 (lambda (&rest ignore)
		   (ignore ignore)
		   (iron-main-epf--exit-panel)
		   (message "Saving dataset '%s' to mainframe cancelled."
			    (iron-main-ds-rep-name
			     iron-main-epf--current-ds)
			    )
		   )
		 )

  (widget-insert "\n")

  (message "IMDS00I: Dataset upload panel set up.")
  (prog1 (widget-setup)
    ;; (widget-forward 1)
    (iron-main-epf--goto-first-widget)
    )
  )


;; Dataset edit dataset member panel.
;; ----------------------------------

(cl-defun iron-main-epf--dataset-edit (session &rest args)
  "Create the IRON MAIN dataset save panel."
  
  (interactive)

  (ignore session args)

  ;; (cl-assert (iron-main-session-p session) t
  ;; 	     "SESSION %S is not a `iron-main-session'"
  ;; 	     session)
  
  (switch-to-buffer "*IRON MAIN dataset edit*")
  
  (kill-all-local-variables)
  
  (let ((inhibit-read-only t))
    (erase-buffer))

  (iron-main-epf-mode)
  
  ;; (iron-main-epf--title-field "Dataset member edit")
  (setq-local header-line-format
	      (iron-main-epf--make-header-line "Dataset member edit"))
  
  (iron-main-epf--dsname-item-field)
  
  (widget-insert "\n\n")
  
  (setq iron-main-epf--filename-widget
	(widget-create 'file
		       :format "File: %v\n"
		       :value (iron-main-ds-rep-name
			       iron-main-epf--current-ds)
		       :size (- 72 (length "File: "))
		       :keymap iron-main-epf-editable-field-keymap))

  (widget-insert iron-main-epf--underline)
  (widget-insert "\n")
  

  (message "IMDS00I: Dataset edit panel set up.")
  (prog1 (widget-setup)
    ;; (widget-forward 1)
    (iron-main-epf--goto-first-widget)
    )
  )


;; Dataset edit local file as dataset member panel.
;; ------------------------------------------------

(cl-defun iron-main-epf--dataset-edit-local (session &rest args)
  "Create the IRON MAIN dataset save panel."
  
  (interactive)

  (ignore session args)

  ;; (cl-assert (iron-main-session-p session) t
  ;; 	     "SESSION %S is not a `iron-main-session'"
  ;; 	     session)
  
  (switch-to-buffer "*IRON MAIN dataset edit*")
  
  (kill-all-local-variables)
  
  (let ((inhibit-read-only t))
    (erase-buffer))

  (iron-main-epf-mode)
  
  ;; (iron-main-epf--title-field "Dataset member edit")
  (setq-local header-line-format
	      (iron-main-epf--make-header-line
	       "Local dataset member (file) edit"))
  
  ;; (iron-main-epf--dsname-item-field)
  
  (widget-insert "\n")
  
  (setq iron-main-epf--filename-widget
	(widget-create 'file
		       :format "File: %v\n"
		       :value (iron-main-ds-rep-name
			       iron-main-epf--current-ds)
		       :size (- 72 (length "File: "))
		       :keymap iron-main-epf-editable-field-keymap
		       ;; :notify
		       :action (lambda (w &rest args)
				 (ignore args)
				 (message
				  (format "FILE -- %s"
					  (widget-value w)))
				 (find-file (widget-value w))
				 )
		       :keymap iron-main-epf-editable-field-keymap
		       ))

  (widget-insert iron-main-epf--underline)
  (widget-insert "\n")
  

  (message "IMDS01I: Local dataset edit panel set up.")
  (prog1 (widget-setup)
    ;; (widget-forward 1)
    (iron-main-epf--goto-first-widget)
    )
  )


;; IRON MAIN frame main, top panel.
;; --------------------------------

(defvar-local iron-main-hercules-pid nil
  "The PID of the Hercules process.  NIL if it is not running or reachable.")


(defvar-local iron-main-epf--session nil
  "The session a panel is attached to.")


(cl-defun iron-main-epf--get-session (panel)
  "Get the IRON MAIN session attached to PANEL.

PANEL must be a buffer or a buffer name."
  (with-current-buffer panel
    iron-main-epf--session))


(cl-defun iron-main-epf--instance-banner (session
					  &aux
					  (pid (iron-main-hs-pid session))
					  )
  "Generates a banner with information about the mainframe.

If no connection is available, the banner will indicate so."

  (if pid
      (format "%s running %s on %s:%s (%s) from %s"
	      (iron-main-session-machine session)
	      (iron-main-session-os-flavor session)
	      (iron-main-hs-http-host session)
	      (iron-main-hs-port session)
	      pid
	      (system-name))
    (format "%s running %s not available from %s"
	    (iron-main-session-machine session)
	    (iron-main-session-os-flavor session)
	    (system-name))
    ))


(defvar-local iron-main-epf--hercules-top-cmds-links ()
  "List of link widgets created for top panel commands.")


(cl-defun iron-main-frame (&optional
			   (machine iron-main-machine)
			   (os-flavor iron-main-os-flavor))
  "Creates the \\='top\\=' IRON MAIN panel.

The optional MACHINE and OS-FLAVOR arguments default to the values of
the variables IRON-MAIN-MACHINE and IRON-MAIN-OS-FLAVOR.

Notes:

This function just calls `iron-main-frame-panel'.

See Also:

IRON-MAIN-FRAME-PANEL, IRON-MAIN-MACHINE and IRON-MAIN-OS-FLAVOR."
  
  (interactive)
  (iron-main-frame-panel machine os-flavor))



(defalias 'iron-main-epf 'iron-main-frame
  "Creates the \\='top\\=' IRON MAIN panel.

The optional MACHINE and OS-FLAVOR arguments default to the values of
the variables IRON-MAIN-MACHINE and IRON-MAIN-OS-FLAVOR.

Notes:

This function is an alias for `iron-main-frame'.

See Also:

IRON-MAIN-FRAME, IRON-MAIN-MACHINE and IRON-MAIN-OS-FLAVOR.")


(defalias 'iron-main-frame-epf 'iron-main-frame
  "Creates the \\='top\\=' IRON MAIN panel.

The optional MACHINE and OS-FLAVOR arguments default to the values of
the variables IRON-MAIN-MACHINE and IRON-MAIN-OS-FLAVOR.

Notes:

This function is an alias for `iron-main-frame'.

See Also:

IRON-MAIN-FRAME, IRON-MAIN-MACHINE and IRON-MAIN-OS-FLAVOR.")


(cl-defun iron-main-frame-panel (&optional
				 (machine iron-main-machine)
				 (os-flavor iron-main-os-flavor)
				 &aux
				 (from-buffer (current-buffer))
				 (instance-banner "")
				 )
  "Creates the \\='top\\=' IRON MAIN panel.

The optional MACHINE and OS-FLAVOR arguments default to the values of
the variables IRON-MAIN-MACHINE and IRON-MAIN-OS-FLAVOR."
  
  (interactive)

  (switch-to-buffer
   (format "*IRON MAIN %s - %s*"
	   iron-main-machine
	   iron-main-os-flavor))
  
  (kill-all-local-variables)
  
  ;; (make-local-variable 'panel-iron-main-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))

  (iron-main-epf-mode)

  ;; Init buffer local variables.

  (setq-local header-line-format
	      (iron-main-epf--make-header-line "Top"))
  
  (setq-local iron-main-machine machine)
  (setq-local iron-main-os-flavor os-flavor)
  (setq-local iron-main-epf--back from-buffer)
  (setq-local iron-main-epf--tag "Top")
  (setq-local iron-main-epf--cmds
	      iron-main-epf--hercules-top-commands)
  
  (when (iron-main-running-machine "Hercules")
    ;; Trying to get the PID.
    ;; All of this should be factored out and make machine/os dependent.
    (let ((pid (progn
		 (iron-main-message "EPF" "FP" 1 "I"
				    "Hercules running; getting PID.")
		 (iron-main-hercules-qpid)))
	  (session
	   (iron-main-session-start 'iron-main-hercules-session))
	  )
      (setq-local iron-main-hercules-pid pid)
      (setq-local iron-main-epf--session session)

      (when pid
	(setf (iron-main-hs-pid session) pid))

      (setf instance-banner
	    (iron-main-epf--instance-banner session))

      ;; Setting the Hercules OS directories.
      ;; Most setups (e.g., tk4- and Jay Moseleys's) assume a
      ;; directory where the OS resides, with a "dasd" subdirectory.

      (let ((os-dir
	     (read-directory-name (format
				   "IRON MAIN: %S resident folder: "
				   iron-main-os-flavor)
				  nil
				  nil
				  t))
	    )
	(when (file-exists-p
	       (file-name-as-directory
		(expand-file-name os-dir)))
	  (setq-local iron-main-hercules-os-dir
		      (file-name-as-directory
		       (expand-file-name os-dir)))
	  (setf (iron-main-hs-os-dir session)
		iron-main-hercules-os-dir))
	
	)
      (let ((dasd-dir
	     (read-directory-name (format
				   "IRON MAIN: %S DASD folder: "
				   iron-main-os-flavor)
				  iron-main-hercules-os-dir
				  nil
				  t
				  "dasd"))
	    )
	(when (file-exists-p
	       (file-name-as-directory
		(expand-file-name dasd-dir)))
	  (setq-local iron-main-hercules-dasd-dir
		      (file-name-as-directory
		       (expand-file-name dasd-dir)))
	  (setf (iron-main-hs-dasd-dir session)
		iron-main-hercules-dasd-dir))
	
	)
      ))

  ;; Not running Hercules.
  (when (not (iron-main-running-machine "Hercules"))
    (message "IMFPF02I: Hercules not running.")
    )

  ;; Let's start!
  
  ;; (iron-main-epf--title-field)		; This will become useless.
  
  (when (iron-main-running-machine "Hercules")

    ;; (iron-main-epf--hercules-top-subpanel
    ;;  iron-main-hercules-os-dir
    ;;  iron-main-hercules-dasd-dir)

    (widget-insert "\n")
    (iron-main-epf--command-field)
  
    (setq-local iron-main-epf--cmds-links
		(iron-main-epf--insert-command-widgets
		 iron-main-epf--cmds))
    )
  (widget-insert "\n\n")
  (widget-insert iron-main-epf--underline)
  (widget-insert "\n")
  (widget-insert instance-banner)
  (widget-insert "\n")


  ;; (iron-main-help-field) ; Not yet.
  (prog1 (widget-setup)
    ;; (widget-forward 1)
    (iron-main-epf--goto-first-widget)
    )
  (iron-main-message "EPF" "IMF" 0 "I"
		     "my Emacs thinks it's an ISPF!")
  )


(cl-defun iron-main-epf--hercules-top-subpanel
    (&optional (os-dir iron-main-hercules-os-dir)
               (dasd-dir iron-main-hercules-dasd-dir))
                                                    
  "Internal function setting up the Hercules part of the top panel.

The arguments OS-DIR and DASD-DIR are referring to the directories
where the relevant bits and pieces used by the emulator can be found."

  ;; This function is just a code organization/refactoring tool.   Do
  ;; not call by itself.

  ;; (widget-insert "OS   ")
  (widget-create 'directory
		 :value os-dir
		 :tag "OS   "
		 ;; :tag (iron-main--shorten-pathname iron-main-hercules-os-dir)
		 ;; :format "%v"
		 ;; :entry-format "V %v"
		 :keymap iron-main-epf-editable-field-keymap
		 :size (+ 4
			  (max (length os-dir)
			       (length dasd-dir))))
  (widget-insert "\n")
  ;; (widget-insert "DASDs")
  (widget-create 'directory
		 :value dasd-dir
		 :tag "DASDs"
		 ;; :tag (iron-main--shorten-pathname iron-main-hercules-dasd-dir)
		 ;; :format "%v"
		 :keymap iron-main-epf-editable-field-keymap
		 :size (+ 4
			  (max (length os-dir)
			       (length dasd-dir))))
  (widget-insert "\n")
  (widget-create 'iron-main-natnum-widget
		 :value iron-main-hercules-http-port
		 :tag "HTTP Port"
		 :format " %v "
		 :keymap iron-main-epf-editable-field-keymap
		 :size 8)
  (widget-insert "\n")
  (widget-create 'iron-main-natnum-widget
		 :value iron-main-hercules-card-reader-port
		 :tag "Reader Port"
		 :format " %v "
		 :keymap iron-main-epf-editable-field-keymap
		 :size 8)
  (widget-insert "\n\n")
  (widget-insert iron-main-epf--overline)
  (widget-insert "\n")
  )


;; IRON MAIN System panel.
;; -----------------------

(defvar-local iron-main-epf--hs-devinfo-ins-pt nil) ; This may go away.


(defconst iron-main-epf--+hercules-devtypes+
  '("CTCA"
    "DASD"
    "DSP"
    "FCP"
    "LINE"
    "OSA"
    "PCH"
    "PRT"
    "RDR"
    "TAPE"
    )
  "List of available Hercules device types." ; Possibly IBM ones.
  )


(defconst iron-main-epf--hercules-dev-commands
  `(("CTCA" iron-main-epf--hercules-devs-ctca
     :header "List of CTCA devices")
    ("DASD" iron-main-epf--hercules-devs-dasd
     :header "List of DASD devices")
    ("DSP" iron-main-epf--hercules-devs-dsp
     :header "List of DSP devices")
    ("FCP" iron-main-epf--hercules-devs-fcp
     :header "List of FCP devices")
    ("LINE" iron-main-epf--hercules-devs-line
     :header "List of LINE devices")
    ("OSA" iron-main-epf--hercules-devs-osa
     :header "List of OSA devices")
    ("PCH" iron-main-epf--hercules-devs-pch
     :header "List of Punch (PCH) devices")
    ("PRT" iron-main-epf--hercules-devs-prt
     :header "Printer List (PRT devices)")
    ("RDR" iron-main-epf--hercules-devs-rdr
     :header "Reader List (RDR devices)")
    ("TAPE" iron-main-epf--hercules-devs-tape
     :header "Tape List (TAPE devices)")
    )
  "A \\='commands alist\\=' for the IRON MAIN devices panel."
  )


(cl-defun iron-main-epf--hercules-clean-devlist (devtype
						 devlist-string)
  "Remove noise that is generated for a teminal output and format.

DEVTYPE is one of the \"devlist\" possible arguments; DEVLIST-STRING
is the string result from invoking the command to the running
Hercules."

  ;; To understand this processing, check the output of the Hercules
  ;; command "devlist DEVTYPE".

  (if (or (null devlist-string) (string= "" devlist-string))
      ""
    ;; Readability first!!!
    (let ((result "")
	  )
      ;; Remove information header.
      (setq result
	    (replace-regexp-in-string
	     "HHC0160[23]I +devlist.*$"
	     ""
	     devlist-string))
    
      ;; Remove extra message for empty lists.
      (setq result
	    (replace-regexp-in-string
	     "HHC00007I.+$"
	     ""
	     result))

      ;; Remove "Empty list" mgs type.
      (setq result
	    (replace-regexp-in-string
	     "HHC02312W "
	     ""
	     result))

      ;; Remove list lines msg type.
      (setq result
	    (replace-regexp-in-string
	     "HHC02279I "
	     ""
	     result))

      ;; Remove blank lines (if it works).
      (setq result
	    (replace-regexp-in-string (rx bol ?\n)
				      ""
				      result))
      result
      )))


(cl-defun iron-main-epf--format-dasd-list (dasdlist)
  "Format the string DASDLIST in columns."
  ;; Very simple minded FTTB.
  ;;
  ;; `dasdlist' is a string with lines like:
  ;;
  ;; 0:0133 2314 dasd/sort03.133 [203 cyls] [0 sfs] IO[23] open
  ;; 0:0134 2314 dasd/sort04.134 [203 cyls] [0 sfs] IO[23] open
  ;;
  ;; where the 7 columns are:
  ;;
  ;; DEVID MODEL HOSTFOLDER CYL SFS IO(channels?) STATUS

  (with-output-to-string
    (let (
	  ;; I should rewrite the next regexp breaking it up and using
	  ;; RX.
	  (col-regexp
	   "\\([:0-9A-Z]+\\) \\([0-9]+\\) \\(.+\\) \\(\\[[^]]+?\\]\\) \\(\\[[^]]+?\\]\\) \\(IO\\[[^]]+?\\]\\) \\(.+\\)")

	  (col-rx
	   (rx (group (+ (any ?: "0-9" "A-Z")))
	       " "
	       (group (+ (any "0-9")))
	       " "
	       (group (+ nonl))
	       " "
	       "["
	       (group (+ (any "0-9")))
	       " cyls"
	       "]"
	       " "
	       "["
	       (group (+ (any "0-9")))
	       " sfs"
	       "]"
	       " "
	       "IO["
	       (group (+ (any "0-9")))
	       "]"		    
	       " "
	       (group (+ nonl))))

	  (dasd-lines (split-string dasdlist "\n"))
	  )

      (ignore col-regexp)		; Left in FTTB.

      (dolist (dasd-line dasd-lines)
	(iron-main-message "EPF" "FDL" 0 "I"
			   "DASD line: |%s|"
			   dasd-line)
	(unless (string= "" dasd-line)
	  (string-match col-rx dasd-line)
	  (let ((devid (match-string 1 dasd-line))
		(model (match-string 2 dasd-line))
		(host-folder (match-string 3 dasd-line))
		(cyl (match-string 4 dasd-line))
		(sfs (match-string 5 dasd-line))
		(io-channels (match-string 6 dasd-line))
		(status (match-string 7 dasd-line))
		)
	    (iron-main-message "EPF" "FDL" 0 "I"
			       "DASD: |%s|\t|%s|\t|%s|\t|[%3s sfs]|\t|%s|\t|%6s|\t|%s"
			       devid
			       model
			       cyl
			       sfs
			       io-channels
			       status
			       host-folder)

	    ;; Maybe reuse the format string for the header line.
	    
	    (princ (format "%6s\t%5s\t%4s\t%3s\t%11s\t%6s\t%s\n"
			   devid
			   model
			   cyl
			   sfs
			   io-channels
			   status
			   host-folder))
	    )))
      )))


(cl-defun iron-main-epf--format-dsp-list (dsplist)
  "Format the string DSPLIST in columns."
  ;; Very simple minded FTTB.
  ;;
  ;; `dasdlist' is a string with lines like:
  ;;
  ;; 0:03C0 3270 GROUP=TCAM IO[6]
  ;;
  ;; where the 4 columns are:
  ;;
  ;; DEVID MODEL GROUP IO(channels?)

  (with-output-to-string
    (let ((col-rx
	   (rx (group (+ (any ?: "0-9" "A-Z")))
	       " "
	       (group (+ (any "0-9")))
	       " "
	       (group (+ nonl))
	       " "
	       "IO["
	       (group (+ (any "0-9")))
	       "]"		    
	       ))

	  (dsp-lines (split-string dsplist "\n"))
	  )

      (dolist (dsp-line dsp-lines)
	(iron-main-message "EPF" "FDL" 0 "I"
			   "DSP line: |%s|"
			   dsp-line)
	(unless (string= "" dsp-line)
	  (string-match col-rx dsp-line)
	  (let ((devid (match-string 1 dsp-line))
		(model (match-string 2 dsp-line))
		(group (match-string 3 dsp-line))
		(io-channels (match-string 4 dsp-line))
		)
	    (iron-main-message "EPF" "FDL" 0 "I"
			       "DSP: |%s|\t|%s|\t|%s|\t|%s|"
			       devid
			       model
			       group
			       io-channels
			       )

	    ;; Maybe reuse the format string for the header line.
	    
	    (princ (format "%6s\t%5s\t%10s\t%11s\n"
			   devid
			   model
			   group
			   io-channels
			   ))
	    )))
      )))


;; (cl-defun iron-main-epf--format-dasd-list (dasdlist)
;;   "Format the string DASDLIST in columns."
;;   ;; Very simple minded FTTB.
;;   ;;
;;   ;; `dasdlist' is a string with lines like:
;;   ;;
;;   ;; 0:0133 2314 dasd/sort03.133 [203 cyls] [0 sfs] IO[23] open
;;   ;; 0:0134 2314 dasd/sort04.134 [203 cyls] [0 sfs] IO[23] open
;;   ;;
;;   ;; where the 7 columns are:
;;   ;;
;;   ;; DEVID MODEL HOSTFOLDER CYL SFS IO(channels?) STATUS

;;   (let ((result "")
;; 	;; I should rewrite the next regexp breaking it up and using
;; 	;; RX.
;; 	(col-regexp
;; 	 "\\([:0-9A-Z]+\\) \\([0-9]+\\) \\(.+\\) \\(\\[[^]]+?\\]\\) \\(\\[[^]]+?\\]\\) \\(IO\\[[^]]+?\\]\\) \\(.+\\)")
;; 	(result-regexp
;; 	 "\\1\t\\2\t\\3\t\\4\t\\5\t\\6\t\\7")
;; 	)
;;     (setf result
;; 	  (replace-regexp-in-string col-regexp
;; 				    result-regexp
;; 				    dasdlist))
    
;;     ;; Not quite right yet, but gettng there...
;;     (format "%s\n%s"
;; 	    "DEVID\tMODEL\tHOST FOLDER\tCYL\tSFS\tIO(channels)\tSTATUS"
;; 	    result)
;;     ))


;; iron-main-epf--hercules-system
;; New version shaped like a command panel.

(cl-defun iron-main-epf--hercules-system (session &rest args)
  "Hercules system inspection panel.

Given a SESSION sets up the \"system\" panel.  ARGS are passed
downstream if needed."

  (ignore args)

  (cl-labels
      ((hercules-devlist (devtype)
	 (let ((dev-list
		(iron-main-hercules-devlist devtype))
	       )
	   (iron-main-epf--hercules-clean-devlist
	    devtype
	    dev-list))
	 )
       
       (systems-panel-setup (&rest args)

	 (ignore args)

	 (setq-local iron-main-epf--tag "Hercules system")

	 (setq-local iron-main-epf--cmds
		     iron-main-epf--hercules-dev-commands)
  
	 ;; (iron-main-epf--title-field "Hercules system")
	 (setq-local header-line-format
		     (iron-main-epf--make-header-line
		      "Hercules system"))

	 ;; Let's start!

	 ;; Devices retrievable from the Hercules command 'devlist'.
	 ;; CTCA, DASD, DSP, FCP, LINE, OSA, PCH, PRT, RDR, and TAPE.

	 (let ((devtypes iron-main-epf--+hercules-devtypes+))
          
	   (widget-insert "Available devices\n\n")

	   (setq-local iron-main-epf--cmd-links
		       (iron-main-epf--insert-command-widgets
			iron-main-epf--hercules-dev-commands))

	   (widget-insert "\n\n")
      
	   (iron-main-message "EPF" "HS" 0 "I" "Hercules system.")
	   (prog1 (widget-setup)
	     (widget-forward 1))
	   ))
       )
    (iron-main-epf--make-panel session
			       "*IRON MAIN Hercules system*"
			       :setup
			       #'systems-panel-setup)
    ))


(cl-defun iron-main-epf--devs-panel (devtype devlist)
  (iron-main-message "EPF" "DP" 0 "I"
		     "devtype %s."
		     devtype)
  (iron-main-message "EPF" "DP" 0 "I"
		     "devlist %s."
		     devlist))


(cl-defun iron-main-epf--hercules-devlist (devtype)
  (iron-main-epf--hercules-clean-devlist
   devtype
   (iron-main-hercules-devlist devtype)))


;; The following functions may eventually benefit from using
;; tabulated-list-mode.

(cl-defun iron-main-epf--dev-listing-panel-setup
    (&rest
     args
     &key
     (tag "DEVTYPE")
     (header-line " DEVICE")
     (devlist-format-func #'identity)
     &allow-other-keys
     )
  (ignore args)
	 
  (setq-local iron-main-epf--tag (format "Hercules %s" tag))

  (setq-local tab-line-format
	      (iron-main-epf--make-header-line
	       (format " Hercules %s" tag)))

  (setq-local
   header-line-format
   header-line
   ;; " DASD"
   ;; " DEVID \tModel\tCYL\tSFS\tIO(channels)\tStatus\tHost Folder"
   )

  (let* ((devlist (iron-main-epf--hercules-devlist tag))
	 (devs (funcall devlist-format-func devlist))
	 )
    (iron-main-message "EPF" "HDD" 0 "I"
		       "%ss\n%s"
		       tag
		       devlist)
    (cond ((not (string= "" devs))
	   (goto-char (point-min))
	   (save-excursion
	     (let ((inhibit-read-only t)
		   (inhibit-modification-hooks t)
		   )
	       (delete-region (point-min) (point-max))))
	   (widget-insert devs)
	   )
	  (t
	   (widget-insert (format "\n%ss will appear here\n" tag)))
	  ))
  )


(cl-defun iron-main-epf--dev-listing-panel-setup-full
    (session
     &rest
     args
     &key
     (tag "DEVTYPE")
     (header-line " DEVICE")
     (devlist-format-func #'identity)
     &allow-other-keys
     )
  (ignore args)

  (cl-flet
      ((listing-panel-setup (&rest args)

	 (ignore args)

	 (setq-local iron-main-epf--tag (format "Hercules %s" tag))

	 (setq-local tab-line-format
		     (iron-main-epf--make-header-line
		      (format " Hercules %s" tag)))

	 (setq-local
	  header-line-format
	  header-line
	  ;; " DASD"
	  ;; " DEVID \tModel\tCYL\tSFS\tIO(channels)\tStatus\tHost Folder"
	  )

	 (let* ((devlist (iron-main-epf--hercules-devlist tag))
		(devs (funcall devlist-format-func devlist))
		)
	   (iron-main-message "EPF" "HDD" 0 "I"
			      "%ss\n%s"
			      tag
			      devlist)
	   (cond ((not (string= "" devs))
		  (goto-char (point-min))
		  (save-excursion
		    (let ((inhibit-read-only t)
			  (inhibit-modification-hooks t)
			  )
		      (delete-region (point-min) (point-max))))
		  (widget-insert devs)
		  )
		 (t
		  (widget-insert (format "\n%ss will appear here\n" tag)))
		 ))
	 )
       )
    (iron-main-epf--make-panel session
			       (format "*IRON MAIN Hercules %s*" tag)
			       :setup
			       #'listing-panel-setup)
    ))


(cl-defun iron-main-epf--hercules-devs-ctca (session &rest args)
  "Gives SESSION it sets up the panel displaying the available CTCAs."
  (ignore args)
  (cl-labels
      ((ctca-panel-setup (&rest args)
	 (ignore args)

	 (setq-local iron-main-epf--tag "Hercules CTCA")

	 (setq-local tab-line-format
		     (iron-main-epf--make-header-line " Hercules CTCA"))

	 (setq-local header-line-format " CTCA")

	 (let ((ctca-devs (iron-main-epf--hercules-devlist "CTCA")))
	   (cond ((not (string= "" ctca-devs))
		  (goto-char (point-min))
		  (save-excursion
		    (let ((inhibit-read-only t)
			  (inhibit-modification-hooks t)
			  )
		      (delete-region (point-min) (point-max))))
		  (save-excursion
		    (widget-insert ctca-devs)
		    ))
		 (t
		  (widget-insert "\nCTCAs will appear here\n"))
		 ))
	 )
       )
    (iron-main-epf--make-panel session
			       "*IRON MAIN Hercules CTCA*"
			       :setup
			       #'ctca-panel-setup)
    ))


(cl-defun iron-main-epf--hercules-devs-dasd (session &rest args)
  "Gives SESSION it sets up the panel displaying the available DASDs."
  (ignore args)
  (cl-labels
      ((dasd-panel-setup (&rest args)
	 (ignore args)

	 
	 (setq-local iron-main-epf--tag "Hercules DASD")

	 (setq-local tab-line-format
		     (iron-main-epf--make-header-line " Hercules DASD"))

	 (setq-local
	  header-line-format
	  ;; " DASD"
	  " DEVID \tModel\tCYL\tSFS\tIO(channels)\tStatus\tHost Folder"
	  )

	 (let* ((dasd-devlist (iron-main-epf--hercules-devlist "DASD"))
		(dasd-devs (iron-main-epf--format-dasd-list dasd-devlist))
		)
	   (iron-main-message "EPF" "HDD" 0 "I"
			      "DASDs\n%s"
			      dasd-devlist)
	   (cond ((not (string= "" dasd-devs))
		  (goto-char (point-min))
		  (save-excursion
		    (let ((inhibit-read-only t)
			  (inhibit-modification-hooks t)
			  )
		      (delete-region (point-min) (point-max))))
		  (widget-insert dasd-devs)
		  )
		 (t
		  (widget-insert "\nDASDs will appear here\n"))
		 ))
	 )
       )
    (iron-main-epf--make-panel session
			       "*IRON MAIN Hercules DASD*"
			       :setup
			       #'dasd-panel-setup)
    ))


(cl-defun iron-main-epf--hercules-devs-dsp (session &rest args)
  "Gives SESSION it sets up the panel displaying the available DSPs."
  (ignore args)
  (cl-labels
      ((dsp-panel-setup (&rest args)
	 (ignore args)

	 
	 (setq-local iron-main-epf--tag "Hercules DSP")

	 (setq-local tab-line-format
		     (iron-main-epf--make-header-line " Hercules DSP"))

	 (setq-local
	  header-line-format
	  ;; " DASD"
	  " DEVID \tModel\tGroup\tIO(channels)"
	  )

	 (let* ((dsp-devlist (iron-main-epf--hercules-devlist "DSP"))
		(dsp-devs (iron-main-epf--format-dsp-list dsp-devlist))
		)
	   (iron-main-message "EPF" "HDD" 0 "I"
			      "DSPs\n%s"
			      dsp-devlist)
	   (cond ((not (string= "" dsp-devs))
		  (goto-char (point-min))
		  (save-excursion
		    (let ((inhibit-read-only t)
			  (inhibit-modification-hooks t)
			  )
		      (delete-region (point-min) (point-max))))
		  (widget-insert dsp-devs)
		  )
		 (t
		  (widget-insert "\nDSPs will appear here\n"))
		 ))
	 )
       )
    (iron-main-epf--make-panel session
			       "*IRON MAIN Hercules DSP*"
			       :setup
			       #'dsp-panel-setup)
    ))


(cl-defun iron-main-epf--hercules-devs-fcp (session &rest args)
  "Gives SESSION it sets up the panel displaying the available FCPs."
  (ignore args)
  (cl-flet
      ((fcp-panel-setup (&rest args)
	 (apply #'iron-main-epf--dev-listing-panel-setup
		:tag "FCP"
		:header-line " FCP"
		args))
	 )
    (iron-main-epf--make-panel session
			       "*IRON MAIN Hercules FCP*"
			       :setup
			       #'fcp-panel-setup)
    ))


(cl-defun iron-main-epf--hercules-devs-line (session &rest args)
  "Gives SESSION it sets up the panel displaying the available LINEs."
  (ignore args)
  (cl-flet
      ((line-panel-setup (&rest args)
	 (apply #'iron-main-epf--dev-listing-panel-setup
		:tag "LINE"
		:header-line " LINE"
		args))
	 )
    (iron-main-epf--make-panel session
			       "*IRON MAIN Hercules LINE*"
			       :setup
			       #'line-panel-setup)
    ))


(cl-defun iron-main-epf--hercules-devs-osa (session &rest args)
  "Gives SESSION it sets up the panel displaying the available OSAs."
  (ignore args)
  (cl-flet
      ((osa-panel-setup (&rest args)
	 (apply #'iron-main-epf--dev-listing-panel-setup
		:tag "OSA"
		:header-line " OSA"
		args))
	 )
    (iron-main-epf--make-panel session
			       "*IRON MAIN Hercules OSA*"
			       :setup
			       #'osa-panel-setup)
    ))


(cl-defun iron-main-epf--hercules-devs-pch (session &rest args)
  "Gives SESSION it sets up the panel displaying the available PCHs."
  (ignore args)
  (cl-flet
      ((pch-panel-setup (&rest args)
	 (apply #'iron-main-epf--dev-listing-panel-setup
		:tag "PCH"
		:header-line " PCH"
		args))
	 )
    (iron-main-epf--make-panel session
			       "*IRON MAIN Hercules PCH*"
			       :setup
			       #'pch-panel-setup)
    ))


(cl-defun iron-main-epf--hercules-devs-prt (session &rest args)
  "Gives SESSION it sets up the panel displaying the available PRTs."
  (ignore args)
  (cl-flet
      ((prt-panel-setup (&rest args)
	 (apply #'iron-main-epf--dev-listing-panel-setup
		:tag "PRT"
		:header-line " PRT"
		args))
	 )
    (iron-main-epf--make-panel session
			       "*IRON MAIN Hercules PRT*"
			       :setup
			       #'prt-panel-setup)
    ))


(cl-defun iron-main-epf--hercules-devs-rdr (session &rest args)
  "Gives SESSION it sets up the panel displaying the available RDRs."
  (ignore args)
  (cl-flet
      ((rdr-panel-setup (&rest args)
	 (apply #'iron-main-epf--dev-listing-panel-setup
		:tag "RDR"
		:header-line " RDR"
		args))
	 )
    (iron-main-epf--make-panel session
			       "*IRON MAIN Hercules RDR*"
			       :setup
			       #'rdr-panel-setup)
    ))


;; (cl-defun iron-main-epf--hercules-devs-tape (session &rest args)
;;   "Gives SESSION it sets up the panel displaying the available TAPEs."
;;   (ignore args)
;;   (cl-flet
;;       ((tape-panel-setup (&rest args)
;; 	 (apply #'iron-main-epf--dev-listing-panel-setup
;; 		:tag "TAPE"
;; 		:header-line " TAPE"
;; 		args))
;; 	 )
;;     (iron-main-epf--make-panel session
;; 			       "*IRON MAIN Hercules TAPE*"
;; 			       :setup
;; 			       #'tape-panel-setup)
;;     ))


(cl-defun iron-main-epf--hercules-devs-tape (session &rest args)
  "Gives SESSION it sets up the panel displaying the available TAPEs."
  (ignore args)

  (iron-main-epf--dev-listing-panel-setup-full
   session
   :tag "TAPE"
   :header-line " TAPE"
   ))


;; IRON MAIN Emacs SP panel.
;; -------------------------

(cl-defun iron-main-epf--emacs-hercules-subpanel
    (session &aux
	     (os-dir (iron-main-hs-os-dir session))
             (dasd-dir (iron-main-hs-dasd-dir session))
	     (hercules-pid (iron-main-hs-pid session))
	     )
                                                    
  "Sets up the Hercules part of the Emacs PF panel.

The arguments OS-DIR and DASD-DIR are referring to the directories
where the relevant bits and pieces used by the emulator can be found.
HERCULES-PID is the emulator process id or NIL if none is running."

  ;; This function is just a code organization/refactoring tool.   Do
  ;; not call by itself.

  (widget-insert "\n")
  (if hercules-pid
      ;; We have an Hercules running?
      (widget-insert (format "Hercules process id: %s\n"
			     hercules-pid))
    (widget-insert "No Hercules process running\n"))
  (widget-insert "\n")
   
  (widget-create 'directory
		 :value os-dir
		 ;; :tag "OS   "
		 ;; :tag (iron-main--shorten-pathname iron-main-hercules-os-dir)
		 :format "OS Folder:    %v "
		 ;; :entry-format "V %v"
		 :keymap iron-main-epf-editable-field-keymap
		 :size (+ 4
			  (max (length os-dir)
			       (length dasd-dir))))
  (widget-insert "\n")
  (widget-create 'directory
		 :value dasd-dir
		 ;; :tag "DASDs"
		 ;; :tag (iron-main--shorten-pathname iron-main-hercules-dasd-dir)
		 :format "DASDs Folder: %v "
		 :keymap iron-main-epf-editable-field-keymap
		 :size (+ 4
			  (max (length os-dir)
			       (length dasd-dir))))
  (widget-insert "\n\n")
  (widget-create 'iron-main-natnum-widget
		 :value iron-main-hercules-http-port
		 :tag "HTTP Port"
		 :format "HTTP Port:    %v "
		 :keymap iron-main-epf-editable-field-keymap
		 :size 8)
  (widget-insert "\n")
  (widget-create 'iron-main-natnum-widget
		 :value iron-main-hercules-card-reader-port
		 :tag "Reader Port"
		 :format "Reader Port:  %v "
		 :keymap iron-main-epf-editable-field-keymap
		 :size 8)
  (widget-insert "\n\n")
  )


(cl-defun iron-main-epf--emacs-pf-system (session &rest args)
  "Emacs PF system ispection panel.

Given a SESSION shows information about the current Emacs SP
connection to the mainframe, if any."

  (ignore args)
  
  (cl-assert (iron-main-session-p session) t
	     "IMFEP0E: SESSION %S is not a `iron-main-session'"
	     session)

  (switch-to-buffer "*IRON MAIN Emacs PF*")

  (kill-all-local-variables)

  (let ((inhibit-read-only t))
    (erase-buffer))
  
  (iron-main-epf-mode)

  ;; Now that we have the session, some of these are repetitions.

  (setq-local iron-main-epf--session session)
  
  (setq-local iron-main-machine
	      (iron-main-session-machine session))
  (setq-local iron-main-os-flavor
	      (iron-main-session-os-flavor session))

  (setq-local iron-main-epf--tag "IRON MAIN Emacs PF")
  
  ;; (iron-main-epf--title-field "Hercules help")
  (setq-local header-line-format
	      (iron-main-epf--make-header-line "Emacs PF"))

  (iron-main-epf--emacs-hercules-subpanel session)

  (message "IMFEPE0I: Emacs PF panel.")
  (prog1 (widget-setup)
    ;; (widget-forward 1)
    (iron-main-epf--goto-first-widget)
    )
  )

;; IRON MAIN Help panel.
;; ---------------------

(defvar-local iron-main-epf--help-ins-pt nil)
(defvar-local iron-main-epf--help-cmd-widget nil)

(cl-defun iron-main-epf--hercules-clean-help (cmd helpstring)
  "Remove extra noise that is generated for a teminal output.

The Hercules \"help\" command formats its output assuming a
teminal output; HELPSTRING contains such output for command CMD
and is cleaned up for presentation in the IRON MAIN panel/buffer."

  ;; To understand this processing, check the output of the Hercules
  ;; commands "help" and "help <cmd>".

  ;; Readability first!!!
  (let ((result ""))
    ;; Remove line headers.
    (setf result
	  (replace-regexp-in-string
	   "HHC0160[23]I "
	   ""
	   helpstring))

    ;; Remove header line.
    (setf result
	  (replace-regexp-in-string
	   "^help *[a-zA-Z0-9]*$"
	   ""
	   result))

    ;; Remove extra help line in "help" result.
    (setf result
	  (replace-regexp-in-string
	   "^HHC01610I .+$"
	   ""
	   result))

    (substring result (string-search cmd result))
    ))


(cl-defun iron-main-epf--hercules-help (session &rest args)
  "Hercules help panel.

Given a SESSION sets up the \"help\" panel.  ARGS are passed
downstream if needed."

  (ignore args)

  (cl-labels
      ((help-top-panel-setup (&rest args)

	 (ignore args)
	    
	 ;; Assuming we are in `current-buffer'.
	    
	 (setq-local iron-main-epf--tag "Hercules help")
  
	 ;; (iron-main-epf--title-field "Hercules help")
	 (setq-local header-line-format
		     (iron-main-epf--make-header-line "Hercules help"))

	 ;; Let's start!

	 (setq-local iron-main-epf--help-ins-pt (point))

	 ;; (message ">>> Point %s" help-ins-pt)
	 (setq-local
	  iron-main-epf--help-cmd-widget
	  (widget-create
	   'editable-field
	   :size 10
	   :value ""			; Initial value.
	   :format "Hercules command (empty for a list): %v "

	   :keymap
	   iron-main-epf-editable-field-keymap

	   :action
	   #'help-subpanel-setup-action	; Lambda
	   )
	  )
	    	
	 (widget-insert "\n")
	 (setq iron-main-epf--help-ins-pt (point))
	    
	 (message "IMPHH0I: Hercules help.")
	 (prog1 (widget-setup)
	   ;; (widget-forward 1)
	   (iron-main-epf--goto-first-widget)
	   )
	 )

       (help-subpanel-setup-action (w &rest ignore)
	 (ignore ignore)
	 (if iron-main-hercules-pid
	     (let ((cmd (widget-value w)))
	       (if (string-equal "" cmd)
		   (iron-main-message "EPF" "HH" 0 "I"
				      "available commands... (%s)"
				      iron-main-epf--help-ins-pt)
		 (iron-main-message "EPF" "HH" 0 "I"
				    "getting help for '%s' (%s)."
				    cmd
				    iron-main-epf--help-ins-pt)
		 )
	       (let ((helpstring
		      (iron-main-hercules-help cmd :check-listening t))
		     )
		 (iron-main-epf--make-subpanel
		  "*IRON MAIN Hercules help display*"
		  #'help-subpanel-setup
		  :header-line
		  (if (string-equal "" cmd)
		      "Commands"
		    (format "Command\tDescription"))
		  :cmd cmd
		  :helpstring helpstring
		  )
		 ))
	   ;; pid NIL; no connection.
	   (iron-main-message "EPF" "HH" 0 "W" "%s."
			      (iron-main-epf--instance-banner session))
	   ))

       (help-subpanel-setup (&key
			     (helpstring "")
			     (cmd "")
			     &allow-other-keys)
	 (iron-main-message "EPF" "HH" 1 "I"
			    "helpstring '%s'"
			    helpstring)
	 (if helpstring
	     (insert (iron-main-epf--hercules-clean-help
		      cmd
		      helpstring)
		     )
	   (insert (format "\nHelp for %s would appear here (%s)"
			   cmd
			   (current-time-string)))
	   )

	 (help-mode)
	 ;; (iron-main-mode)
	 (use-local-map iron-main-epf--test-subpanel-keymap)
	 )
       )				; labels

    (iron-main-epf--make-panel
     session
     "*IRON MAIN Hercules help*"
     :setup
     #'help-top-panel-setup
     )))


;;; Panels and Subpanels.
;;; =====================
;;;
;;; Panels and subpanels are, FTTB, organized as columns of windows.
;;; The idea id to have a "main" panel with "subpanels".  Panels are
;;; actually buffers.
;;;
;;; The buffer local variables serve as fields.

(defvar-local iron-main-epf--is-top-panel nil
  "Indicates whether the buffer/panel/window is a \\='top\\=' one.")


(defvar-local iron-main-epf--parent-panel nil
  "Reference to the \\='parent\\=' panel or NIL.")


(defvar-local iron-main-epf--top-panel-window nil
  "Reference to the window of the \\='top\\=' panel or NIL.")


(defvar-local iron-main-epf--subpanel-window nil
  "Reference to the window of the \\='lower\\=' subpanel or NIL.")


(defvar-local iron-main-epf--panel-saved-modeline nil
  "Saved modeline for panel.

Creating a subpanel may remove the buffer modeline; removing the
subpanel must restore it.")


(cl-defun iron-main-epf--panel-subpanels (subpanel-root)
  (cl-assert (bufferp subpanel-root) t)

  (with-current-buffer subpanel-root
    (if (null iron-main-epf--subpanel-window)
	(list subpanel-root)
      (cons subpanel-root
	    (iron-main-epf--panel-subpanels
	     (get-buffer-window subpanel-root))))))


(cl-defun iron-main-epf--panel-superpanels (subpanel-leaf)
  (cl-assert (bufferp subpanel-leaf) t)

  (cl-labels ((get-super (subpanel-leaf superpanels)

		(with-current-buffer subpanel-leaf
		  (if iron-main-epf--is-top-panel
		      (reverse (cons subpanel-leaf superpanels))
		    (get-super iron-main-epf--parent-panel
			       (cons subpanel-leaf superpanels)))
		  ))
	      )
    (get-super subpanel-leaf ())))



(cl-defun iron-main-epf--dump-panel (panel)
  (with-current-buffer panel
    (message "*")
    (iron-main-message "EPF" "DP" 1 "I" "%s panel." panel)
    (iron-main-message "EPF" "DP" 1 "I"
		       "panel window %s."
		       (get-buffer-window panel))
    (iron-main-message "EPF" "DP" 1 "I"
		       "is top panel %s." iron-main-epf--is-top-panel)
    (iron-main-message "EPF" "DP" 1 "I"
		       "saved modeline: %s."
		       (not (null iron-main-epf--panel-saved-modeline)))
    (iron-main-message "EPF" "DP" 1 "I"
		       "modeline: %s."
		       (not (null mode-line-format)))
    (iron-main-message "EPF" "DP" 1 "I"
		       "parent panel %s." iron-main-epf--parent-panel)
    (iron-main-message "EPF" "DP" 1 "I"
		       "top panel window %s." iron-main-epf--top-panel-window)
    (iron-main-message "EPF" "DP" 1 "I"
		       "subpanel window %s." iron-main-epf--subpanel-window)
    (message "*")
    ))


(cl-defun iron-main-epf--make-panel (session
				     buffer-or-name
				     &rest keys
				     &key
				     (setup (lambda (&rest args)
					      (ignore args)
					      nil))
				     
				     &allow-other-keys)
  "Makes a \\='top level\\=' panel.

The function takes the SESSION, a BUFFER-OR-NAME and creates its
contents (widgets) by invoking the function SETUP on the keys passed
to the call (sans the key SETUP).  SETUP, if not
defined via `cl-defun', can be a lambda essentially taking a &rest
p-list; the value returned by SETUP-FUNCTION is ignored.

`iron-main-epf--make-panel' switches to BUFFER-OR-NAME, sets up
the panel-to-panel navigation, initializes the subpanel local
variables and returns the buffer BUFFER-OR-NAME."
  ;; (ignore args)
  
  (cl-assert (iron-main-session-p session) t
	     "IMEPF0E: SESSION %S is not a `iron-main-session'"
	     session)

  (message "*")
  (iron-main-message "EPF" "MP" 0 "I"
		     "making panel %s."
		     buffer-or-name)

  (switch-to-buffer buffer-or-name)
    
  (kill-all-local-variables)
    
  (let ((inhibit-read-only t))
    (erase-buffer))

  (iron-main-epf-mode)

  ;; Now that we have the session, some of these are repetitions.

  (setq-local iron-main-epf--session session)
  
  (setq-local iron-main-machine
	      (iron-main-session-machine session))
  (setq-local iron-main-os-flavor
	      (iron-main-session-os-flavor session))

  (setq-local iron-main-hercules-pid
	      (iron-main-hs-pid session))

  ;; Panel setup.
  ;; `setq-local' takes only two arguments in older Emacsen.
  
  (setq-local iron-main-epf--is-top-panel t)
  (setq-local iron-main-epf--parent-panel nil)
  (setq-local iron-main-epf--subpanel-window nil)
  (setq-local iron-main-epf--top-panel-window (selected-window))
  (setq-local iron-main-epf--panel-saved-modeline mode-line-format)

  (iron-main-message "EPF" "MP" 1 "I"
		     "saved modeline: %s."
		     (not (null iron-main-epf--panel-saved-modeline)))

  (cl-remf keys :setup)

  (apply setup keys)

  ;; Assume SETUP does not switch buffer.
  
  (iron-main-epf--dump-panel (current-buffer))
  (message "IMEPF01I: panel %s set up." (buffer-name))

  (current-buffer)
  )


(cl-defun iron-main-epf--make-subpanel (buffer-or-name
					setup-function
					&rest keys
					&key
					(top-panel (current-buffer))
					(parent-panel (current-buffer))
					(window-height 0.75)
					(header-line
					 "IRON MAIN Subpanel...")
					(local-keymap
					 iron-main-epf-mode-sub-panel-keymap)
					(keep-modeline nil)
					&allow-other-keys
					)
  "Sets up a \\='subpanel\\='.

The function is called with a BUFFER-OR-NAME which will become the
buffer displayed in the subpanel.  The buffer in the subpanel is
erased.  The buffer, after being erased is set up by invoking
SETUP-FUNCTION within a call to WITH-CURRENT-BUFFER; the
SETUP-FUNCTION is called with the keyword arguments passed to
`iron-main-epf--make-subpanel'.  Hence, SETUP-FUNCTION, if not
defined via `cl-defun', can be a lambda essentially taking a &rest
p-list; the value returned by SETUP-FUNCTION is ignored.

`iron-main-epf--make-subpanel' creates and displays
BUFFER-OR-NAME, sets up the panel-to-panel navigation,
initializes the subpanel local variables and returns the buffer
BUFFER-OR-NAME.

The WINDOW-HEIGHT argument (default 0.75) is the percentage of the
selected window (the \\='main panel\\=') that the subpanel will
occupy.  HEADER-LINE, if not nil will be displayed as the header of
the subpanel.  LOCAL-KEYMAP is the keymap used by the subpanel, it
defaults to `iron-main-epf-mode-sub-panel-keymap', which makes some
assumptions about \\='quitting\\=' the subpanel.  KEEP-MODELINE, if
nil (the default) hides/moves the modeline of the \\='main panel\\='.
"
  ;; I could use `with-current-buffer-window', but it may not work
  ;; with Emacsen earlier than 28.1

  (ignore top-panel parent-panel)

  (message "*")
  (message "*")
  (iron-main-message "EPF" "MSP" 0 "I"
		     "making subpanel %s."
		     buffer-or-name)
  
  (let ((panel-display-window (selected-window)) ; This and the next not quite right.
	(panel-display-buffer (current-buffer))
	(subpanel-display-buffer
	 (get-buffer-create
	  ;; "*IRON MAIN Hercules subdisplay display*"
	  buffer-or-name))
	)


    (iron-main-message "EPF" "MSP" 0 "I"
		       "selected window %s." (selected-window)
		       )
    (iron-main-message "EPF" "MSP" 0 "I"
		       "current buffer %s." (current-buffer)
		       )

    ;; (select-window (split-window-below nil))

    (display-buffer subpanel-display-buffer
		    `(
		      display-buffer-below-selected
		      ;; display-buffer-in-side-window
		      ;; (side . bottom)
		      (window-height . ,window-height)
		      (window-min-height . 5)
		      ;; (inhibit-same-window . t)
		      ))

    (iron-main-message "EPF" "MSP" 1 "I"
		       "subpanel %s displayed."
		       buffer-or-name)

    (iron-main-epf--dump-panel (current-buffer))

    (setq-local iron-main-epf--subpanel-window
		(get-buffer-window subpanel-display-buffer))

    (iron-main-message "EPF" "MSP" 2 "I"
		       "saved modeline \"%s\"."
		       iron-main-epf--panel-saved-modeline)
    
    (unless keep-modeline
      (iron-main-message "EPF" "MSP" 3 "I"
			 "setting mode-line to NIL.")
      (setq-local mode-line-format nil)
      )

    (with-current-buffer subpanel-display-buffer
      (iron-main-message "EPF" "MSP" 4 "I"
			 "subpanel display buffer is %s."
			 (current-buffer))

      (kill-all-local-variables)
      (let ((inhibit-read-only t))
	(erase-buffer))

      (message "*******")
      (iron-main-message "EPF" "MSP" 3 "I"
			 "subpanel modeline is \"%s\"."
			 (null mode-line-format))
      (message "*******")

      ;; `setq-local' takes only two arguments in older Emcasen.
      (setq-local iron-main-epf--is-top-panel nil)

      (setq-local iron-main-epf--parent-panel panel-display-buffer)

      (setq-local iron-main-epf--top-panel-window panel-display-window)

      (setq-local iron-main-epf--saved-modeline mode-line-format)

      (when header-line
	(setq-local header-line-format header-line))

      ;; Use the proper local map for proper killing of subpanels
      ;; ...

      (use-local-map local-keymap)

      ;; Add subpanel stuff here.

      (cl-remf keys :top-panel)
      (cl-remf keys :parent-panel)
      
      (prog1 (apply setup-function keys)

	(iron-main-epf--dump-panel (current-buffer))

	(iron-main-message "EPF" "MSP" 4 "I"
			   "tree %s." (window-tree))
	
	(iron-main-message "EPF" "MSP" 5 "I" "subpanel display set up.")
	))
    ))


;; (cl-defun iron-main-epf--exit-top-panel (&optional
;; 					 (panel-to-exit
;; 					  (current-buffer)))
;;   (interactive)

;;   (with-current-buffer panel-to-exit

;;     (message "*")
;;     (message "IMEPFETP0I: exiting top panel %s." panel-to-exit)
    
;;     (when iron-main-epf--subpanel-window
;;       (let ((sp-buffer (window-buffer iron-main-epf--subpanel-window)))
;; 	(message "IMEPFETP1I: exiting sub panel window %s (buffer %s)."
;; 		 iron-main-epf--subpanel-window
;; 		 sp-buffer
;; 		 )
;; 	(iron-main-epf--exit-subpanel sp-buffer)
;; 	(iron-main-epf--cleanup-subpanel-exit panel-to-exit)
;; 	))

;;     (iron-main-epf--exit-panel panel-to-exit)
;;     ))


;; New version

(cl-defun iron-main-epf--exit-top-panel (&optional
					 (panel-to-exit
					  (current-buffer)))
  ;; This just become a wrapper.
  
  (interactive)

  (message "*")
  (message "IMEPFETP0I: exiting top panel %s." panel-to-exit)
  
  (with-current-buffer panel-to-exit

    (cl-assert iron-main-epf--is-top-panel nil
	       (iron-main--format-message
		"EPF"
		"ETP"
		0
		"E"
		"panel %s is not a top panel."
		panel-to-exit))

    (iron-main-epf--exit-subpanel panel-to-exit)
    ))


(cl-defun iron-main-epf--exit-subpanel (&optional
					(panel-to-exit
					 (current-buffer)))
  (interactive)

  (message "*")
  (iron-main-message "EPF" "ESP" 0 "I"
		     "exiting subpanel %s." panel-to-exit)
  
  (with-current-buffer panel-to-exit

    (iron-main-epf--dump-panel (current-buffer))
    
    (iron-main-epf--signal-exit-down panel-to-exit)

    (iron-main-message "EPF" "ESP" 1 "I"
		       "subpanels exited.")
    
    (if iron-main-epf--is-top-panel
	(iron-main-epf--exit-panel panel-to-exit)
      (let ((panel-window (get-buffer-window panel-to-exit)))
	(iron-main-message "EPF" "ESP" 2 "I"
			   "ask parent to cleanup and quitting window.")
	
	(iron-main-epf--cleanup-subpanel-exit
	 iron-main-epf--parent-panel)
	
	(iron-main-message "EPF" "ESP" 2 "I"
			   "quitting window %s." panel-window)
	(quit-window t panel-window)
	;; (delete-window panel-window)
	)
      )))


(cl-defun iron-main-epf--cleanup-subpanel-exit (panel)
  "Internal function to be called after a subpanel exited."

  (message "*")
  (iron-main-message "EPF" "CSE" 0 "I"
		     "cleanup sub panel exit %s." panel)
  
  (with-current-buffer panel

    (iron-main-epf--dump-panel panel)

    (iron-main-message "EPF" "CSE" 1 "I"
		       "saved modeline: %s."
		       iron-main-epf--panel-saved-modeline)
    
    (when iron-main-epf--panel-saved-modeline
      (message "IMEPFCSE2I: resetting modeline %s."
	       iron-main-epf--panel-saved-modeline)
      (setq-local mode-line-format
		  iron-main-epf--panel-saved-modeline))

    (message "IMEPFCSE3I: setting subpanel window to NIL.")
    (setq-local iron-main-epf--subpanel-window nil)

    (iron-main-message "EPF" "CSE" 4 "I"
		       "cleanud up %s." panel)

    (iron-main-epf--dump-panel panel)
    nil
    )
  )


;; (cl-defun iron-main-epf--exit-subpanel (&optional
;; 					(subpanel-to-exit
;; 					 (current-buffer))
;; 					)
;;   (interactive)

;;   (with-current-buffer subpanel-to-exit
;;     (let ((parent-panel iron-main-epf--parent-panel)
;; 	  (sp-buffer (window-buffer iron-main-epf--subpanel-window))
;; 	  )

;;       (cl-assert parent-panel nil
;; 		 "IMEPFESP01E: exiting subpanel %s with no parent."
;; 		 subpanel-to-exit)

;;       (message "*")
;;       (message "IMEPFESP0I: exiting sub panel %s." subpanel-to-exit)
;;       (when iron-main-epf--subpanel-window
;; 	(message "IMEPFESP1I: exiting sub panel window %s."
;; 		 iron-main-epf--subpanel-window)
;; 	(iron-main-epf--exit-subpanel sp-buffer)
;; 	(iron-main-epf--cleanup-subpanel-exit subpanel-to-exit)
;; 	)

;;       ;; (with-current-buffer parent-panel
;;       ;; 	(setq-local iron-main-epf--subpanel-window nil)

;;       ;; 	(when iron-main-epf--panel-saved-modeline
;;       ;; 	  (setq-local mode-line-format
;;       ;; 		      iron-main-epf--panel-saved-modeline))
;;       ;; 	)

;;       (message "IMEPFESP02I: quitting window %s."
;; 	       (selected-window))
;;       (message "IMEPFESP02I: for buffer %s."
;; 	       subpanel-to-exit)
;;       (message "IMEPFESP02I: with window %s."
;; 	       (get-buffer-window subpanel-to-exit))
;;       (quit-window t (get-buffer-window subpanel-to-exit))

;;       ;; Finally we tell the parent to clean up this subpanel.
;;       (iron-main-epf--cleanup-subpanel-exit parent-panel)
;;       ))
;;   )


(cl-defun iron-main-epf--signal-exit-down (&optional
					   (subroot-panel
					    (current-buffer))
					   )
  ;; SUBROOT-PANEL is the panel with a potential subpanel to exit.
  ;; Note that the panels are actualy organized in a list; hence
  ;; '-ROOT' is a bit of a misnomer.
  
  (interactive)

  (message "**")
  (iron-main-message "EPF" "ESPS" 0 "I"
		     "exiting sub panel of %s." subroot-panel)

  (message "***")

  
  (with-current-buffer subroot-panel

    (iron-main-epf--dump-panel (current-buffer))
    
    (unless iron-main-epf--subpanel-window
      (iron-main-message "EPF" "ESPS" 1 "I" "leaf panel."))
    
    (when iron-main-epf--subpanel-window
      ;; Whe have a subpanel to "exit"...

      (let ((sp-buffer (window-buffer iron-main-epf--subpanel-window)))

	(message "IMEPFESPS1I: exiting sub panel window %s."
		 iron-main-epf--subpanel-window)
	(message "IMEPFESPS1I: on buffer %s."
		 sp-buffer)

	(iron-main-epf--exit-subpanel sp-buffer)

	(message "IMEPFESPS02I: currently selected window %s."
		 (selected-window))
	(message "IMEPFESPS02I: for buffer %s."
		 (window-buffer (selected-window))) ; Pleonastic.

	(message "IMEPFESPS02I: *** quitting window %s."
		 iron-main-epf--subpanel-window)
	(message "IMEPFESPS02I: *** with window %s."
		 (window-buffer iron-main-epf--subpanel-window))
	
	;; (quit-window t iron-main-epf--subpanel-window)
	))
    (iron-main-epf--cleanup-subpanel-exit subroot-panel))
  )


;;; Test subpanels.
;;; ===============
;;;
;;; In the following the entry function is always the unnumbered one.
;;; The numbered ones are just for reminescence.

(defvar iron-main-epf--test-toppanel-keymap
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km widget-keymap)
    (define-key km (kbd "<f3>") 'iron-main-epf--exit-top-panel)
    (define-key km (kbd "q") 'iron-main-epf--exit-top-panel)
    (define-key km (kbd "Q") 'iron-main-epf--exit-top-panel)
    ;; (define-key km (kbd "<down>") 'iron-main-epf--down-key-event)
    km
    )
  )


(defvar iron-main-epf--test-subpanel-keymap
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km widget-keymap)
    (define-key km (kbd "<f3>") 'iron-main-epf--exit-subpanel)
    (define-key km (kbd "q") 'iron-main-epf--exit-subpanel)
    (define-key km (kbd "Q") 'iron-main-epf--exit-subpanel)
    ;; (define-key km (kbd "<up>") 'iron-main-epf--up-key-event)
    ;; (define-key km (kbd "<down>") 'iron-main-epf--down-key-event)
    km
    )
  )


(cl-defun iron-main-epf--up-key-event ()
  (interactive)
  (iron-main-message "EPF" "UKE" 0 "I" "up key pressed.")
  (forward-line -1))


(cl-defun iron-main-epf--down-key-event ()
  (interactive)
  (iron-main-message "EPF" "DKE" 0 "I" "down key pressed.")
  (forward-line 1))


(cl-defun iron-main-epf--test-subpanels (session &rest args)
  "Test functionality for subpanels.

This function is just a placeholder for testing stuff.
"
  (ignore args)
  (cl-assert (iron-main-session-p session) t
	     "IMPTS0E: SESSION %S is not a `iron-main-session'"
	     session)

  (cl-labels
      ((panel-setup (&rest args)
	 (ignore args)
	 (use-local-map iron-main-epf--test-toppanel-keymap)
	 (setq-local header-line-format "IMEPFTS01I")
	 (widget-insert "\nTop panel\n")
     
	 (widget-create
	  'push-button
	  :value "Make Subpanel"
	  :action #'make-subpanel-button-action
	  )
	 (prog1 (widget-setup)
	   ;; (widget-forward 1)
	   (iron-main-epf--goto-first-widget))
	 )
       
       (make-subpanel-button-action (&rest args)
	 (ignore args)
	 (iron-main-message "EPF" "WCB" 1 "I" "creating subpanel %s."
			    (selected-window))
	 (iron-main-epf--make-subpanel
	  "*IRON MAIN Subpanel*"
	  #'subpanel-setup))

       
       (subpanel-setup (&rest args)
	 (ignore args)
	 (widget-insert "\nSubpanel\n")
	 (use-local-map iron-main-epf--test-subpanel-keymap)
	   
	 (widget-create
	  'push-button
	  :value "Make Subsubpanel"
	  :action #'subpanel-button-action
	  )
	 (widget-setup)
	 )

       
       (subpanel-button-action (&rest args)
	 (ignore args)
	 (iron-main-message "EPF" "WCB" 1 "I" "creating subsubpanel %s."
			    (selected-window))
	 (iron-main-epf--make-subpanel
	  "*IRON MAIN Subsubpanel*"
	  #'subsubpanel-setup
	  :window-height 7
	  :header-line "SUB SUB Panel"
	  ))

       
       (subsubpanel-setup (&rest args)
	 (ignore args)
	 (widget-insert "\nSubsubpanel\n")
	 (use-local-map iron-main-epf--test-subpanel-keymap)
	 (widget-setup)
	 )
       )

    (iron-main-epf--make-panel
     session
     "*IRON MAIN Subpanel Test TOP*"
     :setup #'panel-setup
     )
    ))


;;; Panel navigation.
;;; =================

(cl-defun iron-main-epf--exit-panel (&optional
				     (panel-to-exit (current-buffer)))
  "Exit the current panel \\='popping\\=' the \\='stack\\=' of panels.

PANEL-TO-EXIT is the panel to exit, defaulting to the current buffer.
If PANEL-TO-EXIT is not an IRON-MAIN panel, then this function has no
effect.  If the \\='back\\=' buffer is not live just switch to the
buffer that Emacs things is best."

  (interactive)

  (message "IMFP01I Exiting %S" panel-to-exit)
  (with-current-buffer panel-to-exit
    (message ">>> in-panel %S, panel-tag %S, back %S, live %S"
	     iron-main-epf--in-panel
	     iron-main-epf--tag
	     iron-main-epf--back
	     (and (bufferp iron-main-epf--back)
		  (buffer-live-p iron-main-epf--back)))
    (when iron-main-epf--in-panel
      (when (string= iron-main-epf--tag "Top")
	(message "IMFP00I: Exiting IRON MAIN...")
	(sleep-for 3)
	(iron-main-session-delete iron-main-epf--session)
	(message "IMFP00I: IRON MAIN exited."))

      ;; Order of `switch-to-buffer' and `kill-buffer' is important.
      (if (and (bufferp iron-main-epf--back)
	       (buffer-live-p iron-main-epf--back))
	  (progn
	    (switch-to-buffer iron-main-epf--back nil t)
	    (kill-buffer panel-to-exit))
	(progn
	  ;; iron-main-epf-back not a buffer or not live.
	  (switch-to-buffer nil)
	  (kill-buffer panel-to-exit))
	))))



(cl-defun iron-main-epf--invoke-panel
    (from panel-start-function &rest args)
  "Start a new panel by calling PANEL-START-FUNCTION.

The function PANEL-START-FUNCTION is one of the functions setting
up a panel; it is called by applying it to the session attached
to FROM and ARGS.  After calling the PANEL-START-FUNCTION,
`iron-main-epf--invoke-panel' assumes that Emacs has switched
to a new panel/buffer and it sets the buffer local variable
`iron-main-epf-back' variable to FROM (which should be a
panel/buffer itself.

The function returns a list containing two items: the current
buffer (which should be the buffer associated to the panel
created by PANEL-START-FUNCTION, and FROM.

If PANEL-FUNCTION is NIL, this is a no-op"
  
  (when panel-start-function
    (apply panel-start-function
	   (iron-main-epf--get-session from)
	   args)

    (message "IMPIP0I: panel invoked; setting back to %s" from)
    
    (setq-local iron-main-epf--back from)
    (setq-local iron-main-epf--session
		(iron-main-epf--get-session from))
    (list (current-buffer) from))
  )


;;; Epilogue

(provide 'iron-main-epf)

;;; iron-main-epf.el ends here
