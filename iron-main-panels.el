;;; -*- Mode: Emacs-Lisp; lexical-binding: t -*-
;;; iron-main-panels.el --- IRON MAIN "panels" for mainframe interaction.

;;; iron-main-panels.el
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: December 5th, 2020.
;;
;; Version: 20221014.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; This file contains the "panels" that are used to interact with a
;; "mainframe" (again, mostly a MVS 3.8j running on Hercules).
;;
;; The implementation is an interesteing use (or, one should say,
;; "abuse") of the Emacs `widget.el' library to give the "look and feel
;; of using a "mainframe" application in... Emacs (I know: I am a pervert).
;;
;; To use, just issue
;;
;; M-x iron-main-frame
;;
;; The functions in this file have 'iron-main-panels-' as prefix.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'widget)
  (require 'wid-edit))

(require 'iron-main-vars)


;;; IRON MAIN panels.
;;
;; "Panel" is mainframe-speak for "page", or in the context of Emacs,
;; "window/buffer".
;;
;; The code below uses several examples of the "widget" library found
;; in the Emacs Internet.

(defvar-local iron-main-panels--current-ds (make-iron-main-ds-rep))

(defvar-local iron-main-panels--dsname-widget nil)

(defvar-local iron-main-panels--recfm-widget nil)
(defvar-local iron-main-panels--lrecl-widget nil)
(defvar-local iron-main-panels--blksize-widget nil)
(defvar-local iron-main-panels--dsorg-widget nil)

(defvar-local iron-main-panels--vol-widget nil)
(defvar-local iron-main-panels--unit-widget nil)

(defvar-local iron-main-panels--space-unit-widget nil)
(defvar-local iron-main-panels--primary-widget nil)
(defvar-local iron-main-panels--secondary-widget nil)
(defvar-local iron-main-panels--dir-widget nil)


;;; IRON MAIN panel keymaps.

(defvar iron-main-panels-mode-keymap
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km widget-keymap)
    (define-key km (kbd "<f3>") 'iron-main-panels--exit-panel)
    (define-key km (kbd "q") 'iron-main-panels--exit-panel)
    (define-key km (kbd "Q") 'iron-main-panels--exit-panel)
    km
    )
  "The IRON MAIN Panel mode key map.

The key map inherits from `widget-keymap'.  The keys '<f3>' (that is,
'PF3'), 'q' and 'Q' exit the current panel.")


(defvar iron-main-panels-editable-field-keymap
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km widget-field-keymap)

    ;; Redefine F3; the original is
    ;; `kmacro-start-macro-or-insert-counter' from `global-map'.
    
    (define-key km (kbd "<f3>") 'iron-main-panels--exit-panel)
    km
    )
  "The IRON MAIN Panel mode key map.

The key map inherits from `widget-keymap'.  The key '<f3>' (that is,
'PF3') exits the current panel.")


;;; Buffer local variables.

;; Navigation.

(defvar-local iron-main-panels--back nil
  "Local panel variable set by the `invoking' panel."
  ;; Pretty crude for the time being.
  ;; This just acts as a stack.  Whever a panel is "closed", the
  ;; buffer is killed and the content of this variable, a buffer, if
  ;; not nil and live, is restored.
  )


(defvar-local iron-main-panels--in-panel nil
  "When non NIL, the buffer is an IRON MAIN panel.")


(defvar-local iron-main-panels--tag nil
  "A tag that identifies the type of IRON MAIN panel.

The value can be either a string or a symbol.")


(defvar-local iron-main-panels--cmds ()
  "The panel command alist.

This list contains a \"command alist\" which is a specification for
link widgets to insert in a panel.  Each IRON MAIN panel (which is
eventually an Emacsbuffer) can initialize this variable as it wishes.

The format is of a  \"command alist\" is the following:

    (cmd function &rest keys)

Where CMD is a string, FUNCTION a \"panel invocation\" function and
KEYS a list of key-value pairs to be used for `widget-create'.

See Also:

`iron-main-panels--hercules-top-commands',
`iron-main-panels--hercules-dsfs-commands'
")


(defvar-local iron-main-panels--cmds-links ()
  "List of \"link\" widgets for the commands available in the panel.")


;;; Commands alists.
;;
;; Commands alists are lists of "specifications" for link widgets to
;; insert in a panel.
;; Their format is:
;;
;;    (cmd function &rest keys)
;;
;; Where CMD is a string, FUNCTION a "panel invocation" function and
;; KEYS a list of key-value pairs to be used for `widget-create'.

(defvar iron-main-panels--hercules-top-commands
  `(("System" iron-main-panels--hercules-system
     :header "Inspect Hercules system/machine"
     )
    ("Datasets" iron-main-panels--hercules-dsfs-utilities
     :header "Handle files and datasets across systems"
     )
    ("Help" iron-main-panels--hercules-help
     :header "Hercules help"
     )
    ("Exit"   iron-main-panels--exit-panel
     :header "Exit the IRON MAIN current panel or top-level"
     :notify ,(lambda (w &rest args)
		(ignore w args)
		(iron-main-panels--exit-panel)))
    )
  "A 'commands alist' for the IRON MAIN top panel."
  )


(defvar iron-main-panels--hercules-dsfs-commands
  `(("Allocate" iron-main-panels--dataset-allocation
     :header "Allocate a dataset on the mainframe"
     )
    ("Upload" iron-main-panels--dataset-save
     :header "Upload a local file on the mainframe"
     )
    ("Edit"  iron-main-panels--dataset-edit
     :header "Edit a dataset member from the mainframe (if connected)"
     )
    ("Edit local"  iron-main-panels--dataset-edit-local
     :header "Edit a local file"
     )
    ("Exit"   iron-main-panels--exit-panel
     :header "Exit the IRON MAIN current panel or top-level"
     :notify ,(lambda (w &rest args)
		(ignore w args)
		(iron-main-panels--exit-panel)))
    )
  "A 'commands alist' for the IRON MAIN dtaset and filesystem  panel."
  )


(defun iron-main-panels--find-command (cmd commands-alist)
  ;; (assoc cmd commands-alist 'string=)
  (assoc cmd commands-alist)		; Older Emacsen do not accept the third arg.
  )


(defun iron-main-panels--command-field ()
  (widget-insert "Command")
  (widget-create 'integer :size 3 :tag "" :value ""
		 :validate
		 (lambda (cmd)
		   (<= 1 (widget-value cmd) (length iron-main-panels--cmds)))
		 
		 :action
		 (lambda (cmd &optional event)
		   (ignore event)
		   (message ">>> notified")
		   ;; (sleep-for 3)
		   (let* ((cmd-widget
			   (nth (1- (widget-value cmd))
				iron-main-panels--cmds-links))
			  (cmd-notify
			   (iron-main-panels--widget-notify cmd-widget))
			  )
		     (message ">>> calling %s on %s"
		      	      cmd-notify
		     	      cmd-widget)
		     (when cmd-notify
		       (apply cmd-notify cmd-widget ()))
		     ))
		 :keymap
		 iron-main-panels-editable-field-keymap
		 )
  
  (widget-insert "\n\n")
  )


(defun iron-main-panels--insert-command-widgets (cmd-alist)
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
				      (iron-main-panels--find-command
				       (iron-main-panels--widget-tag w)
				       cmd-alist)))
				    )
				(iron-main-panels--invoke-panel
				 (current-buffer)
				 panel-function)
				))))
	   do
	   (widget-insert "\n")
	   ))


;;; iron-main-panel-mode
;; The main mode (major) for the panels.

(define-derived-mode iron-main-panel-mode nil "//IRON-MAIN"
  "IRON MAIN Panel Mode.

Major mode for IRON MAIN Panels.  Mostly a container for variables
and a specialized keymap.

You an use the function key `F3' (i.e., `PF3') or the [Qq] keys to
exit a panel.  Exiting the top panel will exit the IRON MAIN
interface."

  (setq-local iron-main-panels--in-panel t)
  (setq-local iron-main-panels--back nil)

  (use-local-map iron-main-panels-mode-keymap)
  )


;; Panels.
;; The actual panels available.
;; The "fields" (or "widgets") of each panel have (function) names
;; that end in "-field" or "-widget".

;; title-field

(defun iron-main-panels--title-field (&optional title)
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
  (widget-insert (make-string 72 175))	; 175 is the "overline"
  (widget-insert "\n")
  )


;; help-field
;; Unused.

(defun iron-main-panels--help-field ()
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
   (propertize (format "Use `[Qq]' or `PF3' to go back to previous panel.")
	       'face '(fixed-pitch-serif :weight bold)
	       ))
  )


;; Datasets and file system handling panel.
;; ----------------------------------------

(defun iron-main-panels--dsname-item-field ()
  "Create the `dsname' editable-field widget in the IRON MAIN panel."
  (setq iron-main-panels--dsname-widget
	(widget-create 'editable-field
                       :size 46	   ; A name is at most 44 plus quotes.
                       :format "Data set name: %v " ; Text after the field!
		       (iron-main-ds-rep-name iron-main-panels--current-ds)
		       :notify
		       (lambda (w &rest ignore)
			 (ignore ignore)
			 (message "DSN: <%s>."
				  (widget-value w))
			 (setf (iron-main-ds-rep-name
				iron-main-panels--current-ds)
			       (widget-value w)))
		       :keymap
		       iron-main-panels-editable-field-keymap
		       ))
  (widget-insert "\n")
  (setf iron-main-panels--vol-widget
	(widget-create 'editable-field
		       :format "Volume serial: %v"
		       :value (iron-main-ds-rep-vol
			       iron-main-panels--current-ds)
		       :size 6
		       :notify
		       (lambda (w &rest ignore)
			 (ignore ignore)
			 (message "VOL: <%s>."
				  (widget-value w))
			 (setf (iron-main-ds-rep-vol
				iron-main-panels--current-ds)
			       (widget-value w)))
		       :keymap
		       iron-main-panels-editable-field-keymap

		       ))
    )


(defvar iron-main-ds-allocation-dsorg "PDS"
  "The default data set organization (DSORG).")


(defvar-local iron-main-panels--hercules-dsfs-cmds-links ()
  "List of link widgets created for DSFS panel commands.")


(cl-defun iron-main-panels--hercules-dsfs-utilities (session &rest args)
  "Create the IRON MAIN datasets and file system utilities.

The panel presents the options regarding the handling of datasets
(files) on the connected, possibly hosted, operating system and the
file system(s) that Emacs has direct access to; most notably, the
\"host\" file system, say, Linux, Mac OS X or Windows."

  (interactive)

  (ignore args)
  
  (cl-assert (iron-main-session-p session) t
	     "SESSION %S is not a `iron-main-session'"
	     session)

  (switch-to-buffer "*IRON MAIN dataset and file system handling.*")

  (kill-all-local-variables)

  (let ((inhibit-read-only t))
    (erase-buffer))

  (iron-main-panel-mode)

  ;; Now that we have the session, some of these are repetitions.

  (setq-local iron-main-panels--session session)
  
  (setq-local iron-main-machine
	      (iron-main-session-machine session))
  (setq-local iron-main-os-flavor
	      (iron-main-session-os-flavor session))

  (setq-local iron-main-panels--tag "Hercules datasets")
  (setq-local iron-main-panels--cmds iron-main-panels--hercules-dsfs-commands)
  
  (iron-main-panels--title-field
   "Dataset and file system handling panel")

  (widget-insert "\n")

  ;; Let's start!

  (iron-main-panels--command-field)
  
  (setq-local iron-main-panels--cmds-links
	      (iron-main-panels--insert-command-widgets
	       iron-main-panels--cmds))

  (message "IMHS00I: Hercules datasets and file system utilities.")
  (prog1 (widget-setup)
    (widget-forward 1))
  )


;; Dataset allocation.
;; -------------------

(defun iron-main-panels--dataset-allocation (session &rest args)
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

  (iron-main-panel-mode)

  ;; Init buffer local variables.
  
  (setq-local iron-main-panels--tag "Allocation panel")
  (setq-local iron-main-panels--cmds ())

  
  ;; Let's start!
  
  (iron-main-panels--title-field "Dataset allocation panel")
  
  (iron-main-panels--dsname-item-field)
			 
  (widget-insert "\n\n")

  (setf iron-main-panels--recfm-widget
	(widget-create 'editable-field
		       :format "Record format (RECFM):         %v"
		       :value (iron-main-ds-rep-recfm
			       iron-main-panels--current-ds)
		       :size 3
		       :notify (lambda (w &rest ignore)
				 (ignore ignore)
				 (message "RECF: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-recfm
					iron-main-panels--current-ds)
				       (widget-value w)))
		       :keymap
		       iron-main-panels-editable-field-keymap

		       ))
  (widget-insert "\n")

  (setf iron-main-panels--lrecl-widget
	(widget-create 'integer
		       :format "Logical record length (LRECL): %v"
		       :value (iron-main-ds-rep-lrecl
			       iron-main-panels--current-ds)
		       :size 4
		       
		       :value-regexp "[0-9]+"
		       :notify (lambda (w &rest ignore)
				 (ignore ignore)
				 (message "LRECL: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-lrecl
					iron-main-panels--current-ds)
				       (widget-value w)))

		       :keymap
		       iron-main-panels-editable-field-keymap
		       ))
  (widget-insert "\n")

  (setf iron-main-panels--blksize-widget
	(widget-create 'integer
		       :format "Block size (BLKSIZE):          %v"
		       :value (iron-main-ds-rep-blksize
			       iron-main-panels--current-ds)
		       :size 6
		       :value-regexp "[0-9]+"
		       :notify (lambda (w &rest ignore)
				 (ignore ignore)
				 (message "BLKSIZE: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-blksize
					iron-main-panels--current-ds)
				       (widget-value w)))

		       :keymap
		       iron-main-panels-editable-field-keymap
		       ))
  (widget-insert "\n\n")

  (setf iron-main-panels--vol-widget
	(widget-create 'editable-field
		       :format "Volume (VOL):                  %v"
		       :value (iron-main-ds-rep-vol
			       iron-main-panels--current-ds)
		       :size 6
		       :notify (lambda (w &rest ignore)
				 (ignore ignore)
				 (message "VOL: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-vol
					iron-main-panels--current-ds)
				       (widget-value w)))
		       :keymap
		       iron-main-panels-editable-field-keymap

		       ))
  (widget-insert "\n\n")

  (widget-insert "Dataset organization (DSORG): \n")
  (setf iron-main-panels--dsorg-widget
	(widget-create 'radio-button-choice
		       :tag "Dataset organization (DSORG)"
		       :doc "Dataset organization (DSORG)"
		       :value "PO"
		       :void  "PO"
		       :indent 4
		       :help-echo "Choose the dataset organization"
		       :notify (lambda (w &rest ignore)
				 (ignore ignore)
				 (message "Dataset organization: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-dsorg
					iron-main-panels--current-ds)
				       (widget-value w)))
		       '(item :tag "Partitioned Data Set (PO)"
			      :value "PO")
		       ;; '(item :tag "Partitioned Data Set Extended (PDSE)"
		       ;;        :value "PDSE")
		       '(item :tag "Sequential (PS)"
			      :value "PS")
		       ;; Add other ones.
		       ))
  (widget-insert "\n\n")

  (widget-insert "Space allocation:\n")
  (setf iron-main-panels--space-unit-widget
	(widget-create 'radio-button-choice
		       :tag "Dataset space unit (SPACE)"
		       :value "TRK"
		       :void  "TRK"
		       :indent 4
		       :help-echo "Choose the dataset space unit"
		       :notify (lambda (w &rest ignore)
				 (ignore ignore)
				 (message "Dataset space unit: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-space-unit
					iron-main-panels--current-ds)
				       (widget-value w)))
		       '(item "TRK")
		       '(item "CYL")
		       '(item "BLK")
		       ;; Add other ones.
		       ))
    
  (widget-insert "\n")
  (setf iron-main-panels--primary-widget
	(widget-create 'integer
		       :format "Primary: %v"
		       :value (iron-main-ds-rep-lrecl
			       iron-main-panels--current-ds)
		       :size 8
		       :value-regexp "[0-9]+"
		       :notify (lambda (w &rest ignore)
				 (ignore ignore)
				 (message "Primary: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-primary
					iron-main-panels--current-ds)
				       (widget-value w)))

		       :keymap
		       iron-main-panels-editable-field-keymap
		       ))
  (widget-insert "    ")
  (setf iron-main-panels--secondary-widget
	(widget-create 'integer
		       :format "Secondary: %v"
		       :value (iron-main-ds-rep-secondary
			       iron-main-panels--current-ds)
		       :size 8
		       :value-regexp "[0-9]+"
		       :notify (lambda (w &rest ignore)
				 (ignore ignore)
				 (message "Secondary: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-secondary
					iron-main-panels--current-ds)
				       (widget-value w)))
		       :keymap
		       iron-main-panels-editable-field-keymap
		       ))
  (widget-insert "    ")
  (setf iron-main-panels--dir-widget
	(widget-create 'integer
		       :format "Directory blocks: %v"
		       :value (iron-main-ds-rep-directory
			       iron-main-panels--current-ds)
		       :size 4
		       :value-regexp "[0-9]+"
		       :notify (lambda (w &rest ignore)
				 (ignore ignore)
				 (message "Directory blocks: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-directory
					iron-main-panels--current-ds)
				       (widget-value w)))

		       :keymap
		       iron-main-panels-editable-field-keymap
		       ))

  ;; (widget-insert "\n\n\n")
  
  (widget-insert "\n")
  (widget-insert (make-string 72 ?_))
  (widget-insert "\n")
  
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
			   (ignore ignore)
			   (setf (iron-main-ds-rep-name
				  iron-main-panels--current-ds)
				 (widget-value iron-main-panels--dsname-widget)
				 (iron-main-ds-rep-dsorg
				  iron-main-panels--current-ds)
				 (widget-value iron-main-panels--dsorg-widget))
			   (message "DD: <%s>."
				    (iron-main-ds-to-string
				     iron-main-panels--current-ds)
				    ))
                 "Allocate")
  (widget-insert "    ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
			   (ignore ignore)
			   (message "JCL buffer for '%s': ...."
				    (iron-main-ds-rep-name
				     iron-main-panels--current-ds)
				    ))
                 "View job buffer")
  (widget-insert "    ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
			   (ignore ignore)
			   (message "Data set name: %s."
				    (iron-main-ds-rep-name
				     iron-main-panels--current-ds)
				    )
			   (iron-main-panels--dataset-save session)
			   )
                 "Allocate and Save")
  (widget-insert "    ")
  (widget-create 'push-button
                 :notify
		 (lambda (&rest ignore)
		   (ignore ignore)
		   (message "Cancelled dataset '%s' mainframe allocation."
			    (iron-main-ds-rep-name
			     iron-main-panels--current-ds)
			    )
		   )
                 "Cancel")

  (widget-insert "\n")

  (message "IMDS00I: Dataset allocation panel set up.")
  (prog1 (widget-setup)
    (widget-forward 1))
  )


;; Dataset save panel.
;; -------------------

(defvar iron-main-panels--filename-widget "")

(defun iron-main-panels--dataset-save (session &rest args)
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

  (iron-main-panel-mode)
  
  (iron-main-panels--title-field "Dataset member save")
  
  (iron-main-panels--dsname-item-field)
  
  (widget-insert "\n\n")
  
  (setq iron-main-panels--filename-widget
	(widget-create 'file
		       :format "File: %v\n"
		       :value (iron-main-ds-rep-name
			       iron-main-panels--current-ds)
		       :size (- 72 (length "File: "))
		       :keymap
		       iron-main-panels-editable-field-keymap))

  (widget-insert (make-string 72 ?_))
  (widget-insert "\n")
  
  (widget-create 'push-button
                 :notify
		 (lambda (&rest ignore)
		   (ignore ignore)
		   (message "DD: <%s>."
			    (iron-main-ds-to-string
			     iron-main-panels--current-ds)
			    ))		 
                 "Save to mainframe")
  (widget-insert "    ")
  (widget-create 'push-button
                 :notify
		 (lambda (&rest ignore)
		   (ignore ignore)
		   (message "JCL buffer for '%s' and '%s': ...."
			    (iron-main-ds-rep-name
			     iron-main-panels--current-ds)
			    (widget-value
			     iron-main-panels--filename-widget)
			    ))
                 "View job buffer")
  (widget-insert "    ")
  (widget-create 'push-button
                 :notify
		 (lambda (&rest ignore)
		   (ignore ignore)
		   (message "Saving dataset '%s' to mainframe cancelled."
			    (iron-main-ds-rep-name
			     iron-main-panels--current-ds)
			    )
		   )
                 "Cancel")

  (widget-insert "\n")

  (message "IMDS00I: Dataset upload panel set up.")
  (prog1 (widget-setup)
    (widget-forward 1))
  )


;; Dataset edit dataset member panel.
;; ----------------------------------

(defun iron-main-panels--dataset-edit (session &rest args)
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

  (iron-main-panel-mode)
  
  (iron-main-panels--title-field "Dataset member edit")
  
  (iron-main-panels--dsname-item-field)
  
  (widget-insert "\n\n")
  
  (setq iron-main-panels--filename-widget
	(widget-create 'file
		       :format "File: %v\n"
		       :value (iron-main-ds-rep-name
			       iron-main-panels--current-ds)
		       :size (- 72 (length "File: "))))

  (widget-insert (make-string 72 ?_))
  (widget-insert "\n")
  

  (message "IMDS00I: Dataset edit panel set up.")
  (prog1 (widget-setup)
    (widget-forward 1))
  )


;; IRON MAIN frame main, top panel.
;; --------------------------------

(defvar-local iron-main-hercules-pid nil
  "The PID of the Hercules process.  NIL if it is not running or reachable.")


(defvar-local iron-main-panels--session nil
  "The session a panel is attached to.")


(defun iron-main-panels--get-session (panel)
  "Get the IRON MAIN session attached to PANEL.

PANEL must be a buffer or a buffer name."
  (with-current-buffer panel
    iron-main-panels--session))


(defun iron-main-panels--widget-tag (w)
  "Get the :tag of widget W.

Notes:

This function is necessary because it is inexplicably absent from the
`widget.el' library."
  (plist-get (cdr w) :tag))


(defun iron-main-panels--widget-notify (w)
  "Get the :notify function of widget W.

Notes:

This function is necessary because it is inexplicably absent from the
`widget.el' library."
  (plist-get (cdr w) :notify))


(defvar-local iron-main-panels--hercules-top-cmds-links ()
  "List of link widgets created for top panel commands.")


(cl-defun iron-main-frame (&optional
			   (machine iron-main-machine)
			   (os-flavor iron-main-os-flavor))
  "Create the 'top' IRON MAIN panel.

The optional MACHINE and OS-FLAVOR arguments default to the values of
the variables IRON-MAIN-MACHINE and IRON-MAIN-OS-FLAVOR.

Notes:

This function is an alias for `iron-main-frame-panel'.

See Also:

IRON-MAIN-FRAME, IRON-MAIN-MACHINE and IRON-MAIN-OS-FLAVOR."
  
  (interactive)
  (iron-main-frame-panel machine os-flavor))


(cl-defun iron-main-frame-panel (&optional
				 (machine iron-main-machine)
				 (os-flavor iron-main-os-flavor)
				 &aux
				 (from-buffer (current-buffer))
				 (instance-banner "")
				 )
  "Create the 'top' IRON MAIN panel.

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

  (iron-main-panel-mode)

  ;; Init buffer local variables.
  
  (setq-local iron-main-machine machine)
  (setq-local iron-main-os-flavor os-flavor)
  (setq-local iron-main-panels--back from-buffer)
  (setq-local iron-main-panels--tag "Top")
  (setq-local iron-main-panels--cmds
	      iron-main-panels--hercules-top-commands)
  
  (when (iron-main-running-machine "Hercules")
    ;; Trying to get the PID.
    ;; All of this should be fatored out.
    (let ((pid (progn
		 (message "IMMP01I: Hercules running; getting PID.")
		 (iron-main-hercules-qpid)))
	  (session
	   (iron-main-session-start 'iron-main-hercules-session))
	  )
      (setq-local iron-main-hercules-pid pid)
      (setq-local iron-main-panels--session session)

      (if pid
	  (setf (iron-main-hs-pid session)
		pid
		
		instance-banner
		(format "%s running %s on %s:%s (%s) from %s"
			iron-main-machine
			iron-main-os-flavor
			iron-main-hercules-http-host
			iron-main-hercules-http-port
			pid
			(system-name)))
	(setf instance-banner
	      (format "%s running %s not available from %s"
		      iron-main-machine
		      iron-main-os-flavor
		      (system-name)))
	)

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
		       (expand-file-name os-dir))))
	
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
		       (expand-file-name dasd-dir))))
	
	)
      ))

  ;; Not running Hercules.
  (when (not (iron-main-running-machine "Hercules"))
    (message "IMMP01I: Hercules not running.")
    (setf instance-banner
	  (format "%s running %s not available"
		  iron-main-machine
		  iron-main-os-flavor))
    )

  ;; Let's start!
  
  (iron-main-panels--title-field)
  (widget-insert instance-banner)
  (widget-insert "\n")
  
  (when (iron-main-running-machine "Hercules")
    (iron-main-panels--hercules-top-subpanel
     iron-main-hercules-os-dir
     iron-main-hercules-dasd-dir)

    (iron-main-panels--command-field)
  
    (setq-local iron-main-panels--cmds-links
		(iron-main-panels--insert-command-widgets
		 iron-main-panels--cmds))
    )

  ;; (iron-main-help-field) ; Not yet.
  (message "IMMP00I: My Emacs thinks it's an ISPF!")
  (prog1 (widget-setup)
    (widget-forward 1))
  )


(defun iron-main-panels--hercules-top-subpanel (os-dir dasd-dir)
  "Internal function setting up the Hercules part of the top panel.

The arguments OS-DIR and DASD-DIR are referring to the directories
where the relevant bits and pieces used by the emulator can be found."

  (ignore os-dir dasd-dir)
  
  ;; This function is just a code organization/refactoring tool.   Do
  ;; not call by itself.

  (widget-insert "OS   ")
  (widget-create 'directory :value iron-main-hercules-os-dir
		 :tag ""
		 :size (+ 4
			  (max (length iron-main-hercules-os-dir)
			       (length iron-main-hercules-dasd-dir))))
  (widget-insert "\n")
  (widget-insert "DASDs")
  (widget-create 'directory :value iron-main-hercules-dasd-dir
		 :tag ""
		 :size (+ 4
			  (max (length iron-main-hercules-os-dir)
			       (length iron-main-hercules-dasd-dir))))
  (widget-insert "\n\n")
  (widget-insert (make-string 72 175))	; 175 is the "overline"
  (widget-insert "\n")
  )


(defvar-local iron-main-panels--hs-devinfo-ins-pt nil)


(defun iron-main-panels--hercules-clean-devlist (devtype devlist-string)
   "Remove extra noise that is generated for a teminal output and format.

DEVTYPE is one of the \"devlist\" possible arguments; DEVLIST-STRING
is the string result from invoking the command to the running Hercules."

  ;; To understand this processing, check the output of the Hercules
  ;; command "devlist DEVTYPE".

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

    ;; Now we format depending on `devtype'
    (cond ((string-equal devtype "DASD")
	   (message "IMHSP2I: formatting DASD list.")
	   (setq result
		 (iron-main-panels--format-dasd-list result))
	   )
	  (t
	   ;; Noting fttb
	   (message "IMHSP2I: formatting %s list." devtype)
	   (setq result
		 (format "Available %ss.\n\n%s"
			 devtype
			 result))
	   )
	  )
    result
    ))


(defun iron-main-panels--format-dasd-list (dasdlist)
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

  (let ((result "")
	;; I should rewrite the next regexp breaking it up and using
	;; RX.
	(col-regexp
	 "\\([:0-9A-Z]+\\) \\([0-9]+\\) \\(.+\\) \\(\\[[^]]+?\\]\\) \\(\\[[^]]+?\\]\\) \\(IO\\[[^]]+?\\]\\) \\(.+\\)")
	(result-regexp
	 "\\1\t\\2\t\\3\t\\4\t\\5\t\\6\t\\7")
	)
    (setf result
	  (replace-regexp-in-string col-regexp
				    result-regexp
				    dasdlist))
    
    ;; Not quite right yet, but gettng there...
    (format "%s\n%s"
	    "DEVID\tMODEL\tHOST FOLDER\tCYL\tSFS\tIO(channels)\tSTATUS"
	    result)
    ))


(cl-defun iron-main-panels--hercules-system (session &rest args)
  "Hercules system inspection panel.

Given a SESSION sets up the \"system\" panel.  ARGS are passed downstream
if needed."

  (ignore args)
  
  (cl-assert (iron-main-session-p session) t
	     "SESSION %S is not a `iron-main-session'"
	     session)

  (switch-to-buffer "*IRON MAIN Hercules system*")

  (kill-all-local-variables)

  (let ((inhibit-read-only t))
    (erase-buffer))

  (iron-main-panel-mode)

  ;; Now that we have the session, some of these are repetitions.

  (setq-local iron-main-panels--session session)
  
  (setq-local iron-main-machine
	      (iron-main-session-machine session))
  (setq-local iron-main-os-flavor
	      (iron-main-session-os-flavor session))

  (setq-local iron-main-panels--tag "Hercules system")
  
  (iron-main-panels--title-field "Hercules system")

  ;; Let's start!

  ;; Devices retrievable from the Hercules command 'devlist'.
  ;; CTCA, DASD, DSP, FCP, LINE, OSA, PCH, PRT, RDR, and TAPE.

  (let ((devtypes
	 (list "CTCA" "DASD" "DSP" "FCP" "LINE" "OSA" "PCH" "PRT"
	       "RDR" "TAPE"))
	)
  
    (widget-insert "Available devices\n\n")

    (dolist (devtype devtypes)
      (widget-create 'push-button
		     :notify
		     (lambda (w cw &rest ignore)
		       (ignore cw ignore)
		       (message ">>> Pressed %s" (widget-value w))

		       (let ((dev-list
			      (iron-main-hercules-devlist (widget-value w)))
			  
			     (dev-list-clean "")
			     )
			 (when dev-list
			   (when iron-main-panels--hs-devinfo-ins-pt
			     ;; (message ">>> Clean %s devlist." (widget-value w))
			     (setq dev-list-clean
				   (iron-main-panels--hercules-clean-devlist
				    (widget-value w)
				    dev-list))
			     ;; (message ">>> Cleaned helpstring.")
			     (goto-char iron-main-panels--hs-devinfo-ins-pt)
			     (save-excursion
			       (let ((inhibit-read-only t)
				     (inhibit-modification-hooks t)
				     )
				 (delete-region
				  iron-main-panels--hs-devinfo-ins-pt
			    	  (point-max))))
					 
			     (save-excursion
			       ;; Clean up "HHC0*I" messages before inserting.
			       (widget-insert dev-list-clean)
			       )))
			 ))
		     devtype)
      (widget-insert "  "))

    (widget-insert "\n\n")
    (setq-local iron-main-panels--hs-devinfo-ins-pt (point))
    )
  
  (message "IMHS00I: Hercules system.")
  (prog1 (widget-setup)
    (widget-forward 1))
  )


;; Help panel.
;; -----------

(defvar-local iron-main-panels--help-ins-pt nil)
(defvar-local iron-main-panels--help-cmd-widget nil)

(defun iron-main-panels--hercules-clean-help (helpstring)
  "Remove extra noise that is generated for a teminal output.

The Hercules \"help\" command formats its output assuming a teminal
output; HELPSTRING contains such output and is cleaned up for
presentation in the IRON MAIN panel/buffer."

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
    
    result
  ))


(cl-defun iron-main-panels--hercules-help (session &rest args)
  "Hercules help panel.

Given a SESSION sets up the \"help\" panel.  ARGS are passed downstream
if needed."

  (ignore args)
  
  (cl-assert (iron-main-session-p session) t
	     "SESSION %S is not a `iron-main-session'"
	     session)

  (switch-to-buffer "*IRON MAIN Hercules help*")

  (kill-all-local-variables)

  (let ((inhibit-read-only t))
    (erase-buffer))
  
  (iron-main-panel-mode)

  ;; Now that we have the session, some of these are repetitions.

  (setq-local iron-main-panels--session session)
  
  (setq-local iron-main-machine
	      (iron-main-session-machine session))
  (setq-local iron-main-os-flavor
	      (iron-main-session-os-flavor session))

  (setq-local iron-main-panels--tag "Hercules help")
  
  (iron-main-panels--title-field "Hercules help")

  ;; Let's start!

  (setq-local iron-main-panels--help-ins-pt (point))

  ;; (message ">>> Point %s" help-ins-pt)
  (setq-local
   iron-main-panels--help-cmd-widget
   (widget-create 'editable-field
		  :size 10
		  :format "Hercules command (empty for a list): %v"

		  :keymap
		  iron-main-panels-editable-field-keymap

		  :action
		  (lambda (w &rest ignore)
		    (ignore ignore)
		    (if (string-equal "" (widget-value w))
			(message "IMHS00I: Available commands... (%s)"
			       iron-main-panels--help-ins-pt)
		      (message "IMHS00I: Getting help for '%s' (%s)."
				 (widget-value w)
				 iron-main-panels--help-ins-pt)
		      )
		    (let ((helpstring
			    (iron-main-hercules-help (widget-value w)))
			  
			  (helpstring-clean "")
			  )
		      (when helpstring
			(when iron-main-panels--help-ins-pt
			  ;; (message ">>> Clean helpstring.")
			  (setq helpstring-clean
				(iron-main-panels--hercules-clean-help
				 helpstring)
				)
			  ;; (message ">>> Cleaned helpstring.")
			  (goto-char iron-main-panels--help-ins-pt)
			  (save-excursion
			    (let ((inhibit-read-only t)
				  (inhibit-modification-hooks t)
				  )
			      (delete-region
			       iron-main-panels--help-ins-pt
			       (point-max))))
					 
			  (save-excursion
			    ;; Clean up "HHC0*I" messages before inserting.
			    (widget-insert helpstring-clean)
			    )))
		      ))		; Lambda
		  ""			; Initial value.
		  )
   )
	
  (widget-insert "\n")
  (setq iron-main-panels--help-ins-pt (point))
  
  (message "IMHS00I: Hercules help.")
  (prog1 (widget-setup)
    (widget-forward 1))
  )


;;; Panel navigation.
;;; =================

(cl-defun iron-main-panels--exit-panel (&optional
					(panel-to-exit (current-buffer)))
  "Exit the current panel 'popping' the 'stack' of panels.

PANEL-TO-EXIT is the panel to exit, defaulting to the current buffer.
If PANEL-TO-EXIT is not an IRON-MAIN panel, then this function has no
effect.  If the 'back' buffer is not live"

  (interactive)

  (message "IMPF01I Exiting %S" panel-to-exit)
  (with-current-buffer panel-to-exit
    (message ">>> in-panel %S, panel-tag %S, back %S, live %S"
	     iron-main-panels--in-panel
	     iron-main-panels--tag
	     iron-main-panels--back
	     (and (bufferp iron-main-panels--back)
		  (buffer-live-p iron-main-panels--back)))
    (when iron-main-panels--in-panel
      (when (string= iron-main-panels--tag "Top")
	(message "IMMP02I: Exiting IRON MAIN...")
	(sleep-for 3)
	(iron-main-session-delete iron-main-panels--session))

      ;; Order of `switch-to-buffer' and `kill-buffer' is important.
      (if (and (bufferp iron-main-panels--back)
	       (buffer-live-p iron-main-panels--back))
	  (progn
	    (switch-to-buffer iron-main-panels--back nil t)
	    (kill-buffer panel-to-exit))
	(progn
	  ;; iron-main-panel-back not a buffer or not live.
	  (switch-to-buffer "*GNU Emacs*")
	  (kill-buffer panel-to-exit))
	))))


(defun iron-main-panels--invoke-panel (from panel-start-function &rest args)
  "Start a new panel by calling PANEL-START-FUNCTION.

The function PANEL-START-FUNCTION is one of the functions setting
up a panel; it is called by applying it to the session attached
to FROM and ARGS.  After calling the PANEL-START-FUNCTION,
`iron-main-panels--invoke-panel' assumes that Emacs has switched
to a new panel/buffer and it sets the buffer local variable
`iron-main-panel-back' variable to FROM (which should be a
panel/buffer itself.

The function returns a list containing two items: the current
buffer (which should be the buffer associated to the panel
created by PANEL-START-FUNCTION, and FROM.

If PANEL-FUNCTION is NIL, this is a no-op"
  
  (when panel-start-function
    (apply panel-start-function
	   (iron-main-panels--get-session from)
	   args)

    (message ">>> Panel invoked; setting back to %s" from)
    
    (setq-local iron-main-panels--back from)
    (setq-local iron-main-panels--session
		(iron-main-panels--get-session from))
    (list (current-buffer) from))
  )


;;; Epilogue

(provide 'iron-main-panels)

;;; iron-main-panels.el ends here
