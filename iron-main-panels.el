;;; iron-main-panels.el --- Testing widegts.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'widget)
(require 'cl-lib)
(require 'iron-main-vars)

(eval-when-compile
  (require 'wid-edit))


;;; Panels.
;;
;; "Panel" is mainframe-speak for "page", or in the context of Emacs,
;; "window/buffer".
;;
;; The code below uses several examples of the "widget" library found
;; in the Emacs Internet.


(defvar widget-example-repeat)

(defvar iron-main-current-ds (make-iron-main-ds-rep))

(defvar dsname-widget nil)

(defvar recfm-widget nil)
(defvar lrecl-widget nil)
(defvar blksize-widget nil)
(defvar dsorg-widget nil)

(defvar vol-widget nil)
(defvar unit-widget nil)

(defvar space-unit-widget nil)
(defvar primary-widget nil)
(defvar secondary-widget nil)
(defvar dir-widget nil)


(defvar iron-main-panel-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km widget-keymap)
    (define-key km (kbd "<f3>") 'iron-main-exit-panel)
    (define-key km (kbd "q") 'iron-main-exit-panel)
    (define-key km (kbd "Q") 'iron-main-exit-panel)
    km
    )
  "The IRON MAIN Panel mode key map.

The key map inherits from `widget-keymap'.  The keys '<f3>' (that is,
'PF3'), 'q' and 'Q' exit the current panel.")


(defvar-local iron-main-panel-back nil
  "Local panel variable set by the 'invoking' panel."
  ;; Pretty crude for the time being.
  ;; This just acts as a stack.  Whever a panel is "closed", the
  ;; buffer is killed and the content of this variable, a buffer, if
  ;; not nil and live, is restored.
  )


(defvar-local iron-main-in-panel nil
  "When non NIL, the buffer is an IRON MAIN panel.")


(defvar-local iron-main-panel-tag nil
  "A tag that identifies the type of IRON MAIN panel.

The value can be either a string or a symbol.")


;;;; Panels.

(define-derived-mode iron-main-panel-mode nil "//IRON-MAIN"
  "IRON MAIN Panel Mode.

Major mode for IRON MAIN Panels.  Mostly a container for variables
and a specialized keymap.

You an use the function key `F3' (i.e., `PF3') or the [Qq] keys to
exit a panel.  Exiting the top panel will exit the IRON MAIN
interface."

  (setq-local iron-main-in-panel t
	      iron-main-panel-back nil)

  (use-local-map iron-main-panel-mode-map)
  )


(defun iron-main--panel-title (&optional title)
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


;; Unused

(defun iron-main--help-field ()
  "Create the \"help\" field at the bottom of the window."
  ;; Call last before `widget-setup'.

  ;; The next one is a kludge to position the message on the "last"
  ;; window without resorting (as I probably should) to more
  ;; sophisticated Emacs techniques involving minibubber-less windows
  ;; etc.

  (let ((wh (window-total-height))
	(lc (count-lines (window-start) (window-end)))
	(lp-current (line-number-at-pos))
	(lp-window-end (line-number-at-pos (window-end)))
	)
    (cond ((>= lc wh)
	   (move-to-window-line -1)	; Lst visible line.
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


(defun iron-main-dsname-item ()
  "Create the `dsname' editable-field widget in the IRON MAIN panel."
  (setq dsname-widget
	(widget-create 'editable-field
                       :size 46	   ; A name is at most 44 plus quotes.
                       :format "Data set name: %v " ; Text after the field!
		       (iron-main-ds-rep-name iron-main-current-ds)
		       )))


(defvar iron-main-ds-allocation-dsorg "PDS"
  "The default data set organization (DSORG).")


(defun iron-main-dataset-allocation-panel ()
  "Create the IRON MAIN dataset allocation panel."
  (interactive)
  (switch-to-buffer "*IRON MAIN dataset allocation*")
  (kill-all-local-variables)
  (make-local-variable 'panel-iron-main-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))

  (iron-main--panel-title "Dataset panel")
  
  (iron-main-dsname-item)
			 
  (widget-insert "\n\n")

  (setf recfm-widget
	(widget-create 'editable-field
		       :format "Record format (RECFM):         %v"
		       :value (iron-main-ds-rep-recfm
			       iron-main-current-ds)
		       :size 3
		       :notify (lambda (w &rest ignore)
				 (message "RECF: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-recfm
					iron-main-current-ds)
				       (widget-value w)))
		       ))
  (widget-insert "\n")

  (setf lrecl-widget
	(widget-create 'integer
		       :format "Logical record length (LRECL): %v"
		       :value (iron-main-ds-rep-lrecl
			       iron-main-current-ds)
		       :size 4
		       :value-regexp "[0-9]+"
		       :notify (lambda (w &rest ignore)
				 (message "LRECL: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-lrecl
					iron-main-current-ds)
				       (widget-value w)))
		       ))
  (widget-insert "\n")

  (setf blksize-widget
	(widget-create 'integer
		       :format "Block size (BLKSIZE):          %v"
		       :value (iron-main-ds-rep-blksize
			       iron-main-current-ds)
		       :size 6
		       :value-regexp "[0-9]+"
		       :notify (lambda (w &rest ignore)
				 (message "BLKSIZE: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-blksize
					iron-main-current-ds)
				       (widget-value w)))
		       ))
  (widget-insert "\n\n")

  (widget-insert "Dataset organization (DSORG): \n")
  (setf dsorg-widget
	(widget-create 'radio-button-choice
		       :tag "Dataset organization (DSORG)"
		       :doc "Dataset organization (DSORG)"
		       :value "PO"
		       :void  "PO"
		       :indent 4
		       :help-echo "Choose the dataset organization"
		       :notify (lambda (w &rest ignore)
				 (message "Dataset organization: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-dsorg
					iron-main-current-ds)
				       (widget-value w)))
		       '(item :tag "Partitioned Data Set (PO)"
			      :value "PO")
		       ;; '(item :tag "Partitioned Data Set Extended (PDSE)"
		       ;;        :value "PDSE")
		       '(item :tag "Sequential (PS)"
			      :value "PS")
		       ;; Add other ones.
		       ))
  ;; (setf dsorg-widget
  ;; 	(widget-create 'menu-choice
  ;; 		       :tag "Dataset organization (DSORG)"
  ;; 		       :value "PO"
  ;; 		       :void  "PO"
  ;; 		       :choice "Partitioned Data Set (PDS)"
  ;; 		       :help-echo "Choose the dataset organization"
  ;; 		       :notify (lambda (w &rest ignore)
  ;; 				 (message "Dataset organization: <%s>."
  ;; 					  (widget-value w))
  ;; 				 (setf (iron-main-ds-rep-dsorg
  ;; 					iron-main-current-ds)
  ;; 				       (widget-value w)))
  ;; 		       '(item :tag "Partitioned Data Set (PO)"
  ;; 			      :value "PO")
  ;; 		       ;; '(item :tag "Partitioned Data Set Extended (PDSE)"
  ;; 		       ;;        :value "PDSE")
  ;; 		       '(item :tag "Sequential (PS)"
  ;; 			      :value "PS")
  ;; 		       ;; Add other ones.
  ;; 		       ))
  (widget-insert "\n\n")

  (widget-insert "Space allocation:\n")
  (setf space-unit-widget
	(widget-create 'radio-button-choice
		       :tag "Dataset space unit (SPACE)"
		       :value "TRK"
		       :void  "TRK"
		       :indent 4
		       :help-echo "Choose the dataset space unit"
		       :notify (lambda (w &rest ignore)
				 (message "Dataset space unit: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-space-unit
					iron-main-current-ds)
				       (widget-value w)))
		       '(item "TRK")
		       '(item "CYL")
		       '(item "BLK")
		       ;; Add other ones.
		       ))
  
  ;; (setf space-unit-widget
  ;; 	(widget-create 'string
  ;; 		       :format "Space unit: %v"
  ;; 		       :value (iron-main-ds-rep-space-unit
  ;; 			       iron-main-current-ds)
  ;; 		       :size 4
  ;; 		       :value-regexp "\\(CYL\\|TRK\\)"
  ;; 		       :notify (lambda (w &rest ignore)
  ;; 				 (message "Space unit: <%s>."
  ;; 					  (widget-value w))
  ;; 				 (setf (iron-main-ds-rep-space-unit
  ;; 					iron-main-current-ds)
  ;; 				       (widget-value w)))
  ;;                   ))
  
  (widget-insert "\n")
  (setf primary-widget
	(widget-create 'integer
		       :format "Primary: %v"
		       :value (iron-main-ds-rep-lrecl
			       iron-main-current-ds)
		       :size 8
		       :value-regexp "[0-9]+"
		       :notify (lambda (w &rest ignore)
				 (message "Primary: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-primary
					iron-main-current-ds)
				       (widget-value w)))
		       ))
  (widget-insert "    ")
  (setf secondary-widget
	(widget-create 'integer
		       :format "Secondary: %v"
		       :value (iron-main-ds-rep-secondary
			       iron-main-current-ds)
		       :size 8
		       :value-regexp "[0-9]+"
		       :notify (lambda (w &rest ignore)
				 (message "Secondary: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-secondary
					iron-main-current-ds)
				       (widget-value w)))
		       ))
  (widget-insert "    ")
  (setf dir-widget
	(widget-create 'integer
		       :format "Directory blocks: %v"
		       :value (iron-main-ds-rep-directory
			       iron-main-current-ds)
		       :size 4
		       :value-regexp "[0-9]+"
		       :notify (lambda (w &rest ignore)
				 (message "Directory blocks: <%s>."
					  (widget-value w))
				 (setf (iron-main-ds-rep-directory
					iron-main-current-ds)
				       (widget-value w)))
		       ))

  ;; (widget-insert "\n\n\n")
  
  (widget-insert "\n")
  (widget-insert (make-string 72 ?_))
  (widget-insert "\n")
  
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
			   (setf (iron-main-ds-rep-name
				  iron-main-current-ds)
				 (widget-value dsname-widget)
				 (iron-main-ds-rep-dsorg
				  iron-main-current-ds)
				 (widget-value dsorg-widget))
			   (message "DD: <%s>."
				    (iron-main-ds-to-string
				     iron-main-current-ds)
				    ))
                 "Allocate")
  (widget-insert "    ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
			   (message "JCL buffer for '%s': ...."
				    (iron-main-ds-rep-name
				     iron-main-current-ds)
				    ))
                 "View job buffer")
  (widget-insert "    ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
			   (message "Data set name: %s."
				    (iron-main-ds-rep-name
				     iron-main-current-ds)
				    )
			   (iron-main-dataset-save-panel)
			   )
                 "Allocate and Save")
  (widget-insert "    ")
  (widget-create 'push-button
                 :notify
		 (lambda (&rest ignore)
		   (message "Cancelled dataset '%s' allocation in the mainframe."
			    (iron-main-ds-rep-name
			     iron-main-current-ds)
			    )
		   )
                 "Cancel")

  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
  )


(defvar filename-widget "")

(defun iron-main-dataset-save-panel ()
  "Create the IRON MAIN dataset save panel."
  (interactive)
  (switch-to-buffer "*IRON MAIN dataset save*")
  (kill-all-local-variables)
  (make-local-variable 'panel-iron-main-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))

  (iron-main--panel-title "Dataset Panel")
  
  (iron-main-dsname-item)
			 
  (widget-insert "\n\n")

  (setq filename-widget
	(widget-create 'file
		       :format "File: %v\n"
		       :value (iron-main-ds-rep-name iron-main-current-ds)
		       :size (- 72 (length "File: "))))

  (widget-insert (make-string 72 ?_))
  (widget-insert "\n")
  
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
			   (message "DD: <%s>."
				    (iron-main-ds-to-string
				     iron-main-current-ds)
				    ))
                 "Save to mainframe")
  (widget-insert "    ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
			   (message "JCL buffer for '%s' and '%s': ...."
				    (iron-main-ds-rep-name
				     iron-main-current-ds)
				    (widget-value filename-widget)
				    ))
                 "View job buffer")
  (widget-insert "    ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
			   (message "Saving dataset '%s' to mainframe cancelled."
				    (iron-main-ds-rep-name
				     iron-main-current-ds)
				    )
			   )
                 "Cancel")

  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
  )


(defvar-local iron-main-hercules-pid nil
  "The PID of the Hercules process.  NIL if it is not running or reachable.")


(defvar-local iron-main-panel-session nil
  "The session a panel is attached to.")


(defun iron-main-panel-get-session (panel)
  "Get the IRON MAIN session attached to PANEL.

PANEL must be a buffer or a buffer name."
  (with-current-buffer panel
    iron-main-panel-session))


(defun iron-main-widget-tag (w)
  "Get the :tag of widget W.

Notes:

This function is necessary because it is inexplicably absent from the
`widget.el' library."
  (plist-get (cdr w) :tag))


(defun iron-main-widget-notify (w)
  "Get the :notify function of widget W.

Notes:

This function is necessary because it is inexplicably absent from the
`widget.el' library."
  (plist-get (cdr w) :notify))


(cl-defun iron-main-frame-panel (&optional
				 (machine iron-main-machine)
				 (os-flavor iron-main-os-flavor)
				 &aux (from-buffer (current-buffer)))
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
  (setq-local iron-main-machine machine
	      iron-main-os-flavor os-flavor
	      iron-main-panel-back from-buffer
	      iron-main-panel-tag "Top")
  
  (when (iron-main-running-machine "Hercules")
    ;; Trying to get the PID.
    ;; All of this should be fatored out.
    (let ((pid (progn
		 (message "IMMP01I: Hercules running; getting PID.")
		 (iron-main-hercules-qpid)))
	  (session
	   (iron-main-session-start 'iron-main-hercules-session))
	  )
      (setq-local iron-main-hercules-pid pid
		  iron-main-panel-session session)

      (if pid
	  (setf (iron-main-hs-pid session)
		pid
		
		iron-main-instance-banner
		(format "%s running %s on %s:%s (%s) from %s"
			iron-main-machine
			iron-main-os-flavor
			iron-main-hercules-http-host
			iron-main-hercules-http-port
			pid
			(system-name)))
	(setf iron-main-instance-banner
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
    (setf iron-main-instance-banner
	  (format "%s running %s not available"
		  iron-main-machine
		  iron-main-os-flavor))
    )
  
  (iron-main--panel-title)
  (widget-insert iron-main-instance-banner)
  (widget-insert "\n")
  
  ;; (widget-insert "OS Hercules dir:      %s\n"
  ;; 		 iron-main-hercules-os-dir)
  ;; (widget-insert "OS Hercules DASD dir: %s\n"
  ;; 		 iron-main-hercules-os-dir)

  (when (iron-main-running-machine "Hercules")
    (iron-main--hercules-top-subpanel iron-main-hercules-os-dir
				      iron-main-hercules-dasd-dir))


  ;; Let's start!

  ;; (iron-main-help-field) ; Not yet.
  (message "IMMP00I: My Emacs thinks it's an ISPF!")
  (prog1 (widget-setup)
    (widget-forward 1))
  )


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

(defvar iron-main--hercules-top-commands
  `(("System" iron-main--hercules-system
     :header "Inspect Hercules system/machine")
    ("Datasets" iron-main--hercules-os-fds-utilities
     :header "Handle files and datasets across systems")
    ("Help" iron-main--hercules-help
     :header "Hercules help")
    ("Exit"   iron-main-exit-panel
     :header "Exit the IRON MAIN current panel or top-level"
     :notify ,(lambda (w &rest args)
		(iron-main-exit-panel)))
    )
  "A 'commands alist' of commands/widget specifications."
  )


(defun iron-main-find-command (cmd commands-alist)
  (assoc cmd commands-alist 'string=))


(defvar-local iron-main-hercules-top-cmds-links ()
  "List of link widgets created for top panel commands.")


(defun iron-main--hercules-top-subpanel (os-dir dasd-dir)
  "Internal function setting up the Hercules part of the top panel.

The arguments OS-DIR and DASD-DIR are referring to the directories
where the relevant bits and pieces used by the emulator can be found."

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

  (widget-insert "Command")
  (widget-create 'integer :size 3 :tag "" :value ""
		 :validate
		 (lambda (cmd)
		   (<= 1
		       (widget-value cmd)
		       (length iron-main--hercules-top-commands)))
		 
		 :action
		 (lambda (cmd &optional event)
		   ;; (message ">>> notified")
		   ;; (sleep-for 3)
		   (let* ((cmd-widget
			   (nth (1- (widget-value cmd))
				iron-main-hercules-top-cmds-links))
			  (cmd-notify
			   (iron-main-widget-notify cmd-widget))
			  )
		     ;; (message ">>> calling %s on %s"
		     ;; 	      cmd-notify
		     ;; 	      cmd-widget)
		     (when cmd-notify
		       (apply cmd-notify cmd-widget ()))
		     ))
		 )
		       
  (widget-insert "\n\n")

  (make-local-variable 'iron-main-hercules-top-cmds-links)
  
  (cl-loop for option in iron-main--hercules-top-commands
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
			      (let ((panel-function
				     (cl-second
				      (iron-main-find-command
				       (iron-main-widget-tag w)
				       iron-main--hercules-top-commands)))
				    )
				(iron-main-invoke-panel
				 (current-buffer)
				 panel-function)
				))))
	   into cmd-links
	   do
	   (widget-insert "\n")
	   finally
	   (setq-local iron-main-hercules-top-cmds-links
		       cmd-links)
    )
  )

(defvar-local iron-main--hs-devinfo-ins-pt nil)


(defun iron-main--hercules-clean-devlist (devtype devlist-string)
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
		 (iron-main--format-dasd-list result))
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


(defun iron-main--format-dasd-list (dasdlist)
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


(cl-defun iron-main--hercules-system (session &rest args)
  "Hercules system inspection panel.

Given a SESSION sets up the \"system\" panel.  ARGS are passed downstream
if needed."

  (cl-assert (iron-main-session-p session) t
	     "SESSION %S is not a `iron-main-session'"
	     session)

  (switch-to-buffer "*IRON MAIN Hercules system*")

  (kill-all-local-variables)

  (let ((inhibit-read-only t))
    (erase-buffer))

  (iron-main-panel-mode)

  ;; Now that we have the session, some of these are repetitions.

  (setq-local iron-main-panel-session session)
  
  (setq-local iron-main-machine
	      (iron-main-session-machine session)
	      iron-main-os-flavor
	      (iron-main-session-os-flavor session))

  (setq-local iron-main-panel-tag "Hercules system")
  
  (iron-main--panel-title "Hercules system")

  ;; Let's start!

  ;; (when (iron-main-running-machine "Hercules")
  ;;   (iron-main--hercules-top-subpanel iron-main-hercules-os-dir
  ;; 				      iron-main-hercules-dasd-dir))

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
		       (message ">>> Pressed %s" (widget-value w))

		       (let ((dev-list
			      (iron-main-hercules-devlist (widget-value w)))
			  
			     (dev-list-clean "")
			     )
			 (when dev-list
			   (when iron-main--hs-devinfo-ins-pt
			     ;; (message ">>> Clean %s devlist." (widget-value w))
			     (setq dev-list-clean
				   (iron-main--hercules-clean-devlist
				    (widget-value w)
				    dev-list))
			     ;; (message ">>> Cleaned helpstring.")
			     (goto-char iron-main--hs-devinfo-ins-pt)
			     (save-excursion
			       (let ((inhibit-read-only t)
				     (inhibit-modification-hooks t)
				     )
				 (delete-region iron-main--hs-devinfo-ins-pt
			    			(point-max))))
					 
			     (save-excursion
			       ;; Clean up "HHC0*I" messages before inserting.
			       (widget-insert dev-list-clean)
			       )))
			 ))
		     devtype)
      (widget-insert "  "))

    (widget-insert "\n\n")
    (setq-local iron-main--hs-devinfo-ins-pt (point))
    )
  
  (message "IMHS00I: Hercules system.")
  (prog1 (widget-setup)
    (widget-forward 1))
  )


(defvar-local help-ins-pt nil)
(defvar-local help-cmd-widget nil)

(defun iron-main--hercules-clean-help (helpstring)
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


(cl-defun iron-main--hercules-help (session &rest args)
  "Hercules help panel.

Given a SESSION sets up the \"help\" panel.  ARGS are passed downstream
if needed."

  (cl-assert (iron-main-session-p session) t
	     "SESSION %S is not a `iron-main-session'"
	     session)

  (switch-to-buffer "*IRON MAIN Hercules help*")

  (kill-all-local-variables)

  (let ((inhibit-read-only t))
    (erase-buffer))
  (iron-main-panel-mode)

  ;; Now that we have the session, some of these are repetitions.

  (setq-local iron-main-panel-session session)
  
  (setq-local iron-main-machine
	      (iron-main-session-machine session)
	      iron-main-os-flavor
	      (iron-main-session-os-flavor session))

  (setq-local iron-main-panel-tag "Hercules help")
  
  (iron-main--panel-title "Hercules help")

  ;; Let's start!

  (setq-local help-ins-pt (point))

  ;; (message ">>> Point %s" help-ins-pt)
  (setq-local
   help-cmd-widget
   (widget-create 'editable-field
		  :size 10
		  :format "Hercules command (empty for a list): %v"
		  :action
		  (lambda (w &rest ignore)
		    (if (string-equal "" (widget-value w))
			(message "IMHS00I: Getting help for '%s' (%s)."
				 (widget-value w)
				 help-ins-pt)
		      (message "IMHS00I: Available commands... (%s)"
			       help-ins-pt)
		      )
		    (let ((helpstring
			    (iron-main-hercules-help (widget-value w)))
			  
			  (helpstring-clean "")
			  )
		      (when helpstring
			(when help-ins-pt
			  ;; (message ">>> Clean helpstring.")
			  (setq helpstring-clean
				(iron-main--hercules-clean-help
				 helpstring)
				)
			  ;; (message ">>> Cleaned helpstring.")
			  (goto-char help-ins-pt)
			  (save-excursion
			    (let ((inhibit-read-only t)
				  (inhibit-modification-hooks t)
				  )
			      (delete-region help-ins-pt
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
  (setq help-ins-pt (point))
  
  (message "IMHS00I: Hercules help.")
  (prog1 (widget-setup)
    (widget-forward 1))
  )


;;; Panel navigation.

(cl-defun iron-main-exit-panel (&optional (panel-to-exit
					   (current-buffer)))
  "Exit the current panel 'popping' the 'stack' of panels.

PANEL-TO-EXIT is the panel to exit, defaulting to the current buffer.
If PANEL-TO-EXIT is not an IRON-MAIN panel, then this function has no
effect.  If the 'back' buffer is not live"

  (interactive)

  (message "IMPF01I Exiting %S" panel-to-exit)
  (with-current-buffer panel-to-exit
    (message ">>> in-panel %S, panel-tag %S, back %S, live %S"
	     iron-main-in-panel
	     iron-main-panel-tag
	     iron-main-panel-back
	     (and (bufferp iron-main-panel-back)
		  (buffer-live-p iron-main-panel-back)))
    (when iron-main-in-panel
      (when (string= iron-main-panel-tag "Top")
	(message "IMMP02I: Exiting IRON MAIN...")
	(sleep-for 3)
	(iron-main-session-delete iron-main-panel-session))

      ;; Order of `switch-to-buffer' and `kill-buffer' is important.
      (if (and (bufferp iron-main-panel-back)
	       (buffer-live-p iron-main-panel-back))
	  (progn
	    (switch-to-buffer iron-main-panel-back nil t)
	    (kill-buffer panel-to-exit))
	(progn
	  ;; iron-main-panel-back not a buffer or not live.
	  (switch-to-buffer "*GNU Emacs*")
	  (kill-buffer panel-to-exit))
	))))


(defun iron-main-invoke-panel (from panel-start-function &rest args)
  "Start a new panel by calling PANEL-START-FUNCTION.

The function PANEL-START-FUNCTION is one of the functions setting
up a panel; it is called by applying it to the session
attached to FROM and ARGS.  After calling the
PANEL-START-FUNCTION, `iron-main-invoke-panel' assumes that Emacs
has switched to a new panel/buffer and it sets the buffer local
variable `iron-main-panel-back' variable to FROM (which should be
a panel/buffer itself.

The function returns a list containing two items: the current
buffer (which should be the buffer associated to the panel
created by PANEL-START-FUNCTION, and FROM.

If PANEL-FUNCTION is NIL, this is a no-op"
  
  (when panel-start-function
    (apply panel-start-function
	   (iron-main-panel-get-session from)
	   args)
    (setq-local iron-main-panel-back
		from

		iron-main-panel-session
		(iron-main-panel-get-session from))
    (list (current-buffer) from))
  )


;;; Epilogue

(provide 'iron-main-panels)

;;; iron-main-panels.el ends here
