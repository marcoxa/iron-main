;;; iron-main-panels.el --- Testing widegts.
;;; -*- Mode: Emacs-Lisp; lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'widget)

(eval-when-compile
  (require 'wid-edit))


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



(defun iron-main-panel-title ()
  "Create the the IRON MAIN panel title."
  (widget-insert "\n")
  (widget-insert "IRON MAIN Dataset Panel\n")
  (widget-insert (make-string 72 ?_))
  (widget-insert "\n\n")
  )

(defun iron-main-dsname-item ()
  "Create the `dsname' editable-field widget in the IRON MAIN panel."
  (setq dsname-widget
	(widget-create 'editable-field
                       :size 46	   ; A name is at most 44 plus quotes.
                       :format "Data set name: %v " ; Text after the field!
		       (iron-main-ds-rep-name iron-main-current-ds)
		       )))


(defun panel-iron-main ()
  "Create the widgets from the Widget manual."
  (interactive)
  (switch-to-buffer "*IRON MAIN example*")
  (kill-all-local-variables)
  (make-local-variable 'panel-iron-main-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))

  (iron-main-panel-title)
  
  (iron-main-dsname-item)
			 
  (widget-insert "\n\n")
  
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
			   (message "Data set name: <%s>."
				    (widget-value dsname-widget)
				    ))
                 "Allocate")
  (widget-insert "    ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
			   (message "Data set name: %s."
				    (widget-value dsname-widget)
				    ))
                 "Allocate and Save")
  (widget-insert "    ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
			   (message "Data set name: %s."
				    (widget-value dsname-widget)
				    ))
                 "Save")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
  )


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

  (iron-main-panel-title)
  
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
                 :notify (lambda (&rest ignore)
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

  (iron-main-panel-title)
  
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


;;; Epilogue

(provide 'iron-main-panels)

;;; iron-main-panels.el ends here
