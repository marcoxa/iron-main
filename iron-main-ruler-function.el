;;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-

;;; iron-main-ruler-function.el
;;
;; See the file COPYING license and copyright information.
;;
;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Created: December 4th, 2020.
;;
;; Version: 20201202.1
;;
;; Keywords: languages, operating systems.

;;; Commentary:
;;
;; Functionalities to generate a 1-based ruler when using
;;`ruler-mode'.
;;
;; The IRON MAIN modes handle "cards" (i.e., lines) that are 1-based
;; and not 0-based.  In Emacs 27.x there are some functions and
;; variables that can be used to have buffers with 1-based columns,
;; cfr., `column-number-indicator-zero-based'.  Alas, these
;; functionalities have not been incorporated in `ruler-mode' which is
;; stuck at counting columns from 0.
;;
;; Thus the function `iron-main-ruler-function' (which is a minimally
;; changed clone of `ruler-mode-ruler', which can be used as a value
;; for `ruler-mode-ruler-function'.

;;; Code:

(require 'ruler-mode)

;;;###autoload
(defun iron-main-current-column ()
  "This function returns the `current-column', possibly 1-based.

The function tracks the variable `column-number-indicator-zero-based',
and if this is NIL, then it adds 1 to the result of the low-level
function `current-column'."
  (if column-number-indicator-zero-based
      (current-column)
    (1+ (current-column))))


;;;###autoload
(defun iron-main-ruler-function ()
  "Compute and return a header line ruler.

This function produces a ruler that is 1 based if
`column-number-indicator-zero-based' is NIL.

This function can (and should) be used as a value of
`ruler-mode-ruler-function'."

  ;; Unfortunately `ruler-mode' always counts columns form 0.
  ;; The following is a kludge, although it may not be so, as it seems
  ;; very unlikely that `ruler-mode-text-scaled-window-hscroll'
  ;; would return anyithing other than 0.

  ;; More unfortunately, I had to copy-and-paste the whole function to
  ;; make it work sensibly without messing things up too much.
  ;;
  ;; Finally I had to trap the value of `current-column' (see the
  ;; function `iron-main-current-column'.
  
  (let* ((w (ruler-mode-text-scaled-window-width))
         (m (window-margins))
         (f (window-fringes))
         (i (if display-line-numbers
                ;; FIXME: ruler-mode relies on I being an integer, so
                ;; the column numbers might be slightly off if the
                ;; line-number face is customized.
                (round (line-number-display-width 'columns))
              0))
         (j (ruler-mode-text-scaled-window-hscroll))
         ;; Setup the scrollbar, fringes, and margins areas.
         (lf (ruler-mode-space
              'left-fringe
              'face 'ruler-mode-fringes
              'help-echo (format ruler-mode-fringe-help-echo
                                 "Left" (or (car f) 0))))
         (rf (ruler-mode-space
              'right-fringe
              'face 'ruler-mode-fringes
              'help-echo (format ruler-mode-fringe-help-echo
                                 "Right" (or (cadr f) 0))))
         (lm (ruler-mode-space
              'left-margin
              'face 'ruler-mode-margins
              'help-echo (format ruler-mode-margin-help-echo
                                 "Left" (or (car m) 0))))
         (rm (ruler-mode-space
              'right-margin
              'face 'ruler-mode-margins
              'help-echo (format ruler-mode-margin-help-echo
                                 "Right" (or (cdr m) 0))))
         (sb (ruler-mode-space
              'scroll-bar
              'face 'ruler-mode-pad))
         ;; Remember the scrollbar vertical type.
         (sbvt (car (window-current-scroll-bars)))
         ;; Create an "clean" ruler.
         (ruler
          (propertize
           ;; Make the part of header-line corresponding to the
           ;; line-number display be blank, not filled with
           ;; ruler-mode-basic-graduation-char.
           (if display-line-numbers
               (let* ((lndw (round (line-number-display-width 'columns)))
                      ;; We need a multibyte string here so we could
                      ;; later use aset to insert multibyte characters
                      ;; into that string.
                      (s (make-string lndw ?\s t)))
                 (concat s (make-string (- w lndw)
                                        ruler-mode-basic-graduation-char t)))
             (make-string w ruler-mode-basic-graduation-char t))
           'face 'ruler-mode-default
           'local-map ruler-mode-map
           'help-echo (cond
                       (ruler-mode-show-tab-stops
                        ruler-mode-ruler-help-echo-when-tab-stops)
                       (goal-column
                        ruler-mode-ruler-help-echo-when-goal-column)
                       (ruler-mode-ruler-help-echo))))
         k c)

    ;; Kludge to force the ruler start at column 1.
    (unless column-number-indicator-zero-based
      (setq j 1))
    
    ;; Setup the active area.
    (while (< i w)
      ;; Graduations.
      (cond
       ;; Show a number graduation.
       ((= (mod j 10) 0)
        (setq c (number-to-string (/ j 10))
              m (length c)
              k i)
        (put-text-property
         i (1+ i) 'face 'ruler-mode-column-number
         ruler)
        (while (and (> m 0) (>= k 0))
          (aset ruler k (aref c (setq m (1- m))))
          (setq k (1- k))))
       ;; Show an intermediate graduation.
       ((= (mod j 5) 0)
        (aset ruler i ruler-mode-inter-graduation-char)))
      ;; Special columns.
      (cond
       ;; Show the `current-column' marker.
       ((= j (iron-main-current-column)) ; Fix the column if needed.
        (aset ruler i ruler-mode-current-column-char)
        (put-text-property
         i (1+ i) 'face 'ruler-mode-current-column
         ruler))
       ;; Show the `goal-column' marker.
       ((and goal-column (= j goal-column))
        (aset ruler i ruler-mode-goal-column-char)
        (put-text-property
         i (1+ i) 'face 'ruler-mode-goal-column
         ruler)
	(put-text-property
         i (1+ i) 'mouse-face 'mode-line-highlight
         ruler)
        (put-text-property
         i (1+ i) 'help-echo ruler-mode-goal-column-help-echo
         ruler))
       ;; Show the `comment-column' marker.
       ((= j comment-column)
        (aset ruler i ruler-mode-comment-column-char)
        (put-text-property
         i (1+ i) 'face 'ruler-mode-comment-column
         ruler)
	(put-text-property
         i (1+ i) 'mouse-face 'mode-line-highlight
         ruler)
        (put-text-property
         i (1+ i) 'help-echo ruler-mode-comment-column-help-echo
         ruler))
       ;; Show the `fill-column' marker.
       ((= j fill-column)
        (aset ruler i ruler-mode-fill-column-char)
        (put-text-property
         i (1+ i) 'face 'ruler-mode-fill-column
         ruler)
	(put-text-property
         i (1+ i) 'mouse-face 'mode-line-highlight
         ruler)
        (put-text-property
         i (1+ i) 'help-echo ruler-mode-fill-column-help-echo
         ruler))
       ;; Show the `tab-stop-list' markers.
       ((and ruler-mode-show-tab-stops (= j (indent-next-tab-stop (1- j))))
        (aset ruler i ruler-mode-tab-stop-char)
        (put-text-property
         i (1+ i) 'face 'ruler-mode-tab-stop
         ruler)))
      (setq i (1+ i)
            j (1+ j)))
    ;; Return the ruler propertized string.  Using list here,
    ;; instead of concat visually separate the different areas.
    (if (nth 2 (window-fringes))
        ;; fringes outside margins.
        (list "" (and (eq 'left sbvt) sb) lf lm
              ruler rm rf (and (eq 'right sbvt) sb))
      ;; fringes inside margins.
      (list "" (and (eq 'left sbvt) sb) lm lf
            ruler rf rm (and (eq 'right sbvt) sb)))))

;;;; iron-main-ruler-function.el ends here.
