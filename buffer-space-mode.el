;; This mode should only be local to the switcher buffer
;; So you call the switcher then move around there
;; otherwise we need new keybindings and the mode goes global


;; Modes are called with M-x buffer-space-mode
;; However, this might only apply to the localized select buffer

(define-minor-mode buffer-space-mode
  "A mode to select and load projects geometrically."
  :lighter "bsm" ; shows up in mode line along others
  :keymap '(([left] . look-left)
            ([right] . look-right)
            ([up] . look-up)
            ([down] . look-down))
  )

;;; Setup

;; For now I'm just shoving one space in later it'll be a collection
(defvar *buffer-space-spaces* (make-hash-table :test 'equal))

;; each grouping of buffers should probably be EIEIO container
;; TODO 

(defun seed-buffer-space-spaces ()
  "Seeds the *buffer-space-spaces* attempts to make a square."
    (setf *buffer-space-spaces* (make-hash-table :test 'equal))
    (let* ((buffers (buffer-list))
           (rect-edge-length (ceiling (sqrt (length buffers)))))
      (cl-block early-exit ; kind of gross, refactor to calculate x and y while iterating.
        (dotimes (x rect-edge-length)
          (dotimes (y rect-edge-length)
            (puthash (vector x y) (car buffers) *buffer-space-spaces*)
            (setf buffers (cdr buffers))
            (when (null buffers)
              (cl-return-from early-exit))))))
    *buffer-space-spaces*)

;;; Movement functions (interactive to hook to keys)

(defun look-left ()
  (interactive)
  (print "Looking left.")
  )

(defun look-down-left ()
  (interactive)
  (print "Looking bottom left.")
  )

(defun look-down ()
  (interactive)
  (print "Looking bottom.")
  )

(defun look-down-right ()
  (interactive)
  (print "Looking bottom right.")
  )

(defun look-right ()
  (interactive)
  (print "Looking right.")
  )

(defun look-up-right ()
  (interactive)
  (print "Looking top right.")
  )

(defun look-up ()
  (interactive)
  (print "Looking top.")
  )

(defun look-up-left ()
    (interactive)
  (print "Looking top left.")
  )
