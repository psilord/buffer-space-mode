;;; buffer-space-mode.el --- Buffer Space Mode  -*- lexical-binding:t -*-

;; This mode should only be local to the switcher buffer
;; So you call the switcher then move around there
;; otherwise we need new keybindings and the mode goes global


;; Modes are called with M-x buffer-space-mode
;; However, this might only apply to the localized select buffer

(define-minor-mode buffer-space-mode
  "A mode to select and load projects geometrically."
  :lighter "bsm"                        ; shows up in mode line along others
  :keymap '(([left] . bsm-look-left)
            ([right] . bsm-look-right)
            ([up] . bsm-look-up)
            ([down] . bsm-look-down))
  )


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bsm-get-current-func-name (&optional caller-of)
  "Get the symbol of the function this function is called from. If caller-of
is nil, then get the direct bsm-parent of this function. Otherwise get the
caller of the named function passed into caller-of (which is assumed to be
on the stack)."
  ;; 5 is the magic number that makes us look
  ;; above this function

  ;; (backtrace)
  (let* ((bt (vector)))
    ;; get a snapshot of the backtrace when we entered this function.
    (let ((frame-num 0))
      (mapbacktrace (lambda (a b c d)
                      (setq bt
                            (vconcat bt
                                     (list (list frame-num (list a b c d)))))
                      (incf frame-num))))

    (princ (format ";; Full backtrace has %s frame(s):\n" (length bt)))
    (princ ";; -------------------\n")
    (dotimes (fid (length bt))
      (let* ((frame-spec (elt bt fid))
             (frame-num (first frame-spec))
             (frame (second frame-spec)))
        (let ((kind (eql't (car frame))))
          (princ (format ";; [%03s] %s--> (%s %s ...)\n"
                         frame-num
                         (if kind
                             "*"
                           " |")
                         (first frame)
                         (second frame))))))
    (princ ";; -------------------\n")

    ;; Ok, now to do real work:

    ;; 0. Starting at the bottom (most recent) frame.
    ;;
    ;; 1. Skip frames up to (t bsm-get-current-func-name)
    ;;
    ;; 2. if caller-of is nil, then skip upto the next t bsm- frame and
    ;;    that's the answer.
    ;;    if caller-of is non-nil, then skip up to the frame identified, then
    ;;    skip upto the next t bsm- frame and that's the answer.
    (when nil
      (dotimes (fid (length bt))
        (let* ((frame-spec (elt bt fid))
               (frame-num (first frame-spec))
               (frame (second frame-spec)))
          (let ((kind (eql't (car frame))))
            (princ (format ";; [%03s] %s--> (%s %s ...)\n"
                           frame-num
                           (if kind
                               "*"
                             " |")
                           (first frame)
                           (second frame)))))))

    :fixme))

(defun bsm-error (fmt &rest args)
  "Call error with the fmt and args, but try to find the most recently
enclosing function prefixed with bsm- and put it into the error message."
  (apply 'error (concat "Error: (%s): " fmt)
         (bsm-get-current-func-name 'bsm-error) args))

(defun bsm-euclidean-distance (p0 p1)
  "Return the Euclidean distance between two points p0 and p1. The
points must be vectors and of the same length."
  (if (/= (length p0) (length p1))
      (bsm-error "Dimension mismatch: p0: %s p1: %s" p0 p1))
  (sqrt (reduce '+ (map 'vector '- p1 p0))))



;; Testing functions for bsm-error

(defun bsm-broken-garbage-0 ()
  (bsm-error "Busted: %s" 42))
(defun bsm-broken-garbage-1 ()
  (bsm-broken-garbage-0))
(defun bsm-broken-garbage-2 ()
  (bsm-broken-garbage-1))
(defun bsm-broken-garbage-3 ()
  (bsm-broken-garbage-2))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bsm-entity ()
  ((%bsm-loc :accessor bsm-loc
             :initarg :bsm-loc
             :type vector)))

(defclass bsm-entity-buffer (bsm-entity)
  ((%bsm-buffer :accessor bsm-buffer
                :initarg :bsm-buffer)))


(cl-defmethod bsm-desc-entity ((entity bsm-entity-buffer))
  (format "entity-buffer[loc: %s buffer: %s]"
          (bsm-loc entity) (bsm-buffer entity)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For now I'm just shoving one space in later it'll be a collection
(defvar *bsm-buffer-space-spaces* (make-hash-table :test 'equal))

;; each grouping of buffers should probably be EIEIO container
;; TODO

(defun bsm-seed-buffer-space-spaces ()
  "Seeds the *bsm-buffer-space-spaces* attempts to make a square."
  (setf *bsm-buffer-space-spaces* (make-hash-table :test 'equal))
  (let* ((buffers (buffer-list))
         (rect-edge-length (ceiling (sqrt (length buffers)))))
    ;; This is kind of gross, refactor to calculate x and y while iterating.
    (cl-block early-exit
      (dotimes (x rect-edge-length)
        (dotimes (y rect-edge-length)
          (puthash (vector x y)
                   (bsm-entity-buffer :bsm-loc (vector x y)
                                      :bsm-buffer (car buffers))
                   *bsm-buffer-space-spaces*)
          (setf buffers (cdr buffers))
          (when (null buffers)
            (cl-return-from early-exit))))))
  *bsm-buffer-space-spaces*)

(defun bsm-dump-buffer-space ()
  (maphash (lambda (loc ent)
             (princ (format ";; %s -> %s\n" loc (bsm-desc-entity ent))))
           *bsm-buffer-space-spaces*))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement functions (interactive to hook to keys)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bsm-look-left ()
  (interactive)
  (print "Looking left.")
  )

(defun bsm-look-down-left ()
  (interactive)
  (print "Looking bottom left.")
  )

(defun bsm-look-down ()
  (interactive)
  (print "Looking bottom.")
  )

(defun bsm-look-down-right ()
  (interactive)
  (print "Looking bottom right.")
  )

(defun bsm-look-right ()
  (interactive)
  (print "Looking right.")
  )

(defun bsm-look-up-right ()
  (interactive)
  (print "Looking top right.")
  )

(defun bsm-look-up ()
  (interactive)
  (print "Looking top.")
  )

(defun bsm-look-up-left ()
  (interactive)
  (print "Looking top left.")
  )
