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
;;; Globals
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For now I'm just shoving one space in later it'll be a collection
(defvar *bsm-buffer-space-spaces* (make-hash-table :test 'equal))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The bsm-error handling system and associated functions
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; used with bsm-error
(defun bsm-snapshot-stack ()
  "Return a vector with index 0 being the inner most stack frame containing
the call to this function."
  (let* ((bt (vector))
         (frame-num 0))
    (mapbacktrace
     (lambda (a b c d)
       (setq bt (vconcat bt (list (list frame-num (list a b c d)))))
       (incf frame-num)))
    bt))

;; used with bsm-error
(defun bsm-find-frame-num (selector-func bt start-index)
  "Return the first frame-num corresponding to the frame matched by
the selector-func that began its search at start-index.
If the frame cannot be found that matches the selector-func,
return nil."
  (cl-loop
   for fid from start-index below (length bt)
   do (cl-destructuring-bind (frame-num frame) (elt bt fid)
        (cl-destructuring-bind (evald func args flags) frame
          (when (funcall selector-func evald func args flags)
            (cl-return frame-num))))))

(defun bsm-collect-frames (selector-func bt start-index)
  "Starting at the start-index in bt, collect any frames that match the
selector-func and return the list."
  (cl-loop
   for fid from start-index below (length bt)
   when (cl-destructuring-bind (frame-num frame) (elt bt fid)
          (cl-destructuring-bind (evald func args flags) frame
            (funcall selector-func evald func args flags)))
   collect (elt bt fid)))

;; used with bsm-error
(defun bsm-get-bsm-caller-of (&optional caller-of)
  "Get the symbol of the function this function is called from. If caller-of
is nil, then get the direct bsm-parent of this function. Otherwise get the
caller of the named function passed into caller-of (which is assumed to be
on the stack). This function looks explicitly for bsm- prefixed names."
  (let* ((bt (bsm-snapshot-stack))
         (target-frame-num nil)
         (caller-of (if caller-of caller-of 'bsm-get-bsm-caller-of))
         (bsm-func-stack nil))

    ;; Debugging, enable to t when you want to see more detail.
    (when nil
      (princ (format ";; Full backtrace has %s frame(s):\n" (length bt)))
      (princ ";; -------------------\n")
      (dotimes (fid (length bt))
        (let* ((frame-spec (elt bt fid))
               (frame-num (first frame-spec))
               (frame (second frame-spec)))
          (let ((kind (eql't (car frame))))
            (princ
             (format ";; [%3s] %s--> (%s %s ...)\n"
                     frame-num
                     (cond
                      (kind "*")
                      ((and (< (1+ fid) (length bt))
                            (eql 't (first (second (elt bt (1+ fid))))))
                       " /")
                      (t " |"))
                     (first frame)
                     (second frame))))))
      (princ ";; -------------------\n"))

    ;; Ok, now to do real work:

    ;; 0. Starting from the innermost frame, find the frame which is evald and
    ;; has the name of the function in question.
    (setq target-frame-num
          (bsm-find-frame-num
           (lambda (evald func args flags)
             (and (eql 't evald)
                  (eql caller-of func)))
           bt
           0))

    ;; 1. Now, find the next outer bsm- frame. That's the answer of who called
    ;; the caller-of.
    (setq target-frame-num
          (bsm-find-frame-num
           (lambda (evald func args flags)
             (and (eql 't evald)
                  (string-prefix-p "bsm-" (symbol-name func))))
           bt
           (1+ target-frame-num)))

    ;; 2. starting at this frame, collect all of the bsm- function going back
    ;; to the outermost frame.
    (setq bsm-func-stack
          (mapcar (lambda (frame-spec) (second (second frame-spec)))
                  (bsm-collect-frames
                   (lambda (evald func args flags)
                     (and (eql 't evald)
                          (string-prefix-p "bsm-" (symbol-name func))))
                   bt
                   target-frame-num)))

    ;; The first one is the caller of the caller-of frame. Then, it continues
    ;; to the most outer frame, but only of bsm- prefixed functions.
    bsm-func-stack))

;; used with (and it actually is) bsm-error
(defun bsm-error (fmt &rest args)
  "Call error with the fmt and args, but try to find the most recently
enclosing function prefixed with bsm- and put it into the error message."
  (apply 'error (concat "BSM Error: Stack: %s, Message: " fmt)
         ;; Emit a nice list with arrows in it to help understand what it is.
         (cl-loop for func on (bsm-get-bsm-caller-of 'bsm-error)
                  append (if (cdr func)
                             (list (car func) '<-)
                           (list (car func))))
         args))

;; Testing functions for bsm-error

(defun bsm-broken-garbage-0 ()
  (bsm-euclidean-distance [0 0] [1 1 1]))
(defun bsm-broken-garbage-1 ()
  (bsm-broken-garbage-0))
(defun bsm-broken-garbage-2 ()
  (bsm-broken-garbage-1))
(defun bsm-broken-garbage-3 ()
  (bsm-broken-garbage-2))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bsm-euclidean-distance (p0 p1)
  "Return the Euclidean distance between two points p0 and p1. The
points must be vectors and of the same length."
  (if (/= (length p0) (length p1))
      (bsm-error "Dimension mismatch: p0: %s p1: %s" p0 p1))
  (sqrt (reduce '+ (map 'vector '- p1 p0))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bsm-entity ()
  ((%bsm-pinp :accessor bsm-pinp
              :initarg :bsm-pinp
              :initform nil)
   (%bsm-loc :accessor bsm-loc
             :initarg :bsm-loc
             :type vector)))

(defclass bsm-entity-buffer (bsm-entity)
  ((%bsm-buffer :accessor bsm-buffer
                :initarg :bsm-buffer)))


(cl-defmethod bsm-desc-entity ((entity bsm-entity-buffer))
  (format "entity-buffer: P: %s, L: %s, B: %s"
          (if (bsm-pinp entity) "y" "n")
          (bsm-loc entity)
          (bsm-buffer entity)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; each grouping of buffers should probably be EIEIO container
;; TODO

(defun bsm-seed-buffer-space-spaces ()
  "Seeds the *bsm-buffer-space-spaces* attempts to make a square."
  (setf *bsm-buffer-space-spaces* (make-hash-table :test 'equal))
  (let* ((buffers (buffer-list))
         (rect-edge-length (ceiling (sqrt (length buffers)))))
    (cl-loop for i below (length buffers)
             for buf in buffers do
             (let ((x (mod i rect-edge-length))
                   (y (/ i rect-edge-length)))
               (puthash (vector x y)
                        (bsm-entity-buffer :bsm-pinp nil
                                           :bsm-loc (vector x y)
                                           :bsm-buffer buf)
                        *bsm-buffer-space-spaces*)))
    *bsm-buffer-space-spaces*))

(defun bsm-dump-buffer-space ()
  (princ (format ";; The buffer-space contains %s entities:\n"
                 (hash-table-count *bsm-buffer-space-spaces*)))
  (maphash (lambda (loc ent)
             (princ (format ";;  %s -> %s\n" loc (bsm-desc-entity ent))))
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
