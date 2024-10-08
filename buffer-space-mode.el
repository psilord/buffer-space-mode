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
            ([down] . bsm-look-down)))

;; For testing
(define-key global-map (kbd "C-c h") 'bsm-look-left)
(define-key global-map (kbd "C-c l") 'bsm-look-right)
(define-key global-map (kbd "C-c k") 'bsm-look-up)
(define-key global-map (kbd "C-c j") 'bsm-look-down)


(defun my-python-mode-keybindings ()
  "Custom keybindings for Python mode."
  (define-key python-mode-map (kbd "C-c C-r") 'run-python))

(add-hook 'python-mode-hook 'my-python-mode-keybindings)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For now, this is a vector of hash tables that describe spaces
;;
(defvar *bsm-buffer-space-spaces* (vector
                                   (make-hash-table :test 'equal)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The bsm-error handling system and associated functions
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; used with bsm-error
(defun bsm-snapshot-stack ()
  "Return a vector with index 0 being the inner most stack frame containing
the call to this function."
  (let* ((bt nil)
         (frame-num 0))
    (mapbacktrace
     (lambda (evald func args flags)
       (push (list frame-num (list evald func args flags)) bt)
       (cl-incf frame-num)))
    (map 'vector 'identity (nreverse bt))))

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

    ;; TODO: a choice to enable to t when you want to see more detail.
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
          (mapcar
           (lambda (frame-spec)
             ;; TODO: A choice here to include arguments or not,
             ;; currently disabled.
             (if t
                 (second (second frame-spec))
               (list* (second (second frame-spec))
                      (third (second frame-spec)))))

           ;; And the list of bsm- frames of interest.
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
enclosing function prefixed with bsm- along with a userful stack
trace and put it into the error message."
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
  (sqrt (reduce '+ (map 'vector (lambda (x) (expt x 2))
                        (map 'vector '- p1 p0)))))

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


(defun bsm-seed-buffer-space-spaces ()
  "Seeds the *bsm-buffer-space-spaces* attempts to make a square."
  (setf *bsm-buffer-space-spaces* (vector
                                   (make-hash-table :test 'equal)))
  (let* ((buffers (buffer-list))
         (rect-edge-length (ceiling (sqrt (length buffers)))))
    (cl-loop for i below (length buffers)
             for buf in buffers do
             (let ((x (mod i rect-edge-length))
                   (y (/ i rect-edge-length)))
               (puthash `(,x ,y)
                        (bsm-entity-buffer :bsm-pinp nil
                                           :bsm-loc (vector x y)
                                           :bsm-buffer buf)
                        (elt *bsm-buffer-space-spaces* 0))))
    *bsm-buffer-space-spaces*))


;; TODO - you'll see a lot of pos below which is the index of the current workspace
;; this should probably be a global

(defun bsm-dump-buffer-space (&optional pos)
  "Display a dump of current buffers at desktop at position pos."
  (let* ((pos (or pos 0))
         (spaces (elt *bsm-buffer-space-spaces* pos)))
    (princ (format ";; The buffer-space contains %s entities:\n"
                   (hash-table-count spaces)))
    (maphash (lambda (loc ent)
               (princ (format ";;  %s -> %s\n" loc (bsm-desc-entity ent))))
             spaces)))

;; We will use the first element for now, later we need to pick one
(defun bsm-buffer-position (buffer &optional pos)
  "Given a buffer (name OR object) in vector position pos return [x y]"
  (setf pos (or pos 0))
  (let ((current-workspace (elt *bsm-buffer-space-spaces* pos))
        (buffer (get-buffer buffer))) ; We need to use str of buffer
    (cl-loop for key in (hash-table-keys current-workspace)
             do (when (equal buffer (bsm-buffer
                                     (gethash key current-workspace)))
                  (return key)))))

(defun bsm-hash-keep (function hash-table)
  "Filters a hash table keeping entries. keeping predicate should accept key and value."
  (let ((temp (make-hash-table :test (hash-table-test hash-table))))
    (cl-loop for key being the hash-keys of hash-table
             for temp-value = (gethash key hash-table)
             when (funcall function key temp-value) do
             (puthash key temp-value temp))
    temp))

;; TODO unify these into one that dispatches on x or y
(defun bsm-get-aligned-x (x &optional pos)
  "Gets a sorted list of all positions aligned with x. Pos is current workspace."
  (setf pos (or pos 0))
  (let* ((buffers-map (elt *bsm-buffer-space-spaces* pos))
         (key-list (hash-table-keys buffers-map))
         (final-list
          (cl-loop for (xx y)
                   in key-list
                   when (= x xx)
                   collect (list xx y)
                   into pts-list
                   finally (return (sort pts-list (lambda (a b)
                                                    (>= (car a) (car b))))))))
    (setcdr (last final-list) final-list)))

(defun bsm-get-aligned-y (y &optional pos)
  "Gets a sorted list of all positions aligned with x. Pos is current workspace."
  (setf pos (or pos 0))
  (let* ((buffers-map (elt *bsm-buffer-space-spaces* pos))
         (key-list (hash-table-keys buffers-map))
         (final-list
          (cl-loop for (x yy)
                   in key-list
                   when (= y yy)
                   collect (list x yy)
                   into pts-list
                   finally (return (sort pts-list (lambda (a b)
                                                    (>= (cadr a) (cadr b))))))))
    (setcdr (last final-list) final-list)))


;; TODO see if we can reduce some of the guts of these
(defun bsm-move-horizontally (offset &optional pos)
  (setf pos (or pos 0))
  (let* ((buffers-map (elt *bsm-buffer-space-spaces* pos))
         (old-buffer (window-buffer (selected-window)))
         (current-xy-pos (bsm-buffer-position old-buffer pos))
         (x (car current-xy-pos))
         (xy-list (bsm-get-aligned-x x pos))
         (x-index (cl-position current-xy-pos xy-list :test #'equal))
         (next-buffer (bsm-buffer (gethash (elt xy-list (+ x-index offset))
                                           buffers-map))))
    (switch-to-buffer next-buffer nil t)))


(defun bsm-move-vertically (offset &optional pos)
  (setf pos (or pos 0))
  (let* ((buffers-map (elt *bsm-buffer-space-spaces* pos))
         (old-buffer (window-buffer (selected-window)))
         (current-xy-pos (bsm-buffer-position old-buffer pos))
         (y (cadr current-xy-pos))
         (xy-list (bsm-get-aligned-y y pos))
         (y-index (cl-position current-xy-pos xy-list :test #'equal))
         (next-buffer (bsm-buffer (gethash (elt xy-list (+ y-index offset))
                                           buffers-map))))
    (switch-to-buffer next-buffer nil t)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement functions (interactive to hook to keys)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bsm-look-left ()
  (interactive)
  (bsm-move-horizontally -1))

(defun bsm-look-down ()
  (interactive)
  (bsm-move-vertically 1))

(defun bsm-look-right ()
  (interactive)
  (bsm-move-horizontally 1))

(defun bsm-look-up ()
  (interactive)
  (bsm-move-vertically -1))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay Prototyping Code
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For now, I will supply a specific window into which the overlay should go.

;;; ;;;;;;;;;;;;;;;;
;;; Test 0
;;; ;;;;;;;;;;;;;;;;

(defvar *bsm-ov-test-buffer* nil)
(defvar *bsm-ov-test-overlay* nil)

(defun bsm-test-ov-0-create ()

  (let ((name "foo.txt"))

    ;; find or make the new buffer.
    (setq *bsm-ov-test-buffer* (get-buffer-create name))

    (with-current-buffer *bsm-ov-test-buffer*
      (erase-buffer)
      (insert "This is a piece of text.\n")
      (insert "It has a couple of lines.\n")
      (insert "Multiple lines of text are a good thing.\n"))

    (let* ((the-window (get-buffer-window name t))
           (start-pos (window-start the-window))
           (end-pos (window-end the-window)))
      (if *bsm-ov-test-overlay*
          (move-overlay *bsm-ov-test-overlay* start-pos end-pos
                        *bsm-ov-test-buffer*)
        (setq *bsm-ov-test-overlay* (make-overlay start-pos end-pos
                                                  *bsm-ov-test-buffer*))))

    ;; Then open the buffer in another frame/window do you can see what
    ;; is going on in it.

    (overlay-put *bsm-ov-test-overlay* 'face '((:background "blue")
                                               (:foreground "yellow")
                                               (:extend t))))

  ;; You should see the color change.

  )

(defun bsm-test-ov-0-show ()
  ;; Then, let's change what's in the overlay. We will keep the size
  ;; of the change exactly how big the overlay is.

  (overlay-put *bsm-ov-test-overlay*
               'display
               ;; 16 characters.
               "|----FOOBAR---|")

  ;; You should see the text change.
  )

(defun bsm-test-ov-0-hide ()

  ;; This will hide the inserted text, revealing the original text.
  (overlay-put *bsm-ov-test-overlay* 'display nil)

  ;; You should see the text change.
  )


(defun bsm-test-ov-0-destroy ()
  ;; Then we delete the overlay
  (delete-overlay *bsm-ov-test-overlay*)
  (setq *bsm-ov-test-overlay* nil))

;;; ;;;;;;;;;;;;;;;;
;;; Test 1
;;; ;;;;;;;;;;;;;;;;

(defclass bsm-framebuffer ()
  ((%bsm-rows :accessor bsm-rows
              :initarg :bsm-rows)
   (%bsm-cols :accessor bsm-cols
              :initarg :bsm-cols)
   (%bsm-store :accessor bsm-store
               :initarg :bsm-store)))


(defun bsm-make-framebuffer (rows cols &optional init-char)
  (princ "making framebuffer")
  (make-instance 'bsm-framebuffer
                 :bsm-rows rows
                 :bsm-cols cols
                 :bsm-store (make-string (* rows cols)
                                         (or init-char ?\s))))

(defun bsm-get-item (fb row col)
  (elt (bsm-store fb) (+ (* (bsm-cols fb) row) col)))

(defun bsm-set-item (fb row col item)
  (setf (elt (bsm-store fb) (+ (* (bsm-cols fb) row) col)) item))

(defun bsm-render (fb the-buffer )
  (with-current-buffer the-buffer
    (erase-buffer)
    (cl-loop for row from 0 below (bsm-rows fb)
             do (cl-loop for col from 0 below (bsm-cols fb)
                         do (insert (bsm-get-item fb row col)))
             ;; NOTE: Last column is dedicated to newlines in the store.
             ;; Otherwise the fringe might show up. Fix it later.
             (insert ?\n))))

(defun bsm-test-ov-1-create ()

  ;; 0. Get rectangular geometry of the body of the current window.
  ;; 1. Save the buffer currently active in the window.
  ;; 2. Create a new buffer and fill it totally with spaces so it fills
  ;;    the body of the current window.
  ;; 3. Draw the ascii UI over it, possibly in a sub rectangle with images
  ;;    of each buffer you are skimming over behind it.

  ;; KEEP GOING.

  (let* ((ui (get-buffer-create "bsm-ui"))
         (fb (bsm-make-framebuffer (window-body-height)
                                   ;; save a column for newlines, unfortunately
                                   (1- (window-body-width))
                                   ?.)))

    ;; Is this actually a useful thing? Or should I just use a buffer itself
    ;; as the "store" for the data?
    (bsm-set-item fb 0 0 ?*)
    (bsm-set-item fb (1- (bsm-rows fb)) (1- (bsm-cols fb)) ?@)
    (bsm-render fb ui)

    (switch-to-buffer ui nil t)

    )

  (set-window-point (selected-window) 0)
  )
