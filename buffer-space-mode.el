;;; buffer-space-mode.el --- Buffer Space Mode  -*- lexical-binding:t -*-

;; This mode should only be local to the switcher buffer
;; So you call the switcher then move around there
;; otherwise we need new keybindings and the mode goes global

;; Modes are called with M-x buffer-space-mode
;; However, this might only apply to the localized select buffer

(defun bsm-undef-all (&optional perform-gc)
  "unintern all symbols with *bsm- or bsm- as a prefix."
  (cl-flet ((possibly-unintern (symbol)
              (let ((name (symbol-name symbol)))
                (if (or (string-prefix-p "bsm-" name)
                        (string-prefix-p "*bsm-" name))
                    (unintern symbol)))))
    (mapatoms #'possibly-unintern)
    (when perform-gc
      (garbage-collect))))
;; Wipe out all previous definitions cause I'm changing the API so much at this
;; time.
(bsm-undef-all)

;; TODO: This really neeeds to be a major mode.

(defconst buffer-space-mode-map
  ;; NOTE: You can update this, reload the file, and the changed keymap is in
  ;; effect--which is almost always what you want.
  (let ((map (make-sparse-keymap)))
    ;;(define-key map [left] 'bsm-look-left)
    ;;(define-key map [right] 'bsm-look-right)
    ;;(define-key map [up] 'bsm-look-up)
    ;;(define-key map [down] 'bsm-look-down)
    (define-key map (kbd "<f5>") 'bsm-render)
    map))

(define-minor-mode buffer-space-mode
  "A mode to select and load projects geometrically."
  :lighter "bsm")

;; For testing
(define-key global-map (kbd "C-c h") 'bsm-look-left)
(define-key global-map (kbd "C-c l") 'bsm-look-right)
(define-key global-map (kbd "C-c k") 'bsm-look-up)
(define-key global-map (kbd "C-c j") 'bsm-look-down)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For now, there is a single buffer space.
;;
(defvar *bsm-buffer-space* nil)

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

(defun bsm-util-format-ind (indent fmt &rest objects)
  "Compute and return the rendered format string with the supplied
fmt and objects, but put indent spaces infront of ot."
  (apply 'format (concat (make-string indent ?\s) fmt) objects))

;; TODO: maybe alter this to preserve properties and provide &option for
;; choosing this behavior.
(defun bsm-util-replace-char (buffer pos char-as-string)
  "Replace a character in the buffer, not preserving original properties."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (delete-char 1)
      (insert char-as-string))))

(defun bsm-util-replace-string (buffer new-string start end)
  "Replace the string in the buffer range [start, end) by the
new-string. If the new-string is too big, truncate it. If it is
too small, fill the rest with unpropertized space characters to
maintain the same number of characters. Properties are not preserved."
  ;; TODO: If new-string is too small, another option is to leave the other
  ;; characters as you found them. This might be a viable alternative.
  (let* ((num-range-chars (- end start))
         (num-range-chars (if (< num-range-chars 0) 0 num-range-chars))
         (len-new-string (length new-string))
         (clipped-string (subseq new-string 0 (min len-new-string
                                                   num-range-chars)))
         (len-clipped-string (length clipped-string))
         (full-size-string
          (if (< len-clipped-string num-range-chars)
              (concat clipped-string
                      (make-string (- num-range-chars len-clipped-string)
                                   ?\s))
            clipped-string)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char start)
        (delete-char num-range-chars)
        (insert full-size-string)))))

(defun bsm-util-euclidean-distance (p0 p1)
  "Return the Euclidean distance between two points p0 and p1. The
points must be vectors and of the same length."
  (if (/= (length p0) (length p1))
      (bsm-error "Dimension mismatch: p0: %s p1: %s" p0 p1))
  (sqrt (reduce '+ (map 'vector (lambda (x) (expt x 2))
                        (map 'vector '- p1 p0)))))

(defun bsm-util-hash-filter (filter hash-table &optional nil-on-empty)
  "Filters a hash table keeping entries. keeping predicate should
accept key and value. Returns the hash table even if it has a
count of zero UNLESS nil-on-empty is true, in which case nil is
return in that context."
  (let ((results (make-hash-table :test (hash-table-test hash-table))))
    (cl-loop for key being the hash-keys of hash-table
             for value = (gethash key hash-table)
             when (funcall filter key value) do
             (puthash key value results))
    (if (and nil-on-empty
             (zerop (hash-table-count results)))
        nil
      results)))


;; --------------------------------

;; The MIDT is a special hash table implementation.

(defclass bsm-midt ()
  ;; A hash table that allows easy lookup/management by the KEY or the VALUE.
  ((%bsm-midt-not-found-value :reader bsm-midt-not-found-value
                              :initform (gensym "bsm-midt-not-found-value-"))
   (%bsm-midt-by-key :reader bsm-midt-by-key
                     :initarg :bsm-midt-by-key
                     ;; NOTE: hashtable of: KEY -> VALUE
                     :initform nil)
   (%bsm-midt-by-value :reader bsm-midt-by-value
                       :initarg :bsm-midt-by-value
                       ;; NOTE: hashtable of: VALUE -> (cons KEY VALUE)
                       :initform nil)))

(defun bsm-midt-found-p (item midt)
  (not (eq item (bsm-midt-not-found-value midt))))

(defun bsm-midt-make (by-key-test by-value-test)
  (bsm-midt :bsm-midt-by-key (make-hash-table :test by-key-test)
            :bsm-midt-by-value (make-hash-table :test by-value-test)))

(defun bsm-midt-put (key value midt)
  (puthash key value (bsm-midt-by-key midt))
  (puthash value (cons key value) (bsm-midt-by-value midt)))

(defun bsm-midt-get (key-or-value midt &optional default)
  "Will return the associated entry from the mitbl given the key-or-value.
If it is not present return the default."
  (let* ((item-by-key (gethash key-or-value (bsm-midt-by-key midt)
                               (bsm-midt-not-found-value midt))))
    (if (bsm-midt-found-p item-by-key midt)
        item-by-key
      (let ((item-by-value (gethash key-or-value (bsm-midt-by-value midt)
                                    (bsm-midt-not-found-value midt))))
        (if (bsm-midt-found-p item-by-value midt)
            (cdr item-by-value)
          default)))))

(defun bsm-midt-remhash (key-or-value midt)
  (catch 'early-exit
    (let ((item-by-key (gethash key-or-value (bsm-midt-by-key midt)
                                (bsm-midt-not-found-value midt))))
      (when (bsm-midt-found-p item-by-key midt)
        ;; If we found it by the key, then synchronize the by-value table.
        (remhash key-or-value (bsm-midt-by-key midt))
        (remhash item-by-key (bsm-midt-by-value midt))
        (throw 'early-exit nil)))

    ;; Check if we find it by value.
    (let ((item-by-value (gethash key-or-value (bsm-midt-by-value midt)
                                  (bsm-midt-not-found-value midt))))
      (when (bsm-midt-found-p item-by-value midt)
        ;; If we found it by the value, then synchronize the by-key table.
        (remhash (car item-by-value) (bsm-midt-by-key midt))
        (remhash (cdr item-by-value) (bsm-midt-by-value midt))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------

;; This base class represents a rectangular (at this time) boundary wholly
;; contained in a (often) btile which can be rendered given suitable
;; descriptions of where to render it in a derived class. Each side of the
;; boundary can be "open" which means a directional rendering is put there, or
;; "closed" meaning a "wall like" rendering is put there.
(defclass bsm-boundary ()
  ((%bsm-boundary-open-top-p :accessor bsm-boundary-open-top-p
                             :initarg :bsm-boundary-open-top-p
                             :initform nil)
   (%bsm-boundary-open-right-p :accessor bsm-boundary-open-right-p
                               :initarg :bsm-boundary-open-right-p
                               :initform nil)
   (%bsm-boundary-open-bottom-p :accessor bsm-boundary-open-bottom-p
                                :initarg :bsm-boundary-open-bottom-p
                                :initform nil)
   (%bsm-boundary-open-left-p :accessor bsm-boundary-open-left-p
                              :initarg :bsm-boundary-open-left-p
                              :initform nil)))

;; Describe a rectangular boundary starting from [start-row, start-col] to
;; [end-row, end-col]. Note carefully the coordinates are _inclusive_.
(defclass bsm-bound/static (bsm-boundary)
  ((%bsm-bound/static-start-row :accessor bsm-bound/static-start-row
                                :initarg :bsm-bound/static-start-row)
   (%bsm-bound/static-start-col :accessor bsm-bound/static-start-col
                                :initarg :bsm-bound/static-start-col)
   (%bsm-bound/static-end-row :accessor bsm-bound/static-end-row
                              :initarg :bsm-bound/static-end-row)
   (%bsm-bound/static-end-col :accessor bsm-bound/static-end-col
                              :initarg :bsm-bound/static-end-col)))

;; Describe a rectangular boundary by how many rows to reserve away from the
;; outer top, right, bottom, and left sides individually.
(defclass bsm-bound/relative (bsm-boundary)
  ((%bsm-bound/relative-top-reserve
    :accessor bsm-bound/relative-top-reserve
    :initarg :bsm-bound/relative-top-reserve)
   (%bsm-bound/relative-right-reserve
    :accessor bsm-bound/relative-right-reserve
    :initarg :bsm-bound/relative-right-reserve)
   (%bsm-bound/relative-bottom-reserve
    :accessor bsm-bound/relative-bottom-reserve
    :initarg :bsm-bound/relative-bottom-reserve)
   (%bsm-bound/relative-left-reserve
    :accessor bsm-bound/relative-left-reserve
    :initarg :bsm-bound/relative-left-reserve)))


(defun bsm-bound/static-make (start-row start-col end-row end-col
                                        &optional open-top-p open-right-p
                                        open-bottom-p open-left-p)
  "Construct and return a bsm-bound/static instance filled with the supplied
arguments."
  (bsm-bound/static :bsm-bound/static-start-row start-row
                    :bsm-bound/static-start-col start-col
                    :bsm-bound/static-end-row end-row
                    :bsm-bound/static-end-col end-col
                    :bsm-boundary-open-top-p open-top-p
                    :bsm-boundary-open-right-p open-right-p
                    :bsm-boundary-open-bottom-p open-bottom-p
                    :bsm-boundary-open-left-p open-left-p))

(defun bsm-bound/relative-make (top-reserve right-reserve bottom-reserve
                                            left-reserve
                                            &optional open-top-p open-right-p
                                            open-bottom-p open-left-p)
  "Construct and return a bsm-bound/relative instance filled with the supplied
arguments."
  (bsm-bound/relative :bsm-bound/relative-top-reserve top-reserve
                      :bsm-bound/relative-right-reserve right-reserve
                      :bsm-bound/relative-bottom-reserve bottom-reserve
                      :bsm-bound/relative-left-reserve left-reserve
                      :bsm-boundary-open-top-p open-top-p
                      :bsm-boundary-open-right-p open-right-p
                      :bsm-boundary-open-bottom-p open-bottom-p
                      :bsm-boundary-open-left-p open-left-p))

;; --------------------------------

;; This class represents a _preclipped_ rectangle subregion (or a tile) in an
;; emacs *bsm-display-buffer* buffer. The btile is ALWAYS referenced to an
;; absolute character position in the actual display buffer.  The buffer object
;; is a linear array of characters where newlines represent rows of text in a
;; buffer. A constraint about how we use that buffer for display is that the
;; buffer must be filled with the exact number of characters (with newlines at
;; end of line) as fits into an emacs window for the postiion and stride to
;; work out in a tile. The newlines are present to prevent the fringe from
;; being printed out. Note: btiles don't have boundaries in them. The thing
;; using the btile deals with boundary management and rendering the boundary
;; in the btile via utility functions.
;;
;; NOTE: Fringes cannot be turned off in tty mode, so the newline
;; representation must stay to keep a clean fringe area in tty mode.
(defclass bsm-btile ()
  (;; The buffer in which this btile is in an absolute position.
   (%bsm-btile-buffer :accessor bsm-btile-buffer
                      :initarg :bsm-btile-buffer)
   ;; The buffer character position of the upper left corner of this tile.
   (%bsm-btile-pos :accessor bsm-btile-pos
                   :initarg :bsm-btile-pos)
   ;; How many characters to move forward to be at the start of the next row in
   ;; this tile.
   (%bsm-btile-stride :accessor bsm-btile-stride
                      :initarg :bsm-btile-stride)
   ;; How many rows exist in this tile. 0 indexed.
   (%bsm-btile-rows :accessor bsm-btile-rows
                    :initarg :bsm-btile-rows)
   ;; How many columns each row consists of in this tile. 0 indexed.
   (%bsm-btile-cols :accessor bsm-btile-cols
                    :initarg :bsm-btile-cols)))

(defun bsm-btile-make (buffer pos stride rows cols)
  (when (zerop pos)
    (bsm-error "The first charpos index in a buffer is 1, not 0."))
  (bsm-btile :bsm-btile-buffer buffer
             :bsm-btile-pos pos
             :bsm-btile-stride stride
             :bsm-btile-rows rows
             :bsm-btile-cols cols))

(defun bsm-btile-make-subtile (btile subrow-start subcol-start num-rows
                                     num-cols &optional target-btile)

  "Construct a new (or fill in target-btile if supplied) tile that is a
fully contained subregion of the btile. The subrectangle starts at
[subrow-start, subcol-start] and is num-rows and num-cols in extent.
Return the reference to the new, or passed in target-btile, btile."

  (unless (and (>= subrow-start 0)
               (>= subcol-start 0)
               (<= (+ subrow-start num-rows) (bsm-btile-rows btile))
               (<= (+ subcol-start num-cols) (bsm-btile-cols btile)))
    (bsm-error "Subtile must fit into the original btile: btile is %s, subrow-start: %s, subcol-start: %s, num-rows: %s, num-cols: %s\n"
               btile subrow-start subcol-start num-rows num-cols))

  (let ((sub-btile (or target-btile (bsm-btile))))
    (setf (bsm-btile-buffer sub-btile) (bsm-btile-buffer btile)
          (bsm-btile-pos sub-btile) (bsm-btile-get-charpos btile
                                                           subrow-start
                                                           subcol-start)
          (bsm-btile-stride sub-btile) (bsm-btile-stride btile)
          (bsm-btile-rows sub-btile) num-rows
          (bsm-btile-cols sub-btile) num-cols)
    sub-btile))

(defun bsm-btile-valid-coordinate-p (btile row col)
  "Return true if row,col specifies a location inclusive to the btile."
  (and (>= row 0)
       (< row (bsm-btile-rows btile))
       (>= col 0)
       (< col (bsm-btile-cols btile))))

(defun bsm-btile-get-charpos (btile row col)
  "Return the character position of the row,col coordinate in this btile."
  (+ (bsm-btile-pos btile) (* row (bsm-btile-stride btile)) col))

(defun bsm-btile-render-row (btile string row col)
  "Render the string into the buffer starting at the relative
position of row,col in the btile, up to length of string
characters in the row. If the string is too big to fit into the
btile, clip it to fit into the tile. If the initial row,col is
off the btile, do nothing (even if the string would have
otherwise had a part of it show). Return t if anything was
rendered into the buffer nil otherwise."
  (when (bsm-btile-valid-coordinate-p btile row col)
    (let* ((start-pos (bsm-btile-get-charpos btile row col))
           (end-pos (+ start-pos (min (- (bsm-btile-cols btile) col)
                                      (length string)))))
      (bsm-util-replace-string (bsm-btile-buffer btile)
                               string start-pos end-pos)
      t)))

(defun bsm-btile-render-column (btile string row col)
  "Render the string into the buffer starting at the relative
position of row,col in the btile, up to length of string
characters in the column. If the string is too big to fit into
the btile, clip it to fit into the tile. If the initial row,col
is off the btile, do nothing (even if the string would have
otherwize had a part of it show). Return t if anything was
rendered into the buffer nil otherwise."
  (when (bsm-btile-valid-coordinate-p btile row col)
    (let* ((start (bsm-btile-get-charpos btile row col))
           (num-rows (min (- (bsm-btile-rows btile) row)
                          (length string))))
      (dotimes (r num-rows)
        (let ((c (subseq string r (1+ r)))
              (current-row-pos (bsm-btile-get-charpos btile (+ row r) col)))
          (bsm-util-replace-string
           (bsm-btile-buffer btile) c current-row-pos (1+ current-row-pos))))
      t)))

(defun bsm-btile-%render-boundary (btile
                                   start-row start-col end-row end-col
                                   &optional open-top-p open-right-p
                                   open-bottom-p open-left-p)
  "Render a boundary into the btile starting from [start-row, start-col] to
[end-row, end-col], inclusive. Use + for the corners. Use | for
the left and right sides. Use - for the top and bottom sides.  If
open-top-p is true, then replace the - with ^ on the top side. If
open-right-p is true, then replace | with > on the right side. If
open-bottom-p is true, then replace - with v on the bottom. If
open-left-p is true, then replace | with < on the left side. The
btile MUST be at least 2 by 2 AND the boundary must be at least 2
by 2. In addition, start-row must be less than end-row and
start-col must be less than end-col. If not, or, for whatever
reason we don't have enough space, do no rendering. Return t if
rendering happened, nil otherwise."
  (let ((btile-rows (bsm-btile-rows btile))
        (btile-cols (bsm-btile-cols btile))
        ;; 1+ to count the number of rows and cols from the inclusive bounds.
        (bnd-rows (abs (1+ (- end-row start-row))))
        (bnd-cols (abs (1+ (- end-col start-col))))
        (char-closed-top ?-)
        (char-open-top ?^)
        (char-closed-right ?|)
        (char-open-right ?>)
        (char-closed-bottom ?-)
        (char-open-bottom ?v)
        (char-closed-left ?|)
        (char-open-left ?<))

    (when (and (>= btile-rows 2)
               (>= btile-cols 2)
               (>= bnd-rows 2)
               (>= bnd-cols 2)
               (> end-row start-row)
               (> end-col start-col)
               (bsm-btile-valid-coordinate-p btile start-row start-col)
               (bsm-btile-valid-coordinate-p btile end-row end-col))
      (let* ((char-top (if open-top-p
                           char-open-top
                         char-closed-top))
             (char-right (if open-right-p
                             char-open-right
                           char-closed-right))
             (char-bottom (if open-bottom-p
                              char-open-bottom
                            char-closed-bottom))
             (char-left (if open-left-p
                            char-open-left
                          char-closed-left))
             (top-row-inner (make-string (- bnd-cols 2) char-top))
             (top-row (concat "+" top-row-inner "+"))
             (right-col (make-string (- bnd-rows 2) char-right))
             (bottom-row-inner (make-string (- bnd-cols 2) char-bottom))
             (bottom-row (concat "+" bottom-row-inner "+"))
             (left-col (make-string (- bnd-rows 2) char-left)))
        (bsm-btile-render-row btile top-row start-row start-col)
        (bsm-btile-render-column btile right-col (1+ start-row) end-col)
        (bsm-btile-render-row btile bottom-row end-row start-col)
        (bsm-btile-render-column btile left-col (1+ start-row) start-col)
        t))))

(cl-defmethod bsm-btile-render-boundary (btile (boundary bsm-bound/static))
  ;; Directly use the static model.
  (bsm-btile-%render-boundary btile
                              (bsm-bound/static-start-row boundary)
                              (bsm-bound/static-start-col boundary)
                              (bsm-bound/static-end-row boundary)
                              (bsm-bound/static-end-col boundary)
                              (bsm-boundary-open-top-p boundary)
                              (bsm-boundary-open-right-p boundary)
                              (bsm-boundary-open-bottom-p boundary)
                              (bsm-boundary-open-left-p boundary)))

;; If 0 is given for the relative boundaries, then the largest boundary that
;; can be rendered into the btile will be rendered.
(cl-defmethod bsm-btile-render-boundary (btile (boundary bsm-bound/relative))
  ;; Convert the static model, inclusive boundaries.
  (let ((start-row (bsm-bound/relative-top-reserve boundary))
        (start-col (bsm-bound/relative-left-reserve boundary))
        (end-row (- (1- (bsm-btile-rows btile))
                    (bsm-bound/relative-bottom-reserve boundary)))
        (end-col (- (1- (bsm-btile-cols btile))
                    (bsm-bound/relative-right-reserve boundary))))
    (bsm-btile-%render-boundary btile
                                start-row
                                start-col
                                end-row
                                end-col
                                (bsm-boundary-open-top-p boundary)
                                (bsm-boundary-open-right-p boundary)
                                (bsm-boundary-open-bottom-p boundary)
                                (bsm-boundary-open-left-p boundary))))

;; --------------------------------

(defclass bsm-id ()
  ;; mixin class to give things name, nicks, short names, how those names might
  ;; be rendered, etc, etc.
  ((%bsm-id-use-name-p :accessor bsm-id-use-name-p
                       :initarg :bsm-id-use-name-p
                       :initform nil)
   (%bsm-id-name :accessor bsm-id-name
                 :initarg :bsm-id-name
                 :initform nil)))

;; --------------------------------

;; An entity a thing stored in a view. Often it is a buffer that you can
;; switch to or a reference to some other thing like a different buffer-view.
(defclass bsm-entity ()
  ((%bsm-entity-id :reader bsm-entity-id
                   :initarg :bsm-entity-id
                   :initform (bsm-id))
   ;; An item is the primitive thing we're holding.
   (%bsm-entity-item :accessor bsm-entity-item
                     :initarg :bsm-entity-item)))

(defun bsm-entity-is-item (entity item &optional eq-func)
  "Return true if the item held by the entity is equivalent to the
supplied item via the eq-func--otherwise return nil."
  (when entity
    (funcall (or eq-func 'equal) (bsm-entity-item entity) item)))


;; A bsm-entity suitable to hold a reference to a buffer.
(defclass bsm-ent/buf (bsm-entity) ())

(defun bsm-ent/buf-make (buffer &optional name)
  "Create and return a bsm-ent/buf object holding the buffer andd
with the optional name as its name."
  (let ((ent (bsm-ent/buf :bsm-entity-item buffer)))
    (when name
      (let ((id (bsm-entity-id ent)))
        (setf (bsm-id-name id) name
              (bsm-id-use-name-p id) t)))
    ent))

;; A bsm-entity suitable to hold a reference to another bsm-view
(defclass bsm-ent/view (bsm-entity) ())

(defun bsm-ent/view-make (view &optional name)
  "Create and return a bsm-ent/buf object holding the buffer andd
with the optional name as its name."
  (let ((ent (bsm-ent/view :bsm-entity-item view)))
    (when name
      (let ((id (bsm-entity-id ent)))
        (setf (bsm-id-name id) name
              (bsm-id-use-name-p id) t)))
    ent))

;; --------------------------------

;; How we keep track of the location of this entity in a view.
(defclass bsm-loc ()
  ((%bsm-loc-x :accessor bsm-loc-x
               :initarg :bsm-loc-x
               :initform 0)
   (%bsm-loc-y :accessor bsm-loc-y
               :initarg :bsm-loc-y
               :initform 0)))

(defun bsm-loc-equal (k0 k1)
  "Equality tester for two bsm-loc instances. Return T if they are EQ or
if they represent the exact same coordinate."
  (or (eq k0 k1)
      (and (= (bsm-loc-x k0) (bsm-loc-x k1))
           (= (bsm-loc-y k0) (bsm-loc-y k1)))))

(defun bsm-loc-hash (k)
  "Hash function for a bsm-loc instance."
  (+ (* 19 (bsm-loc-x k))
     (* 23 (bsm-loc-y k))))

(define-hash-table-test 'bsm-loc-equal/ht 'bsm-loc-equal 'bsm-loc-hash)

(defun bsm-loc-make (x y)
  (bsm-loc :bsm-loc-x x :bsm-loc-y y))

;; --------------------------------

;; A holder for an entity. It contains buffer-view specific information about
;; the location of this crate, if it is pinned, default colors and other
;; attributes (unless over ridden by the contained entity).
;; NOTE: Crates can NOT be shared across views. But two different crates can
;; contain the exact same reference to an entity.
(defclass bsm-crate ()
  ((%bsm-crate-location :accessor bsm-crate-location
                        :initarg :bsm-crate-location
                        :type bsm-loc)
   (%bsm-crate-pin-p :accessor bsm-crate-pin-p
                     :initarg :bsm-crate-pin-p
                     :initform nil)
   ;; If not specified, leave the back/fore ground color to whatever is in the
   ;; buffer being rendered into. The colors here are strings or nil if there
   ;; is no color.
   (%bsm-crate-background-color :accessor bsm-crate-background-color
                                :initarg :bsm-crate-background-color
                                :initform nil)
   (%bsm-crate-foreground-color :accessor bsm-crate-foreground-color
                                :initarg :bsm-crate-foreground-color
                                :initform nil)
   ;; The entity being held by this crate.
   (%bsm-crate-entity :accessor bsm-crate-entity
                      :initarg :bsm-crate-entity
                      :initform nil)))

(defun bsm-crate-make (loc entity)
  "Construct and return a crate with the specifed location loc and
entity."
  (bsm-crate
   :bsm-crate-location loc
   :bsm-crate-entity entity))

;; TODO: Check if there is a reinitialize-instance equivalent in eieio and use
;; that so we can do it with supplying initargs too.
(defun bsm-crate-reinitialize (crate)
  "Reinitialize the crate to defaults, EXCEPT the location, and return it."
  (setf (bsm-crate-pin-p crate) nil
        (bsm-crate-background-color crate) nil
        (bsm-crate-foreground-color crate) nil
        (bsm-crate-entity crate) nil)
  crate)

(defun bsm-crate-is-item (crate item &optional eq-func)
  "Return true if the item held by the entity in the crate is
equivalent to the supplied item via the eq-func--otherwise return nil."
  (when crate
    (when-let ((entity (bsm-crate-entity crate)))
      (funcall (or eq-func 'equal)
               (bsm-entity-item entity)
               item))))

(defun bsm-crate-render (crate btile)
  "Render the crate using whatever space is provided it in the btile."
  ;; TODO: Currently we render into exactly one row only. We don't utilize
  ;; the 2d tile space at all if asked to do so. This needs improvement.
  (when-let ((entity (bsm-crate-entity crate)))
    (let* ((id (bsm-entity-id entity))
           (name (if (bsm-id-use-name-p id)
                     (bsm-id-name id)
                   (buffer-name (bsm-entity-item entity))))
           (width (bsm-btile-cols btile))
           ;; Here, get enough of the name of the
           (cpos (seq-position
                  name (seq-find (lambda (c)
                                   (and (not (char-equal c ?\s))
                                        (if (= width 1)
                                            (not (char-equal c ?*))
                                          t)))
                                 name)))
           (short-name (subseq name cpos (min (length name) (+ cpos width))))
           (back-color (bsm-crate-background-color crate))
           (fore-color (bsm-crate-foreground-color crate))
           (pinned-p (bsm-crate-pin-p crate))
           (properties `(,@(when back-color
                             `((:background ,back-color)))
                         ,@(when fore-color
                             `((:foreground ,fore-color)))
                         ,@(when pinned-p
                             `((:underline t)))))
           (prop-short-name
            (propertize short-name 'face properties)))
      (bsm-btile-render-row btile prop-short-name 0 0))))

(defun bsm-crate-test-render ()
  "A test function."
  (with-current-buffer "*bsm-space-display*"
    (save-excursion
      (let* ((btile (bsm-btile-make (current-buffer) (point) 80 1 20))
             (entity (bsm-ent/buf-make (current-buffer)))
             (crate (bsm-crate-make (bsm-loc-make 0 0) entity)))

        (setf (bsm-crate-foreground-color crate) "white"
              (bsm-crate-background-color crate) "blue"
              (bsm-crate-pin-p crate) t)
        (bsm-crate-render crate btile)))))

;; --------------------------------

;; A trivial bounding box implementation for 2d.
(defclass bsm-bbox ()
  ((%bsm-bbox-min-x :accessor bsm-bbox-min-x
                    :initarg :bsm-bbox-min-x
                    :initform nil)
   (%bsm-bbox-max-x :accessor bsm-bbox-max-x
                    :initarg :bsm-bbox-max-x
                    :initform nil)
   (%bsm-bbox-min-y :accessor bsm-bbox-min-y
                    :initarg :bsm-bbox-min-y
                    :initform nil)
   (%bsm-bbox-max-y :accessor bsm-bbox-max-y
                    :initarg :bsm-bbox-max-y
                    :initform nil)))

(defun bsm-bbox-clear (bbox)
  "Reset the bbox to have nil for all slots. Return the bbox."
  (setf (bsm-bbox-min-x bbox) nil
        (bsm-bbox-max-x bbox) nil
        (bsm-bbox-min-y bbox) nil
        (bsm-bbox-max-y boox) nil)
  bbox)

(defun bsm-bbox-expand (bbox loc)
  "Recompute how big the bbox would be if loc is added to it."
  ;; Handle min x
  (if (or (null (bsm-bbox-min-x bbox))
          (< (bsm-loc-x loc) (bsm-bbox-min-x bbox)))
      (setf (bsm-bbox-min-x bbox) (bsm-loc-x loc)))
  ;; Handle max x
  (if (or (null (bsm-bbox-max-x bbox))
          (> (bsm-loc-x loc) (bsm-bbox-max-x bbox)))
      (setf (bsm-bbox-max-x bbox) (bsm-loc-x loc)))
  ;; Handle min y
  (if (or (null (bsm-bbox-min-y bbox))
          (< (bsm-loc-y loc) (bsm-bbox-min-y bbox)))
      (setf (bsm-bbox-min-y bbox) (bsm-loc-y loc)))
  ;; Handle max y
  (if (or (null (bsm-bbox-max-y bbox))
          (> (bsm-loc-y loc) (bsm-bbox-max-y bbox)))
      (setf (bsm-bbox-max-y bbox) (bsm-loc-y loc))))

(defun bsm-bbox-width (bbox)
  (let ((min-x (bsm-bbox-min-x bbox))
        (max-x (bsm-bbox-max-x bbox)))
    (if (and min-x max-x)
        (abs (1+ (- max-x min-x)))
      0)))

(defun bsm-bbox-height (bbox)
  (let ((min-y (bsm-bbox-min-y bbox))
        (max-y (bsm-bbox-max-y bbox)))
    (if (and min-y max-y)
        (abs (1+ (- max-y min-y)))
      0)))

;; --------------------------------

;; A view holds a sparse (usually) 2d sheet of crates and function (eventually)
;; to place those crates into locations in the view. Each crate holds a single
;; entity.  The view is responsible for tracking the crates, their locations,
;; and the location of the cursor the user of the view is using to navigate
;; the view.
(defclass bsm-view ()
  (;; Identifies the view, the name MUST be used and set.
   (%bsm-view-id :reader bsm-view-id
                 :initarg :bsm-view-id
                 :initform (bsm-id))
   ;; a single collection of crates in a sparse geometric indexing
   (%bsm-view-crates :reader bsm-view-crates
                     :initarg :bsm-view-crates
                     ;; KEY: location, VALUE: crate
                     :initform (make-hash-table :test 'bsm-loc-equal/ht))
   ;; The minimal bounding box containing all the points.
   (%bsm-view-bbox :reader bsm-view-bbox
                   :initarg :bsm-view-bbox
                   :initform (bsm-bbox))
   ;; NOTE: The cursor location in this view. nil means we don't have any idea
   ;; where the cursor is and need to pick one (hopefully corresponding to the
   ;; buffer we're doing the request from). If there are more than one crates
   ;; holding the buffer and we have nil, then randomly pick one, otherwise see
   ;; if one of the crates matches the current cursor, and of so use that one.
   (%bsm-view-cursor-location :accessor bsm-view-cursor-location
                              :initarg :bsm-view-cursor-location
                              :initform nil)))

(defun bsm-view-assert-name (view)
  "Check that a bsm-view instance has a defined name."
  (let ((name (bsm-id-name (bsm-view-id view))))
    (unless (and name (not (string= name "")))
      (bsm-error "The bsm-view instance must have a proper name: %s" view))
    view))

(defun bsm-view-make (name)
  "Make an empty bsm-view with the supplied name. Ensure name is not NIL"
  (let ((view (bsm-view :bsm-view-id (bsm-id :bsm-id-use-name-p t
                                             :bsm-id-name name))))
    (bsm-view-assert-name view)))

(defun bsm-view-name (view)
  "Return the name of the bsm-view."
  (bsm-id-name (bsm-view-id view)))

(defun bsm-view-put-crate (view crate)
  "Insert the crate into the view at the location specified in crate and
expand the bounding box.
Return the crate."
  (puthash (bsm-crate-location crate)
           crate
           (bsm-view-crates view))
  (bsm-bbox-expand (bsm-view-bbox view) (bsm-crate-location crate))
  crate)

(defun bsm-view-get-crate (view location &optional ensure)
  "Return the crate at the location or nil if none. If ensure is t
then construct an empty crate at the location, insert it into the view,
and return the new crate."
  (let ((crate (gethash location (bsm-view-crates view) nil)))
    (if crate
        crate
      (when ensure
        (bsm-view-put-crate view location
                            (bsm-crate :bsm-crate-location location))))))

(defun bsm-view-recalculate-bbox (view)
  "Recalculate the smallest bounding box for the crates. Return the view."
  (let ((bbox (bsm-bbox-clear (bsm-view-bbox view))))
    (maphash (lambda (loc crate)
               (bsm-bbox-expand bbox (bsm-crate-location)))
             (bsm-crates view))
    view))

(defun bsm-view-rem-crate (view crate)
  "If the location in the crate, when looked up in the crates for
this view, matches with eq the passed in crate then remove the
crate from the view and recalculate the bounding box. Return t if
the crate was found and removed."
  (let* ((loc (bsm-crate-location crate))
         (fcrate (bsm-view-get-crate view loc)))
    (when (and fcrate (eq fcrate crate))
      (remhash loc (bsm-view-crates view))
      (bsm-view-recalculate-bbox view)
      t)))

(defun bsm-view-find-item (view item &optional eq-func)
  "Return a hash of crates in this view keyed by their location
which contains the item or nil if no crate contains the item. The
structure is shared with the view so edits in the crates will
show up in the view."
  (bsm-util-hash-filter
   (lambda (loc crate)
     (bsm-crate-hash-item crate item))
   (bsm-view-crates view)
   t))


(defun bsm-view-render-location (view btile)
  "Render the location into the display buffer, but only within the
range of [start, end)."
  nil)

;; The space has given the view a btile into which it must render the whole
;; of itself. The View will split this up into smaller btiles into which
;; it will render the crates.
;;
;; | view name
;; +-----------------+
;; |                 |
;; |                 |
;; |                 |
;; |                 |
;; |                 |
;; +-----------------+
;;
;; TODO: For now, we assume a btile for a view is at least 4 character rows
;; high and 4 character columns wide.
(defun bsm-view-render (view btile)
  "Render the view's meta data and visible crates into the supplied btile."
  (unless (and (>= (bsm-btile-rows btile) 4)
               (>= (bsm-btile-cols btile) 4))
    (bsm-error "view to small to render: %s\n" view))

  ;; First, draw the name row
  (let* ((view-name (bsm-view-name view))
         (prop-view-name view-name)
         (view-title (concat "| " prop-view-name)))
    (bsm-btile-render-row btile view-title 0 0))

  ;; Next, draw the box boundaries.
  (let* ((boundary (bsm-bound/relative-make 1 0 0 0)))
    (bsm-btile-render-boundary btile boundary)

    ;; Next, cut the inside of the btile up into cells and fill in the crates.
    ;;
    ;; TODO: Here we're gonna choose single characters for testing purposes. It
    ;; should be some function to the number of crates along with readability
    ;; criteria.
    ;;
    ;; TODO: I don't know what those are yet.
    (let* ((view-inner-btile (bsm-btile))
           (row-end (1+ (- (bsm-btile-rows btile) 3)))
           (col-end (1+ (- (bsm-btile-cols btile) 2)))
           (loc (bsm-loc))
           (rendering-btile (bsm-btile)))
      ;; The sub-btile of the entire renderable area used for
      ;; crates in the view's btile is the view-inner-btile
      (bsm-btile-make-subtile btile 2 1
                              (- (bsm-btile-rows btile) 3)
                              (- (bsm-btile-cols btile) 2)
                              view-inner-btile)

      ;; Now get each crate by location in grid and render it.
      ;;
      ;; TODO: We haven't properly figured out the size of the grid, so we're
      ;; assuming 1x1.
      (cl-loop
       for r from 0 below (bsm-btile-rows view-inner-btile)
       do (cl-loop
           for c from 0 below (bsm-btile-cols view-inner-btile)
           do
           ;; The rendering-btile is the btile into which the crate is
           ;; rendered.
           (bsm-btile-make-subtile view-inner-btile r c 1 1 rendering-btile)

           (setf (bsm-loc-y loc) r
                 (bsm-loc-x loc) c)

           ;; Just render crate at [r,c] into rendering-view.
           ;;
           ;; TODO: Take into consideration view-cursor location and centering
           ;; it.
           (when-let ((crate (bsm-view-get-crate view loc)))
             (bsm-crate-render crate rendering-btile)))))))


(defun bsm-view-test-render ()
  "A test function."
  (with-current-buffer "*bsm-space-display*"
    (save-excursion
      (let* ((btile (bsm-btile-make (current-buffer) (point) 80 16 16))
             (entity (bsm-ent/buf-make (current-buffer)))
             (crate (bsm-crate-make (bsm-loc-make 0 0) entity))
             (view (bsm-view-make "test-view")))

        ;; Fill the crate with something intereting.
        (setf (bsm-crate-foreground-color crate) "white"
              (bsm-crate-background-color crate) "blue"
              (bsm-crate-pin-p crate) t)

        (bsm-view-put-crate view crate)

        (bsm-view-render view btile)))))

;; --------------------------------

;; This is not fully spec'ed out, just a placeholder for now.
(defclass bsm-space ()
  ((%bsm-space-selected-view :accessor bsm-space-selected-view
                             :initarg :bsm-space-selected-view
                             :type bsm-view)
   ;; The buffer into which we render the buffer-space.
   (%bsm-space-display-buffer :reader bsm-space-display-buffer
                              :initarg :bsm-space-display-buffer
                              :type buffer)
   (%bsm-space-views :reader bsm-space-views
                     :initarg :bsm-space-views
                     ;; KEY: bsm-view name, VALUE: bsm-view
                     :initform (bsm-midt-make 'equal 'eq)
                     :type bsm-midt)))

(defun bsm-space-put-view (space view-or-name &optional select)
  "If view-or-name is a string, construct a new view by that name and insert it
into space. If it is a view, then get the name from the view and
insert it into space. If select is true, then also select this view in the
space. Return the view."
  (let ((view
         (cond
          ((stringp view-or-name)
           (bsm-view-make view-or-name))
          ((bsm-view-p view-or-name)
           (bsm-view-assert-name view-or-name)
           view-or-name)
          (t
           (bsm-error "view-or-name must be a string or a bsm-view: %s"
                      view-or-name)))))
    (bsm-midt-put (bsm-view-name view) view
                  (bsm-space-views space))
    (when select
      (setf (bsm-space-selected-view space) view))
    view))

(defun bsm-space-get-view (space view-or-name &optional ensure)
  "Return the view in space identified by view-or-name or nil of not
present. If ensure is t and it is not present construct the new
view (or if itis already a view) and add it to the space. Return
the view."
  ;; keep going, implement ensure.
  (let ((view (bsm-midt-get view-or-name (bsm-space-views space) nil)))
    (if view
        view
      (when ensure
        (bsm-space-put-view space view-or-name)))))

(defun bsm-space-make ()
  "Create and return an empty bsm-space instance with a single empty
view named: default."
  (let* ((new-space
          (bsm-space
           ;; TODO: For now, we get a buffer for the rendering and we always
           ;; get the same one. Each individual space likely will have their
           ;; own rendering buffer, but this is convenient now for debugging.
           ;; This buffer will also show up in the view, which is probably ok
           ;; for now. It probably should ONLY exist during the time we're
           ;; actually seeing it, but since the user can always navigate away
           ;; from it that might not be doable.
           :bsm-space-display-buffer
           (get-buffer-create "*bsm-space-display*")))

         (view-or-name "default")
         (view (bsm-space-put-view new-space view-or-name t)))

    new-space))

(defun bsm-debug-render (display-buffer display-window)
  "This function assumes it is called in a with-current-buffer form."
  (let ((time-format "%a %b %d %H:%M:%S %Z %Y\n"))
    (insert (format-time-string time-format (current-time)))
    (insert (format "The window body (width, height) is: (%s, %s)\n"
                    (window-body-width) (window-body-height)))))

;; This function needs a lot of work to behave right.
(defun bsm-space-render (space)
  (when space
    (let* ((display-buffer (bsm-space-display-buffer space))
           (display-window (selected-window))
           (stride (window-body-width))
           ;; Minus 1 because the last column is reserved for newlines.
           (body-width (1- stride))
           (body-height (window-body-height))
           (loc (bsm-loc-make 0 0))
           (sel-view (bsm-space-selected-view space))
           (display-btile (bsm-btile-make display-buffer 1 stride
                                          body-height body-width))
           (display-boundary (bsm-bound/relative-make 0 0 0 0)))
      (with-current-buffer display-buffer
        (save-excursion
          ;; TODO: Find a better way to handle these odd things for other
          ;; emacs modes
          (display-line-numbers-mode -1)

          ;; MUST DO FIRST: Fill it with empty space.
          (erase-buffer)
          ;; NOTE: Last column is dedicated to newlines in the display buffer.
          ;; Otherwise the fringe will show up in tty mode.
          (let ((empty-row (concat (make-string body-width ?\s) "\n")))
            (cl-loop for row from 0 below body-height
                     do (insert empty-row)))

          ;; draw boundary for now cause it is a good sanity check.
          (bsm-btile-render-boundary display-btile display-boundary)

          ;; Draw all of the views in the right places.
          ;;
          ;; TODO: Fixme for having the space assign a location to all views,
          ;; and for rendering all views.
          (let* ((default-view (bsm-space-get-view space "default"))
                 (bbox (bsm-view-bbox default-view))
                 (bbox-width (bsm-bbox-width bbox))
                 (bbox-height (bsm-bbox-height bbox)))
            (bsm-view-render default-view
                             (bsm-btile-make
                              (bsm-space-display-buffer space)
                              ;; NOTE: 1+ to account for newline column in
                              ;; display-buffer.
                              (+ (* (1+ body-width) 5) 10) (1+ body-width)
                              ;; These are in terms of the bbox of the view, we
                              ;; need extra decoration around it to make a
                              ;; minimum sized view display.
                              (+ 3 bbox-height)
                              (+ 2 bbox-width)))))))))

;; --------------------------------

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bsm-seed-buffer-space ()
  "Seeds the *bsm-buffer-space* and attempts to make it square."
  (let* ((new-space (bsm-space-make))
         (selected-view (bsm-space-selected-view new-space))
         (buffers (buffer-list))
         (rect-edge-length (ceiling (sqrt (length buffers)))))
    (princ (format "Inserting %s buffers in a square %s units high and wide."
                   (length buffers) rect-edge-length))
    (cl-loop for i below (length buffers)
             for buf in buffers do
             ;; TODO: Technically, space should have a mapping function to
             ;; select which view something goes into and then the view should
             ;; have a mapping function that can map the entity to the right
             ;; location in the buffer and also (maybe a third?) map the right
             ;; settings into the crate (like colors, etc). That concept is not
             ;; represented in this code yet.
             (let ((x (mod i rect-edge-length))
                   (y (/ i rect-edge-length)))

               ;; Insert an entity into the right location.
               (let* ((entity (bsm-ent/buf-make buf))
                      (loc (bsm-loc-make x y))
                      (name (buffer-name buf))
                      (crate (bsm-crate-make loc entity)))

                 ;; TODO: Test pattern for now to test fore/back ground.
                 (setf (bsm-crate-background-color crate)
                       (if (zerop (mod i 2))
                           "blue"
                         "magenta")

                       (bsm-crate-foreground-color crate) "white")

                 (bsm-view-put-crate selected-view crate))))

    (setf *bsm-buffer-space* new-space)))

;; KEEP GOING

;; TODO - you'll see a lot of pos below which is the index of the current
;; workspace this should probably be a global

;; squishy methods.



;; We will use the first element for now, later we need to pick one
(defun bsm-buffer-position (buffer &optional pos)
  "Given a buffer (name OR object) in vector position pos return [x y]"
  (setf pos (or pos 0))
  (let ((current-workspace (elt *bsm-buffer-space-spaces* pos))
        (buffer (get-buffer buffer)))   ; We need to use str of buffer
    (cl-loop for key in (hash-table-keys current-workspace)
             do (when (equal buffer (bsm-buffer
                                     (gethash key current-workspace)))
                  (return key)))))


;; TODO unify these into one that dispatches on x or y
(defun bsm-get-aligned-x (x &optional pos)
  "Gets a sorted list of all positions aligned with x. Pos is
current workspace."
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

(defun bsm-render ()
  (interactive)
  (bsm-seed-buffer-space)
  (bsm-space-render *bsm-buffer-space*))
