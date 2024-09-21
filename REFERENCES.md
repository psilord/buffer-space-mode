
# Emacs Overlays
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html
  - https://stackoverflow.com/questions/22361912/how-to-overlay-with-text

## Example 1
``` common-lisp
;; example in IELM repl

(setq *bsm-yyy* (get-buffer-create "foo.txt"))
(with-current-buffer *bsm-yyy*
  (insert "This is a piece of text.\n")
  (insert "It has a couple of lines.\n")
  (insert "Multiple lines of text are a good thing.\n"))

(setq *bsm-xxx* (make-overlay 1 16 *bsm-yyy*))

;; Then open the buffer in another frame/window do you can see what
;; is going on in it.

(overlay-put *bsm-xxx* 'face
             '(:background "blue") (:foreground "yellow"))

;; You should see the color change.

;; Then, let's change what's in the overlay. We will keep the size
;; of the change exactly how big the overlay is.

(overlay-put *bsm-xxx* 'display "|----FOOBAR---|")

;; You should see the text change.

;; This will undo it:
(overlay-put *bsm-xxx* 'display nil)

;; Evaluating the two 'display lines alternately will swap them.

;; Then we delete the overlay

(delete-overlay *bsm-xxx*)

;; NOTE: There is some disagreement if the overlay is actually
;; deleted and reclaimable by the garbage collector or if it is
;; hiding out somewhere in some list making all overlays slower
;; and slower. So, buffer-space should at best make one or two overlays
;; that it moves around, displays, and changes the contents of as needed.
```
