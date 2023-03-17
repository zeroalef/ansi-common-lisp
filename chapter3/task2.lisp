(defun ansi-cl-union (left right &optional (acc nil))
  (cond (left (ansi-cl-union (cdr left) right (adjoin (car left) acc)))
        (right (ansi-cl-union right nil acc))
        (t (reverse acc))))


(defun test ()
  (let ((left '(a b c))
        (right '(b a d))
        (result '(a b c d)))
    (equal (ansi-cl-union left right)
           result)))
