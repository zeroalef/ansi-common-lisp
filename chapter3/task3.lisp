(defun ansi-cl-occurences (lst)
  (when (listp lst) (occurences lst nil)))

(defun occurences (lst acc)
  (if (null lst)
      (sort acc #'> :key #'cdr)
      (let ((pair (assoc (car lst) acc)))
        (if pair
            (and (incf (cdr pair))
                 (occurences (cdr lst) acc))
            (occurences (cdr lst) (push (cons (car lst) 1) acc))))))

(defun test ()
  (let ((in-data '(a b a d a d a))
        (out-data '((a . 4) (d . 2) (b . 1))))
    (equal (ansi-cl-occurences in-data)
           out-data)))
