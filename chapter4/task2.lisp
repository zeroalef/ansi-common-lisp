(defun ansi-cl-copy-list (lst)
  (reduce #'cons lst
          :from-end t
          :initial-value nil))

(defun ansi-cl-reverse (lst)
  (reduce #'(lambda (a b) (cons b a)) lst
          :initial-value nil))


(defun test ()
  (let ((lst '(1 2 3 4)))
    (and (equal lst (ansi-cl-copy-list lst))
         (equal (reverse lst) (ansi-cl-reverse lst)))))

(test)
