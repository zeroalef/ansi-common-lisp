;; return t if at least one value of data is list
(defun list-any (data)
  (let ((return-value nil))
    (dolist (item data)
      (setf return-value (or return-value (listp item))))
    return-value))

(defun test ()
  (and
   (list-any '(1 2 3 (4 5)))
   (null (list-any '(1 2 3)))))

(test)
