(defun ansi-cl-assoc->hash (alist &key (test #'eql))
  (let ((ht (make-hash-table :test test)))
    (dolist (pair alist)
      (setf (gethash (car pair) ht) (cdr pair)))
    ht))


(defun ansi-cl-hash->alist (ht)
  (let ((acc))
    (maphash #'(lambda (key value)
                 (push (cons key value) acc)))
    acc))
