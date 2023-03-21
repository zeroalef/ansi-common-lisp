(defun ansi-cl-funcall (fn &rest args)
  (apply fn args))

(defun ansi-cl-adjoin (item lst &rest args)
  (if (apply #'member item lst args)
      lst
      (cons item lst)))

(defun ansi-cl-combiner (x)
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))

(defun ansi-cl-combine (&rest args)
  (apply (ansi-cl-combiner (car args)) args))


;;; task 1
(defun ansi-cl-tokens (str &key (test #'ansi-cl-constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (when p1
      (let ((p2 (position-if #'(lambda (c)
                                 (not (funcall test c)))
                             str :start p1)))
        (cons (subseq str p1 p2)
              (when p2
                (ansi-cl-tokens str test p2)))))))

(defun ansi-cl-constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\space))))

;;; task 2
(defun ansi-cl-bin-search
    (item vec
     &key
       (key #'identity)
       (test #'eql)
       (start 0)
       (end (length vec)))
  (and (not (zerop end))
       (finder item vec start (- end 1) key test)))

(defun finder (item vec start end key test)
  (let ((range (- end start)))
    (if (zerop range)
        (when (funcall test item (funcall key (aref vec start)))
            (aref vec start))
        (let ((mid (+ start (round (/ range 2)))))
          (let ((tmp-item (funcall key (aref vec mid))))
            (if (< item tmp-item)
                (finder item vec start (- mid 1) key test)
                (if (> item tmp-item)
                    (finder item vec (+ mid 1) end key test)
                    (aref vec mid))))))))

;;; task 3
(defun arg-len (&rest args)
  (length args))

;;; task 4
(defun ansi-cl-filter (fn lst)
  (cond ((null lst) nil)
        ((funcall fn (car lst))
         (cons (car lst)
               (ansi-cl-filter fn (cdr lst))))
        (t ansi-cl-filter fn (cdr lst))))

(defun ansi-cl-most (fn lst)
  (let ((vals (ansi-cl-filter fn lst)))
    (values (car vals) (cadr vals))))

;;; task 5
(defun ansi-cl-remove-if (fn lst)
  (ansi-cl-filter #'(lambda (item)
                      (not (funcall fn item)))
                  lst))

;;; task 6
(let ((acc))
  (defun ansi-cl-filter-max (n)
    (if (or (null acc) (> n acc))
        (setf acc n)
        acc))
  (defun ansi-cl-filter-max-reset ()
    (setf acc nil)))

;;; task 7
(let (acc)
  (defun ansi-cl-greater (n)
    (cond ((null acc) (and (setf acc n) nil))
          ((> n acc) (setf acc n))
          (t acc))))

;;; task 8
(defun expensive (n) (* n n))

(let ((acc (make-array 101 :initial-element nil)))
  (defun frugel (n)
    (when (< -1 n 101)
      (or (aref acc n)
          (setf (aref acc n) (expensive n))))))

;;; task 9
(defun apply-oct (fn &rest args)
  (let ((*print-base* 8))
    (apply fn args)))
