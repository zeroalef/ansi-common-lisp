(defun pos+recursive (lst &optional (index 0))
  (if (null lst)
      nil
      (cons (+ index (car lst))
            (pos+recursive (cdr lst) (+ index 1)))))

;; bad realisation
(defun pos+iterative-alternative (lst &optional (index -1))
  (dolist (item lst)
    (incf (nth (incf index) lst) index))
  lst)

;; good realisation
(defun pos+iterative (lst)
  (do ((ls lst (cdr lst))
       (i 0 (+ i 1))
       (acc nil (cons (+ i (car ls)) acc)))
      ((not ls) (reverse acc))))


(defun pos+mapcar (lst)
  (mapcar #'(lambda (x)
              (let ((index -1))
                (incf index)
                (+ x index)))
          lst))



(defun test ()
  (let ((data '(7 5 1 4 10))
        (result '(7 6 3 7 14)))
    (and
     (equal
      (pos+recursive data)
      (pos+iterative data))
     (equal
      (pos+mapcar data)
      result))))
