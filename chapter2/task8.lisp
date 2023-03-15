(defun display-dots-iterative (n)
  (when (and (integerp n) (> n 0))
    (do ((i n (- i 1)))
        ((zerop i) 'done)
      (format t "."))))


(defun display-dots-recursive-core (n)
  (when (plusp n)
    (progn
      (display-dots-recursive-core (- n 1))
      (format t "."))))


(defun display-dots-recursive (n)
  (when (and (integerp n) (plusp n))
    (display-dots-recursive-core n)))


(defun count-atom-iterative (atom-passed data)
  (let ((ret-value 0))
    (dolist (item data)
      (if (listp item)
          (incf ret-value (count-atom-iterative atom-passed item))
          (incf ret-value (if (eql item atom-passed) 1 0))))
    ret-value))


(defun count-atom-recursive-core (a-passed lst acc)
  (cond
    ((null lst) acc)
    ((listp (car lst))
     (count-atom-recursive-core a-passed (cdr lst)
      (+ acc (count-atom-recursive-core a-passed (car lst) 0))))
    ((eql a-passed (car lst))
     (count-atom-recursive-core a-passed (cdr lst) (+ acc 1)))
    (t
     (count-atom-recursive-core a-passed (cdr lst) acc))))

(defun count-atom-recursive (a-passed data)
  (when (listp data)
    (count-atom-recursive-core a-passed data 0)))
