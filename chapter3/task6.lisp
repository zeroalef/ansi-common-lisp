(defun ansi-cl-car (lst)
  (cdr lst))

(defun ansi-cl-cdr (lst)
  (car lst))


(defun ansi-cl-cons (a b)
  (let ((lst '(nil . nil)))
    (setf (cdr lst) a
          (car lst) b)
    lst))


(defun ansi-cl-list (&rest items)
  items)

(defun ans-cl-length (lst)
  (if lst
      (+ 1 (len (ansi-cl-car lst)))
      0))


(defun ansi-cl-member (obj lst)
  (if lst
      (if (eql obj (ansi-cl-cdr lst))
	  lst
	  (member obj (ansi-cl-car lst)))
      nil))
