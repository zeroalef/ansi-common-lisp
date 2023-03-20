(load "../utils.lisp")


(defun square-matrix (n)
  ;; Generate 2D array with n-rank
  (make-array `(,n ,n) :initial-element 0))


(defun matrix-pretty-print (mx)
  ;; Display array. Assuming the array is square matrix
  ;; and the every value if <= 1000
  (let ((len (car (array-dimensions mx))))
    (do ((row 0 (1+ row)))
        ((>= row len))
      (do ((column 0 (1+ column)))
          ((>= column len))
        (format t "~3a" (aref mx row column)))
      (format t "~%")))
  (format t "~%"))

(defun matrix-fill-random (mx n)
  ;; Setting up it into `(random n)` values.
  ;; This function is destructive.
  (let ((len (car (array-dimensions mx))))
    (do ((row 0 (1+ row)))
        ((>= row len))
      (do ((column 0 (1+ column)))
          ((>= column len))
        (setf (aref mx row column) (random n))))))


(defun matrix-rotate (mx direction)
  ;; Rotate matrix considering direction.
  ;; Direction may be 'left or 'right.
  ;; On the another case the it's just copy matrix
  (let* ((dims (array-dimensions mx))
         (len (car dims))
         (new-array (make-array dims)))
    (do ((row 0 (1+ row)))
        ((>= row len))
      (do ((column 0 (1+ column)))
          ((>= column len))
        (setf (aref new-array row column)
              (cond ((eq direction 'left)
                     (aref mx column (- len row 1)))
                    ((eq direction 'right)
                     (aref mx (- len column 1) row))
                    (t (aref mx row column))))))
    new-array))

(defun main ()
  (let ((matrix (square-matrix 5)))
    (matrix-fill-random matrix 15)
    (matrix-pretty-print matrix)
    (matrix-pretty-print (matrix-rotate matrix 'left))))

(main)
