(defun copy-file (from to)
  (with-open-file (in from :direction :input
                           :element-type 'unsigned-byte)
    (with-open-file (out to :direction :output
                            :element-type 'unsigned-byte)
      (do ((i (read-byte in nil -1)
              (read-byte in nil -1)))
          ((minusp i))
        (declare (fixnum i))
        (write-byte i out)))))

(set-macro-character
 #\'
 #'(lambda (stream char)
     (list (quote quote) (read stream t nil t))))

(set-dispatch-macro-character
 #\# #\?
 #'(lambda (stream char1 char2)
     (list 'quote
            (let ((lst nil))
              (dotimes (i (+ (read stream t nil t) 1))
                (push i lst))
              (nreverse lst)))))

(set-macro-character #\} (get-macro-character #\)))

(set-dispatch-macro-character
 #\# #\{
 #'(lambda (stream char1 char2)
     (let ((accum nil)
           (pair (read-delimited-list #\} stream t)))
       (do ((i (car pair) (+ i 1)))
           ((> i (cadr pair))
            (list 'quote (nreverse accum)))
         (push i accum)))))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (loop :with wins = (car lst)
            :with max = (funcall fn wins)
            :for obj :in (cdr lst)
            :for score = (funcall fn obj)
            :when (> score max)
              :do (setf wins obj
                        max score)
            :finally (return (values wins max)))))

(defun num-year (n)
  (if (< n 0)
      (loop :for y :downfrom (- yzero 1)
            :until (<= d n)
            :sum (- (year-days y)) :into d
            :finally (return (values (+ y 1) (- n d))))
      (loop :with prev = 0
            :for y :from yzero
            :until (> d n)
            :do (setf prev d)
            :sum (year-days y) into d
            :finally (return (values (- y 1)
                                     (- n prev))))))
