(defun acl-toplevel ()
  (do ()
      (nil)
    (format t "~%===>")
    (format t (eval (read)))))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro ntimes (n &rest body)
  (let ((g (gensym))
        (h (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
           ((>= ,g ,h))
         ,@body))))


(defun quicksort (vec left right)
  (let ((i left)
        (j right)
        (p (svref vec (round (+ left right) 2))))
    (while (<= i j)
           (while (< (svref vec i) p) (incf i))
           (while (> (svref vec j) p) (decf j))
           (when (<= i j)
             (rotatef (svref vec i) (svref vec j))
             (incf i)
             (decf j)))
    (if (>= (- j left) 1) (quicksort vec left j))
    (if (>= (- right i) 1) (quicksort vec i right)))
  vec)

(defmacro for (var start stop &body body)
  (let ((gstop gensym))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro in (item &rest choises)
  (let ((insym (gensym)))
    `(let ((,imsym ,item))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c)) choises)))))

(defmacro random-choise (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
         (mapcar #'(lambda (expr)
                     `(,(incf key) ,expr))
                 exprs))))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))


;;; task 1
(defun ex-10-1 ()
  (multiple-value-bind (x y z) (values 'a 'b '(c d))
    (equal `((,z ,x z) (x ,y ,@z) ((,@z ,x) z))
           `(((c d) a z) (x b c d) ((c d a) z)))))

;;; task 2
(defmacro ansi-cl-if (pred then else)
  `(cond (,pred ,then)
         (t ,else)))

;;; task 3
(defmacro nth-expr (n &rest exprs)
  (if (and (integerp n)
           (plusp n)
           (<= n (length exprs)))
      (nth (1- n) exprs)
      `(case ,n
         ,@(let ((index -1))
             (mapcar #'(lambda (ex)
                         `(,(incf index) ,ex))
                     exprs)))))

;;; task 4
;; (defmacro ntimes (n &rest body)
;;   (let ((g (gensym))
;;         (h (gensym)))
;;     `(let ((,h ,n))
;;        (do ((,g 0 (+ ,g 1)))
;;            ((>= ,g ,h))
;;          ,@body))))

(defmacro ansi-cl-ntimes (n &rest body)
  (with-gensyms (i h fname)
    `(let ((,h ,n))
       (labels ((,fname (,i)
                  (when (< ,i ,h)
                    ,@body
                    (,fname (1+ ,i)))))
         (,fname 0)))))


;;; task 5
(defmacro n-of (n &body body)
  (with-gensyms (index fname acc)
    `(let ()
       (labels ((,fname (,index ,acc)
                  (if (>= ,index n)
                      (nreverse ,acc)
                      (,fname (1+ ,index) (cons ,@body ,acc)))))
         (,fname 0 nil)))))

;;; task 6
(defmacro just-name (args &body body)
  `((lambda ,args ,@body) ,@args))

;;; task 7
(defmacro ansi-cl-push (item lst)
  `(setf ,lst (cons ,item ,lst)))

(defun push-test ()
  (let ((index 0)
        (a (make-array 3)))
    (setf (aref a 0) '(11)
          (aref a 1) '(22)
          (aref a 2) '(33))
    (push 44 (aref a (incf index)))
    (format t "~A~%" a) ; #((11) (44 22) (33))
    (setf '(22) (aref a 1) index 0)
    ;; Execution of a form compiled with errors.
    ;; Form:
    ;;   (22)
    ;; Compile-time error:
    ;;   illegal function call
    (ansi-cl-push 44 (aref a (incf index)))
    (format t "~A~%" a)))

;;; task 8
(defmacro ansi-cl-double (n)
  (with-gensyms (i)
    `(let ((,i (* ,n 2)))
       (setf ,n ,i))))

;; or
(define-modify-macro ya-double ()
  (lambda (n) (* n 2)))
