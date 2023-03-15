
;; always return nil
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))


;; return position of `x` on `y` list
(defun mistery (x y)
  (cond ((null y) y)
        ((eql (car y) x) 0)
        (t (let ((z (mistery x (cdr y))))
             (and z (+ z 1))))))


(defun test ()
  (and
   (null (or
          (enigma '(2))
          (enigma nil)))
   (and
    (eql 2 (mistery 11 '(1 2 11 3 4)))
    (eql 0 (mistery 11 '(11 1 2 3))))))

(test)
