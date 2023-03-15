#|
;; (remove nil lst) does not change lst
(defun summit (lst)
  (remove nil lst)
  (apply #’+ lst))
|#

(defun summit (lst)
  (apply #'+ (remove nil lst)))

#|
;; it's infinite srcursion on (null nil) because it's equal T
(defun summit (lst)                     ;
(let ((x (car lst)))                    ;
(if (null x)                            ;
(summit (cdr lst))                      ;
(+ x (summit (cdr lst))))))             ;
|#

(defun summit-iterative (lst)
  (let ((ret-value 0))
    (dolist (item lst)
      (when (numberp item)
        (incf ret-value item)))
    ret-value))
