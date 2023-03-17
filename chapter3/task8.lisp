(defun ansi-cl-showdots (lst)
  (if lst
      (if (atom lst)
          lst
          (format nil "~A . ~A"
                  (ansi-cl-showdots (car lst))
                  (ansi-cl-showdots (cdr lst))))
      nil))
