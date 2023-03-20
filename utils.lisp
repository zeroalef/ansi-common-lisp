(defun range (end &optional
                    (optional-end 0 optend-supplied-p)
                    (increase 1))
  "Frontend function to generate a range list"
  (declare (type integer end))
  (declare (type integer optional-end))
  (declare (type integer increase))
  (if optend-supplied-p
      (cond ((> end optional-end)
             (if (or (plusp increase)
                     (zerop increase))
                 (format nil "bad value increase: ~a" increase)
                 (range-core end optional-end increase #'<= nil)))
            ((< end optional-end)
             (if (or (zerop increase)
                     (minusp increase))
                 (format nil "bad value increase: ~a" increase)
                 (range-core end optional-end increase #'>= nil)))
            (t nil))
      (range-core 0 end increase #'>= nil)))


(defun range-core (start end increase predicate acc)
  (if (funcall predicate start end)
      (reverse acc)
      (range-core (+ start increase) end increase predicate (cons start acc))))
