#|
(defun compare (value candidate &key (test #'=) (less #'<) (larger #'>))
  (cond ((funcall test value candidate) 0)
        ((funcall less value candidate) -1)
        ((cunfall larger value candidate) 1)
        ( t nil)))

(defun ansi-cl-bsearch (item vec &key (comp #'compare))
  (when (vectorp vec)
    (let ((len (length vec)))
      (cond ((zerop len)
             nil)
            ((= len 1)
             (let ((v-item (svref vec 0)))
               (when (zerop (funcall comp item v-item))
                 item)))
            (t
             (bsearch-core item vec 0 (- len 1)))))))

(defun bsearch-core (item vec start end &key (comp #'compare))
  ())
|#

(defun ansi-cl-bin-search (item vec)
  (let ((len (length vec)))
    (and (not (zerop len))
         (finder item vec 0 (- len 1)))))

(defun finder (item vec start end)
  (let ((range (- end start)))
    (if (zerop range)
        (if (eql item (aref vec start))
            item
            nil)
        (let ((mid (+ start (round (/ range 2)))))
          (let ((tmp-item (aref vec mid)))
            (if (< item tmp-item)
                (finder item vec start (- mid 1))
                (if (> item tmp-item)
                    (finder item vec (+ mid 1) end)
                    item)))))))


(defun ansi-cl-mirror? (s)
  (let ((len (length s)))
    (do ((forward 0 (+ 1 forward))
         (back (- len 1) (- back 1)))
        ((or (> forward back)
             (not (eql (elt s forward)
                       (elt s back))))
         (> forward back)))))


(defun ansi-cl-tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                    (not (funcall test c)))
                                str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (ansi-cl-tokens str test p2))))
        nil)))

(defun ansi-cl-constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\space))))


(defun ansi-cl-parse-date (str)
  (let ((toks (ansi-cl-tokens str #'ansi-cl-constituent 0)))
    (list (parse-integer (first toks))
          (ansi-cl-parse-month (second toks))
          (parse-integer (third toks)))))
(defconstant +month-names+
  #("jan" "feb" "mar" "apr" "may" "jun"
    "jul" "aug" "sep" "oct" "nov" "dec"))

(defun ansi-cl-parse-month (str)
  (let ((p (position str +month-names+ :test #'string-equal)))
    (when p (+ p 1))))


(defstruct (node
            (:print-function
             (lambda (n s d)
               (format s "#<~A>" (node-elt n)))))
  elt (left nil) (right nil))


(defun ansi-cl-bst-insert (item bst <)
  (if (null bst)
      (make-node :elt item)
      (let ((elt (node-elt bst)))
        (if (eql elt item)
            bst
            (if (funcall < item elt)
                (make-node
                 :elt elt
                 :left (ansi-cl-bst-insert item (node-left bst) <)
                 :right (node-right bst))
                (make-node
                 :elt elt
                 :right (ansi-cl-bst-insert item (node-right bst) <)
                 :left (node-left bst)))))))


(defun ansi-cl-bst-find (item bst <)
  (if (null bst)
      bst
      (let ((elt (node-elt bst)))
        (cond ((eql elt item) bst)
              ((funcall < elt item) (ansi-cl-bst-find item (node-left bst) <))
              (t (ansi-cl-bst-find item (node-right bst) <))))))

(defun ansi-cl-bst-min (bst)
  (and bst
       (or (ansi-cl-bst-min (node-left bst)) bst)))

(defun ansi-cl-bst-max (bst)
  (and bst
       (or (ansi-cl-bst-max (node-right bst)) bst)))


(defun ansi-cl-bst-remove (item bst <)
  (if (null bst)
      bst
      (let ((tmp-item (node-elt bst)))
        (if (eql item tmp-item)
            (ansi-cl-perlocate bst)
            (if (funcall < item tmp-item)
                (make-node
                 :elt tmp-item
                 :left (ansi-cl-bst-remove item (node-left bst) <)
                 :right (node-right bst))
                (make-node
                 :elt tmp-item
                 :right (ansi-cl-bst-remove item (node-right bst) <)
                 :left (node-left bst)))))))

(defun ansi-cl-perlocate (bst)
  (let ((left (node-left bst))
        (right (node-right bst)))
    (cond ((null left) right)
          ((null right) left)
          (t (if (zerop (random 2))
                 (make-node :elt (node-elt (ansi-cl-bst-remove-max left))
                            :right right
                            :left (ansi-cl-bst-remove-max left))
                 (make-node :elt (node-elt (ansi-cl-bst-min right))
                            :right (ansi-cl-bst-remove-min right)
                            :left left))))))

(defun ansi-cl-bst-remove-min (bst)
  (if (null (node-left bst))
      (node-right bst)
      (make-node :elt (node-elt bst)
                 :left (ansi-cl-bst-remove-min (node-left bst))
                 :right (node-right bst))))

(defun ansi-cl-bst-remove-max (bst)
  (if (null (node-right bst))
      (node-left bst)
      (make-node :elt (node-elt bst)
                 :left (node-left bst)
                 :right (ansi-cl-bst-remove-max (node-right bst)))))

(defun ansi-cl-bst-traverse (fn bst)
  (when bst
    (ansi-cl-bst-traverse fn (node-left bst))
    (funcall fn (node-elt bst))
    (ansi-cl-bst-traverse fn (node-right bst))))


(defun ansi-cl-bst->list (bst &optional (direction nil dir-supplied-p))
  (labels ((local-fun (tree acc)
             (if tree
                 (if dir-supplied-p
                     (local-fun (node-right tree)
                                (cons (node-elt tree)
                                      (local-fun (node-left tree) acc)))
                     (local-fun (node-left tree)
                                (cons (node-elt tree)
                                      (local-fun (node-right tree) acc))))
                 acc)))
    (local-fun bst nil)))
