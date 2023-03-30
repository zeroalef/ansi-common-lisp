(defun make-queue ()
  (cons nil nil))

(defun enqueue (item q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list item)))
      (setf (cdr (cdr q)) (list item)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))


(defun our-mapcan (fn &rest lsts)
  (apply #'nconc (apply #'mapcar fn lsts)))


(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun bst-insert! (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (progn (bsti obj bst <)
             bst)))

(defun bsti (obj bst <)
  (let ((elt (node-elt bst)))
    (if (eql obj elt)
        bst
        (if (funcall < obj elt)
            (let ((l (node–l bst)))
              (if l
                  (bsti obj l <)
                  (setf (node–l bst)
                        (make-node :elt obj))))
            (let ((r (node-r bst)))
              (if r
                  (bsti obj r <)
                  (setf (node-r bst)
                        (make-node :elt obj))))))))

(defun bst-delete (obj bst <)
  (if (null bst)
      nil
      (if (eql obj (node-elt bst))
          (del-root bst)
          (progn
            (if (funcall < obj (node-elt bst))
                (setf (node-l bst) (bst-delete obj (node-l bst) <))
                (setf (node-r bst) (bst-delete obj (node-r bst) <)))
            bst))))

(defun del-root (bst)
  (let ((l (node-l bst)) (r (node-r bst)))
    (cond ((null l) r)
          ((null r) l)
          (t
           (if (zerop (random 2))
               (cutnext r bst nil)
               (cutprev l bst nil))))))

(defun cutnext (bst root prev)
  (if (node-l bst)
      (cutnext (node-l bst) root bst)
      (if prev
          (progn
            (setf (node-elt root) (node-elt bst)
                  (node-l prev) (node-r bst))
            root)
          (progn
            (setf (node-l bst)
                  (node-l root))
            bst))))

(defun cutprev (bst root prev)
  (if (node-r bst)
      (cutprev (node-r bst) root bst)
      (if prev
          (progn
            (setf (node-elt root) (node-elt bst)
                  (node-r prev) (node-l bst))
            root)
          (progn
            (setf (node-r bst) (node-r root))
            bst))))

(defun replace-node (old new)
  (setf (node-elt old) (node-elt new)
        (node-l old) (node-l new)
        (node-r old) (node-r new)))

(defun cutmin (bst par dir)
  (if (node-l bst)
      (cutmin (node-l bst) bst :l)
      (progn
        (set-par par dir (node-r bst))
        (node-elt bst))))

(defun cutmax (bst par dir)
  (if (node-r bst)
      (cutmax (node-r bst) bst :r)
      (progn
        (set-par par dir (node-l bst))
        (node-elt bst))))

(defun set-par (par dir val)
  (case dir
    (:l (setf (node-l par) val))
    (:r (setf (node-r par) val))))

;;; double linked list
(defstruct (dl (:print-function print-dl))
  prev data next)

(defun print-dl (dl stream depth)
  (declare (ignore depth))
  (format stream "#<DL ~A>" (dl->list dl)))

(defun dl->list (dl)
  (if (dl-p dl)
      (cons (dl-data dl) (dl->list (dl-next dl)))
      dl))

(defun dl-insert (x dl)
  (let ((item (make-dl :data x :next dl)))
    (when (dl-p dl)
      (if (dl-prev dl)
          (setf (dl-next (dl-prev dl)) item
                (dl-prev item) (dl-prev dl))
          (setf (dl-prev dl) item)))
    item))

(defun dl-list (&rest args)
  (reduce #'dl-insert args
          :from-end t
          :initial-value nil))

(defun dl-remove (lst)
  (if (dl-prev lst)
      (setf (dl-next (dl-prev lst)) (dl-next lst)))
  (if (dl-next lst)
      (setf (dl-prev (dl-next lst)) (dl-prev lst)))
  (dl-next lst))


;;; task 1
;; (let ((item-1 '(a))
;;       (item-2 'a))
;;   (list item-1 item-1 item-1)
;;   (list (list item-2) (list item-2) (list item-2))
;;   (list (list item-2) item1 item-1))

;;; task 3
(defun copy-queue (queue)
  (let ((q (make-queue)))
    (setf (car q) (copy-list (car queue))
          (cdr q) (last (car q)))
    q))

;;; task 4
(defun push-queue (item queue)
  (setf (car queue) (cons item (car queue))))

;;; task 5
(defun move-front (item queue)
  (let ((tmp (delete item (car queue))))
    (setf (car queue) (if (member item (car queue))
                          (cons item (car queue))
                          (car queue))
          (cdr queue) (last (car queue))))
  (car queue))

;;; task 6
(defun circle-tail-find (item xs)
  "find item on cyclic list"
  (labels ((core (ys)
             (cond ((eql item (car ys)) t)
                   ((eq ys xs) nil)
                   (t (core (cdr ys))))))
    (if (eql item (car xs))
        t
        (core (cdr xs)))))

;;; task 7
(defun cyclic-tail-list-p (xs)
  "check if a list is cycles back to the first cons"
  (labels ((core (ys)
             (cond ((null ys) ys)
                   ((eq ys xs) xs)
                   (t (core (cdr ys))))))
    (core (cdr xs))))

;;; task 8
(defun cyclic-head-list-p (xs)
  "check if a list is cycles front to the first cons"
  (eq (car xs) xs))
