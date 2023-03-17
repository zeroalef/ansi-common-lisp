(defun ansi-cl-coply-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (ansi-cl-copy-list (cdr lst)))))



(defun compress (lst)
  (if (consp lst)
      (compr (car lst) 1 (cdr lst))))


(defun compr (item n lst)
  (if (null lst)
      (list (n-items item n))
      (let ((next (car lst)))
        (if (eql next item)
            (compr item (+ n 1) (cdr lst))
            (cons (n-items item n)
                  (compr next 1 (cdr lst)))))))


(defun n-items (item n)
  (if (> n 1)
      (cons n elt) ;; changed considering exersise 3.7
      item))



(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((item (car lst))
            (rest (uncompress (cdr lst))))
        (if (consp item)
            (append (apply #'list-of item) rest)
            (cons item rest)))))


(defun list-of (n item)
  (if (zerop n)
      nil
      (cons item (list-of (- n 1) item))))


(defun ansi-cl-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (ansi-cl-copy-tree (car tr))
            (ansi-cl-copy-tree (cdr tr)))))


(defun ansi-cl-subst (new old tree)
  (cond
    ((eql tree old) new)
    ((atom tree) tree)
    (t (cons (ansi-cl-subst new old (car tree))
             (ansi-cl-subst new old (cdr tree))))))


(defun ansi-cl-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst))
           lst
           (ansi-cl-member-if fn (cdr lst)))))



(defun ansi-cl-assoc (key alist)
  (and (consp alist)
       (let ((pair (car alist)))
         (if (eql key (car pair))
             pair
             (ansi-cl-assoc key (cdr alist))))))


(defparameter *ansi-cl-net* '((a b c) (b c) (c d)))

(defun ansi-cl-shortest-path (start end net)
  (ansi-cl-bfs end (list (list start)) net))


(defun ansi-cl-bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (ansi-cl-bfs end
                   (append (cdr queue) (ansi-cl-new-paths path node net))
                   net))))))

(defun ansi-cl-new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))
