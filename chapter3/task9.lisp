(defparameter *net* '((a b c) (b a c) (c a b d) (d c)))

(defun ansi-cl-new-paths (path node net)
  (let (acc)
    (dolist (x (cdr (assoc node net)))
      (or (member x path)
          (push (cons x path) acc)))
    acc))

(defun ansi-cl-bfs-l (end queue net sol)
  (if  queue
      (let ((path (car queue)))
        (let ((node (car path)))
          (ansi-cl-bfs-l end
                         (append (cdr queue) (ansi-cl-new-paths path node net))
                         net
                         (if (eql node end)  path sol))))
    (reverse sol)))

(defun ansi-cl-longest-path (start end net)
  (ansi-cl-bfs-l end (list (list start)) net nil))
