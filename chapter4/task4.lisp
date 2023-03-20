(defstruct three-child-three
  item left middle right)


(defun ansi-cl-copy-tch (tch)
  (when tct
    (make-three-child-tree
     :item (three-child-three-item tct)
     :left (ansi-cl-copy-tch (three-child-three-left tct))
     :middle (ansi-cl-copy-tch (three-child-three-middle tct))
     :right (ansi-cl-copy-tch (three-child-three-right tct)))))


(defun ansi-cl-tct-find (item tct)
  (when tct
    (or (eql item (three-child-three-item tct))
        (ansi-cl-tct-find (three-child-three-left tct))
        (ansi-cl-tct-find (three-child-three-middle tct))
        (ansi-cl-tct-find (three-child-three-right tct)))))
