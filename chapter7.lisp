(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1 (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
          (buf-new b) (buf-end b))))

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new b) (buf-end b)))

(defun buf-clear (b)
  (setf (buf-start b) -1 (buf-used b) -1
        (buf-new b) -1 (buf-end b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))


(defun file-subst (old new in-file out-file)
  (with-open-file (in in-file :direction :input)
    (with-open-file (out out-file
                         :direction :output
                         :if-exists :supersede)
      (stream-subst old new in out))))

(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((ch (read-char in nil :eof)
             (or (setf from-buf (buf-next buf))
                 (read-char in nil :eof))))
        ((eql ch :eof))
      (cond ((char= ch (char old pos))
             ;; ((or (char= #\+ (char old pos)) (char= ch (char old pos))) ; task 5
             (incf pos)
             (cond ((= pos len)
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)
                    (buf-insert ch buf))))
            ((zerop pos)
             (princ ch out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t
             (unless from-buf (buf-insert ch buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))


;;; task 1
(defun file->string-list (file-name)
  (with-open-file (in file-name :direction :input)
    (do ((line (read-line in nil nil nil) (read-line in nil nil nil))
         (acc nil (cons line acc)))
        ((not line) (nreverse acc)))))

;;; yet another recursive implementation

;; (defun file->string-list (file-name)
;;   (with-open-file (in file-name :direction :input)
;;     (labels ((worker (stream acc)
;;                (let ((line (read-line stream nil nil t)))
;;                  (if (not line)
;;                      (nreverse acc)
;;                      (worker stream (cons line acc))))))
;;       (worker in nil))))


;;; task 2 see above task but replace `read-line` with `read`

;;; task 3
(defun remove-comment (file-in file-out &key (comment "%" supplied-comment-p))
  (when (or supplied-comment-p (stringp comment))
    (with-open-file (in file-in :direction :input)
      (with-open-file (out file-out
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede)
        (do ((line (read-line in nil nil nil) (read-line in nil nil nil)))
            ((not line))
          (let ((pos (search comment line)))
            (format out "~A~%"
                    (if (and pos (plusp pos))
                        (subseq line 0 pos)
                        line))))))))

;;; task 4
(defun display-matrix (mx)
  (let ((size (array-dimensions mx)))
    (when (and (car size)
               (cadr size)
               (null (cddr size)))
      (dotimes (row (first size))
        (dotimes (column (second size))
          (format t "~10,2F" (aref mx row column)))
        (terpri)))))


;;; task 5 see above `stream-subst` implementation

;;; task 6

;;; use c-way format pattern
;;; ~a -- all chars
;;; ~w -- a-z,A-Z,0-9
;;; ~d -- 0-9
;;; ~~ -- itself

(defun ansi-cl-parse (pattern &key (control "~"))
  (let ((len (length pattern)))
    (labels
        ((worker (index trigger acc)
           (if (= index len)
               (coerce (nreverse acc) 'vector)
               (let ((item (aref pattern index)))
                 (cond (trigger
                        (worker (1+ index) nil
                                (cons
                                 (cond ((eq item #\a)
                                        'all)
                                       ((eq item #\w)
                                        'word)
                                       ((eq item #\d)
                                        'digit)
                                       ((eq item control)
                                        'control)
                                       (t item))
                                 acc)))
                       ((eq item control)
                        (worker (1+ index) t acc))
                       (t
                        (worker (1+ index) trigger (cons item acc))))))))
      (worker 0 nil nil))))

;; (princ (ansi-cl-parse "asdf$A$w$$" :control #\$))
