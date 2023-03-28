(defpackage "HENLEY"
  (:use "COMMON-LISP")
  ;; (:nickname "HENLEY")
  (:export "READ_TEXT" "GENERATE_TEXT" "GENERATE-SEN-TEST"))

(in-package henley)

(defparameter *words* (make-hash-table :size 10000))

(defconstant +max-word+ 100)

(defun read-text (pathname)
  (with-open-file (in pathname :direction :input)
    (let ((buffer (make-string +max-word+))
          (pos 0))
         (do ((ch (read-char in nil :eof)
                  (read-char in nil :eof)))
             ((eql ch :eof))
           (if (or (alpha-char-p ch) (char= ch #\'))
               (progn
                 (setf (aref buffer pos) ch)
                 (incf pos))
               (progn
                 (unless (zerop pos)
                   (see (intern (string-downcase (subseq buffer 0 pos))))
                   (setf pos 0))
                 (let ((p (punc ch)))
                   (when p (see p)))))))))

(defun punc (ch)
  (case ch
    (#\. '|.|)
    (#\, '|,|)
    (#\; '|;|)
    (#\! '|!|)
    (#\? '|?|)
    (#\- '|-|)
    (#\— '|—|)))

(let ((prev '|.|))
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *words*))))
      (if (null pair)
          (push (cons symb 1) (gethash prev *words*))
          (incf (cdr pair)))
      (setf prev symb))))


(defun generate-text (n &optional (prev '|.|))
  (if (zerop n)
      (terpri)
      (let ((next (random-next prev)))
        (format t "~A " next)
        (generate-text (1- n) next))))

(defun random-next (prev)
  (let* ((cnoises (gethash prev *words*))
         (i (random (reduce #'+ cnoises :key #'cdr))))
    (dolist (pair cnoises)
      (if (minusp (decf i (cdr pair)))
          (return (car pair))))))


;;; task 4
;;;
;;; Add following code at the top of the code shown in fig 7.1.
;; (defpackage "LING"
;;   (:use "COMMON-LISP")
;;   (:export "BUF" "BREF" "NEW-BUF" "BUF-INSERT" "BUF-POP" "BUF-NEXT" "BUF-RESET" "BUF-CLEAR" "BUF-FLUSH"))
;;
;; (in-package ling)

;;; Add following code at the top of the code shown in Fig. 7.2
;; (defpackage "FILE"
;;   (:use "COMMON-LISP" "LING"))
;; (in-package file)


;;; task 5
;;; https://github.com/HerculesShek/ansi-cl-practise/blob/master/src/ch8-symbols-ex5.lisp
;;; simple direction linked list
;; get the first word and invoke the recursive function--henleyp-process
(defun henleyp-sentence (sen &optional (test #'my-char))
  (multiple-value-bind (first index) (next-w sen test 0)
    (if first
        ;; string-downcase can turn a char into a string  (string-downcase #\A) => "a"
        (let ((first-sym (intern (string-downcase first))))
          (if (gethash first-sym *words*)
              (henleyp-process sen first-sym test index)
              nil)))))

;; working function -- recursive
(defun henleyp-process (sen pre-sym test start)
  (let ((choices (gethash pre-sym *words*)))
    (multiple-value-bind (next index) (next-w sen test start)
      (let ((next-sym (intern (string-downcase next))))
        (if (and next (member next-sym choices :key #'car))
            (if (null index)
                t
                (henleyp-process sen next-sym test index)))))))

;; Get the first word or punctuation of sen from start
;; and the index for getting the next word
(defun next-w (sen &optional (test #'my-char) (start 0))
  (let ((p1 (position-if test sen :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (x)
                                   (not (funcall test x)))
                               sen :start (1+ p1)))
              (prefix (position-if #'punc sen :start start :end p1)))
          (if prefix
              (values (char sen prefix) (1+ prefix))
              (values (subseq sen p1 p2) p2)))
        (values nil nil))))

;; test next-w
(defun tokens (sen &optional (test #'my-char) (start 0))
  (multiple-value-bind (word index) (next-w sen test start)
    (when word
      (format t "~A index is ~A  " word index)
      (format t "length is ~A ~%" (if (characterp word) 1 (length word)))
      (if index
          (tokens sen test index)))))

(defun my-char (c)
  (or (alpha-char-p c)
      (char= c #\')
      (char= c #\-)))


;;; task 6
;;; https://github.com/HerculesShek/ansi-cl-practise/blob/master/src/ch8-symbols-ex6.lisp
(defparameter *fwords* (make-hash-table :size 10000))
(defparameter *bwords* (make-hash-table :size 10000))
(defconstant +wcount+ 4)


(let ((prev '|.|))
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *words*))))
      (if (null pair)
          (push (cons symb 1) (gethash prev *words*))
          (incf (cdr pair)))
      (setf prev symb))))

;; PART II
;; generate a sentence: take a word and generate a sentence
;; with that word in the middle of it.
(defun generate-sen (w)
  (let ((sym (intern (string-downcase w))))
    (do* ((f-next (random-next sym) (random-next f-next))
          (b-next (random-next sym :from-end t) (random-next b-next :from-end t))
          (res (list b-next sym f-next) (append (cons b-next res) (list f-next)))
          (i 1 (1+ i)))
         ((= i +wcount+) res))))

;; get a word according to the preceding word
(defun random-next (prev &key (from-end nil))
  (let ((choices (gethash prev (if from-end *bwords* *fwords*))))
    (if (null choices)
        '|.|
        (let ((i (random (reduce #'+ choices :key #'cdr))))
          (dolist (pair choices)
            (if (minusp (decf i (cdr pair)))
                (return (car pair)))))))
  ;; (let* ((choices (gethash prev (if from-end *bwords* *fwords*)))
  ;;        (i (random (reduce #'+ choices
  ;;                           :key #'cdr))))
  ;;   (if choices
  ;;       (dolist (pair choices)
  ;;         (if (minusp (decf i (cdr pair)))
  ;;             (return (car pair))))))
  )

(defun generate-sen-test ()
  (read-text "./text/lost.txt")
  (generate-sen "you"))
