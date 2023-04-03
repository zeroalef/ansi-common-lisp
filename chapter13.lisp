;; (defparameter *arr*
;;   (make-array '(1000 10000)
;;               :element-type 'double-float
;;               :initial-element 1.0d0))

(defun fast-sum (a)
  (declare (type (simple-array double-float (1000 10000)) a))
  (let ((sum 0.0d0))
    (declare (type sum double-float))
    (dotimes (i 1000)
      (dotimes (j 10000)
        (incf sum (aref a i j))))
    sum))

(defun slow-sum (a)
  (let ((sum 0.0d0))
    (dotimes (i 1000)
      (dotimes (j 10000)
        (incf sum (aref a i j))))
    sum))


(defun ansi-cl-adjoin (item xs &rest args)
  (declare (dynamic-extent args))
  (if (apply #'member item xs args)
      xs
      (cons item xs)))



(defstruct ship
  name flag tons)

(defparameter *pool*
  (make-array 1000
              :fill-pointer t
              :element-type 'ship
              :initial-element (make-ship :name "" :flag "" :tons 0)))

(defparameter *harbour*
  (make-hash-table
   :size 1100
   :test #'eql))


(defun enter (n f d)
  (let ((s (if (plusp (length *pool*))
               (vector-pop *pool*)
               (make-ship))))
    (setf (ship-name s) n
          (ship-flag s) f
          (ship-tons s) d
          (gethash n *harbour*) s)))

(defun find-ship (n)
  (gethash n *harbour*))

(defun leave (n)
  (let ((s (gethash n *harbour*)))
    (remhash n *harbour*)
    (vector-push s *pool*)))


;;; task 1
(declaim (inline square))
(defun square (x)
  (* x x))

(defun calll-square (n)
  (square n))
;; CL-USER> (disassemble 'calll-square)
;; ; disassembly for CALLL-SQUARE
;; ; Size: 33 bytes. Origin: #x53677D34                          ; CALLL-SQUARE
;; ; 34:       498B4510         MOV RAX, [R13+16]                ; thread.binding-stack-pointer
;; ; 38:       488945F8         MOV [RBP-8], RAX
;; ; 3C:       488BD6           MOV RDX, RSI
;; ; 3F:       488BFE           MOV RDI, RSI
;; ; 42:       FF1425F800A052   CALL QWORD PTR [#x52A000F8]      ; GENERIC-*
;; ; 49:       488B75F0         MOV RSI, [RBP-16]
;; ; 4D:       488BE5           MOV RSP, RBP
;; ; 50:       F8               CLC
;; ; 51:       5D               POP RBP
;; ; 52:       C3               RET
;; ; 53:       CC10             INT3 16                          ; Invalid argument count trap
;; NIL
;; CL-USER> (disassemble 'square)
;; ; disassembly for SQUARE
;; ; Size: 33 bytes. Origin: #x5364AD34                          ; SQUARE
;; ; 34:       498B4510         MOV RAX, [R13+16]                ; thread.binding-stack-pointer
;; ; 38:       488945F8         MOV [RBP-8], RAX
;; ; 3C:       488BD6           MOV RDX, RSI
;; ; 3F:       488BFE           MOV RDI, RSI
;; ; 42:       FF1425F800A052   CALL QWORD PTR [#x52A000F8]      ; GENERIC-*
;; ; 49:       488B75F0         MOV RSI, [RBP-16]
;; ; 4D:       488BE5           MOV RSP, RBP
;; ; 50:       F8               CLC
;; ; 51:       5D               POP RBP
;; ; 52:       C3               RET
;; ; 53:       CC10             INT3 16                          ; Invalid argument count trap
;; NIL
;; CL-USER>

;;; task 2
(defun foo (n)
  (if (zerop n)
      0
      (1+ (foo (1- n)))))

;; CL-USER> (time (foo 100000))
;; Control stack guard page temporarily disabled: proceed with caution
;; Evaluation took:
;;   2.071 seconds of real time
;;   0.003892 seconds of total run time (0.003892 user, 0.000000 system)
;;   0.19% CPU
;;   3 forms interpreted
;;   7,449,794,928 processor cycles
;;   5,765,136 bytes consed

;;   before it was aborted by a non-local transfer of control.

; Evaluation aborted on #<SB-KERNEL::CONTROL-STACK-EXHAUSTED {1004C49F03}>.


(defun tail-foo (n)
  (labels ((core (k acc)
             (if (zerop k)
                 acc
                 (core (1- k) (1+ acc)))))
    (core n 0)))
;; CL-USER> (time (tail-foo 100000))
;; Evaluation took:
;;   0.004 seconds of real time
;;   0.000830 seconds of total run time (0.000830 user, 0.000000 system)
;;   25.00% CPU
;;   2,984,688 processor cycles
;;   0 bytes consed

;; 100000
;; CL-USER>

;;; task 3-5 not interested
