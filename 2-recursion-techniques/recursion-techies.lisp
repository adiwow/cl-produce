#|
NOTE:
Reference from CLAGITSC
This is just a NOTES not a project.
|#



;====================================
;;;           RECURSION           ;;;
;====================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Double-Test Tail Recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Template:
(defun func (X)
  (cond (end-test-1 end-value-1)
	(end-test-2 end-value-2)
	(T (func reduced-x))))

(defun find-odd (x)
  (cond ((null x) nil)
	((oddp (first x)) t)
	(t (find-odd (rest x)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Regular Recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;create a new stacks frame because of multiplication it performed first before the function
(defun fact (n)
  (cond ((zerop n) 1)
	(t (* n (fact (- n 1))))))

;;;tail-recursion that will save time and resources that used the last operation in the function as recursive calls
;;;instead of adding new frames to stack, it updates the function and jumps back to the beggining of function
(defun factorial (n &optional (acc 1))
  (cond ((zerop n) acc)
	(t (factorial (1- n)(* acc n)))))

;;Sample Usage
(factorial 5) ; This will return 120


;;;Recursion using labels

(defun foo (arg)
  "Meh."
  (labels ((fun (val acc)
             (cond ((null val) (nreverse acc))
                   (t (fun (cdr val)
                           (cons (+ 1 (car val)) acc))))))
    (fun arg nil)))

;;Sample Usage
(foo '(1 2 3 4 5))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Single-Test Tail Recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Template:
(defun func (X)				
  (cond (end-test end-value)
	(T (func reduced-x))))

;;Example:
;;Func:      FIND-FIRST-ATOM
;;End-test:  (ATOM X)
;;End-value: X
;;Reduced-x: (FIRST X)

(defun find-first-atom (x)
  (cond ((atom x) x)
	(t (find-first-atom (first x)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Single-Test Augmenting Recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Template:
(defun func (X)
  (cond (end-test end-value)
	(T (aug-fun aug-val
		    (func reduced-x)))))

;; Example:
;; Func:      COUNT-SLICES
;; End-test:  (NULL X)
;; End-value: 0
;; Aug-fun:   +
;; Aug-val:   1
;; Reduced-x: (REST X)

(defun count-slices (x)
  (cond ((null x) 0)
	(t (+ 1 (count-slices (rest x))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;List-Consing Recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(A Special Case of Augmenting Recursion)
;;;Template:

(defun func (N)
  (cond (end-test NIL)
	(T (CONS new-element
		 (func reduced-n)))))

;;Example:
;;Func:        LAUGH
;;End-test:    (ZEROP N)
;;New-element: ’HA
;;Reduced-n:   (- N 1)

(defun laugh (n)
  (cond ((zerop n) nil)
	(t (cons 'ha (laugh (- n 1))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Simultaneous Recursion on Several Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(Using the Single-Test Tail Recursion Template)
;;Template:

(defun func (N X)
  (cond (end-test end-value)
	(t (func reduced-n reduced-x))))

;;Example:
;;Func:       MY-NTH
;;End-test:   (ZEROP N)
;;End-value:  (FIRST X)
;;Reduced-n:  (- N 1)
;;Reduced-x:  (REST X)

(defun my-nth (n x)
  (cond ((zerop n) (first x))
	(t (my-nth (- n 1) (rest x)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Conditional Augmentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Template:
#|(defun func (X)
  (cond (end-test end-value)
	(aug-test (aug-fun aug-val
			   (func reduced-x))
		  (T (func reduced-x))))
|#  
;; Example:
;; Func: EXTRACT-SYMBOLS
;; End-test: (NULL X)
;; End-value: NIL
;; Aug-test: (SYMBOLP (FIRST X))
;; Aug-fun: CONS
;; Aug-val: (FIRST X)
  ;; Reduced-x: (REST X)

(defun extract-symbols (x)
  (cond ((null x) nil)
	((symbolp (first x))
	 (cons (first x)
	       (extract-symbols (rest x))))
	(t (extract-symbols (rest x)))))

;;using labels
(defun extract-symbols (arg)
  (labels ((extract (x)
             (cond
               ((null x) nil)
               ((symbolp (first x))
                (cons (first x)
                      (extract (rest x))))
               (t (extract (rest x))))))
    (extract arg)))


;;Sample Usage
(foo '(1 2 3 4 5))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multiple Recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Template:
  (defun func (N)
    (cond (end-test-1 end-value-1)
	  (end-test-2 end-value-2)
	  (T (combiner (func first-reduced-n)
		       (func second-reduced-n)))))
;; Example:
;; Func:              FIB
;; End-test-1:        (EQUAL N 0)
;; End-value-1:       1
;; End-test-2:        (EQUAL N 1)
;; End-value-2:       1
;; Combiner:          +
;; First-reduced-n:   (− N 1)
;; Second-reduced-n:  (− N 2)
(defun fib (n)
  (cond ((equal n 0) 1)
	((equal n 1) 1)
	(t (+ (fib (- n 1))
	      (fib (- n 2))))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
;;;CAR/CDR Recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
;;(A Special Case of Multiple Recursion)
  ;;Template:
  (defun func (X)
    (cond (end-test-1 end-value-1)
	  (end-test-2 end-value-2)
	  (T (combiner (func (CAR X))
		       (func (CDR X))))))
  
;; Example:
;; Func:        FIND-NUMBER
;; End-test-1:  (NUMBERP X)
;; End-value-1: X
;; End-test-2:  (ATOM X)
;; End-value-2: NIL
;; Combiner:    OR

(defun find-number (x)
  (cond ((numberp x) x)
	((atom x) nil)
	(t (or (find-number (car x))
	       (find-number (cdr x))))))


(defun fibonacci (n &optional (a 0) (b 1))
  "Generate the Fibonacci sequence up to the nth term."
  (cond ((zerop n) nil)
        ((= n 1) (list a))
        (t (let ((next (fibonacci (1- n) b (+ a b))))
             (cons a next)))))

(defun factorial (n &optional (acc 1))
  (if (zerop n)
      acc
      (factorial (1- n) (* acc n))))

