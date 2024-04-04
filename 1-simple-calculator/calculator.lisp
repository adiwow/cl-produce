;;; a simple calculator that accepts only non-decimal integer.
;;; you can just modified this code in able to experiment and enhance whatever you like, This is just a representation of how to create a simple calculator in a way of defining a function (using conditional,case and assignment such as let)and reading an input using *query-io* 

(defun calculate (num1 num2 operator)
  "Perform arithmetic operation based on operator."
  (cond
    ((eql operator 'a) (+ num1 num2))
    ((eql operator 'b) (- num1 num2))
    ((eql operator 'c) (* num1 num2))
    ((eql operator 'd) (/ num1 num2))
    (t (format t "Invalid operator!~%"))))

(defun read-number ()
  "Read a number from user input."
  (format *query-io* "Enter a number: ")
  (finish-output *query-io*)
  (read *query-io*))

(defun read-operator ()
  "Read an operator from user input."
  (format *query-io* "Enter an operator:~%a.) Addition~%b.) Subtraction~%c.) Multiplication~%d.) Division~%")
  (finish-output *query-io*)
  (let ((operator (read-char *query-io*)))
    (case operator
      (#\a 'a)
      (#\b 'b)
      (#\c 'c)
      (#\d 'd)
      (t (format t "Invalid operator! Try again.~%")
         (read-operator)))))

(defun simple-calculator ()
  "Simple calculator program."
  (format t "Welcome to Simple Calculator!~%")
  (let ((num1 (read-number)))
    (let ((num2 (read-number)))
      (let ((operator (read-operator)))
        (format t "Result: ~a~%" (calculate num1 num2 operator))))))

;; Run the calculator
(simple-calculator)
