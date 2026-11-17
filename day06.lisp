(uiop:define-package :aoc-25/day06
  (:use :cl :aoc-25/utils)
  (:import-from :cl-ppcre
                #:all-matches-as-strings)
  (:documentation "--- Day 6: Trash Compactor ---"))

(in-package :aoc-25/day06)

(defun parse-worksheet-1 (lines)
  (let ((numbers
          (mapcar (lambda (line) (mapcar #'parse-integer
                                         (all-matches-as-strings "\\d+" line)))
                  (butlast lines)))
        (operators
          (all-matches-as-strings "\\+|\\*" (last1 lines))))
    (loop for nums in (transpose numbers)
          for op in operators
          collect (cons op nums))))

(defun column-numbers (lines)
  (let* ((rows (loop for line in lines
                     collect (loop for c across line
                                   collect (digit-char-p c))))
         (groups (loop with group = '()
                       for column in (transpose rows)
                       if (every #'null column)
                         collect (nreverse group) into groups
                         and do (setf group '())
                       else
                         do (push column group)
                       finally (return (nconc groups (list (nreverse group)))))))
    (loop for group in groups
          collect (loop for column in group
                        collect (digits-to-number (remove-if #'null column))))))

(defun parse-worksheet-2 (lines)
  (let ((numbers (column-numbers (butlast lines)))
        (operators
          (all-matches-as-strings "\\+|\\*" (last1 lines))))
    (loop for nums in numbers
          for op in operators
          collect (cons op nums))))

(defun evaluate-list (list)
  (destructuring-bind (op . nums) list
    (reduce (find-symbol op :cl) nums)))

;; => 5227286044585
(defun part1 (&optional (input (read-input :format :lines)))
  (loop for problem in (parse-worksheet-1 input)
        sum (evaluate-list problem)))

;; => 10227753257799
(defun part2 (&optional (input (read-input :format :lines)))
  (loop for problem in (parse-worksheet-2 input)
        sum (evaluate-list problem)))

(defparameter *example* '("123 328  51 64 "
                          " 45 64  387 23 "
                          "  6 98  215 314"
                          "*   +   *   +  "))
