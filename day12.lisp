(uiop:define-package :aoc-25/day12
  (:use :cl :aoc-25/utils)
  (:import-from :cl-ppcre
                #:split
                #:all-matches-as-strings)
  (:documentation "--- Day 12: Christmas Tree Farm ---"))

(in-package :aoc-25/day12)

(defun parse-integers (str)
  (mapcar #'parse-integer (all-matches-as-strings "\\d+" str)))

(defun parse-input (input)
  (let ((numbers (mapcar #'parse-integers
                         (split "\\n" (last1 (split "\\n\\n" input))))))
    (loop for nn in numbers
          collect (cons (* (first nn) (second nn))
                        (* (* 3 3) (apply #'+ (nthcdr 2 nn)))))))

;; => 440
(defun part1 (&optional (input (parse-input (read-input))))
  (loop for (area . needed) in input
        count (>= area needed)))
