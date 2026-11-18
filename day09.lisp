(uiop:define-package :aoc-25/day09
  (:use :cl :aoc-25/utils)
  (:import-from :cl-ppcre #:split)
  (:documentation "--- Day 9: Movie Theater ---"))

(in-package :aoc-25/day09)

(defun parse-line (line)
  (apply #'make-point-2d (mapcar #'parse-integer (split "," line))))

(defun parse-input (lines)
  (mapcar #'parse-line lines))

(defun area (a b)
  (* (1+ (abs (- (realpart a) (realpart b))))
     (1+ (abs (- (imagpart a) (imagpart b))))))

;; => 4715966250
(defun part1 (&optional (corners (parse-input (read-input :format :lines))))
  (loop for (head . tails) on corners
        maximize (loop for tail in tails
                       maximize (area head tail))))

;; => 1530527040
(defun part2 (&optional (input (parse-input (read-input :format :lines))))
  input)

(defparameter *example* (parse-input '("7,1"
                                       "11,1"
                                       "11,7"
                                       "9,7"
                                       "9,5"
                                       "2,5"
                                       "2,3"
                                       "7,3")))
