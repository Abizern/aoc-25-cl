(uiop:define-package :aoc-25/day08
  (:use :cl :aoc-25/utils)
  ;; (:import-from :cl-ppcre #:split #:register-groups-bind)
  (:documentation "--- Day 8: Playground ---"))

(in-package :aoc-25/day08)

(defun parse-line (line)
  line)

(defun parse-input (lines)
  (mapcar #'parse-line lines))

;; => 72150
(defun part1 (&optional (input (parse-input (read-input :format :lines))))
  input)

;; => 3926518899
(defun part2 (&optional (input (parse-input (read-input :format :lines))))
  input)
