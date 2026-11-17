(uiop:define-package :aoc-25/day04
  (:use :cl :aoc-25/utils)
  (:documentation "--- Day 4: Printing Department ---"))

(in-package :aoc-25/day04)

(defun roll-p (c)
  (char= #\@ c))

(defun accessible-p (grid cell)
  (> 4 (count-if #'roll-p
                 (mapcar (lambda (c) (cell-value grid c))
                         (cell-neighbours grid cell :diagonals t)))))

(defun find-accessible-rolls (grid)
  (let ((rolls (cells-matching grid #'roll-p )))
    (count-if #'identity (mapcar (lambda (c)
                                   (accessible-p grid c))
                                 rolls))))

;; => 1384
(defun part1 (&optional (grid (lines-to-grid (read-input :format :lines))))
  (find-accessible-rolls grid))

;; => 8013
(defun part2 (&optional (grid (lines-to-grid (read-input :format :lines))))
  grid)

(defparameter *example* (lines-to-grid '("..@@.@@@@."
                                         "@@@.@.@.@@"
                                         "@@@@@.@.@@"
                                         "@.@@@@..@."
                                         "@@.@@@@.@@"
                                         ".@@@@@@@.@"
                                         ".@.@.@.@@@"
                                         "@.@@@.@@@@"
                                         ".@@@@@@@@."
                                         "@.@.@@@.@.")))
