(uiop:define-package :aoc-25/day04
  (:use :cl :aoc-25/utils)
  (:documentation "--- Day 4: Printing Department ---"))

(in-package :aoc-25/day04)

(defun roll-p (c)
  (char= #\@ c))

(defun accessible-p (grid cell)
  (> 4 (count-if #'roll-p
                 (cell-neighbours grid cell :diagonals t)
                 :key (lambda (c) (cell-value grid c)))))

(defun filter-accessible-rolls (grid rolls)
  (remove-if-not (lambda (c) (accessible-p grid c)) rolls))

(defun accessible-rolls (grid)
  (filter-accessible-rolls
   grid
   (cells-matching grid #'roll-p)))

(defun remove-rolls (grid rolls)
  (loop for roll in rolls
        do (setf (cell-value grid roll) #\.)))

;; => 1384
(defun part1 (&optional (grid (lines-to-grid (read-input :format :lines))))
 (length (accessible-rolls grid)))

;; => 8013
(defun part2 (&optional (grid (lines-to-grid (read-input :format :lines))))
  (loop with g = (copy-grid grid)
        for accessible = (accessible-rolls g)
        while accessible
        sum (length accessible)
        do (remove-rolls g accessible)))

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
