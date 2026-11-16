(uiop:define-package :aoc-25/day01
  (:use :cl :aoc-25/utils)
  ;; (:import-from :cl-ppcre #:split #:register-groups-bind)
  (:documentation ""))

(in-package :aoc-25/day01)

;; For optimised SBCL builds, declare inside the function that needs it:
;;   (declare (optimize (speed 3) (safety 0) (debug 0)))

(defun parse-line (line)
  line)

(defun parse-input (lines)
  (mapcar #'parse-line lines))

;; =>
(defun part1 (&optional (input (parse-input (read-input :format :lines))))
  input)

;; =>
(defun part2 (&optional (input (parse-input (read-input :format :lines))))
  input)
