(uiop:define-package :aoc-25/day03
  (:use :cl :aoc-25/utils)
  ;; (:import-from :cl-ppcre #:split #:register-groups-bind)
  (:documentation "--- Day 3: Lobby ---"))

(in-package :aoc-25/day03)

(defun parse-input (lines)
  (mapcar #'digits lines))

(defun largest-number (lst)
  (let ((butlast (butlast lst))
        (last (last1 lst)))
    (loop with digits = 0
          with tens = 0
          for n in butlast
          do (cond
               ((> n tens) (setf tens n digits 0))
               ((> n digits) (setf digits n)))
          finally (return (digits-to-number (list tens (if (> last digits) last digits)))))))

;; => 17452
(defun part1 (&optional (input (parse-input (read-input :format :lines))))
  (reduce #'+ (mapcar #'largest-number input)))

;; => 173300819005913
(defun part2 (&optional (input (parse-input (read-input :format :lines))))
  input)

(defparameter *example* (mapcar #'digits '("987654321111111"
                                          "811111111111119"
                                          "234234234234278"
                                          "818181911112111")))
