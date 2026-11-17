(uiop:define-package :aoc-25/day01
  (:use :cl :aoc-25/utils)
  (:import-from :cl-ppcre #:split #:register-groups-bind)
  (:documentation "--- Day 1: Secret Entrance ---"))

(in-package :aoc-25/day01)

(defun parse-line (line)
  (register-groups-bind (dir (#'parse-integer val))
      ("(L|R)(\\d+)" line)
    (cons (if (string= "L" dir) :L :R) val)))

(defun parse-input (lines)
  (mapcar #'parse-line lines))

(defun turn-dial (pos turn)
  (let ((dir (car turn))
        (val (cdr turn)))
    (ecase dir
      (:L (mod (- pos val) 100))
      (:R (mod (+ pos val) 100)))))

(defun crossing-0 (pos turn)
  (let* ((dir (car turn))
         (val (cdr turn))
         (cnt (floor val 100))
         (new-pos (turn-dial pos turn)))
    (cond
      ((= 0 new-pos) (incf cnt))
      ((and (eq :L dir) (/= 0 pos) (> new-pos pos)) (incf cnt))
      ((and (eq :R dir) (< new-pos pos)) (incf cnt)))
    (list new-pos cnt)))

;; => 1102
(defun part1 (&optional (turns (parse-input (read-input :format :lines))))
  (loop for pos = 50 then (turn-dial pos turn)
        for turn in turns
        counting (= 0 pos)))

;; => 6175
(defun part2 (&optional (turns (parse-input (read-input :format :lines))))
  (loop with pos = 50
        with cnt = 0
        for turn in turns
        do (destructuring-bind (new-pos c) (crossing-0 pos turn)
             (setf pos new-pos)
             (incf cnt c))
        finally (return cnt)))

(defparameter *example* (mapcar #'parse-line '("L68"
                                               "L30"
                                               "R48"
                                               "L5"
                                               "R60"
                                               "L55"
                                               "L1"
                                               "L99"
                                               "R14"
                                               "L82")))
