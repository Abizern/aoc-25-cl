(uiop:define-package :aoc-25/day02
  (:use :cl :aoc-25/utils)
  (:import-from :cl-ppcre
                #:all-matches-as-strings
                #:split)
  (:documentation "--- Day 2: Gift Shop ---"))

(in-package :aoc-25/day02)

(defun parse-input (line)
  (mapcar (lambda (string) (mapcar #'parse-integer (split "-" string)))
          (all-matches-as-strings "(\\d+-\\d+)" line)))

(defun invalid-1 (number)
  (let* ((str (format nil "~a" number))
         (len (length str))
         (mid (floor len 2)))
    (when (evenp len)
      (string= (subseq str 0 mid) (subseq str mid len)))))

(defun invalid-2 (number)
  (let* ((str (format nil "~a" number))
         (len (length str))
         (doubled (subseq (concatenate 'string str str) 1 (- (* 2 len) 1))))
    (when (>= len 2)
      (search str doubled))))

;; => 18595663903
(defun part1 (&optional (ranges (parse-input (read-input))))
  (loop for (start end) in ranges
        sum (loop for n from start upto end
                  if (invalid-1 n)
                    sum n)))

;; => 19058204438
(defun part2 (&optional (ranges (parse-input (read-input))))
  (loop for (start end) in ranges
        sum (loop for n from start upto end
                  if (invalid-2 n)
                    sum n)))
