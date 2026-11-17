(uiop:define-package :aoc-25/day05
  (:use :cl :aoc-25/utils)
  (:import-from :cl-ppcre
                #:split
                #:register-groups-bind)
  (:documentation "--- Day 5: Cafeteria ---"))

(in-package :aoc-25/day05)

(defun parse-range (str)
  (register-groups-bind ((#'parse-integer lo hi))
      ("(\\d+)-(\\d+)" str)
    (cons lo hi)))

(defun merge-ranges (ranges)
  "Return RANGES, a list of (LOW . HIGH) conses as a list of disjoint
ranges in order. Ranges that overlap or are adjacent are combined.
RANGES does not need to be sorted."
  (let ((rr (sort (copy-list ranges) #'< :key #'car))
        (merged '()))
    (dolist (r rr (nreverse merged))
      (destructuring-bind (lo . hi) r
        (let ((current (first merged)))
          (if (and current (<= lo (1+ (cdr current))))
              (setf (cdr current) (max (cdr current) hi))
              (push (cons lo hi) merged)))))))

(defun fresh-p-fn (ranges)
  (lambda (ingredient)
    (loop for (low . high) in ranges
          thereis (<= low ingredient high))))

(defun parse-input (input)
  (let* ((parts (split "\\n\\n" input))
         (ranges (merge-ranges (mapcar #'parse-range (split "\\n" (first parts)))))
         (ingredients (mapcar #'parse-integer (split "\\n" (second parts)))))
    (list ranges ingredients)))

;; => 744
(defun part1 (&optional (input (parse-input (read-input))))
  (destructuring-bind (ranges ingredients) input
    (count-if (fresh-p-fn ranges) ingredients)))

;; => 347468726696961
(defun part2 (&optional (input (parse-input (read-input))))
  (let ((ranges (first input)))
    (loop for (low . high) in ranges
          sum (1+ (- high low)))))
