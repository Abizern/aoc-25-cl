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
  (rect-area (make-rect-from-points a b)))

(defun build-edges (corners)
  (loop for (a . rest) on corners
        for b = (or (first rest) (first corners))
        collect (cons a b)))

(defun cut-by-edges-p (rect edges)
  (destructuring-bind (rx1 rx2 ry1 ry2) (rect-corners rect)
    (loop for (a . b) in edges
          thereis (if (= (realpart a) (realpart b))
                      (let ((vx (realpart a)))
                        (and (< rx1 vx rx2)
                             (< (max (min (imagpart a) (imagpart b)) ry1)
                                (min (max (imagpart a) (imagpart b)) ry2))))
                      (let ((hy (imagpart a)))
                        (and (< ry1 hy ry2)
                             (< (max (min (realpart a) (realpart b)) rx1)
                                (min (max (realpart a) (realpart b)) rx2))))))))

(defun in-polygon-p (x y edges)
  (let ((inside nil))
    (loop for (a . b) in edges
          for xi = (realpart a)
          for yi = (imagpart a)
          for xj = (realpart b)
          for yj = (imagpart b)
          when (and (not (eq (> yi y) (> yj y)))
                    (< x (+ (/ (* (- xj xi) (- y yi))
                               (- yj yi))
                            xi)))
            do (setf inside (not inside)))
    inside))

(defun valid-rectangle-p (a b edges)
  (let ((rect (make-rect-from-points a b)))
    (and (not (cut-by-edges-p rect edges))
         (let ((cx (/ (+ (r2-x1 rect) (r2-x2 rect)) 2))
               (cy (/ (+ (r2-y1 rect) (r2-y2 rect)) 2)))
           (in-polygon-p cx cy edges)))))

;; => 4715966250
(defun part1 (&optional (corners (parse-input (read-input :format :lines))))
  (loop for (head . tails) on corners
        maximize (loop for tail in tails
                       maximize (area head tail))))

;; => 1530527040
(defun part2 (&optional (corners (parse-input (read-input :format :lines))))
  (let ((edges (build-edges corners))
        (best 0))
    (loop for (head . tails) on corners
          do (loop for tail in tails
                   for candidate = (area head tail)
                   when (and (> candidate best)
                             (valid-rectangle-p head tail edges))
                     do (setf best candidate)))
    best))

(defparameter *example* (parse-input '("7,1"
                                       "11,1"
                                       "11,7"
                                       "9,7"
                                       "9,5"
                                       "2,5"
                                       "2,3"
                                       "7,3")))
