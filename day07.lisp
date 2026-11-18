(uiop:define-package :aoc-25/day07
  (:use :cl :aoc-25/utils)
  (:import-from :alexandria #:hash-table-keys)
  (:documentation "--- Day 7: Laboratories ---"))

(in-package :aoc-25/day07)

(defun start-point (grid)
  (first (cells-matching grid (lambda (c) (char= c #\S)))))

(defun count-beams (grid)
  (let* ((start (start-point grid))
         (beams (list start))
         (height (grid-rows grid))
         (count 0))
    (dotimes (i (- height (car start) 1))
      (let ((next-beams (make-hash-table :test #'equal)))
        (dolist (beam beams)
          (let* ((r (1+ (car beam)))
                 (c (cdr beam))
                 (ch (cell-value grid (cons r c))))
            (if (char= ch #\.)
                (setf (gethash (cons r c) next-beams) t)
                (progn
                  (incf count)
                  (setf (gethash (cons r (1- c)) next-beams) t
                        (gethash (cons r (1+ c)) next-beams) t)))))
        (setf beams (hash-table-keys next-beams))))
    count))

(defun count-timelines (grid)
  (let* ((start (start-point grid))
         (timelines (make-hash-table :test #'equal))
         (height (grid-rows grid)))
    (setf (gethash start timelines) 1)
    (dotimes (i (- height (car start) 1))
      (let ((next-timelines (make-hash-table :test #'equal)))
        (maphash (lambda (beam count)
                   (let* ((r (1+ (car beam)))
                          (c (cdr beam))
                          (ch (cell-value grid (cons r c))))
                     (if (char= ch #\.)
                         (incf (gethash (cons r c) next-timelines 0) count)
                         (progn
                           (incf (gethash (cons r (1- c)) next-timelines 0) count)
                           (incf (gethash (cons r (1+ c)) next-timelines 0) count)))))
                 timelines)
        (setf timelines next-timelines)))
    (loop for count being the hash-values of timelines
          sum count)))

;; => 1573
(defun part1 (&optional (grid (lines-to-grid (read-input :format :lines))))
  (count-beams grid))

;; => 15093663987272
(defun part2 (&optional (grid (lines-to-grid (read-input :format :lines))))
  (count-timelines grid))

(defparameter *example* (lines-to-grid '(".......S......."
                                         "..............."
                                         ".......^......."
                                         "..............."
                                         "......^.^......"
                                         "..............."
                                         ".....^.^.^....."
                                         "..............."
                                         "....^.^...^...."
                                         "..............."
                                         "...^.^...^.^..."
                                         "..............."
                                         "..^...^.....^.."
                                         "..............."
                                         ".^.^.^.^.^...^."
                                         "...............")))
