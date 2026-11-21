(uiop:define-package :aoc-25/day10
  (:use :cl :aoc-25/utils)
  (:import-from :alexandria #:map-combinations)
  (:import-from :cl-ppcre
                #:all-matches-as-strings
                #:split)
  (:documentation "--- Day 10: Factory ---"))

(in-package :aoc-25/day10)

(defstruct machine
  (target-mask 0 :type fixnum)
  (wirings '() :type list)
  (joltages '() :type list))

(defun parse-ints (str)
  (mapcar #'parse-integer (all-matches-as-strings "\\d+" str)))

(defun parse-line (line)
  (let* ((bracket-end (position #\] line))
         (brace-start (position #\{ line))
         (lights-str (subseq line 1 bracket-end))
         (wirings-str (subseq line (+ bracket-end 2) (1- brace-start)))
         (joltages-str (subseq line brace-start))
         (target-mask (loop for ch across lights-str
                            for idx from 0
                            when (char= ch #\#)
                              sum (ash 1 idx)))
         (wirings (mapcar #'parse-ints
                          (all-matches-as-strings "\\([^)]+\\)" wirings-str)))
         (joltages (parse-ints joltages-str)))
    (make-machine :target-mask target-mask
                  :wirings wirings
                  :joltages joltages)))


(defun parse-input (lines)
  (mapcar #'parse-line lines))

(defun wiring-mask (indices)
  (reduce (lambda (acc i) (logior acc (ash 1 i))) indices :initial-value 0))

(defun min-toggles (m)
  (let ((target (machine-target-mask m))
        (masks (mapcar #'wiring-mask (machine-wirings m))))
    (loop for k from 1 to (length masks)
          thereis (block found
                    (map-combinations
                     (lambda (combo)
                       (when (= target (reduce #'logxor combo :initial-value 0))
                         (return-from found k)))
                     masks
                     :length k)
                    nil))))

(defun min-presses (m)
  (let* ((wirings (machine-wirings m))
         (targets (coerce (machine-joltages m) 'vector))
         (rows (length targets))
         (cols (length wirings))
         (mat (make-array (list rows (1+ cols)) :initial-element 0)))
    ;; Populate matrix coefficients and target column
    (loop for wiring in wirings
          for c from 0
          do (dolist (r wiring)
               (setf (aref mat r c) 1)))
    (loop for r from 0 below rows
          do (setf (aref mat r cols) (aref targets r)))
    (let* ((pivots (rref mat))
           (free-cols (loop for c from 0 below cols
                            unless (member c pivots)
                              collect c))
           ;; Upper bound for each free button press count
           (free-bounds (loop for c in free-cols
                              collect (loop for r in (nth c wirings)
                                            minimize (aref targets r))))
           (best most-positive-fixnum))
      ;; Search over free variables and compute dependent pivot variables
      (labels ((search-free (rem-cols rem-bounds chosen)
                 (if (null rem-cols)
                     (let ((total (reduce #'+ chosen :key #'cdr)))
                       (when (< total best)
                         (let ((valid t))
                           (loop for r from 0 below (length pivots)
                                 for val = (- (aref mat r cols)
                                              (loop for (f-col . f-val) in chosen
                                                    sum (* (aref mat r f-col) f-val)))
                                 do (if (and (integerp val) (>= val 0))
                                        (incf total val)
                                        (setf valid nil))
                                 while valid)
                           (when (and valid (< total best))
                             (setf best total)))))
                     (let ((col (first rem-cols))
                           (bound (first rem-bounds)))
                       (loop for val from 0 to bound
                             do (search-free (rest rem-cols)
                                             (rest rem-bounds)
                                             (acons col val chosen)))))))
        (search-free free-cols free-bounds '()))
      best)))

;; => 491
(defun part1 (&optional (machines (parse-input (read-input :format :lines))))
  (loop for m in machines
        sum (min-toggles m)))

;; => 20617
(defun part2 (&optional (machines (parse-input (read-input :format :lines))))
  (loop for m in machines
        sum (min-presses m)))

(defparameter *example* (parse-input '("[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
                                       "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
                                       "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")))
