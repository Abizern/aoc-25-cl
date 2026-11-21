(uiop:define-package :aoc-25/day11
  (:use :cl :aoc-25/utils)
  (:import-from :cl-ppcre #:all-matches-as-strings)
  (:documentation "--- Day 11: Reactor ---"))

(in-package :aoc-25/day11)

(defun parse-input (lines)
  (let ((adjacency (make-hash-table :test #'equal)))
    (dolist (line lines)
      (let ((tokens (all-matches-as-strings "[a-z]+" line)))
        (when tokens
          (setf (gethash (first tokens) adjacency) (rest tokens)))))
    adjacency))

(defun count-simple-paths (node adjacency memo)
  (multiple-value-bind (cached found-p) (gethash node memo)
    (cond
      (found-p cached)
      ((string= node "out")
       (setf (gethash node memo) 1))
      (t
       (let ((total (loop for neighbour in (gethash node adjacency)
                          sum (count-simple-paths neighbour adjacency memo))))
         (setf (gethash node memo) total))))))

(defun count-valid-paths (node mask adjacency memo)
  (let* ((new-mask (cond ((string= node "dac") (logior mask 1))
                         ((string= node "fft") (logior mask 2))
                         (t mask)))
         (state (cons node new-mask)))
    (multiple-value-bind (cached found-p) (gethash state memo)
      (cond
        (found-p cached)
        ((string= node "out")
         (setf (gethash state memo) (if (= new-mask 3) 1 0)))
        (t
         (let ((total (loop for neighbour in (gethash node adjacency)
                            sum (count-valid-paths neighbour new-mask adjacency memo))))
           (setf (gethash state memo) total)))))))

;; => 497
(defun part1 (&optional (table (parse-input (read-input :format :lines))))
  (count-simple-paths "you" table (make-hash-table :test #'equal)))

;; => 358564784931864
(defun part2 (&optional (table (parse-input (read-input :format :lines))))
  (count-valid-paths "svr" 0 table (make-hash-table :test #'equal)))

(defparameter *example1*
  (parse-input '("aaa: you hhh"
                 "you: bbb ccc"
                 "bbb: ddd eee"
                 "ccc: ddd eee fff"
                 "ddd: ggg"
                 "eee: out"
                 "fff: out"
                 "ggg: out"
                 "hhh: ccc fff iii"
                 "iii: out")))

(defparameter *example2*
  (parse-input '("svr: aaa bbb"
                 "aaa: fft"
                 "fft: ccc"
                 "bbb: tty"
                 "tty: ccc"
                 "ccc: ddd eee"
                 "ddd: hub"
                 "hub: fff"
                 "eee: dac"
                 "dac: fff"
                 "fff: ggg hhh"
                 "ggg: out"
                 "hhh: out")))
