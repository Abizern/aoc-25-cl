(uiop:define-package :aoc-25/day08
  (:use :cl :aoc-25/utils)
  (:import-from :cl-ppcre #:split)
  (:documentation "--- Day 8: Playground ---"))

(in-package :aoc-25/day08)

(defstruct (point-3d (:conc-name p3-))
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum))

(defun squared-distance (a b)
  (let ((dx (- (p3-x a) (p3-x b)))
        (dy (- (p3-y a) (p3-y b)))
        (dz (- (p3-z a) (p3-z b))))
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defun parse-junctions (lines)
  (loop for line in lines
        for numbers = (mapcar #'parse-integer (split "," line))
        collect (make-point-3d :x (first numbers)
                               :y (second numbers)
                               :z (third numbers))))

(defun generate-sorted-pairs (junctions)
  (let ((pairs (loop for (head . tails) on junctions
                     nconc (loop for tail in tails
                                 collect (cons (cons head tail)
                                               (squared-distance head tail))))))
    (mapcar #'car (sort pairs #'< :key #'cdr))))

(defun chain-size-list (junctions pairs)
  (let ((adjacency (make-hash-table :test #'eq))
        (visited (make-hash-table :test #'eq))
        (sizes '()))
    (dolist (pair pairs)
      (let ((a (car pair))
            (b (cdr pair)))
        (push b (gethash a adjacency))
        (push a (gethash b adjacency))))
    (dolist (junction junctions)
      (unless (gethash junction visited)
        (let ((stack (list junction))
              (current-size 0))
          (loop while stack
                for current = (pop stack)
                unless (gethash current visited)
                  do (setf (gethash current visited) t)
                     (incf current-size)
                     (dolist (neighbour (gethash current adjacency))
                       (unless (gethash neighbour visited)
                         (push neighbour stack))))
          (push current-size sizes))))
    (sort sizes #'>)))

;; => 72150
(defun part1 (&optional (junctions (parse-junctions (read-input :format :lines)))
                (connections 1000))
  (let* ((closest-pairs (generate-sorted-pairs junctions))
         (pairs (subseq closest-pairs 0 connections))
         (sizes (chain-size-list junctions pairs)))
    (* (first sizes) (second sizes) (third sizes))))

;; => 3926518899
(defun part2 (&optional (junctions (parse-junctions (read-input :format :lines))))
  (let* ((n (length junctions))
         (closest-pairs (generate-sorted-pairs junctions))
         (start 0)
         (end (1- (length closest-pairs)))
         (target end))
    (loop while (<= start end)
          for mid = (floor (+ start end) 2)
          for sizes = (chain-size-list junctions (subseq closest-pairs 0 (1+ mid)))
          do (if (= (first sizes) n)
                 (setf target mid
                       end (1- mid))
                 (setf start (1+ mid))))
    (let ((target-pair (nth target closest-pairs)))
      (* (p3-x (car target-pair))
         (p3-x (cdr target-pair))))))

;;; Using Disjoint-set-unions to solve this.

(defstruct dsu
  (parent #() :type simple-vector)
  (size #() :type simple-vector)
  (components 0 :type fixnum))

(defun create-dsu (n)
  (let ((parent (make-array n))
        (size (make-array n :initial-element 1)))
    (dotimes (i n)
      (setf (aref parent i) i))
    (make-dsu :parent parent :size size :components n)))

(defun dsu-find (dsu x)
  "Find the root representative of X."
  (let* ((parent (dsu-parent dsu))
         (p (aref parent x)))
    (if (= p x)
        x
        (setf (aref parent x) (dsu-find dsu p)))))

(defun dsu-union (dsu x y)
  "Merge the sets containing X and Y. Return T if a merge occurred."
  (let ((root-x (dsu-find dsu x))
        (root-y (dsu-find dsu y)))
    (when (/= root-x root-y)
      (let ((parent (dsu-parent dsu))
            (size (dsu-size dsu)))
        ;; Attach the small tree under the larger tree
        (when (< (aref size root-x) (aref size root-y))
          (rotatef root-x root-y))
        (setf (aref parent root-y) root-x)
        (incf (aref size root-x) (aref size root-y))
        (decf (dsu-components dsu))
        t))))

(defun generate-indexed-pairs (junctions-vec)
  "Return a sorted list of ((idx-a . idx-b) . dist-sq). from JUNCTIONS."
  (let* ((n (length junctions-vec))
         (pairs (loop for i from 0 below (1- n)
                      for a = (aref junctions-vec i)
                      nconc (loop for j from (1+ i) below n
                                  for b  = (aref junctions-vec j)
                                  collect (cons (cons i j)
                                                (squared-distance a b))))))
    (mapcar #'car (sort pairs #'< :key #'cdr))))

;; 72150
(defun part1-dsu (&optional (junctions (parse-junctions (read-input :format :lines))))
  (let* ((junctions-vec (coerce junctions 'vector))
         (sorted-pairs (generate-indexed-pairs junctions-vec))
         (n (length junctions))
         (dsu (create-dsu n)))
    (loop for (i . j) in sorted-pairs
          repeat 1000
          do (dsu-union dsu i j))
    (let ((sizes (loop for i from 0 below n
                       when (= (aref (dsu-parent dsu) i) i)
                         collect (aref (dsu-size dsu) i))))
      (setf sizes (sort sizes #'>))
      (* (first sizes) (second sizes) (third sizes)))))

(defun part2-dsu (&optional (junctions (parse-junctions (read-input :format :lines))))
  (let* ((junctions-vec (coerce junctions 'vector))
         (sorted-pairs (generate-indexed-pairs junctions-vec))
         (dsu (create-dsu (length junctions-vec))))
    (loop for (i . j) in sorted-pairs
          do (dsu-union dsu i j)
          when (= (dsu-components dsu) 1)
            return (* (p3-x (aref junctions-vec i))
                      (p3-x (aref junctions-vec j))))))

(defparameter *example* (parse-junctions '("162,817,812"
                                           "57,618,57"
                                           "906,360,560"
                                           "592,479,940"
                                           "352,342,300"
                                           "466,668,158"
                                           "542,29,236"
                                           "431,825,988"
                                           "739,650,466"
                                           "52,470,668"
                                           "216,146,977"
                                           "819,987,18"
                                           "117,168,530"
                                           "805,96,715"
                                           "346,949,466"
                                           "970,615,88"
                                           "941,993,340"
                                           "862,61,35"
                                           "984,92,344"
                                           "425,690,689")))
