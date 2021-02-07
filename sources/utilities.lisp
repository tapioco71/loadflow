;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; utilities.lisp
;;;;
;;;; Copyright (c) 2020 Angelo Rossi
;;;;
;; This file is part of Loadflow (LF).
;;
;;    Loadflow (LF) is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    Loadflow (LF) is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with Loadflow (LF).  If not, see <http://www.gnu.org/licenses/>.

(in-package #:loadflow)

;; Parameters.

;; Structures.

(defstruct (line-struct (:constructor make-line))
  (name (symbol-name (gensym "line-")) :type (or symbol keyword string (unsigned-byte 64)))
  (conductor-length 0d0 :type real)
  (conductor-resistivity 0d0 :type real)
  (conductor-formation 1 :type (integer 1))
  (insulation-permittivity 1d0 :type real)
  (insulation-permeability 1d0 :type real)
  (conductor-positions nil :type list)
  (conductor-cross-sections nil :type list))


;; Funtions

(defun make-line (&rest parameters &key
                                     (name (symbol-name (gensym "line-")) name-p)
                                     (conductor-length 1d0 conductor-length-p)
                                     (conductor-resistivity 1.78d-8 conductor-resistivity-p)
                                     (conductor-formation 1 conductor-formation-p)
                                     (insulation-permittivity 1d0 insulation-permittivity-p)
                                     (insulation-permeability 1d0 insulation-permeability-p)
                                     (conductor-positions nil conductor-positions-p)
                                     (conductor-cross-sections nil conductor-cross-sections-p))
  (declare (ignorable parameters
                      name
                      conductor-length
                      conductor-resistivity
                      conductor-formation
                      insulation-permittivity
                      insulation-permeability
                      conductor-positions
                      conductor-cross-sections))
  (when name-p
    (check-type name (or symbol keyword string (unsigned-byte 64))))
  (when conductor-length-p
    (check-type conductor-length real))
  (when conductor-resistivity
    (check-type conductor-resistivity real))
  (when conductor-formation-p
    (check-type conductor-formation (integer 1)))
  (when insulation-permittivity-p
    (check-type insulation-permittivity real))
  (when insulation-permeability-p
    (check-type insulation-permeability real))
  (when conductor-positions-p
    (check-type conductor-positions list)
    (dolist (c conductor-positions)
      (check-type c grid:foreign-array)))
  (when conductor-cross-sections-p
    (check-type conductor-cross-sections list)
    (dolist (c conductor-cross-sections)
      (check-type c real)))
  (assert (= (length conductor-cross-sections)
             (length conductor-positions)))
  (let ((object (allocate-instance (find-class 'line-struct))))
    (setf (line-struct-name object) name
          (line-struct-conductor-length object) conductor-length
          (line-struct-conductor-resistivity object) conductor-resistivity
          (line-struct-conductor-formation object) conductor-formation
          (line-struct-insulation-permittivity object) insulation-permittivity
          (line-struct-insulation-permeability object) insulation-permeability
          (line-struct-conductor-positions object) conductor-positions
          (line-struct-conductor-cross-sections object) conductor-cross-sections)
    object))

(defun make-line-pi-model (&rest parameters &key
                                           (line nil line-p)
                                           (frequency 50d0 frequency-p))
  "Create a line model."
  (declare (ignorable parameters line frequency))
  (when line-p
    (check-type line line-struct))
  (when frequency-p
    (check-type frequency real))
  (let* ((a-matrix (grid:make-foreign-array 'double-float
                                            :dimensions (list (length (line-struct-conductor-positions line))
                                                              (length (line-struct-conductor-positions line)))
                                            :initial-element 0d0))
         (m-matrix (grid:make-foreign-array 'double-float
                                            :dimensions (list (length (line-struct-conductor-positions line))
                                                              (length (line-struct-conductor-positions line)))
                                            :initial-element 0d0))
         (d-matrix (grid:make-foreign-array 'double-float
                                            :dimensions (list (length (line-struct-conductor-positions line))
                                                              (length (line-struct-conductor-positions line)))
                                            :initial-element 0d0))
         (image-d-matrix (grid:make-foreign-array 'double-float
                                                  :dimensions (list (length (line-struct-conductor-positions line))
                                                                    (length (line-struct-conductor-positions line)))
                                                  :initial-element 0d0))
         (c-matrix (grid:make-foreign-array 'double-float
                                            :dimensions (list (length (line-struct-conductor-positions line))
                                                              (length (line-struct-conductor-positions line)))
                                            :initial-element 0d0))
         (l-vector (grid:make-foreign-array 'double-float
                                            :dimensions (length (line-struct-conductor-positions line))
                                            :initial-element 0d0))
         (r-vector (grid:make-foreign-array 'double-float
                                            :dimensions (length (line-struct-conductor-positions line))
                                            :initial-element 0d0))
         (k #(0.779d0 0.726d0 0.758d0 0.768d0 0.772d0 0.774d0 0.776d0 0.809d0 0.826d0 0.810d0 1d0))
         (beta (/ (* gsll:+mksa-vacuum-permeability+
                     (line-struct-conductor-length line)
                     (line-struct-insulation-permeability line))
                  (* 2d0 pi)))
         (rs nil)
         (cs nil)
         (ls nil))
    (labels ((alpha (n i)
               (exp (- (/ (* #c(0d0 1d0) 2d0 pi i) n)))))
      (loop
        with voltage = 0d0
        for i from 0 below (length (line-struct-conductor-positions line))
        do
           (loop
             initially (setq voltage 0d0)
             for j from 0 below (length (line-struct-conductor-positions line))
             do
                (setf (grid:gref d-matrix i j) (grid:norm (gsl:elt- (grid:copy (nth i (line-struct-conductor-positions line)))
                                                                    (grid:copy (nth j (line-struct-conductor-positions line)))))
                      (grid:gref image-d-matrix i j) (grid:norm (gsl:elt- (grid:copy (nth i (line-struct-conductor-positions line)))
                                                                          (gsll:elt* (grid:copy-to #(1d0 -1d0) 'grid:foreign-array)
                                                                                     (grid:copy (nth j (line-struct-conductor-positions line)))))))
                (if (= i j)
                    (progn
                      (setf (grid:gref m-matrix i i) (- (* beta (log (* (aref k (1- (line-struct-conductor-formation line)))
                                                                        (sqrt (/ (nth i (line-struct-conductor-cross-sections line))
                                                                                 pi))))))
                            (grid:gref a-matrix i i) (* (/ (* 2d0
                                                              pi
                                                              gsll:+mksa-vacuum-permittivity+
                                                              (line-struct-insulation-permittivity line)))
                                                        (log (/ (grid:gref image-d-matrix i i)
                                                                (sqrt (/ (nth i (line-struct-conductor-cross-sections line))
                                                                         pi))))))
                      (incf voltage (* #c(0d0 -1d0) (grid:gref m-matrix i i))))
                    (progn
                      (setf (grid:gref m-matrix i j) (- (* beta (log (grid:gref d-matrix i j))))
                            (grid:gref a-matrix i j) (* (/ (* 2d0
                                                              pi
                                                              gsll:+mksa-vacuum-permittivity+
                                                              (line-struct-insulation-permittivity line)))
                                                        (log (/ (grid:gref image-d-matrix i j)
                                                                (grid:gref d-matrix i j)))))
                      (incf voltage (* #c(0d0 -1d0)
                                       (alpha (length (line-struct-conductor-positions line))
                                              (- j i))
                                       (grid:gref m-matrix i j))))))
           (setf (grid:gref l-vector i) (abs (imagpart voltage))
                 (grid:gref r-vector i) (/ (* (line-struct-conductor-length line)
                                              (line-struct-conductor-resistivity line))
                                           (nth i (line-struct-conductor-cross-sections line)))))
      (multiple-value-bind (lu-matrix permutation signum)
          (gsll:lu-decomposition (grid:copy a-matrix))
        (declare (ignore signum))
        (let ((gamma-matrix (gsll:lu-invert (grid:copy lu-matrix) permutation)))
          (when gamma-matrix
            (loop
              for i from 0 below (length (line-struct-conductor-positions line))
              do
                 (loop
                   for j from 0 below (length (line-struct-conductor-positions line))
                   if (= i j)
                     sum (* (grid:gref gamma-matrix i i)
                            (alpha (length (line-struct-conductor-positions line))
                                   j))
                       into cp
                   else
                     do
                        (setf (grid:gref c-matrix i j) (* (abs (grid:gref gamma-matrix i j))
                                                          (line-struct-conductor-length line)))
                   finally (setf (grid:gref c-matrix i i) (* (abs cp)
                                                             (line-struct-conductor-length line))))))
          (setq rs (/ (loop
                        for i from 0 below (grid:dim0 r-vector)
                        sum (grid:gref r-vector i))
                      (grid:dim0 r-vector))
                cs (/ (loop
                        for i from 0 below (grid:dim0 c-matrix)
                        sum (loop
                              for j from 0 below (grid:dim1 c-matrix)
                              sum (grid:gref c-matrix i j)))
                      (grid:dim0 c-matrix))
                ls (/ (loop
                        for i from 0 below (grid:dim0 l-vector)
                        sum (grid:gref l-vector i))
                      (grid:dim0 l-vector)))
          (values d-matrix
                  image-d-matrix
                  m-matrix
                  a-matrix
                  r-vector
                  l-vector
                  c-matrix
                  (list (make-bipole :kind :capacitance
                                     :nodes (list 0 1)
                                     :model-function #'(lambda ())
                                     :model-parameters (list :value (/ cs 2d0)))
                        (make-bipole :kind :impedance
                                     :nodes (list 1 2)
                                     :model-function #'(lambda ())
                                     :model-parameters (list :value (complex rs (* 2d0 pi frequency ls))))
                        (make-bipole :kind :capacitance
                                     :nodes (list 0 2)
                                     :model-function #'(lambda ())
                                     :model-parameters (list :value (/ cs 2d0))))))))))


;; Utilities

(defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst))) (rest remain))))))

(defun combinations (a)
  (let ((return-value nil))
    (loop
      for i from 0 below (length a)
      do
         (loop
           for j from i below (length a)
           do
              (setq return-value (append return-value (list (list (nth i a) (nth j a)))))))
    return-value))

(defun rotate (a n)
  (let ((x nil))
    (setq x (pop a))
    (when x
      (append a (list x)))))

(defun matrix-trace (a)
  (check-type a grid:foreign-array)
  (assert (= (length (grid:dimensions a)) 2))
  (when (= (grid:dim0 a)
           (grid:dim1 a))
    (loop
      for i from 0 below (grid:dim0 a)
      sum (grid:gref a i i) into tr
      finally (return tr))))

;; Macros.
