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

(defstruct (line-struct (:constructor make-cable))
  (name (symbol-name (gensym "cable-")) :type (or symbol keyword string (unsigned-byte 64)))
  (conductor-length 0d0 :type real)
  (conductor-resistivity 0d0 :type real)
  (conductor-formation 1 :type (integer 1))
  (insulation-permittivity 1d0 :type real)
  (insulation-permeability 1d0 :type real)
  (conductor-positions nil :type list)
  (conductor-cross-sections nil :type list))


;; Funtions

(defun make-line (&rest parameters &key
                                     (name (symbol-name (gensym "cable-")) name-p)
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
  (let ((object (allocate-instance (find-class 'cable-struct))))
    (setf (cable-struct-name object) name
          (cable-struct-conductor-length object) conductor-length
          (cable-struct-conductor-resistivity object) conductor-resistivity
          (cable-struct-conductor-formation object) conductor-formation
          (cable-struct-insulation-permittivity object) insulation-permittivity
          (cable-struct-insulation-permeability object) insulation-permeability
          (cable-struct-conductor-positions object) conductor-positions
          (cable-struct-conductor-cross-sections object) conductor-cross-sections)
    object))

                         
(defun make-line-model (&rest parameters &key
                                            (cable nil cable-p))
  "Create a cable model."
  (declare (ignorable parameters cable))
  (when cable-p
    (check-type cable cable-struct))
  (let* ((a-matrix (grid:make-foreign-array 'double-float
                                            :dimensions (list (length (cable-struct-conductor-positions cable))
                                                              (length (cable-struct-conductor-positions cable)))
                                            :initial-element 0d0))
         (m-matrix (grid:make-foreign-array 'double-float
                                            :dimensions (list (length (cable-struct-conductor-positions cable))
                                                              (length (cable-struct-conductor-positions cable)))
                                            :initial-element 0d0))
         (d-matrix (grid:make-foreign-array 'double-float
                                            :dimensions (list (length (cable-struct-conductor-positions cable))
                                                              (length (cable-struct-conductor-positions cable)))
                                            :initial-element 0d0))
         (image-d-matrix (grid:make-foreign-array 'double-float
                                                  :dimensions (list (length (cable-struct-conductor-positions cable))
                                                                    (length (cable-struct-conductor-positions cable)))
                                                  :initial-element 0d0))
         (c-matrix (grid:make-foreign-array 'double-float
                                            :dimensions (list (length (cable-struct-conductor-positions cable))
                                                              (length (cable-struct-conductor-positions cable)))
                                            :initial-element 0d0))
         (l-vector (grid:make-foreign-array 'double-float
                                            :dimensions (length (cable-struct-conductor-positions cable))
                                            :initial-element 0d0))
         (r-vector (grid:make-foreign-array 'double-float
                                            :dimensions (length (cable-struct-conductor-positions cable))
                                            :initial-element 0d0))
         (k #(0.779d0 0.726d0 0.758d0 0.768d0 0.772d0 0.774d0 0.776d0 0.809d0 0.826d0 0.810d0 1d0))
         (beta (/ (* gsll:+mksa-vacuum-permeability+
                     (cable-struct-conductor-length cable)
                     (cable-struct-insulation-permeability cable))
                  (* 2d0 pi))))
    (labels ((alpha (n i)
               (exp (- (/ (* #c(0d0 1d0) 2d0 pi i) n)))))
      (loop
        with voltage = 0d0
        for i from 0 below (length (cable-struct-conductor-positions cable))
        do
           (loop
             initially (setq voltage 0d0)
             for j from 0 below (length (cable-struct-conductor-positions cable))
             do
                (setf (grid:gref d-matrix i j) (grid:norm (gsl:elt- (grid:copy (nth i (cable-struct-conductor-positions cable)))
                                                                    (grid:copy (nth j (cable-struct-conductor-positions cable)))))
                      (grid:gref image-d-matrix i j) (grid:norm (gsl:elt- (grid:copy (nth i (cable-struct-conductor-positions cable)))
                                                                          (gsll:elt* (grid:copy-to #(1d0 -1d0) 'grid:foreign-array)
                                                                                     (grid:copy (nth j (cable-struct-conductor-positions cable)))))))
                (if (= i j)
                    (progn
                      (setf (grid:gref m-matrix i i) (- (* beta (log (* (aref k (1- (cable-struct-conductor-formation cable)))
                                                                        (sqrt (/ (nth i (cable-struct-conductor-cross-sections cable))
                                                                                 pi))))))
                            (grid:gref a-matrix i i) (* (/ (* 2d0
                                                              pi
                                                              gsll:+mksa-vacuum-permittivity+
                                                              (cable-struct-insulation-permittivity cable)))
                                                        (log (/ (grid:gref image-d-matrix i i)
                                                                (sqrt (/ (nth i (cable-struct-conductor-cross-sections cable))
                                                                         pi))))))
                      (incf voltage (* #c(0d0 -1d0) (grid:gref m-matrix i i))))
                    (progn
                      (setf (grid:gref m-matrix i j) (- (* beta (log (grid:gref d-matrix i j))))
                            (grid:gref a-matrix i j) (* (/ (* 2d0
                                                              pi
                                                              gsll:+mksa-vacuum-permittivity+
                                                              (cable-struct-insulation-permittivity cable)))
                                                        (log (/ (grid:gref image-d-matrix i j)
                                                                (grid:gref d-matrix i j)))))
                      (incf voltage (* #c(0d0 -1d0)
                                       (alpha (length (cable-struct-conductor-positions cable))
                                              (- j i))
                                       (grid:gref m-matrix i j))))))
           (setf (grid:gref l-vector i) (abs (imagpart voltage))
                 (grid:gref r-vector i) (/ (* (cable-struct-conductor-length cable)
                                              (cable-struct-conductor-resistivity cable))
                                           (nth i (cable-struct-conductor-cross-sections cable)))))
      (multiple-value-bind (lu-matrix permutation signum)
          (gsll:lu-decomposition (grid:copy a-matrix))
        (declare (ignore signum))
        (let ((gamma-matrix (gsll:lu-invert (grid:copy lu-matrix) permutation)))
          (when gamma-matrix
            (loop
              for i from 0 below (length (cable-struct-conductor-positions cable))
              do
                 (loop
                   for j from 0 below (length (cable-struct-conductor-positions cable))
                   if (= i j)
                     sum (* (grid:gref gamma-matrix i i)
                            (alpha (length (cable-struct-conductor-positions cable))
                                   j))
                       into cp
                   else
                     do
                        (setf (grid:gref c-matrix i j) (* (abs (grid:gref gamma-matrix i j))
                                                          (cable-struct-conductor-length cable)))
                   finally (setf (grid:gref c-matrix i i) (* (abs cp)
                                                             (cable-struct-conductor-length cable))))))
          (values d-matrix
                  image-d-matrix
                  m-matrix
                  a-matrix
                  r-vector
                  l-vector
                  c-matrix))))))
