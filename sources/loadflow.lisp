;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; loadflow.lisp
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

;; Functions.

(defun assign-initial-solution (&rest parameters &key
                                                   (problem nil problem-p)
                                                   (nodes-table nil nodes-table-p)
                                                   (voltages-vector nil voltages-vector-p)
                                                   (thetas-vector nil thetas-vector-p)
                                                   (verbose nil verbose-p))
  (declare (ignorable parameters
                      problem
                      nodes-table
                      voltages-vector
                      thetas-vector
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when nodes-table-p
    (check-type nodes-table hash-table))
  (when voltages-vector-p
    (check-type voltages-vector grid:foreign-array))
  (when thetas-vector-p
    (check-type thetas-vector grid:foreign-array))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (and verbose-p
             (> verbose 5))
    (printout :message "entering assign-initial-solution().~&"))
  (let ((ok? nil))
    (with-hash-table-iterator (nodes-iterator nodes-table)
      (loop
         named nodes-loop
         initially (setq ok? t)
         do
           (multiple-value-bind (more? node-name node-number)
               (nodes-iterator)
             (if more?
                 (let ((node (first (select-element :predicate (where :name node-name)
                                                    :network (problem-struct-network problem)))))
                   (if node
                       (case (node-struct-kind node)
                         (:generation
                          (case (bond-struct-kind (node-struct-bond node))
                            (:p-v ;; Q and theta unknown - |V| value is in bond-struct :voltage-magnitude and initial value for theta is in :data
                             (when (integerp verbose)
                               (when (> verbose 10)
                                 (printout :message
                                           "node ~a (~a) applying voltage magnitude bond as initial value: ~s.~&"
                                           node-name
                                           node-number
                                           (node-struct-bond node))
                                 (printout :message
                                           "node ~a (~a) applying voltage phase initial value: ~s.~&"
                                           node-name
                                           node-number
                                           (node-struct-data node))))
                             (setf (grid:gref voltages-vector node-number) (bond-struct-voltage-magnitude (node-struct-bond node))
                                   (grid:gref thetas-vector node-number) (getf (node-struct-data node) :voltage-phase)))
                            (:q-v ;; P and theta unknown - |V| value is in bond-struct :voltage-magnitude and initial value for theta is in :data
                             (when (integerp verbose)
                               (when (> verbose 10)
                                 (printout :message
                                           "node ~a (~a) applying voltage magnitude bond as initial value: ~s.~&"
                                           node-name
                                           node-number
                                           (node-struct-bond node))
                                 (printout :message
                                           "node ~a (~a) applying voltage phase initial value: ~s.~&"
                                           node-name
                                           node-number
                                           (node-struct-data node))))
                             (setf (grid:gref voltages-vector node-number) (bond-struct-voltage-magnitude (node-struct-bond node))
                                   (grid:gref thetas-vector node-number) (getf (node-struct-data node) :voltage-phase)))
                            (:p-q ;; |V| and theta unknown - initial values for |V| and theta are in :data
                             (when (integerp verbose)
                               (when (> verbose 10)
                                 (printout :message
                                           "node ~a (~a) applying voltage phasor initial value: ~s.~&"
                                           node-name
                                           node-number
                                           (node-struct-data node))))
                             (setf (grid:gref voltages-vector node-number) (getf (node-struct-data node) :voltage-magnitude)
                                   (grid:gref thetas-vector node-number) (getf (node-struct-data node) :voltage-phase)))
                            (:v-theta ;; P and Q unknown - |V| and arg(V) values are in bond-struct :voltage-magnitude and :voltage-phase
                             (when (integerp verbose)
                               (when (> verbose 10)
                                 (printout :message
                                           "node ~a (~a) applying voltage phasor bond as initial value: ~s.~&"
                                           node-name
                                           node-number
                                           (node-struct-bond node))))
                             (setf (grid:gref voltages-vector node-number) (bond-struct-voltage-magnitude (node-struct-bond node))
                                   (grid:gref thetas-vector node-number) (bond-struct-voltage-phase (node-struct-bond node))))))
                         (:load
                          (case (bond-struct-kind (node-struct-bond node))
                            (:p-q ;; |e| and theta unknown - initial values for |e| and theta are in :data
                             (when (integerp verbose)
                               (when (> verbose 10)
                                 (printout :message
                                           "node ~a (~a) applying voltage phasor initial value: ~s.~&"
                                           node-name
                                           node-number
                                           (node-struct-data node))))
                             (setf (grid:gref voltages-vector node-number) (getf (node-struct-data node) :voltage-magnitude)
                                   (grid:gref thetas-vector node-number) (getf (node-struct-data node) :voltage-phase)))
                            (:v=f(Q)
                              (setf (grid:gref voltages-vector node-number) (getf (node-struct-data node) :voltage-magnitude)
                                    (grid:gref thetas-vector node-number) (getf (node-struct-data node) :voltage-phase)))))
                         (:interconnection
                          (when (integerp verbose)
                            (when (> verbose 10)
                              (printout :message
                                        "node ~a (~a) applying voltage phasor initial value: ~s.~&"
                                        node-name
                                        node-number
                                        (node-struct-data node))))
                          (setf (grid:gref voltages-vector node-number) (getf (node-struct-data node) :voltage-magnitude)
                                (grid:gref thetas-vector node-number) (getf (node-struct-data node) :voltage-phase))))
                       (progn
                         (setq ok? nil)
                         (when (integerp verbose)
                           (when (> verbose 10)
                             (printout :error "no node ~a found.~&" node-name)))
                         (return-from nodes-loop))))
                 (return-from nodes-loop)))))
    (when (integerp verbose)
      (when (> verbose 10)
        (printout :message "initial voltages vector = ~s.~&" voltages-vector)
        (printout :message "initial thetas vector = ~s.~&" thetas-vector)))
    (when (integerp verbose)
      (when (> verbose 5)
        (printout :message "exiting assign-initial-solution().~%~%")))
    (values voltages-vector
            thetas-vector
            ok?)))

(defun calculate-pq-residuals (&rest parameters &key
                                                  (problem nil problem-p)
                                                  (nodes-table nil nodes-table-p)
                                                  (elements-table nil elements-table-p)
                                                  (admittances-matrix nil admittances-matrix-p)
                                                  (voltages-vector nil voltages-vector-p)
                                                  (thetas-vector nil thetas-vector-p)
                                                  (p-vector nil p-vector-p)
                                                  (q-vector nil q-vector-p)
                                                  (delta-p-vector nil delta-p-vector-p)
                                                  (delta-q-vector nil delta-q-vector-p)
                                                  (verbose nil verbose-p))
  (declare (ignorable parameters
                      problem
                      nodes-table
                      elements-table
                      admittances-matrix
                      voltages-vector
                      thetas-vector
                      p-vector
                      q-vector
                      delta-p-vector
                      delta-q-vector
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when nodes-table-p
    (check-type nodes-table hash-table))
  (when elements-table-p
    (check-type elements-table hash-table))
  (when admittances-matrix-p
    (check-type admittances-matrix grid:foreign-array))
  (when voltages-vector-p
    (check-type voltages-vector grid:foreign-array))
  (when thetas-vector-p
    (check-type thetas-vector grid:foreign-array))
  (when p-vector-p
    (check-type p-vector grid:foreign-array))
  (when q-vector-p
    (check-type q-vector grid:foreign-array))
  (when delta-p-vector-p
    (check-type delta-p-vector grid:foreign-array))
  (when delta-q-vector-p
    (check-type delta-q-vector grid:foreign-array))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (integerp verbose)
    (when (> verbose 5)
      (printout :message "entering calculate-pq-residuals().~&")))
  (let ((ok? nil))
    (labels ((nodal-active-power (k v theta y)
               (* (abs (grid:gref v k))
                  (loop
                     for i from 0 below (grid:dim0 v)
                     sum (* (abs (grid:gref v i))
                            (abs (grid:gref y k i))
                            (cos (- (grid:gref theta k)
                                    (grid:gref theta i)
                                    (phase (grid:gref y k i))))))))
             (nodal-reactive-power (k v theta y)
               (* (abs (grid:gref v k))
                  (loop
                     for i from 0 below (grid:dim0 v)
                     sum (* (abs (grid:gref v i))
                            (abs (grid:gref y k i))
                            (sin (- (grid:gref theta k)
                                    (grid:gref theta i)
                                    (phase (grid:gref y k i)))))))))
      (with-hash-table-iterator (nodes-iterator nodes-table)
        (loop
           named nodes-loop
           initially (setq ok? t)
           do
             (multiple-value-bind (more? node-name node-number)
                 (nodes-iterator)
               (if more?
                   (let ((node (first (select-element :predicate (where :name node-name)
                                                      :network (problem-struct-network problem)))))
                     (if node
                         (case (node-struct-kind node)
                           (:generation
                            (case (bond-struct-kind (node-struct-bond node))
                              (:p-v
                               (setf (grid:gref p-vector node-number) (nodal-active-power node-number
                                                                                          voltages-vector
                                                                                          thetas-vector
                                                                                          admittances-matrix)
                                     (grid:gref q-vector node-number) (nodal-reactive-power node-number
                                                                                            voltages-vector
                                                                                            thetas-vector
                                                                                            admittances-matrix)
                                     (grid:gref delta-p-vector node-number) (- (bond-struct-active-power (node-struct-bond node))
                                                                               (grid:gref p-vector node-number))
                                     (grid:gref delta-q-vector node-number) 0d0))
                              (:q-v
                               (setf (grid:gref p-vector node-number) (nodal-active-power node-number
                                                                                          voltages-vector
                                                                                          thetas-vector
                                                                                          admittances-matrix)
                                     (grid:gref q-vector node-number) (nodal-reactive-power node-number
                                                                                            voltages-vector
                                                                                            thetas-vector
                                                                                            admittances-matrix)
                                     (grid:gref delta-p-vector node-number) 0d0
                                     (grid:gref delta-q-vector node-number) (- (bond-struct-reactive-power (node-struct-bond node))
                                                                               (grid:gref q-vector node-number))))
                              (:p-q
                               (setf (grid:gref p-vector node-number) (nodal-active-power node-number
                                                                                          voltages-vector
                                                                                          thetas-vector
                                                                                          admittances-matrix)

                                     (grid:gref q-vector node-number) (nodal-reactive-power node-number
                                                                                            voltages-vector
                                                                                            thetas-vector
                                                                                            admittances-matrix)
                                     (grid:gref delta-p-vector node-number) (- (bond-struct-active-power (node-struct-bond node))
                                                                               (grid:gref p-vector node-number))
                                     (grid:gref delta-q-vector node-number) (- (bond-struct-reactive-power (node-struct-bond node))
                                                                               (grid:gref q-vector node-number))))
                              (:v-theta
                               (setf (grid:gref p-vector node-number) (nodal-active-power node-number
                                                                                          voltages-vector
                                                                                          thetas-vector
                                                                                          admittances-matrix)
                                     (grid:gref q-vector node-number) (nodal-reactive-power node-number
                                                                                            voltages-vector
                                                                                            thetas-vector
                                                                                            admittances-matrix)
                                     (grid:gref delta-p-vector node-number) 0d0
                                     (grid:gref delta-q-vector node-number) 0d0))))
                           (:load
                            (case (bond-struct-kind (node-struct-bond node))
                              (:v=f(Q)
                                ())
                              (:p-q
                               (setf (grid:gref p-vector node-number) (nodal-active-power node-number
                                                                                          voltages-vector
                                                                                          thetas-vector
                                                                                          admittances-matrix)
                                     (grid:gref q-vector node-number) (nodal-reactive-power node-number
                                                                                            voltages-vector
                                                                                            thetas-vector
                                                                                            admittances-matrix)
                                     (grid:gref delta-p-vector node-number) (- (- (bond-struct-active-power (node-struct-bond node)))
                                                                               (grid:gref p-vector node-number))
                                     (grid:gref delta-q-vector node-number) (- (- (bond-struct-reactive-power (node-struct-bond node)))
                                                                               (grid:gref q-vector node-number))))))
                           (:interconnection
                            (setf (grid:gref p-vector node-number) (nodal-active-power node-number
                                                                                       voltages-vector
                                                                                       thetas-vector
                                                                                       admittances-matrix)
                                  (grid:gref q-vector node-number) (nodal-reactive-power node-number
                                                                                         voltages-vector
                                                                                         thetas-vector
                                                                                         admittances-matrix)
                                  (grid:gref delta-p-vector node-number) (- (grid:gref p-vector node-number))
                                  (grid:gref delta-q-vector node-number) (- (grid:gref q-vector node-number)))))
                         (progn
                           (setq ok? nil)
                           (when (integerp verbose)
                             (when (> verbose 10)
                               (printout :error "no node named ~a in the network.~&" node-name)))
                           (return-from nodes-loop))))
                   (return-from nodes-loop)))))
      (when (integerp verbose)
        (when (> verbose 10)
          (printout :message "P = ~a.~&" p-vector)
          (printout :message "Q = ~a.~&" q-vector)
          (printout :message "delta-P = ~s.~&" delta-p-vector)
          (printout :message "delta-Q = ~s.~&" delta-q-vector)))
      (when (integerp verbose)
        (when (> verbose 5)
          (printout :message "exiting calculate-pq-residuals().~%~%")))
      (values p-vector
              q-vector
              delta-p-vector
              delta-q-vector
              ok?))))

(defun calculate-jacobian (&rest parameters &key
                                              (dp/dtheta-matrix nil dp/dtheta-matrix-p)
                                              (dq/dtheta-matrix nil dq/dtheta-matrix-p)
                                              (dp/dv-matrix nil dp/dv-matrix-p)
                                              (dq/dv-matrix nil dq/dv-matrix-p)
                                              (admittances-matrix nil admittances-matrix-p)
                                              (voltages-vector nil voltages-vector-p)
                                              (thetas-vector nil thetas-vector-p)
                                              (verbose nil verbose-p))
  (declare (ignorable parameters
                      dp/dtheta-matrix
                      dq/dtheta-matrix
                      dp/dv-matrix
                      dq/dv-matrix
                      admittances-matrix
                      voltages-vector
                      thetas-vector
                      verbose))
  (when dp/dtheta-matrix-p
    (check-type dp/dtheta-matrix grid:foreign-array))
  (when dq/dtheta-matrix-p
    (check-type dq/dtheta-matrix grid:foreign-array))
  (when dp/dv-matrix-p
    (check-type dp/dv-matrix grid:foreign-array))
  (when dq/dv-matrix-p
    (check-type dq/dv-matrix grid:foreign-array))
  (when admittances-matrix-p
    (check-type admittances-matrix grid:foreign-array))
  (when voltages-vector-p
    (check-type voltages-vector grid:foreign-array))
  (when thetas-vector-p
    (check-type thetas-vector grid:foreign-array))
  (assert (and (= (grid:dim0 admittances-matrix)
                  (grid:dim0 voltages-vector))
               (= (grid:dim1 admittances-matrix)
                  (grid:dim0 thetas-vector))))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (integerp verbose)
    (when (> verbose 5)
      (printout :message "entering calculate-jacobian().~&")))
  (let ((jacobian-threads nil)
        (jacobian-locks nil)
        (jacobian-matrix nil)
        (ok? nil))
    (labels ((dp/dtheta (v theta y k h)
               (if (= k h)
                   (- (* (abs (grid:gref v k))
                         (loop
                            with admittance = nil
                            for i from 0 below (grid:dim0 v)
                            when (/= i k)
                            sum (* (abs (grid:gref v i))
                                   (abs (grid:gref y k i))
                                   (sin (- (grid:gref theta k)
                                           (grid:gref theta i)
                                           (phase (grid:gref y k i))))))))
                   (* (abs (grid:gref v k))
                      (abs (grid:gref v h))
                      (abs (grid:gref y k h))
                      (sin (- (grid:gref theta k)
                              (grid:gref theta h)
                              (phase (grid:gref y k h)))))))
             (dq/dtheta (v theta y k h)
               (if (= k h)
                   (* (abs (grid:gref v k))
                      (loop
                         for i from 0 below (grid:dim0 v)
                         when (/= i k)
                         sum (* (abs (grid:gref v i))
                                (abs (grid:gref y k i))
                                (cos (- (grid:gref theta k)
                                        (grid:gref theta i)
                                        (phase (grid:gref y k i)))))))
                   (- (* (abs (grid:gref v k))
                         (abs (grid:gref v h))
                         (abs (grid:gref y k h))
                         (cos (- (grid:gref v k)
                                 (grid:gref v h)
                                 (phase (grid:gref y k h))))))))
             (dp/dv (v theta y k h)
               (if (= k h)
                   (loop
                      for i from 0 below (grid:dim0 v)
                      if (/= i k)
                      sum (* (abs (grid:gref v i))
                             (abs (grid:gref y k i))
                             (cos (- (grid:gref theta k)
                                     (grid:gref theta i)
                                     (phase (grid:gref y k i)))))
                      else
                      sum (* 2d0
                             (abs (grid:gref v k))
                             (abs (grid:gref y k k))
                             (cos (phase (grid:gref y k k)))))
                   (* (abs (grid:gref v k))
                      (abs (grid:gref y k h))
                      (cos (- (grid:gref theta k)
                              (grid:gref theta h)
                              (phase (grid:gref y k h)))))))
             (dq/dv (v theta y k h)
               (if (= k h)
                   (loop
                      for i from 0 below (grid:dim0 v)
                      if (/= i k)
                      sum (* (abs (grid:gref v i))
                             (abs (grid:gref y k i))
                             (sin (- (grid:gref theta k)
                                     (grid:gref theta i)
                                     (phase (grid:gref y k i)))))
                      else
                      sum (- (* 2d0
                                (abs (grid:gref v k))
                                (abs (grid:gref y k k))
                                (sin (phase (grid:gref y k k))))))
                   (* (abs (grid:gref v k))
                      (abs (grid:gref y k h))
                      (sin (- (grid:gref theta k)
                              (grid:gref theta h)
                              (phase (grid:gref y k h)))))))
             (update-dp/dtheta-matrix (m v theta y)
               (loop
                  for i from 0 below (grid:dim0 m)
                  do
                    (loop
                       for j from 0 below (grid:dim1 m)
                       do
                         (setf (grid:gref m i j) (dp/dtheta v theta y i j)))))
             (update-dq/dtheta-matrix (m v theta y)
               (loop
                  for i from 0 below (grid:dim0 m)
                  do
                    (loop
                       for j from 0 below (grid:dim1 m)
                       do
                         (setf (grid:gref m i j) (dq/dtheta v theta y i j)))))
             (update-dq/dv-matrix (m v theta y)
               (loop
                  for i from 0 below (grid:dim0 m)
                  do
                    (loop
                       for j from 0 below (grid:dim1 m)
                       do
                         (setf (grid:gref m i j) (dq/dv v theta y i j)))))
             (update-dp/dv-matrix (m v theta y)
               (loop
                  for i from 0 below (grid:dim0 m)
                  do
                    (loop
                       for j from 0 below (grid:dim1 m)
                       do
                         (setf (grid:gref m i j) (dp/dv v theta y i j))))))
      (setq jacobian-locks (list (bt:make-lock (symbol-name (gensym "update-dp/dtheta-lock-")))
                                 (bt:make-lock (symbol-name (gensym "update-dq/dtheta-lock-")))
                                 (bt:make-lock (symbol-name (gensym "update-dp/dv-lock-")))
                                 (bt:make-lock (symbol-name (gensym "update-dq/dv-lock-"))))
            jacobian-threads (list (bt:make-thread #'(lambda ()
                                                       (bt:with-lock-held ((first jacobian-locks))
                                                         (update-dp/dtheta-matrix dp/dtheta-matrix
                                                                                  (grid:copy voltages-vector)
                                                                                  (grid:copy thetas-vector)
                                                                                  (grid:copy admittances-matrix))))
                                                   :name (symbol-name (gensym "dp/dtheta-thread-")))
                                   (bt:make-thread #'(lambda ()
                                                       (bt:with-lock-held ((second jacobian-locks))
                                                         (update-dq/dtheta-matrix dq/dtheta-matrix
                                                                                  (grid:copy voltages-vector)
                                                                                  (grid:copy thetas-vector)
                                                                                  (grid:copy admittances-matrix))))
                                                   :name (symbol-name (gensym "dp/dtheta-thread-")))
                                   (bt:make-thread #'(lambda ()
                                                       (bt:with-lock-held ((third jacobian-locks))
                                                         (update-dp/dv-matrix dp/dv-matrix
                                                                              (grid:copy voltages-vector)
                                                                              (grid:copy thetas-vector)
                                                                              (grid:copy admittances-matrix))))
                                                   :name (symbol-name (gensym "dp/dv-thread-")))
                                   (bt:make-thread #'(lambda ()
                                                       (bt:with-lock-held ((fourth jacobian-locks))
                                                         (update-dq/dv-matrix dq/dv-matrix
                                                                              (grid:copy voltages-vector)
                                                                              (grid:copy thetas-vector)
                                                                              (grid:copy admittances-matrix))))
                                                   :name (symbol-name (gensym "dq/dv-thread-")))))
      (loop
         for lock in jacobian-locks
         do
           (bt:acquire-lock lock)
           (bt:release-lock lock))
      (setq jacobian-matrix (grid:concatenate-grids (grid:concatenate-grids (grid:copy dp/dtheta-matrix)
                                                                            (grid:copy dp/dv-matrix)
                                                                            :axis 1)
                                                    (grid:concatenate-grids (grid:copy dq/dtheta-matrix)
                                                                            (grid:copy dq/dv-matrix)
                                                                            :axis 1)
                                                    :axis 0))
      (if jacobian-matrix
          (progn
            (setq ok? t)
            (when (integerp verbose)
              (when (> verbose 10)
                (printout :message "dp/dtheta = ~s~&" dp/dtheta-matrix)
                (printout :message "dq/dtheta = ~s~&" dq/dtheta-matrix)
                (printout :message "dp/dv = ~s~&" dp/dv-matrix)
                (printout :message "dq/dv = ~s~&" dq/dv-matrix)
                (printout :message "Jacobian = ~s.~&" jacobian-matrix))))
          (progn
            (when (integerp verbose)
              (when (> verbose 10)
                (prinout :error "could not calculate Jacobian matrix.~&")))))
      (when (integerp verbose)
        (when (> verbose 5)
          (printout :message "exiting calculate-jacobian().~%~%")))
      (values jacobian-matrix
              ok?))))

(defun calculate-currents (&rest parameters &key
                                              (problem nil problem-p)
                                              (nodes-table nil nodes-table-p)
                                              (elements-table nil elements-table-p)
                                              (voltages-vector nil voltages-vector-p)
                                              (thetas-vector nil thetas-vector-p)
                                              (p-vector nil p-vector-p)
                                              (q-vector nil q-vector-p)
                                              (verbose nil verbose-p))
  (declare (ignorable problem
                      nodes-table
                      elements-table
                      voltages-vector
                      thetas-vector
                      p-vector
                      q-vector
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when nodes-table-p
    (check-type nodes-table hash-table))
  (when elements-table-p
    (check-type elements-table hash-table))
  (when voltages-vector-p
    (check-type voltages-vector grid:foreign-array))
  (when thetas-vector-p
    (check-type thetas-vector grid:foreign-array))
  (when p-vector-p
    (check-type p-vector grid:foreign-array))
  (when q-vector-p
    (check-type q-vector grid:foreign-array))
  (when (and verbose-p
             (> verbose 5))
    (printout :message "entering calculate-currents().~&"))
  (let ((current nil)
        (currents nil)
        (bond-current nil)
        (bond-currents nil)
        (ok? nil))
    (with-hash-table-iterator (nodes-iterator nodes-table)
      (loop
         named nodes-loop
         initially (setq ok? t)
         do
           (multiple-value-bind (more? node-name node-number)
               (nodes-iterator)
             (if more?
                 (let ((node (first (select-element :predicate (where :name node-name)
                                                    :network (problem-struct-network problem)))))
                   (if node
                       (case (node-struct-kind node)
                         (:generation
                          (case (bond-struct-kind (node-struct-bond node))
                            (:p-v
                             (setq bond-current (->phasor (/ (conjugate (complex (bond-struct-active-power (node-struct-bond node))
                                                                                 (grid:gref q-vector node-number)))
                                                             (* (grid:gref voltages-vector node-number)
                                                                (exp (* #c(0d0 1d0)
                                                                        (grid:gref thetas-vector node-number)))))))
                             (push (list :bond-name (bond-struct-name (node-struct-bond node))
                                         :current bond-current)
                                   bond-currents)
                             (when (and (integerp verbose)
                                        (> verbose 10))
                               (printout :message "Generation P-E bond ~a, current = ~s.~%" (bond-struct-name (node-struct-bond node)) bond-current)))
                            (:q-v
                             (setq bond-current (->phasor (/ (conjugate (complex (grid:gref p-vector node-number)
                                                                                 (bond-struct-reactive-power (node-struct-bond node))))
                                                             (* (grid:gref voltages-vector node-number)
                                                                (exp (* #c(0d0 1d0)
                                                                        (grid:gref thetas-vector node-number)))))))
                             (push (list :bond-name (bond-struct-name (node-struct-bond node))
                                         :current bond-current)
                                   bond-currents)
                             (when (and (integerp verbose)
                                        (> verbose 10))
                               (printout :message "Generation Q-E bond ~a, current = ~s.~%" (bond-struct-name (node-struct-bond node)) bond-current)))
                            (:p-q
                             (setq bond-current (->phasor (/ (conjugate (complex (bond-struct-active-power (node-struct-bond node))
                                                                                 (bond-struct-reactive-power (node-struct-bond node))))
                                                             (* (grid:gref voltages-vector node-number)
                                                                (exp (* #c(0d0 1d0)
                                                                        (grid:gref thetas-vector node-number)))))))
                             (push (list :bond-name (bond-struct-name (node-struct-bond node))
                                         :current bond-current)
                                   bond-currents)
                             (when (and (integerp verbose)
                                        (> verbose 10))
                               (printout :message "Generation P-Q bond ~a, current = ~s.~%" (bond-struct-name (node-struct-bond node)) bond-current)))
                            (:v-theta
                             (push (list :bond-name (bond-struct-name (node-struct-bond node))
                                         :current (->phasor (/ (conjugate (complex (grid:gref p-vector node-number)
                                                                                   (grid:gref q-vector node-number)))
                                                               (* (abs (grid:gref voltages-vector node-number))
                                                                  (exp (* #c(0d0 1d0)
                                                                          (grid:gref thetas-vector node-number)))))))
                                   bond-currents)
                             (when (and (integerp verbose)
                                        (> verbose 10))
                               (printout :message "Generation V-theta bond ~a, current = ~s.~%" (bond-struct-name (node-struct-bond node)) bond-current)))))
                         (:load
                          (case (bond-struct-kind (node-struct-bond node))
                            (:p-q
                             (setq bond-current (->phasor (/ (- (conjugate (complex (bond-struct-active-power (node-struct-bond node))
                                                                                    (bond-struct-reactive-power (node-struct-bond node)))))
                                                             (* (grid:gref voltages-vector node-number)
                                                                (exp (* #c(0d0 1d0)
                                                                        (grid:gref thetas-vector node-number)))))))
                             (push (list :bond-name (bond-struct-name (node-struct-bond node))
                                         :current bond-current)
                                   bond-currents)
                             (when (and (integerp verbose)
                                        (> verbose 10))
                               (printout :message "Load P-Q bond ~a, current = ~s.~%" (bond-struct-name (node-struct-bond node)) bond-current))))))
                       (progn
                         (when (and verbose-p
                                    (> verbose 10))
                           (printout :error "no node named ~a found.~&" node-name))
                         (setq ok? nil)
                         (return-from nodes-loop))))
                 (return-from nodes-loop)))))
    (when ok?
      (with-hash-table-iterator (elements-iterator elements-table)
        (loop
           named elements-loop
           initially (setq ok? t)
           for i from 0
           do
             (multiple-value-bind (more? element-name nodes)
                 (elements-iterator)
               (if more?
                   (let ((element (first (select-element :predicate (where :name element-name)
                                                         :network (problem-struct-network problem)))))
                     (if element
                         (let ((voltage-across (case (length nodes)
                                                 (2
                                                  (- (* (grid:gref voltages-vector (second nodes))
                                                        (exp (* #c(0d0 1d0)
                                                                (grid:gref thetas-vector (second nodes)))))
                                                     (* (grid:gref voltages-vector (first nodes))
                                                        (exp (* #c(0d0 1d0)
                                                                (grid:gref thetas-vector (first nodes)))))))
                                                 (1
                                                  (* (grid:gref voltages-vector (first nodes))
                                                     (exp (* #c(0d0 1d0)
                                                             (grid:gref thetas-vector (first nodes))))))
                                                 (t
                                                  ()))))
                           (setq current (product (->phasor voltage-across)
                                                  (case (bipole-struct-kind element)
                                                    ((or :resistance :impedance)
                                                     (->phasor (/ 1d0 (getf (bipole-struct-model-parameters element) :value))))
                                                    (:inductance
                                                     (->phasor (/ 1d0 (complex 0d0 (* 2d0
                                                                                      pi
                                                                                      (problem-struct-frequency problem)
                                                                                      (getf (bipole-struct-model-parameters element) :value))))))
                                                    (:capacitance
                                                     (->phasor (complex 0d0 (* 2d0
                                                                               pi
                                                                               (problem-struct-frequency problem)
                                                                               (getf (bipole-struct-model-parameters element) :value)))))
                                                    (:conductance
                                                     (->phasor (getf (bipole-struct-model-parameters element) :value)))
                                                    (:susceptance
                                                     (->phasor (complex 0d0 (getf (bipole-struct-model-parameters element) :value))))
                                                    (:admittance
                                                     (->phasor (getf (bipole-struct-model-parameters element) :value))))))
                           (push (list :bipole-name (bipole-struct-name element)
                                       :current current
                                       :voltage-across (->phasor voltage-across))
                                 currents)
                           (when (and verbose-p
                                      (> verbose 10))
                             (printout :message
                                       "~a(~{~a~^, ~}) voltage across = ~s, current ~s~%"
                                       element-name
                                       nodes
                                       voltage-across
                                       current)))
                         (progn
                           (when (and verbose-p
                                      (> verbose 10))
                             (printout :error "more than two nodes for ~a bipole.~&" element-name))
                           (setq ok? nil))))
                   (return-from elements-loop))))))
    (when (and verbose-p
               (> verbose 5))
      (printout :message "exiting calculate-currents().~%~%"))
    (values currents
            bond-currents
            ok?)))

(defun check-generators-reactive-power ()
  t)

(defun check-voltages ()
  t)

(defun calculate-loadflow ()
  ())

(defun change-transformers-tap ()
  ())

(defun update-admittance-matrix ()
  ())

(defun change-nodes (source destination)
  ())

(defun solve-system (&rest parameters &key
                                        (delta-p-vector nil delta-p-vector-p)
                                        (delta-q-vector nil delta-q-vector-p)
                                        (jacobian-matrix nil jacobian-matrix-p)
                                        (cv-matrix nil cv-matrix-p)
                                        (ctheta-matrix nil ctheta-matrix-p)
                                        (verbose nil verbose-p))
  (declare (ignorable parameters
                      delta-p-vector
                      delta-q-vector
                      jacobian-matrix
                      cv-matrix
                      ctheta-matrix
                      verbose))
  (when delta-p-vector-p
    (check-type delta-p-vector grid:foreign-array))
  (when delta-q-vector-p
    (check-type delta-q-vector grid:foreign-array))
  (when jacobian-matrix-p
    (check-type jacobian-matrix grid:foreign-array))
  (when cv-matrix-p
    (check-type cv-matrix grid:foreign-array))
  (when ctheta-matrix-p
    (check-type ctheta-matrix grid:foreign-array))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (integerp verbose)
    (when (> verbose 5)
      (printout :message "entering solve-system().~&")))
  (let* ((order (grid:dim0 jacobian-matrix))
         (x-vector nil)
         (y-vector (grid:concatenate-grids (gsll:matrix-product (grid:copy ctheta-matrix)
                                                                (grid:copy delta-p-vector))
                                           (gsll:matrix-product (grid:copy cv-matrix)
                                                                (grid:copy delta-q-vector))
                                           :axis 0))
         (delta-thetas nil)
         (delta-voltages nil)
         (ok? nil))
    (when (integerp verbose)
      (when (> verbose 10)
        (printout :message
                  "y = ~s, dim(s) = ~s.~&"
                  y-vector
                  (grid:dimensions y-vector))))
    (handler-case
        (multiple-value-bind (lu-matrix permutation signum)
            (gsll:lu-decomposition (grid:copy jacobian-matrix))
          (declare (ignore signum))
          (let ((initial-solution (gsll:lu-solve lu-matrix
                                                 (grid:copy y-vector)
                                                 permutation
                                                 t)))
            (setq x-vector (gsll:lu-refine (grid:copy jacobian-matrix)
                                           lu-matrix permutation
                                           (grid:copy y-vector)
                                           initial-solution))
            (if x-vector
                (progn
                  (loop
                     named solution-check-loop
                     finally (setq ok? t)
                     for i from 0 below (grid:dim0 x-vector)
                     when (gsl:nanp (grid:gref x-vector i))
                     do
                       (when (integerp verbose)
                         (when (> verbose 10)
                           (printout :error
                                     "Solution element is not a number x(~a)!~&"
                                     i)))
                       (setq ok? nil)
                       (return-from solution-check-loop))
                  (when ok?
                    (when (integerp verbose)
                      (when (> verbose 10)
                        (printout :message
                                  "x = ~s, dim(s) = ~s.~&"
                                  x-vector
                                  (grid:dimensions x-vector))
                        (printout :message
                                  "permutation = ~s.~&"
                                  permutation)))
                    (setq delta-thetas (gsll:matrix-product (grid:transpose (grid:copy ctheta-matrix))
                                                            (grid:subgrid x-vector (list (grid:dim0 ctheta-matrix)) (list 0)))
                          delta-voltages (gsll:matrix-product (grid:transpose (grid:copy cv-matrix))
                                                              (grid:subgrid x-vector (list (grid:dim0 cv-matrix)) (list (grid:dim0 ctheta-matrix))))))
                  (when (integerp verbose)
                    (when (> verbose 10)
                      (printout :error "no solution.~&")))))))
      (error (e)
        (setq ok? nil)))
    (when (integerp verbose)
      (when (> verbose 10)
        (printout :message "delta-voltages = ~s.~&" delta-voltages)
        (printout :message "delta-thetas = ~s.~&" delta-thetas)))
    (when (integerp verbose)
      (when (> verbose 5)
        (printout :message "exiting solve-system().~%~%")))
    (values delta-thetas
            delta-voltages
            ok?)))

(defun update-solution (&rest parameters &key
                                           (problem nil problem-p)
                                           (nodes-table nil nodes-table-p)
                                           (delta-voltages-vector nil delta-voltages-vector-p)
                                           (delta-thetas-vector nil delta-thetas-vector-p)
                                           (voltages-vector nil voltages-vector-p)
                                           (thetas-vector nil thetas-vector-p)
                                           (cv-matrix nil cv-matrix-p)
                                           (ctheta-matrix nil ctheta-matrix-p)
                                           (verbose nil verbose-p))
  (declare (ignorable parameters
                      problem
                      nodes-table
                      delta-voltages-vector
                      delta-thetas-vector
                      voltages-vector
                      thetas-vector
                      cv-matrix
                      ctheta-matrix
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when nodes-table-p
    (check-type nodes-table hash-table))
  (when delta-voltages-vector-p
    (check-type delta-voltages-vector grid:foreign-array))
  (when delta-thetas-vector-p
    (check-type delta-thetas-vector grid:foreign-array))
  (when voltages-vector-p
    (check-type voltages-vector grid:foreign-array))
  (when thetas-vector-p
    (check-type thetas-vector grid:foreign-array))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (problem-struct-alpha problem)
    (assert (= (grid:dim0 (problem-struct-alpha problem))
               (grid:dim0 thetas-vector))))
  (when (problem-struct-beta problem)
    (assert (= (grid:dim0 (problem-struct-beta problem))
               (grid:dim0 voltages-vector))))
  (when (integerp verbose)
    (when (> verbose 5)
      (printout :message "entering update-solution().~&")))
  (let ((ok? nil))
    (with-hash-table-iterator (nodes-iterator nodes-table)
      (loop
         named nodes-loop
         do
           (multiple-value-bind (more? node-name node-number)
               (nodes-iterator)
             (if more?
                 (let ((node (first (select-element :predicate (where :name node-name)
                                                    :network (problem-struct-network problem)))))
                   (if node
                       (case (node-struct-kind node)
                         (:generation
                          (case (bond-struct-kind (node-struct-bond node))
                            (:p-q
                             (incf (grid:gref voltages-vector node-number) (if (problem-struct-beta problem)
                                                                               (* (grid:gref (problem-struct-beta problem) node-number)
                                                                                  (grid:gref delta-voltages-vector node-number))
                                                                               (grid:gref delta-voltages-vector node-number)))
                             (incf (grid:gref thetas-vector node-number) (if (problem-struct-alpha problem)
                                                                             (* (grid:gref (problem-struct-alpha problem) node-number)
                                                                                (grid:gref delta-thetas-vector node-number))
                                                                             (grid:gref delta-thetas-vector node-number))))
                            ((or :p-v :q-v)
                             (setf (grid:gref voltages-vector node-number) (bond-struct-voltage-magnitude (node-struct-bond node)))
                             (incf (grid:gref thetas-vector node-number) (if (problem-struct-alpha problem)
                                                                             (* (grid:gref (problem-struct-alpha problem) node-number)
                                                                                (grid:gref delta-thetas-vector node-number))
                                                                             (grid:gref delta-thetas-vector node-number))))
                            (:v-theta
                             (setf (grid:gref voltages-vector node-number) (bond-struct-voltage-magnitude (node-struct-bond node))
                                   (grid:gref thetas-vector node-number) (bond-struct-voltage-phase (node-struct-bond node))))))
                         (:load
                          (case (bond-struct-kind (node-struct-bond node))
                            (:v=f(Q)
                             ())
                            (:p-q
                             (incf (grid:gref voltages-vector node-number) (if (problem-struct-beta problem)
                                                                               (* (grid:gref (problem-struct-beta problem) node-number)
                                                                                  (grid:gref delta-voltages-vector node-number))
                                                                               (grid:gref delta-voltages-vector node-number)))
                             (incf (grid:gref thetas-vector node-number) (if (problem-struct-alpha problem)
                                                                             (* (grid:gref (problem-struct-alpha problem) node-number)
                                                                                (grid:gref delta-thetas-vector node-number))
                                                                             (grid:gref delta-thetas-vector node-number))))))
                         (:interconnection
                          (incf (grid:gref voltages-vector node-number) (if (problem-struct-beta problem)
                                                                            (* (grid:gref (problem-struct-beta problem) node-number)
                                                                               (grid:gref delta-voltages-vector node-number))
                                                                            (grid:gref delta-voltages-vector node-number)))
                          (incf (grid:gref thetas-vector node-number) (if (problem-struct-alpha problem)
                                                                          (* (grid:gref (problem-struct-alpha problem) node-number)
                                                                             (grid:gref delta-thetas-vector node-number))
                                                                          (grid:gref delta-thetas-vector node-number)))))
                       (progn
                         (setq ok? nil)
                         (when (integerp verbose)
                           (when (> verbose 10)
                             (printout :error "node ~a not found.~&" node-name)))
                         (return-from nodes-loop))))
                 (progn
                   (setq ok? t)
                   (return-from nodes-loop))))))
    (when (integerp verbose)
      (when (> verbose 10)
        (printout :message "voltages vector = ~s.~&" voltages-vector)
        (printout :message "thetas vector = ~s.~&" thetas-vector)))
    (when (integerp verbose)
      (when (> verbose 5)
        (printout :message "exiting update-solution().~%~%")))
    ok?))

(defun check-power-tollerance (&rest parameters &key
                                                  (problem nil problem-p)
                                                  (nodes-table nil nodes-table-p)
                                                  (delta-p-vector nil delta-p-vector-p)
                                                  (delta-q-vector nil delta-q-vector-p)
                                                  (verbose nil verbose-p))
  (declare (ignorable parameters
                      problem
                      nodes-table
                      delta-p-vector
                      delta-q-vector
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when nodes-table-p
    (check-type nodes-table hash-table))
  (when delta-p-vector-p
    (check-type delta-p-vector grid:foreign-array)
    (assert (= (length (grid:dimensions delta-p-vector)) 1)))
  (when delta-q-vector-p
    (check-type delta-q-vector grid:foreign-array)
    (assert (= (length (grid:dimensions delta-q-vector)) 1)))
  (when (and delta-p-vector-p
             delta-p-vector-p)
    (assert (equalp (grid:dimensions delta-p-vector)
                    (grid:dimensions delta-q-vector))))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (integerp verbose)
    (when (> verbose 5)
      (printout :message "entering check-power-tollerance().~&")))
  (let ((ok? nil))
    (with-hash-table-iterator (nodes-iterator nodes-table)
      (loop
         named nodes-loop
         finally (setq ok? t)
         do
           (multiple-value-bind (more? node-name node-number)
               (nodes-iterator)
             (if more?
                 (let ((node (first (select-element :predicate (where :name node-name)
                                                    :network (problem-struct-network problem)))))
                   (if node
                       (progn
                         (when (>= (abs (grid:gref delta-p-vector node-number))
                                   (realpart (problem-struct-epsilon-power problem)))
                           (when (integerp verbose)
                             (when (> verbose 10)
                               (printout :message
                                         "active power is over the selected tolerance |~a| > ~a.~&"
                                         (grid:gref delta-p-vector node-number)
                                         (realpart (problem-struct-epsilon-power problem)))))
                           (return-from nodes-loop))
                         (when (>= (abs (grid:gref delta-q-vector node-number))
                                   (imagpart (problem-struct-epsilon-power problem)))
                           (when (integerp verbose)
                             (when (> verbose 10)
                               (printout :message
                                         "reactive power is over the selected tolerance |~a| > ~a.~&"
                                         (grid:gref delta-q-vector node-number)
                                         (imagpart (problem-struct-epsilon-power problem)))))
                           (return-from nodes-loop)))
                       (progn
                         (setq ok? nil)
                         (return-from nodes-loop))))
                 (progn
                   (setq ok? t)
                   (when (integerp verbose)
                     (when (> verbose 10)
                       (printout :message "power requirements met.~&")))
                   (return-from nodes-loop))))))
    (when (integerp verbose)
      (when (> verbose 5)
        (printout :message "exiting check-power-tollerance().~%~%")))
    ok?))

(defun output-solution (&rest parameters &key
                                           (problem nil problem-p)
                                           (nodes-table nil nodes-table-p)
                                           (elements-table nil elements-table-p)
                                           (voltages-vector nil voltages-vector-p)
                                           (thetas-vector nil thetas-vector-p)
                                           (p-vector nil p-vector-p)
                                           (q-vector nil q-vector-p)
                                           (verbose nil verbose-p))
  (declare (ignorable parameters
                      problem
                      nodes-table
                      elements-table
                      voltages-vector
                      thetas-vector
                      p-vector
                      q-vector
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when nodes-table-p
    (check-type nodes-table hash-table))
  (when elements-table-p
    (check-type elements-table hash-table))
  (when voltages-vector-p
    (check-type voltages-vector grid:foreign-array))
  (when thetas-vector-p
    (check-type thetas-vector grid:foreign-array))
  (when p-vector-p
    (check-type p-vector grid:foreign-array))
  (when q-vector-p
    (check-type q-vector grid:foreign-array))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (integerp verbose)
    (when (> verbose 5)
      (printout :message "entering output-solution().~&")))
  (let ((voltages (make-array (grid:dim0 voltages-vector) :element-type 'phasor-struct :initial-element (make-phasor))))
    (loop
       for i from 0 below (grid:dim0 voltages-vector)
       do
         (setf (aref voltages i) (make-phasor :magnitude (grid:gref voltages-vector i)
                                              :argument (grid:gref thetas-vector i))))
    (multiple-value-bind (currents bond-currents ok?)
        (calculate-currents :problem problem
                            :nodes-table nodes-table
                            :elements-table elements-table
                            :voltages-vector voltages-vector
                            :thetas-vector thetas-vector
                            :p-vector p-vector
                            :q-vector q-vector
                            :verbose verbose)
      (if ok?
          (when (integerp verbose)
            (when (> verbose 10)
              (progn
                (printout :message "Voltages = ~s~%" voltages)
                (printout :message "Currents = ~s~%" currents)
                (printout :message "Bond Currents = ~s~%" bond-currents))))
          (when (integerp verbose)
            (when (> verbose 10)
              (printout :error "could not calculate currents.~&"))))
      (when (integerp verbose)
        (when (> verbose 5)
          (printout :message "exiting output-solution().~%~%")))
      (values voltages
              currents
              bond-currents
              ok?))))

(defun loadflow (&rest parameters &key
                                    (problem-file-pathname nil problem-file-pathname-p)
                                    (verbose 10 verbose-p))
  (declare (ignorable parameters
                      problem-file-pathname
                      verbose))
  (when problem-file-pathname-p
    (check-type problem-file-pathname pathname))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (and verbose-p
             (> verbose 5))
    (printout :message "entering loadflow().~&"))
  (let ((return-value nil))
    (multiple-value-bind (problem ok?)
        (get-input-data :problem-file-pathname problem-file-pathname
                        :verbose verbose)
      (if ok? ;; is input data ok?
          (multiple-value-bind (nodes-table elements-table ok?)
              (setup-problem :problem problem
                             :verbose verbose)
            (if ok? ;; is setup of data ok?
                (multiple-value-bind (cv-matrix ctheta-matrix ok?)
                    (create-connection-matrices :problem problem
                                                :nodes-table nodes-table
                                                :elements-table elements-table
                                                :verbose verbose)
                  (if ok? ;; are connection matrices succesfully created?
                      (multiple-value-bind (voltages-vector thetas-vector p-vector q-vector delta-p-vector delta-q-vector ok?)
                          (create-vectors :problem problem
                                          :nodes-table nodes-table
                                          :verbose verbose)
                        (if ok? ;; are vectors succesfully created?
                            (multiple-value-bind (admittances-matrix ok?)
                                (create-admittances-matrix :problem problem
                                                           :nodes-table nodes-table
                                                           :elements-table elements-table
                                                           :verbose verbose)
                              (if ok? ;; admittances matrix is succesfully created.
                                  (multiple-value-bind (dp/dtheta-matrix dq/dtheta-matrix dp/dv-matrix dq/dv-matrix ok?)
                                      (create-jacobian-matrix :problem problem
                                                              :verbose verbose)
                                    (if ok? ;; Jacobian matrix created!
                                        (multiple-value-bind (voltages-vector thetas-vector ok?)
                                            (assign-initial-solution :problem problem
                                                                     :nodes-table nodes-table
                                                                     :voltages-vector voltages-vector
                                                                     :thetas-vector thetas-vector
                                                                     :verbose verbose)
                                          (if ok? ;; initial solution assigned.
                                              ;; main loop start.
                                              (setq return-value (loop
                                                                    named main-loop
                                                                    initially (progn
                                                                                (when (integerp verbose)
                                                                                  (when (> verbose 10)
                                                                                    (printout :message "entering main loop.~%~%"))))
                                                                    do
                                                                      (multiple-value-bind (p-vector q-vector delta-p-vector delta-q-vector ok?)
                                                                          (calculate-pq-residuals :problem problem
                                                                                                  :nodes-table nodes-table
                                                                                                  :elements-table elements-table
                                                                                                  :admittances-matrix admittances-matrix
                                                                                                  :voltages-vector voltages-vector
                                                                                                  :thetas-vector thetas-vector
                                                                                                  :p-vector p-vector
                                                                                                  :q-vector q-vector
                                                                                                  :delta-p-vector delta-p-vector
                                                                                                  :delta-q-vector delta-q-vector
                                                                                                  :verbose verbose)
                                                                        (if ok?
                                                                            ;; solving loop start.
                                                                            (when (loop
                                                                                     named solving-loop
                                                                                     initially (progn
                                                                                                 (when (integerp verbose)
                                                                                                   (when (> verbose 10)
                                                                                                     (printout :message "entering solving loop.~%~%"))))
                                                                                     finally (progn
                                                                                               (when (integerp verbose)
                                                                                                 (when (> verbose 10)
                                                                                                   (printout :error "iterations maximum count reached (~a).~&" (problem-struct-maximum-iterations-count problem))))
                                                                                               (return-from main-loop))
                                                                                     for i from 1 upto (problem-struct-maximum-iterations-count problem)
                                                                                     do
                                                                                       (when (integerp verbose)
                                                                                         (when (> verbose 5)
                                                                                           (printout :message "iteration #~a~%~%" i)))
                                                                                       (multiple-value-bind (jacobian-matrix ok?)
                                                                                           (calculate-jacobian :dp/dtheta-matrix dp/dtheta-matrix
                                                                                                               :dq/dtheta-matrix dq/dtheta-matrix
                                                                                                               :dp/dv-matrix dp/dv-matrix
                                                                                                               :dq/dv-matrix dq/dv-matrix
                                                                                                               :admittances-matrix admittances-matrix
                                                                                                               :voltages-vector voltages-vector
                                                                                                               :thetas-vector thetas-vector
                                                                                                               :verbose verbose)
                                                                                         (if ok? ;; Jacobian is ok!
                                                                                             (multiple-value-bind (delta-thetas-vector delta-voltages-vector ok?)
                                                                                                 (solve-system :delta-p-vector delta-p-vector
                                                                                                               :delta-q-vector delta-q-vector
                                                                                                               :jacobian-matrix jacobian-matrix
                                                                                                               :cv-matrix cv-matrix
                                                                                                               :ctheta-matrix ctheta-matrix
                                                                                                               :verbose verbose)
                                                                                               (if ok? ;; system is solved.
                                                                                                   (progn ;; then update the voltages and thetas vectors.
                                                                                                     (if (update-solution :problem problem
                                                                                                                          :nodes-table nodes-table
                                                                                                                          :delta-voltages-vector delta-voltages-vector
                                                                                                                          :delta-thetas-vector delta-thetas-vector
                                                                                                                          :voltages-vector voltages-vector
                                                                                                                          :thetas-vector thetas-vector
                                                                                                                          :cv-matrix cv-matrix
                                                                                                                          :ctheta-matrix ctheta-matrix
                                                                                                                          :verbose verbose)
                                                                                                         (multiple-value-bind (p-vector q-vector delta-p-vector delta-q-vector ok?)
                                                                                                             (calculate-pq-residuals :problem problem
                                                                                                                                     :nodes-table nodes-table
                                                                                                                                     :elements-table elements-table
                                                                                                                                     :admittances-matrix admittances-matrix
                                                                                                                                     :voltages-vector voltages-vector
                                                                                                                                     :thetas-vector thetas-vector
                                                                                                                                     :p-vector p-vector
                                                                                                                                     :q-vector q-vector
                                                                                                                                     :delta-p-vector delta-p-vector
                                                                                                                                     :delta-q-vector delta-q-vector
                                                                                                                                     :verbose verbose)
                                                                                                           (if ok?
                                                                                                               (when (check-power-tollerance :problem problem
                                                                                                                                             :nodes-table nodes-table
                                                                                                                                             :delta-p-vector delta-p-vector
                                                                                                                                             :delta-q-vector delta-q-vector
                                                                                                                                             :verbose verbose)
                                                                                                                 (multiple-value-bind (voltages currents bond-currents ok?)
                                                                                                                     (output-solution :problem problem
                                                                                                                                      :nodes-table nodes-table
                                                                                                                                      :elements-table elements-table
                                                                                                                                      :voltages-vector voltages-vector
                                                                                                                                      :thetas-vector thetas-vector
                                                                                                                                      :p-vector p-vector
                                                                                                                                      :q-vector q-vector
                                                                                                                                      :verbose verbose)
                                                                                                                   (if ok?
                                                                                                                       (progn
                                                                                                                         (setf (problem-struct-solution problem) (list (list :powers (loop
                                                                                                                                                                                        for i from 0 below (grid:dim0 p-vector)
                                                                                                                                                                                        collect (make-phasor :magnitude (abs (complex (grid:gref p-vector i)
                                                                                                                                                                                                                                      (grid:gref q-vector i)))
                                                                                                                                                                                                             :argument (atan (grid:gref q-vector i)
                                                                                                                                                                                                                             (grid:gref p-vector i)))))
                                                                                                                                                                       (list :voltages voltages)
                                                                                                                                                                       (list :currents currents)
                                                                                                                                                                       (list :bond-currents bond-currents)
                                                                                                                                                                       (list :iteration-count i))
                                                                                                                               return-value problem)
                                                                                                                         (return-from solving-loop t))
                                                                                                                       (return-from solving-loop nil))))
                                                                                                               (progn ;; power residuals are not evaluated.
                                                                                                                 (when (integerp verbose)
                                                                                                                   (when (> verbose 10)
                                                                                                                     (printout :error "could not compute power residuals.~&")))
                                                                                                                 (return-from main-loop))))
                                                                                                         (progn
                                                                                                           (when (integerp verbose)
                                                                                                             (when (> verbose 10)
                                                                                                               (printout :error "could not update solution.~&")))
                                                                                                           (return-from main-loop))))
                                                                                                   (progn
                                                                                                     (when (integerp verbose)
                                                                                                       (when (> verbose 10)
                                                                                                         (printout :error "could not solve system.~&")))
                                                                                                     (return-from main-loop))))
                                                                                             (progn
                                                                                               (when (integerp verbose)
                                                                                                 (when (> verbose 10)
                                                                                                   (printout :error "could not compute the Jacobian.~&")))
                                                                                               (return-from main-loop)))))
                                                                              ;; solving loop end.
                                                                              (when (integerp verbose)
                                                                                (when (> verbose 10)
                                                                                  (printout :message "solution reached: ~s ~s.~&" voltages-vector thetas-vector)))
                                                                              (return-from main-loop return-value))
                                                                            (progn ;; initial power residuals not successfully computed.
                                                                              (when (integerp verbose)
                                                                                (when (> verbose 10)
                                                                                  (printout :error "could not compute inital power residuals.~&")))
                                                                              (return-from main-loop))))))
                                              ;; main loop end.
                                              (when (integerp verbose) ;; initial solution not assigned.
                                                (when (> verbose 10)
                                                  (printout :error "could not assign an initial solution.~&")))))
                                        (when (integerp verbose) ;; Jacobian matrix not created.
                                          (when (> verbose 10)
                                            (printout :error "could not create the Jacobian matrix.~&")))))
                                  (when (integerp verbose) ;; admittances matrix was not created.
                                    (when (> verbose 10)
                                      (printout :error "could no create the admittances matrix.~&")))))
                            (when (integerp verbose) ;; vectors were not created.
                              (when (> verbose 10)
                                (printout :error "could not create vectors.~&")))))
                      (when (integerp verbose) ;; connection matrices are not created.
                        (when (> verbose 10)
                          (printout :error "could not create connection matrices.~&")))))
                (when (integerp verbose) ;; setup of data is not ok.
                  (when (> verbose 10)
                    (printout :error "could not setup problem ~a.~&" (problem-struct-name problem))))))
          (when (integerp verbose) ;; input data is not ok.
            (when (> integerp 10)
              (printout :error "could not read problem ~s.~&" problem-file-pathname)))))
    return-value))

(defun main (argv)
  (declare (ignore argv))
  (print (funcall #'loadflow argv))
  (sb-ext:quit :unix-status 0))
