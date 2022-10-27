;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; loadflow.lisp
;;;
;;; Loadflow problem definitions and utilities.

(in-package #:loadflow)

;; Parameters.

;; Structures.

;; Functions.

(defun assign-initial-solution (&rest parameters &key
                                                   (problem nil problem-p)
                                                   (voltages-vector nil voltages-vector-p)
                                                   (thetas-vector nil thetas-vector-p)
                                                   (verbose nil verbose-p))
  "Assign initial solution to the problem."
  (declare (ignorable parameters
                      problem
                      voltages-vector
                      thetas-vector
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when voltages-vector-p
    (check-type voltages-vector grid:foreign-array))
  (when thetas-vector-p
    (check-type thetas-vector grid:foreign-array))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (integerp verbose)
    (when (> verbose 20)
      (printout *standard-output* :message "entering assign-initial-solution().~&")))
  (let ((ok? nil))
    (loop
      named nodes-loop
      for node in (extract-nodes problem)
      do
         (case (node-struct-kind node)
           (:generation
            (case (bond-struct-kind (node-struct-bond node))
              (:p-v ;; Q and theta unknown - |V| value is in bond-struct :voltage-magnitude and initial value for theta is in :data
               (when (integerp verbose)
                 (when (> verbose 10)
                   (printout *standard-output*
                             :message
                             "node ~a (~a) applying voltage magnitude bond as initial value: ~s.~&"
                             (node-struct-name node)
                             (node-struct-tag node)
                             (node-struct-bond node))
                   (printout *standard-output*
                             :message
                             "node ~a (~a) applying voltage phase initial value: ~s.~&"
                             (node-struct-name node)
                             (node-struct-tag node)
                             (node-struct-data node))))
               (setf (grid:gref voltages-vector (node-struct-tag node)) (bond-struct-voltage-magnitude (node-struct-bond node))
                     (grid:gref thetas-vector (node-struct-tag node)) (getf (node-struct-data node) :voltage-phase))
               (setq ok? t))
              (:q-v ;; P and theta unknown - |V| value is in bond-struct :voltage-magnitude and initial value for theta is in :data
               (when (integerp verbose)
                 (when (> verbose 10)
                   (printout *standard-output*
                             :message
                             "node ~a (~a) applying voltage magnitude bond as initial value: ~s.~&"
                             (node-struct-name node)
                             (node-struct-tag node)
                             (node-struct-bond node))
                   (printout *standard-output*
                             :message
                             "node ~a (~a) applying voltage phase initial value: ~s.~&"
                             (node-struct-name node)
                             (node-struct-tag node)
                             (node-struct-data node))))
               (setf (grid:gref voltages-vector (node-struct-tag node)) (bond-struct-voltage-magnitude (node-struct-bond node))
                     (grid:gref thetas-vector (node-struct-tag node)) (getf (node-struct-data node) :voltage-phase))
               (setq ok? t))
              (:p-q ;; |V| and theta unknown - initial values for |V| and theta are in :data
               (when (integerp verbose)
                 (when (> verbose 10)
                   (printout *standard-output*
                             :message
                             "node ~a (~a) applying voltage phasor initial value: ~s.~&"
                             (node-struct-name node)
                             (node-struct-tag node)
                             (node-struct-data node))))
               (setf (grid:gref voltages-vector (node-struct-tag node)) (getf (node-struct-data node) :voltage-magnitude)
                     (grid:gref thetas-vector (node-struct-tag node)) (getf (node-struct-data node) :voltage-phase))
               (setq ok? t))
              (:v-theta ;; P and Q unknown - |V| and arg(V) values are in bond-struct :voltage-magnitude and :voltage-phase
               (when (integerp verbose)
                 (when (> verbose 10)
                   (printout *standard-output*
                             :message
                             "node ~a (~a) applying voltage phasor bond as initial value: ~s.~&"
                             (node-struct-name node)
                             (node-struct-tag node)
                             (node-struct-bond node))))
               (setf (grid:gref voltages-vector (node-struct-tag node)) (bond-struct-voltage-magnitude (node-struct-bond node))
                     (grid:gref thetas-vector (node-struct-tag node)) (bond-struct-voltage-phase (node-struct-bond node)))
               (setq ok? t))))
           (:load
            (case (bond-struct-kind (node-struct-bond node))
              (:p-q ;; |e| and theta unknown - initial values for |e| and theta are in :data
               (when (integerp verbose)
                 (when (> verbose 10)
                   (printout *standard-output*
                             :message
                             "node ~a (~a) applying voltage phasor initial value: ~s.~&"
                             (node-struct-name node)
                             (node-struct-tag node)
                             (node-struct-data node))))
               (setf (grid:gref voltages-vector (node-struct-tag node)) (getf (node-struct-data node) :voltage-magnitude)
                     (grid:gref thetas-vector (node-struct-tag node)) (getf (node-struct-data node) :voltage-phase))
               (setq ok? t))
              (:v=f(Q)
               (setf (grid:gref voltages-vector (node-struct-tag node)) (getf (node-struct-data node) :voltage-magnitude)
                     (grid:gref thetas-vector (node-struct-tag node)) (getf (node-struct-data node) :voltage-phase))
               (setq ok? t))))
           (:interconnection
            (when (integerp verbose)
              (when (> verbose 10)
                (printout *standard-output*
                          :message
                          "node ~a (~a) applying voltage phasor initial value: ~s.~&"
                          (node-struct-name node)
                          (node-struct-tag node)
                          (node-struct-data node))))
            (setf (grid:gref voltages-vector (node-struct-tag node)) (getf (node-struct-data node) :voltage-magnitude)
                  (grid:gref thetas-vector (node-struct-tag node)) (getf (node-struct-data node) :voltage-phase))
            (setq ok? t))))
    (when (integerp verbose)
      (when (> verbose 10)
        (printout *standard-output* :message "initial voltages vector = ~s.~&" voltages-vector)
        (printout *standard-output* :message "initial thetas vector = ~s.~&" thetas-vector)))
    (when (integerp verbose)
      (when (> verbose 20)
        (printout *standard-output* :message "exiting assign-initial-solution().~%~%")))
    (values voltages-vector
            thetas-vector
            ok?)))

(defun calculate-power-residuals (&rest parameters &key
                                                     (problem nil problem-p)
                                                     (admittances-matrix nil admittances-matrix-p)
                                                     (voltages-vector nil voltages-vector-p)
                                                     (thetas-vector nil thetas-vector-p)
                                                     (p-vector nil p-vector-p)
                                                     (q-vector nil q-vector-p)
                                                     (delta-p-vector nil delta-p-vector-p)
                                                     (delta-q-vector nil delta-q-vector-p)
                                                     (verbose nil verbose-p))
  "Calculate power residuals to use in Jacobian calculations."
  (declare (ignorable parameters
                      problem
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
      (printout *standard-output* :message "entering calculate-power-residuals().~&")))
  (let ((ok? nil))
    (handler-case
        (labels ((nodal-active-power (k v theta y)
                   (loop
                     for i from 0 below (grid:dim0 v)
                     sum (* (abs (grid:gref v k))
                            (abs (grid:gref v i))
                            (abs (grid:gref y k i))
                            (cos (- (grid:gref theta k)
                                    (grid:gref theta i)
                                    (phase (grid:gref y k i)))))))
                 (nodal-reactive-power (k v theta y)
                   (loop
                     for i from 0 below (grid:dim0 v)
                     sum (* (abs (grid:gref v k))
                            (abs (grid:gref v i))
                            (abs (grid:gref y k i))
                            (sin (- (grid:gref theta k)
                                    (grid:gref theta i)
                                    (phase (grid:gref y k i))))))))
          (loop
            initially (setq ok? t)
            named nodes-loop
            for node in (remove-if-not #'(lambda (x)
                                           (typep x 'node-struct))
                                       (problem-struct-network problem))
            do
               (case (node-struct-kind node)
                 (:generation
                  (case (bond-struct-kind (node-struct-bond node))
                    (:p-v
                     (setf (grid:gref p-vector (node-struct-tag node)) (nodal-active-power (node-struct-tag node)
                                                                                           voltages-vector
                                                                                           thetas-vector
                                                                                           admittances-matrix)
                           (grid:gref q-vector (node-struct-tag node)) (nodal-reactive-power (node-struct-tag node)
                                                                                             voltages-vector
                                                                                             thetas-vector
                                                                                             admittances-matrix)
                           (grid:gref delta-p-vector (node-struct-tag node)) (- (bond-struct-active-power (node-struct-bond node))
                                                                                (grid:gref p-vector (node-struct-tag node)))
                           (grid:gref delta-q-vector (node-struct-tag node)) 0d0)
                    (:q-v
                     (setf (grid:gref p-vector (node-struct-tag node)) (nodal-active-power (node-struct-tag node)
                                                                                           voltages-vector
                                                                                           thetas-vector
                                                                                           admittances-matrix)
                           (grid:gref q-vector (node-struct-tag node)) (nodal-reactive-power (node-struct-tag node)
                                                                                             voltages-vector
                                                                                             thetas-vector
                                                                                             admittances-matrix)
                           (grid:gref delta-p-vector (node-struct-tag node)) 0d0
                           (grid:gref delta-q-vector (node-struct-tag node)) (- (bond-struct-reactive-power (node-struct-bond node))
                                                                                (grid:gref q-vector (node-struct-tag node)))))
                    (:p-q
                     (setf (grid:gref p-vector (node-struct-tag node)) (nodal-active-power (node-struct-tag node)
                                                                                           voltages-vector
                                                                                           thetas-vector
                                                                                           admittances-matrix)
                           (grid:gref q-vector (node-struct-tag node)) (nodal-reactive-power (node-struct-tag node)
                                                                                             voltages-vector
                                                                                             thetas-vector
                                                                                             admittances-matrix)
                           (grid:gref delta-p-vector (node-struct-tag node)) (- (bond-struct-active-power (node-struct-bond node))
                                                                                (grid:gref p-vector (node-struct-tag node)))
                           (grid:gref delta-q-vector (node-struct-tag node)) (- (bond-struct-reactive-power (node-struct-bond node))
                                                                                (grid:gref q-vector (node-struct-tag node)))))
                    (:v-theta
                     (setf (grid:gref p-vector (node-struct-tag node)) (nodal-active-power (node-struct-tag node)
                                                                                           voltages-vector
                                                                                           thetas-vector
                                                                                           admittances-matrix)
                           (grid:gref q-vector (node-struct-tag node)) (nodal-reactive-power (node-struct-tag node)
                                                                                             voltages-vector
                                                                                             thetas-vector
                                                                                             admittances-matrix)
                           (grid:gref delta-p-vector (node-struct-tag node)) 0d0
                           (grid:gref delta-q-vector (node-struct-tag node)) 0d0)))))
                 (:load
                  (case (bond-struct-kind (node-struct-bond node))
                    (:v=f(Q)
                     ())
                    (:p-q
                     (setf (grid:gref p-vector (node-struct-tag node)) (nodal-active-power (node-struct-tag node)
                                                                                           voltages-vector
                                                                                           thetas-vector
                                                                                           admittances-matrix)
                           (grid:gref q-vector (node-struct-tag node)) (nodal-reactive-power (node-struct-tag node)
                                                                                             voltages-vector
                                                                                             thetas-vector
                                                                                             admittances-matrix)
                           (grid:gref delta-p-vector (node-struct-tag node)) (- (- (bond-struct-active-power (node-struct-bond node)))
                                                                                (grid:gref p-vector (node-struct-tag node)))
                           (grid:gref delta-q-vector (node-struct-tag node)) (- (- (bond-struct-reactive-power (node-struct-bond node)))
                                                                                (grid:gref q-vector (node-struct-tag node)))))))
                 (:interconnection
                  (setf (grid:gref p-vector (node-struct-tag node)) (nodal-active-power (node-struct-tag node)
                                                                                        voltages-vector
                                                                                        thetas-vector
                                                                                        admittances-matrix)
                        (grid:gref q-vector (node-struct-tag node)) (nodal-reactive-power (node-struct-tag node)
                                                                                          voltages-vector
                                                                                          thetas-vector
                                                                                          admittances-matrix)
                        (grid:gref delta-p-vector (node-struct-tag node)) (- (grid:gref p-vector (node-struct-tag node)))
                        (grid:gref delta-q-vector (node-struct-tag node)) (- (grid:gref q-vector (node-struct-tag node)))))))
          (when ok?
            (when (integerp verbose)
              (when (> verbose 10)
                (printout *standard-output* :message "P = ~a.~&" p-vector)
                (printout *standard-output* :message "Q = ~a.~&" q-vector)
                (printout *standard-output* :message "delta-P = ~s.~&" delta-p-vector)
                (printout *standard-output* :message "delta-Q = ~s.~&" delta-q-vector))))
          (when (integerp verbose)
            (when (> verbose 5)
              (printout *standard-output* :message "exiting calculate-power-residuals().~%~%"))))
      (error (e)
        (setq ok? nil)))
    (values p-vector
            q-vector
            delta-p-vector
            delta-q-vector
            ok?)))

(defun calculate-jacobian (&rest parameters &key
                                              (problem nil problem-p)
                                              (dp/dtheta-matrix nil dp/dtheta-matrix-p)
                                              (dq/dtheta-matrix nil dq/dtheta-matrix-p)
                                              (dp/dv-matrix nil dp/dv-matrix-p)
                                              (dq/dv-matrix nil dq/dv-matrix-p)
                                              (admittances-matrix nil admittances-matrix-p)
                                              (voltages-vector nil voltages-vector-p)
                                              (thetas-vector nil thetas-vector-p)
                                              (verbose nil verbose-p))
  "Calculate the complete Jacobian for the complete solution method."
  (declare (ignorable parameters
                      problem
                      dp/dtheta-matrix
                      dq/dtheta-matrix
                      dp/dv-matrix
                      dq/dv-matrix
                      admittances-matrix
                      voltages-vector
                      thetas-vector
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
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
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (integerp verbose)
    (when (> verbose 5)
      (printout *standard-output* :message "entering calculate-jacobian().~&")))
  (let ((nodes (extract-nodes problem))
        (jacobian-threads nil)
        (jacobian-matrix nil)
        (ok? nil))
    (labels ((dp/dtheta (v theta y k h)
               (if (= k h)
                   (- (loop
                        for i from 0 below (grid:dim0 v)
                        when (/= i k)
                          sum (* (abs (grid:gref v k))
                                 (abs (grid:gref v i))
                                 (abs (grid:gref y k i))
                                 (sin (- (grid:gref theta k)
                                         (grid:gref theta i)
                                         (phase (grid:gref y k i)))))))
                   (* (abs (grid:gref v k))
                      (abs (grid:gref v h))
                      (abs (grid:gref y k h))
                      (sin (- (grid:gref theta k)
                              (grid:gref theta h)
                              (phase (grid:gref y k h)))))))
             (dq/dtheta (v theta y k h)
               (if (= k h)
                   (loop
                     for i from 0 below (grid:dim0 v)
                     when (/= i k)
                       sum (* (abs (grid:gref v k))
                              (abs (grid:gref v i))
                              (abs (grid:gref y k i))
                              (cos (- (grid:gref theta k)
                                      (grid:gref theta i)
                                      (phase (grid:gref y k i))))))
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
                     if (= i k)
                       sum (* 2d0
                              (abs (grid:gref v k))
                              (abs (grid:gref y k k))
                              (cos (phase (grid:gref y k k))))
                     else
                       sum (* (abs (grid:gref v i))
                              (abs (grid:gref y k i))
                              (cos (- (grid:gref theta k)
                                      (grid:gref theta i)
                                      (phase (grid:gref y k i))))))
                   (* (grid:gref v k)
                      (abs (grid:gref y k h))
                      (cos (- (grid:gref theta k)
                              (grid:gref theta h)
                              (phase (grid:gref y k h)))))))
             (dq/dv (v theta y k h)
               (if (= k h)
                   (loop
                     for i from 0 below (grid:dim0 v)
                     if (= i k)
                       sum (* -2d0
                              (abs (grid:gref v k))
                              (abs (grid:gref y k k))
                              (sin (phase (grid:gref y k k))))
                     else
                       sum (* (abs (grid:gref v i))
                              (abs (grid:gref y k i))
                              (sin (- (grid:gref theta k)
                                      (grid:gref theta i)
                                      (phase (grid:gref y k i))))))
                   (* (abs (grid:gref v k))
                      (abs (grid:gref y k h))
                      (sin (- (grid:gref theta k)
                              (grid:gref theta h)
                              (phase (grid:gref y k h)))))))
             (update-dp/dtheta-matrix (m v theta y)
               (when (integerp verbose)
                 (when (> verbose 5)
                   (printout *standard-output* :message "entering update-dp/dtheta-matrix().~&")))
               (loop
                 with node-a = nil
                 with row = 0
                 for i from 0 below (grid:dim0 theta)
                 do
                    (setq node-a (first (select-element :predicate (where-node :tag i)
                                                        :elements nodes)))
                    (when node-a
                      (when (cond
                              ((interconnection-p node-a) t)
                              ((or (load-p node-a)
                                   (generation-p node-a))
                               (cond
                                 ((p-q-p (node-struct-bond node-a)) t)
                                 ((p-v-p (node-struct-bond node-a)) t)
                                 ((q-v-p (node-struct-bond node-a)) t)
                                 ((v=f[Q]-p (node-struct-bond node-a)) t))))
                        (loop
                          with node-b = nil
                          with column = 0
                          for j from 0 below (grid:dim0 theta)
                          do
                             (setq node-b (first (select-element :predicate (where-node :tag j)
                                                                 :elements nodes)))
                             (when node-b
                               (when (cond
                                       ((interconnection-p node-b) t)
                                       ((or (load-p node-b)
                                            (generation-p node-b))
                                        (cond
                                          ((p-q-p (node-struct-bond node-b)) t)
                                          ((p-v-p (node-struct-bond node-b)) t)
                                          ((q-v-p (node-struct-bond node-b)) t)
                                          ((v=f[Q]-p (node-struct-bond node-b)) t))))
                                 (setf (grid:gref dp/dtheta-matrix row column) (dp/dtheta v theta y i j))
                                 (incf column))))
                        (incf row))))
               (when (integerp verbose)
                 (when (> verbose 5)
                   (printout *standard-output* :message "exiting update-dp/dtheta-matrix().~&~&"))))
             (update-dq/dtheta-matrix (m v theta y)
               (when (integerp verbose)
                 (when (> verbose 5)
                   (printout *standard-output* :message "entering update-dq/dtheta-matrix().~&")))
               (loop
                 with node-a = nil
                 with row = 0
                 for i from 0 below (grid:dim0 v)
                 do
                    (setq node-a (first (select-element :predicate (where-node :tag i)
                                                        :elements nodes)))
                    (when node-a
                      (when (cond
                              ((interconnection-p node-a) t)
                              ((or (generation-p node-a)
                                   (load-p node-a))
                               (cond
                                 ((p-q-p (node-struct-bond node-a)) t)
                                 ((p-v-p (node-struct-bond node-a)) t)
                                 ((q-v-p (node-struct-bond node-a)) t)
                                 ((v=f[Q]-p (node-struct-bond node-a)) t))))
                        (loop
                          with node-b = nil
                          with column = 0
                          for j from 0 below (grid:dim0 theta)
                          do
                             (setq node-b (first (select-element :predicate (where-node :tag j)
                                                                 :elements nodes)))
                             (when node-b
                               (when (cond
                                       ((interconnection-p node-b) t)
                                       ((or (load-p node-b)
                                            (generation-p node-b))
                                        (cond
                                          ((p-q-p (node-struct-bond node-b)) t)
                                          ((p-v-p (node-struct-bond node-b)) t)
                                          ((q-v-p (node-struct-bond node-b)) t)
                                          ((v=f[Q]-p (node-struct-bond node-b)) t))))
                                 (setf (grid:gref dq/dtheta-matrix row column) (dq/dtheta v theta y i j))
                                 (incf column))))
                        (incf row))))
               (when (integerp verbose)
                 (when (> verbose 5)
                   (printout *standard-output* :message "exiting update-dq/dtheta-matrix().~&"))))
             (update-dq/dv-matrix (m v theta y)
               (when (integerp verbose)
                 (when (> verbose 5)
                   (printout *standard-output* :message "entering update-dq/dv-matrix().~&")))
               (loop
                 with node-a = nil
                 with row = 0
                 for i from 0 below (grid:dim0 v)
                 do
                    (setq node-a (first (select-element :predicate (where-node :tag i)
                                                        :elements nodes)))
                    (when node-a
                      (when (cond
                              ((interconnection-p node-a) t)
                              ((or (load-p node-a)
                                   (generation-p node-a))
                               (cond
                                 ((p-q-p (node-struct-bond node-a)) t)
                                 ((v=f[Q]-p (node-struct-bond node-a)) t))))
                        (loop
                          with node-b = nil
                          with column = 0
                          for j from 0 below (grid:dim0 v)
                          do
                             (setq node-b (first (select-element :predicate (where-node :tag j)
                                                                 :elements nodes)))
                             (when node-b
                               (when (cond
                                       ((interconnection-p node-b) t)
                                       ((or (generation-p node-b)
                                            (load-p node-b))
                                        (cond
                                          ((p-q-p (node-struct-bond node-b)) t)
                                          ((v=f[Q]-p (node-struct-bond node-b)) t))))
                                 (setf (grid:gref dq/dv-matrix row column) (dq/dv v theta y i j))
                                 (incf column))))
                        (incf row))))
               (when (integerp verbose)
                 (when (> verbose 5)
                   (printout *standard-output* :message "exiting update-dq/dv-matrix().~&"))))
             (update-dp/dv-matrix (m v theta y)
               (when (integerp verbose)
                 (when (> verbose 5)
                   (printout *standard-output* :message "entering update-dp/dv-matrix().~&")))
               (loop
                 with node-a = nil
                 with row = 0
                 for i from 0 below (grid:dim0 theta)
                 do
                    (setq node-a (first (select-element :predicate (where-node :tag i)
                                                        :elements nodes)))
                    (when node-a
                      (when (cond
                              ((interconnection-p node-a) t)
                              ((or (generation-p node-a)
                                   (load-p node-a))
                               (cond
                                 ((p-q-p (node-struct-bond node-a)) t)
                                 ((v=f[Q]-p (node-struct-bond node-a)) t))))
                        (loop
                          with node-b = nil
                          with column = 0
                          for j from 0 below (grid:dim0 v)
                          do
                             (setq node-b (first (select-element :predicate (where-node :tag j)
                                                                 :elements nodes)))
                             (when node-b
                               (when (cond
                                       ((interconnection-p node-b) t)
                                       ((or (generation-p node-b)
                                            (load-p node-b))
                                        (cond
                                          ((p-q-p (node-struct-bond node-b)) t)
                                          ((v=f[Q]-p (node-struct-bond node-b)) t))))
                                 (setf (grid:gref dp/dv-matrix row column) (dp/dv v theta y i j))
                                 (incf column))))
                        (incf row))))
               (when (integerp verbose)
                 (when (> verbose 5)
                   (printout *standard-output* :message "exiting update-dp/dv-matrix().~&")))))
      (if (problem-struct-threaded problem)
          (progn
            (when (integerp verbose)
              (when (> verbose 10)
                (printout *standard-output* :message "using threads to compute Jacobian.~&")))
            (setq jacobian-threads (list (bt:make-thread #'(lambda ()
                                                             (update-dp/dtheta-matrix dp/dtheta-matrix
                                                                                      (grid:copy voltages-vector)
                                                                                      (grid:copy thetas-vector)
                                                                                      (grid:copy admittances-matrix)))
                                                         :name (symbol-name (gensym "dp/dtheta-thread-")))
                                         (bt:make-thread #'(lambda ()
                                                             (update-dq/dtheta-matrix dq/dtheta-matrix
                                                                                      (grid:copy voltages-vector)
                                                                                      (grid:copy thetas-vector)
                                                                                      (grid:copy admittances-matrix)))
                                                         :name (symbol-name (gensym "dp/dtheta-thread-")))
                                         (bt:make-thread #'(lambda ()
                                                             (update-dp/dv-matrix dp/dv-matrix
                                                                                  (grid:copy voltages-vector)
                                                                                  (grid:copy thetas-vector)
                                                                                  (grid:copy admittances-matrix)))
                                                         :name (symbol-name (gensym "dp/dv-thread-")))
                                         (bt:make-thread #'(lambda ()
                                                             (update-dq/dv-matrix dq/dv-matrix
                                                                                  (grid:copy voltages-vector)
                                                                                  (grid:copy thetas-vector)
                                                                                  (grid:copy admittances-matrix)))
                                                         :name (symbol-name (gensym "dq/dv-thread-")))))
            (loop
              for thread in jacobian-threads
              do
                 (loop
                   while (bt:thread-alive-p thread))
                 (when (integerp verbose)
                   (when (> verbose 10)
                     (printout *standard-output*
                               :message
                               "thread ~s finished.~&"
                               (bt:thread-name thread))))))
          (progn
            (when (integerp verbose)
              (when (> verbose 10)
                (printout *standard-output* :message "sequentially computing Jacobian.~&")))
            (update-dp/dtheta-matrix dp/dtheta-matrix
                                     (grid:copy voltages-vector)
                                     (grid:copy thetas-vector)
                                     (grid:copy admittances-matrix))
            (update-dq/dv-matrix dq/dv-matrix
                                 (grid:copy voltages-vector)
                                 (grid:copy thetas-vector)
                                 (grid:copy admittances-matrix))
            (update-dq/dtheta-matrix dq/dtheta-matrix
                                     (grid:copy voltages-vector)
                                     (grid:copy thetas-vector)
                                     (grid:copy admittances-matrix))
            (update-dp/dv-matrix dp/dv-matrix
                                 (grid:copy voltages-vector)
                                 (grid:copy thetas-vector)
                                 (grid:copy admittances-matrix))))
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
                (printout *standard-output* :message "dp/dtheta = ~s~&" dp/dtheta-matrix)
                (printout *standard-output* :message "dq/dtheta = ~s~&" dq/dtheta-matrix)
                (printout *standard-output* :message "dp/dv = ~s~&" dp/dv-matrix)
                (printout *standard-output* :message "dq/dv = ~s~&" dq/dv-matrix)
                (printout *standard-output* :message "Jacobian = ~s.~&" jacobian-matrix))))
          (progn
            (when (integerp verbose)
              (when (> verbose 10)
                (prinout *standard-output* :error "could not calculate Jacobian matrix.~&")))))
      (when (integerp verbose)
        (when (> verbose 5)
          (printout *standard-output* :message "exiting calculate-jacobian().~%~%")))
      (values jacobian-matrix ok?))))

(defun calculate-currents (&rest parameters &key
                                              (problem nil problem-p)
                                              (voltages-vector nil voltages-vector-p)
                                              (thetas-vector nil thetas-vector-p)
                                              (p-vector nil p-vector-p)
                                              (q-vector nil q-vector-p)
                                              (verbose nil verbose-p))
  (declare (ignorable problem
                      voltages-vector
                      thetas-vector
                      p-vector
                      q-vector
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when voltages-vector-p
    (check-type voltages-vector grid:foreign-array))
  (when thetas-vector-p
    (check-type thetas-vector grid:foreign-array))
  (when p-vector-p
    (check-type p-vector grid:foreign-array))
  (when q-vector-p
    (check-type q-vector grid:foreign-array))
  (when (integerp verbose)
    (when (> verbose 5)
      (printout *standard-output* :message "entering calculate-currents().~&")))
  (let ((nodes (extract-nodes problem))
        (bipoles (extract-bipoles problem))
        (current nil)
        (currents nil)
        (bond-current nil)
        (bond-currents nil)
        (ok? nil))
    (loop
      named nodes-loop
        initially (setq ok? t)
      for node in nodes
      do
         (case (node-struct-kind node)
           (:generation
            (case (bond-struct-kind (node-struct-bond node))
              (:p-v
               (setq bond-current (->phasor (/ (conjugate (complex (bond-struct-active-power (node-struct-bond node))
                                                                   (grid:gref q-vector (node-struct tag node))))
                                               (* (grid:gref voltages-vector node-number)
                                                  (exp (* #c(0d0 1d0)
                                                          (grid:gref thetas-vector node-number)))))))
               (push (list :bond-name (bond-struct-name (node-struct-bond node))
                           :current bond-current)
                     bond-currents)
               (when (integerp verbose)
                 (when (> verbose 10)
                   (printout *standard-output* :message "Generation P-E bond ~a, current = ~s.~%" (bond-struct-name (node-struct-bond node)) bond-current))))
              (:q-v
               (setq bond-current (->phasor (/ (conjugate (complex (grid:gref p-vector (node-struct-tag node))
                                                                   (bond-struct-reactive-power (node-struct-bond node))))
                                               (* (grid:gref voltages-vector (node-struct-tag node))
                                                  (exp (* #c(0d0 1d0)
                                                          (grid:gref thetas-vector (node-struct-tag node))))))))
               (push (list :bond-name (bond-struct-name (node-struct-bond node))
                           :current bond-current)
                     bond-currents)
               (when (integerp verbose)
                 (when (> verbose 10)
                   (printout *standard-output* :message "Generation Q-E bond ~a, current = ~s.~%" (bond-struct-name (node-struct-bond node)) bond-current))))
              (:p-q
               (setq bond-current (->phasor (/ (conjugate (complex (bond-struct-active-power (node-struct-bond node))
                                                                   (bond-struct-reactive-power (node-struct-bond node))))
                                               (* (grid:gref voltages-vector (node-struct-tag node))
                                                  (exp (* #c(0d0 1d0)
                                                          (grid:gref thetas-vector (node-struct-tag node))))))))
               (push (list :bond-name (bond-struct-name (node-struct-bond node))
                           :current bond-current)
                     bond-currents)
               (when (integerp verbose)
                 (when (> verbose 10)
                   (printout *standard-output* :message "Generation P-Q bond ~a, current = ~s.~%" (bond-struct-name (node-struct-bond node)) bond-current))))
              (:v-theta
               (push (list :bond-name (bond-struct-name (node-struct-bond node))
                           :current (->phasor (/ (conjugate (complex (grid:gref p-vector (node-struct-tag node))
                                                                     (grid:gref q-vector (node-struct-tag node))))
                                                 (* (abs (grid:gref voltages-vector (node-struct-tag node)))
                                                    (exp (* #c(0d0 1d0)
                                                            (grid:gref thetas-vector (node-struct-tag node))))))))
                     bond-currents)
               (when (integerp verbose)
                 (when (> verbose 10)
                   (printout *standard-output* :message "Generation V-theta bond ~a, current = ~s.~%" (bond-struct-name (node-struct-bond node)) bond-current))))))
           (:load
            (case (bond-struct-kind (node-struct-bond node))
              (:p-q
               (setq bond-current (->phasor (/ (- (conjugate (complex (bond-struct-active-power (node-struct-bond node))
                                                                      (bond-struct-reactive-power (node-struct-bond node)))))
                                               (* (grid:gref voltages-vector (node-struct-tag node))
                                                  (exp (* #c(0d0 1d0)
                                                          (grid:gref thetas-vector (node-struct-tag node))))))))
               (push (list :bond-name (bond-struct-name (node-struct-bond node))
                           :current bond-current)
                     bond-currents)
               (when (integerp verbose)
                 (when (> verbose 10)
                   (printout *standard-output* :message "Load P-Q bond ~a, current = ~s.~%" (bond-struct-name (node-struct-bond node)) bond-current))))))))
    (when ok?
      (loop
        named bipoles-loop
          initially (setq ok? t)
        with voltage-across = nil
        with nodes-numbers = nil
        for bipole in bipoles
        for i from 0
        do
           (setq nodes-numbers (loop
                                 with node = nil
                                 for node-name in (bipole-struct-nodes bipole)
                                 do
                                    (setq node (first (select-element :predicate (where :name node-name)
                                                                      :elements nodes)))
                                 unless (reference-p node)
                                   collect (node-struct-tag node)))
           (setq voltage-across (case (length nodes-numbers)
                                  (2
                                   (- (* (grid:gref voltages-vector (second nodes-numbers))
                                         (exp (* #c(0d0 1d0)
                                                 (grid:gref thetas-vector (second nodes-numbers)))))
                                      (* (grid:gref voltages-vector (first nodes-numbers))
                                         (exp (* #c(0d0 1d0)
                                                 (grid:gref thetas-vector (first nodes-numbers)))))))
                                  (1
                                   (* (grid:gref voltages-vector (first nodes-numbers))
                                      (exp (* #c(0d0 1d0)
                                              (grid:gref thetas-vector (first nodes-numbers))))))
                                  (t
                                   (when (integerp verbose)
                                     (when (> verbose 10)
                                       (printout *standard-output*
                                                 :error
                                                 "bipole ~a wrong number of nodes (~a).~&"
                                                 (bipole-struct-name bipole)
                                                 (length nodes-numbers))))
                                   (setq ok? nil)
                                   (return-from bipoles-loop))))
           (setq current (product (->phasor voltage-across)
                                  (case (bipole-struct-kind bipole)
                                    ((or :resistance :impedance)
                                     (->phasor (/ 1d0 (getf (bipole-struct-model-parameters bipole) :value))))
                                    (:inductance
                                     (->phasor (/ 1d0 (complex 0d0 (* 2d0
                                                                      pi
                                                                      (problem-struct-frequency problem)
                                                                      (getf (bipole-struct-model-parameters bipole) :value))))))
                                    (:capacitance
                                     (->phasor (complex 0d0 (* 2d0
                                                               pi
                                                               (problem-struct-frequency problem)
                                                               (getf (bipole-struct-model-parameters bipole) :value)))))
                                    (:conductance
                                     (->phasor (getf (bipole-struct-model-parameters bipole) :value)))
                                    (:susceptance
                                     (->phasor (complex 0d0 (getf (bipole-struct-model-parameters bipole) :value))))
                                    (:admittance
                                     (->phasor (getf (bipole-struct-model-parameters bipole) :value))))))
           (push (list :bipole-name (bipole-struct-name bipole)
                       :current current
                       :voltage-across (->phasor voltage-across))
                 currents)
           (when (integerp verbose)
             (when (> verbose 10)
               (printout *standard-output*
                         :message
                         "~a(~{~a~^, ~}) voltage across = ~s, current ~s~%"
                         (bipole-struct-name bipole)
                         nodes-numbers
                         voltage-across
                         current)))))
    (when (integerp verbose)
      (when (> verbose 5)
        (printout *standard-output* :message "exiting calculate-currents().~%~%")))
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
                                        (epsilon 1d-6 epsilon-p)
                                        (maximum-refinement-iteration-count 1000 maximum-refinement-iteration-count-p)
                                        (verbose nil verbose-p))
  (declare (ignorable parameters
                      delta-p-vector
                      delta-q-vector
                      jacobian-matrix
                      cv-matrix
                      ctheta-matrix
                      epsilon
                      maximum-refinement-iteration-count
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
  (when epsilon-p
    (check-type epsilon real)
    (assert (> epsilon 0.0)))
  (when maximum-refinement-iteration-count-p
    (check-type maximum-refinement-iteration-count (integer 1)))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (integerp verbose)
    (when (> verbose 5)
      (printout *standard-output* :message "entering solve-system().~&")))
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
        (printout *standard-output*
                  :message
                  "y = ~s, dim(s) = ~s.~&"
                  y-vector
                  (grid:dimensions y-vector))))
    (handler-case
        (multiple-value-bind (lu-matrix permutation signum)
            (gsll:lu-decomposition (grid:copy jacobian-matrix))
          (declare (ignore signum))
          (let ((residuals (grid:make-foreign-array 'double-float
                                                    :dimensions (grid:dimensions y-vector)
                                                    :initial-element 0d0))
                (initial-solution (gsll:lu-solve (grid:copy lu-matrix)
                                                 (grid:copy y-vector)
                                                 permutation
                                                 t)))
            (loop
              named refinement-loop
              for i from 0 below maximum-refinement-iteration-count
              do
                 (setq x-vector (gsll:lu-refine (grid:copy jacobian-matrix)
                                                (grid:copy lu-matrix)
                                                permutation
                                                (grid:copy y-vector)
                                                initial-solution
                                                residuals))
                 (when (integerp verbose)
                   (when (> verbose 10)
                     (printout *standard-output*
                               :message
                               "Residuals norm: ~a~&"
                               (grid:norm residuals))))
                 (when (< (grid:norm residuals) epsilon)
                   (return-from refinement-loop)))
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
                             (printout *standard-output*
                                       :error
                                       "Solution element is not a number x(~a)!~&"
                                       i)))
                         (setq ok? nil)
                         (return-from solution-check-loop))
                  (when ok?
                    (when (integerp verbose)
                      (when (> verbose 10)
                        (printout *standard-output*
                                  :message
                                  "x = ~s, dim(s) = ~s.~&"
                                  x-vector
                                  (grid:dimensions x-vector))
                        (printout *standard-output*
                                  :message
                                  "permutation = ~s.~&"
                                  permutation)))
                    (setq delta-thetas (gsll:matrix-product (grid:transpose (grid:copy ctheta-matrix))
                                                            (grid:subgrid x-vector (list (grid:dim0 ctheta-matrix)) (list 0)))
                          delta-voltages (gsll:matrix-product (grid:transpose (grid:copy cv-matrix))
                                                              (grid:subgrid x-vector (list (grid:dim0 cv-matrix)) (list (grid:dim0 ctheta-matrix)))))))
                (progn
                  (setq ok? nil)
                  (when (integerp verbose)
                    (when (> verbose 10)
                      (printout *standard-output* :error "no solution.~&")))))))
      (error (e)
        (setq ok? nil)))
    (when (integerp verbose)
      (when (> verbose 10)
        (printout *standard-output* :message "delta-voltages = ~s.~&" delta-voltages)
        (printout *standard-output* :message "delta-thetas = ~s.~&" delta-thetas)))
    (when (integerp verbose)
      (when (> verbose 5)
        (printout *standard-output* :message "exiting solve-system().~%~%")))
    (values delta-thetas
            delta-voltages
            ok?)))

(defun update-solution (&rest parameters &key
                                           (problem nil problem-p)
                                           (delta-voltages-vector nil delta-voltages-vector-p)
                                           (delta-thetas-vector nil delta-thetas-vector-p)
                                           (voltages-vector nil voltages-vector-p)
                                           (thetas-vector nil thetas-vector-p)
                                           (cv-matrix nil cv-matrix-p)
                                           (ctheta-matrix nil ctheta-matrix-p)
                                           (verbose nil verbose-p))
  (declare (ignorable parameters
                      problem
                      delta-voltages-vector
                      delta-thetas-vector
                      voltages-vector
                      thetas-vector
                      cv-matrix
                      ctheta-matrix
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
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
  (when (integerp verbose)
    (when (> verbose 5)
      (printout *standard-output* :message "entering update-solution().~&")))
  (let ((ok? nil))
    (loop
      named nodes-loop
      finally (setq ok? t)
      for node in (extract-nodes problem)
      do
         (case (node-struct-kind node)
           (:generation
            (case (bond-struct-kind (node-struct-bond node))
              (:p-q
               (incf (grid:gref voltages-vector (node-struct-tag node)) (* (problem-struct-beta problem)
                                                                           (grid:gref delta-voltages-vector (node-struct-tag node))))
               (incf (grid:gref thetas-vector (node-struct-tag node)) (* (problem-struct-alpha problem)
                                                                         (grid:gref delta-thetas-vector (node-struct-tag node)))))
              ((or :p-v :q-v)
               (setf (grid:gref voltages-vector (node-struct-tag node)) (bond-struct-voltage-magnitude (node-struct-bond node)))
               (incf (grid:gref thetas-vector (node-struct-tag node)) (* (problem-struct-alpha problem)
                                                                         (grid:gref delta-thetas-vector (node-struct-tag node)))))
              (:v-theta
               (setf (grid:gref voltages-vector (node-struct-tag node)) (bond-struct-voltage-magnitude (node-struct-bond node))
                     (grid:gref thetas-vector (node-struct-tag node)) (bond-struct-voltage-phase (node-struct-bond node))))))
           (:load
            (case (bond-struct-kind (node-struct-bond node))
              (:v=f[Q]
               ())
              (:p-q
               (incf (grid:gref voltages-vector (node-struct-tag node)) (* (problem-struct-beta problem)
                                                                           (grid:gref delta-voltages-vector (node-struct-tag node))))
               (incf (grid:gref thetas-vector (node-struct-tag node)) (* (problem-struct-alpha problem)
                                                                         (grid:gref delta-thetas-vector (node-struct-tag node)))))))
           (:interconnection
            (incf (grid:gref voltages-vector (node-struct-tag node)) (* (problem-struct-beta problem)
                                                                        (grid:gref delta-voltages-vector (node-struct-tag node))))
            (incf (grid:gref thetas-vector (node-struct-tag node)) (* (problem-struct-alpha problem)
                                                                      (grid:gref delta-thetas-vector (node-struct-tag node)))))))
    (when (integerp verbose)
      (when (> verbose 10)
        (printout *standard-output* :message "voltages vector = ~s.~&" voltages-vector)
        (printout *standard-output* :message "thetas vector = ~s.~&" thetas-vector)))
    (when (integerp verbose)
      (when (> verbose 5)
        (printout *standard-output* :message "exiting update-solution().~%~%")))
    ok?))

(defun check-power-tolerance (&rest parameters &key
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
      (printout *standard-output* :message "entering check-power-tollerance().~&")))
  (let ((ok? nil))
    (loop
      named nodes-loop
      finally (setq ok? t)
      for node in (extract-nodes problem)
      unless (reference-p node)
        do
           (when (>= (abs (grid:gref delta-p-vector (node-struct-tag node)))
                     (realpart (problem-struct-epsilon-power problem)))
             (when (integerp verbose)
               (when (> verbose 10)
                 (printout *standard-output*
                           :message
                           "active power is over the selected tolerance |~a| > ~a.~&"
                           (grid:gref delta-p-vector (node-struct-tag node))
                           (realpart (problem-struct-epsilon-power problem)))))
             (return-from nodes-loop))
           (when (>= (abs (grid:gref delta-q-vector (node-struct-tag node)))
                     (imagpart (problem-struct-epsilon-power problem)))
             (when (integerp verbose)
               (when (> verbose 10)
                 (printout *standard-output*
                           :message
                           "reactive power is over the selected tolerance |~a| > ~a.~&"
                           (grid:gref delta-q-vector (node-struct-tag node))
                           (imagpart (problem-struct-epsilon-power problem)))))
             (return-from nodes-loop)))
    (when ok?
      (when (integerp verbose)
        (when (> verbose 10)
          (printout *standard-output* :message "power requirements met.~&"))))
    (when (integerp verbose)
      (when (> verbose 5)
        (printout *standard-output* :message "exiting check-power-tollerance().~%~%")))
    ok?))

(defun output-solution (&rest parameters &key
                                           (problem nil problem-p)
                                           (voltages-vector nil voltages-vector-p)
                                           (thetas-vector nil thetas-vector-p)
                                           (p-vector nil p-vector-p)
                                           (q-vector nil q-vector-p)
                                           (iterations-count nil iterations-count-p)
                                           (stream-object *standard-output* stream-object-p)
                                           (verbose nil verbose-p))
  "Printout the computed solution for the powerflow problem."
  (declare (ignorable parameters
                      problem
                      voltages-vector
                      thetas-vector
                      p-vector
                      q-vector
                      iterations-count
                      stream-object
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when voltages-vector-p
    (check-type voltages-vector grid:foreign-array))
  (when thetas-vector-p
    (check-type thetas-vector grid:foreign-array))
  (when p-vector-p
    (check-type p-vector grid:foreign-array))
  (when q-vector-p
    (check-type q-vector grid:foreign-array))
  (when iterations-count-p
    (check-type iterations-count (unsigned-byte 64)))
  (when stream-object-p
    (check-type stream-object stream))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (integerp verbose)
    (when (> verbose 10)
      (printout stream-object :message "entering output-solution().~&")))
  (let* ((nodes (remove-if-not #'(lambda (x)
                                   (typep x 'node-struct))
                               (problem-struct-network problem)))
         (voltages nil)
         (powers nil))
    (multiple-value-setq (powers voltages)
      (loop
        with node-name = nil
        for i from 0 below (grid:dim0 p-vector)
        do
           (setq node-name (node-struct-name (first (select-element :predicate (where :tag i)
                                                                    :elements nodes))))
        collect (list :node-name node-name
                      :magnitude (grid:gref voltages-vector i)
                      :argument (grid:gref thetas-vector i))
          into voltages
        collect (list :node-name node-name
                      :active-power (grid:gref p-vector i)
                      :reactive-power (grid:gref q-vector i))
          into powers
        finally (return (values powers voltages))))
    (multiple-value-bind (currents bond-currents ok?)
        (calculate-currents :problem problem
                            :voltages-vector voltages-vector
                            :thetas-vector thetas-vector
                            :p-vector p-vector
                            :q-vector q-vector
                            :verbose verbose)
      (if ok?
          (progn
            (setf (problem-struct-solution problem) (list (list :powers powers)
                                                          (list :voltages voltages)
                                                          (list :currents currents)
                                                          (list :bond-currents bond-currents)
                                                          (list :iterations-count iterations-count)))
            (when (integerp verbose)
              (when (> verbose 10)
                (progn
                  (printout stream-object :message "Voltages = ~s~%" voltages)
                  (printout stream-object :message "Currents = ~s~%" currents)
                  (printout stream-object :message "Bond Currents = ~s~%" bond-currents)))))
          (when (integerp verbose)
            (when (> verbose 10)
              (printout stream-object :error "could not calculate currents.~&"))))
      (when (integerp verbose)
        (when (> verbose 5)
          (printout stream-object :message "exiting output-solution().~%~%")))
      (values (problem-struct-solution problem) ok?))))

(defun loadflow (&rest parameters &key
                                    (problem nil problem-p)
                                    (problem-file-pathname nil problem-file-pathname-p)
                                    (maximum-iterations-count 10 maximum-iterations-count-p)
                                    (minimum-iterations-count 1 minimum-iterations-count-p)
                                    (alpha 1d0 alpha-p)
                                    (beta 1d0 beta-p)
                                    (epsilon-power #c(1d3 1d3) epsilon-power-p)
                                    (epsilon 1d-6 epsilon-p)
                                    (maximum-refinement-iteration-count 1000 maximum-refinement-iteration-count-p)
                                    (verbose nil verbose-p))
  "The main function to solve the powerflow problem."
  (declare (ignorable parameters
                      problem
                      problem-file-pathname
                      maximum-iterations-count
                      minimum-iterations-count
                      alpha
                      beta
                      epsilon-power
                      epsilon
                      maximum-refinement-iteration-count
                      verbose))
  (when (and problem-p problem-file-pathname)
    (error 'wrong-parameters-error :parameters (list problem problem-file-pathname)))
  (when problem-p
    (check-type problem problem-struct))
  (when problem-file-pathname-p
    (check-type problem-file-pathname pathname))
  (when maximum-iterations-count-p
    (check-type maximum-iterations-count (integer 1)))
  (when minimum-iterations-count-p
    (check-type minimum-iterations-count (integer 1)))
  (when alpha-p
    (check-type alpha real)
    (assert (> alpha 0)))
  (when beta-p
    (check-type beta real)
    (assert (> beta 0)))
  (when epsilon-power-p
    (check-type epsilon-power complex)
    (assert (> (abs epsilon-power) 0)))
  (when epsilon-p
    (check-type epsilon real)
    (assert (> epsilon 0.0)))
  (when maximum-refinement-iteration-count-p
    (check-type maximum-refinement-iteration-count (integer 1)))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (and verbose-p
             (> verbose 5))
    (printout *standard-output* :message "entering loadflow().~&"))
  (let ((cv-matrix nil)
        (ctheta-matrix nil)
        (voltages-vector nil)
        (thetas-vector nil)
        (p-vector nil)
        (q-vector nil)
        (delta-p-vector nil)
        (delta-q-vector nil)
        (admittances-matrix nil)
        (dp/dtheta-matrix nil)
        (dq/dtheta-matrix nil)
        (dp/dv-matrix nil)
        (dq/dv-matrix nil)
        (jacobian-matrix nil)
        (iteration nil)
        (return-value nil)
        (ok? nil)
        (state 'idle))
    (loop
      named state-machine-loop
      do
         (case state
           (idle
            (if problem-p
                (setq state 'setup-problem)
                (setq state 'get-input-data)))
           (get-input-data
            (multiple-value-setq (problem ok?)
              (load-problem :problem-file-pathname problem-file-pathname
                            :verbose verbose))
            (if ok? ;; is input data ok?
                (setq state 'setup-problem)
                (setq state 'exit-with-error)))
           (setup-problem
            (if (setup-problem :problem problem
                               :verbose verbose)
                (progn
                  (when maximum-iterations-count-p
                    (when (integerp verbose)
                      (when (> verbose 5)
                        (printout *standard-output*
                                  :warning "overriding problem maximum iterations count ~a with ~a.~&"
                                  (problem-struct-maximum-iterations-count problem)
                                  maximum-iterations-count)))
                    (setf (problem-struct-maximum-iterations-count problem) maximum-iterations-count))
                  (when minimum-iterations-count-p
                    (when (integerp verbose)
                      (when (> verbose 5)
                        (printout *standard-output*
                                  :warning "overriding problem minimum iterations count ~a with ~a.~&"
                                  (problem-struct-minimum-iterations-count problem)
                                  minimum-iterations-count)))
                    (setf (problem-struct-minimum-iterations-count problem) minimum-iterations-count))
                  (when alpha-p
                    (when (integerp verbose)
                      (when (> verbose 5)
                        (printout *standard-output*
                                  :warning "overriding problem alpha value ~a with ~a.~&"
                                  (problem-struct-alpha problem)
                                  alpha)))
                    (setf (problem-struct-alpha problem) alpha))
                  (when beta-p
                    (when (integerp verbose)
                      (when (> verbose 5)
                        (printout *standard-output*
                                  :warning "overriding problem beta value ~a with ~a.~&"
                                  (problem-struct-beta problem)
                                  alpha)))
                    (setf (problem-struct-beta problem) beta))
                  (when epsilon-power-p
                    (when (integerp verbose)
                      (when (> verbose 5)
                        (printout *standard-output*
                                  :warning "overriding problem epsilon power value ~a with ~a.~&"
                                  (problem-struct-epsilon-power problem)
                                  epsilon-power)))
                    (setf (problem-struct-epsilon-power problem) epsilon-power))
                  (setq state 'create-connection-matrices))
                (progn
                  (error 'problem-setup-error :problem-name (problem-struct-name problem))
                  (setq state 'exit-with-error))))
           (create-connection-matrices
            (multiple-value-setq (cv-matrix ctheta-matrix ok?)
              (create-connection-matrices :problem problem
                                          :verbose verbose))
            (if ok?
                (setq state 'create-vectors)
                (progn
                  (when (integerp verbose) ;; connection matrices are not created.
                    (when (> verbose 10)
                      (printout *standard-output* :error "could not create connection matrices.~&")))
                  (setq state 'exit-with-error))))
           (create-vectors
            (multiple-value-setq (voltages-vector thetas-vector p-vector q-vector delta-p-vector delta-q-vector ok?)
              (create-vectors :problem problem
                              :verbose verbose))
            (if ok?
                (setq state 'create-admittances-matrix)
                (progn
                  (when (integerp verbose) ;; vectors were not created.
                    (when (> verbose 10)
                      (printout *standard-output* :error "could not create vectors.~&")))
                  (setq state 'exit-with-error))))
           (create-admittances-matrix
            (multiple-value-setq (admittances-matrix ok?)
              (create-admittances-matrix :problem problem
                                         :verbose verbose))
            (if ok?
                (setq state 'create-jacobian-submatrices)
                (progn
                  (when (integerp verbose) ;; admittances matrix was not created.
                    (when (> verbose 10)
                      (printout *standard-output* :error "could no create the admittances matrix.~&")))
                  (setq state 'exit-with-error))))
           (create-jacobian-submatrices
            (multiple-value-setq (dp/dtheta-matrix dq/dtheta-matrix dp/dv-matrix dq/dv-matrix ok?)
              (create-jacobian-submatrices :problem problem
                                           :verbose verbose))
            (if ok?
                (setq state 'assign-initial-solution)
                (progn
                  (when (integerp verbose) ;; Jacobian submatrices were not created.
                    (when (> verbose 10)
                      (printout *standard-output* :error "could not create the Jacobian matrix.~&")))
                  (setq state 'exit-with-error))))
           (assign-initial-solution
            (multiple-value-setq (voltages-vector thetas-vector ok?)
              (assign-initial-solution :problem problem
                                       :voltages-vector voltages-vector
                                       :thetas-vector thetas-vector
                                       :verbose verbose))
            (if ok?
                (setq state 'main-loop)
                (progn
                  (when (integerp verbose) ;; initial solution not assigned.
                    (when (> verbose 10)
                      (printout *standard-output* :error "could not assign an initial solution.~&")))
                  (setq state 'exit-with-error))))
           (main-loop
            (when (integerp verbose)
              (when (> verbose 10)
                (printout *standard-output* :message "entering main loop.~%~%")))
            (setq state 'first-time-calculate-power-residuals))
           (first-time-calculate-power-residuals
            (multiple-value-setq (p-vector q-vector delta-p-vector delta-q-vector ok?)
              (calculate-power-residuals :problem problem
                                         :admittances-matrix admittances-matrix
                                         :voltages-vector voltages-vector
                                         :thetas-vector thetas-vector
                                         :p-vector p-vector
                                         :q-vector q-vector
                                         :delta-p-vector delta-p-vector
                                         :delta-q-vector delta-q-vector
                                         :verbose verbose))
            (if ok?
                (setq state 'solving-loop-setup)
                (progn
                  (when (integerp verbose)
                    (when (> verbose 10)
                      (printout *standard-output* :error "could not compute initial power residuals values.~&")))
                  (setq state 'exit-with-error))))
           (solving-loop-setup
            (when (integerp verbose)
              (when (> verbose 10)
                (printout *standard-output* :message "entering solving loop.~%~%")))
            (setq iteration 1
                  state 'solving-loop))
           (solving-loop
            (if (> iteration (problem-struct-maximum-iterations-count problem))
                (progn
                  (when (integerp verbose)
                    (when (> verbose 10)
                      (printout *standard-output*
                                :error "maximum iterations count reached (~a). No solution.~&"
                                (problem-struct-maximum-iterations-count problem))))
                  (setq state 'exit-with-error))
                (progn
                  (when (integerp verbose)
                    (when (> verbose 10)
                      (printout *standard-output* :message "iteration #~a~%~%" iteration)))
                  (setq state 'calculate-jacobian-matrix))))
           (calculate-jacobian-matrix
            (multiple-value-setq (jacobian-matrix ok?)
              (calculate-jacobian :problem problem
                                  :dp/dtheta-matrix dp/dtheta-matrix
                                  :dq/dtheta-matrix dq/dtheta-matrix
                                  :dp/dv-matrix dp/dv-matrix
                                  :dq/dv-matrix dq/dv-matrix
                                  :admittances-matrix admittances-matrix
                                  :voltages-vector voltages-vector
                                  :thetas-vector thetas-vector
                                  :verbose verbose))
            (if ok? ;; Jacobian is ok!
                (setq state 'solve-system)
                (progn
                  (when (integerp verbose)
                    (when (> verbose 10)
                      (printout *standard-output* :error "could not calculate Jacobian matrix.~&")))
                  (setq state 'exit-with-error))))
           (solve-system
            (multiple-value-setq (delta-thetas-vector delta-voltages-vector ok?)
              (solve-system :delta-p-vector delta-p-vector
                            :delta-q-vector delta-q-vector
                            :jacobian-matrix jacobian-matrix
                            :cv-matrix cv-matrix
                            :ctheta-matrix ctheta-matrix
                            :epsilon epsilon
                            :maximum-refinement-iteration-count maximum-refinement-iteration-count
                            :verbose verbose))
            (if ok? ;; system is solved.
                (setq state 'update-solution)
                (progn
                  (when (integerp verbose)
                    (when (> verbose 10)
                      (printout *standard-output* :error "could not solve system.~&")))
                  (setq state 'exit-with-error))))
           (update-solution
            (if (update-solution :problem problem
                                 :delta-voltages-vector delta-voltages-vector
                                 :delta-thetas-vector delta-thetas-vector
                                 :voltages-vector voltages-vector
                                 :thetas-vector thetas-vector
                                 :cv-matrix cv-matrix
                                 :ctheta-matrix ctheta-matrix
                                 :verbose verbose)
                (setq state 'calculate-power-residuals)
                (progn
                  (when (integerp verbose)
                    (when (> verbose 10)
                      (printout *standard-output* :error "could not update solution.~&")))
                  (setq state 'exit-with-error))))
           (calculate-power-residuals
            (multiple-value-setq (p-vector q-vector delta-p-vector delta-q-vector ok?)
              (calculate-power-residuals :problem problem
                                         :admittances-matrix admittances-matrix
                                         :voltages-vector voltages-vector
                                         :thetas-vector thetas-vector
                                         :p-vector p-vector
                                         :q-vector q-vector
                                         :delta-p-vector delta-p-vector
                                         :delta-q-vector delta-q-vector
                                         :verbose verbose))
            (if ok?
                (setq state 'check-power-tolerance)
                (progn
                  (when (integerp verbose)
                    (when (> verbose 10)
                      (printout *standard-output* :error "could not compute power residuals.~&")))
                  (setq state 'exit-with-error))))
           (check-power-tolerance
            (if (and (check-power-tolerance :problem problem
                                            :delta-p-vector delta-p-vector
                                            :delta-q-vector delta-q-vector
                                            :verbose verbose)
                     (> iteration (problem-struct-minimum-iterations-count problem)))
                (setq state 'output-solution)
                (progn
                  (incf iteration)
                  (setq state 'solving-loop))))
           (output-solution
            (multiple-value-bind (solution ok?)
                (output-solution :problem problem
                                 :voltages-vector voltages-vector
                                 :thetas-vector thetas-vector
                                 :p-vector p-vector
                                 :q-vector q-vector
                                 :iterations-count iteration
                                 :verbose verbose)
              (if ok?
                  (progn
                    (setf (problem-struct-solution problem) solution
                          return-value problem)
                    (setq state 'exit))
                  (progn
                    (when (integerp verbose)
                      (when (> verbose 10)
                        (printout *standard-output* :error "could not output solution.~&")))
                    (setq state 'exit-with-error)))))
           (exit
            (setq ok? t)
            (return-from state-machine-loop))
           (exit-with-error
            (setq ok? nil)
            (return-from state-machine-loop))))
    return-value))

(defun main (argv)
  "Main function which is called by the shell."
  (declare (ignore argv))
  (print (funcall #'loadflow argv))
  (sb-ext:quit :unix-status 0))
