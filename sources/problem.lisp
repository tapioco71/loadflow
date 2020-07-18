;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; problem.lisp
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

(defparameter *solver-types* '(:newton-raphson-polar
                               :newton-raphson-complex
                               :carpentier
                               :stott
                               :ward-hale
                               :glimn-stagg))

;; Structures.

(defstruct (problem-struct (:constructor make-problem))
  (name "" :type (or symbol string null))
  (solver-type :newton-raphson-polar :type (or keyword null))
  (maximum-iterations-count 100 :type (integer 1))
  (epsilon-power nil :type (or real complex phasor-struct null))
  (alpha nil :type (or (array * (*)) grid:foreign-array null))
  (beta nil :type (or (array * (*)) grid:foreign-array null))
  (frequency nil :type (or real null))
  (author "" :type (or string null))
  (date 0 :type (or (unsigned-byte 64) null))
  (date-modified 0 :type (or (unsigned-byte 64) null))
  (network nil :type list)
  (solution nil :type list)
  (data nil :type list))

;; Functions.

(defun make-problem (&rest parameters &key
                                        (name (symbol-name (gensym "problem-")) name-p)
                                        (solver-type :newton-raphson-polar solver-type-p)
                                        (maximum-iterations-count 100 maximum-iterations-count-p)
                                        (epsilon-power #c(1d-3 1d-3) epsilon-power-p)
                                        (alpha nil alpha-p)
                                        (beta nil beta-p)
                                        (frequency 50d0 frequency-p)
                                        (author "" author-p)
                                        (date (get-universal-time) date-p)
                                        (date-modified (get-universal-time) date-modified-p)
                                        (network nil network-p)
                                        (solution nil solution-p)
                                        (data nil data-p))
  "Create a new object of problem structure type."
  (declare (ignorable parameters
                      name
                      solver-type
                      maximum-iterations-count
                      epsilon-power
                      alpha
                      beta
                      frequency
                      author
                      date
                      date-modified
                      network
                      solution
                      data))
  (when name-p
    (check-type name (or symbol string null)))
  (when solver-type-p
    (assert (member solver-type *solver-types* :test #'equalp)))
  (when maximum-iterations-count-p
    (check-type maximum-iterations-count (integer 1)))
  (when epsilon-power-p
    (check-type epsilon-power (or real complex phasor-struct)))
  (when alpha-p
    (check-type alpha (or (array * (*)) grid:foreign-array null)))
  (when beta-p
    (check-type beta (or (array * (*)) grid:foreign-array null)))
  (when frequency-p
    (check-type frequency (or real null)))
  (when author-p
    (check-type author (or string null)))
  (when date-p
    (check-type date (or (unsigned-byte 64) null)))
  (when date-modified-p
    (check-type date-modified (or (unsigned-byte 64) null)))
  (when network-p
    (check-type network list)
    (loop
       for x in network
       do
         (check-type x network-element-struct)))
  (when solution-p
    (check-type solution list))
  (when data-p
    (check-type data list))
  (let ((object (allocate-instance (find-class 'problem-struct))))
    (setf (problem-struct-name object) name
          (problem-struct-solver-type object) solver-type
          (problem-struct-maximum-iterations-count object) maximum-iterations-count
          (problem-struct-epsilon-power object) epsilon-power
          (problem-struct-alpha object) (if alpha
                                            (grid:copy-to alpha 'grid:foreign-array 'double-float)
                                            nil)
          (problem-struct-beta object) (if beta
                                           (grid:copy-to beta 'grid:foreign-array 'double-float)
                                           nil)
          (problem-struct-frequency object) frequency
          (problem-struct-author object) author
          (problem-struct-date object) date
          (problem-struct-date-modified object) date-modified
          (problem-struct-network object) network
          (problem-struct-solution object) solution
          (problem-struct-data object) data)
    object))

;; Other functions related to the problem struct.

(defun load-problem (&rest parameters &key
                                        (problem-file-pathname #p"problem.dat" problem-file-pathname-p)
                                        (verbose nil verbose-p))
  "Load a problem from disk."
  (declare (ignorable parameters
                      problem-file-pathname
                      verbose))
  (when problem-file-pathname-p
    (check-type problem-file-pathname pathname))
  (when (integerp verbose)
    (when (> verbose 5)
      (printout :message "entering load-problem().~&")))
  (let ((*package* (find-package 'loadflow))
        (data nil)
        (ok? nil))
    (with-open-file (s problem-file-pathname)
      (when (integerp verbose)
        (when (> verbose 10)
          (printout :message "Loading problem file ~s~&" problem-file-pathname)))
      (setq data (read s)
            ok? t))
    (when (integerp verbose)
      (when (> verbose 10)
        (printout :message "problem = ~s.~&" data)))
    (when (integerp verbose)
      (when (> verbose 5)
        (printout :message "exiting load-problem().~%~%")))
    (values data
            ok?)))

(defun get-input-data (&rest parameters &key
                                          (problem-file-pathname nil problem-file-pathname-p)
                                          (verbose nil verbose-p))
  (declare (ignorable parameters
                      problem-file-pathname
                      verbose))
  (when problem-file-pathname-p
    (check-type problem-file-pathname pathname))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (and verbose-p
             (> verbose 5))
    (printout :message "get-input-data()~&"))
  (multiple-value-bind (problem ok?)
      (load-problem :problem-file-pathname problem-file-pathname
                    :verbose verbose)
    (if ok?
        (when (and verbose-p
                   (> verbose 10))
          (printout :message "problem ~a loaded.~&" (problem-struct-name problem)))
        (when (and verbose-p
                   (> verbose 10))
          (printout :error "could not load problem in file ~s.~&" problem-file-pathname)))
    (when (and verbose-p
               (> verbose 5))
      (printout :message "exiting get-input-data().~%~%"))
    (values problem
            ok?)))

(defun setup-problem (&rest parameters &key
                                         (problem nil problem-p)
                                         (verbose nil verbose-p))
  "Setup the loadflow problem."
  (declare (ignorable parameters
                      problem
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (and (numberp verbose)
             (> verbose 5))
    (printout :message "entering setup-problem().~&"))
  (let ((nodes-table (make-hash-table :test #'equalp))
        (elements-table (make-hash-table :test #'equalp))
        (return-value nil)
        (ok? nil))
    (loop
       named state-machine-loop
       with state = 'nodes-check
       do
         (case state
           (nodes-check
            (when (and verbose-p
                       (> verbose 10))
              (printout :message "check nodes syntax.~&"))
            (loop
               named nodes-check-loop
               for element in (problem-struct-network problem)
               with nodes-count = 0
               initially (setq state 'no-reference-node-error)
               do
                 (typecase element
                   (node-struct
                    (case (node-struct-kind element)
                      ((or :load :generation :voltage :interconnection)
                       (when (and verbose-p
                                  (> verbose 10))
                         (printout :message "adding node ~a of type ~a as number ~a.~&" (node-struct-name element) (node-struct-kind element) nodes-count))
                       (setf (gethash (node-struct-name element) nodes-table) nodes-count)
                       (incf nodes-count))
                      (:reference
                       (when (and verbose-p
                                  (> verbose 10))
                         (printout :message "adding node ~a as network reference.~&" (node-struct-name element)))
                       (setf (gethash (node-struct-name element) nodes-table) -1)
                       (setq state 'elements-check))
                      (t
                       (setq state 'error-handling)
                       (when (and verbose-p
                                  (> verbose 10))
                         (printout :error "node ~a unknow type ~s.~&" (node-struct-name element) (node-struct-kind element)))
                       (return-from nodes-check-loop))))
                   (t
                    ()))))
           (elements-check
            (when (and verbose-p
                       (> verbose 10))
              (printout :message "check elements syntax.~&"))
            (loop
               named elements-check-loop
               initially (setq state 'setup-return-values
                               ok? t)
               for element in (problem-struct-network problem)
               do
                 (typecase element
                   (bipole-struct
                    (when (and (numberp verbose)
                               (> verbose 10))
                      (printout :message "adding bipole ~a of type ~s and nodes ~s.~&" (bipole-struct-name element) (bipole-struct-kind element) (bipole-struct-nodes element)))
                    (if (= (length (bipole-struct-nodes element)) 2)
                        (loop
                           for node-name in (bipole-struct-nodes element)
                           with nodes-numbers = ()
                           finally (setf (gethash (bipole-struct-name element) elements-table) nodes-numbers)
                           do
                             (multiple-value-bind (node-number valid?)
                                 (gethash node-name nodes-table)
                               (if valid?
                                   (when (>= node-number 0)
                                     (push node-number nodes-numbers))
                                   (progn
                                     (setq state 'error-handling)
                                     (when (and (numberp verbose)
                                                (> verbose 10))
                                       (printout :error "in bipole ~a no node named ~a in the network.~&" (bipole-struct-name element) node-name))
                                     (return-from elements-check-loop)))))
                        (progn
                          (setq state 'error-handling)
                          (when (and (numberp verbose)
                                     (> verbose 10))
                            (printout :error "bipole ~a hasn't got 2 connection nodes.~&" (bipole-struct-name element)))
                          (return-from elements-check-loop))))
                   (multipole-struct
                    (setq state 'error-handling)
                    (when (and (numberp verbose)
                               (> verbose 10))
                      (printout :error "multipoles are not implemented yet, check element named ~a.~&" (multipole-struct-name element)))
                    (return-from elements-check-loop))
                   (node-struct
                    ())
                   (t
                    (setq state 'error-handling)
                    (when (and (numberp verbose)
                               (> verbose 10))
                      (printout :error "unknown element ~s in the network.~&" element))
                    (return-from elements-check-loop)))))
           (no-reference-node-error
            (when (and (numberp verbose)
                       (> verbose 10))
              (printout :error "no reference node in the network.~&"))
            (setq state 'error-handling))
           (error-handling
            (setq state 'exit-state-machine))
           (setup-return-values
            (setq state 'exit-state-machine))
           (exit-state-machine
            (return-from state-machine-loop))
           (t
            (when (and (numberp verbose)
                       (> verbose 10))
              (printout :error "unknown state ~s.~&" state))
            (setq state 'error-handling))))
    (when (and (numberp verbose)
               (> verbose 10))
      (printout :message "exiting setup-problem().~%~%"))
    (values nodes-table
            elements-table
            ok?)))

(defun create-connection-matrices (&rest parameters &key
                                                      (problem nil problem-p)
                                                      (nodes-table nil nodes-table-p)
                                                      (elements-table nil elements-table-p)
                                                      (verbose nil verbose-p))
  "The connection matrix take in account the reduced unknown matrix which is
   used as unknown in the solving system Delta A = J X."
  (declare (ignorable parameters
                      problem
                      nodes-table
                      elements-table
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when nodes-table-p
    (check-type nodes-table hash-table))
  (when elements-table-p
    (check-type elements-table hash-table))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (and verbose-p
             (> verbose 5))
    (printout :message "entering create-connection-matrices().~&"))
  (let* ((nodes-count (1- (hash-table-count nodes-table)))
         (temp-cv-matrix (make-array (list 0 0) :element-type 'double-float :initial-element 0d0 :adjustable t))
         (temp-ctheta-matrix (make-array (list 0 0) :element-type 'double-float :initial-element 0d0 :adjustable t))
         (ok? nil))
    (when (> nodes-count 0)
      (with-hash-table-iterator (nodes-table-iterator nodes-table)
        (loop
           named nodes-loop
           initially (setq ok? t)
           with node = nil
           with generation-nodes-count = 0
           with load-nodes-count = 0
           with unknown-voltages-count = 0
           with unknown-thetas-count = 0
           finally (unless (= (+ (* 2 load-nodes-count)
                                 generation-nodes-count)
                              (+ unknown-thetas-count
                                 unknown-voltages-count))
                     (when (and verbose-p
                                (> verbose 10))
                       (printout :error
                                 "system equations vs unknown is not consistent (~a /= ~a).~&"
                                 (+ (* 2 load-nodes-count)
                                    generation-nodes-count)
                                 (+ unknown-thetas-count
                                    unknown-voltages-count)))
                     (setq ok? nil))
           do
             (multiple-value-bind (more? node-name node-number)
                 (nodes-table-iterator)
               (if more?
                   (progn
                     (setq node (first (select-element :predicate (where :name node-name)
                                                       :network (problem-struct-network problem))))
                     (case (node-struct-kind node)
                       (:generation
                        (case (bond-struct-kind (node-struct-bond node))
                          ((or :p-v :q-v)
                           (adjust-array temp-ctheta-matrix
                                         (list (1+ unknown-thetas-count) nodes-count)
                                         :element-type 'double-float)
                           (setf (aref temp-ctheta-matrix unknown-thetas-count node-number) 1d0)
                           (incf unknown-thetas-count))
                          (:p-q
                           (adjust-array temp-cv-matrix
                                         (list (1+ unknown-voltages-count) nodes-count)
                                         :element-type 'double-float)
                           (adjust-array temp-ctheta-matrix
                                         (list (1+ unknown-thetas-count) nodes-count)
                                         :element-type 'double-float)
                           (setf (aref temp-cv-matrix unknown-voltages-count node-number) 1d0
                                 (aref temp-ctheta-matrix unknown-thetas-count node-number) 1d0)
                           (incf unknown-voltages-count)
                           (incf unknown-thetas-count)
                           (incf generation-nodes-count))
                          (:v-theta
                           (incf generation-nodes-count))
                          (t
                           (setq ok? nil)
                           (when (integerp verbose)
                             (when (> verbose 5)
                               (printout :error
                                         "bond ~a unknown kind ~a for generation node ~a.~&"
                                         (bond-struct-name (node-struct-bond node))
                                         (bond-struct-kind (node-struct-bond node))
                                         (node-struct-name node))))
                           (return-from nodes-loop))))
                       (:load
                        (case (bond-struct-kind (node-struct-bond node))
                          (:v=f(Q)
                           ())
                          (:p-q
                           (adjust-array temp-cv-matrix
                                         (list (1+ unknown-voltages-count) nodes-count)
                                         :element-type 'double-float)
                           (adjust-array temp-ctheta-matrix
                                         (list (1+ unknown-thetas-count) nodes-count)
                                         :element-type 'double-float)
                           (setf (aref temp-cv-matrix unknown-voltages-count node-number) 1d0
                                 (aref temp-ctheta-matrix unknown-thetas-count node-number) 1d0)
                           (incf unknown-voltages-count)
                           (incf unknown-thetas-count)
                           (incf load-nodes-count))
                          (t
                           (setq ok? nil)
                           (when (integerp verbose)
                             (when (> verbose 5)
                               (printout :error
                                         "bond ~a unknown kind ~a for load node ~a.~&"
                                         (bond-struct-name (node-struct-bond node))
                                         (bond-struct-kind (node-struct-bond node))
                                         (node-struct-name node))))
                           (return-from nodes-loop))))
                       (:interconnection
                        (adjust-array temp-cv-matrix
                                      (list (1+ unknown-voltages-count) nodes-count)
                                      :element-type 'double-float)
                        (adjust-array temp-ctheta-matrix
                                      (list (1+ unknown-thetas-count) nodes-count)
                                      :element-type 'double-float)
                        (setf (aref temp-cv-matrix unknown-voltages-count node-number) 1d0
                              (aref temp-ctheta-matrix unknown-thetas-count node-number) 1d0)
                        (incf unknown-voltages-count)
                        (incf unknown-thetas-count)
                        (incf load-nodes-count))))
                   (return-from nodes-loop)))))
      (setq cv-matrix (grid:copy-to temp-cv-matrix 'grid:foreign-array)
            ctheta-matrix (grid:copy-to temp-ctheta-matrix 'grid:foreign-array))
      (when (and verbose-p
                 (> verbose 10))
        (printout :message "Cv = ~s, dim(Cv) = ~s.~&" cv-matrix (grid:dimensions cv-matrix))
        (printout :message "Ctheta = ~s, dim(Cv) = ~s.~&" ctheta-matrix (grid:dimensions ctheta-matrix)))
      (when (and verbose-p
                 (> verbose 5))
        (printout :message "exiting create-connection-matrices().~%~%"))
      (values cv-matrix
              ctheta-matrix
              ok?))))

(defun create-vectors (&rest parameters &key
                                          (problem nil problem-p)
                                          (nodes-table nil nodes-table-p)
                                          (verbose nil verbose-p))
  "Create new vectors from nodes count."
  (declare (ignorable parameters
                      problem
                      nodes-table
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when nodes-table-p
    (check-type nodes-table hash-table))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (integerp verbose)
    (when (> verbose 5)
      (printout :message "entering create-vectors().~&")))
  (let ((order (1- (hash-table-count nodes-table)))
        (voltages-vector nil)
        (thetas-vector nil)
        (p-vector nil)
        (q-vector nil)
        (delta-p-vector nil)
        (delta-q-vector nil)
        (ok? nil))
    (if (> order 0)
        (progn
          (setq voltages-vector (grid:make-foreign-array 'double-float
                                                         :dimensions order
                                                         :initial-element 0d0)
                thetas-vector (grid:make-foreign-array 'double-float
                                                       :dimensions order
                                                       :initial-element 0d0)
                p-vector (grid:make-foreign-array 'double-float
                                                  :dimensions order
                                                  :initial-element 0d0)
                q-vector (grid:make-foreign-array 'double-float
                                                  :dimensions order
                                                  :initial-element 0d0)
                delta-p-vector (grid:make-foreign-array 'double-float
                                                        :dimensions order
                                                        :initial-element 0d0)
                delta-q-vector (grid:make-foreign-array 'double-float
                                                        :dimensions order
                                                        :initial-element 0d0)
                ok? t)
          (when (integerp verbose)
            (when (> verbose 10)
              (printout :message "voltages vector = ~s, dim(V) = ~a.~&" voltages-vector (grid:dimensions voltages-vector))
              (printout :message "thetas vector = ~s, dim(theta) = ~a.~&" thetas-vector (grid:dimensions thetas-vector))
              (printout :message "delta-p vector = ~s, dim(delta-p) = ~a.~&" delta-p-vector (grid:dimensions delta-p-vector))
              (printout :message "delta-q vector = ~s, dim(delta-q) = ~a.~&" delta-q-vector (grid:dimensions delta-q-vector)))))
        (when (integerp verbose)
          (when (> verbose 10)
            (printout :error "vectors order <= 0.~&"))))
    (when (integerp verbose)
      (when (> verbose 5)
        (printout :message "exiting create-vectors().~%~%")))
    (values voltages-vector
            thetas-vector
            p-vector
            q-vector
            delta-p-vector
            delta-q-vector
            ok?)))

(defun create-admittances-matrix (&rest parameters &key
                                                     (problem nil problem-p)
                                                     (nodes-table nil nodes-table-p)
                                                     (elements-table nil elements-table-p)
                                                     (verbose nil verbose-p))
  "Create the admittance matrix from nodes and elements of the network."
  (declare (ignorable parameters
                      problem
                      nodes-table
                      elements-table
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when nodes-table-p
    (check-type nodes-table hash-table))
  (when elements-table-p
    (check-type elements-table hash-table))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (and verbose-p
             (> verbose 5))
    (printout :message "entering create admittances matrix().~&"))
  (let* ((order (1- (hash-table-count nodes-table)))
         (admittances-matrix (grid:make-foreign-array '(complex double-float)
                                                      :dimensions (list order order)
                                                      :initial-element #c(0d0 0d0)))
         (ok? nil))
    (with-hash-table-iterator (iterator elements-table)
      (loop
         initially (setq ok? t)
         with element = nil
         with value = nil
         named main-loop
         do
           (multiple-value-bind (more? element-name nodes-numbers)
               (iterator)
             (if more?
                 (progn
                   (setq element (first (select-element :predicate (where :name element-name)
                                                        :network (problem-struct-network problem))))
                   (typecase element
                     (bipole-struct
                      (when (and verbose-p
                                 (> verbose 10))
                        (printout :message "bipole ~a ~s.~&" element-name nodes-numbers))
                      ;;(setq value (->phasor (getf (bipole-struct-model-parameters element) :value)))
                      (setq value (getf (bipole-struct-model-parameters element) :value))
                      (if value
                          (progn
                            (case (bipole-struct-kind element)
                              (:resistance
                               (if (realp value)
                                   (setq value (/ 1d0 (complex value 0d0)))
                                   (progn
                                     (when (and verbose-p
                                                (> verbose 10))
                                       (printout :error
                                                 "no suitable resistance value ~a: allowed values are real.~&"
                                                 element-name))
                                     (setq ok? nil)
                                     (return-from main-loop))))
                              (:inductance
                               (if (realp value)
                                   (setq value (/ 1d0
                                                  (complex 0d0
                                                           (* 2d0 pi (problem-struct-frequency problem) value))))
                                   (progn
                                     (when (and verbose-p
                                                (> verbose 10))
                                       (printout :error "no suitable inductance value ~a: allowed values are real.~&" element-name))
                                     (setq ok? nil)
                                     (return-from main-loop))))
                              (:capacitance
                               (if (realp value)
                                   (setq value (complex 0d0 (* 2d0 pi (problem-struct-frequency problem) value)))
                                   (progn
                                     (when (and verbose-p
                                                (> verbose 10))
                                       (printout :error "no suitable capacitance value ~a: allowed values are real.~&" element-name))
                                     (setq ok? nil)
                                     (return-from main-loop))))
                              (:reactance
                               (if (realp value)
                                   (setq value (/ 1d0 (complex 0d0 value)))
                                   (progn
                                     (when (and verbose-p
                                                (> verbose 10))
                                       (printout :error "no suitable reactance value ~a: allowed values are real.~&" element-name))
                                     (setq ok? nil)
                                     (return-from main-loop))))
                              (:impedance
                               (if (complexp value)
                                   (setq value (/ 1d0 value))
                                   (progn
                                     (when (and verbose-p
                                                (> verbose 10))
                                       (printout :error "no suitable impedance value ~a: allowed values are complex.~&" element-name))
                                     (setq ok? nil)
                                     (return-from main-loop))))
                              (:conductance
                               (if (realp value)
                                   (setq value (complex value 0d0))
                                   (progn
                                     (when (and verbose-p
                                                (> verbose 10))
                                       (printout :error "no suitable conductance value ~a: allowed values are real.~&" element-name))
                                     (setq ok? nil)
                                     (return-from main-loop))))
                              (:susceptance
                               (if (realp value)
                                   (setq value (complex 0d0 value))
                                   (progn
                                     (when (and verbose-p
                                                (> verbose 10))
                                       (printout :error "no suitable susceptance value ~a: allowed values are real.~&" element-name))
                                     (setq ok? nil)
                                     (return-from main-loop))))
                              (:admittance
                               (if (complexp value)
                                   ()
                                   (progn
                                     (when (and verbose-p
                                                (> verbose 10))
                                       (printout :error "no suitable admittance value for ~a: allowed values are complex.~&" element-name))
                                     (setq ok? nil)
                                     (return-from main-loop)))))
                            (case (length nodes-numbers)
                              (1
                               (incf (grid:gref admittances-matrix (first nodes-numbers) (first nodes-numbers)) value))
                              (2
                               (if (check-connection-nodes element)
                                   (progn
                                     (decf (grid:gref admittances-matrix (first nodes-numbers) (second nodes-numbers)) value)
                                     (decf (grid:gref admittances-matrix (second nodes-numbers) (first nodes-numbers)) value)
                                     (incf (grid:gref admittances-matrix (first nodes-numbers) (first nodes-numbers)) value)
                                     (incf (grid:gref admittances-matrix (second nodes-numbers) (second nodes-numbers)) value))
                                   (progn
                                     (setq ok? nil)
                                     (when (and verbose-p
                                                (> verbose 10))
                                       (printout :error "bipole ~a has got wrong connection nodes ~s.~&" (bipole-struct-name element) (bipole-struct-nodes element)))
                                     (return-from main-loop))))))
                          (progn
                            (setq ok? nil)
                            (when (and verbose-p
                                       (> verbose 10))
                              (printout :error "wrong value ~s for bipole ~a.~&" value element-name))
                            (return-from main-loop))))
                     (multipole-struct
                      (setq ok? nil)
                      (when (and verbose-p
                                 (> verbose 10))
                        (printout :error "multipole elements are not implemented yet.~&"))
                      (return-from main-loop))))
                 (return-from main-loop)))))
    (when (and verbose-p
               (> verbose 10))
      (printout :message "Y = ~s.~&" admittances-matrix)
      (when (> verbose 5)
        (printout :message "exiting create-admittances-matrix().~%~%")))
    (values admittances-matrix
            ok?)))

(defun create-jacobian-matrix (&rest parameters &key
                                                  (problem nil problem-p)
                                                  (verbose nil verbose-p))
  (declare (ignorable parameters
                      problem
                      verbose))
  (when problem-p
    (check-type problem problem-struct))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (integerp verbose)
    (when (> verbose 5)
      (printout :message "entering create-jacobian-matrix().~&")))
  (let ((nodes (remove-if-not #'(lambda (x)
                                  (typep x 'node-struct))
                              (problem-struct-network problem)))
        (dp/dtheta-matrix nil)
        (dq/dtheta-matrix nil)
        (dp/dv-matrix nil)
        (dq/dv-matrix nil)
        (ok? nil))
    (if nodes
        (multiple-value-bind (p-nodes-count q-nodes-count)
            (count-power-nodes :nodes nodes
                               :verbose verbose)
          (if (and (> p-nodes-count 0)
                   (> q-nodes-count 0))
              (setq ok? t
                    dp/dtheta-matrix (grid:make-foreign-array 'double-float
                                                              :dimensions (list p-nodes-count p-nodes-count)
                                                              :initial-element 0d0)
                    dq/dtheta-matrix (grid:make-foreign-array 'double-float
                                                              :dimensions (list q-nodes-count p-nodes-count)
                                                              :initial-element 0d0)
                    dp/dv-matrix (grid:make-foreign-array 'double-float
                                                          :dimensions (list p-nodes-count q-nodes-count)
                                                          :initial-element 0d0)
                    dq/dv-matrix (grid:make-foreign-array 'double-float
                                                          :dimensions (list q-nodes-count q-nodes-count)
                                                          :initial-element 0d0))
              (when (integerp verbose)
                (when (> verbose 10)
                  (printout :message "could not create the Jacobian matrix.~&")))))
        (when (integerp verbose)
          (when (> verbose 10)
            (printout :message "no nodes in the network.~&"))))
    (when (integerp verbose)
      (when (> verbose 5)
        (printout :message "exiting calculate-jacobian().~%~%")))
    (values dp/dtheta-matrix
            dq/dtheta-matrix
            dp/dv-matrix
            dq/dv-matrix
            ok?)))

(defun count-power-nodes (&rest parameters &key
                                             (nodes nil nodes-p)
                                             (verbose nil verbose-p))
  (declare (ignorable parameters
                      nodes))
  (when nodes-p
    (check-type nodes list)
    (loop
       for x in nodes
       do
         (check-type x node-struct)))
  (when verbose-p
    (check-type verbose (or integer null)))
  (when (integerp verbose)
    (when (> verbose 5)
      (printout :message "entering count-power-nodes().~&")))
  (let ((p-nodes-count 0)
        (q-nodes-count 0))
    (loop
       for node in nodes
       do
         (case (node-struct-kind node)
           (:generation
            (when (integerp verbose)
              (when (> verbose 10)
                (printout :message "generation node ~a bond type ~s.~&" (node-struct-name node) (node-struct-bond node))))
            (case (bond-struct-kind (node-struct-bond node))
              (:p-v
               (incf p-nodes-count))
              (:q-v
               (incf q-nodes-count))
              (:p-q
               (incf p-nodes-count)
               (incf q-nodes-count))
              (:v-theta
               ())))
           (:load
            (when (integerp verbose)
              (when (> verbose 10)
                (printout :message "load node ~a bond type ~s.~&" (node-struct-name node) (node-struct-bond node))))
            (case (bond-struct-kind (node-struct-bond node))
              (:p-q
               (incf p-nodes-count)
               (incf q-nodes-count))))
           (:interconnection
            (when (integerp verbose)
              (when (> verbose 10)
                (printout :message "interconnection node ~a.~&" (node-struct-name node))))
            (incf p-nodes-count)
            (incf q-nodes-count))))
    (when (integerp verbose)
      (when (> verbose 10)
        (printout :message "P nodes count = ~a, Q nodes count = ~a.~&" p-nodes-count q-nodes-count)))
    (when (integerp verbose)
      (when (> verbose 5)
        (printout :message "exiting count-power-nodes().~%~%")))
    (values p-nodes-count q-nodes-count)))
