;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; multipole.lisp
;;;
;;; Multipole network model.

(in-package #:loadflow)

;; Parameters.

(defparameter *multipole-types* '())
(defparameter *bipole-types* '(:admittance
                               :impedance
                               :resistance
                               :reactance
                               :inductance
                               :capacitance
                               :susceptance
                               :elastance
                               :i-v-model
                               :v-i-model
                               :p-q-model))

;; Structures.

(defstruct (multipole-struct (:include network-element-struct)
                             (:constructor make-multipole))
  (nodes nil :type list)
  (voltages nil :type list)
  (currents nil :type list)
  (model-function nil :type (or function null))
  (model-parameters nil :type list))

(defstruct (bipole-struct (:include multipole-struct)
                          (:constructor make-bipole)))

;; Functions.

(defun make-multipole (&rest parameters &key
                                          (name (symbol-name (gensym "multipole-")) name-p)
                                          (kind nil kind-p)
                                          (nodes nil nodes-p)
                                          (voltages '(0d0 0d0) voltages-p)
                                          (currents '(0d0) currents-p)
                                          (model-function #'(lambda (v i)
                                                              ())
                                                          model-function-p)
                                          (model-parameters nil model-parameters-p))
  (declare (ignorable parameters
                      name
                      kind
                      nodes
                      voltages
                      currents
                      model-function
                      model-parameters))
  (when name-p
    (check-type name (or symbol string (unsigned-byte 64) null)))
  (when kind-p
    (check-type kind (or keyword null)))
  (when nodes-p
    (check-type nodes list)
    (assert (>= (length nodes) 2))
    (loop
       for x in nodes
       do
         (check-type x (or symbol string (unsigned-byte 64) null))))
  (when voltages-p
    (check-type voltages list)
    (assert (= (length nodes)
               (length voltages)))
    (loop
       for x in voltages
       do
         (check-type x (or real complex null))))
  (when currents-p
    (check-type currents list)
    (cond
      ((<= (length nodes) 2)
       (assert (= (length currents) (1- (length nodes)))))
      (t
       (assert (= (length currents) (length nodes)))))
    (loop
       for x in currents
       do
         (check-type x (or real complex null))))
  (when model-function-p
    (check-type model-function (or function null)))
  (when model-parameters-p
    (check-type model-parameters list))
  (assert (eql (type-of voltages)
               (type-of currents)))
  (when (typep voltages 'grid:foreign-array)
    (assert (and
             (= (length (grid:dimensions voltages)) 1)
             (equalp (grid:dimensions voltages)
                     (grid:dimensions currents))
             (= (length nodes) (first (grid:dimensions voltages))))))
  (let ((object (allocate-instance (find-class 'multipole-struct))))
    (setf (multipole-struct-name object) name
          (multipole-struct-kind object) kind
          (multipole-struct-nodes object) nodes
          (multipole-struct-voltages object) voltages
          (multipole-struct-currents object) currents
          (multipole-struct-model-function object) model-function
          (multipole-struct-model-parameters object) model-parameters)
    object))

(defun make-bipole (&rest parameters &key
                                       (name (symbol-name (gensym "bipole-")) name-p)
                                       (kind :admittance kind-p)
                                       (nodes '(node-1 node-2) nodes-p)
                                       (voltages '(0d0 0d0) voltages-p)
                                       (current 0d0 current-p)
                                       (model-function #'(lambda (v i)
                                                           ())
                                                       model-function-p)
                                       (model-parameters nil model-parameters-p))
  (declare (ignorable parameters
                      name
                      kind
                      nodes
                      voltages
                      current
                      model-function
                      model-parameters))
  (when name-p
    (check-type name (or symbol string (unsigned-byte 64) null)))
  (when kind-p
    (check-type kind (or keyword null))
    (assert (member kind *bipole-types* :test #'equalp)))
  (when nodes-p
    (check-type nodes list)
    (assert (= (length nodes) 2))
    (loop
       for x in nodes
       do
         (check-type x (or symbol string (unsigned-byte 64) null))))
  (when voltages-p
    (check-type voltages list)
    (assert (= (length voltages) 2)))
  (when current-p
    (check-type current (or real complex null)))
  (when model-function-p
    (check-type model-function function))
  (when model-parameters-p
    (check-type model-parameters list))
  (let ((object (allocate-instance (find-class 'bipole-struct))))
    (setf (bipole-struct-name object) name
          (bipole-struct-kind object) kind
          (bipole-struct-nodes object) nodes
          (bipole-struct-voltages object) voltages
          (bipole-struct-currents object) (list current)
          (bipole-struct-model-function object) model-function
          (bipole-struct-model-parameters object) model-parameters)
    object))

;; Other functions.

(defun check-connection-nodes (m)
  (check-type m multipole-struct)
  (< (loop
        with nodes-list = (multipole-struct-nodes m)
        while nodes-list
        maximize (count (first nodes-list) nodes-list :test #'equalp) into maximum-count
        do
          (setq nodes-list (remove (first nodes-list) nodes-list))
        finally (return maximum-count))
     (length (multipole-struct-nodes m))))

(defun ->admittance (b)
  (check-type b bipole-struct)
  (case (bipole-struct-kind b)
    (:resistance
     (when (realp (getf (bipole-struct-model-parameters b) :value))
       (->phasor (/ 1d0 (getf (bipole-struct-model-parameters b) :value)))))
    (:reactance
     (when (realp (getf (bipole-struct-model-parameters b) :value))
       (->phasor (/ 1d0 #(complex 0d0 (getf (bipole-struct-model-parameters b) :value))))))
    (:impedance
     (when (numberp (getf (bipole-struct-model-parameters b) :value))
       (->phasor (/ 1d0 (getf (bipole-struct-model-parameters b) :value)))))
    (:conductance
     (when (realp (getf (bipole-struct-model-parameters b) :value))
       (->phasor (getf (bipole-struct-model-parameters b) :value))))
    (:admittance
     (when (numberp (getf (bipole-struct-model-parameters b) :value))
         (->phasor (getf (bipole-struct-model-parameters b) :value))))))
