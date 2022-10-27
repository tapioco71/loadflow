;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; bond.lisp
;;;
;;; Bond structure definition.

(in-package #:loadflow)

;; Parameters.

(defparameter *bond-kinds* '(:p-v
                             :q-v
                             :p-q
                             :v-theta
                             :v=f[Q]))

;; Structures.

(defstruct (bond-struct (:include network-element-struct)
                        (:constructor make-bond))
  (active-power nil :type (or real null))
  (reactive-power nil :type (or real null))
  (voltage-magnitude nil :type (or real null))
  (voltage-phase nil :type (or real null))
  (load-factor nil :type (or real null))
  (model nil :type (or model-struct null)))

;; Functions.

(defun make-bond (&rest parameters &key
                                     (name (symbol-name (gensym "bond-")) name-p)
                                     (kind nil kind-p)
                                     (data nil data-p)
                                     (active-power nil active-power-p)
                                     (reactive-power nil reactive-power-p)
                                     (voltage-magnitude nil voltage-magnitude-p)
                                     (voltage-phase nil voltage-phase-p))

  "Create a bond object from bond structure."
  (declare (ignorable parameters
                      name
                      kind
                      active-power
                      reactive-power
                      voltage-magnitude
                      voltage-phase))
  (when name-p
    (check-type name (or symbol string (unsigned-byte 64) null)))
  (when kind-p
    (check-type kind (or keyword null))
    (assert (member kind *bond-kinds* :test #'equal)))
  (case kind
    (:p-v
     (assert (and active-power-p
                  voltage-magnitude-p
                  (not (or reactive-power-p
                           voltage-phase-p)))))
    (:q-v
     (assert (and reactive-power-p
                  voltage-magnitude-p
                  (not (or active-power-p
                           voltage-phase-p)))))
    (:p-q
     (assert (and active-power-p
                  reactive-power-p
                  (not (or voltage-magnitude-p
                           voltage-phase)))))
    (:v-theta
     (assert (and voltage-magnitude-p
                  voltage-phase-p
                  (not (or active-power
                           reactive-power))))))
  (when active-power-p
    (check-type active-power (or real null)))
  (when reactive-power-p
    (check-type reactive-power (or real null)))
  (when voltage-magnitude-p
    (check-type voltage-magnitude (or real null)))
  (when voltage-phase-p
    (check-type voltage-phase (or real null)))
  (let ((object (allocate-instance (find-class 'bond-struct))))
    (setf (bond-struct-name object) name
          (bond-struct-kind object) kind)
    (when active-power-p
      (setf (bond-struct-active-power object) active-power))
    (when reactive-power-p
      (setf (bond-struct-reactive-power object) reactive-power))
    (when voltage-magnitude-p
      (setf (bond-struct-voltage-magnitude object) voltage-magnitude))
    (when voltage-phase-p
      (setf (bond-struct-voltage-phase object) voltage-phase))
    object))


(defmethod p-q-p ((object bond-struct))
  (equalp (bond-struct-kind object) :p-q))

(defmethod p-v-p ((object bond-struct))
  (equalp (bond-struct-kind object) :p-v))

(defmethod q-v-p ((object bond-struct))
  (equalp (bond-struct-kind object) :q-v))

(defmethod v-theta-p ((object bond-struct))
  (equalp (bond-struct-kind object) :v-theta))

(defmethod v=f[Q]-p ((object bond-struct))
  (equalp (bond-struct-kind object) :v=f[Q]))

(defmethod kind-p ((object bond-struct) bond-kind)
  (assert (member bond-kind *bond-kind* :test #'equalp))
  (equalp (bond-struct-kind object) bond-kind))
