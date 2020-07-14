;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; bond.lisp
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

(defparameter *bond-kinds* '(:p-v
                             :q-v
                             :p-q
                             :v-theta))

;; Structures.

(defstruct (bond-struct (:include network-element-struct)
                        (:constructor make-bond))
  (active-power nil :type (or real null))
  (reactive-power nil :type (or real null))
  (voltage-magnitude nil :type (or real null))
  (voltage-phase nil :type (or real null)))

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
