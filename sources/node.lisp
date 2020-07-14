;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; node.lisp
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

(defparameter *node-kinds* '(:generation
                             :load
                             :voltage
                             :interconnection
                             :reference))

;; Structures.

(defstruct (node-struct (:include network-element-struct)
                        (:constructor make-node))
  (bond nil :type (or bond-struct null)))

;; Functions.

(defun make-node (&rest parameters &key
                                     (name (symbol-name (gensym "node-")) name-p)
                                     (kind nil kind-p)
                                     (data nil data-p)
                                     (bond nil bond-p))
  "Create a bode/bar object for the node structure."
  (declare (ignorable parameters
                      name
                      kind
                      data
                      bond))
  (when name-p
    (check-type name (or symbol string (unsigned-byte 64) null)))
  (when kind-p
    (check-type kind (or keyword list)))
  (when data-p
    (check-type data list))
  (when bond-p
    (check-type bond (or bond-struct null)))
  (let ((object (allocate-instance (find-class 'node-struct))))
    (setf (node-struct-name object) name
          (node-struct-kind object) kind
          (node-struct-data object) data
          (node-struct-bond object) bond)
    object))

;; Methods.

(defmethod node-valid-kind-p ((object node-struct))
  (member (node-struct-kind object)
          *node-kinds*
          :test #'equalp))

(defmethod node-valid-bond-p ((object node-struct))
  (case (node-struct-kind object)
    (:generation
     (case (bond-struct-kind (node-struct-kind object))
       (:p-e t)
       (:q-e t)
       (:p-q t)
       (:e-theta)))
    (:load
     (case (bond-struct-kind (node-struct-kind object))
       (:p-q t)))))
