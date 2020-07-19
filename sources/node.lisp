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
  (tag nil :type (or (unsigned-byte 64) null))
  (bond nil :type (or bond-struct null)))

;; Functions.

(defun make-node (&rest parameters &key
                                     (name (symbol-name (gensym "node-")) name-p)
                                     (kind nil kind-p)
                                     (data nil data-p)
                                     (tag nil tag-p)
                                     (bond nil bond-p))
  "Create a bode/bar object for the node structure."
  (declare (ignorable parameters
                      name
                      kind
                      data
                      tag
                      bond))
  (when name-p
    (check-type name (or symbol string (unsigned-byte 64) null)))
  (when kind-p
    (check-type kind (or keyword list)))
  (when data-p
    (check-type data list))
  (when tag-p
    (check-type tag (or (unsigned-byte 64) null)))
  (when bond-p
    (check-type bond (or bond-struct null)))
  (let ((object (allocate-instance (find-class 'node-struct))))
    (setf (node-struct-name object) name
          (node-struct-kind object) kind
          (node-struct-data object) data
          (node-struct-tag object) tag
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
       (:p-v t)
       (:q-v t)
       (:p-q t)
       (:v-theta)))
    (:load
     (case (bond-struct-kind (node-struct-kind object))
       (:v=f(Q) t)
       (:p-q t)))))

(defmethod reference-p ((object node-struct))
  (equalp (node-struct-kind object) :reference))

(defmethod load-p ((object node-struct))
  (equalp (node-struct-kind object) :load))

(defmethod generation-p ((object node-struct))
  (equalp (node-struct-kind object) :generation))

(defmethod interconnection-p ((object node-struct))
  (equalp (node-struct-kind object) :interconnection))

(defmethod kind-p ((object node-struct) node-kind)
  (assert (member node-kind *node-kind* :test #'equalp))
  (equalp (node-struct-kind object) node-kind))

(defun where-node (&rest parameters &key
                                      (data nil data-p)
                                      (tag nil tag-p)
                                      (bond nil bond-p))
  "Return a closure which test, for different combinations of
   keys, the node-struct object argument."
  (declare (ignorable parameters
                      name
                      kind))
  (when data-p
    (check-type data list))
  (when tag-p
    (check-type tag (or (unsigned-byte 64) null)))
  (when bond-p
    (check-type bond bond-struct))
  #'(lambda (x)
      (and (if data-p
               (equalp (node-struct-data x) data)
               t)
           (if tag-p
               (eql (node-struct-tag x) tag)
               t)
           (if bond-p
               (equalp (node-struct-bond x) bond)
               t))))
