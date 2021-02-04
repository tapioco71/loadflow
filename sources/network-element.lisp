;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; network-element.lisp
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

;; Structures.

(defstruct (network-element-struct (:constructor make-network-element))
  (name "" :type (or symbol string (unsigned-byte 64) null))
  (kind nil :type (or keyword null))
  (data nil :type list))

;; Functions.

(defun make-network-element (&rest parameters &key
                                                (name nil name-p)
                                                (kind nil kind-p)
                                                (data nil data-p))
  (declare (ignorable parameters
                      name
                      kind
                      data))
  (when name-p
    (check-type name (or symbol string (unsigned-byte 64) null)))
  (when kind-p
    (check-type kind (or keyword null)))
  (when data-p
    (check-type data list))
  (let ((object (allocate-instance (find-class 'network-element-struct))))
    (setf (network-element-struct-name object) name
          (network-element-struct-kind object) kind
          (network-element-struct-data object) data)
    object))

;; Functions.

(defun select-element (&rest parameters &key
                                          (predicate nil predicate-p)
                                          (elements nil elements-p))
  "Apply the predicate function to each of the network elements and return a
   list of them to whom predicate it is true."
  (declare (ignorable parameters
                      predicate
                      elements))
  (when predicate-p
    (check-type predicate function))
  (when elements-p
    (check-type elements list))
  (remove-if-not predicate
                 elements))

(defun where (&rest parameters &key
                                 (name nil name-p)
                                 (kind nil kind-p)
                                 (tag nil tag-p))
  "Return a closure which test for different combinations of
   keys the argument."
  (declare (ignorable parameters
                      name
                      kind
                      tag))
  (when name-p
    (check-type name (or symbol string (unsigned-byte 64) null)))
  (when kind-p
    (check-type kind (or keyword null)))
  #'(lambda (x)
      (and (if name-p
               (equalp (network-element-struct-name x) name)
               t)
           (if kind-p
               (equalp (network-element-struct-kind x) kind)
               t)
           (if tag-p
               (= (node-struct-tag x) tag)
               t))))
