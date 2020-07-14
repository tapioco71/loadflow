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

;; Classes.

;; Structures.

(defstruct (phasor-struct (:constructor make-phasor))
  (magnitude 0d0 :type (or real null))
  (argument 0d0 :type (or real null)))

;; Functions.

(defun make-phasor (&rest parameters &key
                                       (magnitude 0d0 magnitude-p)
                                       (argument 0d0 argument-p))
  (declare (ignorable parameters
                      magnitude
                      argument))
  (when magnitude-p
    (check-type magnitude (or real null)))
  (when argument-p
    (check-type argument (or real null)))
  (let ((object (allocate-instance (find-class 'phasor-struct))))
    (setf (phasor-struct-magnitude object) magnitude
          (phasor-struct-argument object) argument)
    object))

(defun phasor-p (p)
  (typep p 'phasor-struct))

(defun real->phasor (r)
  (check-type r real)
  (make-phasor :magnitude r))

(defun complex->phasor (c)
  (check-type c complex)
  (make-phasor :magnitude (abs c)
               :argument (phase c)))

(defun ->phasor (v)
  (typecase v
    (real (make-phasor :magnitude v))
    (complex (complex->phasor v))
    (phasor-struct v)
    (t nil)))

(defun phasor->complex (p)
  (check-type p phasor-struct)
  (* (phasor-struct-magnitude p)
     (exp (* #c(0d0 1d0) (phasor-struct-argument p)))))

;; Methods.

(defmethod (setf magnitude) (new-value (p phasor-struct))
  (check-type p phasor-struct)
  (check-type new-value real)
  (setf (phasor-struct-magnitude p) new-value))

(defmethod (setf argument) (new-value (p phasor-struct))
  (check-type p phasor-struct)
  (check-type new-value real)
  (setf (phasor-struct-argument p) new-value))

(defmethod magnitude ((object phasor-struct))
  (phasor-struct-magnitude object))

(defmethod argument ((object phasor-struct))
  (phasor-struct-argument object))

(defmethod real-part ((object phasor-struct))
  (* (magnitude object) (cos (argument object))))

(defmethod imaginary-part ((object phasor-struct))
  (* (magnitude object) (sin (argument object))))

(defmethod rotate ((object phasor-struct) angle)
  (check-type angle real)
  (make-phasor :magnitude (magnitude object)
               :argument (+ (argument object)
                            angle)))

(defmethod conj ((object phasor-struct))
  (make-phasor :magnitude (magnitude object)
               :argument (- (argument object))))

(defmethod add ((object phasor-struct) p)
  (check-type p phasor-struct)
  (complex->phasor (+ (phasor->complex object)
                      (phasor->complex p))))

(defmethod subtract ((object phasor-struct) p)
  (check-type p phasor-struct)
  (complex->phasor (- (phasor->complex object)
                      (phasor->complex p))))

(defmethod product ((object phasor-struct) p)
  (check-type p (or real phasor-struct))
  (typecase p
    (real
     (product object (make-phasor :magnitude p)))
    (phasor-struct
     (make-phasor :magnitude (* (magnitude object)
                                (magnitude p))
                  :argument (+ (argument object)
                               (argument p))))))

(defmethod divide ((object phasor-struct) p)
  (check-type p (or real phasor-struct))
  (typecase p
    (real
     (divide object (make-phasor :magnitude p)))
    (phasor-struct
     (make-phasor :magnitude (/ (magnitude object)
                                (magnitude p))
                  :argument (- (argument object)
                               (argument p))))))
