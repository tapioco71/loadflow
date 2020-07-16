;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; model.lisp
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

(defstruct (model-struct (:constructor make-model))
  (name nil :type (or symbol string (unsigned-byte 64) null))
  (body-file-pathname nil :type (or pathname null))
  (function-name nil :type (or symbol null))
  (function-parameters nil :type list))

(defun make-model (&rest parameters &key
				      (name nil name-p)
				      (body-file-pathname nil body-file-pathname-p)
				      (function-name nil function-name-p)
				      (function-parameters nil function-parameters-p))
  (declare (ignorable parameters
		      name
		      body-file-pathname
		      function-name
		      function-parameters))
  (when name-p
    (check-type name (or symbol string (unsigned-byte 64) null)))
  (when body-file-pathname-p
    (check-type body-file-pathname (or pathname null)))
  (when function-name-p
    (check-type function-name (or symbol null)))
  (when function-parameters-p
    (check-type function-parameters list))
  (let ((object (allocate-instance (find-class 'model-struct))))
    (setf (model-struct-name object) name
	  (model-struct-body-file-pathname object) body-file-pathname
	  (model-struct-function-name object) function-name
	  (model-struct-function-parameters object) function-parameters)
    object))
