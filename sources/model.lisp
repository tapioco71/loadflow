;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; model.lisp
;;;
;;; Network elements model.

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
