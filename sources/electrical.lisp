;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; node.lisp
;;;
;;; Collection of functions related to EE domain.

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
