;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; message.lisp
;;;
;;; Functions related to program output.

(in-package #:loadflow)

;; Messages types.

(defparameter *message-types* '(:message :warning :error :results))

;; Functions.

(defun printout (s mode message &rest parameters)
  "Print into the right output stream message for the three categories :message
   :error and :results."
  (check-type s stream)
  (assert (member mode *message-types* :test #'equalp))
  (check-type message string)
  (let ((format-parameters (case mode
                             (:message
                              (concatenate 'list
                                           (list s
                                                 (concatenate 'string
                                                              "--> "
                                                              message))
                                           parameters))
                             (:warning
                              (concatenate 'list
                                           (list s
                                                 (concatenate 'string
                                                              "WARNING: "
                                                              message))
                                           parameters))
                             (:error
                              (concatenate 'list
                                           (list s
                                                 (concatenate 'string
                                                              "ERROR: "
                                                              message))
                                           parameters))
                             (:results
                              (concatenate 'list
                                           (list s
                                                 "RESULTS: ~s~&")
                                           parameters))
                             (t
                              ()))))
    (when format-parameters
      (apply #'format
             format-parameters)
      (finish-output s))))
