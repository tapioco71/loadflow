;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; message.lisp
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
