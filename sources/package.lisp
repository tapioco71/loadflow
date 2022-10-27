;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; package.lisp
;;;
;;; Copyright (c) 2020-2022 Angelo Rossi <angelo.rossi.homelab@gmail.com>
;;;
;;; This file is part of Loadflow (LF).
;;;
;;;    Loadflow (LF) is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    Loadflow (LF) is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with Loadflow (LF).  If not, see <http://www.gnu.org/licenses/>.

(defpackage #:loadflow
  (:nicknames #:lf)
  (:use #:cl)
  (:export #:network-element
           #:node-struct
           #:multipole-struct
           #:bipole-struct
           #:bond-struct
           #:problem-struct
           #:printout
           #:make-network-element
           #:make-node
           #:make-multipole
           #:make-bipole
           #:make-bond
           #:make-problem
           #:load-problem
           #:setup-problem
           #:create-connection-matrices
           #:create-vectors
           #:create-admittances-matrix
           #:create-jacobian-submatrices
           #:assign-initial-solution
           #:calculate-power-residuals
           #:calculate-jacobian
           #:solve-system
           #:update-solution
           #:check-power-tolerance
           #:output-solution
           #:loadflow
           #:parametric-loadflow
           #:main))
