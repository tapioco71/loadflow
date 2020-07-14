;;;; -*- mode: Lisp; syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; loadflow.asd
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

(asdf:defsystem #:loadflow
  :description "Loadflow software"
  :author "Angelo Rossi <angelo.rossi.homelab@gmail.com>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:gsll #:bordeaux-threads)
  :components ((:file "sources/package")
               (:file "sources/message")
               (:file "sources/electrical")
               (:file "sources/network-element")
               (:file "sources/bond")
               (:file "sources/node")
               (:file "sources/multipole")
               (:file "sources/problem")
               (:file "sources/loadflow")))
