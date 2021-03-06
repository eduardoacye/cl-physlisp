;; -*- mode: lisp; coding: utf-8 -*-

;; 2016 - Eduardo Acuña Yeomans

;; cl-physlisp.asd

(asdf:defsystem #:cl-physlisp
  :description "Física en lisp"
  :author "Eduardo Acuña Yeomans <eduardo.acye@gmail.com>"
  :license "GPLv3"
  :components ((:file "packages")
               (:file "special"            :depends-on ("packages"))
               (:file "utils"              :depends-on ("special" "packages"))
               (:file "macros"             :depends-on ("packages"))
               (:file "dimensions"         :depends-on ("macros"))
               (:file "cl-physlisp"        :depends-on ("dimensions"))))


;; LICENSE:
;;
;; cl-physlisp: a library to bring physics mojo and common lisp magic together
;; Copyright (C) 2016 Eduardo Acuña Yeomans
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
