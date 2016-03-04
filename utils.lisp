;; -*- mode: lisp; coding: utf-8 -*-

;; 2016 - Eduardo Acuña Yeomans

;; utils.lisp

(in-package #:cl-physlisp)

(defun make-dimension-code ()
  (make-array *dimension-rank* :element-type 'fixnum :initial-element 0))

(defun show-registry (table)
  (loop for key being the hash-keys of table using (hash-value value)
	do (format t "~%KEY ~a~%NAME ~s~%FORM ~s~%CODE ~s~%"
		   key
		   (if (slot-boundp value 'name) (name value) "unnamed")
		   (if (slot-boundp value 'form) (form value) "unformed")
		   (if (slot-boundp value 'code) (code value) "uncoded"))))

(defun show-code->dimension ()
  (show-registry *code->dimension*))

(defun show-name->dimension ()
  (show-registry *name->dimension*))

(defun show-form->dimension ()
  (show-registry *form->dimension*))



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
