;; -*- mode: lisp; coding: utf-8 -*-

;; 2016 - Eduardo Acuña Yeomans

;; macros.lisp

(in-package #:cl-physlisp)

(defmacro define-basic-dimensions (&rest dimensions)
  (cond ((member 'dimensionless dimensions)
	 (error "defining the primitive dimension DIMENSIONLESS isn't permitted."))
	((/= (length dimensions) (length (remove-duplicates dimensions)))
	 (error "defining duplicate basic dimensions."))
	(t
	 (when *basic-dimensions*
	   (format t "~&WARNING: redefining primitive dimensions."))
	 (setf *basic-dimensions* dimensions)
	 (setf *dimension-rank* (length dimensions))
	 (setf *code->dimension* (make-hash-table :test #'equalp))
	 (setf *form->dimension* (make-hash-table :test #'equal))
	 (setf *name->dimension* (make-hash-table :test #'eq))
	 (let ((code (make-dimension-code))
	       (name 'dimensionless)
	       (form 'dimensionless)
	       (dim  (make-instance 'dimension)))
	   (setf (slot-value dim 'code) code)
	   (setf (slot-value dim 'name) name)
	   (setf (slot-value dim 'form) form)
	   (setf (gethash code *code->dimension*) dim)
	   (setf (gethash name *name->dimension*) dim)
	   (setf (gethash form *form->dimension*) dim))
	 (loop for name in dimensions
	       for i upto (1- *dimension-rank*)
	       do
		  (let ((code (make-dimension-code))
			(form name)
			(dim (make-instance 'dimension)))
		    (setf (elt code i) 1)
		    (setf (slot-value dim 'code) code)
		    (setf (slot-value dim 'name) name)
		    (setf (slot-value dim 'form) form)
		    (setf (gethash code *code->dimension*) dim)
		    (setf (gethash name *name->dimension*) dim)
		    (setf (gethash form *form->dimension*) dim))))))

(defmacro define-dimension (name dimension)
  (cond ((not *basic-dimensions*)
	 (error "defining compound dimension without defining basic dimensions."))
	((member name *basic-dimensions*)
	 (error "dimension ~a is defined as a basic dimension." name))
	((gethash name *name->dimension*)
	 (error "dimension ~a is defined as a compound dimension." name))
	(t
	 (setf (slot-value dimension 'name) name)
	 (setf (gethash name *name->dimension*) dimension))))


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
