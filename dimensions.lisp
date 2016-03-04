;; -*- mode: lisp; coding: utf-8 -*-

;; 2016 - Eduardo Acuña Yeomans

;; dimensions.lisp

(in-package #:cl-physlisp)

(defclass dimension ()
  ((code :reader code)
   (form :reader form)
   (name :reader name)))

(defmethod print-object ((dim dimension) stream)
  (cond ((slot-boundp dim 'name)
         (format stream "#D~a" (list (name dim))))
        ((slot-boundp dim 'form)
         (format stream "#D~a" (form dim)))
        ((slot-boundp dim 'code)
         (format stream "#D~a" (code dim)))
        (t
         (error "can't print a dimension without name, form or code."))))

(defun make-dimension (sexp)
  (typecase sexp
    (symbol
     (let ((dimension (gethash sexp *name->dimension*)))
       (if dimension
	   dimension
	   (error "unrecognized dimension ~a." sexp))))
    (vector
     (let ((dimension (gethash sexp *code->dimension*)))
       (if dimension
	   dimension
	   (code->dimension sexp))))
    (cons
     (if (= (length sexp) 1)
	 (let ((dimension (gethash (car sexp) *name->dimension*)))
	   (if dimension
	       dimension
	       (error "unrecognized dimension ~a." (car sexp))))
	 (let ((dimension (gethash sexp *form->dimension*)))
	   (if dimension
	       dimension
	       (form->dimension sexp)))))
    (otherwise
     (error "the form ~a can't be interpreted as a dimension." sexp))))


(defun code->dimension (code)
  (if (/= (length code) *dimension-rank*)
      (error "can't create a dimension based on a code with rank different than ~a."
	     *dimension-rank*)
      (let ((basecode    (make-dimension-code))
	    (numerator   nil)
	    (denominator nil))
	(dotimes (i *dimension-rank*)
	  (setf (elt basecode i) 1)
	  (let ((dimension (gethash basecode *code->dimension*))
		(exponent  (elt code i)))
	    (unless (zerop exponent)
	      (if (plusp exponent)
		  (if (= exponent 1)
		      (push (name dimension) numerator)
		      (push (list '^ (name dimension) exponent) numerator))
		  (if (= exponent -1)
		      (push (name dimension) denominator)
		      (push (list '^ (name dimension) (abs exponent)) denominator)))))
	  (setf (elt basecode i) 0))
	(cond ((not numerator)
	       (setf numerator 'dimensionless))
	      ((= 1 (length numerator))
	       (setf numerator (car numerator)))
	      (t
	       (push '* numerator)))
	(cond ((not denominator)
	       (setf denominator 'dimensionless))
	      ((= 1 (length denominator))
	       (setf denominator (car denominator)))
	      (t
	       (push '* denominator)))
	(let* ((form (if (eq denominator 'dimensionless) numerator (list '/ numerator denominator)))
	       (dimension (make-instance 'dimension)))
	  (setf (slot-value dimension 'code) code)
	  (setf (slot-value dimension 'form) form)
	  (setf (gethash code *code->dimension*) dimension)
	  (setf (gethash form *form->dimension*) dimension)
	  dimension))))


(defun form->dimension (form)
  (cond ((symbolp form)
	 (let ((dimension (gethash form *name->dimension*)))
	   (if dimension
	       dimension
	       (error "can't create a dimension based on the undefined dimension ~a." form))))
	((numberp form)
	 (let ((dimension (gethash 'dimensionless *name->dimension*)))
	   (if dimension
	       dimension
	       (error "dimensionless not found! eay fucked up!"))))
	((consp form)
	 (let ((dimension (gethash form *form->dimension*)))
	   (if dimension
	       dimension
	       (let ((op   (car form))
		     (args (cdr form)))
		 (case op
		   ((*)
		    (let ((code (reduce #'(lambda (a b) (map 'vector #'+ a b))
					(mapcar #'code (mapcar #'form->dimension args)))))
		      (let ((dimension (gethash code *code->dimension*)))
			(if dimension
			    dimension
			    (code->dimension code)))))
		   ((^)
		    (let ((dimension   (form->dimension (first args)))
			  (repetitions (second args)))
		      (let ((code (reduce #'(lambda (a b) (map 'vector #'+ a b))
					  (make-list repetitions :initial-element (code dimension)))))
			(let ((dimension (gethash code *code->dimension*)))
			  (if dimension
			      dimension
			      (code->dimension code))))))
		   ((/)
		    (let ((code (reduce #'(lambda (a b) (map 'vector #'- a b))
					(mapcar #'code (mapcar #'form->dimension args)))))
		      (let ((dimension (gethash code *code->dimension*)))
			(if dimension
			    dimension
			    (code->dimension code)))))
		   ((+ -)
		    (let* ((dimensions (mapcar #'form->dimension args))
			   (codes      (mapcar #'code dimensions)))
		      (if (every #'(lambda (x) (equalp x (car codes))) (cdr codes))
			  (first dimensions)
			  (error "attempting to sum or subtract distinct dimensions.")))))))))
	((vectorp form)
	 (let ((dimension (gethash form *code->dimension*)))
	   (if dimension
	       dimension
	       (code->dimension form))))))

(set-dispatch-macro-character
 #\# #\d
 #'(lambda (stream subchar arg)
     (declare (ignore subchar arg))
     (let ((sexp (read stream t)))
       (make-dimension sexp))))


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
