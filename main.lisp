(defpackage :dbind
  (:use :cl :papply :chiku.util))

(in-package :dbind)

(defun proc-sealed-lambda-list (sealed-var sealed-lambda-list)
  (let* (sealed-vars
         (lambda-list
           (chiku.util:mapleaf #'(if (eq a0 sealed-var)
                        (car (push (gensym "SEALED")
                                   sealed-vars))
                        a0)
                    sealed-lambda-list)))
    (values lambda-list sealed-vars)))

(proc-sealed-lambda-list '_ '((_ (_ (_ &rest data))) _))

(defmacro dbind (low-line-lambda-list expression &body body)
  (multiple-value-bind (lambda-list sealed-vars)
    (proc-sealed-lambda-list '_ low-line-lambda-list)
    `(destructuring-bind ,lambda-list
       ,expression
       (declare (ignore ,@sealed-vars))
       (progn ,@body))))

(dbind ((_ ((_ &rest column-names) (_ &rest data))) _)
       '((:RESULTS
           ((:COLUMNS "n")
            (:DATA
              ((:ROW ((:NAME . "Kaoru")))
               (:META ((:ID . 4) (:TYPE . "node") (:DELETED))))
              ((:ROW ((:NAME . "Tsukasa")))
               (:META ((:ID . 5) (:TYPE . "node") (:DELETED)))))))
         (:ERRORS))
       (values column-names data))
