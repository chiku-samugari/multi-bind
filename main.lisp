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
       ,@body)))

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

;;; 1 denotes 1st order lambda list. Since the lambda list is 1st order,
;;; which means flat, it does not offers destructuring feature.
(defmacro mvbind1 (low-line-lambda-list1 expression &body body)
  (multiple-value-bind (lambda-list sealed-vars)
    (proc-sealed-lambda-list '_ low-line-lambda-list1)
    `(multiple-value-bind ,lambda-list
       ,expression
       (declare (ignore ,@sealed-vars))
       ,@body)))

(multiple-value-bind (a b) (get-decoded-time)
  a)

(mvbind1 (_ _ _ a) (get-decoded-time)
  a)

;;; About the combination of MVBIND1 and DBIND, be aware that only the
;;; first level of the low-line lambda list is to catch the multiple
;;; values. There provided no method to return a list that includes
;;; multiple values. From this reason, we can replace MVBIND1 but cannot
;;; replace DBIND.
;;;
;;;   (dbind (a _ x) expr body)
;;;    == (mvbind ((a _ x)) expr body)
;;;
;;; We can use the combination of DBIND and MULTIPLE-VALUE-LIST for the
;;; same purpose. If we wants to avoid to use MULTIPLE-VALUE-LIST, then
;;; we have to give up to specify DECLARE for the variables.
(mvbind (_ (a _) _ (b)) (values 'garbage '(0 1) 'garbage2 '(2))
  (print b))

(mvbind (_ var1 _ var3) (values 'garbage '(0 1) 'garbage2 '(0))
  (dbind (a _) var1
    (dbind (b) var3
      #+nil(declare (ignorable a)) ; Error!
      (print b))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun gen-dbind-stack (vars low-line-lambda-lists body)
    (cond ((null mvvars) `(progn ,@body))
          ((eq (car mvvars) '_)
           (gen-dbind-stack
             (cdr mvvars) (cdr low-line-lambda-lists) body))
          (t `(dbind ,(car low-line-lambda-lists) ,(car mvvars)
                ,(gen-dbind-stack
                   (cdr mvvars) (cdr low-line-lambda-lists) body))))))

(gen-dbind-stack '(_ var1 _ var3) '(_ (a _) _ (b)) '(print b))

(defmacro mvbind (low-line-lambda-list expression &body body)
  (let ((mvvars (loop :for var :in low-line-lambda-list
                      :for i = 0 :then (1+ i)
                      :collect (if (eq var '_)
                                 '_
                                 (gensym[] "MVVAR" i)))))
    `(mvbind1 ,mvvars ,expression
       ,(gen-dbind-stack mvvars low-line-lambda-list body))))

(mvbind (_) (values 1)
  (print 'done))

(mvbind (_ (a _) _ (b)) (values 'garbage '(0 1) 'garbage2 '(2))
  (print b))

(defmacro mvbind-decl (low-line-lambda-list expression &body body)
  `(dbind ,low-line-lambda-list (multiple-value-list ,expression)
     ,@body))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun gen-dbind-stack (vars low-line-lambda-lists body)
    (cond ((null vars) body)
          ((eq (car vars) '_)
           (gen-dbind-stack
             (cdr vars) (cdr low-line-lambda-lists) body))
          (t `((dbind ,(car low-line-lambda-lists) ,(car vars)
                ,@(gen-dbind-stack
                    (cdr vars) (cdr low-line-lambda-lists) body))))))

  (defun declare-used-p (low-line-lambda-list body)
    (and (listp (car body))
         (eq  (caar body)'declare)
         (not (every #'symbolp low-line-lambda-list)))))

(defmacro mvbind (low-line-lambda-list expression &body body)
  (if (declare-used-p low-line-lambda-list body)
    `(dbind ,low-line-lambda-list (multiple-value-list ,expression)
       ,@body)
    (let ((mvvars (loop :for var :in low-line-lambda-list
                        :for i = 0 :then (1+ i)
                        :collect (if (eq var '_)
                                   '_
                                   (gensym[] "MVVAR" i)))))
      `(mvbind1 ,mvvars ,expression
         ,@(gen-dbind-stack mvvars low-line-lambda-list body)))))

(mvbind (_) (values 1)
  (print 'done))

(mvbind (_ (a _) _ (b)) (values 'garbage '(0 1) 'garbage2 '(2))
  a
  (print b))

(mvbind (_ (a _) _ (b)) (values 'garbage '(0 1) 'garbage2 '(2))
  (declare (ignore a))
  (print b))

(DBIND (_ (A _) _ (B))
    (MULTIPLE-VALUE-LIST (VALUES 'GARBAGE '(0 1) 'GARBAGE2 '(2)))
  (DECLARE (IGNORE A))
  (PRINT B))

(mvbind (_ (a _) _ (b)) (values 'garbage '(0 1) 'garbage2 '(2))
  (print b))
