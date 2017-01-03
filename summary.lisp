(defpackage :dbind
  (:use :cl :papply :chiku.util))

(in-package :dbind)

;;; Rainy lambda list is an extended lambda list whose parameters are
;;; rainy parameters. A rainy parameter is either a 1st order parameter
;;; or a 2nd order parameter. 1st order parameter is a symbol, while a
;;; 2nd order parameter is a multi level list of symbols (i.e. a tree of
;;; symbols implemented by lists). Except one special symbol called
;;; ``raindrop'', no symbol is allowed to appear multiple times in a
;;; rainy lambda list. Which symbol is the raindrop is decided in the
;;; application of Rainy lambda list.
;;;  A rainy lambda list is, from structural viewpoint, a variation of
;;; destructuring lambda list. These differs in 2 aspects. First, it is
;;; not allowed to use a single dot (.) within rainy lambda list. &REST
;;; works fine to express rest parameters. Second, one special symbol,
;;; raindrop, is allowed to appear multiple times. Symbols appear in
;;; rainy lambda list are used as variables in some applications.
;;; Variables named by raindrops are called ``raindrop variables''.
;;;
;;;  I will use this rainy lambda list in DBIND and MVBIND to attach
;;; igrnored variables feature to DESTRUCTURING-BIND and
;;; MULTIPLE-VALUE-BIND. However, I name the structure not the function.
;;; Ignoring some variables is the function of DBIND, not the function
;;; of rainy lambda list. If I name a function, then one actual
;;; structure that is used to provide 2 different function must be given
;;; different names. It is useful in some cases, and ugly in some other
;;; cases. But these 2 naming policy is blended in the specification of
;;; lambda list family in Common Lisp.
(defun seal-raindrops (raindrop rainy-lambda-list)
  (let* (sealed-vars
         (lambda-list
           (mapleaf #'(if (eq a0 raindrop)
                        (car (push (gensym "SEALED")
                                   sealed-vars))
                        a0)
                    rainy-lambda-list)))
    (values lambda-list sealed-vars)))

(seal-raindrops '_ '((_ (_ (_ &rest data))) _))

;;; Low-line lambda list is a rainy lambda list whose raindrop is _, a
;;; symbol whose name is composed of a single low-line (#\_).
(defmacro dbind (low-line-lambda-list expression &body body)
  " A variation of DESTRUCTURING-BIND that adopts low-line lambda list
   instead of destructuring lambda list. Low-line lambda list is a rainy
   lambda list whose raindrop is _, a symbol whose name is composed of a
   single low-line (#\_).
    As well as DESTRUCTURING-BIND, DBIND recognizes symbols in
   LOW-LINE-LAMBDA-LIST as variables and binds them to the corresponding
   values in the tree structure returned from EXPRESSION. Raindrop
   variables are DECLAREd as IGNORE (i.e. they are unavailable in BODY)."
  (multiple-value-bind (lambda-list sealed-vars)
    (seal-raindrops '_ low-line-lambda-list)
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
  " A variation of MULTIPLE-VALUE-BIND that adopts low-line lambda list1
   instead of the list of variables. MVBIND1 DECLAREs raindrop variables
   as IGNORE and thus, these variables are not available in BODY.
    Low-line lambda list1 is a Low-line lambda list which is composed of
   only symbols. tailing `1' means first order, a flat list of symbols."
  (multiple-value-bind (lambda-list sealed-vars)
    (seal-raindrops '_ low-line-lambda-list1)
    `(multiple-value-bind ,lambda-list
       ,expression
       (declare (ignore ,@sealed-vars))
       ,@body)))

(describe 'dbind)

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
;;; same purpose. It is disadvantageous for perfoance because a list is
;;; constructed. But if we wants to avoid to use MULTIPLE-VALUE-LIST,
;;; then we have to give up to specify DECLARE for the variables.
(mvbind (_ (a _) _ (b)) (values 'garbage '(0 1) 'garbage2 '(2))
  (print b))

(mvbind (_ var1 _ var3) (values 'garbage '(0 1) 'garbage2 '(0))
  (dbind (a _) var1
    (dbind (b) var3
      #+nil(declare (ignorable a)) ; Error!
      (print b))))

(mvbind (_) (values 1)
  (print 'done))

(mvbind (_ (a _) _ (b)) (values 'garbage '(0 1) 'garbage2 '(2))
  (print b))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun parameter1-p (param)
    "Checks if PARAM is a 1st order parameter."
    (symbolp param))

  (defun parameter2-p (param)
    "Checks if PARAM is a 2nd order parameter."
    (listp param))

  (defun gen-dbind-stack (params low-line-lambda-lists body)
    (cond ((null params) body)
          ((parameter1-p (car low-line-lambda-lists))
           (gen-dbind-stack
             (cdr params) (cdr low-line-lambda-lists) body))
          (t `((dbind ,(car low-line-lambda-lists) ,(car params)
                ,@(gen-dbind-stack
                    (cdr params) (cdr low-line-lambda-lists) body))))))

  (defun declare-used-p (body)
    (and (listp (car body))
         (eq (caar body) 'declare)))

  (defun lambda-list1-p (destructuring-lambda-list)
    (every #'parameter1-p destructuring-lambda-list))

  (defun cloak-parameter2 (low-line-lambda-list)
    "Cloaks 2nd order parameters by GENSYMs."
    (loop :for param :in low-line-lambda-list
          :for i = 0 :then (1+ i)
          :collect (if (parameter1-p param)
                     param
                     (gensym[] "2ND-PARAMETER" i)))))

(defmacro mvbind (low-line-lambda-list expression &body body)
  " A combination of MVBIND1 and DBIND. EXPRESSION returns multiple
   values and each parameter of LOW-LINE-LAMBDA-LIST is bound to the
   respective returned value. 1st order parameters are bound to the
   returned value itself. 2nd order parameters are bound in the
   destructuring manner. Raindrop variables are DECLAREd as IGNORE and
   made unavailable in BODY.
    Low-line lambda list is a rainy lambda list whose raindrop is _, a
   symbol whose name is composed of a single low-line (#\_).
    In order to support DECLARE specified by users, the combination of a
   single DBIND and MULTIPLE-VALUE-LIST is used when DECLARE is used at
   the top of BODY. It does not indebted by the multiple value in that
   situation. MVBIND1 can be an alternative if the destructuring feature
   is not needed."
  (cond ((lambda-list1-p low-line-lambda-list)
         `(mvbind1 ,low-line-lambda-list ,expression
            ,@body))
        ((declare-used-p body)
         `(dbind ,low-line-lambda-list (multiple-value-list ,expression)
            ,@body))
        (t (let ((mvvars (cloak-parameter2 low-line-lambda-list)))
             `(mvbind1 ,mvvars ,expression
                ,@(gen-dbind-stack mvvars low-line-lambda-list body))))))

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

(multiple-value-bind (a) (values 1) a)

;;; Flat low-line lambda list
(mvbind1 (a b) (values 1 2) a)
(mvbind (a b) (values 1 2) a)
(mvbind (a b) (values 1 2) (declare (ignore b)) a)
(mvbind (a b) (values 1 2) (declare (ignore a)) b)

(mvbind (a (b)) (values 1 (list 2)) a)
(mvbind (a (b)) (values 1 (list 2)) b)
(mvbind (a (b)) (values 1 (list 2)) (declare (ignore a)) b)

;;; Short values
(mvbind ((a) (b)) (values (list 1))) ; ERROR!

(mvbind ((a) b) (values (list 1)) ; NOT error
  a)

(mvbind ((a) b) (values (list 1))
  b)

;;; Too many values
(mvbind ((a) (b)) (values (list 1) (list 2) 3)
  (print a))

;;; Explanation
;;;
;;; 0. Name the structure, or name the function
;;; 1. Parameters vs Variables
;;; 2. 1st and 2nd order parameters
;;; 3. What is low-line lambda list?
;;; 4. Description of DBIND and MVBIND
;;;
;;; Consideration
;;;
;;; 1. Should an error be raised when values are in short?
;;;  First order parameters of MVBIND behaves same as the variables of
;;; MULTIPLE-VALUE-BIND. Therefore, first order parameters should not
;;; raise an error even if the values are in short.
;;;
;;;   (mvbind ((a) b) (values (list 1)))
;;;
;;; What should happen if no value is supplied to the 2nd order
;;; parameter?
;;;
;;;   (mvbind ((a) (b)) (values (list 1)))
;;;
;;; 2nd order parameters behave same as the variables of
;;; DESTRUCTURING-BIND and it raises error in such case.
;;;
;;;   (destructuring-bind (x y (list 1)) ; => ERROR!
;;;
;;; I decided to raise an error in this situation based on this aspect.
;;;
;;; 2. Should an error be raised when values are too much?
;;;
;;;  No. The reason is same to the Consideration 1. All multiple values
;;; are caught by MULTIPLE-VALUE-BIND (through MVBIND1 and each caught
;;; value is destructed by DBIND if needed -- if the parameter is 2nd
;;; order for short.
;;;
;;; 3. LET1 style binding in DBIND ... should it be supported?
;;;  Scheme has a variation of LET that is dedicated to bind a single
;;; value.
;;;
;;;    (let1 x 10 (print x))
;;;
;;; It is easy enough to support this style of binding in DBIND. But it
;;; does not destruct something. Principally, one operation should
;;; provide one feature. DBIND com bines destructuring and handy
;;; expression to skip some values, MVBIND combines DBIND with
;;; MULTIPLE-VALUE-BIND.

;;; UBIND stands for Universal BIND
;;; MBIND stands for Multiple BIND
;;; Fmm... still not enough to call it Universal.
(defmacro mbind (low-line-lambda-list expression &body body)
  (cond ((parameter1-p low-line-lambda-list)
         `(let ((,low-line-lambda-list ,expression))
            ,@body))
        ((lambda-list1-p low-line-lambda-list)
         `(mvbind1 ,low-line-lambda-list ,expression
            ,@body))
        ((declare-used-p body)
         `(dbind ,low-line-lambda-list (multiple-value-list ,expression)
            ,@body))
        (t (let ((mvvars (cloak-parameter2 low-line-lambda-list)))
             `(mvbind1 ,mvvars ,expression
                ,@(gen-dbind-stack mvvars low-line-lambda-list body))))))

(mbind a 1 a)

(mbind (a) 1
  (declare (ignorable a))
  a)

(mbind (a b c) (list 1 2 3)
  (declare (ignorable b c))
  a)

(dbind (a _ c) (list 1 2 3)
  (list a c ))

(mbind ((a _ c)) (list 1 2 3)
  (cons a c))

(defpackage :empty)

(in-package :empty)

(cl:print dbind::*tmp*)