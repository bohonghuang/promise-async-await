(defpackage promise-async-await
  (:use #:cl #:alexandria #:cl-cont #:cl-cont-optimizer
        #:org.shirakumo.promise)
  (:export #:async #:await #:ajoin #:aselect #:alet))

(in-package #:promise-async-await)

(defmacro async (&body body)
  (with-gensyms (succeed result)
    `(with-promise (,succeed)
       (with-cont-optimizer
         (with-call/cc
           (let ((,result (progn . ,body)))
             (,succeed ,result)))))))

(defun funcall-with-async-continuation-constructor ()
  (declare (special *async-continuation-constructor*))
  (let ((constructor *async-continuation-constructor*))
    (lambda (continuation &rest args &aux (*async-continuation-constructor* constructor))
      (declare (special *async-continuation-constructor*) (dynamic-extent args))
      (apply continuation args))))

(defparameter *async-continuation-constructor* #'funcall-with-async-continuation-constructor)
(declaim (type (function () (values (function ((or function cont::funcallable/cc) &rest t)))) *async-continuation-constructor*))

(defmacro funcall-with-special-variable-captured (funcall vars)
  (with-gensyms (continuation args)
    (let ((var-names (mapcar (compose #'gensym #'symbol-name) vars)))
      (once-only (funcall)
        `(let ,(mapcar #'list var-names vars)
           (lambda (,continuation &rest ,args)
             (declare (dynamic-extent ,args))
             (let ,(mapcar #'list vars var-names)
               (declare (special . ,vars))
               (apply ,funcall ,continuation ,args))))))))

(defmacro async-special-variable-binder (vars)
  (with-gensyms (constructor)
    `(let ((,constructor *async-continuation-constructor*))
       (lambda () (funcall-with-special-variable-captured (funcall ,constructor) ,vars)))))

(defmacro await (awaitable)
  (with-gensyms (promise cc)
    `(let/cc ,cc
       (let ((,promise ,awaitable))
         (then ,promise (curry (funcall *async-continuation-constructor*) ,cc))
         (tick ,promise 0)))))

(defun aselect (&rest awaitables)
  (any awaitables))

(defun ajoin (&rest awaitables)
  (all awaitables))

(defun body-declarations (body)
  (loop :for forms :on body
        :if (and (listp (car forms)) (eq (caar forms) 'declare))
          :collect (car forms) :into declarations
        :else
          :return (values declarations forms)))

(body-declarations '((declare (type fixnum a))
                     (values t)))

(defmacro alet (bindings &body body)
  (multiple-value-bind (declarations body) (body-declarations body)
    (let ((spvars (loop :for (nil . clauses) :in declarations
                        :append (loop :for (declaration-type . vars) :in clauses
                                      :when (eq declaration-type 'special)
                                        :append (mapcar (curry #'make-list 2 :initial-element) vars)))))
      `(let ,(loop :for binding :in bindings
                   :for (var val) := (ensure-cons binding)
                   :for spvar-cons := (assoc var spvars)
                   :if spvar-cons
                     :do (setf (cdr spvar-cons) (list (setf var (gensym (symbol-name var)))))
                   :else
                     :do (assert (not (boundp var)) () "Special variable ~A must be declared as SPECIAL explicitly." var)
                   :unless (eq var val)
                     :collect `(,var ,val))
         ,@declarations
         ,@(if spvars
               (with-gensyms (f cc)
                 `((let ((,f (lambda (,cc . ,(mapcar #'first spvars)) (funcall ,cc (progn . ,body)))))
                     (let/cc ,cc
                       (progv '(*async-continuation-constructor* . ,(mapcar #'first spvars))
                           (list (async-special-variable-binder ,(mapcar #'first spvars)) . ,(mapcar #'second spvars))
                         (funcall ,f ,cc . ,(mapcar #'first spvars)))))))
               body)))))
