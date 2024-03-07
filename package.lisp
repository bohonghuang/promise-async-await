(defpackage promise-async-await
  (:use #:cl #:alexandria #:cl-cont #:cl-cont-optimizer
        #:org.shirakumo.promise)
  (:export #:async #:await #:ajoin #:aselect #:*async-continuation-constructor* #:async-special-variable-binder))

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
