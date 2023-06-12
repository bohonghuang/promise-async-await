(defpackage promise-async-await
  (:use #:cl #:alexandria #:cl-cont #:cl-cont-optimizer
        #:org.shirakumo.promise)
  (:export #:async #:await #:ajoin #:aselect))

(in-package #:promise-async-await)

(defmacro async (&body body)
  (with-gensyms (succeed result)
    `(with-promise (,succeed)
       (with-cont-optimizer
         (with-call/cc
           (let ((,result (progn . ,body)))
             (,succeed ,result)))))))

(defmacro await (awaitable)
  (with-gensyms (promise cc)
    `(call/cc (lambda (,cc)
                (let ((,promise ,awaitable))
                  (then ,promise ,cc)
                  (tick ,promise 0))))))

(setf (fdefinition 'ajoin) (fdefinition 'all)
      (fdefinition 'aselect) (fdefinition 'any))
