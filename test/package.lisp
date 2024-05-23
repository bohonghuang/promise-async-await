(defpackage promise-async-await.test
  (:use #:cl #:parachute #:promise-async-await))

(in-package #:promise-async-await.test)

(define-test suite)

(defun random-async ()
  (async (random 100)))

(defun random-sum-async ()
  (async
    (+ (await (random-async))
       (await (random-async))
       (await (random-async))
       (await (random-async))
       (await (random-async))
       (await (random-async))
       (await (random-async)))))

(defun sleep-async (secs)
  (org.shirakumo.promise:with-promise (succ)
    (bt2:make-thread (lambda ()
                       (sleep secs)
                       (succ secs)))))

(defun sleep-1234-async ()
  (async
    (+ (await (sleep-async 1/100))
       (await (sleep-async 2/100))
       (await (sleep-async 3/100))
       (await (sleep-async 4/100)))))

(defun sleep-1234-aselect-ajoin ()
  (async
    (apply #'+ (cons (await (aselect (sleep-async 1/100) (sleep-async 4/100)))
                     (await (ajoin (sleep-async 2/100) (sleep-async 3/100)))))))

(define-test simple :parent suite
  (org.shirakumo.promise:clear)
  (org.shirakumo.promise:then
   (random-sum-async)
   (lambda (value) (true (integerp value))))
  (org.shirakumo.promise:tick-all 0))

(define-test sleep :parent suite
  (org.shirakumo.promise:clear)
  (async
    (let ((result (await (sleep-1234-async))))
      (is = 1/10 result))
    (let ((result (await (sleep-1234-aselect-ajoin))))
      (is member '(6/100 9/100) result)))
  (loop :while org.shirakumo.promise::*promises*
        :do (org.shirakumo.promise:tick-all 0.01)
            (sleep 0.01)))

(defvar *sp1* 0)
(defvar *sp2*)
(defvar *sp3*)

(define-test special-variable :parent suite :depends-on (sleep)
  (org.shirakumo.promise:clear)
  (let ((*sp1* 1) (*sp2* 2) (*sp3* 3) (*sp4* 4))
    (declare (special *sp4*))
    (let ((*async-continuation-constructor* (async-special-variable-binder (*sp1* *sp2*))))
      (let ((*async-continuation-constructor* (async-special-variable-binder (*sp3* *sp4*))))
        (async
          (is = 1 *sp1*)
          (is = 2 *sp2*)
          (is = 3 *sp3*)
          (is = 4 *sp4*)
          (await (sleep-async 1/100))
          (is = 1 *sp1*)
          (is = 2 *sp2*)
          (is = 3 *sp3*)
          (is = 4 *sp4*)
          (await (sleep-async 1/100))
          (is = 1 *sp1*)
          (is = 2 *sp2*)
          (is = 3 *sp3*)
          (is = 4 *sp4*)))))
  (loop :while org.shirakumo.promise::*promises*
        :do (org.shirakumo.promise:tick-all 0.01)
            (sleep 0.01)))

(define-test nest :parent suite :depends-on (sleep)
  (org.shirakumo.promise:clear)
  (async
    (let ((task-1 (async (loop :repeat 5 :do (await (sleep-async 0.05)) :sum 1)))
          (task-2 (async (loop :repeat 5 :do (await (sleep-async 0.05)) :sum 2))))
      (let ((result (reduce #'+ (await (ajoin task-1 task-2)))))
        (is = 15 result))))
  (loop :while org.shirakumo.promise::*promises*
        :do (org.shirakumo.promise:tick-all 0.01)
            (sleep 0.01)))
