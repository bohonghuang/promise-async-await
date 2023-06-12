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
    (apply #'+ (cons (await (aselect (list (sleep-async 1/100) (sleep-async 4/100))))
                     (await (ajoin (list (sleep-async 2/100) (sleep-async 3/100))))))))

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
