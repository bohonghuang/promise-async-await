(defsystem promise-async-await
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :description "Async/await support for Shinmera's promise library."
  :homepage "https://github.com/bohonghuang/promise-async-await"
  :bug-tracker "https://github.com/bohonghuang/promise-async-await/issues"
  :source-control (:git "https://github.com/bohonghuang/promise-async-await.git")
  :components ((:file "package"))
  :depends-on (#:alexandria #:cl-cont #:cl-cont-optimizer #:promise)
  :in-order-to ((test-op (test-op #:promise-async-await/test))))

(defsystem promise-async-await/test
  :depends-on (#:promise-async-await #:parachute #:bordeaux-threads)
  :pathname "./test/"
  :components ((:file "package"))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:promise-async-await.test))))
