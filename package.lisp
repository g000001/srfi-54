;;;; package.lisp

(cl:in-package :cl-user)


(defpackage "https://github.com/g000001/srfi-54"
  (:use)
  (:export cat)
  (:size 1))


(defpackage "https://github.com/g000001/srfi-54#internals"
  (:use "https://github.com/g000001/srfi-54"
        cl
        fiveam
        mbe
        srfi-5)
  (:shadowing-import-from srfi-23 error)
  (:shadowing-import-from srfi-5 let)
  (:shadow loop t))


;;; *EOF*
