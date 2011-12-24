;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-54
  (:use)
  (:export :cat))

(defpackage :srfi-54.internal
  (:use :srfi-54 :cl :fiveam :mbe :srfi-5)
  (:shadowing-import-from :srfi-23 :error)
  (:shadowing-import-from :srfi-5 :let)
  (:shadow :loop))
