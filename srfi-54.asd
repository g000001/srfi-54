;;;; srfi-54.asd

(cl:in-package :asdf)

(defsystem :srfi-54
  :serial t
  :depends-on (:mbe :fiveam :srfi-23 :srfi-5)
  :components ((:file "package")
               (:file "srfi-54")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-54))))
  (load-system :srfi-54)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-54.internal :srfi-54))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
