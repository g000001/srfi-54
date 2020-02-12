;;;; srfi-54.asd

(cl:in-package :asdf)


(defsystem :srfi-54
  :version "20200213"
  :description "SRFI 54 for CL: Formatting"
  :long-description "SRFI 54 for CL: Formatting
https://srfi.schemers.org/srfi-54"
  :author "Joo ChurlSoo"
  :maintainer "CHIBA Masaomi"
  :license "Unlicense"
  :serial t
  :depends-on (:mbe :fiveam :srfi-23 :srfi-5)
  :components ((:file "package")
               (:file "srfi-54")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-54))))
  (let ((name "https://github.com/g000001/srfi-54")
        (nickname :srfi-54))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-54))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-54#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-54)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
