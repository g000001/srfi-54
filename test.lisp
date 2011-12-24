(cl:in-package :srfi-54.internal)

(def-suite srfi-54)

(in-suite srfi-54)

(defmacro dotests (&body body)
  (do ((*gensym-counter* 0)
       (tests (reverse body)
                         (cdddr tests) )
       (es '()
           (destructuring-bind (test => res &rest ignore)
                               tests
             (declare (ignore => ignore))
             (cons `(TEST ,(gensym)
                      (IS (EQUAL ,test ,res )))
                   es ))))
      ((endp tests)
       `(progn ,@es)) ))

(dotests
  (cat 129.995 -10 2.0) => "130.00    "
  (cat 129.995 10 2.0) => "    130.00"
  (cat 129.985 10 2.0) => "    129.98"
  ;; -> "    129.99"
  (cat 129.985001 10 2.0) => "    129.98"
  (cat 129.995 2.0 :exact) => "#e130.00"
  (cat 129 -2.0) => "129.00"
  (cat 129 2.0) => "#e129.00"
  (cat 129 10 2.0 #\0 :sign) => "#e+0129.00"
  (cat 129 10 2.0 #\* :sign) => "*#e+129.00"

  (cat 1/3) => "1/3"
  (cat 1/3 10 2.0) => "    #e0.33"
  (cat 1/3 10 -2.0) => "      0.33"
  (cat 129.995 10 '(#\, 2)) => " 1,29.99,5"
  (cat 129995 10 '(#\,) :sign) => "  +129,995"
  (cat (cat 129.995 0.0) '(0 -1)) => "130"
  (cat 99.5 10 :sign :octal) => "#i#o+307/2"
  (cat 99.5 10 :sign :octal :exact) => "  #o+307/2"
  (cat #x123 :octal :sign) => "#o+443"
  (cat #x123 -10 2.0 :sign #\*) => "#e+291.00*"
  #-allegro (cat #c(-1.2345e+15 1.2355e-15) 3.0)
  #-allegro =>
  #-allegro "-1.234e15+1.236e-15i"
  #+allegro (cat #c(-1.2345e+15 1.2355e-15) 3.0)
  #+allegro =>
  #+allegro "-1.234e+15+1.236e-15i"
  #-allegro (cat 1.2345e+15 10 3.0 :sign)
  #-allegro =>
  #-allegro " +1.234e15"
  #+allegro (cat 1.2345e+15 10 3.0 :sign)
  #+allegro =>
  #+allegro "+1.234e+15"
  (cat "string" -10) => "string    "
  (cat "string" 10 (list #'string-upcase)) => "    STRING"
  (cat "string" 10 (list #'string-upcase) '(-2)) => "      RING"
  (cat "string" 10 `(,#'string-capitalize) '(2 3)) => "     Sting"
  (cat "string" `(,#'reverse ,#'string-upcase)) => "GNIRTS"
  (cat #\a 10) => "         a"
  (cat 'symbol 10) => "    SYMBOL"
  (cat '#(#\a "str" s)) => "#(#\\a \"str\" S)"
  (cat '(#\a "str" s)) => "(#\\a \"str\" S)"

  (let ((*standard-output* (make-string-output-stream)))
    (list (cat '(#\a "str" s) T)
          (get-output-stream-string *standard-output*) ))
  => '("(#\\a \"str\" S)" "(#\\a \"str\" S)")

  (let ((*standard-output* (make-string-output-stream)))
    (list (cat '(#\a "str" s) *standard-output*)
          (get-output-stream-string *standard-output*) ))
  => '("(#\\a \"str\" S)" "(#\\a \"str\" S)")

  (cat 3 (cat 's) " " (cat "str" (lambda (obj srm) (write obj :stream srm))))
  => "3S \"str\""

  (let ((*standard-output* (make-string-output-stream)))
    (list (cat 3 T (cat 's) " " (cat "str"
                                     (lambda (obj srm) (write obj :stream srm)) ))
          (get-output-stream-string *standard-output*) ))
  => '("3S \"str\"" "3S \"str\"")


  (let ((*standard-output* (make-string-output-stream)))
    (list (cat 3 T (cat 's T) " "
               (cat "str"
                    (lambda (obj srm) (write obj :stream srm)) ))
          (get-output-stream-string *standard-output*) ))
  => '("3S \"str\"" "S3S \"str\"")

  )

(progn
  (defstruct (example
                (:predicate example?)
                (:conc-name "GET-") )
    num
    str )

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (symbol-function 'set-num!)
          (lambda (obj val)
            (funcall #'(setf get-num) val obj) ))
    (setf (symbol-function 'set-str!)
          (lambda (obj val)
            (funcall #'(setf get-str) val obj) )))

    (defparameter ex (make-example :num 123 :str "string"))

    (defun record->string (object)
      (cat (get-num object) "-" (get-str object)))

    (defun record-writer (object string-port)
      (if (example? object)
          (progn (princ (get-num object) string-port)
                 (princ "-" string-port)
                 (princ (get-str object) string-port))
          (funcall (or (and (or (stringp object)
                                (characterp object)
                                (typep object 'boolean))
                            #'princ)
                       (lambda (obj srm)
                         (write obj :stream srm)))
                   object string-port)))
    )

(dotests
  (cat ex) => "#S(EXAMPLE :NUM 123 :STR \"string\")"
  (cat ex 20 #'record-writer) => "          123-string"
  (cat ex 20 #'record-writer
       `(,(lambda (x) (delete-if #'digit-char-p x))
          ,#'string-upcase ,#'reverse)
       '(0 -1) #\-)
  => "--------------GNIRTS"
  (cat "string" 20 #'record-writer
       (list #'string-upcase) '(2 3) #\-)
  => "---------------STING"
  (cat 12 20 #'record-writer 3.0) => "            #e12.000"
  (cat ex 20 (cons #'example? #'record->string)) => "          123-string"
  (cat "string" 20 (cons #'example? #'record->string)
       (list #'string-upcase) '(2 3) #\-)
  => "---------------STING"
  (cat 12 20 (cons #'example? #'record->string) -3.0)
  => "              12.000"
  (cat ex 20 (cons #'example? #'record->string)
       `(,(lambda (x) (delete #'digit-char-p x))
          ,#'string-upcase ,#'reverse)
       '(0 -1) #\-)
  => "----------123-string"
  )

;;; eof
