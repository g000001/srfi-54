;;;; srfi-54.lisp

(cl:in-package "https://github.com/g000001/srfi-54#internals")

(defun std-princ-to-string (obj)
  (#|with-standard-io-syntax|#
   let ((*print-case* :upcase))
    (princ-to-string obj)))

(defun std-write (&rest args)
  (#|with-standard-io-syntax|#
   let ((*print-case* :upcase))
    (apply #'write args)))

(defun std-princ (&rest args)
  (#|with-standard-io-syntax|#
   let ((*print-case* :upcase))
    (apply #'princ args)))

(define-syntax alet-cat*                ; borrowed from SRFI-86
  (syntax-rules ()
    ((alet-cat* z (a . e) bd ***)
     (with ((y (gensym)))
       (let ((y z))
         (%alet-cat* y (a . e) bd ***))))))

(define-syntax %alet-cat*               ; borrowed from SRFI-86
  (syntax-rules ()
    ((%alet-cat* z ((n d t ***)) bd ***)
     (let ((n (if (null z)
		  d
		  (if (null (cdr z))
		      (wow-cat-end z n t ***)
		      (error "alet*: too many arguments" (cdr z))))))
       bd ***))
    ((%alet-cat* z ((n d t ***) . e) bd ***)
     (let ((n (if (null z)
		  d
                  (wow-cat! z n d t ***))))
       (%alet-cat* z e bd ***)))
    ((%alet-cat* z e bd ***)
     (let ((e z))
       bd ***))))

(define-syntax wow-cat!                 ; borrowed from SRFI-86
  (syntax-rules ()
    ((wow-cat! z n d)
     (let ((n (car z)))
       (setq z (cdr z))
       n))
    ((wow-cat! z n d t)
     (with ((lp (gensym)))
       (let ((n (car z)))
         (if t
             (progn (setq z (cdr z)) n)
             (let lp ((head (list n)) (tail (cdr z)))
                  (if (null tail)
                      d
                      (let ((n (car tail)))
                        (if t
                            (progn (setq z (append (reverse head) (cdr tail))) n)
                            (lp (cons n head) (cdr tail))))))))))
    ((wow-cat! z n d t ts)
     (with ((lp (gensym)))
       (let ((n (car z)))
         (if t
             (progn (setq z (cdr z)) ts)
             (let lp ((head (list n)) (tail (cdr z)))
                  (if (null tail)
                      d
                      (let ((n (car tail)))
                        (if t
                            (progn (setq z (append (reverse head) (cdr tail))) ts)
                            (lp (cons n head) (cdr tail))))))))))
    ((wow-cat! z n d t ts fs)
     (let ((n (car z)))
       (if t
	   (progn (setq z (cdr z)) ts)
	   (progn (setq z (cdr z)) fs))))))

(define-syntax wow-cat-end              ; borrowed from SRFI-86
  (syntax-rules ()
    ((wow-cat-end z n)
     (car z))
    ((wow-cat-end z n t)
     (let ((n (car z)))
       (if t n (error "alet[*]: too many argument" z))))
    ((wow-cat-end z n t ts)
     (let ((n (car z)))
       (if t ts (error "alet[*]: too many argument" z))))
    ((wow-cat-end z n t ts fs)
     (let ((n (car z)))
       (if t ts fs)))))

(defun str-index (str char)
  (let ((len (length str)))
    (let lp ((n 0))
      (and (< n len)
	   (if (char= char (char str n))
	       n
	       (lp (+ n 1)))))))

(defun every? (pred ls)
  (let lp ((ls ls))
    (or (null ls)
	(and (funcall pred (car ls))
	     (lp (cdr ls))))))

(defun part (pred ls)
  (let lp ((ls ls) (true '()) (false '()))
    (cond
     ((null ls) (cons (reverse true) (reverse false)))
     ((funcall pred (car ls)) (lp (cdr ls) (cons (car ls) true) false))
     (:else (lp (cdr ls) true (cons (car ls) false))))))

(defun e-mold (num pre)
  (let* ((str (std-princ-to-string (float num)))
	 (e-index (str-index str #\e)))
    (if e-index
	(concatenate 'string
                     (mold (subseq str 0 e-index) pre)
                     (subseq str e-index (length str)))
	(mold str pre))))

(defun mold (str pre)
  (let ((ind (str-index str #\.)))
    (if ind
	(let ((d-len (- (length str) (+ ind 1))))
	  (cond
	   ((= d-len pre) str)
	   ((< d-len pre) (concatenate 'string
                                       str
                                       (make-string (- pre d-len) :initial-element #\0)))
	   ;;((char< #\4 (string-ref str (+ 1 ind pre)))
	   ;;(let ((com (expt 10 pre)))
	   ;;  (number->string (/ (round (* (string->number str) com)) com))))
	   ((or (char< #\5 (char str (+ 1 ind pre)))
		(and (char= #\5 (char str (+ 1 ind pre)))
		     (or (< (+ 1 pre) d-len)
			 (member (char str (+ ind (if (= 0 pre) -1 pre)))
			       '(#\1 #\3 #\5 #\7 #\9)))))
	    (coerce
	     (let* ((minus (char= #\- (char str 0)))
		    (str (subseq str (if minus 1 0) (+ 1 ind pre)))
		    (char-list
		     (reverse
		      (let lp ((index (- (length str) 1))
			       (raise cl:T))
			(if (= -1 index)
			    (if raise '(#\1) '())
			    (let ((chr (char str index)))
			      (if (char= #\. chr)
				  (cons chr (lp (- index 1) raise))
				  (if raise
				      (if (char= #\9 chr)
					  (cons #\0 (lp (- index 1) raise))
					  (cons (code-char
						 (+ 1 (char-code chr)))
						(lp (- index 1) nil)))
				      (cons chr (lp (- index 1) raise))))))))))
	       (if minus (cons #\- char-list) char-list))
	     'STRING))
	   (:else
	    (subseq str 0 (+ 1 ind pre)))))
	(concatenate 'string
                     str
                     "."
                     (make-string pre :initial-element #\0)))))

(defun separate (str sep num opt)
  (let* ((len (length str))
	 (pos (if opt
		  (let ((pos (rem (if (eq opt :minus) (- len 1) len)
                                  num)))
		    (if (= 0 pos) num pos))
		  num)))
    (apply #'concatenate
           'string
	   (let loop ((ini 0)
		      (pos (if (eq opt :minus) (+ pos 1) pos)))
	     (if (< pos len)
		 (cons (subseq str ini pos)
		       (cons sep (loop pos (+ pos num))))
		 (list (subseq str ini len)))))))

(defun cat (object &rest rest)
  (let* ((str-rest (part #'stringp rest))
	 (str-list (car str-rest))
	 (rest-list (cdr str-rest)) )
    (if (null rest-list)
	(apply #'concatenate
               'string
	       (cond
                 ((numberp object) (std-princ-to-string object))
                 ((stringp object) object)
                 ((characterp object) (string object))
                 ((typep object 'boolean) (if object "T" "nil"))
                 ((symbolp object) (string object))
                 (:else
                  (get-output-stream-string
                   (let ((str-port (make-string-output-stream)))
                     (std-write object :stream str-port)
                     str-port ))))
	       str-list )
        (alet-cat* rest-list
         ((width 0 (integerp width))
          (port nil (or (typep port 'boolean)
                        (and (streamp port)
                             (output-stream-p port)))
                (if (eq port cl:T) *standard-output* port) )
          (char #\space (characterp char))
          (converter nil (and (consp converter)
                              (functionp (car converter))
                              (functionp (cdr converter)) ))
          (precision nil (and (floatp precision)
                              (zerop (rem precision 1))))
          (sign nil (eq :sign sign))
          (radix :decimal
                 (member radix '(:decimal :octal :binary :hexadecimal)) )
          (exactness nil (member exactness '(:exact :inexact)))
          (separator nil (and (listp separator)
                              (< 0 (length separator) 3)
                              (characterp (car separator))
                              (or (null (cdr separator))
                                  (let ((n (cadr separator)))
                                    (and (integerp n) (rationalp n)
                                         (< 0 n) )))))
          (writer nil (functionp writer))
          (pipe nil (and (listp pipe)
                         (not (null pipe))
                         (every? #'functionp pipe) ))
          (take nil (and (listp take)
                         (< 0 (length take) 3)
                         (every? (lambda (x)
                                   (and (integerp x) (rationalp x)) )
                                 take ))))
         (let* ((str
                 (cond
                   ((and converter
                         (funcall (car converter) object) )
                    (let* ((str (funcall (cdr converter) object))
                           (pad (- (abs width) (length str))) )
                      (cond
                        ((<= pad 0) str)
                        ((< 0 width) (concatenate
                                      'string
                                      (make-string pad
                                                   :initial-element char)
                                      str))
                        (:else (concatenate
                                'string
                                str
                                (make-string pad :initial-element char))) )))
                   ((numberp object)
                    (and (not (eq radix :decimal)) precision
                         (error "cat: non-decimal cannot have a decimal point") )
                    (and precision (< precision 0) (eq exactness :exact)
                         (error "cat: exact number cannot have a decimal point without exact sign") )
                    (let* ((exact-sign (and precision
                                            (<= 0 precision)
                                            (or (eq exactness :exact)
                                                (and (rationalp object)
                                                     (not (eq exactness
                                                              :inexact ))))
                                            "#e" ))
                           (inexact-sign (and (not (eq radix :decimal))
                                              (or (and (not (rationalp object))
                                                       (not (eq exactness
                                                                :exact )))
                                                  (eq exactness :inexact) )
                                              "#i" ))
                           (radix-sign (cdr (assoc radix
                                                  '((:decimal . nil)
                                                    (:octal . "#o")
                                                    (:binary . "#b")
                                                    (:hexadecimal . "#x") ))))
                           (plus-sign (and sign (< 0 (realpart object)) "+"))
                           (exactness-sign (or exact-sign inexact-sign))
                           (str
                            (if precision
                                (let ((precision (rationalize
                                                  (abs precision) ))
                                      (imag (imagpart object)) )
                                  (if (= 0 imag)
                                      (e-mold object precision)
                                      (concatenate 'string
                                       (e-mold (realpart object) precision)
                                       (if (< 0 imag) "+" "")
                                       (e-mold imag precision)
                                       "i" )))
                                (format nil
                                        "~VR"
                                        (cdr (assoc radix '((:decimal . 10)
                                                            (:octal . 8)
                                                            (:binary . 2)
                                                            (:hexadecimal . 16) )))
                                        (cond
                                          (inexact-sign (rationalize object))
                                          (exactness
                                           (if (eq exactness :exact)
                                               (rationalize object)
                                               (float object) ))
                                          (:else object) ))))
                           (str
                            (if (and separator
                                     (not (or (and (eq radix :decimal)
                                                   (str-index str #\e) )
                                              (str-index str #\i)
                                              (str-index str #\/) )))
                                (let ((sep (string (car separator)))
                                      (num (if (null (cdr separator))
                                               3 (cadr separator)))
                                      (dot-index (str-index str #\.)) )
                                  (if dot-index
                                      (concatenate 'string
                                       (separate (subseq str 0 dot-index)
                                                 sep num (if (< object 0)
                                                             :minus cl:T))
                                       "."
                                       (separate (subseq
                                                  str (+ 1 dot-index)
                                                  (length str) )
                                                 sep num nil))
                                      (separate str sep num (if (< object 0)
                                                                :minus cl:T))))
                                str ))
                           (pad (- (abs width)
                                   (+ (length str)
                                      (if exactness-sign 2 0)
                                      (if radix-sign 2 0)
                                      (if plus-sign 1 0) )))
                           (pad (if (< 0 pad) pad 0)) )
                      (if (< 0 width)
                          (if (digit-char-p char)
                              (if (< (realpart object) 0)
                                  (concatenate 'string (or exactness-sign "")
                                                 (or radix-sign "")
                                                 "-"
                                                 (make-string pad
                                                              :initial-element char)
                                                 (subseq str 1
                                                         (length
                                                          str )))
                                  (concatenate 'string (or exactness-sign "")
                                                 (or radix-sign "")
                                                 (or plus-sign "")
                                                 (make-string pad
                                                              :initial-element char)
                                                 str ))
                              (concatenate 'string (make-string pad
                                                                :initial-element char)
                                             (or exactness-sign "")
                                             (or radix-sign "")
                                             (or plus-sign "")
                                             str ))
                          (concatenate 'string (or exactness-sign "")
                                         (or radix-sign "")
                                         (or plus-sign "")
                                         str
                                         (make-string pad
                                                      :initial-element char) ))))
                   (:else
                    (let* ((str (cond
                                  (writer (get-output-stream-string
                                           (let ((str-port
                                                  (make-string-output-stream) ))
                                             (funcall writer object str-port)
                                             str-port )))
                                  ((stringp object) object)
                                  ((characterp object) (string object))
                                  ((typep object 'boolean) (if object "T" "nil"))
                                  ((symbolp object) (symbol-name object))
                                  (:else (get-output-stream-string
                                          (let ((str-port (make-string-output-stream)))
                                            (std-write object :stream str-port)
                                            str-port )))))
                           (str (if pipe
                                    (let loop ((str (funcall (car pipe) str))
                                               (fns (cdr pipe)) )
                                         (if (null fns)
                                             str
                                             (loop (funcall (car fns) str)
                                                   (cdr fns) )))
                                    str ))
                           (str
                            (if take
                                (let ((left (car take))
                                      (right (if (null (cdr take))
                                                 0 (cadr take)))
                                      (len (length str)) )
                                  (labels ((substr (str beg end)
                                             (let ((end (cond
                                                          ((< end 0) 0)
                                                          ((< len end) len)
                                                          (:else end) ))
                                                   (beg (cond
                                                          ((< beg 0) 0)
                                                          ((< len beg) len)
                                                          (:else beg) )))
                                               (if (and (= beg 0) (= end len))
                                                   str
                                                   (subseq str beg end) ))))
                                    (concatenate 'string
                                                 (if (< left 0)
                                                     (substr str (abs left) len)
                                                     (substr str 0 left) )
                                                 (if (< right 0)
                                                     (substr str 0 (+ len right))
                                                     (substr str (- len right) len) ))))
                                str ))
                           (pad (- (abs width) (length str))) )
                      (cond
                        ((<= pad 0) str)
                        ((< 0 width)
                         (concatenate 'string
                                      (make-string pad :initial-element char)
                                      str))
                        (:else
                         (concatenate 'string
                                      str
                                      (make-string pad
                                                   :initial-element char))))))))
                (str (apply #'concatenate 'string str str-list)) )
           (and port (std-princ str port))
           str )))))

;;; eof
