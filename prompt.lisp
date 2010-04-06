
;(in-package #:clalish)

(defmacro thunk (&rest forms)
  `(lambda () ,@forms))

(defmacro cons! (a b)
  `(setf ,b (cons ,a ,b)))

;;; Prompt string

#+clisp
(progn
  
(unless (boundp '*clisp-count*)
  (defparameter *clisp-count* (getenv "CLISP_COUNT"))
  (setf *clisp-count* (if *clisp-count* (1+ (parse-integer *clisp-count*)) 1))
  (setf (getenv "CLISP_COUNT") (write-to-string *clisp-count*))
  (cons!
   (thunk (setf (getenv "CLISP_COUNT") (write-to-string (1- *clisp-count*))))
   custom:*fini-hooks*)
  (defparameter *clisp-indent* (string-repeat "  " (1- *clisp-count*))))

(setf custom:*prompt-start* "")
(setf custom:*prompt-finish* "")
(setf custom:*prompt-break* (thunk ""))
(setf custom:*prompt-step* (thunk ""))
(setf custom:*prompt-body*
      (thunk
       (concat
        *clisp-indent*
        (getenv "USER")
        " "
        (replace-all (sh-string-std :pwd) (getenv "HOME") "~")
        (string-if (ext:prompt-new-package)
                   (concat " "
                           (string-downcase
                            (ext:package-shortest-name *package*))))
        (string-if (>0 (ext:break-level))
                   (re-format " break[~a]" (ext:break-level)))
        (string-if (>0 (ext:step-level))
                   (re-format " step[~a]" (ext:step-level)))
        " ")))

) ; clisp only

