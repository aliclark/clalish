;-*-Lisp-*-

;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; TODO: work out how to do wild pathnames or some kind of
;;;; completion.
;;;; TODO: use functions instead of macros for the command interfaces.
;;;; TODO: make a nice reader macro to convert any args in a list from
;;;;       symbol to string,
;;;; eg #s(emerge --search lisp) -> (emerge "--search" "lisp")
;;;; TODO: work out this packaging stuff so I can play nicely with
;;;; others.
;;;; TODO: use a portable run-program or try write one.

(defpackage #:clalish (:use #:ext))
;(in-package #:clalish)

;;; Possible methods to retrieve pipes after piping them:

;;; (piped command command command)

;;; and/or

;;; Use pipe-chains - The output pipe of the last command
;;; is consed to a list of the connecting pipes to the next
;;; command, which is consed to a list of the connecting
;;; pipes to the next command, etc.

;;; A new form, with-open-piping, wraps with-open-pipe
;;; and also goes on the close the intra piping afterwards.



; If we can't get proper piping, this may suffice.
;(defmacro compile-cmd (form)
;  (let ((cn (cmd-name form))
;        (ca (cmd-args form))
;        (ci (cmd-in form))
;        (co (cmd-out form)))
;    
;    (implode " "
;      (cons
;        cmd-name
;        (mapcar #'shell-arg-string cmd-args)))
;    ))
;(cmd (grep "usb" :in (dmesg)))
;(run-shell-command "dmesg | grep \"usb\"")



(defmacro w/var (name val &body forms)
  `(let ((,name ,val))
     ,@forms))

(defmacro w/gens (syms &rest forms)
  `(let ,(mapcar (lambda (sym) (list sym '(gensym))) syms)
    ,@forms))

(defmacro defun-cond (name args &rest forms)
  `(defun ,name ,args
     (cond ,@forms)))

;;; Lambda

(defun partial (fn &rest early)
  (lambda (&rest late)
    (apply fn (append early late))))

(defun partialr (fn &rest early)
  (lambda (&rest late)
    (apply fn (append late early))))

(defun build-composed (fns args)
  (if (null (cdr fns))
    `(apply #',(car fns) ,args)
    (list (car fns) (build-composed (cdr fns) args))))

;;; Misc

(defun >0 (x)
  (> x 0))

(defun-cond to-symbol (x)
  ((stringp x) (make-symbol x))
  ('t x))

(defun-cond flatten (input &optional accumulator)
  ((null input) accumulator)
  ((atom input) (cons input accumulator))
  ('t (flatten (first input) (flatten (rest input) accumulator))))

;;; Strings

(defparameter *line-feed* (format nil "~%"))

(defun re-format (&rest args)
  (apply #'format (cons nil args)))

(defun concat (&rest args)
  (reduce (lambda (x acc) (concatenate 'string x acc)) args :initial-value ""))

(defun string-if (test then)
  (if test then ""))

(defun string-if-not (test then)
  (string-if (not test) then))

(defun remove-trailing-feed (str)
  (subseq str 0 (1- (length str))))

(defun ensure-ending (base end)
  (let ((sl (length base)))
    (if (>0 sl)
      (if (not (equal (subseq base (- sl (length end)) sl) end))
        (concat base end)
        base)
      end)))

(defun string-repeat (str times)
  (if (zerop times) ""
    (concat str (string-repeat str (1- times)))))

(defun string-char-split (string char)
  (loop for i = 0 then (1+ j)
    as j = (position char string :start i)
    collect (subseq string i j)
    while j))

(defun replace-all (string part replacement &key (test #'char=))
  (with-output-to-string (out)
    (loop with part-length = (length part)
      for old-pos = 0 then (+ pos part-length)
      for pos = (search part string
                  :start2 old-pos
                  :test test)
      do (write-string string out
           :start old-pos
           :end (or pos (length string)))
      when pos do (write-string replacement out)
      while pos)))

;; list has its contents to-string'ed.
;; symbols go to lowercase string.
(defun-cond to-string (x)
  ((eq x nil) "")
  ((listp x) (mapcar #'to-string x))
  ((symbolp x) (symbol-name x))
  ('t (format nil "~a" x)))

;;; Generating shell command strings

;; list has its contents shell-escape'd
;; first add a backslash for every existing backslash.
;; add a backslash for every existing double quote.
(defun-cond shell-escape (x)
  ((listp x) (mapcar #'shell-escape x))
  ((stringp x) (replace-all (replace-all x "\\" "\\\\") "\"" "\\\""))
  ('t x))

(defun shell-argument-string (x)
  (shell-escape (to-string x)))

(defun shell-args (x)
  (mapcar #'shell-argument-string x))

;; Deprecated - now using run-program.
(defun command-string (name args sudo-p)
  (concat
    (string-if sudo-p "sudo ")
    (to-string name)
    (string-if-not (null args) " ")
    (re-format "~{\"~a\"~^ ~}" (flatten (shell-args args)))))

;;; Working with pipes and lines

(defun to-lines (string)
  (string-char-split string #\Newline))

(defun for-lines-open (fun it)
  (w/var line (read-line it nil nil)
    (if (eq line nil)
      nil
      (progn
        (funcall fun line)
        (for-lines-open fun it)))))

(defun for-lines-and-close (fun stream)
  (with-open-stream (it stream)
    (for-lines-open fun it)))

(defun read-string-open (stream &optional (start ""))
  (w/var line (read-line stream nil nil)
    (if (eq line nil)
      (string-if (>0 (length start))
        (remove-trailing-feed start))
      (read-string-open stream (concat start line *line-feed*)))))

(defun read-string-and-close (stream)
  (with-open-stream (it stream)
    (read-string-open it)))

;;; Pipe chains

(defun close-pipe-chain (p)
  (if (null p)
    'done
    (progn
      (mapcar #'close (first p))
      (close-pipe-chain (rest p)))))

(defmacro with-open-pipe-chain ((it p) &rest forms)
  (w/gens (the-p)
    `(post
       (let ((,the-p ,p))
         (with-open-stream (,it ,(first the-p))
           ,@forms)
         (close-pipe-chain ,(rest the-p))))))

;;; Piped - another way of expressing piping

(defmacro piped (&rest forms)
  4)

;; We want the output of foo to pipe into baz
;; and the result pipe is returned.
;; (piped (sh foo bar) (sh baz wallop))

;;; Shell interface

(defparameter *sudo-p* nil)

(defmacro just (&body forms)
  `(w/var *sudo-p* t
    (progn ,@forms)))

(defmacro sys (name &rest args)
  `(sh-term-std ',name ,@(mapcar (lambda (x) `(quote ,x)) args)))

;; If you pass a pipe as the input argument,
;; this will close it for you.
;; In future I might change this behaviour if I can
;; get real piping to happen behind the scenes instead.
;;
;; If this function outputs a pipe, and you must always close it when
;; you're done with it.
;;
;; I'd like to extend this so the notion of a prefix command like "sudo"
;; is abstracted, but right now I'm not that bothered.
;;
(defun sh (name &key (from :terminal inp) (to :terminal) (args nil) (wait t))
  (let* ((input-type (if (or (streamp from) (stringp from)) :stream from))
         (out-stream? (eq to :stream))
         (pipes-or-code
           (multiple-value-list
             (ext:run-program (if *sudo-p* "sudo" (shell-argument-string name))
               :arguments
               (shell-args (if *sudo-p* (cons name args) args))
               :input input-type
               :output (if out-stream? :stream to)
               :wait wait))))
    (if (eq input-type :stream)
      (progn
        (let ((proc-input-taker (funcall (if out-stream? #'third #'first)
                                  pipes-or-code)))
          (with-open-stream (proc-stdin proc-input-taker)
            (if (streamp from)
              (for-lines-and-close
                (lambda (ln)
                  (format proc-stdin "~a~%" ln))
                from)
              (progn
                (write-string from proc-stdin)
                (finish-output proc-stdin)))))
        ;; If we are using streams in and out, close the
        ;; bidirectional one.
        (if out-stream?
          (progn
            (close (first pipes-or-code))
            (second pipes-or-code))
          nil))
      ;; If input-type is not stream, then pipes-or-code
      ;; can only be a list of one value - a pipe or a status.
      (first pipes-or-code))))

(defun sh-pipe (name from &rest args)
  (sh name :from from :to :stream :args args))

(defun sh-string (name from &rest args)
  (read-string-and-close (sh name :from from :to :stream :args args)))

(defun sh-term (name from &rest args)
  (sh name :from from :args args))

(defun sh-pipe-std (name &rest args)
  (apply #'sh-pipe name :terminal args))

(defun sh-string-std (name &rest args)
  (apply #'sh-string name :terminal args))

(defun sh-term-std (name &rest args)
  (apply #'sh-term name :terminal args))

;;; defsh

(defun argument-name-p (symb)
  (not (equalp (subseq (symbol-name symb) 0 1) "&")))

(defmacro with-args-to-string (args &rest forms)
  `(let ,(mapcar
           (lambda (arg) `(,arg (to-string ,arg)))
           (remove-if-not #'argument-name-p args))
     ,@forms))

;; An interface which converts it arguments to strings,
;; so if you type (my-sh-foo 4 'bar) it sees
;; (my-sh-foo "4" "bar")
(defmacro defsh (name args &rest forms)
  `(defun ,name ,args
     (with-args-to-string ,args
       ,@forms)))

(defmacro lamsh (args &rest forms)
  `(lambda ,args
     (with-args-to-string ,args
       ,@forms)))

(defmacro defsh-partial (name fun &rest pre)
  `(defsh ,name (&rest args)
     (apply #',fun ,@pre args)))

(defmacro defsh-compose (name &rest fns)
  (w/gens (args)
    `(defsh ,name (&rest ,args)
       ,(build-composed fns args))))

(defmacro defsh-sequence (name &rest cmds)
  (w/gens (args)
    `(defsh ,name (&rest ,args)
       ,@(mapcar (lambda (cmd) `(apply #',cmd ,args)) cmds))))

;;; Automatically creating functions for commands
;;; - This uses eval to define the macros
;;; which is more or less an ugly hack,
;;; but as far as I know the only way of doing it.
;;;
;;; But even still, I need eval to dynamically
;;; make the functions / or macros.

(defmacro defshell (name &optional dir)
  `(defsh ,name (&rest args)
     (apply
       #'sh-term-std
       ,(concat
          (string-if dir (ensure-ending (to-string dir) "/"))
          (shell-argument-string name))
       args)))

(defun bin-aliased (name dir)
  (if (not (find-symbol name))
    (eval `(defshell ,(intern name) ,dir))))

(defun alias-directory-bins (dir)
  (for-lines-and-close
    (partialr #'bin-aliased dir)
    (sh-pipe-std :ls dir)))

;;; Extras

(defun dirall ()
  (directory #p"*"))

(defun file-part (x)
  (pathname-file-name (pathname x)))

(defsh hidden-file-p (x)
  (equal (subseq (file-part x) 0 1) "."))

(defun to-filename-list (x)
  x)

(defmacro w/files (name file-list &rest forms)
  `(dolist (,name ,(to-filename-list file-list))
     ,@forms))

(defun setenv (name val)
  (setf (getenv name) val))

;;; Autoload the initfile

(let ((initfile (concat (ensure-ending (getenv "HOME") "/") ".clalishrc")))
  (when (probe-file initfile)
    (load initfile)))

