;;; banana.el --- Monads for elisp. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; Created: 1st February 2014
;; Package-requires: ((dash "2.5.0") (dash-functional "1.0.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bananas for elisp... I mean Monads.

;; It's very much a work in progress, please heed that warning.

;;; Code:
(require 'help-fns)
(require 'dash)
(require 'dash-functional)

;; add font-locking for instance declarations
(font-lock-add-keywords
 'emacs-lisp-mode `((,(concat "("
                              (regexp-opt '("instance-functor"
                                            "instance-monad") t)
                              "\\>"
                              " +\\(\\(?:\\s_\\|\\sw\\)+\\)"
                              " +\\(where\\)")
                     (1 font-lock-keyword-face)
                     (2 font-lock-constant-face)
                     (3 font-lock-keyword-face))))

(defun banana-arity (function)
  "Return minimum and maximum number of args allowed for FUNCTION.

The returned value is a pair (MIN . MAX).  MIN is the minimum
number of args.  MAX is the maximum number or the symbol `many',
for functions with `&rest' args.

Keyword arguments are *not* supported and the return value is
undefined for such functions."
  (if (subrp function)
      (subr-arity function)
    (let* ((arglist (help-function-arglist function))
           (required (--take-while (not (or (eq it '&optional)
                                            (eq it '&rest))) arglist))
           (optional (--take-while (not (eq it '&rest)) (cdr (memq '&optional arglist))))
           (rest (cdr (memq '&rest arglist))))
      (unless (eq t arglist)
        (cons (length required)
              (cond (rest 'many)
                    (optional (+ (length required) (length optional)))
                    (t (length required))))))))

(defun banana-curry (function)
  "Curry FUNCTION"
  (let* ((arity (banana-arity function))
         (min-arity (car arity))
         (max-arity (cdr arity))
         (arglist (help-function-arglist function))
         (required (--take-while (not (or (eq it '&optional)
                                          (eq it '&rest))) arglist)))
    ;; special-case (0 . many) to binary functions. --- unsafe!!!!!
    (when (and (= min-arity 0)
               (eq max-arity 'many))
      (setq min-arity 2)
      (setq required '(x y)))
    (-concat '(closure (t)) (cdr (--reduce-r-from `(lambda (,it) ,acc) `(,function ,@required) required)))))

(defun $ (fun &rest args)
  "Call FUN with arguments ARGS.

This is similar to `funcall' except it automatically curries FUN
and any functions in the argument list.

See `banana-curry'."
  (let ((r (banana-curry fun)))
    (--each args
      (setq r (funcall r (cond
                          ((functionp it)
                           (banana-curry it))
                          (t it)))))
    r))

(defun banana-get-type (thing)
  "Get the type of banana THING."
  (if (listp thing) 'List (elt thing 0)))


;;;;; Functor class
(defvar functor-dispatch-table-fmap (make-hash-table)
  "Hashtable that dispatches fmap based on functor type.")
(defvar functor-dispatch-table-const (make-hash-table)
  "Hashtable that dispatches const based on functor type.")

(defmacro instance-functor (name where &rest body)
  "Define instance of Functor class for type NAME.

You have to define one operation `fmap'.  Optionally, `const' can
be defined if default implementation (fmap . const) is found
inefficient.

Example:
  (instance-functor List where
    (fmap (f x) (mapcar f x)))"
  (declare (indent 2))
  `(progn
     (puthash ',name (lambda ,@(cdr (assoc 'fmap body))) functor-dispatch-table-fmap)
     ,(when (assoc 'const body)
        `(puthash ',name (lambda ,@(cdr (assoc 'const body))) functor-dispatch-table-const))))

(defun functor-fmap (function thing)
  "Map FUNCTION over THING.

Type: (a -> b) -> f a -> f b"
  (let* ((type (banana-get-type thing))
         (fmap (gethash
                type
                functor-dispatch-table-fmap
                (lambda (_ _) (error "Functor instance for %s not defined" type)))))
    (funcall fmap function thing)))

(defalias '<$> 'functor-fmap)

(defun functor-const (value thing)
  "Replace all locations in THING with the same VALUE.

Type: a -> f b -> f a"
  (let* ((type (banana-get-type thing))
         (const (gethash
                 type
                 functor-dispatch-table-const
                 (lambda (c l) (functor-fmap (lambda (_) c)) l))))
    (funcall const value thing)))

(defalias '<$ 'functor-const)


;;;;; Monad class
(defvar monad-dispatch-table-bind (make-hash-table)
  "Hashtable that dispatches the bind action based on monad type.")
(defvar monad-dispatch-table-then (make-hash-table)
  "Hashtable that dispatches the then action based on monad type.")
(defvar monad-dispatch-table-return (make-hash-table)
  "Hashtable that dispatches the return action based on monad type.")
(defvar monad-type nil ;; so much hack it hurts
  "Holds the current type of monad while execution is in
`monad-bind' so that `monad-return' can correctly re-wrap the
values.  It is defined as `defvaf' to preserve dynamic scoping.")

(defmacro instance-monad (name where &rest body)
  "Define instance of Monad class for type NAME.

You have to define two operations, `bind' and `return'.
Optionally, `then' can be defined if default
implementation (monad-bind x (lambda (_) k)) is found
inefficient.  The definitions can be in any order.

Example:
  (instance-monad List where
    (return (x) (list x))
    (bind (x f) (-mapcat f x)))"
  (declare (indent 2))
  `(progn
     (puthash ',name (lambda ,@(cdr (assoc 'return body))) monad-dispatch-table-return)
     (puthash ',name (lambda ,@(cdr (assoc 'bind body))) monad-dispatch-table-bind)
     ,(when (assoc 'then body)
        `(puthash ',name (lambda ,@(cdr (assoc 'then body))) monad-dispatch-table-then))
     nil))

(defun monad-bind (thing function)
  "Sequentially compose two actions, passing any value produced
by the first as an argument to the second.

This means, \"unbox\" the value of THING and pass it to FUNCTION.

Type: m a -> (a -> m b) -> m b

Example: (monad-bind '(1 2 3) (lambda (x) (list (1+ x) (1- x))))"
  (let* ((monad-type (banana-get-type thing))
         (bind (gethash
                monad-type
                monad-dispatch-table-bind
                (lambda (_ _) (error "Monad instance for %s not defined" monad-type)))))
    (funcall bind thing function)))

(defalias '>>= 'monad-bind)

(defun monad-then (thing another)
  "Sequentially compose two actions, discarding any value
produced by the first, like sequencing operators (such as the
semicolon) in imperative languages.

Type: m a -> m b -> m b

Example: (monad-then '(1 2) '(3 4)) => '(3 4 3 4)"
  (let* ((monad-type (banana-get-type thing))
         (then (gethash
                monad-type
                monad-dispatch-table-then
                (lambda (_ _) (monad-bind thing (lambda (_) another))))))
    (funcall then thing another)))

(defalias '>> 'monad-then)

(defun monad-return (thing &optional monad-type)
  "Inject a value THING into the monadic type.

Type: a -> m a"
  (when (not monad-type)
    (error "Failed to infer the monad return type. I was probably called outside of bind context."))
  (let* ((ret (gethash
               monad-type
               monad-dispatch-table-return
               (lambda (_) (error "Monad instance for %s not defined" monad-type)))))
    (funcall ret thing)))

(defun monad-join (m)
  "The `join' function is the conventional monad join operator.
It is used to remove one level of monadic structure, projecting
its bound argument into the outer level.

Type: m (m a) -> m a"
  (monad-bind m 'identity))

(defun monad-lift (f a)
  "Promote a function to a monad.

Type: (a -> b) -> m a -> m b

Example: (monad-lift '1+ (just 2)) => [Maybe Just 3]"
  (monad-bind a (lambda (x) (monad-return (funcall f x)))))

(defun monad-lift2 (f a b)
  "Promote a binary function to a monad.

Type: (a -> b -> r) -> m a -> m b -> m r

Example: (monad-lift2 '+ (just 1) (just 2)) => [Maybe Just 3]"
  (monad-bind a (lambda (x) (monad-bind b (lambda (y) (monad-return (funcall f x y)))))))

(defun monad-compose-right (f g)
  "Compose two monadic actions into one left-to-right.

Type: (a -> m b) -> (b -> m c) -> a -> m c"
  (lambda (x) (monad-bind (funcall f x) g)))

(defun monad-compose-left (g f)
  "Compose two monadic actions into one right-to-left.

This is like `monad-compose-right' with arguments flipped.

Type: (b -> m c) -> (a -> m b) -> a -> m c"
  (lambda (x) (monad-bind (funcall f x) g)))

(defalias '>=> 'monad-compose-right)
(defalias '<=< 'monad-compose-left)

;; this is so crap it hurts. Not to be used yet.
(defmacro monad-do (&rest things)
  (cond
   ((eq (cadr things) '<-)
    `(monad-bind
      ,(caddr things)
      (lambda (,(car things))
        (monad-do ,@(cdddr things)))))
   ((car things)
    (if (cdr things)
        `(progn
           (car things)
           (monad-do ,@(cdr things)))
      (car things)))))


;;;;; data types
(defun banana-format-type (type &optional wrap)
  "Create pretty-formated type declaration from data specification"
  (cond
   ((symbolp type)
    (symbol-name type))
   ((listp type)
    (cond
     ((= (length type) 1)
      (concat "[" (banana-format-type (car type) t) "]"))
     (t
      (let ((fmtd (concat (symbol-name (car type))
                          " "
                          (mapconcat (-cut banana-format-type <> t) (cdr type) " "))))
        (if wrap (concat "(" fmtd ")") fmtd)))))
   ((vectorp type)
    (concat "[" (banana-format-type (elt type 0) t) "]"))))

(defun banana-get-args (args)
  "Generate argument list for type constructor from data specification."
  (--map (cl-gensym "B") args))

;; TODO: add extractors too!
(defmacro banana-data (name &rest stuff)
  "Define an algebraic data type.

It creates all the constructors with names of form:
  (lowercase class)-(lowercase constructor)

For each constructor there is a test method to see if instance is
of that type with name:
  (lowercase class)-is-(lowercase constructor)-p

Syntax is like Haskell."
  (let* ((parts (--split-with (not (eq '= it)) stuff))
         (types (car parts))
         (all-defs (cdadr parts))
         (definitions (--remove (equal it '(|)) (--partition-by (eq '| it) all-defs)))
         (type-name (symbol-name name))
         (constructors
          (-mapcat
           (lambda (def)
             (let* ((constructor (car def))
                    (args (cdr def))
                    (constructor-args (banana-get-args args))
                    (constructor-name (symbol-name constructor))
                    (name-function (intern (concat (downcase type-name) "-" (downcase constructor-name))))
                    (name-predicate (intern (concat (downcase type-name) "-is-"
                                                    (downcase constructor-name) "-p"))))
               (list
                `(defun ,name-function ,constructor-args
                   ,(format "Create instance of %s with constructor %s\n\nType: %s"
                            type-name constructor-name
                            (concat
                             (let ((args-formated (mapconcat 'banana-format-type args " -> ")))
                               (if (equal "" args-formated) ""
                                 (concat args-formated " -> ")))
                             (concat type-name " " (mapconcat 'banana-format-type types " "))))
                   (vector ',name ',constructor ,@constructor-args))
                `(defun ,name-predicate (thing)
                   ,(format "Return non-nil if THING is instance %s of type %s\n\nType: %s -> Bool"
                            constructor-name type-name
                            (concat type-name " " (mapconcat 'banana-format-type types " ")))
                   (and (eq (elt thing 0) ',name)
                        (eq (elt thing 1) ',constructor))))))
           definitions)))
    (cons 'progn constructors)))


;;;;; Maybe data type & instances
;; so far types are just rough convention [Type Constructor data]
;; lists are special-cased so you can simply use (bla bla bla)

(banana-data Maybe a = Nothing | Just a)

(defalias 'just 'maybe-just)
(defalias 'nothing 'maybe-nothing)

(defun maybe-from-just (thing)
  "Extract value from Just THING.

Throw an error if THING is Nothing.

Type: Maybe a -> a"
  (if (maybe-is-nothing-p thing)
      (error "There is Nothing in there!")
    (elt thing 2)))

(instance-monad Maybe where
  ;; a -> m a
  (return (x) (just x))
  ;; m a -> (a -> m b) -> m b
  (bind (x f) (if (maybe-is-nothing-p x)
                  (nothing)
                (funcall f (maybe-from-just x)))))

(instance-functor Maybe where
  ;; (a -> b) -> m a -> m b
  (fmap (f x) (if (maybe-is-nothing-p x)
                  (nothing)
                (just (funcall f (maybe-from-just x))))))


;;;;; Either data type
(banana-data Either a b = Left a | Right b)

(defun either-from-left (thing)
  "Extract the value of Left THING.

Type: Either a b -> a"
  (if (not (either-is-left-p thing))
      (error "This is not a Left thing.")
    (elt thing 2)))

(defun either-from-right (thing)
  "Extract the value of Right THING.

Type: Either a b -> b"
  (if (not (either-is-right-p thing))
      (error "This is not a Right thing.")
    (elt thing 2)))

(instance-functor Either where
  (fmap (f x) (if (either-is-left-p x) x
                (either-right (funcall f (either-from-right x))))))

(instance-monad Either where
  (return (x) (either-right x))
  (bind (x f) (if (either-is-left-p x) x
                (funcall f (either-from-right x)))))


;;;;; List data type
(instance-monad List where
  (return (x) (list x))
  (bind (x f) (-mapcat f x)))

(instance-functor List where
  (fmap (f x) (mapcar f x)))

(provide 'banana)

;;; banana.el ends here
