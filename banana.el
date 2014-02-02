;;; banana.el --- Monads for elisp. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; Created: 1st February 2014
;; Package-requires: ((dash "2.5.0"))

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
(require 'dash)

;;;;; Functor class
(defvar functor-dispatch-table-fmap (make-hash-table))

(defmacro instance-functor (name where &rest body)
  (declare (indent 2))
  `(progn
     (puthash ',name (lambda ,@(cdr (assoc 'fmap body))) functor-dispatch-table-fmap)))

(defun functor-bind (function thing)
  (let* ((type (if (listp thing) 'List (elt thing 0)))
         (fmap (gethash
                type
                functor-dispatch-table-fmap
                (lambda (_ _) (error "Functor instance for %s not defined" type)))))
    (funcall fmap function thing)))

;;;;; Monad class
(defvar monad-dispatch-table-bind (make-hash-table))
(defvar monad-dispatch-table-then (make-hash-table))
(defvar monad-dispatch-table-return (make-hash-table))
(defvar monad-type nil) ;; so much hack it hurts

(defmacro instance-monad (name where &rest body)
  (declare (indent 2))
  `(progn
     (puthash ',name (lambda ,@(cdr (assoc 'return body))) monad-dispatch-table-return)
     (puthash ',name (lambda ,@(cdr (assoc 'bind body))) monad-dispatch-table-bind)
     ,(when (assoc 'then body)
        `(puthash ',name (lambda ,@(cdr (assoc 'then body))) monad-dispatch-table-then))
     nil))

(defun monad-bind (thing function)
  (let* ((monad-type (if (listp thing) 'List (elt thing 0)))
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
  (let* ((monad-type (if (listp thing) 'List (elt thing 0)))
         (then (gethash
                monad-type
                monad-dispatch-table-then
                (lambda (_ _) (monad-bind thing (lambda (_) another))))))
    (funcall then thing another)))
(defun monad-return (thing)
  (when (not monad-type)
    (error "Failed to infer the monad return type. I was probably called outside of bind context."))
  (let* ((ret (gethash
               monad-type
               monad-dispatch-table-return
               (lambda (_) (error "Monad instance for %s not defined" monad-type)))))
    (funcall ret thing)))

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
  "Compose two monadic actions into one left-to-right."
  (lambda (x) (monad-bind (funcall f x) g)))

(defun monad-compose-left (g f)
  "Compose two monadic actions into one right-to-left.

This is like `monad-compose-right' with arguments flipped."
  (lambda (x) (monad-bind (funcall f x) g)))

(defalias '>=> 'monad-compose-right)
(defalias '<=< 'monad-compose-left)

;; this is so crap it hurts.
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

;;;;; Maybe data type & instances
;; so far types are just rough convention [Type Constructor data]
;; lists are special-cased so you can simply use (bla bla bla)

;; a -> m a
(defun just (thing)
  (vector 'Maybe 'Just thing))

;; m a
(defun nothing ()
  (vector 'Maybe 'Nothing))

;; m a -> a
(defun maybe-from-just (thing)
  (elt thing 2))

;; m a -> bool
(defun maybe-is-nothing-p (thing)
  (equal thing [Maybe Nothing]))

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

;;;;; List data type
(instance-monad List where
  (return (x) (list x))
  (bind (x f) (-mapcat f x)))

(instance-functor List where
  (fmap (x f) (mapcar f x)))

(provide 'banana)

;;; banana.el ends here
