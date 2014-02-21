;;; parselib.el --- Monadic parsing for elisp -*- lexical-binding: t -*-

;; Copyright (C) 2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; Created: 21st February 2014
;; Package-requires: ((dash "2.5.0") (dash-functional "1.0.0") (banana "0.0.1"))

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

;; Monadic parsing for elisp

;; It's very much a work in progress, please heed that warning.

;;; Code:
(require 'help-fns)
(require 'dash)
(require 'dash-functional)
(require 'banana)

(banana-data Pair a b = Pair a b)

(defalias 'pair 'pair-pair)

(defun pair-fst (thing)
  "Extract the first value of Pair.

Type: Pair a b -> a"
  (if (not (pair-is-pair-p thing))
      (error "This is not a Pair thing.")
    (elt thing 2)))

(defun pair-snd (thing)
  "Extract the second value of Pair.

Type: Pair a b -> b"
  (if (not (pair-is-pair-p thing))
      (error "This is not a Pair thing.")
    (elt thing 3)))

;; fix formatting of function type
(banana-data Parser a = P (String -> [(Pair a String)]))

(defun parser-from-p (thing)
  "Extract value from P THING.

Type: Parser a -> String -> [(a,String)]"
  (if (not (parser-is-p-p thing))
      (error "This is not a parser.")
    (elt thing 2)))

(defun parser-apply (parser inp)
  (funcall (parser-from-p parser) inp))

(instance-functor Parser where
  (fmap (f x) (parser-p
               (lambda (inp)
                 (-map
                  (lambda (it)
                    (pair (funcall f (pair-fst it))
                          (pair-snd it)))
                  (parser-apply x inp))))))

(instance-monad Parser where
  (return (x) (parser-p (lambda (inp) (list (pair x inp)))))
  (bind (x f) (parser-p (lambda (inp)
                          (-mapcat
                           (lambda (it)
                             (parser-apply (funcall f (pair-fst it))
                                           (pair-snd it)))
                           (parser-apply x inp))))))

;; write a helper macro for creating parsers
;; Parser Char
(defun parser-item ()
  (parser-p (lambda (inp)
              (cond
               ((not inp) nil)
               (t (list (pair (car inp) (cdr inp))))))))

;; (parser-apply (>> (>> (parser-item) (parser-item)) (parser-item)) '(a b c))
;; (parser-apply (parser-item) '(a b c))
;; (parser-apply (>> (>> (parser-item) (parser-item)) (parser-item)) '(a b))

;; Parser a -> Parser a
(defun parser-first (p)
  (parser-p (lambda (inp)
              (let ((re (parser-apply p inp)))
                (cond
                 ((not re) nil)
                 (t (list (car re))))))))

;; (Char -> Bool) -> Parser Char
(defun parser-sat (pred)
  (>>= (parser-item) (lambda (x)
                       (if (funcall pred x)
                           ;; inside the lambda we can't infer the
                           ;; type, so we need to help out :/
                           (monad-return x 'Parser)
                         ;; replace with mzero when MonadPlus is done
                         (monad-return nil 'Parser)))))

;; Char -> Parser Char
(defun parser-char (char)
  (parser-sat (lambda (x) (equal x char))))

;; (parser-apply (>> (parser-item) (parser-char 'b)) '(a b c))
;; (parser-apply (>> (parser-item) (parser-char 'b)) '(a c c))
;; (parser-apply (parser-char 'a) '(a b c))
;; (parser-apply (>> (>> (parser-item) (parser-item)) (parser-item)) '(a b))

(provide 'parselib)

;;; parselib.el ends here
