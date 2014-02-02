# banana.el

Monads are like burittos, or koalas... I've always seen them as bananas. Do you like bananas? I hope you do.

# Examples

```scheme
;; how to handle division by zero in a chain of operations
(-> (just 1)
  (monad-bind (lambda (x)
                (if (/= x 0)
                    (just (/ 4 x))
                  (nothing))))
  (monad-bind (lambda (x) (just (1+ x)))))

(monad-do p <- '(1 2 3) q <- '(1 2 3) (monad-return (cons p q)))
;; which expands to
(monad-bind
 '(1 2 3)
 (lambda (p)
   (monad-bind
    '(1 2 3)
    (lambda (q)
      (monad-return (cons p q))))))
;; which is equivalent to
(monad-lift2 'cons '(1 2 3) '(1 2 3))

;; example of operation on undefined input
(monad-do x <- (just 1) y <- (nothing) (monad-return (+ x y)))

;; monad-bind is also aliased to >>=
(>>= '(1 2 3) (lambda (p) (>>= '(1 2 3) (lambda (q) (monad-return (cons p q))))))
(>>= (just 1) (lambda (p) (>>= (just 2) (lambda (q) (monad-return (+ p q))))))

;; better error handling with Either monad, where we can provide error message.
(monad-join
 (monad-lift2
  (lambda (x y)
    (if (= y 0)
        (either-left "Division by zero")
      (either-right (/ x y))))
  (either-right 4)
  (either-right 0)))
;; in do notation
(monad-do x <- (either-right 4)
          y <- (either-right 0)
          (if (= y 0)
              (either-left "Division by zero")
            (either-right (/ x y))))
```
