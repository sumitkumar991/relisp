# relisp

relisp is a Lisp Interpreter written in clojure. Writing an interpreter is good exercise to 

understand how programming languages work.

## Features

Relisp supports all the basic arithmetic operators & list modifier functions in lisp.

Supported special forms include:

### if, define, lambda, funcall, map, reduce

## How to run

enter command `lein run` in your project root, now enter the lisp expression to be evaluated & hit enter.

## Some Examples of expression

```
    (+ 1 4)
    
    (define x 10)
    
    (if (= x 10) true false)
    
    (reduce + (list 1 3 5))
    
    (def sum2 (lambda (a b) (+ a b)))
    (funcall sum2 4 5)
    
    (map 1+ (list 2 3 4))
    
```
