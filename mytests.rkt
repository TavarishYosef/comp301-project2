#lang eopl
(require eopl/tests/private/utils)

(require "data-structures.rkt")  ; for expval constructors
(require "lang.rkt")             ; for scan & parse
(require "interp.rkt")  

(zero?-exp (number->expval 0))