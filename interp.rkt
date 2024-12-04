#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))
      
      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond 
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2))
                              ((= op 2) (* num1 num2))
                                    ;; -----------------------
                                    ;; INSERT YOUR CODE HERE 
                                    ;; -----------------------
                              ((= op 3) (/ num1 num2))
                              (else (- num1 num2))
                                    ;; -----------------------
                              )))
                        
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot))
                              ((= op 2) (cons (* num1 num2top) num2bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons (* num1 num2bot) num2top))
                              (else (cons (- (* num1 num2bot) num2top) num2bot))
                              ;; -----------------------

                              
                              ))))

                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot))
                              ((= op 2) (cons (* num1top num2) num1bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons num1top (* num2 num1bot)))
                              (else (cons (- num1top (* num2 num1bot)) num1bot))
                              ;; -----------------------
                              ))))

                        (else
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) ;; add
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) ;; multiply
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons (* num1top num2bot) (* num1bot num2top))) 
                              (else (cons (- (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot)))
                              ;; ----------------------- 
                            ))))))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (if (number? num1)
                        (if (zero? num1)
                          (bool-val #t)
                          (bool-val #f))
                          ;; -----------------------
                          ;; INSERT YOUR CODE HERE 
                          ;; -----------------------
                        (if (zero? (car num1))
                            (bool-val #t)
                            (bool-val #f))
                          ;; ----------------------- 
                        ))))

      

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;; -----------------------
      ;; INSERT YOUR CODE HERE 
      ;; -----------------------
      (list-exp () (list-val '()))

      (cons-exp (exp1 lstexp)
                (let ((val (value-of exp1 env))
                      (lst (expval->list (value-of lstexp env))))
                  (list-val (cons (expval->num val) lst))))

      (mul-exp (lstexp)
               (let ((lst (expval->list (value-of lstexp env))))
                (num-val (if (null? lst)
                            0
                            (apply * lst)))))
      
      (min-exp (lstexp)
               (let ((lst (expval->list (value-of lstexp env))))
                (if (null? lst) 
                  (num-val -1)
                  (num-val (apply min lst)))))

      (if-elif-exp (exp1 exp2 exp3 exp4 exp5)
                   (let ([bool1 (expval->bool (value-of exp1 env))])
                     (if bool1
                         (value-of exp2 env)
                         (let ([bool3 (expval->bool (value-of exp3 env))])
                           (if bool3
                               (value-of exp4 env)
                               (value-of exp5 env))))))
      
      (rational-exp (num1 num2)
                    (if (zero? num2)
                        (eopl:error "rational-exp: denominator cannot be zero")
                        (rational-val (cons num1 num2))))

      (simpl-exp (exp1)
                (let ((num (expval->rational (value-of exp1 env))))
                  (if (number? num) (num-val num)
                      (let* ((numer (car num)) 
                            (denom (cdr num))
                            (gcd-val ((if (zero? denom) numer 
                                      (gcd denom (remainder numer denom)))))
                           )
                      (rational-val (cons (/ numer gcd-val) (/ denom gcd-val)))  
                      )
                  )
                )
      )
      

      ;; -----------------------

      )))