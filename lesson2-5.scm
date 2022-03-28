#lang racket

;;;
;;; 1.首先要做的是，搞清楚要解释的是什么
;;; base on abstraction
;;; 1.解释基本类型，数字，字符串等
;;; 2.解释变量，也就是symbol，比如'x，不再是pair类型的variable
;;; 3.解释函数，(lambda (x) (* x x))，不再是pair类型的function
;;; 4.解释调用，((lambda (x) (* x x)) 3)，不再是pair类型的call
;;; 5.解释二元运算，(+ 1 2)，不再是pair类型的binop
;;; 6.解释if，(if (predicate) ... ,不再是pair类型的if
;;;

(require data/queue)

(define q (make-queue))
;;(enqueue! q 1)
;;(dequeue! q)
(queue-empty? q)

(define (addTask task)
  (enqueue! q task))

(define (run)
  (cond
    [(queue-empty? q) (println "task run finished!")]
    [else
     (let ([task (dequeue! q)])
       (task))
     (run)]))


;;定义闭包
(struct Closure (f env))

  
;;环境   
;;定义查找结构
(define emptyTable '())
;(println emptyTable)

(define (addTable key value oldTable)
  (cons (cons key value) oldTable))

(define (lookupTable key table)
  (let ([p (assq key table)])
    (cond
      [(not p) #f]
      [else (cdr p)])))

(define menu1
  (addTable 'sa 45
            (addTable 'beer 35 emptyTable)))
(lookupTable 'sa menu1)
(lookupTable 'sa111 menu1)

(define emptyEnv emptyTable)
(define extEnv addTable)
(define lookupEnv lookupTable)

(quote a);;并不是一个pair，也不是一般的过程，生成的是一个symbol


(define atom? (or/c number? symbol? boolean? string?))

(define baseType? (or/c number? boolean? string?))

(define (binop? exp)
  (match exp
    [`(+ ,e1 ,e2) #t]
    [`(- ,e1 ,e2) #t]
    [`(* ,e1 ,e2) #t]
    [`(/ ,e1 ,e2) #t]
    [else #f]))


(define trans-binop (lambda (op)
                       (cond
                         ((equal? op `+) +)
                         ((equal? op `-) -)
                         ((equal? op `*) *)
                         ((equal? op `/) /)
                         ((equal? op `<) <)
                         ((equal? op `<=) <=)
                         ((equal? op `>=) >=)
                         ((equal? op `>) >))))
 
(baseType? 3)

(define (interp-in exp env)
    (match exp
      [(? baseType? exp) exp]
      [(? symbol? exp) (let ([val (lookupEnv exp env)])
                         (cond 
                           [(not val) (error "no valid variable: " exp)]
                           [else val]))]
      [`(lambda (,param) ,body) (Closure `(lambda (,param) ,body) env)]
      [`(,op ,e1 ,e2) ((trans-binop op) (interp-in e1 env) (interp-in e2 env))]
      [`(,op ,arg)
       (let ([opVal (interp-in op env)]
             [argVal (interp-in arg env)])
         (match opVal
           [(Closure `(lambda (,param) ,body) env)
            (interp-in body (extEnv param argVal env))]
           [else (error "invoke not procedure: " exp)]))]))
(define (interp exp)
  (interp-in exp emptyEnv))

(interp '((lambda (x) x) 3))
(interp '((lambda (x) (* x x)) 3))
(interp '((lambda (x) (* x x)) (+ 1 4)))
(interp '(((lambda (f) (lambda (x) (f x))) (lambda(y) (* y y))) 6)) 

;(interp-in 42 '())
;(interp-in "good" '())
;(interp-in 'a '((a . 3)))
;;(interpIn 'a '((b . 3)))
;
;(interp-in '(lambda (x) x) '((x . 3)))



(define (interp-in-cps exp env k)
    (match exp
      [(? baseType? exp) (k exp)]
      [(? symbol? exp) (let ([val (lookupEnv exp env)])
                         (cond 
                           [(not val) (error "no valid variable: " exp)]
                           [else (k val)]))]
      [`(lambda (,param) ,body) (k (Closure `(lambda (,param) ,body) env))]
      [`(,op ,e1 ,e2) (interp-in-cps e1 env (lambda (e1v)
                                              (interp-in-cps e2 env (lambda(e2v)
                                                                      (k ((trans-binop op) e1v e2v))))))]
      [`(,op ,arg)
       (interp-in-cps op env (lambda (opv)
                               (interp-in-cps arg env (lambda (argv)
                                                        (match opv
                                                          [(Closure `(lambda (,param) ,body) env)
                                                           (interp-in-cps body (extEnv param argv env) k)]
                                                          [else (error "invoke not procedure: " exp)])))))]))
(define (interp-cps exp k)
  (interp-in-cps exp emptyEnv k))

(println '========)

(interp-cps '((lambda (x) x) 3) println)
(interp-cps '((lambda (x) (* x x)) 3) println)
(interp-cps '((lambda (x) (* x x)) (+ 1 4)) println)
(interp-cps '(((lambda (f) (lambda (x) (f x))) (lambda(y) (* y y))) 6) println) 
    