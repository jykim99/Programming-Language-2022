#lang racket
(provide (all-defined-out)) ;; exports the defined variables in this file.

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; Definitions for extra requirements should be here.
(struct glet (var e body) #:transparent) ;; a global binding that overrides any local binding (similar to the following ML code: let var = e in body)

(struct num-array  (size) #:transparent)  ;; a number array  (initialized to zeroes), e.g., (num-array-var 10)
                                                     ;; e.g. (num-array 4)

(struct num-array-at   (e1 e2) #:transparent) ;; e1 evaluates to num-array and e2 evaluates to racket int (index of the value to access) index starts from 0
                                              ;; (num-array-at (num-array 4) 3)
                                              ;; (num-array-at (num-array 4) 4) ;  this should give a nice error message (like "array access out of bound")
                                              ;; (num-array-at (num-array 4) -1) ;  this should give a nice error message (like "array access out of bound")

(struct num-array-set  (e1 e2 e3) #:transparent) ;; e1 evaluates to a num-array value, e2 evaluates to racket int (index of the value to access), and e3 evaluates to a MUPL int
                                              ;; (num-array-set (num-array 4) 0 (int 42))
                                              ;; (num-array-set (num-array 4) 5 (int 42)) ; this should give a nice error message (like "array access out of bound")
                                              ;; (num-array-set (num-array 4) -1 (int 42)) ; this should give a nice error message (like "array access out of bound")

(struct gb (e) #:transparent)

(define (num-array-object? v) ;; hackish implementation for num-array object testing. We assume that if a value is mpair, it is a num-array object.
  (mpair? v))

(define (array-length array)
  (if (eq? (mcdr array) null)
      1
      (+ 1 (array-length (mcdr array)))))

(define (make-array-object length)  
    (if (= length 0)
        null
        (mcons (int 0) (make-array-object (- length 1)))))

(define (set-array-val array index val)
  (if (= index 0)
      (set-mcar! array val)
      (set-array-val (mcdr array) (- index 1) val)))

(define (update-gb env acc v)
  (if (null? env)
      acc
      (if (gb? (car env))
          (let ([tmp (cdr (gb-e (car env)))])
            (if (closure? tmp)
                (let ([closure-e (closure-env tmp)])
                  (update-gb (cdr env) (append acc (list (cons (car (gb-e (car env))) (closure (append (list v) closure-e) (closure-fun tmp))))) v))
                (update-gb (cdr env) (append acc (list (gb-e (car env)))) v)))
            (let ([tmp (cdr (car env))])
              (if (closure? tmp)
                  (let ([closure-e (closure-env tmp)])
                    (update-gb (cdr env) (append acc (list (cons (car (car env)) (closure (append (list v) closure-e) (closure-fun tmp))))) v))
                  (update-gb (cdr env) (append acc (list (car env))) v)))
  )))

(define (get-nth array n)
  (if (= n 0)
      (mcar array)
      (get-nth (mcdr array) (- n 1))))

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem 1

;; CHANGE (put your solutions here)

(define (racketlist->mupllist li)
  (cond [(null? li) (aunit)]
        [(pair? li) (apair (car li) (racketlist->mupllist (cdr li)))]))

(define (mupllist->racketlist li)
  (cond [(aunit? li) '()]
        [(apair? li) (cons (apair-e1 li) (mupllist->racketlist (apair-e2 li)))]))


;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

(define (gbenvlookup env str originalenv)
  (cond [(null? env) (envlookup originalenv str)]
        [(gb? (car env)) (if (equal? (car (gb-e (car env))) str) (cdr (gb-e (car env))) (gbenvlookup (cdr env) str originalenv))]
        [#t (gbenvlookup (cdr env) str (append originalenv (list (car env))))]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)
         (gbenvlookup env (var-string e) '())]
        [(int? e)
         e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(fun? e)
         (closure env e)]
        [(ifgreater? e)
         (let ([e1 (eval-under-env (ifgreater-e1 e) env)]
               [e2 (eval-under-env (ifgreater-e2 e) env)]
               )
           (if (and (int? e1) (int? e2))
               (if (> (int-num e1) (int-num e2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")
           ))]
        [(mlet? e)
         (let ([ex (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (append (list (cons (mlet-var e) ex)) env)))]
        [(glet? e)
         (let ([ex (eval-under-env (glet-e e) env)])
           (eval-under-env (glet-body e) (append (list (gb (cons (glet-var e) ex))) (update-gb env '() (gb (cons (glet-var e) ex))))))]
        [(closure? e) e]
        [(call? e)
         (let ([e1 (eval-under-env (call-funexp e) env)])
               (if (closure? e1)
                   (let ([e2 (eval-under-env (call-actual e) env)])
                         (eval-under-env (fun-body (closure-fun e1))
                                         (append (closure-env e1)
                                                 (list (cons (fun-formal (closure-fun e1)) e2)
                                                       (if (fun-nameopt (closure-fun e1))
                                                           (cons (fun-nameopt (closure-fun e1)) e1)
                                                           (cons null null))
                                                       )
                                                 ))
                         )
                   (error "A function is not a closure")))]
        [(apair? e)
         (let (
               [e1 (eval-under-env (apair-e1 e) env)]
               [e2 (eval-under-env (apair-e2 e) env)])
               (apair e1 e2))]
        [(fst? e)
         (let ([p (eval-under-env (fst-e e) env)])
           (if (apair? p)
               (apair-e1 p)
               (error "fst: not a apair value")))]
        [(snd? e)
         (let ([p (eval-under-env (snd-e e) env)])
           (if (apair? p)
               (apair-e2 p)
               (error "snd: not a apair value")))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(num-array? e)
         (let ([size (num-array-size e)])
           (make-array-object size))]
        [(num-array-at? e)
         (letrec ([array (eval-under-env (num-array-at-e1 e) env)]
               [index (num-array-at-e2 e)])
           (if (< index (array-length array))
               (get-nth array index)
               (error "array access out of bound")))]
        [(num-array-set? e)
          (letrec ([array (eval-under-env (num-array-set-e1 e) env)]
               [index (num-array-set-e2 e)]
               [v (eval-under-env (num-array-set-e3 e) env)])
            (if (< index (array-length array))
               (begin (set-array-val array index v) v)
               (error "array access out of bound")))]
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

(define exp1 (add (int 3) (int 4)))
(eval-under-env exp1 null) ; this should return (int 7)
(define exp2 (add (var "x") (var "y")))
(eval-under-env exp2 (list (cons "x" (int 3)) (cons "y" (int 4)))) ; this should return (int 7)


;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* li e)
  (if (null? li) e
      (mlet (car (car li)) (cdr (car li)) (mlet* (cdr li) e))))


(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
    (ifgreater (var "_x") (var "_y") e4
               (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "f"
       (fun "lambda" "l"
            (ifaunit (var "l")
                     (aunit)
                     (apair
                      (call (var "f") (fst (var "l")))
                      (call (var "lambda") (snd (var "l"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map")
                   (fun #f "x" (add (var "x") (var "i")))))))

