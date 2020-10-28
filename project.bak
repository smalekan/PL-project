#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs
(struct var      (string)              #:transparent)   ;; a variable, e.g., (var "foo")r
(struct int      (num)                 #:transparent)   ;; a constant number, e.g., (int 17)
(struct add      (e1 e2)               #:transparent)   ;; add two expressions
(struct mult     (e1 e2)               #:transparent)   ;; multiply two numbers
(struct neg      (e)                   #:transparent)   ;; neg of a number
(struct fun      (nameopt formal body) #:transparent)   ;; a recursive(?) 1-argument functionif e1< e2 : true
(struct islthan  (e1 e2)               #:transparent)   ;; if e1 > e2 then int 1 else int 0
(struct ifzero   (e1 e2 e3)            #:transparent)   ;; if e1=0 then e2 else e3
(struct ifgthan  (e1 e2 e3 e4)         #:transparent)   ;; if e1 > e2 then e3 else e4
(struct call     (funexp actual)       #:transparent)   ;; function call
(struct mlet     (var e body)          #:transparent)   ;; a local binding (let var = e in body) 
(struct apair    (e1 e2)               #:transparent)   ;; make a new pair
(struct first    (e)                   #:transparent)   ;; get first part of a pair
(struct second   (e)                   #:transparent)   ;; get second part of a pair
(struct munit    ()                    #:transparent)   ;; unit value -- good for ending a list
(struct ismunit  (e)                   #:transparent)   ;; evaluate to 1 if e is unit else 0


;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)


;; Problem 1

;;  part1
(define (racketlist->numexlist xs)
  (if (null? xs)
      (munit)
      (apair (car xs) (racketlist->numexlist (cdr xs)))))


;;  part2-with error-handler
;(define (numexlist->racketlist xs)
 ; (if (equal? (eval-exp (ismunit xs)) (int 1)) '()
  ;    (cons (eval-exp (first xs))
   ;         (numexlist->racketlist (eval-exp(second xs))))))



;;  part2-without error handler (for test case)
(define (numexlist->racketlist xs)
  (if (munit? xs) '()
      (cons (apair-e1 xs) (numexlist->racketlist  (apair-e2 xs)))))


;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))


;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.


;;  Add

(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "NUMEX addition applied to non-number")))]

        
;;  Mult
        
    [(mult? e)
     (let ([v1 (eval-under-env (mult-e1 e) env)]
           [v2 (eval-under-env (mult-e2 e) env)])
       (if (and (int? v1)
                (int? v2))
           (int (* (int-num v1)
                   (int-num v2)))
           (error "NUMEX multplication applied to non-number")))]

    
;;  Neg
    
    [(neg? e) 
         (let ([v1 (eval-under-env (neg-e e) env)])
           (if (int? v1)
               (int (- (int-num v1)))
               (error "NUMEX negation applied to non-number")))]

    
;;  Function
    
   [(fun? e)
     (closure env e)]

    
;; int value
    [(int? e)
     (if (integer? (int-num e))           
       e
     (error "int struct value is not valid"))]

    
;;  closure
    
    [(closure? e) e]

    
;; islthan
    
    [(islthan? e) 
         (let ([v1 (eval-under-env (islthan-e1 e) env)]
               [v2 (eval-under-env (islthan-e2 e) env)])
         (if (and (int? v1)
                    (int? v2))
               (cond [(< (int-num v1) 
                       (int-num v2))
                     (int 1)]
                     [#t (int 0)])
                                   
               (error "NUMEX lessthan applied to non-number")))]


    
;;  if-zero (not lazy evaluation)
    
;    [(ifzero? e)
;    (let ([v1 (eval-under-env (ifzero-e1 e) env)])
;       (if (int? v1)
;       (if (equal? (int-num v1) 0) (eval-under-env (ifzero-e2 e) env) (eval-under-env (ifzero-e3 e) env))
;       (error "NUMEX ifzero applied to non-number")))]


    
;;  if-zero (lazy evaluation)
    
        [(ifzero? e)
         (let ([v1 (eval-under-env (ifzero-e1 e) env)])
           (if (int? v1)
               (if (equal? (int-num v1) 0) (eval-under-env (ifzero-e2 e) env) (eval-under-env (ifzero-e3 e) env))
               (error "NUMEX ifzero applied to non-number ")))]

        

;;  ifgthan (not lazy evaluation)
    
;    [(ifgthan? e)
;        (let ([v1 (eval-under-env (ifgthan-e1 e) env)]
;              [v2 (eval-under-env (ifgthan-e2 e) env)]
;              [v3 (eval-under-env (ifgthan-e3 e) env)]
;               [v4 (eval-under-env (ifgthan-e4 e) env)])
;           (if (and (int? v1) (int? v2))
;               (if (> (int-num v1) (int-num v2)) v3 v4)


        
;; ifgthan (lazy evaluation)
        
    [(ifgthan? e)
     (let ([v1 (eval-under-env (ifgthan-e1 e) env)]
           [v2 (eval-under-env (ifgthan-e2 e) env)])
       (if (and (int? v1)
                (int? v2))
           (if (> (int-num v1) (int-num v2))
               (eval-under-env (ifgthan-e3 e) env)
               (eval-under-env (ifgthan-e4 e) env))
           (error "NUMEX ifgthan applied to non-number")))]
    
    
;;  mlet 

    [(mlet? e)
     (let* ([v (eval-under-env (mlet-e e) env)]
            [ext-env (cons (cons (mlet-var e) v) env)])
       (eval-under-env (mlet-body e) ext-env))]


    
;;  call
    
    [(call? e)
     (let ([v1 (eval-under-env (call-funexp e) env)]
           [v2 (eval-under-env (call-actual e) env)])
       (if (closure? v1)
           (let* ([c-fun (closure-fun v1)]
                  [c-env (closure-env v1)]
                  [ext-env-temp (cons (cons (fun-formal c-fun) v2) c-env)]
                  [ext-env (if (fun-nameopt c-fun)
                               (cons (cons (fun-nameopt c-fun) v1) ext-env-temp)
                               ext-env-temp)])
             (eval-under-env (fun-body c-fun) ext-env))
           (error "NUMEX call first-subexpression not closure")))]


    
;;  apair
    
    [(apair? e)
     (let ([v1 (eval-under-env (apair-e1 e) env)]
           [v2 (eval-under-env (apair-e2 e) env)])
       (apair v1 v2))]


    
;;  first
    
    [(first? e)
     (let ([v (eval-under-env (first-e e) env)])
       (if (apair? v)
           (apair-e1 v)
           (error "Numex first applied to non-pair")))]
    

    
;; second
    
    [(second? e)
     (let ([v (eval-under-env (second-e e) env)])
       (if (apair? v)
           (apair-e2 v)
           (error "numlix snd applied to non-pair")))]

    
    
;;  ismunit
    
    [(ismunit? e)
     (let ([v (eval-under-env (ismunit-e e) env)])
       (if (munit? v) (int 1) (int 0)))]

    
    
;;  munit
    
    [(munit? e)
     (munit)]


    
;;  other
    
    [#t (error (format "bad numex expression: ~v" e))]))


;;  Do NOT change
(define (eval-exp e)
  (eval-under-env e null))


;;  Problem 3

;;  first-part

(define (ifmunit e1 e2 e3) (ifgthan (ismunit e1) (int 0) e2 e3))



;;  second-part

(define (mlet* bs e2)
  (if (null? bs)
      e2
      (mlet (caar bs) (cdar bs) (mlet* (cdr bs) e2))))



;;  third-part

(define (ifeq e1 e2 e3 e4) 
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgthan (var "_x") (var "_y") e4 (ifgthan (var "_y") (var "_x") e4 e3))))



;;  Problem 4

;;  first-part

(define numex-map
  (fun #f "numex-fun" (fun "g" "numex-lst" (ifmunit (var "numex-lst")
                                                  (munit)
                                                  (apair (call (var "numex-fun") (first (var "numex-lst")))
                                                         (call (var "g") (second (var "numex-lst"))))))))

;;  second-part

(define numex-mapAddN 
  (mlet "map" numex-map
        (fun #f "i" (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))



;;  Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
