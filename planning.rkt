#lang racket
(provide (all-defined-out))

(module+ test
  (require rackunit))
(struct action (name grounded params prereqs effect neg-effect) #:transparent)

(define (action->string a)
  (match a
    [(action name _ params _ _ _)
     (format "~a(~a)" name (string-join (map symbol->string params) ","))]))

(define (abstract-action? a)
  (and (action? a)
       (not (action-grounded a))))
(define (ground-action? a)
  (and (action? a)
       (action-grounded a)))
(struct problem (name actions init goal objs) #:transparent)
(define relation? (listof symbol?))

(define-syntax define/problem
  (syntax-rules ()
    [(_ #:name name #:inits inits #:goal goal #:objects objs)
     (define name (problem (quote name) (box '()) (quote inits) (quote goal) (quote objs)))]))

(define-syntax define/action
  (syntax-rules ()
    [(_ #:name name #:problem p #:params params #:prereqs pr #:effect eff #:neg-effect neg-eff)
     (begin
       (define name (action (quote name) #f (quote params) (quote pr) (quote eff) (quote neg-eff)))
       (set-box! (problem-actions p) (cons name (unbox (problem-actions p)))))]))
    

(define state? (listof relation?))

(struct state-graph (neighbors) #:transparent)
(define edge?
  (list/c state? ground-action? state?))

(define (new-state-graph)
  (state-graph (box '())))

(define (build-init-state-graph p)
  (new-state-graph))

(define (state-graph-edges g)
  (unbox (state-graph-neighbors g)))


(define (find-neighbors graph state (format 'list))
  (map
   edge->dest
   (filter
    (src-is state)
    (state-graph-edges graph))))
   

(define (edge->dest e)
  (match e
    [(list _ _ from) from]))

(define (src-is state)
  (λ (edge)
    (match edge
      [(list to _ _) (equal? to state)])))

(define (find-edge src dst g)
  (match (filter
          (λ (e) (match e
                   [(list tsrc _ tdst)  (and (equal? tsrc src)
                                             (equal? tdst dst))]))
          (state-graph-edges g))
    [(list e) e]
    [_ (error "Edge does not exist!")]))

;; Can be made TCO w/ minimal effor
(define (path->edges g path)
  (match path
    ['() '()]
    [(cons _ '()) '()]
    [(cons a (cons b rst))
     (cons (find-edge a b g)
           (path->edges g (cons b rst)))]))

(define/contract (add-neighbor! graph from action to)
  (-> state-graph? state? ground-action? state? void?)
  (set-box! (state-graph-neighbors graph)
            (cons
             (list from action to)
             (unbox (state-graph-neighbors graph)))))

(define (expand-frontier! graph state p)
  (for [(ground-action (all-applicable-actions p state))]
    (add-neighbor! graph state ground-action (apply-action ground-action state))))

;; An solution-aglorithm is a function from problems -> (graph X path)

#;
(define (solve-problem alg p)
  (define-values (g path) (alg p))
  (render-state-graph g p path)
  path)

(define (breadth-first-search p)
  (define g (new-state-graph))
  (define path-to-goal (bfs p g (problem-init p) {set}))
  (values g path-to-goal))

(define (bfs p g cur-state seen)
  (cond
    [(seen? cur-state seen) #f]
    [(is-goal? p cur-state) (cons cur-state '())]
    [else
     (define now-seen (set-add seen cur-state))
     (expand-frontier! g cur-state p)
     (define paths-from-here
       (filter-map
        (λ (neighbor)
          (bfs p g neighbor now-seen))
        (find-neighbors g cur-state)))
     (if (empty? paths-from-here)
         #f ;; no path to goal
         (cons cur-state (argmin length paths-from-here)))]))
    
(define (seen? cur-state seen)
  (set-member? seen cur-state))

(define (is-goal? p cur-state)
  (for/and [(clause (problem-goal p))]
    (member clause cur-state)))
      


(define (∪ . xs)
  (if (empty? xs)
      (set)
      (apply set-union xs)))


;; TODO this should be memo-ized
;; Also, some type system could make this more efficient
(define (all-possible-args a p)
  (apply cartesian-product (build-list (action-arity a) (constant (problem-objs p)))))

;; Check if an instantiation exists for an action
;; If so, return the instantiated action
(define/contract (is-applicable? a p s)
  (-> abstract-action? problem? state? (or/c ground-action? false?))
  (ormap
   (λ (args)
     (let [(grounded (instantiate a args))]
       (if (applicable? grounded s)
           grounded
           #f)))
   (all-possible-args a p)))

(define (filter-map f lst)
  (filter identity (map f lst)))


(define (constant x)
  (λ (y) x))
        

(define/problem
  #:name robots
  #:inits ((loc r1 l1))
  #:goal ((loc r1 l2))
  #:objects (r1 l1 l2))

(define/action
  #:name move
  #:problem robots
  #:params (r l1 l2)
  #:prereqs ((loc r l1))
  #:effect ((loc r l2))
  #:neg-effect ((loc r l1)))



(define (all-applicable-actions p s)
  (filter-map
   (λ (a) (is-applicable? a p s))
   (unbox (problem-actions p))))


(define (action-arity a)
  (length (action-params a)))

(define (apply-action a s)
  (match a
    [(action _ _ _ _ eff neg-eff)
     (append eff (remove-all neg-eff s))]))
(module+ test
  (check-equal?
   (apply-action (instantiate move '(r1 l1 l2)) '((loc r1 l1)))
   '((loc r1 l2))))

(define (remove-all negs s)
  (foldl (λ (x next-s) (remove x next-s)) s negs))
(module+ test
  (check-equal?
   (remove-all '(h e l o) '(a b c d e f g h i j k l m n o))
   '(a b c d f g i j k m n)))



(define (applicable? act state)
  (andmap (λ (r) (present? r state))
          (action-prereqs act)))
      
  
(module+ test
  (check-equal?
   (applicable? (instantiate move '(r1 l1 l2))
                '((loc r1 l1)))
   '((loc r1 l1)))
   
  (check-false
   (applicable? (instantiate move '(r1 l2 l3))
                '((loc r1 l1)))))


(define (present? r s)
  (member r s))

(define (instantiate act args)
  (unless (= (length args) (length (action-params act)))
    (arity-error act args))
  (define env (zip (action-params act) args))
  (instantiate-action act env))

(module+ test
  (check-equal?
   (instantiate move '(r1 l1 l2))
   (action 'move #t '(r1 l1 l2) '((loc r1 l1)) '((loc r1 l2)) '((loc r1 l1)))))

(define (arity-error act args)
  (error (format "Arity Error! Action: ~a, Expected: ~a, got: ~a"
                 (action-name act)
                 (length (action-params act))
                 (length args))))

(define (instantiate-action a env)
  (define (instantiate-relations relations)
    (map (λ (r) (instantiate-relation r env)) relations))
  (match a
    [(action name #f params pr eff neg-eff)
     (action name #t (map second env)
             (instantiate-relations pr)
             (instantiate-relations eff)
             (instantiate-relations neg-eff))]))
   
    
             

(define (instantiate-relation relation env)
  (match relation
    [(cons name rst)
     (cons name (map (λ (x) (instantiate-symbol x env)) rst))]))

(module+ test
  (check-equal? (instantiate-relation '(loc x l) '((loc z) (x r1) (l l2)))
                '(loc r1 l2)))

(define (instantiate-symbol x env)
  (if (x . ∈ . env)
      (lookup x env)
      x))
(module+ test
  (check-equal? (instantiate-symbol 'x '((x y))) 'y)
  (check-equal? (instantiate-symbol 'x '((z y))) 'x))

(define (∈ x lst) (member x (map first lst)))

(define (lookup x lst)
  (match (assoc x lst)
    [#f (error "Unbound Variable: " x)]
    [(list _ value) value]))
  



(define (zip a b)
  (map list a b))