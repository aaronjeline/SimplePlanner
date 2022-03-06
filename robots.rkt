#lang racket
(require "planning.rkt" "graphviz.rkt")


(define/problem
  #:name robots
  #:inits ((loc r1 l1)
           (pos c1 l1)
           (cargo r1 empty)
           (location l1)
           (location l2)
           (adjacent l1 l2)
           (robot r1)
           (crate c1))
  #:goal ((loc r1 l2)
          (pos c1 l2))
  #:objects (r1 l1 l2 c1))

(define/action
  #:name move
  #:problem robots
  #:params (r la lb)
  #:prereqs ((loc r la)
             (location la)
             (location la)
             (adjacent la lb)
             (robot r))
  #:effect ((loc r l2))
  #:neg-effect ((loc r l1)))

(define/action
  #:name pickup
  #:problem robots
  #:params (r c l)
  #:prereqs ((loc r l)
             (pos c l)
             (cargo r empty)
             (robot r)
             (crate c)
             (location l))
             
  #:effect ((cargo r c)
            (pos c r))
  #:neg-effect ((cargo r empty)
                (pos c l)))

(define/action
  #:name drop
  #:problem robots
  #:params (r c l)
  #:prereqs ((loc r l)
             (pos c r)
             (cargo r c)
             (robot r)
             (crate c)
             (location l))
  #:effect ((cargo r empty)
            (pos c l))
  #:neg-effect ((pos c r)
                (cargo r c)))


(define-values (g path-to-goal) (breadth-first-search robots))

(render-state-graph g robots path-to-goal)