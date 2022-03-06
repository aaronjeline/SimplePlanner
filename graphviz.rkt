#lang racket
(provide render-state-graph)
(require "planning.rkt" racket/trace)

(define DOT_LOCATION "/opt/homebrew/bin/dot")

(define (render-state-graph graph p path-to-goal)
  (define dot (graph->dot graph p path-to-goal))
  (display dot)
  (define dot_filename (make-temporary-file))
  (define png_filename (make-temporary-file "~a.png"))
  (with-output-to-file dot_filename
    #:exists 'truncate
    (λ ()
      (display dot)))
  (system (format "~a -Tpng ~a > ~a" DOT_LOCATION dot_filename png_filename))
  (system (format "~a ~a" (get-open-cmd) png_filename)))

(define (get-open-cmd)
  (match (system-type 'os)
    ['unix "xdg-open"]
    ['macosx "open"]
    [else (error "Unsupported operating system: " else)]))

(define preamble
  '("digraph {"))

(define epilogue
  '("}"))
        

(define (graph->dot graph p path-to-goal)
  (string-join
   `(,@preamble
     ,@(render-graph graph (path->edges graph path-to-goal))
     ,@(label-start p)
     ,@(label-goal path-to-goal)
     ,@epilogue)
   "\n"))


(define (label-start p)
  (list (format "\"~a\" [color = green]" (problem-init p))))

(define (label-goal path-to-goal)
  (if path-to-goal
      (list
       (format "\"~a\" [color = blue]" (last path-to-goal)))
      '()))




(define/contract (render-graph graph edges)
  (-> state-graph? list? (listof string?))
  (for/list [(edge (state-graph-edges graph))]
    (render-edge edge edges)))

(define/contract (render-edge edge edges)
  (-> edge? list? string?)
  (match edge
    [(list to action from)
     (format "\"~a\" -> \"~a\" [ label=\"~a\", labal = ~a];"
             to
             from
             (action->string action)
             (if (member edge edges)
                 "blue"
                 "black"))]))


