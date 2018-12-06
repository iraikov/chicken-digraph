;;
;; 
;; Directed graph in adjacency list format.
;; Based on code from MLRISC.
;;
;; Copyright 2007-2018 Ivan Raikov.
;;
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.

(module digraph

        (digraph? make-digraph graph-name graph-info new-id!  add-node!
                  add-edge!  remove-node!  set-in-edges!  set-out-edges!  set-entries!
                  set-exits!  garbage-collect!  nodes edges order capacity out-edges
                  in-edges succ pred succ-list pred-list has-edge has-node node-info
                  node-info-set!  entries exits foreach-node
                  foreach-edge fold-nodes fold-edges roots terminals)
		   
  (import scheme (chicken base) (chicken pretty-print)
          (only srfi-1 fold any remove filter-map )
          dyn-vector matchable yasos)

  (define (digraph:error x . rest)
    (let ((port (open-output-string)))
      (let loop ((objs (cons x rest)))
        (if (null? objs)
            (begin
              (newline port)
              (error 'digraph (get-output-string port)))
            (begin (display (car objs) port)
                   (display " " port)
                   (loop (cdr objs)))))))
  
  (define-predicate  digraph?)
  
  (define-operation (graph-name graph))
  (define-operation (graph-info graph))
  (define-operation (new-id! graph))
  (define-operation (add-node! graph i info))
  (define-operation (add-edge! graph e))
  (define-operation (remove-node! graph i))
  (define-operation (set-in-edges! graph j edges))
  (define-operation (set-out-edges! graph i edges))
  (define-operation (set-entries! graph ns))
  (define-operation (set-exits! graph ns))
  (define-operation (garbage-collect! graph))
  (define-operation (nodes graph))
  (define-operation (edges graph))
  (define-operation (order graph))
  (define-operation (capacity graph))
  (define-operation (out-edges graph n))
  (define-operation (in-edges graph n))
  (define-operation (succ graph n))
  (define-operation (pred graph n))
  (define-operation (succ-list graph))
  (define-operation (pred-list graph))
  (define-operation (has-edge graph i j))
  (define-operation (has-node graph i))
  (define-operation (node-info graph n))
  (define-operation (node-info-set! graph n info))
  (define-operation (entries graph))
  (define-operation (exits graph))
;  (define-operation (entry-edges graph n))
;  (define-operation (exit-edges graph n))
  (define-operation (foreach-node graph f))
  (define-operation (foreach-edge graph f))
  (define-operation (fold-nodes graph f init))
  (define-operation (fold-edges graph f init))
  (define-operation (roots graph))          
  (define-operation (terminals graph))


  (define (make-digraph name #!key
                        (info #f)
                        (node-list (list))
                        (succ-list (list))
                        (pred-list (list)))
    
    (define nodes-vector   (list->dynvector node-list 'none))
    (define succ-vector    (list->dynvector succ-list (list)))
    (define pred-vector    (list->dynvector pred-list (list)))

    (define node-count     (make-parameter 0))
    (define edge-count     (make-parameter 0))
    (define entries        (make-parameter (list)))
    (define exits          (make-parameter (list)))
    (define new-nodes      (make-parameter (list)))
    (define garbage-nodes  (make-parameter (list)))

    (define (graph-new-id!) 
      (match (new-nodes)
             (()      (dynvector-length nodes-vector))
             ((h . t) (begin
                        (new-nodes t)
                        h))
             (else (digraph:error 'new-id! ": invalid new-nodes " (new-nodes)))))
  
    (define (graph-garbage-collect!) =
      (new-nodes (append (new-nodes) (garbage-nodes)))
      (garbage-nodes (list)))

    (define (get-nodes)
      (dynvector-fold (lambda (i st v) (if (eq? 'none v)  st  (cons (list i v) st)))
                      (list) nodes-vector))

    (define (get-edges)
      (dynvector-fold 
       (lambda (i st v) 
         (match v (() st) (else (append v st)))) (list) succ-vector))

    (define (graph-fold-nodes f init) 
      (dynvector-fold 
       (lambda (i st v) (f i v st))
       init nodes-vector))

    (define (graph-fold-edges f init)
      (dynvector-fold 
       (lambda (i st v) 
        (fold (match-lambda* (((i j info) ax) (f i j info ax))) st v))
      init succ-vector))

    (define (graph-capacity) (dynvector-length nodes-vector))

    (define (graph-add-node! i info)
      (if (eq? 'none (dynvector-ref nodes-vector i))
          (node-count (+ 1 (node-count))))
      (dynvector-set! nodes-vector i info))

    (define (graph-add-edge! e)
      (match e
             ((i j info)   
              (let ((oi (dynvector-ref succ-vector i))
		  (oj (dynvector-ref pred-vector j)))
	      (dynvector-set! succ-vector i (cons e oi))
	      (dynvector-set! pred-vector j (cons e oj))
	      (edge-count (+ 1 (edge-count)))))
	   (else (digraph:error 'add-edge ": invalid edge " e))))

    (define (graph-set-out-edges! i edges)
      (define (remove-pred elst j ax)
        (match elst 
               (() (dynvector-set! pred-vector j ax))
               (((i1 _ _) . es)  (let ((e (car elst)))
                                   (remove-pred es j (if (= i1 i) ax (cons e ax)))))
               (else   (digraph:error 'remove-pred ": invalid edge list " elst))))

      (define (remove-edge e)
        (match e 
               ((i1 j _)  (begin
			    (if (not (= i i1)) (digraph:error 'set-out-edges))
			    (remove-pred (dynvector-ref pred-vector j) j (list))))
               (else (digraph:error 'remove-edge ": invalid edge " e))))
      
      (define (add-pred e)
        (match e 
               ((_ j _)  (dynvector-set! pred-vector j (cons e (dynvector-ref pred-vector j))))
               (else (digraph:error 'add-pred ": invalid edge " e))))
      
      (let ((old-edges (dynvector-ref succ-vector i)))
        (for-each remove-edge old-edges)
        (dynvector-set! succ-vector i edges)
        (for-each add-pred edges)
        (edge-count (- (+ (edge-count) (length edges)) (length old-edges)))))


    (define (graph-set-in-edges! j edges)
      (define (remove-succ elst i ax)
        (match elst 
               (() (dynvector-set! succ-vector i ax))
               (((_ j1 _) . es)  (let ((e (car elst)))
                                   (remove-succ es i (if (= j1 j) ax (cons e ax)))))
               (else   (digraph:error 'remove-succ ": invalid edge list " elst))))

      (define (remove-edge e)
        (match e 
               ((i j1 _)  (begin
			    (if (not (= j j1)) (digraph:error 'set-in-edges))
			    (remove-succ (dynvector-ref succ-vector i) i (list))))
               (else (digraph:error 'remove-edge ": invalid edge " e))))
      
      (define (add-succ e)
        (match e 
               ((i _ _)  (dynvector-set! succ-vector i (cons e (dynvector-ref succ-vector i))))
               (else (digraph:error 'add-succ ": invalid edge " e))))

      (let ((old-edges (dynvector-ref pred-vector j)))
        (for-each remove-edge old-edges)
        (dynvector-set! pred-vector j edges)
        (for-each add-succ edges)
        (edge-count (- (+ (edge-count) (length edges)) (length old-edges)))))

    (define (graph-remove-node! i)
      (if (not (eq? 'none (dynvector-ref nodes-vector i)))
          (begin
            (graph-set-out-edges! i (list))
            (graph-set-in-edges! i  (list))
            (dynvector-set! nodes-vector i 'none)
            (node-count (- (node-count) 1))
            (garbage-nodes (cons i (garbage-nodes)))
            )))
  
    (define (remove-nodes! ns) (for-each remove-node! ns))
    (define (graph-set-entries! ns)  (entries ns))
    (define (graph-set-exits! ns)    (exits ns))
    (define (get-entries)      (entries))
    (define (get-exits)        (exits))
    (define (graph-out-edges n)      (dynvector-ref succ-vector n))
    (define (graph-in-edges n)       (dynvector-ref pred-vector n))

    (define (get-roots)
      (filter-map
       (lambda (n)
         (if (null?
              ;; check only edges from other nodes
              (remove (o (cut = <> (car n)) car)
                      (graph-in-edges (car n))))
             (car n)
             #f))
       (get-nodes)))

    (define (get-terminals)
      (filter-map
       (lambda (n)
         (if (null?
              ;; check only edges to other nodes
              (remove (o (cut = <> (car n)) cadr)
                      (graph-out-edges (car n))))
             (car n)
             #f))
       (get-nodes)))
  
    (define (get-succ n)
      (map (lambda (x) (list-ref x 1)) (dynvector-ref succ-vector n)))
    (define (get-pred n)
      (map (lambda (x) (list-ref x 0)) (dynvector-ref pred-vector n)))

    (define (graph-has-edge i j)
      (any (lambda (e) 
             (match e ((_ j1 _) (= j j1))
                    (else (digraph:error 'has-edge ": invalid edge " e))))
           (dynvector-ref succ-vector i)))
    
    (define (graph-has-node n)
      (not (eq? 'none (dynvector-ref nodes-vector n))))
    
    (define (graph-node-info n)
      (let ((info (dynvector-ref nodes-vector n)))
        (and (not (eq? 'none info)) info)))
    
    (define (graph-node-info-set! n v)
      (dynvector-set! nodes-vector n v))


    (define (graph-foreach-node f)
      (dynvector-for-each
       (lambda (i x) (if (not (eq? 'none x))  (f i x)))
       nodes-vector))

    (define (graph-foreach-edge f)
      (dynvector-for-each f succ-vector))

    
    ;; Dispatcher
    (object
     ((digraph? self)                #t)
     ((graph-name self)              name)
     ((graph-info self)              info)
     ((new-id! self)                 (graph-new-id!))
     ((add-node! self i info)        (graph-add-node! i info))
     ((add-edge! self e)             (graph-add-edge! e))
     ((remove-node! self i)          (graph-remove-node! i))
     ((set-in-edges! self j edges)   (graph-set-in-edges! j edges))
     ((set-out-edges! self i edges)  (graph-set-out-edges! i edges))
     ((set-entries! self ns)         (graph-set-entries! ns))
     ((set-exits! self ns)           (graph-set-exits! ns))
     ((garbage-collect! self)        (graph-garbage-collect!))
     ((nodes self)                   (get-nodes))
     ((edges self)                   (get-edges))
     ((order self)                   (node-count))
     ((size self)                    (edge-count))
     ((capacity self)                (graph-capacity))
     ((out-edges self n)             (graph-out-edges n))
     ((in-edges self n)              (graph-in-edges n))
     ((succ self n)                  (get-succ n))
     ((pred self n)                  (get-pred n))
     ((succ-list self)               (dynvector->list succ))
     ((pred-list self)               (dynvector->list pred))
     ((has-edge self i j)            (graph-has-edge i j))
     ((has-node self i)              (graph-has-node i))
     ((node-info self i)             (graph-node-info i))
     ((node-info-set! self i info)   (graph-node-info-set! i info))
     ((entries self)                 (get-entries))
     ((exits self)                   (get-exits))
     ((foreach-node self f)          (graph-foreach-node f))
     ((foreach-edge self f)          (graph-foreach-edge f))
     ((fold-nodes self f init)       (graph-fold-nodes f init))
     ((fold-edges self f init)       (graph-fold-edges f init))
     ((roots self)                   (get-roots))
     ((terminals self)               (get-terminals))
     ((show self)                    (pretty-print
                                      (list `(nodes . ,(dynvector->list nodes))
                                            `(succ  . ,(dynvector->list succ))
                                            `(pred  . ,(dynvector->list pred)))))
     ))

  
)
