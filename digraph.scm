;;
;; 
;; Directed graph in adjacency list format.
;; Based on code from MLRISC.
;;
;; Copyright 2007-2011 Ivan Raikov and the Okinawa Institute of Science and Technology.
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

 (make-digraph)
		   
 (import scheme chicken data-structures extras )

 (require-extension srfi-1 dyn-vector matchable )

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

(define (make-digraph name info . rest)
 (let-optionals  rest ((node-list (list)) (succ-list (list)) (pred-list (list)))
  (define nodes    (list->dynvector node-list 'none))
  (define succ     (list->dynvector succ-list (list)))
  (define pred     (list->dynvector pred-list (list)))

  (define node-count     0)
  (define edge-count     0)
  (define entries        (list))
  (define exits          (list))
  (define new-nodes      (list))
  (define garbage-nodes  (list))

  (define (new-id!) 
    (match new-nodes
	   (()      (dynvector-length nodes))
	   ((h . t) (begin
		      (set! new-nodes t)
		      h))
	   (else (digraph:error 'new-id ": invalid new-nodes " new-nodes))))
  
  (define (garbage-collect!) =
    (set! new-nodes (append new-nodes garbage-nodes))
    (set! garbage-nodes (list)))

  (define (get-nodes)
    (dynvector-fold (lambda (i st v) (if (eq? 'none v)  st  (cons (list i v) st)))
     (list) nodes))

  (define (get-edges)
    (concatenate 
     (dynvector-fold 
      (lambda (i st v) 
	(match v (() st) (else (cons v st)))) (list) succ)))

  (define (order)  node-count)

  (define (size)   edge-count)

  (define (capacity) (dynvector-length nodes))

  (define (add-node! i info)
    (if (eq? 'none (dynvector-ref nodes i))
	(set! node-count (fx+ 1 node-count)))
    (dynvector-set! nodes i info))

  (define (add-edge! e)
    (match e
	   ((i j info)   
	    (let ((oi (dynvector-ref succ i))
		  (oj (dynvector-ref pred j)))
	      (dynvector-set! succ i (cons e oi))
	      (dynvector-set! pred j (cons e oj))
	      (set! edge-count (fx+ 1 edge-count))))
	   (else (digraph:error 'add-edge ": invalid edge " e))))

  (define (set-out-edges! i edges)
    (define (remove-pred elst j ax)
      (match elst 
	     (() (dynvector-set! pred j ax))
	     (((i1 _ _) . es)  (let ((e (car elst)))
				 (remove-pred es j (if (fx= i1 i) ax (cons e ax)))))
	     (else   (digraph:error 'remove-pred ": invalid edge list " elst))))

    (define (remove-edge e)
      (match e 
	     ((i1 j _)  (begin
			    (if (not (fx= i i1)) (digraph:error 'set-out-edges))
			    (remove-pred (dynvector-ref pred j) j (list))))
	     (else (digraph:error 'remove-edge ": invalid edge " e))))

    (define (add-pred e)
      (match e 
	     ((_ j _)  (dynvector-set! pred j (cons e (dynvector-ref pred j))))
	     (else (digraph:error 'add-pred ": invalid edge " e))))
    
    (let ((old-edges (dynvector-ref succ i)))
      (for-each remove-edge old-edges)
      (dynvector-set! succ i edges)
      (for-each add-pred edges)
      (set! edge-count (fx- (fx+ edge-count (length edges)) (length old-edges)))))


  (define (set-in-edges! j edges)
    (define (remove-succ elst i ax)
      (match elst 
	     (() (dynvector-set! succ i ax))
	     (((_ j1 _) . es)  (let ((e (car elst)))
				 (remove-succ es i (if (fx= j1 j) ax (cons e ax)))))
	     (else   (digraph:error 'remove-succ ": invalid edge list " elst))))

    (define (remove-edge e)
      (match e 
	     ((i j1 _)  (begin
			    (if (not (fx= j j1)) (digraph:error 'set-in-edges))
			    (remove-succ (dynvector-ref succ i) i (list))))
	     (else (digraph:error 'remove-edge ": invalid edge " e))))

    (define (add-succ e)
      (match e 
	     ((i _ _)  (dynvector-set! succ i (cons e (dynvector-ref succ i))))
	     (else (digraph:error 'add-succ ": invalid edge " e))))

    (let ((old-edges (dynvector-ref pred j)))
      (for-each remove-edge old-edges)
      (dynvector-set! pred j edges)
      (for-each add-succ edges)
      (set! edge-count (fx- (fx+ edge-count (length edges)) (length old-edges)))))

  (define (remove-node! i)
    (if (not (eq? 'none (dynvector-ref nodes i)))
	(begin
	  (set-out-edges! i (list))
	  (set-in-edges! i  (list))
	  (dynvector-set! nodes i 'none)
	  (set! node-count (fx- node-count 1))
	  (set! garbage-nodes (cons i garbage-nodes))
	  (void))))
  
  (define (remove-nodes! ns) (for-each remove-node! ns))
  (define (set-entries! ns)  (set! entries ns))
  (define (set-exits! ns)    (set! exits ns))
  (define (get-entries)      entries)
  (define (get-exits)        exits)
  (define (out-edges n)      (dynvector-ref succ n))
  (define (in-edges n)       (dynvector-ref pred n))

  (define (get-succ n)       (map (lambda (x) (list-ref x 1)) (dynvector-ref succ n)))
  (define (get-pred n)       (map (lambda (x) (list-ref x 0)) (dynvector-ref pred n)))

  (define (has-edge i j)     (any (lambda (e) 
				    (match e ((_ j1 _) (fx= j j1))
					   (else (digraph:error 'has-edge ": invalid edge " e))))
				  (dynvector-ref succ i)))
  
  (define (has-node n)       (not (eq? 'none (dynvector-ref nodes n))))

  (define (node-info n)      (let ((info (dynvector-ref nodes n)))
			       (and (not (eq? 'none info)) info)))

  (define (node-info-set! n v)   (dynvector-set! nodes n v))


  (define (foreach-node f)   (dynvector-for-each (lambda (i x) (if (not (eq? 'none x))  (f i x)))
						 nodes))

  (define (foreach-edge f)   (dynvector-for-each f succ))

  
  ;; Dispatcher
  (lambda (selector)
      (case selector
	((name)              name)
	((graph-info)        info)
	((new-id!)           new-id!)
	((add-node!)         add-node!)
	((add-edge!)         add-edge!)
	((remove-node!)      remove-node!)
	((set-in-edges!)     set-in-edges!)
	((set-out-edges!)    set-out-edges!)
	((set-entries!)      set-entries!)
	((set-exits!)        set-exits!)
	((garbage-collect!)  garbage-collect!)
	((nodes)             get-nodes)
	((edges)             get-edges)
	((order)             order)
	((size)              size)
	((capacity)          capacity)
	((out-edges)         out-edges)
	((in-edges)          in-edges)
	((succ)              get-succ)
	((pred)              get-pred)
	((succ-list)         (lambda () (dynvector->list succ)))
	((pred-list)         (lambda () (dynvector->list pred)))
	((has-edge)          has-edge)
	((has-node)          has-node)
	((node-info)         node-info)
	((node-info-set!)    node-info-set!)
	((entries)           get-entries)
	((exits)             get-exits)
	((entry-edges)       (lambda (x) (list)))
	((exit-edges)        (lambda (x) (list)))
	((foreach-node)      foreach-node)
        ((foreach-edge)      foreach-edge)
	((roots)             (lambda ()
			       (filter-map (lambda (n)
                                             (if (null?
                                                  ;; check only edges from other nodes
                                                  (remove (o (cut fx= <> (car n)) car)
                                                          (in-edges (car n))))
                                                 (car n)
                                                 #f))
					   (get-nodes))))
	((terminals)         (lambda ()
			       (filter-map (lambda (n)
                                             (if (null?
                                                  ;; check only edges to other nodes
                                                  (remove (o (cut fx= <> (car n)) cadr)
                                                          (out-edges (car n))))
                                                 (car n)
                                                 #f))
					   (get-nodes))))
	((debug)             (list (cons nodes (dynvector->list nodes))
				   (cons succ (dynvector->list succ))
				   (cons pred (dynvector->list pred))))
        (else
          (digraph:error 'selector ": unknown message " selector " sent to a graph"))))))
)
