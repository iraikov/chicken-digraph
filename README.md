# chicken-digraph

Scheme implementation of a directed graph in adjacency list format

The `digraph` library is an implementation of a directed graph, where
the edges are stored as adjacency lists indexed by node number.

## Directed graph procedures

The digraph object is created by procedure `make-digraph`:


<procedure>make-digraph:: NAME INFO [NODE-LIST [SUCC-LIST [PRED-LIST]]] -> SELECTOR</procedure>

where:

- `NAME` is the graph name (string or symbol)
- `INFO` is an optional metadata object of an arbitrary type or {{#f}}
- `NODE-LIST` is an optional list of nodes to be inserted in the graph; each element of the list must be of the form {{(N INFO)}} where {{N}} is a unique node number (integer), and {{INFO}} is an optional metadata object describing the node. 
- `SUCC-LIST` and {{PRED-LIST}} can be used to define the graph edges upon graph creation. If supplied, these arguments must be lists in which every element is of the form {{(I J INFO)}}, where {{I}} and {{J}} are node numbers, and {{INFO}} is an optional metadata object.


The returned yasos object supports the following operations:

- 'name` : returns the graph name (string or symbol)
- 'info` : returns the graph metadata (arbitrary type)
- 'new-id!` : returns the lowest available node number
- 'add-node!` : procedure `LAMBDA N INFO` which inserts in the graph node with number `N` and metadata `INFO`- if the node already exists in the graph, it will be overwritten with the new metadata
- 'add-edge!` : procedure `LAMBDA EDGE` which inserts in the graph the specifed edge- the edge is given by a list of the form `(I J INFO)`, where `I` and `J` are source and destination nodes, respectively, and `INFO` is edge metadata of arbitrary type
- 'remove-node!` : procedure `LAMBDA N` which removes node `N` and all its edges from the graph
- 'nodes` : returns a list with the nodes of the graph and their metadata
- 'edges` : returns a list with the edges of the graph and their metadata
- 'roots` : returns a list with all nodes in the graph that do not have an predecessor
- 'terminals` : returns a list with all nodes in the graph that do not have a successor
- 'order` : returns the number of nodes in the graph
- 'size` : returns the number of edges in the graph
- 'capacity` : returns the size of the underlying dynamic vector
- 'succ` : procedure `LAMBDA N` which returns a list with the successor nodes of node `N`
- 'pred` : procedure `LAMBDA N` which returns a list with the predecessor nodes of node `N`
- 'succ-list` : returns a list containing the successor nodes for each node.
- 'pred-list` : returns a list containing the predecessor nodes for each node.
- 'out-edges` : procedure `LAMBDA N` which returns a list with the outgoing edges of node `N`
- 'in-edges` : procedure `LAMBDA N` which returns a list with the incoming edges of node `N`
- 'has-edge` : procedure `LAMBDA I J` which returns true if edge `I -> J` exists in the graph and false otherwise
- 'has-node` : procedure `LAMBDA N` which returns true if node `N` exists in the graph and false otherwise
- 'node-info` : procedure `LAMBDA N` which returns the metadata for node `N`
- 'node-info-set!` : procedure `LAMBDA N V` which sets the metadata for node `N`
- 'fold-node` :  procedure `LAMBDA F RES` which iterates over the nodes in the graph by invoking function `F` on the node number, metadata of each node, and accumulator variable `RES`
- 'fold-edge` : procedure `LAMBDA F RES` which iterates over the edges in the graph by invoking function `F` on each edge and accumulator variable `RES`
- 'foreach-node` : procedure `LAMBDA F` which iterates over the nodes in the graph by invoking function `F` on the node number and metadata of each node
- 'foreach-edge` : procedure `LAMBDA F` which iterates over the edges in the graph by invoking function `F` on each edge


## Examples

```scheme

 ;; example adapted from graph example in the Boost library documentation
 (import srfi-1 digraph matchable)
    
 (define g (make-digraph 'depgraph "dependency graph"))
    
 (define used-by
    (list 
      (cons 'dax_h 'foo_cpp) (cons 'dax_h 'bar_cpp) (cons 'dax_h 'yow_h)
      (cons 'yow_h 'bar_cpp) (cons 'yow_h 'zag_cpp) (cons 'boz_h 'bar_cpp)
      (cons 'boz_h 'zig_cpp) (cons 'boz_h 'zag_cpp) (cons 'zow_h 'foo_cpp)
      (cons 'foo_cpp 'foo_o) (cons 'foo_o 'libfoobar_a) 
      (cons 'bar_cpp 'bar_o) (cons 'bar_o 'libfoobar_a) 
      (cons 'libfoobar_a 'libzigzag_a) (cons 'zig_cpp 'zig_o) 
      (cons 'zig_o 'libzigzag_a) (cons 'zag_cpp 'zag_o) 
      (cons 'zag_o 'libzigzag_a) (cons 'libzigzag_a 'killerapp)))
 
 
 (define node-list (delete-duplicates 
 		   (concatenate (list (map car used-by) (map cdr used-by)))))
 
 (define node-ids (list-tabulate (length node-list) values))
  
 (for-each (lambda (i n) (add-node! g i n)) node-ids node-list)
 (define node-map (zip node-list node-ids))
 
 (for-each (lambda (e) 
 	    (match e ((ni . nj) (let ((i (car (alist-ref ni node-map)))
 				      (j (car (alist-ref nj node-map))))
 				  (add-edge! g (list i j (format "~A->~A" ni nj)))))
 		   (else (error "invalid edge " e))))
 	  used-by)
 (print (nodes g))
 (print (edges g))
 
 (remove-node! g 0)
 (print (nodes g))
 (print (edges g))

```


## License

```
 Copyright 2007-2018 Ivan Raikov.
 
 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or (at
 your option) any later version.
 
 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.
 
 A full copy of the GPL license can be found at
 <http://www.gnu.org/licenses/>.

```
