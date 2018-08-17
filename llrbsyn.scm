;; (C) 2013 Jörg F. Wittenberger.

;; Redistribution permitted under either GPL, LGPL or BSD style
;; license.

;; Changes
;; Rewritten from the 2008 version; now in syntax-rules.

;; Note: this file is intented to be of temporary nature.  It's
;; contents shall be moved into llrbtree.scm after the transition to
;; the new code is completed.

;;* Left Leaning Red Black Tree

;;** Code Generator

;; Generate LLRB trees within arbitrary datastructures.

(define-syntax define-llrbtree/positional
  (syntax-rules ()
    ((_
      ;; The "features" is a list of symbols to control code
      ;; expansion.  "pure" expands to an implementation, which never
      ;; updates nodes.  "ordered" will enforce total order among the
      ;; element.  "leftmost" will include code to maintain a leftmost
      ;; value of the tree (not recommended, may be removed).
      features
      ;; The "update*" syntax must accept a node structure and
      ;; key-value pairs.  Keys are color:, left: and right:

      ;; "update" : If feature "pure" is set, "update" must expand
      ;; to a newly allocated node, otherwise is MUST expand to a
      ;; side effect full update of the original node.
      update
      ;; The following identifiers are bound in the expanded code.
      ;; Pass #f for procedures not to be expanded.
      init-root-node!		;; defined
      t-lookup			;; defined
      t-min			;; defined
      t-fold			;; defined
      t-for-each		;; defined
      t-insert			;; defined
      t-delete			;; defined
      t-delete-min		;; defined
      t-empty?			;; defined

      ;; These syntax is used expand to code for comparision
      ;; expressions.
      t-k-eq?			;; key<>node-key "equal"
      t-eq?			;; node-key<>node-key "equal"
      t-k-<?			;; key<>node-key "less then"
      t-<?			;; node<>node "less then"

      ;; Accessors to the elements of the tree.
      left
      right
      color
      )
     (begin
       (define-syntax if/pure
	 (syntax-rules (pure)
	   ((_ kt kf) (if/pure features kt kf))
	   ((_ () kt kf) kf)
	   ((_ (pure . more) kt kf) kt)
	   ((_ (kw . more) kt kf) (if/pure more kt kf))))

       (define-syntax if/ordered
	 (syntax-rules (ordered)
	   ((_ kt kf) (if/ordered features kt kf))
	   ((_ () kt kf) kf)
	   ((_ (ordered . more) kt kf) kt)
	   ((_ (kw . more) kt kf) (if/ordered more kt kf))))

       (define-syntax if/leftmost
	 (syntax-rules (leftmost)
	   ((_ kt kf) (if/leftmost features kt kf))
	   ((_ () kt kf) kf)
	   ((_ (leftmost . more) kt kf) kt)
	   ((_ (kw . more) kt kf) (if/leftmost more kt kf))))

       (define-syntax cond-define
	 (syntax-rules ()
	   ((_ (#f . params) . body) #f)
	   ((_ (id . params) . body)
	    (define (id . params) . body))))

       (define-syntax root-node (syntax-rules () ((_ x) (left x))))

#|
Root pointers not yet working.
       (define-syntax empty?
	 (syntax-rules () ((_ t n) (if/pure (not n) (eq? t n)))))
       (define-syntax empty (syntax-rules () ((_ t) (if/pure #f t))))
|#
       (define-syntax empty?
	 (syntax-rules () ((_ t n) (not n))))
       (define-syntax empty (syntax-rules () ((_ t) #f)))

       (define-syntax black (syntax-rules () ((_ t) (if/pure #t t))))
       (define-syntax red (syntax-rules () ((_) #f)))
       ;; In black? t is not used but kept for consistency with red?.
       (define-syntax red?
	 (syntax-rules () ((_ t n) (if (empty? t n) #f (not (color n))))))
       (define-syntax ptred?
	 (syntax-rules () ((_ t r sel) (if (empty? t r) #f (red? t (sel r))))))
       (define-syntax black? (syntax-rules () ((_ t n) (color n))))
       (define-syntax color-black? (syntax-rules () ((_ t c) c)))

       (define-syntax with-n-node
	 (syntax-rules ()
	   ((_ 1 t n l r c ())
	    (if (empty? t n) n
		(update n left: l right: r color: c)))
	   ((_ 1 t n l r c (step . more))
	    (begin (step t n l r c)
		   (with-n-node 1 t n l r c more)))
	   ((_ t node . steps)
	    (let ((n.n node))
	      (let ((l (left n.n))
		    (r (right n.n))
		    (c (color n.n)))
		(with-n-node 1 t n.n l r c steps))))))

       (define-syntax color-flip-node!
	 (syntax-rules ()
	   ((_ t n) (if (empty? t n) n
			(update n color: (if (black? t n) (red) (black t)))))))

       (define-syntax color-flip!
	 (syntax-rules ()
	   ((_ t n.n n.l n.r n.c)
	    (if (not (empty? t n.n))
		(begin
		  (set! n.l (color-flip-node! t n.l))
		  (set! n.r (color-flip-node! t n.r))
		  (set! n.c (if (color-black? t n.c) (red) (black t))))))))

       (define-syntax rotate-left!
	 (syntax-rules ()
	   ((_ t n.n n.l n.r n.c)
	    (begin
	      (set! n.l (update n.n left: n.l right: (left n.r) color: (red)))
	      (set! n.n n.r)
	      (set! n.r (right n.r))))))

       (define-syntax rotate-right!
	 (syntax-rules ()
	   ((_ t n.n n.l n.r n.c)
	    (begin
	      (set! n.r (update n.n left: (right n.l) right: n.r color: (red)))
	      (set! n.n n.l)
	      (set! n.l (left n.l))))))

       (define-syntax fixup!
	 (syntax-rules ()
	   ((_ t n.n n.l n.r n.c)
	    (begin
	      (if (red? t n.r)
		  (rotate-left! t n.n n.l n.r n.c))
	      (if (and (red? t n.l) (ptred? t n.l left))
		  (rotate-right! t n.n n.l n.r n.c))
	      (if (and (red? t n.l) (red? t n.r))
		  (color-flip! t n.n n.l n.r n.c))))))

       (define-syntax move-red-right!
	 (syntax-rules ()
	   ((_ t n.n n.l n.r n.c)
	    (begin
	      (color-flip! t n.n n.l n.r n.c)
	      (if (ptred? t n.l left)
		  (begin
		    (rotate-right! t n.n n.l n.r n.c)
		    (color-flip! t n.n n.l n.r n.c)))))))

       (define-syntax move-red-left!
	 (syntax-rules ()
	   ((_ t n.n n.l n.r n.c)
	    (begin
	      (color-flip! t n.n n.l n.r n.c)
	      (if (ptred? t n.r left)
		  (begin
		    (set! n.r (with-n-node t n.r rotate-right!))
		    (rotate-left! t n.n n.l n.r n.c)
		    (color-flip! t n.n n.l n.r n.c)))))))

       (define-syntax delete-min
	 (syntax-rules ()
	   ((_ t %%set-leftmost! result-box %n)
	    (let delete-min-loop ((n %n))
	      (if (empty? t (left n))
		  (begin
		    (vector-set! result-box 0 n)
		    (if/leftmost
		     (if %%set-leftmost! (%%set-leftmost! n))
		     #f)
		    (left n))
		  (letrec-syntax
		      ((doit (syntax-rules ()
			       ((_ t n.n n.l n.r n.c)
				(begin
				  (if (and (not (red? t n.l))
					   (not (ptred? t n.l left)))
				      (move-red-left! t n.n n.l n.r n.c))
				  (set! n.l (delete-min-loop n.l))
				  (fixup! t n.n n.l n.r n.c))))))
		    (with-n-node t n doit)))))))

       (cond-define (init-root-node! t) (update t color: (black t) left: (empty t)))
       (cond-define (t-empty? t) (empty? t (root-node t)))
       (cond-define
	(t-lookup t k)
	(let lookup ((node (root-node t)))
	  (cond
	   ((empty? t node) node)
	   ((t-k-eq? k node) node)
	   ((t-k-<? k node) (lookup (left node)))
	   (else (lookup (right node))))))
       (cond-define
	(t-min t)
	(if (empty? t (root-node t)) #f
	    (let min-loop ((node (root-node t)))
	      (cond
	       ((empty? t (left node)) node)
	       (else (min-loop (left node)))))))
       (cond-define
	(t-fold procedure init t)
	(define (tfold init node)
	  (if (empty? t node)
	      init
	      (tfold (procedure node (tfold init (right node))) (left node))))
	(tfold init (root-node t)))
       (cond-define
	(t-for-each procedure t)
	(let llrb-for-each-loop ((node (root-node t)))
	  (or (empty? t node)
	      (begin
		(procedure node)
		(llrb-for-each-loop (left node))
		(llrb-for-each-loop (right node))))))
       (cond-define
	(t-insert t k set-leftmost! upd dflt)
	
	(if/pure #f (update upd color: (red) left: (empty t) right: (empty t)))
	(let ((nr
	       (let llrb-insert-loop ((node (root-node t))
				      (sl set-leftmost!))
		 (if (empty? t node)
		     (let ((n (if dflt (upd (dflt)) upd)))
		       (if sl (begin (sl n) n) n))
		     (let-syntax
			 ((doit (syntax-rules ()
				  ((_ t n.n n.l n.r n.c)
				   (begin
				     (if/ordered
				      (if (t-k-eq? k n.n)
					  (set! n.n
						(let ((n (if dflt (upd n.n) upd)))
						  (update n left: n.l right: n.r color: n.c)))
					  (if (t-k-<? k n.n)
					      (set! n.l (llrb-insert-loop n.l sl))
					      (set! n.r (llrb-insert-loop n.r #f))))
				      (if (t-k-<? k n.n)
					  (set! n.l (llrb-insert-loop n.l sl))
					  (set! n.r (llrb-insert-loop n.r #f))))
				     (if (and (red? t n.r) (not (red? t n.l)))
					 (rotate-left! t n.n n.l n.r n.c))
				     (if (and (red? t n.l)
					      (red? t (left n.l)))
					 (rotate-right! t n.n n.l n.r n.c))
				     (if (and (red? t n.l) (red? t n.r))
					 (color-flip! t n.n n.l n.r n.c)))))))
		       (with-n-node t node doit))))))
	  #;(if (red? t nr)
	      (set! nr (update nr color: (black t))))
	  (update t left: nr color: (black t))))
       (cond-define
	(t-delete-min t . cont)
	(define set-leftmost! (and (pair? cont) (pair? (cdr cont)) (cadr cont)))
	(if (empty? t (root-node t))
	    (if/pure
	     (if (pair? cont) ((car cont) t #f) t)
	     #f)
	    (let* ((min (vector #f))
		   (r (delete-min t set-leftmost! min (root-node t))))
	      (if/leftmost
	       (if (and set-leftmost! (empty? t r))
		   (set-leftmost! r))
	       #f)
	      ;; Right or wrong?
	      ;; (if (,(red? 't) r)
	      ;; 	 (set! r ,(update 'r color: (black 't))))
	      (if/pure
	       (let ((t (update t left: r color: (black t))))
		 (if (pair? cont) ((car cont) t (vector-ref min 0)) t))
	       (begin
		 (update t left: r color: (black t))
		 (vector-ref min 0))))))
       (cond-define
	(t-delete t n/k . rest)
	(define (delete! set-leftmost! h)
	  (if (if/ordered
	       (t-k-<? n/k h)
	       (and (not (eq? n/k h)) (t-<? n/k h)))
	      (let-syntax
		  ((doit (syntax-rules ()
			   ((_ t n.n n.l n.r n.c)
			    (begin
			      (if (and (not (red? t n.l))
				       (not (ptred? t n.l left)))
				  (move-red-left! t n.n n.l n.r n.c))
			      (set! n.l (if (empty? t n.l) (empty t)
					    (delete! set-leftmost! n.l)))
			      (if/leftmost
			       (if (and set-leftmost! (empty? t n.l))
				   (set-leftmost! n.n))
			       #f)
			      (fixup! t n.n n.l n.r n.c))))))
		(with-n-node t h doit))
	      (let-syntax
		  ((doit (syntax-rules ()
			   ((_ t n.n n.l n.r n.c)
			    (begin
			      (if (red? t n.l)
				  (rotate-right! t n.n n.l n.r n.c))
			      (if (and (if/ordered (t-k-eq? n/k n.n) (eq? n/k n.n))
				       (empty? t n.r))
				  (set! n.n n.r)
				  (begin
				    (if (and (not (red? t n.r))
					     (not (ptred? t n.r left)))
					(move-red-right! t n.n n.l n.r n.c))
				    (if (if/ordered (t-k-eq? n/k n.n) (eq? n/k n.n))
					(let ((minv (vector #f)))
					  (set! n.r (delete-min t #f minv n.r))
					  (set! n.n (vector-ref minv 0)))
					(if (not (empty? t n.r))
					    (set! n.r (delete! #f n.r))))
				    (fixup! t n.n n.l n.r n.c))))))))
		(with-n-node t h doit))))
	(define set-leftmost! (and (pair? rest) (car rest)))
	(if (empty? t (root-node t)) t
	    (let ((r (delete! set-leftmost! (root-node t))))
	      #;(if (red? t r)
		  (set! r (update r color: (black t))))
	      (update t left: r color: (black t)))))
       )
     )))

#|
;; Test
(use srfi-1)
(define-record-type <property>
  (make-property color left right name value)
  property?
  (color property-color property-color-set!)
  (left property-left property-left-set!)
  (right property-right property-right-set!)
  (name property-name property-name-set!)
  (value property-value property-value-set!))

(define-syntax property-update
  (syntax-rules (left: right: color:)
    ((_ 1 n l r c ())
     (make-property c l r (property-name n) (property-value n)))
    ((_ 1 n l r c (left: v . more))
     (property-update 1 n v r c more))
    ((_ 1 n l r c (right: v . more))
     (property-update 1 n l v c more))
    ((_ 1 n l r c (color: v . more))
     (property-update 1 n l r v more))
    ((_ n . more)
     (property-update 1 n (property-left n) (property-right n) (property-color n) more))
    ))

(define-syntax property-k-n-eq?
  (syntax-rules () ((_ k n) (eq? k (property-name n)))))

(define-syntax property-k-n-lt
  (syntax-rules () ((_ k n) (string<? (symbol->string k) (symbol->string (property-name n))))))

(define-syntax property-n-n-lt
  (syntax-rules () ((_ node1 node2) (string<? (symbol->string (property-name node1))
					      (symbol->string (property-name node2))))))

(define-llrbtree/positional
  (ordered pure)
  property-update
  property-set-init!	           ;; defined
  property-lookup		   ;; defined
  #f				   ;; no min defined
  property-set-fold		   ;; defined
  property-set-for-each ;#f				   ;; no for-each defined
  property-node-insert!		   ;; defined
  property-delete!		   ;; defined
  #f				   ;; no delete-min defined
  property-set-empty?		   ;; defined
  property-k-n-eq?
  property-k-n-lt
  property-n-n-lt
  property-left
  property-left-set!
  property-right
  property-right-set!
  property-color
  property-color-set!
  #f)


(define pt (property-set-init! (make-property #f #f #f #f #f)))

(define pt2
  (fold
   (lambda (p pt)
     (property-node-insert! pt (car p) (make-property #f #f #f (car p) (cdr p))))
   pt
   '((one . 1)
     (two . 2)
     (three . 3))))

(property-set-for-each
 (lambda (n) (format #t "~a: ~a\n" (property-name n) (property-value n)))
 pt2)

|#
