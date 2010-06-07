;;;; network stuff
;;;; Charles Hoffman
;;;; University of Northern Iowa
;;;; Apr. 30, 2006
;-----------------------------------------------------------------------

;;; basic node class
(defclass node ()
  ((links
    :initform nil
    :reader   links
    :documentation
    "The links that this node possesses (or at least knows about)")
   (name
    :initarg  :name
    :initform nil
    :reader   name
    :documentation
    "An identifying name for this node (optional)")
   (home
    :initarg :home
    :initform nil
    :reader   home
    :documentation
    "A reference to the network this node is part of")))


(defmethod initialize-instance :after ((new-node node) &key link-points)
  (with-slots (name home) new-node
    (dolist (old-node link-points)
      (make-link home
                 new-node
                 (translate-nodearg (home new-node) old-node)))
    (if home
        (if (typep new-node (node-type home))
            (progn
              (push new-node (slot-value home 'nodes))
              (incf (slot-value home 'node-count))
              (conn-dist-inc home (link-count new-node))
              (if name
                  (symbol-macrolet
                      ((loc (gethash name (nodehash home))))
                    (if (null loc)
                        (setf loc new-node)
                      (error
                       "node name already in use on this network")))))
          (error "node type mismatch for this network")))))



;-----------------------------------------------------------------------

;;; basic link class -- non-directed (both ways) links.
(defclass link () 
  ((node1 
    :initarg   :node1
    :reader    node1
    :documentation
    "The node at one end of this link.")
   (node2
    :initarg   :node2
    :reader    node2
    :documentation
    "The node at the other end of this link.")
   (weight
    :initform  1
    :reader    weight
    :documentation
    "Weight of link.  This being a 'non-weighted'
     link, has a constant value of 1.")))


(defmethod home ((link link))
  (if (eq (home (node1 link)) (home (node2 link)))
      (home (node1 link))
      (error "home: mismatched home-slots of nodes on link")))

;;; Initialization for link
;;; takes care of adding a newly instantiated link to the links slots
;;; of the nodes specified as args to make-instance.
(defmethod initialize-instance :after ((new-link link) &key)
  ; actually this if is a hack so as to skip this initialization
  ; on directed-links.  Figure out a better deal for this later.
  (if (not (typep new-link 'directed-link))
      (progn
        (add-link-to-node (node1 new-link) new-link)
        (add-link-to-node (node2 new-link) new-link))))

;;; adds the given link to the given node.  Not to be used directly, but
;;; does check to make sure at least one end of the link matches this
;;; node.
(defmethod add-link-to-node ((node node) (link link))
  (if (or (eq node (node1 link))
          (eq node (node2 link)))
      (push link (slot-value node 'links))
      (error "add-link: node does not match either end of link")))

;;; takes care of adding a newly instantiated link to the links slot
;;; of the network where the nodes it links are located and updating
;;; its connectivity distribution (which only counts the tail end of
;;; directed links)
(defmethod add-link-to-node :after ((node node) (link link))
  (if (home link)
      (progn
        ; add link to network's link collection
        (push link (slot-value (home link) 'links))
        ; update connectivity distribution
        (conn-dist-inc (home link) (link-count node))
        (conn-dist-dec (home link) (1- (link-count node)))
        ; increment to "number of ends"
        (incf (slot-value (home link) 'nends)))))

;;; remove the given link from the given node.  Not to be used directly,
;;; but does check to make sure the given link is found at the node.
(defmethod remove-link-from-node ((link link) (node node))
  (delete link (slot-value node 'links)))

;;; updated the network after removing a link
(defmethod remove-link-from-node :after ((link link) (node node))
  (if (home link)
      (progn
        (delete link (slot-value (home link) 'links))
        (conn-dist-inc (home link) (link-count node))
        (conn-dist-dec (home link) (1+ (link-count node)))
        (decf (slot-value (home link) 'nends)))))


;;; some basic subclassen
;-----------------------------------------------------------------------
;; weighted link
(defclass weighted-link (link)
  ((weight
    :initform 1
    :initarg  :weight
    :accessor weight
    :documentation
    "Weight of this link.  Defaults to 1 but can be specified at
     instantiation or setf'ed at any later time.")))



;-----------------------------------------------------------------------
;; directed link
(defclass directed-link (link)
  ((node1 
    :initarg   :tail
    :initarg   :node1
    :reader    tail
    :reader    node1
    :documentation
    "The tail (source) of this link.  'node1' is kept as a reader for
     this, as well as tail, for compatibility with general link
     methods.")
   (node2
    :initarg   :head
    :initarg   :node2
    :reader    head
    :reader    node2
    :documentation
    "The head (destination) of this link.  'node2' is kept as a reader
     for this, as well as head, for compatibility with general link
     methods.")))


;; initialization for directed-link.
;; Only the tail node will have a reference to this link, so that in
;; normal path-finding operations the link will only be followed in
;; the proper direction.  If one needs to find the incoming
;; directed-links to a node, one will just have to search the network
;; for them.  So there.
(defmethod initialize-instance :after ((new-link directed-link) &key)
  (add-link-to-node (tail new-link) new-link))

(defmethod add-link-to-node ((node node) (link directed-link))
  ;(format t "adding ~a to ~a~%" link node)
  (if (eq node (tail link))
      (push link (slot-value node 'links))
      (error "add-link: node does not match tail of directed-link")))



;-----------------------------------------------------------------------

(defclass network ()
  ((node-type
    :initarg  :node-type
    :initform 'node
    :reader   node-type)
   (link-type
    :initarg  :link-type
    :initform 'link
    :reader   link-type)
   (nodes
    :initform nil
    :reader   nodes)
   (node-count
    :initform 0
    :reader   node-count)
   (nodehash
    :initform (make-hash-table :test #'equal)
    :reader   nodehash)
   (links
    :initform nil
    :reader   links)
   (conn-dist
    :initform (make-hash-table :test #'=)
    :reader   conn-dist)
   (conn-dist-max
    :initform 0
    :reader   conn-dist-max)
   (nends
    :initform 1)
   (path-hash
    :initform (make-hash-table :test #'equal)
    :reader   path-hash)))


;;; can be used to ensure that the node-count slot
;;; is correct.
(defmethod count-nodes ((net network)) 
  (setf (slot-value net 'node-count) (length (nodes net))))

;;; fetch a node by its name
(defmethod get-node-by-name ((net network) name)
  (gethash name (nodehash net)))

;;; fetch a node by it's order (reversed index of its position in nodes
;;; list)
(defmethod get-node-by-order ((net network) n)
  (elt (nodes net) (- (- (node-count net) 1) n)))

;;; utility function to translate a node-argument to a node
;;; first tries to see if arg is found as a node's name in node-hash
;;; if not, then if arg is an int it tries indexing (nodes net) by it
;;; (which will error if arg is out out range)
;;; finally just returns the original arg
;;; under the assumption that it's already an actual node
(defmethod translate-nodearg ((net network) arg)
  (if (typep arg 'node) arg
    (or (get-node-by-name net arg)
        (if (integerp arg) 
            (get-node-by-order net arg)
            (error "translate-nodearg: node specifier not found")))))

;--------------------------------------------
;;; methods for the connectivity distribution
(defmethod conn-dist-ref ((net network) n)
  (gethash n (slot-value net 'conn-dist) 0))

(defmethod conn-dist-inc ((net network) n &optional (by 1))
  (if (> n (conn-dist-max net))
      (setf (slot-value net 'conn-dist-max) n))
    (setf (gethash n (slot-value net 'conn-dist))
          (+ (conn-dist-ref net n) by)))

(defmethod conn-dist-dec ((net network) n &optional (by 1))
  (conn-dist-inc net n (- by)))

;;; re-count the entire connectivity distribution
;;; and average connectivity
(defmethod conn-dist-refresh ((net network))
  (setf (slot-value net 'conn-dist) (make-hash-table :test #'=))
  (let ((nends 0))
    (dolist (node (nodes net))
      (conn-dist-inc net (link-count node))
      (incf nends (link-count node))
    (setf (slot-value net 'nends) nends))))

;-----------------------------------------------------

;------------ "public" stuff


(defmethod link-count ((node node))
  (length (links node)))



;;; print of a node with its links and the nodes they link to
(defmethod print-node ((node node))
  (format t "~a (~a)~%" (name node) node)
  (dolist (link (links node))
    (format t "     ~a -> ~a (~a)~%" 
	    link (name (other-end node link)) (other-end node link))))



(defmethod make-node ((net network) &key link-points name)
  (make-instance (node-type net)
                 :name        name
                 :home        net
                 :link-points link-points))



(defmethod remove-node ((node node))
  (dolist (each-link (slot-value node 'links))
    (remove-link each-link))
  (if (home node)
      (progn
        (if (name node)
            (remhash (name node) (slot-value (home node) 'nodehash)))
        (delete node (slot-value (home node) 'nodes)))))



(defmethod node-index ((node node) (net network))
  (position node (slot-value net 'nodes)))



(defmethod pretty ((node node))
  (or (name node) (node-index node (home node)) node))
  


;;; returns an existing link from this-node to other-node or nil.
(defmethod find-link-to ((this-node node) (that-node node))
  (find that-node (links this-node)
        :key #'(lambda (link)
                 (other-end this-node link))))



;;; what node is on the other end of this link? returns nil if this link
;;; doesn't go with this node at all.
(defmethod other-end ((node node) (link link))
  (cond ((eq node (node1 link)) (node2 link))
	((eq node (node2 link)) (node1 link))))



;;; find a link from node1 to node2 and delete it.  Should work for
;;; directed or non-directed links (for directed links, arguments
;;; should be in the order tail, head.  The removed link is returned
;;; so as to check whether such a link was actually found.
(defmethod remove-link-from ((node1 node) (node2 node))
  (let ((link (find-link-to node1 node2)))
    (if link (remove-link link))
    link))



;;; instantiates a link between two nodes
;;; only if no such link already exists.  Only checks for links
;;; FROM node1 TO node2, so should work properly with directed links.
(defmethod conditional-link ((node1 node)
                             (node2 node)
                             &optional (link-type 'link))  
  (if (not (find-link-to node1 node2)) 
      (make-instance link-type :node1 node1 :node2 node2)
      nil))



;;; adds a link from node1 to node2 on the
;;; given network (using whatever link-type is specified for it)
(defmethod make-link ((net network) (node1 node) (node2 node))
  (conditional-link node1 node2 (link-type net)))



;;; Same as above, but located nodes by name of order.
(defmethod make-link ((net network) node1 node2)
  (make-link net 
             (translate-nodearg net node1)
             (translate-nodearg net node2)))



;;; remove a link from its nodes and from the network
(defmethod remove-link ((link link))
  (remove-link-from-node (node1 link) link)
  (remove-link-from-node (node2 link) link))

(defmethod remove-link ((link directed-link))
  (remove-link-from-node (tail link) link))



;;; print the state of a network, using node names
(defmethod print-network ((net network))
  (dotimes (i (node-count net))
    (let ((node (get-node-by-order net i)))
      (format t "[~a]: " i)
      ;reverse of list order
      (format t "~a  " node)
      (if (name node) (format t "\"~a\"" (name node)))
      (format t "~%")
      (dolist (link (links node))
        (if (name (other-end node link))
            (format t "     -> \"~a\"~%" (name (other-end node link)))
            (format t "     -> ~a~%"     (other-end node link)))))))



;;; returns a list of lists comprising the nodes along all the
;;; paths (in reverse order) between the two argument nodes
;;; (presuming they are in the same network -- network itself not 
;;; required as an argument)
(defmethod collect-paths ((start-node node)
                          (end-node node)
                          &optional so-far found)
  (let ((try
	 (remove-if
	  #'(lambda (link)
              (member (other-end start-node link) so-far))
	  (links start-node))))
    (cond ((eq start-node end-node)
	   (cons (cons end-node so-far) found))
	  ((null try) found)
	  (t (progn
	       (dolist (link try)
		 (setf found
		       (append
			(collect-paths (other-end start-node link)
                                       end-node
                                       (cons start-node so-far)
				       found))))
	       found)))))



;;; length of the shortest path netween two nodes in a network -
;;; simple version - returns the length of the shortest path returned
;;; by collect-paths
(defmethod shortest-path-length (node1 node2 (net network))
  (apply #'min 
         (mapcar #'length
                 (collect-paths (translate-nodearg net node1)
                                (translate-nodearg net node2)))))



;;; choose one random node from a network
(defmethod pick-node ((net network) &optional exclude)
  (let ((random-node (elt (nodes net) (random (node-count net))))
        (exclude     (mapcar #'(lambda (node)
                                 (translate-nodearg net node))
                             exclude)))
    (if (member random-node exclude)
        (pick-node net exclude)
        random-node)))



;;; choose a set of n nodes from a network
;;; (a list of nodes to leave out is optional)
;;; returns the set as a list of nodes
(defmethod pick-nodeset ((net network) n &optional exclude)
  (if (zerop n)
      '()
      (let ((added   (pick-node net exclude))
            (exclude (mapcar #'(lambda (node)
                                 (translate-nodearg net node))
                             exclude)))
        (cons added
              (pick-nodeset net (- n 1) (cons added exclude))))))



(defmethod conn-dist-print ((net network))
  (dotimes (i (1+ (conn-dist-max net)))
    (format t "~a~5t|~a~%" i (conn-dist-ref net i))))



(defmethod conn-avg ((net network))
    (with-slots (nends node-count) net
      (if (zerop node-count) 0 (/ nends node-count))))


(defmethod path-table-print ((net network))
  (maphash 
   #'(lambda (pair path)
       (format t "[~a -> ~a]: ~a~%"
               (pretty (car pair)) (pretty (cadr pair))
               (mapcar #'pretty path)))
   (slot-value net 'path-hash)))



;;; returns a function to find a node in the given network by name or
;;; serial number.
(defmethod find-function ((net network))
  #'(lambda (id) (translate-nodearg net id)))




;-----------------------------------------------------------------------

;;; some simple test networks, useful for testing stuff on a small scale
;;; as well as for demonstration purposes (i.e. my presentation!)

;;; 1 0 5
;;; |\|/|\
;;; | 6 | 3
;;; |/ \|/
;;; 2   4

(defun simplegraph ()
  (let ((graph (make-instance 'network)))
    (flet ((find (find-function graph)))
      (dotimes (n 7)
        (make-node graph :name n))
      (make-link graph 0 6)
      (make-link graph 1 2)
      (make-link graph 1 6)
      (make-link graph 2 6)
      (make-link graph 3 4)
      (make-link graph 3 5)
      (make-link graph 4 5)
      (make-link graph 4 6)
      (make-link graph 5 6)
      graph)))


;;; a simple directed version, having all the same links in both
;;; directions except between 6 and 1 which is directed from 6 to 1
;;; only
(defun simpledirgraph ()
  (let ((graph (make-instance 'network
                              :link-type 'directed-link)))
    (flet ((find (find-function graph)))
      (dotimes (n 7)
        (make-node graph :name n))
      (make-link graph 0 6)
      (make-link graph 6 0)
      
      (make-link graph 1 2)
      (make-link graph 2 1)
      
      (make-link graph 6 1) ; doesn't have the matching 1->6 link! 
      
      (make-link graph 2 6)
      (make-link graph 6 2)
      
      (make-link graph 3 4)
      (make-link graph 4 3)
      
      (make-link graph 3 5)
      (make-link graph 5 3)
      
      (make-link graph 4 5)
      (make-link graph 5 4)
      
      (make-link graph 4 6)
      (make-link graph 6 4)
      
      (make-link graph 5 6)
      (make-link graph 6 5)
      graph)))


(defun simpleweightgraph ()
  (let ((graph (make-instance 'network
                              :link-type 'weighted-link)))
    (flet ((find (find-function graph)))
      (dotimes (n 7)
        (make-node graph :name n))
      (setf (weight (make-link graph 0 6)) 2)
      (setf (weight (make-link graph 1 2)) 1)
      (setf (weight (make-link graph 1 6)) 1)
      (setf (weight (make-link graph 2 6)) 1)
      (setf (weight (make-link graph 3 4)) 1)
      (setf (weight (make-link graph 3 5)) 1)
      (setf (weight (make-link graph 4 5)) 1)
      (setf (weight (make-link graph 4 6)) 1)
      (setf (weight (make-link graph 5 6)) 1)
      graph)))


;-----------------------------------------------------------------------
; this algorithm could discover the shortest paths to all other nodes
; from a brand new node, but doesn't take into account any paths it made
; shorter by springing forth into existence.
;        (dolist (some-node (nodes net))
;          (let ((current-path 
;                 (path-table-ref (home link)
;                           this-node
;                           some-node))
;                (continuation
;                 (path-table-ref (home link)
;                           (other-end node link)
;                           some-node)))
;            (if (eq continue 'none)
;                (path-table-set (home link) this-node some-node 'none)
;                (let ((new-path (cons this-node continue)))
;                  (if (or (eq current-path 'none)
;                          (< (length (new-path))
;                             (length current-path)))
;                      (path-table-set (home link)
;                                    this-node
;                                    some-node
;                                    new-path)))))))))
;-----------------------------------------------------------------------


; let's do us up some algorithms!

;;; creates a 2D array of weight/length/capacity/whatever of immediate
;;; links between any pairs of nodes that have immediate links (and
;;; the symbol 'none when they don't).
(defmethod link-matrix ((net network) &optional slot-name)
  (let ((w (make-array (list (node-count net) (node-count net)))))
    (dotimes (i (node-count net))
      (dotimes (j (node-count net))
        (let* ((node1 (get-node-by-order net i))
               (node2 (get-node-by-order net j))
               (link  (find-link-to node1 node2)))
          (setf (aref w i j)
                (if (eq node1 node2)
                    0
                    (if link 
                        (if slot-name (slot-value link slot-name) 1)
                        'none))))))
    w))


;;; returns a 2d array of the shortest-path-lengths
;;; using Floyd's algorithm
(defmethod floyd ((net network) &optional slot-name)
  (let ((d (link-matrix net slot-name)))
    (dotimes (k (node-count net))
      (dotimes (i (node-count net))
        (dotimes (j (node-count net))
          (setf (aref d i j)
                (cond ((or (eq 'none (aref d i k))
                           (eq 'none (aref d k j)))
                       (aref d i j))
                      ((eq 'none (aref d i j))
                       (+ (aref d i k)
                          (aref d k j)))
                      (t (min (aref d i j)
                              (+ (aref d i k)
                                 (aref d k j)))))))))
  d))


(defmethod floyd-and-average ((net network) &optional slot-name)
  (let ((d (floyd net slot-name))
        (total 0))
    (dotimes (i (node-count net))
      (dotimes (j (node-count net))
        (let ((x (aref d i j)))
          (incf total (if (eq x 'none) 0 x)))))
    (values d (/ total (* (node-count net) (1- (node-count net)))))))
        


;;; finds cliques of size k in a network (non-directed links only)
;;; using a backtracking algorithm described by Hyung-Joon Kim,
;;; University of Washington
;;; http://zoony23.cafe24.com/school/clique_algorithm.html

;;; helper methods:

;;; cut the tail of lst starting at elt
(defun cut-from (elt lst)
  (cond ((eq elt (car lst)) nil)
        (t (cons (car lst) (cut-from elt (cdr lst))))))


;;; get candidates for extending clique
;;; (helper method for find-k-cliques)
(defmethod get-candidates ((net network) cliquelist)
  (let ((candidates nil))
    (if (null cliquelist)
        (dolist (n (nodes net))
          (push (list n) candidates))
        (dolist (n (cut-from (car cliquelist) (nodes net)))
          (if (can-extend-clique n cliquelist)
              (push (cons n cliquelist) candidates))))
    candidates))


;;; if cliquelist is a list of nodes in a clique, returns whether
;;; node connects to all nodes in clique (and thus can extend into
;;; an n+1-clique)
(defmethod can-extend-clique ((node node) cliquelist)
  (or (null cliquelist) 
      (and (find-link-to node (car cliquelist))
           (can-extend-clique node (cdr cliquelist)))))


;;; and now, the clique-finder itself:
(let ((found-cliques nil))
  (defmethod find-k-cliques ((net network) k
                             &optional (cliquelist nil) (j 0))
    (if (zerop j) (setf found-cliques nil))
    (if (= j k)
          (push cliquelist found-cliques)
      (dolist (a (get-candidates net cliquelist))
        (append (find-k-cliques net k a (1+ j)) found-cliques)))
    found-cliques))


;-----------------------------------------------------------------------
; --------------------- the basic scale-free model ---------------------

(defclass sfnet (network) ())


;;; start the network out with two nodes and a link between them
(defmethod initialize-instance :after ((net sfnet) &key)
  (make-node net)
  (make-node net)
  (make-link net 0 1)
  net)


;;; advance the model one iteration 
;;; by adding a node with two links to existing nodes
(defmethod tick ((net sfnet))
  (make-instance (node-type net)
                 :home        net
                 :link-points (pick-nodeset net 2))
  net)


;;; override of pick-node for scale-free model.  implements preferential
;;; attachment by first choosing a node from the network, then randomly
;;; choosing the node at either end with 50-50 probability.  By this
;;; method, it is believed that the chance of a node being chosen is
;;; proportional to its connectivity.
(defmethod pick-node ((net sfnet) &optional exclusions)
  (let ((random-link
         (elt (links net) (random (length (links net))))))
    (let ((choose
           (if (zerop (random 2))
               (node1 random-link)
               (node2 random-link))))
      (if (member choose exclusions)
          (pick-node net exclusions)
          choose))))



;;; scale-free test
(defun sftest (n)
  (let ((s (make-instance 'sfnet)))
    (dotimes (k n) (tick s))
  s))

;-----------------------------------------------------------------------

;;; for friendlier display

;;; convert a list of nodes to a list of their names
(defun namelist (nodelist) (mapcar #'name nodelist))

;;; same as collect-paths, but represented by their names in network
(defun name-lists (lists)
   (mapcar #'namelist lists))


