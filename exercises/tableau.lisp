;;; current development: clash

;;;;;;;;;;;;;;;
;;;;;utils;;;;;
(defun snoc(an-atom a-list)
  "cons to end of list."
  (reverse (cons an-atom (reverse a-list))))

(defun is-atom(clause)
  "returns nil if clause is not an atom, which includes lisp's atoms
and their negations."
  (cond ((atom clause) clause)
	((and (modifier-is clause 'not) (atom (cadr clause)) (equal (length clause) 2)) clause)
	(t nil)))

(defun modifier-is(clause modifier)
  "checks if the operator of clause is modifier."
  (equal (first clause) modifier))

(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))

(defun conslist(elist a-list)
  "cons elist a-list if elist is a list, else cons (list elist) a-list."
  (if (is-atom elist)
      (cons (list elist) a-list)
      (progn (assert (listp elist)) (cons elist a-list))))

(defun consapp(elist a-list)
  "cons elist a-list if elist is atom, else append the two."
  (if (is-atom elist)
      (cons elist a-list)
      (append elist a-list)))

(defun invert-signal(formula)
  "inverts signal of atomic formula"
  (assert (is-atom formula))
  (if (atom formula)
      (norm-aux-negate formula)
      (cadr formula)))

;;;;;;;;;;;;;
;;;;;NNF;;;;;
(defun nnf-not(clause)
  "argument is (rest (not clause)). this function applies NNF to a
clause."
  (cond ((and (equal (length clause) 1) (eql t (first clause))) nil)
	((and (equal (length clause) 1) (equal nil (first clause))) t)
	((and (equal (length clause) 1) (atom (first clause))) (cons 'not clause))
	((modifier-is clause 'not) (cadr clause))
	((modifier-is clause 'or) (nnf-or (rest clause)))
	((modifier-is clause 'and) (nnf-and (rest clause)))
	((modifier-is clause 'some) (nnf-some (rest clause)))
	((modifier-is clause 'only) (nnf-only (rest clause)))
	(t nil)))

(defun nnf-aux-negate(clause)
  "returns negated clause, already calling nnf-not when appropriate."
  (assert (not (and (not (atom clause)) (equal (length clause) 1))))
  (if (atom clause) (list 'not clause) (nnf-not clause)))

;;;;;rules;;;;;
(defun nnf-or(clause)
  "applies nnf rule for or."
  (assert (>= (length clause) 2))
  (cons 'and (mapcar #'nnf-aux-negate clause)))

(defun nnf-and(clause)
  "applies nnf rule for and."
  (assert (>= (length clause) 2))
  (cons 'or (mapcar #'nnf-aux-negate clause)))

(defun nnf-some(clause)
  "format is (some role (concept)). applies nnf rule for some (∃)."
  (assert (equal (length clause) 2))
  (cons 'only (list (first clause) (nnf-aux-negate (second clause)))))

(defun nnf-only(clause)
  "format is (only role (concept)). applies nnf rule for only (∀)."
  (assert (equal (length clause) 2))
  (cons 'some (list (first clause) (nnf-aux-negate (second clause)))))

(defun aux-to-NNF(clause)
  "auxliary function to to-nnf."
  (mapcar #'to-NNF clause))

(defun to-NNF(clause)
  "turns a clause into NNF."
  (cond ((is-atom clause) clause)
	((modifier-is clause 'not) (to-NNF (nnf-not (cadr clause))))
	(t (aux-to-NNF clause)))) ;;can implement check here

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;normalization;;;;;
(defun norm-implies(clause)
  "argument is (rest (implies clause))."
  (assert (equal (length clause) 2)) ;binary relation
  (cons 'or (list (list 'not (first clause)) (second clause))))

(defun norm-equiv(clause)
  "argument is (rest (equiv clause)). turns equivalence (≡) into and's
and or's"
  (assert (>= (length clause) 2))
  (cons 'or (list (cons 'and (mapcar #'norm-aux-negate clause)) (cons 'and clause))))

(defun norm-aux-negate(clause)
  "negates clause."
  (assert (not (and (not (atom clause)) (equal (length clause) 1))))
  (list 'not clause))

(defun aux-normalize(clause)
  "auxiliary function to normalize."
  (mapcar #'normalize clause))

(defun normalize(clause)
  "turns a clause with implication and equivalence into something that
uses only and's or's & not's."
  (cond ((is-atom clause)
	  clause)
	((modifier-is clause 'equiv)
	  (normalize (norm-equiv (rest clause))))
	((modifier-is clause 'implies)
	  (normalize (norm-implies (rest clause))))
	(t
	 (aux-normalize clause))))

;;;;;;;;;;;;;;;;;
;;;;;tableau;;;;;

(defstruct (node
	     (:print-function
	      (lambda (node stream k)
		(null k)  ;ignoring the second argument k (level)
		(format stream "~A" (node-formula node)))))
  "structures nodes."
  (formula)
  (parent nil)
  (visited nil))

(defun norm(clause)
  "applies (to-nnf (normalize clause))."
  (to-nnf (normalize clause)))

(defun KB-to-nnf(KB)
  "takes a KB (list of clauses) and returns their conjunction in NNF."
  (if (equal (length kb) 1)
      (progn (assert (is-atom (first kb))) (norm (first kb)))
      (cons 'and (mapcar #'norm kb))))

(defun and-tree(parent-var parent-formula)
  "takes the formulas in (rest (and f1 f2 ... fn)) and makes nodes
with the first having parent-var as parent and the rest having the
previous node as parent. returns (cons leaf children), ordered by
atoms/ands|ors"
  (let ((children nil)
	(leaf nil))
    (dolist (child-formula parent-formula)
      (let ((child-var (gensym)))
	(setf child-var (make-node :formula child-formula :parent parent-var))
	(setf parent-var child-var)
	(setf children (push-formula children child-var))
	(setf leaf child-var)))
    (cons leaf children)))

(defun or-tree(parent-var parent-formula)
  "takes the formulas in (rest (or f1 f2 ... fn)) and makes nodes,
each having parent-var as parent. returns the nodes made, ordered by
atoms|ands|ors."
  (let ((children nil))
    (dolist (child-formula parent-formula)
      (let ((child-var (gensym)))
	(setf child-var (make-node :formula child-formula :parent
	parent-var))
	(setf children (push-formula children child-var))))
    children))

(defun push-formula(a-list formula-var)
  "takes a list/stack and cons/snoc'es formulas according to their
type (ands and atoms are prioritized over ors)."
  ;;um átomo depois de or é uma leaf? se conseguir fazer todos os ands
  ;;antes de ors, sim.
  (let ((formula (node-formula formula-var)))
    (cond ((is-atom formula)
	   (progn (setf (node-visited formula-var) T)
		    (cons formula-var a-list)))
	  ((modifier-is formula 'and) (cons formula-var a-list))
	  ((modifier-is formula 'or) (snoc formula-var a-list)))))
  
(defun make-tree(parent-var parent-formula)
  "makes nodes from parent-formula with parent-var as
parent (parent-var not necessarily maps to parent-formula, as in
tableau the parent might not be the 'biological' parent)"
  (cond ((is-atom parent-formula) parent-var)
	((modifier-is parent-formula 'and)
	 (and-manage-tree (and-tree parent-var (rest parent-formula))))
	((modifier-is parent-formula 'or)
	 (or-manage-tree (or-tree parent-var (rest parent-formula))))))

(defun and-manage-tree(nodes)
  "takes the children made by and-tree and recursively calls maketree
on them if they haven't been visited yet"
  (let ((leaf (first nodes)) ; first element of nodes provided by
			     ; and-tree has the a copy of the leaf
	(children nil))
    (dolist (node (rest nodes))
      (when (null (node-visited node)) ;ignore atoms, because they are
				       ;not lost if they are leaf
	(setf (node-visited node) T) (setf
	children (consapp (make-tree leaf (node-formula node))
	children))))
    (if (null children)
	leaf
	children)))

(defun or-manage-tree(nodes)
  "takes the children made by or-tree and recursively calls make-tree
on them if they haven't been visited yet."
  (let ((children nil))
    (dolist (node nodes)
;      (when (null (node-visited node)) ;can't ignore atoms, because
;      they can be leaves
	(setf (node-visited node) T)
	       (setf children (consapp (make-tree node (node-formula
	       node)) children)));)
    (if (null children)
	nodes
	children)))

(defun tableau(KB query)
  "takes a list of formulas and a query, negates the query, creates
the root node, calls make-tree, then draw-tree, to return list of
branches."
  (let ((queried-kb (kb-to-nnf (snoc (list 'not query) kb)))
	(kb-var (gensym)))
    (setf kb-var (make-node :formula queried-kb :visited T))
    (draw-tree (make-tree kb-var queried-kb))))

(defun draw-branch(node &optional branch)
  "takes a node (leaf) as input and goes up its parents,
returning a list of their formulas in generational order"
  (let ((parent (node-parent node)))
    (if (null parent)
	(cons (node-formula node) branch)
	(draw-branch parent (cons (node-formula node) branch)))))

(defun draw-tree(nodes &optional tree)
  "takes a list of nodes (leaves) and goes up to their parents drawing
the tree they form."
  (if (null nodes)
      tree
      (draw-tree (rest nodes) (cons (draw-branch (first nodes)) tree))))
#|
(defun node-is-atomic-formula(node)
  "returns the node if node has atomic formula, else returns nil."
  (let ((formula (node-formula node)))
    (if (is-atom formula)
	node
	nil)))

(defun atomic-formulas-in-branch(branch)
  "returns all the atomic formulas of a list/branch"
  (let ((atomic-nodes (remove-if-not #'node-is-atomic-formula branch)))
    (mapcar #'node-formula atomic-nodes)))
|#

(defun atomic-branch(branch)
  "takes a list of formulas and returns the atomic ones."
  (remove-if-not #'is-atom branch))

(defun find-clash-branch(atomic-branch)
  "takes formulas as input, not nodes. a clash can only occur between
two atoms."
  (let ((test-atom (invert-signal (first atomic-branch)))
	(test-branch (rest atomic-branch)))
    (dolist (formula test-branch)
      (when (equal test-atom formula)
	(return-from find-clash formula)))
    (if (equal (length test-branch) 1)
	nil
	(find-clash (rest atomic-branch)))))

;;;;;;;;;;;;;;;;;;
;;;;;graphviz;;;;;
(defun to-graphviz(KB query)
  (princ "strict graph G { node[shape=\"underline\"];")
  (princ (format nil "~{~{\"~<~S~>\"~^ -- ~};~%~}" (tableau KB
  query)))
  (princ "}")
  (values))

;;;;;;;;;;;;;;;
;;;;;tests;;;;;
(defun stupid-tests()
  "because I don't know yet how to do proper testing in CL"
  (assert (equal '(OR (NOT A) (NOT B)) (nnf-and (rest '(and A B)))))
  (assert (equal '(NOT A) (nnf-aux-negate 'A)))
  (assert (equal '(OR (NOT A) (NOT B)) (nnf-aux-negate '(and A B))))
  (assert (equal '(AND (NOT A) (NOT B)) (nnf-or (rest '(or A B)))))
  (assert (equal '(OR (NOT A) B) (norm-implies (rest '(implies A B)))))
  (assert (equal '(OR (AND (NOT A) (NOT B)) (AND A B)) (norm-equiv (rest '(equiv A B)))))
  (assert (equal '(ONLY C (OR (NOT D) (NOT E))) (nnf-some (rest '(some C (and D E))))))
  (assert (equal '(SOME C (AND (NOT E) (NOT F))) (nnf-only (rest '(only C (or E F))))))
  (assert (equal '(OR (ONLY R (OR (NOT A) B)) (SOME R (AND A B))) (to-nnf '(or (not (some r (and A (not B)))) (not (only r (or (not A) (not (not (not B))))))))))
  (assert (equal '((OR (NOT A) (AND (OR B E) (OR (NOT B) (NOT E)))) (AND C (NOT D))) (KB-to-nnf '((not (and a (equiv b e))) (not (or (not c) d))))))
  ;;(assert (equal '(AND (AND C (NOT D)) E C (NOT D)) (tableau-and (rest '(and C (not D))) '(and (and C (not D)) E))))
  (assert (equal '(OR (AND (NOT (OR (NOT A) B)) (NOT (OR (NOT A) B)) (NOT (OR (NOT (NOT B)) (NOT A)))) (AND (OR (NOT A) B) (OR (NOT A) B) (OR (NOT (NOT B)) (NOT A)))) (normalize '(equiv (implies a b) (or (not a) b) (implies (not b) (not a))))))
  t)
