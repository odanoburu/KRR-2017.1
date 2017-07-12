;; assumes well-formed input (and (and A) B) is not valid, as (and is
;; a more than unary predicate.

;; main entry points are is-sat, tableau, and tableau-kb. you can also
;; run sbcl --noinform --load tableau.fasl --eval "(progn (to-graphviz
;; 'formula (sb-ext:quit))" | dot -T png -o tree.png

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

(defun conslist(elist a-list)
  "(cons elist a-list) if elist is a list, else (cons (list elist)
a-list)."
  (if (is-atom elist)
      (cons (list elist) a-list)
      (progn (assert (listp elist)) (cons elist a-list))))

(defun consapp(elist a-list)
  "(cons elist a-list) if elist is atom, else append the two."
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

;;;;;entry;;;;;
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
	     (:print-function ;this will make node printing only print
			      ;its #formula
	      (lambda (node stream k)
		(null k)  ;ignoring the second argument k (level)
		(format stream "#~A" (node-formula node)))))
  "structures nodes."
  (formula)
  (parent nil)
  (visited nil))

(defun node-is-atomic(node)
  "checks if node's formula is atomic"
  (if (is-atom (node-formula node))
      node
      nil))

(defun norm(clause)
  "applies (to-nnf (normalize clause))."
  (to-nnf (normalize clause)))

(defun KB-to-nnf(KB)
  "takes a KB (list of clauses) and returns their conjunction in NNF."
  (if (equal (length kb) 1)
      (progn (assert (is-atom (first kb))) (norm (first kb)))
      (cons 'and (mapcar #'norm kb))))

(defun push-formula(a-list formula-var)
  "takes a list/stack and cons/snoc'es formulas according to their
type (ands and atoms are prioritized over ors)."
  (let ((formula (node-formula formula-var)))
    (cond ((is-atom formula)
	   (progn (setf (node-visited formula-var) T)
		    (cons formula-var a-list)))
	  ((modifier-is formula 'and) (cons formula-var a-list))
	  ((modifier-is formula 'or) (snoc formula-var a-list)))))

(defun push-formulas(a-list formulas)
  "wraps push formula to push several formulas."
  (if (endp formulas)
      a-list
      (push-formulas (push-formula a-list (first formulas)) (rest formulas))))

(defun and-tree(parent-var parent-formula &optional children)
  "takes the formulas in (rest (and f1 f2 ... fn)) and makes nodes
with the first having parent-var as parent and the rest having the
previous node as parent. returns the leaf and the children's formulas,
ordered by atoms/ands|ors"
  (if (endp parent-formula)
      (values parent-var (mapcar #'node-formula (remove-if #'node-is-atomic children)))
      (let ((child-var (gensym)))
	(setf child-var (make-node :formula (first parent-formula) :parent parent-var))
	(and-tree child-var (rest parent-formula) (cons child-var children)))))

(defun or-tree(parent-var parent-formula &optional children)
  "takes the formulas in (rest (or f1 f2 ... fn)) and makes nodes,
each having parent-var as parent. returns the nodes made (leaves), and
their formulas."
  (if (endp parent-formula)
      (values children (mapcar #'node-formula (remove-if #'node-is-atomic children)))
      (let ((child-var (gensym)))
	(setf child-var (make-node :formula (first parent-formula) :parent parent-var))
	(or-tree parent-var (rest parent-formula) (cons child-var children)))))

(defun tableaulify(leaves formulas)
  "receives formulas in a list and nodes (leaves), and recursively
calls make-tree with each leaf as parent-node"
  (if (endp formulas)
      leaves
      (let ((formula (first formulas)))
	(mapcar #'(lambda (leaf)
		    (multiple-value-bind (new-leaves children) (make-tree leaf formula)
		      (tableaulify new-leaves (push-formulas (rest formulas) children)))) leaves))))
;;ver history
;;multiple bindings so that and-tree returns leaf and non atomic children, leaf goes to leaves and the children go to nodes with push formula

(defun make-tree(parent-var parent-formula)
  "makes nodes from parent-formula with parent-var as
parent (parent-var not necessarily maps to parent-formula, as in
tableau the parent might not be the 'biological' parent)."
  (cond ((is-atom parent-formula) (values parent-var nil))
	((modifier-is parent-formula 'and)
	 (and-tree parent-var (rest parent-formula)))
	((modifier-is parent-formula 'or)
	 (or-tree parent-var (rest parent-formula)))))

(defun tableau(formula)
  "takes a formula creates the root node, calls make-tree, then
draw-tree, to return list of branches."
  (let ((queried-formula (norm formula))
	(root-var (gensym)))
    (setf root-var (make-node :formula queried-formula :visited T))
    (draw-tree (make-tree root-var queried-formula))))

(defun tableau-kb(KB query)
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
	(return-from find-clash-branch formula)))
    (if (< (length test-branch) 1)
	nil
	(find-clash-branch (rest atomic-branch)))))

(defun find-clash-tree(tree)
  "takes a list of branches/tree (list of lists of formulas) as input,
returns the clashes found in the same order."
  (let ((clashes nil))
    (dolist (branch tree)
      (setf clashes (cons (find-clash-branch branch) clashes)))
    (reverse clashes)))

(defun is-sat(formula)
  "returns t if formula is satisfied is satisfatible, else nil"
  (let ((clashes (find-clash-tree (mapcar #'atomic-branch (tableau formula))))) ;removes non-atomic formulas to speed up computation
    (if (null (member nil clashes)) ;if there are only clashes this is t
	nil
	t)))

;;;;;;;;;;;;;;;;;;
;;;;;graphviz;;;;;
(defun duplicate-invert-clashes(clashes &optional result)
  "takes a list of clashes, removes nils, and outputs a list with each
clash followed by its negation"
  (let ((clash (first clashes)))
    (if (null clashes)
	result
	(duplicate-invert-clashes (rest clashes)
				  (cons clash (cons (invert-signal clash) result))))))

(defun to-graphviz(formula)
  "plot tableaux derived from (and KB (not query) and their clashes."
  (let* ((tree (tableau formula))
	(clashes (find-clash-tree (mapcar #'atomic-branch tree))))
    (princ "strict graph G { node[shape=\"underline\"];")
    (princ (format nil "~{~{\"~<~S~>\"~^ -- ~};~%~}" tree))
    (princ (format nil "~{\"~<~S~>\"~^ -- \"~<~S~>\"[color=\"red\"];~% ~}" (duplicate-invert-clashes (remove-if #'null clashes))))
    (princ "}")
    (values)))

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
