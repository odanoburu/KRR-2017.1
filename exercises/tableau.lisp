;;;;;;;;;;;;;;;
;;;;;utils;;;;;
(defun snoc(an-atom a-list)
  "cons to end of list."
  (reverse (cons an-atom (reverse a-list))))

(defun cons-mod(modifier clause)
  ""
  (if (null modifier)
      clause
      (cons modifier clause)))

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

(defun list-mod(clause a-list)
  ""
  (if (null a-list)
      (list clause)
      (list clause a-list)))

;;;;;;;;;;;;;
;;;;;NNF;;;;;
(defun nnf-not(clause)
  "argument is (rest (not clause)). this functions applies NNF to a
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
  (cond ((is-atom clause) clause)
	((modifier-is clause 'equiv) (normalize (norm-equiv (rest clause))))
	((modifier-is clause 'implies) (normalize (norm-implies (rest clause))))
	(t (aux-normalize clause))))

;;;;;;;;;;;;;;;;;
;;;;;tableau;;;;;
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
previous node as parent. returns the children made, ordered by
atoms|ands|ors"
  (let ((children nil)
	(leaf nil))
    (dolist (child-formula parent-formula)
      (let ((child-var (gentemp)))
	(setf (symbol-value child-var) (make-formula :formula child-formula :parent parent-var))
	(setf parent-var child-var)
	;;(setf parent-formula child-formula)
	(setf children (push-formula children child-var))))
    children))

(defun or-tree(parent-var parent-formula)
  "takes the formulas in (rest (or f1 f2 ... fn)) and makes nodes,
each having parent-var as parent. returns the nodes made, ordered by
atoms|ands|ors."
  (let ((children nil))
    (dolist (child-formula parent-formula)
      (let ((child-var (gentemp)))
	(setf (symbol-value child-var) (make-formula
					:formula child-formula :parent
					parent-var))
	(setf children (push-formula children child-var))))
    children))

(defun push-formula(a-list formula-var)
  "takes a list/stack and cons/snoc'es formulas according to their
type (ands and atoms are prioritized over ors)."
  ;;um átomo depois de or é uma leaf? se conseguir fazer todos os ands antes de ors, sim.
  (let ((formula (formula-formula (symbol-value formula-var)))) ;;(varname (get-value-formula-in formula-object)
    (cond ((is-atom
    formula) (progn (setf (formula-visited (symbol-value formula-var))
			  T)
		    (cons formula-var a-list)))
	  ((modifier-is formula 'and) (cons formula-var a-list))
	  ((modifier-is formula 'or) (snoc formula-var a-list)))))
  
(defun make-tree(parent-var parent-formula)
  ""
  (cond ((is-atom parent-formula) parent-formula)
	((modifier-is parent-formula 'and)
	 (and-manage-tree (and-tree parent-var (rest parent-formula))))
	((modifier-is parent-formula 'or)
	 (or-tree parent-var (rest parent-formula)))))

(defun and-manage-tree(nodes)
  ""
  (let ((leaf (first (last nodes))))
    (dolist (node nodes)
      (when (null (formula-visited (symbol-value node))) ;;ignore atoms
	(make-tree leaf (formula-formula (symbol-value node))))))) ;;makes children pointing to leaf, not "parent"

(defun and-manage-tree(nodes)
  ""
  (let ((leaf (first (last nodes)))
	(result nil))
    (dolist (node nodes)
      (when (null (formula-visited (symbol-value node)))
	(setf result (cons (make-tree leaf (formula-formula (symbol-value node))) result))))
    (if (null result)
	leaf
	result)))

(defun or-manage-tree(nodes)
  ""
  (dolist (node nodes)
    (when (null (formula-visited (symbol-value node)))
      (make-tree node (formula-formula (symbol-value node))))))

(defun tableau(KB query)
  ""
  (let* ((queried-kb (kb-to-nnf (snoc (list 'not query) kb)))
	 (kb-var (make-formula :formula queried-kb :visited T)))
    (make-tree kb-var queried-kb)))

(defstruct formula
  ""
  (formula)
  (parent nil)
  (visited nil))

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
