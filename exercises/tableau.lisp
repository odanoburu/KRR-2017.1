;;questions:

;;how do I write a function that turns a clause into NNF form? should
;;I do outer->inner or inner->outer layers? how to decide if it is
;;finished?

;;should I go for the propositional case first, so that I can see how
;;to write the tableau algorithm?

;;how do I aggregate my auxiliary functions and apply them correctly?

(defun snoc(an-atom a-list)
  "cons to end of list."
  (reverse (cons an-atom (reverse a-list))))

(defun is-atom(clause)
  "returns nil if clause is not an atom, which includes lisp's atoms
and their negations."
  (cond ((atom clause) clause)
	((and (equal (first clause) 'not) (atom (cadr clause)) (equal (length clause) 2)) clause)
	(t nil)))

(defun nnf-not(clause)
  "argument is (rest (not clause)). this functions applies NNF to a
clause."
  (cond ((and (equal (length clause) 1) (eql t (first clause))) nil)
	((and (equal (length clause) 1) (equal nil (first clause))) t)
	((and (equal (length clause) 1) (atom (first clause))) (cons 'not clause))
	((equal (first clause) 'not) (cadr clause))
	((equal (first clause) 'or) (nnf-or (rest clause)))
	((equal (first clause) 'and) (nnf-and (rest clause)))
	((equal (first clause) 'some) (nnf-some (rest clause)))
	((equal (first clause) 'only) (nnf-only (rest clause)))
	(t nil)))

(defun nnf-aux-negate(clause)
  "returns negated clause, already calling nnf-not when appropriate."
  (assert (not (and (not (atom clause)) (equal (length clause) 1))))
  (if (atom clause) (list 'not clause) (nnf-not clause)))

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

;;deprecated in favor of mapcar version (which is also non destructive).
(defun aux-to-NNF(clause)
  "auxliary function to to-nnf."
  (let ((result nil))
    (dolist (element clause)
      (setf result (snoc (to-NNF element) result)))
    result))
;;;

(defun aux-to-NNF(clause)
  "auxliary function to to-nnf."
  (mapcar #'to-NNF clause))

(defun to-NNF(clause)
  "turns a clause into NNF."
  (cond ((is-atom clause) clause)
	((equal (first clause) 'not) (to-NNF (nnf-not (cadr clause))))
	(t (aux-to-NNF clause)))) ;;can implement check here

;;not including implies and equivalence
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

;;deprecated for above function
(defun aux-normalize(clause)
  "auxliary function to normalize."
  (let ((result nil))
    (dolist (element clause)
      (setf result (snoc (normalize element) result)))
    result))
;;;

(defun normalize(clause)
  "turns a clause with implication and equivalence into something that
uses only and's or's & not's."
  (cond ((is-atom clause) clause)
	((equal (first clause) 'equiv) (normalize (norm-equiv (rest clause))))
	((equal (first clause) 'implies) (normalize (norm-implies (rest clause))))
	(t (aux-normalize clause))))

(defun norm(clause)
  "applies (to-nnf (normalize clause))."
  (to-nnf (normalize clause)))

;; deprecated
(defun KB-to-nnf(KB)
  "takes a KB (list of clauses) and returns their conjunction in NNF."
  (if (equal (length kb) 1)
      (norm (first kb))
      (let ((nnf-kb nil))
	(dolist (clause kb)
	  (setf nnf-kb (snoc (norm clause) nnf-kb)))
	(cons 'and nnf-kb))))
;;;

(defun KB-to-nnf(KB)
  "takes a KB (list of clauses) and returns their conjunction in NNF."
  (if (equal (length kb) 1)
      (progn (assert (is-atom (first kb))) (norm (first kb)))
      (cons 'and (mapcar #'norm kb))))

;;might be better if I call the future aux-tableau on the resulting formulas already, right?
(defun tableau-and(clause NNF-KB)
  "clause is (rest '(and clause)). applies and rule to clause and
returns NNF-KB with the resulting children added."
  (dolist (formula clause)
    (setf nnf-kb (snoc formula nnf-kb)))
  nnf-kb)

(defun tableau-and(clause NNF-KB)
  "clause is (rest '(and clause)). applies and rule to clause and
returns NNF-KB with the resulting children added."
  (mapcar #'))

(defun tableau-or(clause NNF-KB)
  ""
  (dolist (formula clause)
    (setf nnf-kb (snoc))))

(defun tableau(KB query)
  ""
  )


;;because I don't know yet how to do proper testing in CL
(defun stupid-tests()
  ""
  (assert (equal '(OR (NOT A) (NOT B)) (nnf-and (rest '(and A B)))))
  (assert (equal '(NOT A) (nnf-aux-negate 'A)))
  (assert (equal '(OR (NOT A) (NOT B)) (nnf-aux-negate '(and A B))))
  (assert (equal '(AND (NOT A) (NOT B)) (nnf-or (rest '(or A B)))))
  (assert (equal '(OR (NOT A) B) (norm-implies (rest '(implies A B)))))
  (assert (equal '(OR (AND (NOT A) (NOT B)) (AND A B)) (norm-equiv (rest '(equiv A B)))))
  (assert (equal '(ONLY C (OR (NOT D) (NOT E))) (nnf-some (rest '(some C (and D E))))))
  (assert (equal '(SOME C (AND (NOT E) (NOT F))) (nnf-only (rest '(only C (or E F))))))
  (assert (equal '(OR (ONLY R (OR (NOT A) B)) (SOME R (AND A B))) (to-nnf '(or (not (some r (and A (not B)))) (not (only r (or (not A) (not (not (not B))))))))))
  (assert (equal '(AND (OR (NOT A) (AND (OR B E) (OR (NOT B) (NOT E)))) (AND C (NOT D))) (KB-to-nnf '((not (and a (equiv b e))) (not (or (not c) d))))))
  (assert (equal '(AND (AND C (NOT D)) E C (NOT D)) (tableau-and (rest '(and C (not D))) '(and (and C (not D)) E))))
  (assert (equal '(OR (AND (NOT (OR (NOT A) B)) (NOT (OR (NOT A) B)) (NOT (OR (NOT (NOT B)) (NOT A)))) (AND (OR (NOT A) B) (OR (NOT A) B) (OR (NOT (NOT B)) (NOT A)))) (normalize '(equiv (implies a b) (or (not a) b) (implies (not b) (not a))))))
  t)
