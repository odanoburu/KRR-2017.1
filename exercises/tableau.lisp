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

(defun aux-to-NNF(clause)
  "auxliary function to to-nnf."
  (let ((result nil))
    (dolist (element clause)
      (setf result (snoc (to-NNF element) result)))
    result))

(defun to-NNF(clause)
  "turns a clause into NNF."
  (cond ((is-atom clause) clause)
	((equal (first clause) 'not) (to-NNF (nnf-not (cadr clause))))
	(t (aux-to-NNF clause))))

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
  "auxliary function to normalize."
  (let ((result nil))
    (dolist (element clause)
      (setf result (snoc (normalize element) result)))
    result))

(defun normalize(clause)
  "turns a clause into something that uses only and's or's & not's."
  (cond ((is-atom clause) clause)
	((equal (first clause) 'equiv) (normalize (norm-equiv (rest clause))))
	((equal (first clause) 'implies) (normalize (norm-implies (rest clause))))
	(t (aux-normalize clause))))


;;because I don't know yet how do to proper testing in CL
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
  t)
