;;questions:

;;how do I write a function that turns a clause into NNF form? should
;;I do outer->inner or inner->outer layers? how to decide if it is
;;finished?

;;should I go for the propositional case first, so that I can see how
;;to write the tableau algorithm?

;;how do I aggregate my auxiliary functions and apply them correctly?


(defun to-NNF(clause)
  ""
  (let ((modifier (first clause)))
    (cond ((equal modifier 'not) (funcall nnf-not (rest clause)))
	  ((equal modifier 'reverse) (funcall modifier (rest clause)))
	  ((equal modifier 'implies) (funcall modifier (rest clause)))
	  ((equal modifier 'and) (funcall nnf-and (rest clause)))
	  ((equal modifier 'or) (funcall nnf-or (rest clause)))
	  ((equal modifier 'some) (funcall modifier (rest clause)))
	  ((equal modifier 'only) (funcall modifier (rest clause)))
	  ((equal modifier 'subsume) (funcall modifier (rest clause)))
	  ((equal modifier 'equiv) (funcall modifier (rest clause)))
	  (t clause))))

(defun nnf-not(clause)
  "argument is (rest (not clause))"
  (cond ((and (equal (length clause) 1) (eql t (first clause))) nil)
	((and (equal (length clause) 1) (equal nil (first clause))) t)
	((equal (length clause) 1) (cons 'not clause))
	((equal (first clause) 'not) (nnf-not-not (rest clause)))
	((equal (first clause) 'or) (nnf-or (rest clause)))
	((equal (first clause) 'and) (nnf-and (rest clause)))
	(t (print "oops"))))

(defun nnf-not-not(clause)
  ""
  (if (equal (first clause) 'not) (rest clause)
      clause))

(defun aux-negate(clause)
  "returns negated clause"
  (assert (not (and (not (atom clause)) (equal (length clause) 1))))
  (list 'not clause))

(defun nnf-or(clause)
  ""
  (assert (>= (length clause) 2))
  (cons 'and (mapcar #'aux-negate clause)))

(defun nnf-and(clause)
  ""
  (assert (>= (length clause) 2))
  (cons 'or (mapcar #'aux-negate clause)))

(defun nnf-some(clause)
  "(some role (concept))"
  (assert (equal (length clause) 2))
  (cons 'only (list (first clause) (aux-negate (second clause)))))

(defun nnf-only(clause)
  "(only role (concept))"
  (assert (equal (length clause) 2))
  (cons 'some (list (first clause) (aux-negate (second clause)))))

(defun norm-implies(clause)
  "argument is (rest (implies clause))"
  (assert (equal (length clause) 2)) ;binary relation
  (cons 'or (list (list 'not (first clause)) (second clause))))

(defun norm-equiv(clause)
  ""
  (assert (>= (length clause) 2))
  (cons 'or (list (cons 'and (mapcar #'aux-negate clause)) (cons 'and
  clause))))

(defun stupid-tests()
  ""
  (assert (equal '(OR (NOT A) (NOT B)) (nnf-and (rest '(and A B)))))
  (assert (equal '(NOT A) (aux-negate 'A)))
  (assert (equal '(NOT (AND A B)) (aux-negate '(and A B))))
  (assert (equal '(AND (NOT A) (NOT B)) (nnf-or (rest '(or A B)))))
  (assert (equal '(OR (NOT A) B) (norm-implies (rest '(implies A B)))))
  (assert (equal '(OR (AND (NOT A) (NOT B)) (AND A B)) (norm-equiv (rest '(equiv A B)))))
  (assert (equal '(ONLY C (NOT (AND D E))) (nnf-some (rest '(some C (and D E))))))
  (assert (equal '(SOME C (NOT (OR E F))) (nnf-only (rest '(only C (or E F))))))
  t)
