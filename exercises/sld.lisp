;exercise 1 of chapter 5 of BRACHMAN & LEVESQUE: KRR 2004
;by bruno cuconato (@odanoburu)
;unlicensed to the public domain.

;input: a finite list of atomic sentences, q 1 , . . . , q n
;output: YES or NO according to whether a given KB entails all of the q i
;1. if all of the goals q i are marked as solved, then return YES
;2. check if there is a clause [ p, ¬p 1 , . . . , ¬p n ] in KB, such that all of its
;negative atoms p 1 , ..., p n are marked as solved, and such that the
;positive atom p is not marked as solved
;3. if there is such a clause, mark p as solved and go to step 1
;4. otherwise, return NO

(defparameter *CHILD-KB*
  (copy-tree '((Toddler)
    (Toddler Child)
    (Child Male Boy)
    (Infant Child)
    (Child Female Girl)
    (Female)))
  "A simple KB."
  )

(defvar *KB* *CHILD-KB*
  "the KB explored by forward chaining."
  )

(defun make-atom(negative-atom ix)
  "adds clause ix to negative-atom's :on-clauses property."
  (push ix (get negative-atom :on-clauses)))

(defun make-atoms(negative-atoms ix)
  "iterates over negative-atoms provided and passes them to make-atom
function"
  (dolist (negative-atom negative-atoms)
    (make-atom negative-atom ix)))

(defun clause-symbol(ix)
  "takes a clause index and returns the symbol that refers to it."
  (intern (format nil "clause~D" ix)))

(defun make-conclude(clause ix)
  "each clause is a symbol named after its index. this function sets
the clauses' :conclusion property after its conclusion (positive atom
in the horn clause) and the :remaining property after the number of
negative atoms left to resolve in the clause"
  (setf (get (clause-symbol ix) :conclusion) (first (last clause)))
  (setf (get (clause-symbol ix) :remaining) (- (length clause) 1)))
  
(defun parse-clause(clause ix)
  "parses clause; adds its negative atoms to the at"
  (cond ((null clause) nil)
	(t (if (not (= 1 (length clause)))
	       (make-atoms (butlast clause) ix)); for non-atomic clauses, gets all negatively appearing atoms
	   (make-conclude clause ix); makes clause conclusion
	   )))

(defun parse-kb(kb)
  "parses KB in appropriate format"
  (loop for clause in kb
     for ix from 0
     do (parse-clause clause ix)))

(defun feed-stack(kb)
  "searches through the KB to check for atoms proven to be true."
  (let ((true-atoms nil))
  (loop for clause in kb
     for ix from 0
     when (= (get (clause-symbol ix) :remaining) 0); if all negative atoms in clause have been resolved
     do (if (null (get (get (clause-symbol ix) :conclusion) :visited)); if conclusion has not been visited yet
	 (push (get (clause-symbol ix) :conclusion) true-atoms))); add conclusion to stack, so that it can be propagated
  true-atoms))

(defun pop-stack(stack)
  "takes the first element and from the stack and resolves it with all
the atoms it can, by decrementing the :remaining property on the
clauses it appears negatively. also marks it as visited."
  (dolist (ix (get (first stack) :on-clauses))
    (setf (get (first stack) :visited) t)
    (setf (get (clause-symbol ix) :remaining) (- (get (clause-symbol ix) :remaining) 1)))
  (rest stack))

(defun query-kb(kb query)
  "query the given kb, which is a list of horn clauses. query must be a positive atom."
  (parse-kb kb); populate symbol's property lists
  (let ((stack nil))
    (setf stack (feed-stack kb)); init stack
    (loop while (or (not (member query stack)) (not (null stack))); if stack is empty or query is in stack, stop.
	 do (setf stack (pop-stack stack)); resolve first member of stack
	 (setf stack (feed-stack kb)))
    (assert (null stack))
    (if (member query stack)
	t
	nil)))
