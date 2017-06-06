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

(defparameter *child-KB*
  (copy-tree '((Toddler)
    ((not Toddler) Child)
    ((not Child) (not Male) Boy)
    ((not Infant) Child)
    ((not Child) (not Female) Girl)
    (Female)))
  "A simple KB.")

(defvar *KB* *child-KB*
  "the KB explored by forward chaining.")

(defparameter *query*
  '(Girl)
  "the query to be made to the KB. will be negated.")

(setf *STACK*
      nil)

(setf *ATOMS*
      nil)

(setf *CONCLUSIONS*
      nil)

(defun add-atom(atom ix atoms)
  "pushes an empty atom record to the ATOMS variable, except for its on-clauses property, which has the clause where it was found."
  (push (list :atom atom :visited nil :on-clauses (list ix)) atoms))

(defun update-atom(atom ix atoms)
  "adds to the atom's on-clauses property another clause where it was found."
  (let ((atomo (find-atom atom)))
    (setf (getf atomo :on-clauses) (cons ix (get-indices atom atoms)))))

(defun find-atom(atom atoms)
  "returns the property list of a given atom"
  (find atom atoms :key (lambda (record) (getf record :atom))))

;(defun remove-atom(atom)
;  "returns the property list of all atoms except for the one provided."
;  (remove-if-not #'(lambda(record) (not (equal (getf record :atom) atom))) *ATOMS*))

(defun get-indices(atom atoms)
  "gets indices of clauses where atom is found."
  (getf (find-atom atom atoms) :on-clauses))

(defun make-atoms(clause ix atoms)
  "takes a clause and recursively builds the ATOMS variable."
  (cond ((null clause) atoms)
	(t (if (null (find-atom (first clause) atoms))
	    (add-atom (first clause) ix atoms); if atom not in ATOMS
	    (update-atom (first clause) ix atoms)); if atom already in ATOMS
	 (make-atoms (rest clause) ix atoms)))); recurse

(defun make-conclude(clause ix clauses)
  "make the plist conclusion, where the property name is the atom/goal and the value is the number of atoms appearing negatively in the clause that are not yet known to be true."
  (push (cons ix (list (first (last clause)) (- (length clause) 1))) clauses))

(defun parse-kb(kb atoms conclusions)
  "parses KB in appropriate format"
  (loop for clause in kb
     for ix from 0
     do (make-atoms clause ix atoms)
     do (make-conclude clause ix conclusions)))

(defun add-to-goals(atom goals &optional (solved nil))
  (push (list atom solved) goals))

;(defun query-kb(query kb)
;  (add-to-goals query)))

(defun checkgoals(goals)
  (loop for goal in goals
     when (null (second goal))
     return 1))

(defun search-resolutions(kb conclusions)
  )

(defun forward-chain(kb goals conclusions)
  (search-resolutions kb conclusions)
  (let ((solved (checkgoals goals)))
  (cond ((null solved) (print "NO"))
	((= 1 solved) (print "YES"))
      (t (forward-chain kb goals conclusions))
      )))
