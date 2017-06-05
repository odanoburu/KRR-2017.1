;exercise 1 of chapter 5 of BRACHMAN & LEVESQUE: KRR 2004
;by bruno cuconato (@odanoburu)
;unlicensed to the public domain.

(defparameter *child-KB*
  '((Toddler)
    ((not Toddler) Child)
    ((not Child) (not Male) Boy)
    ((not Infant) Child)
    ((not Child) (not Female) Girl)
    (Female))
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

(defun add-atom(atom ix)
  "pushes an empty atom record to the ATOMS variable, except for its on-clauses property, which has the clause where it was found."
  (push (list :atom atom :visited nil :on-clauses (list ix)) *ATOMS*))

(defun update-atom(atom ix)
  "adds to the atom's on-clauses property another clause where it was found."
  (let ((atomo (find-atom atom)))
    (setf (getf atomo :on-clauses) (cons ix (get-indices atom)))))

(defun find-atom(atom)
  "returns the property list of a given atom"
  (first (remove-if-not #'(lambda(record) (equal (getf record :atom) atom)) *ATOMS*)))

;(defun remove-atom(atom)
;  "returns the property list of all atoms except for the one provided."
;  (remove-if-not #'(lambda(record) (not (equal (getf record :atom) atom))) *ATOMS*))

(defun get-indices(atom)
  "gets indices of clauses where atom is found."
  (getf (find-atom atom) :on-clauses))

(defun make-atoms(clause ix)
  "takes a clause and recursively builds the ATOMS variable."
  (cond ((null clause) *ATOMS*)
	(t (if (null (find-atom (first clause)))
	    (add-atom (first clause) ix); if atom not in ATOMS
	    (update-atom (first clause) ix)); if atom already in ATOMS
	 (make-atoms (rest clause) ix)))); recurse

(defun make-conclude(clause ix)
  "make the plist conclusion, where the property name is the atom/goal and the value is the number of atoms appearing negatively in the clause that are not yet known to be true."
  (push (cons ix (list (first (last clause)) (- (length clause) 1))) *CONCLUSIONS*))

(defun parse-kb(kb)
  "parses KB in appropriate format"
  (loop for clause in kb
     for ix from 0
     do (make-atoms clause ix)
     do (make-conclude clause ix)))

(defun add-to-goals(atom &optional (solved nil))
  (push (list atom solved) *GOALS*))

(defun query-kb(query kb)
  (add-to-goals query)))

(defun forward-chain(kb goals conclusions)
  (search-resolutions kb conclusions)
  (let ((solved (checkgoals goals)))
  (cond ((null solved) (print "NO"))
	((= 1 solved) (print "YES"))
      (t (forward-chain kb goals conclusions))
      ))
