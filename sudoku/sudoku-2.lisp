(in-package :snark-user)

(initialize)
(use-paramodulation t)
;; (use-number-sorts t)
;; (use-code-for-numbers t)

(declare-subsort 'value 'integer)
;; (declare-subsort 'row 'value)
;; (declare-subsort 'col 'value)

(assert '(or
	  (= ?n.value 0) (= ?n.value 1) (= ?n.value 2) (= ?n.value 3) (= ?n.value 4)
	  (= ?n.value 5) (= ?n.value 6) (= ?n.value 7) (= ?n.value 8)))

;; (declare-constant 'zero  :sort 'value)
;; (declare-constant 'one   :sort 'value)
;; (declare-constant 'two   :sort 'value)
;; (declare-constant 'three :sort 'value)
;; (declare-constant 'four  :sort 'value)
;; (declare-constant 'five  :sort 'value)
;; (declare-constant 'six   :sort 'value)
;; (declare-constant 'seven :sort 'value)
;; (declare-constant 'eight :sort 'value)
;; (declare-constant 'nine  :sort 'value)

(declare-relation 'cell 3 :sort '(t value))
(declare-relation 'same-group 2 :sort '(t value) :commutative t :allowed-in-answer nil)

;; (assert '(forall (?x ?y)
;; 	  (implies
;; 	   (and
;; 	    (>= ?x 0) (<= ?x 9)
;; 	    (>= ?y 0) (<= ?y 9))
;; 	   (exists (?z)
;; 	    (and (= (place ?x ?y) ?z)
;; 		 (>= ?z 0) (<= ?z 9))))))


(assert '(forall ((?row   :sort value)
		  (?col1  :sort value)
		  (?col2  :sort value)
		  (?value :sort value))
	  (implies (and (cell ?row ?col1 ?value) (cell ?row ?col2 ?value)) (= ?col1 ?col2))))

(assert '(forall ((?row1  :sort value)
		  (?row2  :sort value)
		  (?col   :sort value)
		  (?value :sort value))
	  (implies (and (cell ?row1 ?col ?value) (cell ?row2 ?col ?value)) (= ?row1 ?row2))))


(assert '(forall ((?value :sort value)) (same-group ?value ?value)))
;; (assert '(implies (same-group x y) (same-group y x)))

(assert '(forall ((?x :sort value)
		  (?y :sort value)
		  (?z :sort value))
	  (implies (and (same-group ?x ?y) (same-group ?y ?z)) (same-group ?x ?z))))

(assert '(same-group 0 1))
(assert '(same-group 1 2))

(assert '(same-group 3 4))
(assert '(same-group 4 5))

(assert '(same-group 6 7))
(assert '(same-group 7 8))

(assert '(not (same-group 0 3)))
(assert '(not (same-group 3 6)))
;; (assert '(not (same-group 0 6)))

(assert '(forall ((?x1 :sort value)
		  (?x2 :sort value)
		  (?y1 :sort value)
		  (?y2 :sort value)
		  (?z  :sort value))
	  (implies (and
		    (same-group ?x1 ?x2)
		    (same-group ?y1 ?y2)
		    (cell ?x1 ?y1 ?z)
		    (cell ?x2 ?y2 ?z))
	   (and (= ?x1 ?x2) (= ?y1 ?y2)))))


(assert '(forall ((?x :sort value)
		  (?z :sort value))
	  (exists ((?y :sort value)) (cell ?x ?y ?z))))

(assert '(forall ((?y :sort value)
		  (?z :sort value))
	  (exists ((?x :sort value)) (cell ?x ?y ?z))))

(assert '(cell 0 0 1))
(assert '(cell 0 3 2))
(assert '(cell 0 6 3))
(assert '(cell 1 1 2))
(assert '(cell 1 4 3))
(assert '(cell 1 7 4))
(assert '(cell 2 2 3))
(assert '(cell 2 5 4))
(assert '(cell 2 8 5))
(assert '(cell 3 0 6))
(assert '(cell 3 3 4))
(assert '(cell 3 6 5))
(assert '(cell 4 1 7))
(assert '(cell 4 4 5))
(assert '(cell 4 7 6))
(assert '(cell 5 2 8))
(assert '(cell 5 5 6))
(assert '(cell 5 8 7))
(assert '(cell 6 0 8))
(assert '(cell 6 3 0))
(assert '(cell 6 6 7))
(assert '(cell 7 1 0))
(assert '(cell 7 4 1))
(assert '(cell 7 7 8))
(assert '(cell 8 2 1))
(assert '(cell 8 5 2))
(assert '(cell 8 8 4))
