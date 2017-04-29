
(in-package :snark-user)

(initialize)
(use-paramodulation t)
;; (use-number-sorts t)
;; (use-code-for-numbers t)

(declare-sort 'value)
; (declare-subsort 'row 'value)
; (declare-subsort 'col 'value)

(declare-constant 'zero  :sort 'value)
(declare-constant 'one   :sort 'value)
(declare-constant 'two   :sort 'value)
(declare-constant 'three :sort 'value)
(declare-constant 'four  :sort 'value)
(declare-constant 'five  :sort 'value)
(declare-constant 'six   :sort 'value)
(declare-constant 'seven :sort 'value)
(declare-constant 'eight :sort 'value)
(declare-constant 'nine  :sort 'value)

(declare-relation 'cell 3 :sort '(t value value value))
(declare-relation 'same-interval 2 :sort '(t value value) :commutative t :allowed-in-answer nil)

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


(assert '(forall ((?value :sort value)) (same-interval ?value ?value)))
;; (assert '(implies (same-interval x y) (same-interval y x)))

(assert '(forall ((?x :sort value)
		  (?y :sort value)
		  (?z :sort value))
	  (implies (and (same-interval ?x ?y) (same-interval ?y ?z)) (same-interval ?x ?z))))

(assert '(same-interval zero one))
(assert '(same-interval one two))

(assert '(same-interval three four))
(assert '(same-interval four five))

(assert '(same-interval six seven))
(assert '(same-interval seven eight))

(assert '(same-interval zero three))
(assert '(same-interval three six))
(assert '(same-interval zero six))

(assert '(forall ((?x1 :sort value)
		  (?x2 :sort value)
		  (?y1 :sort value)
		  (?y2 :sort value)
		  (?z  :sort value))
	  (implies (and
		    (same-interval ?x1 ?x2)
		    (same-interval ?y1 ?y2)
		    (cell ?x1 ?y1 ?z)
		    (cell ?x2 ?y2 ?z))
	   (and (= ?x1 ?x2) (= ?y1 ?y2)))))


(assert '(forall ((?x :sort value)
		  (?z :sort value))
	  (exists ((?y :sort value)) (cell ?x ?y ?z))))

(assert '(forall ((?y :sort value)
		  (?z :sort value))
	  (exists ((?x :sort value)) (cell ?x ?y ?z))))

(assert '(cell zero zero one))
(assert '(cell zero three two))
(assert '(cell zero six three))
(assert '(cell one one two))
(assert '(cell one four three))
(assert '(cell one seven four))
(assert '(cell two two three))
(assert '(cell two five four))
(assert '(cell two eight five))
(assert '(cell three zero six))
(assert '(cell three three four))
(assert '(cell three six five))
(assert '(cell four one seven))
(assert '(cell four four five))
(assert '(cell four seven six))
(assert '(cell five two eight))
(assert '(cell five five six))
(assert '(cell five eight seven))
(assert '(cell six zero eight))
(assert '(cell six three zero))
(assert '(cell six six seven))
(assert '(cell seven one zero))
(assert '(cell seven four one))
(assert '(cell seven seven eight))
(assert '(cell eight two one))
(assert '(cell eight five two))
(assert '(cell eight eight four))
