;; This is an experiment using SNARK Prover
;; http://www.ai.sri.com/snark/tutorial/tutorial.html#htoc11

;; Tested with SBCL (http://www.sbcl.org), SNARK was installed via
;; http://quicklisp.org.

;; KB

(in-package :snark-user)

(initialize)
(use-resolution t)


(assert '(Man john))
(assert '(Woman jane))
(assert '(Company faultyInsurancecompany))
(assert '(Knife butcherknife1))

(assert '(Rich john))
(assert '(not (HappilyMarried jim)))
(assert '(WorksFor jim fic))
(assert '(Bloody butcherknife1))
(assert '(ClosedForRepairs marTDiner))

(assert '(= john (ceoOf fic)))
(assert '(= john (bestFriendOf jim)))
(assert '(= fic faultyInsurancecompany))

(assert '(forall (?y)
	  (implies
	   (and (Rich ?y) (Man ?y))
	   (Loves ?y jane))))

(assert '(forall (?y)
	  (implies
	   (and (Woman ?y) (not (= jane ?y)))
	   (Loves ?y john))))

(assert '(forall (?x ?y)
	  (implies
	   (Loves ?x ?y)
	   (not (Blackmails ?x ?y)))))

(assert '(or
	  (Loves jane john)
	  (Loves jane jim)))

(assert '(exists (?x)
	  (and (Adult ?x) (Blackmails ?x john))))

;;; necessary fact

(assert '(forall (?x)
	  (implies (Adult ?x) (or (Man ?x) (Woman ?x)))))

;; Implicitt facts

(prove '(exits (?x) (and (Company ?x) (Loves (ceoOf ?x) jane))))


(prove '(implies
	 (forall (?x) (implies (Man ?x) (not (Blackmails ?x john))))
	 (exists (?y) (and (Loves john ?y) (Blackmails ?y john)))))
