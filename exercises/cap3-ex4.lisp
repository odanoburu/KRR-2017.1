
;; SNARK prover


(assert '(and
	  (=> (truth_teller henri) (not (truth_teller pierre)))
	  (=> (truth_teller pierre) (not (truth_teller henri)))
	  (or (truth_teller henri) (truth_teller pierre))))

(assert '(forall (?X ?Q)
	  (iff
	   (answer_yes ?X ?Q)
	   (or
	    (and (truth_teller ?X) (true ?Q))
	    (and (not (truth_teller ?X)) (not (true ?Q)))))))

(assert '(iff (true gauche) go_left))

(assert '(forall (?X ?Q)
	  (iff
	   (true (dit_oui ?X ?Q))
	   (answer_yes ?X ?Q))))

(assert '(forall (?X ?Q)
	  (iff
	   (true (dit_non ?X ?Q))
	   (not (answer_yes ?X ?Q)))))

(prove '(iff (answer_yes henri ?Q) go_left) :answer '(ans ?Q))

(prove '(iff (answer_yes henri gauche) go_left))
