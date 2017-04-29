
(in-package :snark-user)

(initialize)
(use-resolution t)

(assert '(forall (?x)
	  (plus zero ?x ?x)))


(assert '(forall (?x ?y ?z)
	  (implies
	   (plus ?x ?y ?z)
	   (plus (s ?x) ?y (s ?z)))))


;; Implicit facts

(prove '(plus (s (s zero)) (s (s (s zero))) (s (s (s (s (s zero)))))))


(prove '(exists (?u) (plus (s (s zero)) (s (s (s zero))) ?u)) :answer '(a ?u))

