(assert '(APM T))

(assert '(APM J))

(assert '(APM M))

(assert '(forall (?x) (or (not (APM ?x)) (S ?x) (MC ?x))))

(assert '(forall (?x) (or (not (MC ?x)) (not (LR ?x)))))

(assert '(forall (?x) (or  (LS ?x) (not (S ?x)))))

(assert '(not (LS M )))

(assert '(not (LR M )))

(assert '(LS T))

(assert '(LR T))

(prove '((and (MC ?x) (not (S ?x)))) :answer '(ans ?x))

;OUTPUT:

; The current SNARK option values are
;    (USE-RESOLUTION T)
;       (USE-HYPERRESOLUTION NIL)
;       (USE-NEGATIVE-HYPERRESOLUTION NIL)
;       (USE-UR-RESOLUTION NIL)
;       (USE-UR-PTTP NIL)
;       (USE-PARAMODULATION NIL)
;       (USE-FACTORING NIL)
;       (USE-EQUALITY-FACTORING NIL)
;       (USE-CONDENSING T)
;       (USE-RESOLVE-CODE NIL)
;       (USE-UNIT-RESTRICTION NIL)
;       (USE-INPUT-RESTRICTION NIL)
;       (USE-LITERAL-ORDERING-WITH-RESOLUTION NIL)
;       (USE-LITERAL-ORDERING-WITH-HYPERRESOLUTION NIL)
;       (USE-LITERAL-ORDERING-WITH-NEGATIVE-HYPERRESOLUTION NIL)
;       (USE-LITERAL-ORDERING-WITH-UR-RESOLUTION NIL)
;       (USE-LITERAL-ORDERING-WITH-PARAMODULATION NIL)
;       (USE-SUBSUMPTION T)
;       (USE-SUBSUMPTION-BY-FALSE :FALSE)
;       (USE-SIMPLIFICATION-BY-UNITS T)
;       (USE-SIMPLIFICATION-BY-EQUALITIES T)
;       (USE-TERM-ORDERING :RPO)
;       (USE-DEFAULT-ORDERING T)
;       (1-ARY-FUNCTIONS>2-ARY-FUNCTIONS-IN-DEFAULT-ORDERING NIL)
;       (ORDERING-FUNCTIONS>CONSTANTS NIL)
;       (RPO-STATUS :MULTISET)
;       (KBO-STATUS :LEFT-TO-RIGHT)
;       (USE-INDEFINITE-ANSWERS NIL)
;       (USE-CONDITIONAL-ANSWER-CREATION NIL)
;       (USE-CONSTRAINT-SOLVER-IN-SUBSUMPTION NIL)
;       (ALLOW-SKOLEM-SYMBOLS-IN-ANSWERS T)
;       (REWRITE-ANSWERS NIL)
;       (USE-CONSTRAINT-PURIFICATION NIL)
;       (USE-FUNCTION-CREATION NIL)
;       (USE-REPLACEMENT-RESOLUTION-WITH-X=X NIL)
;       (USE-PARAMODULATION-ONLY-INTO-UNITS NIL)
;       (USE-PARAMODULATION-ONLY-FROM-UNITS NIL)
;       (USE-SINGLE-REPLACEMENT-PARAMODULATION NIL)
;       (ASSERT-CONTEXT :ROOT)
;       (ASSERT-SUPPORTED T)
;       (ASSUME-SUPPORTED T)
;       (PROVE-SUPPORTED T)
;       (ASSERT-SEQUENTIAL NIL)
;       (ASSUME-SEQUENTIAL NIL)
;       (PROVE-SEQUENTIAL NIL)
;       (NUMBER-OF-GIVEN-ROWS-LIMIT NIL)
;       (NUMBER-OF-ROWS-LIMIT NIL)
;       (AGENDA-LENGTH-BEFORE-SIMPLIFICATION-LIMIT 10000)
;       (AGENDA-LENGTH-LIMIT 3000)
;       (RUN-TIME-LIMIT NIL)
;       (ROW-WEIGHT-LIMIT NIL)
;       (ROW-WEIGHT-BEFORE-SIMPLIFICATION-LIMIT NIL)
;       (LEVEL-PREF-FOR-GIVING NIL)
;       (AGENDA-ORDERING-FUNCTION ROW-PRIORITY)
;       (PRUNING-TESTS (ROW-WEIGHT-LIMIT-EXCEEDED))
;       (PRUNING-TESTS-BEFORE-SIMPLIFICATION (ROW-WEIGHT-BEFORE-SIMPLIFICATION-LIMIT-EXCEEDED))
;       (USE-CLAUSIFICATION T)
;       (USE-EQUALITY-ELIMINATION NIL)
;       (USE-MAGIC-TRANSFORMATION NIL)
;       (USE-AC-CONNECTIVES T)
;       (USE-PURITY-TEST NIL)
;       (USE-RELEVANCE-TEST NIL)

(Row 1
   (APM T)
   ASSERTION) 
(Row 2
   (APM J)
   ASSERTION) 
(Row 3
   (APM M)
   ASSERTION) 
(Row 4
   (OR (NOT (APM ?X)) (S ?X) (MC ?X))
   ASSERTION) 
(Row 5
   (OR (NOT (MC ?X)) (NOT (LR ?X)))
   ASSERTION) 
(Row 6
   (OR (LS ?X) (NOT (S ?X)))
   ASSERTION) 
(Row 7
   (NOT (LS M))
   ASSERTION) 
(Row 8
   (NOT (LR M))
   ASSERTION) 
(Row 9
   (LS T)
   ASSERTION) 
(Row 10
   (LR T)
   ASSERTION) 
(Row 11
   (OR (NOT (MC ?X)) (S ?X))
   NEGATED_CONJECTURE
   Answer (ANS ?X)) 
(Row 12
   (NOT (MC T))
   (RESOLVE 5 10)) 
(Row 13
   (NOT (S M))
   (RESOLVE 7 6)) 
(Row 14
   (OR (NOT (MC ?X)) (LS ?X))
   (RESOLVE 6 11)
   Answer (ANS ?X)) 
(Row 15
   (MC M)
   (REWRITE (RESOLVE 4 3) 13)) 
(Row 16
   (OR (S J) (MC J))
   (RESOLVE 4 2)) 
(Row 17
   (S T)
   (REWRITE (RESOLVE 4 1) 12)) 
(Row 18
   (OR (NOT (APM ?X)) (S ?X))
   (RESOLVE 11 4)
   Answer (ANS ?X)) 
(Row 19
   (OR (NOT (APM ?X)) (MC ?X) (LS ?X))
   (RESOLVE 6 4)) 
(Row 20
   (OR (NOT (APM ?X)) (S ?X) (NOT (LR ?X)))
   (RESOLVE 5 4)) 
(Row 21
   FALSE
   (REWRITE (RESOLVE 13 11) 15)
   Answer (ANS M)) 

(Refutation
(Row 3
   (APM M)
   ASSERTION)
(Row 4
   (OR (NOT (APM ?X)) (S ?X) (MC ?X))
   ASSERTION)
(Row 6
   (OR (LS ?X) (NOT (S ?X)))
   ASSERTION)
(Row 7
   (NOT (LS M))
   ASSERTION)
(Row 11
   (OR (NOT (MC ?X)) (S ?X))
   NEGATED_CONJECTURE
   Answer (ANS ?X))
(Row 13
   (NOT (S M))
   (RESOLVE 7 6))
(Row 15
   (MC M)
   (REWRITE (RESOLVE 4 3) 13))
(Row 21
   FALSE
   (REWRITE (RESOLVE 13 11) 15)
   Answer (ANS M))
)

; Summary of computation:
;        23 formulas have been input or derived (from 14 formulas).
;        21 (91%) were retained.  Of these,
;           21 (100%) are still being kept.
; 
; Run time in seconds excluding printing time:
;     0.001   1%   Assert                       (11 calls)
;     0.000   0%   Process new row              (22 calls)
;     0.001   1%   Resolution                   (13 calls)
;     0.000   0%   Condensing                   (6 calls)
;     0.000   0%   Forward subsumption          (10 calls)
;     0.000   0%   Backward subsumption         (10 calls)
;     0.000   0%   Forward simplification       (22 calls)
;     0.000   0%   Backward simplification      (21 calls)
;     0.000   0%   Sortal reasoning             (26 calls)
;     0.121  98%   Other
;     0.123        Total
;     8.636        Real time
; 
; Term-hash-array has 20 terms in all.
; Feature-vector-row-index has 10 entries (10 at peak, 10 added, 0 deleted).
; Feature-vector-row-index has 19 nodes (19 at peak, 19 added, 0 deleted).
;  Retrieved 1 possibly forward subsuming row in 9 calls.
;  Retrieved 1 possibly backward subsumed row in 9 calls.
; Path-index has 21 entries (21 at peak, 21 added, 0 deleted).
; Path-index has 27 nodes (27 at peak, 27 added, 0 deleted).
; Trie-index has 21 entries (21 at peak, 21 added, 0 deleted).
; Trie-index has 27 nodes (27 at peak, 27 added, 0 deleted).
; Retrieved 5 generalization terms in 24 calls.
; Retrieved 11 instance terms in 11 calls.
; Retrieved 21 unifiable terms in 18 calls.
; 
; The agenda of rows to process has 1 entry:
;     1 with value 8
; The agenda of rows to give has 7 entries:
;     2 with value (4 4)           3 with value (4 7)            2 with value (4 10)
