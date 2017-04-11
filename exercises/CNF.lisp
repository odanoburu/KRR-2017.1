;; The procedure to convert any propositional formula to CNF is as follows:

;; 1. eliminate (emplies) and (equivalent), using the fact that these are abbreviations
;; for formulas using only  (and), (or) and (not)

;; 2. move (not) inwards so that it appears only in front of an atom, using the follow-
;; ing equivalences

;;; (equivalent (not (not alpha)) alpha)
;;; (equivalent (not (or beta alpha)) (and (not beta) (alpha)))
;;; (equivalent (not (and beta alpha)) (or (not beta) (alpha)))

;; 3. distribute   over  , using the following equivalences:

;;; (equivalent (or alpha (and beta gama)) (and (or alpha beta) (or alpha gama)))

;; test:
;; (CNF '(emplies (emplies p q) r) '(not (not a)) '(not (or m n)))
;;
;; output:
;; (AND (AND (OR R P) (OR R (NOT Q))) A (AND (NOT M) (NOT N)))

(defun CNF (&rest b)
  (let ((new-b (mapcar 'convert b)))
    (append '(and) new-b)))


(defun convert (formula)
  (let* ((formula-1 (convert-rule-1 formula))
	 (formula-2 (convert-rule-2 formula-1))
	 (formula-3 (convert-rule-3 formula-2)))
    formula-3))


(defun convert-rule-1 (formula)
  (if (and (listp formula) (equal (car formula) 'emplies))
      (cons 'or (cons (append '(not) (list (convert-rule-1 (cadr formula)))) (cons (convert-rule-1 (caddr formula)) nil)))
      formula))


(defun convert-rule-2 (formula)
  (if (listp formula)
      (if (equal (car formula) 'not)
	     (cadr (mapcar 'rule-2 formula))
	     (if (cddr formula)
		 (append (list (car formula)) (list (convert-rule-2 (cadr formula))) (list (convert-rule-2 (caddr formula))))
		 formula))
      formula))


(defun rule-2 (formula)
  (cond ((equal formula 'not)  nil)
	((equal formula 'and)  'or)
	((equal formula  'or)  'and)
	((atom  formula)  (append '(not) (list formula)))
	((and (equal (car formula) 'not) (null (cddr formula))) (cadr formula))
	((equal (car formula) 'not) (cdr formula))
	((listp formula) (mapcar 'rule-2 formula))))


(defun convert-rule-3 (formula)
  (if (and (listp formula) (equal (car formula) 'or))
      (cond ((and (atom (cadr formula)) (listp (caddr formula)) (equal (caaddr formula) 'and))
	     (let* ((alpha (cadr formula))
		    (form (caddr formula))
		    (beta (cadr form))
		    (gama (caddr form)))
	       (append '(and) (list (list 'or alpha beta)) (list (list 'or alpha gama)))))
	    ((and (listp (cadr formula)) (equal (caadr formula) 'and) (atom (caddr formula)))
	     (let* ((alpha (caddr formula))
		    (form (cadr formula))
		    (beta (cadr form))
		    (gama (caddr form)))
	       (append '(and) (list (list 'or alpha beta)) (list (list 'or alpha gama)))))
	    (t (append '(or) (list (convert-rule-3 (cadr formula)) (convert-rule-3 (caddr formula))))))
      formula))


