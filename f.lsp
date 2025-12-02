 ; PROBLEMA 7
 ; a). testare daca o lista este liniara - DONE
 ; b). substituie prima aparitie a unui element intr-o lista data - DONE
 ; c). se inlocuieste fiecare sublista a unei liste cu ultimul ei element - DONE
 ; d). interclasarea fara pastrarea dublurilor a doua liste liniare sortate - DONE


; a).
; --------Modelul matematic--------

; verif_liniar(l1..ln) = | adevarat, n=0 
;                        | verif_liniar(l2..ln), n!=0 si l1-liniara
;                        | fals, n!=0 si l1-neliniara

; --------Cod Clisp----------------

(defun verif_liniar(l)
	(cond
		((null l) T)
		((AND (atom (car l)) (NOT (LISTP (car l)))) (verif_liniar (cdr l)))
		(T NIL)
	)
)

; --------Teste--------------------

; (verif_liniar '(1 2 3))  -> T
; (verif_liniar '(1 () 3)) -> NIL
; (verif_liniar '())       -> T
; (verif_liniar '(()))     -> NIL
; (verif_liniar '(1 (2) 3))-> NIL



; b).
; --------Modelul matematic--------
; subs_prima_aparitie(e1,e2,l1..ln)=| [], n=0
;                                   | l1 U subs_prima_aparitie(e1,e2,l2..ln), n!=0 si l1!=e1
;                                   | e2 U l2..ln, n!=0 si l1==e1

; ap_elem(e, l1..ln)=| fals, n=0 
;                    | false, l1!=e, n=1
;                    | true, l1=e, n>=1
;                    | ap_elem(e, l1) OR ap_elem(e, l2..ln), l1!=e, n>=1 si l1-lista

; subs_prima_aparitie_niv(e1,e2,l1..ln)=| [], n=0
;                                       | l1 U subs_prima_aparitie_niv(e1,e2,l2..ln), n!=0 si l1!=e1 si ap_elem(e, l1)=fals
;                                       | subs_prima_aparitie_niv(e1,e2,l1) U l2..ln, n!=0 si l1!=e1 si ap_elem(e, l1)=adevarat
;                                       | e2 U l2..ln, n!=0 si l1==e1

; --------Cod Clisp----------------

(defun ap_elem(e l)
(cond
		((null l) NIL)
		((EQUAL l e) T)
		((AND (ATOM l) (NOT(EQUAL e l))) NIL)
		((EQUAL (car l) e) T)
		(T (OR (ap_elem e (car l )) (ap_elem e (cdr l ))))
	)
)

(defun subs_prima_aparitie(e1 e2 l)
	(cond
		((null l) NIL)
		((EQUAL (car l) e1) (cons e2 (cdr l)))
		(T (cons (car l) (subs_prima_aparitie e1 e2 (cdr l))))
	)
)

(defun subs_prima_aparitie_niv(e1 e2 l)
	(progn
	;(print l)
	(cond
		((null l) NIL)
		((EQUAL (car l) e1) (cons e2 (cdr l)))
		((EQUAL NIL (ap_elem e1 (car l))) (cons (car l) (subs_prima_aparitie_niv e1 e2 (cdr l))))
		(T (cons (subs_prima_aparitie_niv e1 e2 (car l)) (cdr l)))
	)
	)
)

; --------Teste--------------------

; (subs_prima_aparitie '1 '2 '(1 2 1)) -> (2 2 1)
; (subs_prima_aparitie '7 '2 '(1 2 3)) -> (1 2 3)
; (subs_prima_aparitie '1 '2 '(3 2 1)) -> (3 2 2)
; (subs_prima_aparitie '1 '2 '(1 1 1)) -> (2 1 1)
; (subs_prima_aparitie '1 '2 '())      -> NIL
; (subs_prima_aparitie '1 '(A) '(1 2 1))->((A) 2 1)
; (subs_prima_aparitie '1 '2 '(1))     -> (2)

; (ap_elem '7 '(1 2 (7)))
; (ap_elem '7 '(7 2 3))
; (ap_elem '7 '(1 2 (3 (7))))
; (ap_elem '7 '())
; (ap_elem '7 '3)

; (subs_prima_aparitie_niv '7 '2 '(1 2 (7 7))) -> (1 2 (2 7))
; (subs_prima_aparitie_niv '7 '2 '((7) 2 (7 7))) -> ((2) 2 (7 7))
; (subs_prima_aparitie_niv '7 '2 '((7))) -> ((2))
; (subs_prima_aparitie_niv '3 '2 '((7))) -> ((7))
; (subs_prima_aparitie_niv '3 '2 '()) -> NIL

; c).
; --------Modelul matematic--------

; subs_ultim(l1..ln) = | [], n=0
;                      | l1, n=1 si l1-atom
;                      | subs_ultim(l1), n=1 si l1-lista
;                      | subs_ultim(l2..ln), n>1

; subs_ultim_p(l1..ln) = | [], n=0
;                        | l1 U subs_ultim_p(l2..ln), n!=0 si l1-atom
;                        | subs_ultim(l1) U subs_ultim_p(l2..ln), n!=0 si l1-lista

; --------Cod Clisp----------------

(defun subs_ultim(l)
	(progn
	;(print l)
	(cond
		((null l) NIL)
		((atom l) l)
		((null (cdr l)) (subs_ultim(car l)))
		(T (subs_ultim(cdr l)))
	)
	)
)

(defun subs_ultim_p(l)
	(cond 
		((null l) NIL)
		((atom (car l)) (cons (car l) (subs_ultim_p(cdr l))))
		(T (cons(subs_ultim (car l)) (subs_ultim_p(cdr l))))
	)
)

; --------Teste--------------------
; (subs_ultim_p '(1 2 3)) -> (1 2 3)
; (subs_ultim_p '((1) 2 3)) -> (1 2 3)
; (subs_ultim_p '((1 2 3) 4 5)) -> (3 4 5)
; (subs_ultim_p '(a (b c)(d (e (f))))) -> (a c f)
; (subs_ultim_p '(a (b c)(d ((e) f)))) -> (a c f)

; (a (b c)(d (e (f)))) -> (a c (e(f))) -> (a c (f)) -> (a c f) 
; (a (b c)(d ((e) f))) -> (a c ((e) f)) -> (a c f)

; (subs_ultim '(d (e (f)))) -> f
; (subs_ultim '(d)) -> d
; (subs_ultim '(d e)) -> e
; (subs_ultim '()) ->nil



; d).
; --------Modelul matematic--------

; elim_inceput(l1..ln, e)= | [], n=0
;                          | elim_inceput(l2..ln), n!=0 si l1==e
;                          | l, n!=0 si l1!=e ; ori nu mai avem aparitii de e
											  ; ori nu am avut niciuna

; interclasare(l1..ln, r1..rm) = | r, n=0 
;                                | l, m=0
;                                | l1 U interclasare(
;							     |    elim_inceput(l2..ln, l1), 
;								 |    elim_inceput(r1..rm, l1)), l1<r1, n!=0 si m!=0
;                                | r1 U interclasare(
;							     |    elim_inceput(l1..ln, r1), 
;								 |    elim_inceput(r2..rm, r1)), l1>=r1, n!=0 si m!=0

; --------Cod Clisp----------------

(defun elim_inceput(l e)
	(cond
		((null l) nil)
		((equal(car l) e) (elim_inceput (cdr l) e))
		(T l)
	)
)

(defun interclasare(l r)
	(progn
	;(print l)
	;(print r)
	(cond
		((and (null l) (null r)) nil)
		((null l) (cons (car r) (interclasare l (elim_inceput (cdr r) (car r)))))
		((null r) (cons (car l) (interclasare (elim_inceput (cdr l) (car l)) r)))
		(
			(< (car l) (car r)) 
			(cons (car l) (interclasare (elim_inceput (cdr l) (car l)) 
				                        (elim_inceput r (car l))))
		)
		(T 
			(cons (car r) (interclasare (elim_inceput l (car r)) 
			                           (elim_inceput (cdr r) (car r))))
		)
	)
	)
)

; --------Teste--------------------

; (elim_inceput '(1 1 2) '1) -> (2)
; (elim_inceput '(1 1 2) '3) -> (1 1 2)
; (elim_inceput '(1 1) '1)   -> NIL
; (elim_inceput '() '1)      -> NIL

; (interclasare '(1 1 2 4 4 6) '(5 6 7)) -> (1 2 4 5 6 7)
; (interclasare '() '(5 6 7)) -> (5 6 7)
; (interclasare '(5 6 7) '()) -> (5 6 7)
; (interclasare '(8 9) '(5 6 7)) -> (5 6 7 8 9)
; (interclasare '() '(5 5 6 7 7)) -> (5 6 7)
; (interclasare '(5 5 6 7 7) '()) -> (5 6 7)
; (interclasare '() '()) -> NIL