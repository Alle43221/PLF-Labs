 ; PROBLEMA 5
 ; Sa se intoarca adancimea la care se afla un nod intr-un arbore de tipul (1)
 ; (nod nr_subarbori lista-subarbore-1 lista-subarbore-2)
 ; (A 2 B 0 C 2 D 0 E 0)


; --------Modelul matematic--------

; stang(l) = parcurg_stang(l, 0, 0) 

; drept(l) = parcurg_drept(l, 0, 0)

; parcurg_stang(l1..ln, n, m)= | [], n=0
;                              | [], n=m+1, n!=0
;                              | l1 U l2 U parcurg_stang(l3..ln, n+1, m+l2), altfel

; parcurg_drept(l1..ln, n, m)= | [], n=0
;                              | l1..ln, n=m+1, n!=0
;                              | parcurg_drept(l3..ln, n+1, m+l2), altfel

; cautare_nod(l1..ln, nivel, elem) = | -1, n=0
;                                    | nivel, l1=elem, n!=0
;                                    | cautare_nod(stang(l), nivel+1, elem), l1!=elem, 
;                                    |                       n!=0 si cautare_nod(drept(l))=-1
;                                    | cautare_nod(drept(l), nivel+1, elem), altfel


; determinare_nivel(l, elem) = cautare_nod(l, 0, elem)

; --------Cod Clisp----------------

(defun determinare_nivel (l elem)
	(cautare_nod l 0 elem)
)

(defun cautare_nod (l nivel elem)
	(cond
		((null l) -1)
		((EQUAL (car l) elem) nivel)
		((EQUAL (cautare_nod (stang l) (+ nivel 1) elem) -1) 
			(cautare_nod (drept l) (+ nivel 1) elem))
		(T (cautare_nod (stang l) (+ nivel 1) elem))
	)
)

(defun parcurg_drept (ab n m)
	(cond
		((null ab) nil)
		((= n (+ m 1)) ab)
		(T (parcurg_drept (cddr ab) (+ n 1) (+ (cadr ab) m)))
	)
)
(defun drept (ab)
	(parcurg_drept (cddr ab) 0 0)
)

(defun parcurg_stang (ab n m)
	(cond
		((null ab) nil)
		((= n (+ m 1)) nil)
		(T (cons (car ab) 
			(cons
				 (cadr ab) 
				 (parcurg_stang (cddr ab) (+ 1 n) (+ (cadr ab) m)))
			)
		)
	)
)

(defun stang (ab)
	(parcurg_stang (cddr ab) 0 0)
)


; --------Teste--------------------

; 0     A   
; 1   B   C
; 2      D E

; (determinare_nivel '(A 2 B 0 C 2 D 0 E 0) 'F) -> -1 (nu exista)
; (stang '(A 2 B 0 C 2 D 0 E 0)) -> (B 0)
; (drept '(A 2 B 0 C 2 D 0 E 0)) -> (C 2 D 0 E 0)
; (determinare_nivel '(A 2 B 0 C 2 D 0 E 0) 'A) -> 0
; (determinare_nivel '(A 2 B 0 C 2 D 0 E 0) 'B) -> 1
; (determinare_nivel '(A 2 B 0 C 2 D 0 E 0) 'C) -> 1
; (determinare_nivel '(A 2 B 0 C 2 D 0 E 0) 'D) -> 2
; (determinare_nivel '(A 2 B 0 C 2 D 0 E 0) 'E) -> 2
; (determinare_nivel '(A 2 B 1 H 1 I 1 J 0 C 2 D 2 F 1 K 0 G 0 E 0) 'E) -> 2