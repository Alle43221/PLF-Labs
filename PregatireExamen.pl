% pregatire examen

% --------------------------- 1 b ------------------------------

% Sa se scrie un predicat care adauga intr-o lista dupa fiecare element par valoarea 1

% modif_lista(l1..ln) = |[], n=0
%                       |l1 + 1 + modif_lista(l2..ln), daca n!=0 si l1 par
%                       |l1 + modif_lista(l2..ln), daca n!=0 si l1 impar


% modif_lista([], []).

% modif_lista([H|T], [H|Rez]):
%	mod(H,2)=:=1,
%   modif_lista(T, Rez).

% modif_lista([H|T], [H, 1|Rez]):
%	mod(H,2)=:=0,
%   modif_lista(T, Rez).

% ---------------------------- 1 a ----------------------------

% Sa se scrie un predicat care intoarce diferenta a doua multimi

% dif(l1..ln, k1..km) = | [], daca n=[]
%                       | l1 + dif(l2..ln, k1..kn), daca l1 nu apartine lui k
%                       | dif(l2..ln, k1..kn), daca l1 apartine lui k

 apare([H|T], E):-
	H=\=E,
	apare(T, E).

 apare([H|_], E):-
	H=E.

% dif([], _, []).
% dif([H|T], B, [H|Rez]):-
%	\+ apare(B, H),
%	dif(T, B, Rez).
% dif([H|T], B, Rez):-
%	apare(B, H),
%	dif(T, B, Rez).

% ---------------------------- 2 b ----------------------------

% Sa se scrie un predicat care adauga dupa elementele de pe pozitii
% puteri ale lui doi o valoare v data

% putere_doi(n)= | adevarat, n=1
%                | fals, n%2=1 si n!=1
%                | putere_doi(n/2), n%2=0 si n!=1

% adaug(l1..ln, ind, v) = | [], n=0
%                         | l1 + v + adaug(l2..ln, ind+1, v), ind putere de doi si n!=0
%						  | l1 + adaug(l2..ln, ind+1, v), ind nu e putere de doi si n!=0

putere_doi(N):-
          N>1,!,
          mod(N,2)=:=0,
          N1 is N/2, 
          putere_doi(N1).
putere_doi(N):-N=:=1.

adaug([], _,_, []).
adaug([H|T], I, V, [H, V|Rez]):-
	putere_doi(I),
	adaug(T, I+1, V, Rez).

adaug([H|T], I, V, [H|Rez]):-
	\+ putere_doi(I),
	adaug(T, I+1, V, Rez).

adaug_v(L, V, Rez):-adaug(L, 1, V, Rez).

% ---------------------------- cmmdc ----------------------------

% CMMDC

% cmmdc(A, B)= |A, B=0
%              |cmmdc(B, A%B), B!=0

% ---------------------------- P2 10 a ------------------------

% adauga dupa al 1 lea element, 3, 7, 15-lea etc un element dat e



adaug([H|T], I, C, E, [H, E | Rez]):-
        I=:=C,
        I1 is I+1,
        C1 is C+C+1,
        adaug(T, I1, C1, E, Rez).
adaug([H|T], I, C, E, [H | Rez]):-
        I=\=C,
        I1 is I+1,
        adaug(T, I1, C, E, Rez).
adaug([], _, _, _, []).

% adaug([1,2,3,4,5,6,7], 1, 1, 9, Rez).

% ---------------------------- P2 13 a ------------------------

% sa se adauge intr-o lista dupa fiecare element divizorii lui

adaug_div([], _, []).
adaug_div([_], 0, []).

adaug_div([H|T], Nr, [Nr|Rez]):-
	Nr>=1,
	mod(H, Nr)=:=0,
	Nr1 is Nr-1,
	adaug_div([H|T], Nr1, Rez).

adaug_div([H|T], Nr, Rez):-
	Nr>=1,
	mod(H, Nr)=\=0,
	Nr1 is Nr-1,
	adaug_div([H|T], Nr1, Rez).

adaug_div([_,E|T], 0, [E|Rez]):-
	adaug_div([E|T], E, Rez).

% adaug_div([0, 1,6, 12], 0, Rez).

% ---------------------------- P2 2,3 a ------------------------

elem_min([H, E|T], H):-
	elem_min([E|T], Rez),
	H<Rez.

elem_min([H, E|T], Rez):-
	elem_min([E|T], Rez),
	H>=Rez.
elem_min([E], E).

elim_elem([], _, []).
elim_elem([H|T], E, [H|Rez]):-
	H=\=E,
	elim_elem(T, E, Rez).
elim_elem([H|T], E, Rez):-
	H=:=E,
	elim_elem(T, E, Rez).

elim_prim_elem([E|T], E, T).
elim_prim_elem([], _, []).
elim_prim_elem([H|T], E, [H|Rez]):-
	H=\=E,
	elim_prim_elem(T, E, Rez).

sortare([], []).
sortare(T, [Min|Rez]):-
	elem_min(T, Min),
	elim_prim_elem(T, Min, L),
	sortare(L, Rez).

sortare_fara_repetitii([], []).
sortare_fara_repetitii(T, [Min|Rez]):-
	elem_min(T, Min),
	elim_elem(T, Min, L),
	sortare_fara_repetitii(L, Rez).


% ------------------------------- P2 14 a -----------------------

predecesor([H|T], [Dif1|Rez], F):-
	Dif is H-F,
	Dif<0,
	Dif1 is Dif+10,
	predecesor(T, Rez, 1).

predecesor([H|T], [Dif|Rez], F):-
	Dif is H-F,
	Dif>=0,
	predecesor(T, Rez,0).
predecesor([], [], _).

inserare_la_capat([], E, [E]).
inserare_la_capat([L|T], E, [L|Rez]):-inserare_la_capat(T, E, Rez).

inversare_lista([], []).
inversare_lista([H|L], Rez):-
    inversare_lista(L, Rez1),
	inserare_la_capat(Rez1, H, Rez).

inversare([], Col, Col).
inversare([H|T], Col, Rez):-
	inversare(T, [H|Col], Rez).
	
precedent(L, Ras1):-
	inversare(L, [], Rez),
	predecesor(Rez, Ras, 1),
	inversare(Ras, [], Ras1).

% ---------------------------- P2 8 a ---------------------------

urmator([H|T], [Dif1|Rez], F):-
	Dif is H+F,
	Dif>9,
	Dif1 is Dif-10,
	urmator(T, Rez, 1).

urmator([H|T], [Dif|Rez], F):-
	Dif is H+F,
	Dif=<9,
	urmator(T, Rez,0).
urmator([], [1], 1).
urmator([], [], 0).

succesor(L, Ras1):-
	inversare(L, [], Rez),
	urmator(Rez, Ras, 1),
	inversare(Ras, [], Ras1).

% -----------------------  P2 7 a -----------------------------

inmultire([H|T], [Dif1|Rez], F, Val):-
	Dif is Val*H + F,
	Dif>9,
	Dif1 is mod(Dif, 10),
	Flag is Dif//10,
	inmultire(T, Rez, Flag, Val).

inmultire([H|T], [Dif|Rez], F, Val):-
	Dif is Val*H + F,
	Dif=<9,
	inmultire(T, Rez, 0, Val).
inmultire([], [Flag], Flag, _).
inmultire([], [], 0, _).

multiply(L, Val, Ras1):-
	inversare(L, [], Rez),
	inmultire(Rez, Ras, 0, Val),
	inversare(Ras, [], Ras1).

% -----------------------  P2 4 a -----------------------------

elim_inceput([H|T], E, Rez):-
	H = E,
	elim_inceput(T, E, Rez).
elim_inceput([H|T], E, [H|T]):-
	H=\=E.
elim_inceput([], _, []).

interclasare([],[],[]).
interclasare([], [H|T], [H|Rez]):-
	elim_inceput(T, H, Rez1),
	interclasare([], Rez1, Rez).
interclasare([H|T], [], [H|Rez]):-
	elim_inceput(T, H, Rez1),
	interclasare([], Rez1, Rez).

interclasare([H1|T1], [H2|T2], [H2|Rez]):-
	H2<H1,
	elim_inceput(T2, H2, Rez2),
	interclasare([H1|T1], Rez2, Rez).

interclasare([H1|T1], [H2|T2], [H1|Rez]):-
	H1=<H2,
	elim_inceput(T1, H1, Rez1),
	elim_inceput([H2|T2], H1, Rez2),
	interclasare(Rez1, Rez2, Rez).

% ----------------------- P1 inserarea pe pozitia a n-a in lista

insert([H|T], N, Val, [Val|[H|T]]):-
	N = 1.

insert([H|T], N, Val, [H|Rez]):-
	N =\= 1,
	N1 is N-1,
	insert(T, N1, Val, Rez).

% ------------------------- P1 suma alternanta l1 - l2 + l3 - ...

suma_alt([], _, 0).
suma_alt([H|T], 1, Sum1):-
	suma_alt(T, 0, Sum),
	Sum1 is Sum+H.
suma_alt([H|T], 0, Sum1):-
	suma_alt(T, 1, Sum),
	Sum1 is Sum-H.

% ------------------------- P2 stergere secv val consecutive

sterge_cons([],_,[]).
sterge_cons([H1,H2|T], _, Rez):-
	H2 =:= H1+1,
	sterge_cons([H2|T], 1, Rez).

sterge_cons([H1,H2|T], 1, Rez):-
	H2 =\= H1+1,
	sterge_cons([H2|T], 0, Rez).

sterge_cons([H1,H2|T], 0, [H1|Rez]):-
	H2 =\= H1+1,
	sterge_cons([H2|T], 0, Rez).

sterge_cons([H1|[]], 0, [H1]).

sterge_cons([_|[]], 1, []).

% ------------------------- P1 transformarea unei lista in multime in ordinea ultimei aparitii


trans([], []).
trans([H|T], Col):-
	apare(T, H),
	trans(T, Col).

trans([H|T], [H|Col]):-
	\+ apare(T, H),
	trans(T, Col).

trans_multime(L, Rez):-
	trans(L, Rez1),
	inversare(Rez1, [], Rez).

% ------------------------- P1 transformarea unei lista in multime in ordinea primei aparitii


trans_invers([], []).
trans_invers([H|T], [H|Col]):-
	elim_elem(T, H, Rez),
	trans_invers(Rez, Col).


% ------------------------- P2 inlocuirea unui element cu elementele unei liste

ls_concat([], L, L).
ls_concat([H|T], L, [H|Rez]):-ls_concat(T, L, Rez).

elem_lista([], _, _, []).
elem_lista([H|T], E, List, [H|Rez]):-
    E =\= H,
	elem_lista(T, E, List, Rez).
elem_lista([E|T], E, List, Rez):-
	elem_lista(T, E, List, Rez1),
	ls_concat(List, Rez1, Rez).

% ------------------------- P2 suma a doua nr lista

  adunare([H1|T1],[H2|T2],[Dif1|Rez], F):-
	Dif is H1+H2+F,
	Dif1 is mod(Dif,10),
	F1 is div(Dif,10),
	adunare(T1, T2, Rez, F1).

  adunare([],[H2|T2], [Dif1|Rez], F):-
	Dif is H2+F,
	Dif1 is mod(Dif,10),
	F1 is div(Dif,10),
	adunare([], T2, Rez, F1).

  adunare([H2|T2], [], [Dif1|Rez], F):-
	Dif is H2+F,
	Dif1 is mod(Dif,10),
	F1 is div(Dif,10),
	adunare(T2, [], Rez, F1).

  adunare([], [], [F], F).

  adunare_liste(L, K, Ras1):-
	inversare(L, [], Rez),
	inversare(K, [], Rez1),
	adunare(Rez, Rez1, Ras, 0),
	inversare(Ras, [], Ras1).


% ------------------------- P2 cea mai lunga secventa de numere pare consecutive

% verificarile pentru final
secv_pare([],Candidat, _, Len, Max, Candidat, _):-
	Max<Len.
secv_pare([],_, Candidat_Maxim, Len, Max, Candidat_Maxim, _):-
	Max>=Len.

% par dar nu crescator, fara actualizare maxim
secv_pare([H|T], _, Candidat_Maxim, Len, Max, Rez, Ant):-
	0 =:= mod(H, 2),
	Ant =\= -1,
	H =\= Ant+2,
	Len=<Max,
	secv_pare(T, [H], Candidat_Maxim, 1, Max, Rez, H).

% par dar nu crescator, cu actualizare maxim
secv_pare([H|T], Candidat, _, Len, Max, Rez, Ant):-
	0 =:= mod(H, 2),
	Ant =\= -1,
	H =\= Ant+2,
	Len>Max,
	secv_pare(T, [H], Candidat, 1, Len, Rez, H).

% par dar nu primul
secv_pare([H|T], Candidat, Candidat_Maxim, Len, Max, Rez, Ant):-
	0 =:= mod(H, 2),
	Ant =\= -1,
	H =:= Ant+2,
	L1 is Len+1,
	secv_pare(T, [H|Candidat], Candidat_Maxim, L1, Max, Rez, H).

% par dar primul
secv_pare([H|T], Candidat, Candidat_Maxim, Len, Max, Rez, Ant):-
    0 =:= mod(H, 2),
    Ant =:= -1,
	L1 is Len+1,
	secv_pare(T, [H|Candidat], Candidat_Maxim, L1, Max, Rez, H).

% impar cu actualizare maxim
secv_pare([H|T], Candidat, _, Len, Max, Rez, _):-
	1 =:= mod(H, 2),
	Len>Max,
	secv_pare(T, [], Candidat, 0, Len, Rez, -1).

% impar fara actualizare maxim
secv_pare([H|T], _, Candidat_Maxim, Len, Max, Rez, _):-
	1 =:= mod(H, 2),
	Len=<Max,
	secv_pare(T, [], Candidat_Maxim, 0, Max, Rez, -1).

det_secv_pare(L, Rez1):-
	secv_pare(L, [], [], 0,0,Rez, -1),
	inversare(Rez, [], Rez1).

% TO DO
% munte/vale