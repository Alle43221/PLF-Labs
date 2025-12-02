% Examen
% Definiti un predicat care, dintr-o lista de numere, 
% produce o lista de perechi (numar, n),
% unde numar apare in lista initiala de n ori.

% Modele matematice ----------------------------------------------

% elim_elem(l1..ln, elem)=| [], n=0
%                         | l1 + elim_elem(l2..ln), n!=0 si l1!=elem
%                         | elim_elem(l2..ln), n!=0 si l1=elem

% numarare_ap(l1..ln, elem)=| 0, n=0
%                           | 1 + numarare_ap(l2..ln), n!=0 si l1=elem
%                           | numarare_ap(l2..ln), n!=0 si l1!=elem

% predicat(l1..ln) =| [], n=0
%                   | [l1, numarare_ap(l1..ln)] + predicat(elim_elem(l2..ln, l1)), n!=0

% Implementare Prolog ----------------------------------------------

% elim_elem(L: lista eterogena de intregi, E: intreg, Rez: lista eterogena de intregi)
% L -> lista din care se va elimina elementul E
% E -> elementul caruia trebuie sa ii eliminam toate aparitiile din lista L
% Rez -> lista ce contine rezultatul eliminarii tuturor aparitiilor lui E din lista L
% model de flux: (i, i, o) determinist
elim_elem([], _, []).
elim_elem([H|T], E, Rez):-
	H=E,
	elim_elem(T, E, Rez).
elim_elem([H|T], E, [H|Rez]):-
	H=\=E,
	elim_elem(T, E, Rez).

% numarare_ap(L: lista eterogena de intregi, E: intreg, Rez: intreg)
% L -> lista in care trebuie sa numaram aparitiile lui E
% E -> elementul caruia ii vom numara aparitiile in lista L
% Rez -> numarul de aparitii ale lui E in lista L
% model de flux: (i, i, o) determinist
numarare_ap([], _, 0).
numarare_ap([H|T], E, Rez1):-
	H=E,
	numarare_ap(T, E, Rez),
	Rez1 is Rez+1.

numarare_ap([H|T], E, Rez):-
	H=\=E,
	numarare_ap(T, E, Rez).

% predicat(L:lista eterogena de intregi, Rez: Lista de perechi de intregi)
% L -> lista pe baza careia vom crea un vector de frecventa
% Rez -> vectorul de frecventa creat pe baza elementelor din lista L
% model de flux: (i, o) determinist
predicat([], []).
predicat([H|T], [[H,Ap]|Rez]):-
	numarare_ap([H|T], H, Ap),
	elim_elem(T, H, Elim),
	predicat(Elim, Rez).

% Teste ------------------------------------------------------------

% elim_elem([], 2, Rez).
% elim_elem([1, 1, 5, 4, 1],1, Rez).
% elim_elem([1, 1, 5, 4, 1],7, Rez).
% elim_elem([1,1,1], 1, Rez).

% numarare_ap([], 2, Rez).
% numarare_ap([1], 1, Rez).
% numarare_ap([1,2,4,2,5], 2, Rez).
% numarare_ap([1,2,4,2,5], 7, Rez).
% numarare_ap([7,7,7], 7, Rez).

% predicat([11,22,11,22,11,33,11], Rez).
% predicat([], Rez).
% predicat([11,11,11,11], Rez).

% Teste bilet
% predicat([11,22,11,22,11,33,11], Rez).
% predicat([1,2,3,4,5], Rez).
% predicat([1,1,1,1,1], Rez).
% predicat([1,2,1,3,2,1,5], Rez).




