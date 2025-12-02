
% verif_ap(L: lista, E: integer)
% L - lista de numere in care vom cauta elementul
% E - elementul de cautat
% model de flux (i,o) sau (i,i) sau (o, i) sau (o, o)

verif_ap([H|_], E):-
		H=E.
verif_ap([H|T], E):-
		H\=E,!,
		verif_ap(T, E).

% ------------------------------------------------------

% el_elem(L: lista, E: integer, R: lista)
% L - lista de numere din care vom elimina elementul E
% A - elementul care trebuie eliminat
% R - rezultatul (lista obtinuta dupa prelucrare)
% model de flux (i,i,o) sau (i,i,i)

el_elem([H|T],E,L):-
	H=E,!,
	el_elem(T,E,L).
el_elem([H|T],E,[H|L]):-
	el_elem(T,E,L).
el_elem([], _, []).

% ------------------------------------------------------


% el_rep(L: lista, R: lista)
% L - lista de numere din care vom elimina repetitiile
% R - rezultatul (lista obtinuta dupa prelucrare)
% model de flux (i,o) sau (i,i)

el_rep([H|T], R):-
	verif_ap(T, H),!,
	el_elem([H|T], H, L),
	el_rep(L, R).

% el_rep([H|[]], [H]).
el_rep([], []).
el_rep([H|T], [H|R]):-
	el_rep(T, R).

% -------------------------------------------------------

% Exemple teste:

% el_rep([],A).  -> A=[]
% el_rep([], []). -> true.
% el_rep([], [A]). -> false.
% el_rep([1], A). -> A=[1]
% el_rep([A],[A]). -> true.
% el_rep([1], [2]). -> false.
% el_rep([A,A], []). -> true.
% el_rep([1,1], B). -> B=[]
% el_rep([1, 2], C). -> C=[1,2].
% el_rep([1, 2], [1]). -> false.
% el_rep([1, 1, 2], C). -> C=[2].	
% el_rep([1, 2, 1], C). -> C=[2].
% el_rep([2, 1, 1], C). -> C=[2].
% el_rep([1, 2, 2], [1]). -> true.
% el_rep([A, A], [A]). -> false.	

% -------------------------------------------------------

% Model matematic:

% eliminare a elementelor care se repeta in lista

% el_rep(l1...ln) =      | []                    ,n=0
%                        | [l1]                  ,n=1 
%                        | l1 ⨁ el_rep(l2...ln, l1)    ,n>1 si verif_ap(l2...ln, l1)=fals
%                        | el_rep(el_elem(l2...ln, l1)) ,n>1 si verif_ap(l2...ln, l1)=adevarat
%
% verif_ap(l1...ln, e) = | fals                  ,n=0
%                        | adevarat              ,n!=0 si l1=e
%                        | verif_ap(l2...ln, e)  ,n!=0 si l1!=e
%
% el_elem(l1...ln, e) =      | []                         ,n=0
%						     | el_elem(l2...ln, e)        ,n!=0 si l1!=el_rep
%							 | l1 ⨁ el_elem(l2...ln, e,) ,n!=0 si l1=el_rep