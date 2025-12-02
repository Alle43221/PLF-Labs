
% elem_maxim(L: lista, A: integer)
% L - lista NEVIDA de numere in care vom cauta elementul maxim
% A - rezultatul (elementul maxim din lista)
% model de flux (i,o) sau (i,i) sau (o, i) sau (o, o)

elem_maxim([A|[]], A).
elem_maxim([H|T], A):-
	elem_maxim(T, A1),
	H>A1, !,
	A is H.
elem_maxim([_|T], A):-
	elem_maxim(T, A1),
	A is A1.

% -------------------------------------

% el_elem_maxim(L: lista, R: lista)
% L - lista de numere din care vom elimina elementul maxim
% A - rezultatul (lista fara aparitiile elementului maxim)
% model de flux (i,o) sau (i,i)

el_elem_maxim([H|T], L):-
	elem_maxim([H|T], M),
	el_elem([H|T], M, L).
el_elem_maxim([],[]).

% -------------------------------------

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

% --------------------------------------

% Exemple teste:

% el_elem_maxim([], []). -> true.
% el_elem_maxim([], A). -> A=[]
% el_elem_maxim([], [B]). -> false.
% el_elem_maxim([A], B). -> B=[].
% el_elem_maxim([A], []). -> true.
% el_elem_maxim([A], [B]). -> false.
% el_elem_maxim([1, 1], []). -> true.
% el_elem_maxim([1,1], B). -> B=[]
% el_elem_maxim([1, 1], [B]). -> false.
% el_elem_maxim([1,2], [1]). -> true.
% el_elem_maxim([1,2], [2]). -> false.
% el_elem_maxim([1,2], A). -> A=[1].
% el_elem_maxim([-1, -2], [-2]). -> true.
% el_elem_maxim([-1, -2], [-1]). -> false.
% el_elem_maxim([-1, -2], A). -> A=[-2].
% el_elem_maxim([-1, -2, -1], [-2]). -> true.
% el_elem_maxim([-1, -2, -1], A). -> A=[-2].
% el_elem_maxim([-2, -1, -2], A). -> A=[-2, -2].
% el_elem_maxim([-2, -1, -2], [-2,-2]). -> true.

% --------------------------------------

% Model matematic:

% el_elem(l1...ln, e) =      | []                        ,n=0
%			     | el_elem(l2...ln, e)       ,n!=0 si l1=e
%			     | l1 ⨁ el_elem(l2...ln, e)  ,n!=0 si l1!=e
% 
% elem_maxim(l1...ln)= | l1                    ,n=1 
%                      | elem_maxim(l2...ln)   ,elem_maxim(l2...ln)>l1 si n!=1
%	               | l1                    ,elem_maxim(l2...ln)<=l1 si n!=1
%
% el_elem_maxim(l1...ln) = | []                                     ,n=0
%			   | el_elem(l1...ln, elem_maxim(l1...ln))  ,n!=0
