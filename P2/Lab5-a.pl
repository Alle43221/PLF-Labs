% LAB3 - problema 5a.
% formarea unei liste cu pozitiile pe care apare cel mai mare element din sir


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

% poz_l(L: lista, E: integer, Rez: lista, Poz: integer)
% L - lista de numere in care vom cauta elementul E
% E - un element care exista in lista L
% Rez - lista pozitiilor pe care se afla elementul E in lista L
% Poz - pozitia curenta in lista L
% model de flux (i, i, o, i) sau (i, i, i, i)

poz_l([], _, [], _).
poz_l([L|H], E, [Poz|Rez], Poz):-
          L=E, !,
          Poz1 is Poz+1,
          poz_l(H, E, Rez, Poz1).
poz_l([_|H], E, Rez, Poz):-
          Poz1 is Poz+1,
          poz_l(H, E, Rez, Poz1).

% lista_poz(L: lista, Rez: lista) 
% L - lista de numere pentru care vom crea lista Rez cu pozitiile celui mai mare element
% Rez - lista pozitiilor pe care se afla cel mai mare element in lista L
% model de flux (i, i) sau (i, o)

lista_poz([], []):-!.
lista_poz(L, Rez):-
        elem_maxim(L, M), !,
        poz_l(L, M, Rez, 1).

% ------------------------------------------------------------------------------

% Teste
% lista_poz([], []) -> true.
% lista_poz([], Rez) -> Rez=[]
% lista_poz([2], Rez) -> Rez=[1]
% lista_poz([3,2,1], Rez) -> Rez=[1]
% lista_poz([3,3,1], Rez) -> Rez=[1,2]
% lista_poz([3,3,3], Rez) -> Rez=[1,2,3]
% lista_poz([3,2,1], [1]) -> true.
% lista_poz([3,3,1], [1, 2]) -> true.
% lista_poz([3,3,3], [1, 2, 3]) -> true.

% ------------------------------------------------------------------------------

% Model matematic

% elem_maxim(l1...ln)= | l1                    ,n=1 
%                      | elem_maxim(l2...ln)   ,elem_maxim(l2...ln)>l1 si n!=1
%                      | l1                    ,elem_maxim(l2...ln)<=l1 si n!=1

% poz_l(l1...ln, e, poz_curent) = | [], n=0
%                                 | poz_l(l2..ln, e, ind+1), n!=0 si l1!=e
%                                 | ind ⨁ poz_l(l2..ln, e, ind+1), n!=0 si l1=e

% lista_poz(l1...ln) = poz_l(l1...ln, elem_maxim(l1...ln), 1)

