

% find(list of numbers, el) (i, o) - determinist 
find([E], E) :- 
       !. 
find([H|T], M) :- 
       find(T, M), 
       M =< H, 
       !. 
find([H|_], H). 
 
% del(list of numbers, list of numbers) (i, o) - determinist 
del([],[]). 
del([H|T], T) :- 
       find([H|T], M), 
       H is M, 
       !. 
 
del([H|T], [H|Rez]) :- 
       del(T, Rez). 


% sP(L:list of numbers, L: list of number) 
% (i,o) – nondeterm 
sP([],[]). 
sP([_|T],S):-sP(T,S). 
sP([H|T],[H|S]):- H mod 2 =:=0,  
              !,   
              sP(T,S). 
sP([H|T],[H|S]):-sI(T,S).

% sI(L:list of numbers, L: list of number) 
% (i,o) – nondeterm 
sI([H],[H]):-H mod 2 =\=0, !. 
sI([_|T],S):-sI(T,S). 
sI([H|T],[H|S]):-H mod 2 =:=0,  
              !,  
              sI(T,S). 
sI([H|T],[H|S]):-sP(T,S).  

% g(L:list, E: element, LRez: list) 
% (i, i, o) – nedeterminist 
g([H|_], E, [E,H]). 
g([_|T], E, P):- 
g(T, E, P). 


% f(L:list, LRez: list) 
% (i, o) – nedeterminist 
f([H|T],P):- 
g(T, H, P). 
f([_|T], P):- 
f(T, P). 

nr_impare_lista([], 0).
nr_impare_lista([H|T], P):-
       H mod 2 =:= 0,!, nr_impare_lista(T, P1), P is P1.
nr_impare_lista([_|T], P):-nr_impare_lista(T, P1), P is P1+1.

stergere_n(L, Rez):-
       stergere_aux(L, 1, 2, Rez).

stergere_aux([], _, _, []):-!.

stergere_aux([_|T], M, M, Rez):-!,
       Poz1 is M+1,
       M1 is M+4,
       stergere_aux(T, Poz1, M1, Rez).
stergere_aux([H|T], Poz, M, [H|Rez]):-Poz1 is Poz+1, stergere_aux(T, Poz1, M, Rez).

% stergere_n([1,2,3,4,5,6,7,8], P).

lista_subm_k(L, 1, [Rez]):-candidat(L, Rez).
lista_subm_k(L, 2, [Nou, I1]):-
       candidat(L, Nou),
       lista_subm_k(L, 1, [I1]),
       Nou<I1.
 lista_subm_k(L, K, [Nou, A, B|Rez]):-
       K>2, K1 is K-1,
       candidat(L, Nou),
       lista_subm_k(L, K1, [A,B|Rez]),
       R1 is B-A,
       R2 is A-Nou,
       R1 =:= R2.

rezolva(L, K, X):-findall(Rez, lista_subm_k(L,K, Rez), X).

% lista_subm_k([1,5,2,9,3], 2, Rez).
% rezolva([1,5,2,9,3], 2, X).
% rezolva([3,2,1], 2, X).

candidat([H|_], H).
candidat([_|T], Rez):-candidat(T, Rez).

r2023([], 0).
r2023([H|T], S):-r2023(T, S1), S1 is S-H.

% r2023([1,2,3], S)

q2024(b, a).
r2024(a, c).
r2024(b,c).
r2024(a, X):-
       \+q2024(b, X), r2024(b, X).

% r2024(a, X).

