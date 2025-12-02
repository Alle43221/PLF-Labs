% Lab7 - problema 4

% Se dau doua numere naturale n si m 
% Se cere sa se afiseze in toate modurile posibile toate numerele de la 1 la n, 
% astfel incat intre oricare doua numere afisate pe pozitii consecutive,
% diferenta in modul sa fie >= m


% ---------------------------------------------------------------------------

% Program Prolog

% candidat(N: intreg, Rez: intreg)
% N -> limita superioara a valorilor lui Rez
% Rez -> un numar din intervalul [1, n]
% model de flux (i, o) nedeterminist
candidat(N, N).
candidat(N, Rez):-
        N>1,
        N1 is N-1,
        candidat(N1, Rez).

% nu_apare(L: lista eterogena de intregi, E: intreg)
% L -> lista in care cautam elementul E
% E -> elementul de cautat in lista L
% model de flux (i, i) determinist
nu_apare([], _).
nu_apare([H|T], E):-
        H=\=E,
        nu_apare(T, E).

% permutari_aux(N: intreg, M: intreg, Rez: lista eterogena de intregi, Len: intreg, Col: lista eterogena de intregi)
% N -> numarul de elemente din permutare
% M -> valoarea care va fi mai mica sau egala cu diferenta absoluta dintre doua elemente pe pozitii consecutive din Rez
% Rez -> lista care contine permutarea construita
% Len -> lungimea curenta a permutarii
% Col -> lista in care construim permutarea
% model de flux (i, i, o, i, i) nedeterminist

permutari_aux(N, _, Col, N, Col).

permutari_aux(N, M, Rez, 0, Col):-
        candidat(N, Next),
        permutari_aux(N, M, Rez, 1, [Next|Col]).

permutari_aux(N, M, Rez, Len, [H|T]):-
        candidat(N, Next),
        abs(H-Next)>=M,
        nu_apare([H|T], Next),
        Len1 is Len+1,
        permutari_aux(N, M, Rez, Len1, [Next|[H|T]]).

% permutare_m(N: intreg, M: intreg, Rez: lista eterogena de intregi)
% N -> numarul de elemente din permutare
% model de flux (i, i, o) nedeterminist
permutare_m(N, M, Rez):- 
    permutari_aux(N, M, Rez, 0, []).

colectare_toate(N, M, Rez):- findall(X, permutare_m(N, M, X), Rez).


% ---------------------------------------------------------------------------

% Teste

% nu_apare([1,2,3], 3). -> true.
% nu_apare([1,2,3], 4). -> false.
% candidat(5, Rez).
% colectare_toate(5, 2, Rez), write(Rez).
% colectare_toate(5, 1, Rez), write(Rez).

% ---------------------------------------------------------------------------

% Modele matematice si modelul recursiv pentru generare

% candidat ne va genera backtracking-ul pentru alegerea elementelor din permutare
% candidat(n) = 1. n
%               2. candidat(n-1), daca n>1

% verifica daca elem NU apare ca element in lista l
% nu_apare(l1..ln, elem) = | adevarat, n=0
%                          | fals, n!=0 si l1=elem
%                          | nu_apare(l1..ln, elem), n!=0 si l1!=elem

% alegem o abordare bottom-up
% permutari_aux(n, M, lungime) = | [candidat(n)], lungime=1
%                                | candidat(n) ⨁ permutari_aux(n, M, lungime-1), daca
%                                               lungime!=1, 
%                                               abs(candidat(n)-l1 pt permutari_aux(n, M, lungime-1))>=M
%                                               si nu_apare(permutari_aux(n, M, lungime-1), candidat(n))

% permutare_m(n, M) = permutari_aux(n, M, n)

% colectare_toate(n, M) = reuniune de permutare_m(n, M)
