sudoku(S) :- resolve(S), mostraSolucao(S).

resolve(S) :- 
    resolveLinhas(S),
    resolveColunas(S),
    resolveQuadros(S).

resolveLinhas([]) :- !.
resolveLinhas([A|B]) :- diferente(A), resolveLinhas(B).

diferente([A, B, C, D]) :- 
   num(A), num(B), num(C), num(D), 
   A\=B, A\=C, A\=D, B\=C, B\=D, C\=D.

num(1).
num(2).
num(3).
num(4).

resolveColunas([]) :- !.
resolveColunas(S) :-
  transpor(S, S_trans),
  resolveLinhas(S_trans).

transpor([[]|_], []) :- !.
transpor(L, [Coluna|Colunas]) :-
  extrairCabeca(L, Coluna, Resto),
  transpor(Resto, Colunas).

extrairCabeca([], [], []).
extrairCabeca([[Cabeca|RestoLinha]|RestoLinhas], [Cabeca|Coluna], [RestoLinha|Resto]) :-
  extrairCabeca(RestoLinhas, Coluna, Resto).

resolveQuadros([]) :- !.
resolveQuadros(S) :-
  separarQuadros(S, Quadros),
  resolveQuadrosAux(Quadros).

separarQuadros([], []) :- !.
separarQuadros([[A,B,C,D],[E,F,G,H]|Resto], [[A,B,E,F],[C,D,G,H]|QuadrosRestantes]) :-
  separarQuadros(Resto, QuadrosRestantes).

resolveQuadrosAux([]) :- !.
resolveQuadrosAux([Quadro|Quadros]) :-
  diferente(Quadro),
  resolveQuadrosAux(Quadros).

mostraSolucao([]):- !.
mostraSolucao([A|B]) :- 
    mostraLinha(A), nl, mostraSolucao(B).

mostraLinha([]) :- !.
mostraLinha([A|B]) :- 
    write(A), write(' '), mostraLinha(B).

%   Exemplo de consulta:
%   -------------------
%?- sudoku([[4,_,_,_],[_,3,_,_],[_,_,1,_],[_,1,_,2]]).
