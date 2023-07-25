# Sudoku 4x4 

  > O objetivo desse trabalho é completar o código abaixo que implementa um sistema para resolver o sudoku 4x4.

  ## Passos
  1. Estude o código abaixo, que já implementa o sistema para verificar se nas linhas todos os valores são diferentes entre 1 e 4.
  2. Implemente os predicados que estão comentados (resolveColunas e resolveQuadros), para testar se nas colunas todos os valores são diferentes (de 1 a 4) e se os quatro quadros 2x2 também todos os valores são diferentes (de 1 a 4).
  3. **Observação**: Este código só pode ser completado, os predicados que já estão implementados não devem ser modificados.

   ## Código para completar
```prolog
sudoku(S) :- resolve(S), mostraSolucao(S).

resolve(S) :- 
    resolveLinhas(S).
    %resolveColunas(S),
    %resolveQuadros(S).

resolveLinhas([]) :- !.
resolveLinhas([A|B]) :- diferente(A), resolveLinhas(B).

diferente([A, B, C, D]) :- 
   num(A), num(B), num(C), num(D), 
   A\=B, A\=C, A\=D, B\=C, B\=D, C\=D.

num(1).
num(2).
num(3).
num(4).

mostraSolucao([]):- !.
mostraSolucao([A|B]) :- 
    mostraLinha(A), nl, mostraSolucao(B).

mostraLinha([]) :- !.
mostraLinha([A|B]) :- 
    write(A), write(' '), mostraLinha(B).

%   Exemplo de consulta:
%   -------------------
%?- sudoku([[4,_,_,_],[_,3,_,_],[_,_,1,_],[_,1,_,2]]).
```

  