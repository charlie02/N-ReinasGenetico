%Instituto Tecnológico de Costa Rica
%Escuela de Ingeniería en Computación
%Lenguajes de Programación
%Profesor: Eddy Ramírez
%Alumno: Carlos Fallas Victor
%Carné: 201115046

decodificar(Solucion,Result):-length(Solucion,Largo),decodificar(Solucion,Largo,1,[],Result).
decodificar([],_Largo,_Actual,Acum,Acum):-!.
decodificar([H|T],Largo,Actual,Acum,Result):- crearFila(H,Largo,Fila),Actual2 is Actual+1,
	                                      decodificar(T,Largo,Actual2,[Fila|Acum],Result).

crearFila(Pos,Largo,Fila):-crearFila(Pos,Largo,1,"",Fila).
crearFila(_Pos,Largo,Actual,Acum,Fila):-Largo<Actual,name(Fila,Acum),!.
crearFila(Pos,Largo,Pos,Acum,Fila):-Actual is Pos+1,append(Acum," Q",Acum2),!,crearFila(Pos,Largo,Actual,Acum2,Fila).
crearFila(Pos,Largo,Actual,Acum,Fila):-Actual2 is Actual+1,append(Acum," _",Acum2),crearFila(Pos,Largo,Actual2,Acum2,Fila).

noAtaca([]).
noAtaca([H|T]):-noAtaca(T,H,T,[H|T],2,1).
noAtaca([],_Elem,_Revizando,_Copia,_Largo,_Pos):-!.
noAtaca([HOrig|TOrig],_Elem,[],Copia,_Largo,Pos):-Pos2 is Pos+1,!,noAtaca(TOrig,HOrig,Copia,Copia,1,Pos2).

noAtaca(Orig,Elem,[Elem|TRev],Copia,Largo,Pos):-Largo2 is Largo+1,!,noAtaca(Orig,Elem,TRev,Copia,Largo2,Pos).
noAtaca(Orig,Elem,[HRev|TRev],Copia,Largo,Pos):-abs(Elem-HRev)=\=abs(Pos-Largo),Largo2 is Largo+1,!,
						noAtaca(Orig,Elem,TRev,Copia,Largo2,Pos).

iota(Largo,Lista):-iota(Largo,[],Lista).
iota(0,Acum,Acum):-!.
iota(Largo,Acum,Lista):-Largo2 is Largo-1,iota(Largo2,[Largo|Acum],Lista).

permu(Numero,X):- iota(Numero,Lista),permu(Lista,X,[]).
permu([],[],_Acum).
permu(L,[H|T],Acum) :-
 append(V,[H|U],L),
 append(V,U,W),append(Acum,[H],Acum2),noAtaca(Acum2),permu(W,T,Acum2).


nq(Tamano,Result):-iota(Tamano,Lista),!,nqAux(Lista,Result).
nqAux(Lista,Result):-permu(Lista,X,[]),decodificar(X,Result).
