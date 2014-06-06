-module(reinas).
-export([nReinas/4]).

%Instituto Tecnológico de Costa Rica
%Escuela de Ingeniería en Computación
%Lenguajes de Programación
%Profesor: Eddy Ramírez
%Alumno: Carlos Fallas Victor
%Carné: 201115046

%----------------------------Miselanias---------------------------------------------
%Función Imprimir
%Se encarga de imprimir un lista que recibe como parametro
imprimir(Lista)->io:format("~p~n",[Lista]).
%Función: Eliminar Elemento
%Elimina un todas las apariciones de un elemento en una lista
%Parámetros: Elem: El elemento a eliminar, Lista: la lista en la cual se va a eliminar el elemento
eliminarElem(Elem,Lista)->[X || X<- Lista , X /= Elem].
%----------------------
%Función Puntos Cruse
%Esta función es utilizada para obtener los punto de intercambio de genes para el cruse
%Obtiene dos puntos al azar entre 1 y el parametro Tope, con la particularidad de que
%el primer valor retornado sea menor al segundo
%Parámetros:Tope: El valor maximo que puede tomar un punto
%Retorno: una tupla conteniendo los dos puntos
puntosCruse(Tope)->puntosCruse(Tope,0,0).
puntosCruse(Tope,N1,N2) when (N1 >1)and (N2<Tope)and (N1<N2)->{N1,N2};
puntosCruse(Tope,N1,N2) when (N1>1) and (N2<Tope)and (N2<N1)->{N2,N1};
puntosCruse(Tope,_N1,_N2)->puntosCruse(Tope,random:uniform(Tope),random:uniform(Tope)).
%----------------------
%Función Indice
%Obtiene el indice de la primera aparicion de un Elemento en una lista
%Parámetros:	Elem: El elemento a buscar,Lista: la lista en la cual se va a buscar el elemento
%Retorno el indice del elemento.
indice(Elem,Lista)->indice(Elem,Lista,0).
indice(Elem,[Elem|_T],Acum)->Acum+1;
indice(Elem,[_H|T],Acum)->indice(Elem,T,Acum+1).
%-----------------------
%Función Decodificar
%Decodificar una individuo en una lista de string para representar una fila del tablero
%Parámetros: Lista-> El individuo que se va a decodificar
%Retorno: una lista de Strings representando la posición de las reinas en el tablero 
decodificar(Lista)->decodificar(Lista,length(Lista),[]).
decodificar([],_Largo,Acum)->lists:reverse(Acum);
decodificar([H|T],Largo,Acum)->decodificar(T,Largo,[crearString(H,Largo,1,"")|Acum]).
%Funcion crearString
%crea un string que repreta la posición de una reina en una fila del tablero
%Parámetros: Reina: la posicion de la reina en la fila, Largo: de la Fila, Actual: el largo actual de Acum, Acum: Resultado de la función
%Retorno: un string que representa la posición de una reina 
crearString(Reina,Reina,Reina,Acum)->Acum++" Q"; %si el largo de Acum es igual a la posición de la reina, inserta en el string(Acum) una "Q"   
crearString(_Reina,Largo,Largo,Acum)->Acum++" _"; 
crearString(Reina,Largo,Reina,Acum)->crearString(Reina,Largo,Reina+1,Acum++" Q");
crearString(Reina,Largo,Actual,Acum)->crearString(Reina,Largo,Actual+1,Acum++" _").
%-------------------
%Función obtenerMejorIndividio
%Obtiene el mejor individio de una población, esto lo hace comparando con la función de fitness cual individio se adapta mejor
%Parámetro: La población de la cual se desea obtener el mejor individuo 
%Retorno: El mejor individuo de la poblacion.
obtenerMejorIndividuo([H|T])-> obtenerMejorIndividuo(T,H).
obtenerMejorIndividuo([],Actual)->Actual;
obtenerMejorIndividuo([H|T],Actual)->obtenerMejorIndividuo(T,compararIndividuos(H,Actual,fitness(H),fitness(Actual))).
%Funcion comparar individios
%Compara cual individuo se adapta mejor según la formula de fitness.
%Parámetros: A: el primer individuo,B: el segundo individuo, FitnessA: El valor de fitness de A, FitnessB: El valor de fitness de B
%Retorna el individuo que mejor se adapte según la formula de fitness
compararIndividuos(A,_B,FitnessA,FitnessB)when FitnessA < FitnessB->A; %Para este caso entre menor sea el valor de fitness mejor
compararIndividuos(_A,B,_FitnessA,_FitnessB)->B.
%-------------------
%Función MejorFitness
%Busca y retorna el mejor valor de fitness de la población
%Parámetro: La población de la cual se quiere saber cual es el mejor valor de fitness
%Retorno: El mejor valor de fitness de la población
mejorFitness([H|T])->mejorFitness(T,fitness(H)).
mejorFitness([],Actual)->Actual;
mejorFitness([H|T],Actual)->mejorFitness(T,compararFitness(fitness(H),Actual)).
compararFitness(A,B)when A<B->A;
compararFitness(_A,B)->B.
%----------------
%Función SeleccionaTres
%Selecciona tres individuos al azar de una población para ser utilizados en la selección por torneo
%Parámetro: La población de la cual se desea seleccionar los individuos
%Retorno:  Una lista conteniendo tres individuo ordenados según su fitness
seleccionaTres(Poblacion)->seleccionaTres(Poblacion,length(Poblacion),[lists:nth(random:uniform(length(Poblacion)),Poblacion)]).
seleccionaTres(_Poblacion,_TamPoblacion,Acum)when length(Acum)==3->ordenarFitness(Acum);
seleccionaTres(Poblacion,TamPoblacion,Acum)->X=lists:nth(random:uniform(TamPoblacion),Poblacion),
								seleccionaTres(Poblacion,TamPoblacion,concatNoRepetidos(X,Acum)).
%-----------------
%Función concatNoRepetidos
%Concatena un elemento al frente una lista, si el elemento no existe en la lista.
%Parámetros: Elem: El elemento a concatenar, Lista: la lista donde se va a concatenar el elemento
%Retorno: el parámetro Lista conteniendo el Elemento (Elem), si no existía en Lista, o Lista no conteniendo a Elem, si este ya existía en Lista.
concatNoRepetidos(Elem,Lista)->concatNoRepetidos(Elem,Lista,lists:member(Elem,Lista)).
concatNoRepetidos(_Elem,Lista,true)->Lista;
concatNoRepetidos(Elem,Lista,false)->[Elem]++Lista.

%Crear la Poblacion Inicial ----------------------------------------------------------------
%Función CrearPoblacionIncial
%Crea la población inicial que va a ser utilizada como base para resolver el problema. La población no va a tener elementos repetidos.
%Parámetros: TamTablero: El tamaño del tablero, TamPoblacion: El tamaño que va a tener la población
%Retorno. Una lista conteniendo al grupo de individuos creados.
crearPoblacionInicial(0,_TamPoblacion)->[];
crearPoblacionInicial(_TamTablero,0)->[];
crearPoblacionInicial(TamTablero,TamPoblacion)->io:format("Creando Poblacion Inicial~n"),crearPoblacionInicial(TamTablero,TamPoblacion,[crearIndividuo(TamTablero)]).
crearPoblacionInicial(_TamTablero,TamPoblacion,Acum)when length(Acum)==TamPoblacion->Acum;
crearPoblacionInicial(TamTablero,TamPoblacion,Acum)->Ind =crearIndividuo(TamTablero),crearPoblacionInicial(TamTablero,TamPoblacion,concatNoRepetidos(Ind,Acum)).
%Función CrearIndividuo
%Crea un individuo, representado como una lista conteniendo números consecutivos (inciando en 1) en desorden
%Parámetros: TamTablero, el largo de la lista.
%Retorna: una lista de números consecutivos en desorden, que para efectos de este problema es un individuo
crearIndividuo(TamTablero)->crearIndividuo(lists:seq(1,TamTablero),[]).
crearIndividuo(Lista,Acum)when length(Lista)==1->Lista++Acum;
crearIndividuo(Lista,Acum)->Elem=lists:nth(random:uniform(length(Lista)),Lista),crearIndividuo(eliminarElem(Elem,Lista),[Elem|Acum]).

%mutacion--------------------------------------------------------------------------------------------
%Funcion Mutar
%Se encarga de mutar a un individuo, si un número tirado al azar es menor o igual a la probabilidad de mutación
%Parametros: Ind1: el individuo a mutar, ProbMut: la probabilidad de ocurra la mutación 
mutar(Ind1,ProbMut)->mutacion(Ind1,ProbMut,random:uniform()).
mutacion(Ind,Prob,Valor)when  Valor =< Prob ->swap(Ind,random:uniform(length(Ind)),random:uniform(length(Ind)));
mutacion(Ind,_Prob,_Valor)->Ind.

swap(Ind,Pos1,Pos2)->[swapAux(X,lists:nth(Pos1,Ind),lists:nth(Pos2,Ind))|| X <- Ind].
swapAux(Elem1,Elem1,Elem2)->Elem2;
swapAux(Elem2,Elem1,Elem2)->Elem1;
swapAux(Actual,_Elem1,_Elem2)->Actual.
%Cruse ----------------------------------------------------------------------------------------------
pmxCrossover(Ind1,Ind2,Mutacion)->X=puntosCruse(length(Ind1)), %selecciona dos punto al azar para realizar el cruce PMX,
                        mutar(eliminarRepetidos(Ind1,Ind2,crossover(Ind1,Ind2,X),X),Mutacion). %mete Mutacion

crossover(Ind1,Ind2,{Punto1,Punto2})->crossover(Ind1,Ind2,[],Punto1,Punto2,1).
crossover([],_Ind2,Hijo1,_Punto1,_Punto2,_Actual)->lists:reverse(Hijo1);
crossover([H1|T1],[_H2|T2],Hijo1,Punto1,Punto2,Actual)when (Actual<Punto1) or (Actual>Punto2)->crossover(T1,T2,[H1|Hijo1],Punto1,Punto2,Actual+1);
crossover([_H1|T1],[H2|T2],Hijo1,Punto1,Punto2,Actual)when (Punto1=<Actual) and (Actual=<Punto2)->crossover(T1,T2,[H2|Hijo1],Punto1,Punto2,Actual+1).

eliminarRepetidos(Ind1,Ind2,Hijo1,{Punto1,Punto2})->eliminarRepetidos(Ind1,Ind2,Hijo1,[],Punto1,Punto2,1).

eliminarRepetidos(_Ind1,_Ind2,[],NHijo1,_Punto1,_Punto2,_Actual)->lists:reverse(NHijo1); 

eliminarRepetidos(Ind1,Ind2,[H1|T1],NHijo1,Punto1,Punto2,Actual)when (Actual>=Punto1) and (Actual=<Punto2)->
				eliminarRepetidos(Ind1,Ind2,T1,[H1|NHijo1],Punto1,Punto2,Actual+1);
eliminarRepetidos(Ind1,Ind2,[H1|T1],NHijo1,Punto1,Punto2,Actual)when (Actual>Punto2)->
				eliminarRepetidosAux(Ind1,Ind2,[H1|T1],NHijo1,Punto1,Punto2,Actual,(lists:member(H1,NHijo1)));
eliminarRepetidos(Ind1,Ind2,[H1|T1],NHijo1,Punto1,Punto2,Actual)->
				eliminarRepetidosAux(Ind1,Ind2,[H1|T1],NHijo1,Punto1,Punto2,Actual,(lists:member(H1,T1))).

eliminarRepetidosAux(Ind1,Ind2,[H1|T1],NHijo1,Punto1,Punto2,Actual,true)->
				eliminarRepetidos(Ind1,Ind2,[lists:nth(indice(H1,Ind2),Ind1)|T1],NHijo1,Punto1,Punto2,Actual);
eliminarRepetidosAux(Ind1,Ind2,[H1|T1],NHijo1,Punto1,Punto2,Actual,false)->
				eliminarRepetidos(Ind1,Ind2,T1,[H1|NHijo1],Punto1,Punto2,Actual+1).
%Funciones para el Calculo del Fitness
ordenarFitness([Pivote|T])->ordenarFitness([ X || X <- T, fitness(X) < fitness(Pivote)]) ++[Pivote] ++ordenarFitness([ X || X <- T, fitness(X) >= fitness(Pivote)]);
ordenarFitness([]) -> [].

fitness(Lista)->lists:sum([colDiagonalesAux(X,Lista) || X<-Lista])/2.

colDiagonalesAux(H,[H|T])->colDiagonales(H,1,T,2,0);
colDiagonalesAux(H,Lista)->colDiagonales(H,indice(H,Lista),Lista,1,0).

colDiagonales(_Colum,_Fila,[],_IndActual,Acum)->Acum;
colDiagonales(Colum,Fila,[Colum|T],IndActual,Acum)->colDiagonales(Colum,Fila,T,IndActual+1,Acum);
colDiagonales(Colum,Fila,[H|T],IndActual,Acum)when abs(Colum-H) == abs(Fila-IndActual) -> colDiagonales(Colum,Fila,T,IndActual+1,Acum+1);
colDiagonales(Colum,Fila,[_H|T],IndActual,Acum)->colDiagonales(Colum,Fila,T,IndActual+1,Acum).

%acceso al programa
nReinas(Tablero,_Pob,Mut,_Param)when (Tablero <4) or (Mut>1) or (Mut <0)->io:format("Error, Los parametros ingresados son incorrectos~n");
nReinas(Tablero,Poblacion,Mut,{limit,Generaciones,Elitismo})->generaciones(crearPoblacionInicial(Tablero,Poblacion),Generaciones,Elitismo,Mut,no);
nReinas(Tablero,Poblacion,Mut,{unlimit,Elitismo})->generaciones(crearPoblacionInicial(Tablero,Poblacion),-1,Elitismo,Mut,no);
nReinas(Tablero,Poblacion,Mut,{hilos,CantHilos,Param})->crearHilos(Tablero,Poblacion,Mut,Param,CantHilos);
nReinas(_Tablero,_Poblacion,_Mut,_Param)->io:format("Error de parametros~n").

generaciones(Poblacion,Generaciones,Elitismo,Mut,Servidor)->generacionesAux(Poblacion,Generaciones,Elitismo,Mut,mejorFitness(Poblacion),Servidor).

generacionesAux(Poblacion,_Generaciones,_Elitismo,_Mut,UltMejorFitness,no)when UltMejorFitness==0->io:format("Un resultado es~n"),imprimir(decodificar(obtenerMejorIndividuo(Poblacion)));
generacionesAux(Poblacion,_Generaciones,_Elitismo,_Mut,UltMejorFitness,Servidor)when UltMejorFitness==0->Servidor!decodificar(obtenerMejorIndividuo(Poblacion));

generacionesAux(_Poblacion,0,_Elitismo,_Mut,_UltMejorFitness,no)->io:format("Se alcanzo del limite de generaciones~n");
generacionesAux(_Poblacion,0,_Elitismo,_Mut,_UltMejorFitness,Servidor)->Servidor!sin;
generacionesAux(Poblacion,Generaciones,Elitismo,Mut,_UltMejorFitness,no)->io:format("El mejor de la Generacion es: ~n"),
																	imprimir(decodificar(obtenerMejorIndividuo(Poblacion))),
																	NuevaPoblacion=seleccionTorneo(Poblacion,Elitismo,Mut),
																    generacionesAux(NuevaPoblacion,Generaciones-1,Elitismo,Mut,mejorFitness(NuevaPoblacion),no);
generacionesAux(Poblacion,Generaciones,Elitismo,Mut,_UltMejorFitness,Servidor)->
																	NuevaPoblacion=seleccionTorneo(Poblacion,Elitismo,Mut),
																    generacionesAux(NuevaPoblacion,Generaciones-1,Elitismo,Mut,mejorFitness(NuevaPoblacion),Servidor).

seleccionTorneo(Poblacion,si,Mutacion)->seleccionTorneoAux(Poblacion,Mutacion,[obtenerMejorIndividuo(Poblacion)]);
seleccionTorneo(Poblacion,no,Mutacion)->seleccionTorneoAux(Poblacion,Mutacion,[]).

seleccionTorneoAux(Poblacion,_Mutacion,Acum)when length(Poblacion)=<length(Acum)->Acum;
seleccionTorneoAux(Poblacion,Mutacion,Acum)->Seleccionados=seleccionaTres(Poblacion),
											Hijo=pmxCrossover(lists:nth(1,Seleccionados),lists:nth(2,Seleccionados),Mutacion),
											 seleccionTorneoAux(Poblacion,Mutacion,concatNoRepetidos(Hijo,Acum)).

crearHilos(Tablero,Poblacion,Mut,Param,Hilos)->X=spawn(fun()->servidor(Hilos) end),crearHilos(Tablero,Poblacion,Mut,Param,Hilos,X).
crearHilos(Tablero,Poblacion,Mut,Param,1,Servidor)->Pob = crearPoblacionInicial(Tablero,Poblacion),spawn(fun()->nReinasH(Pob,Mut,Param,Servidor) end);
crearHilos(Tablero,Poblacion,Mut,Param,Hilos,Servidor)->Pob = crearPoblacionInicial(Tablero,Poblacion),spawn(fun()->nReinasH(Pob,Mut,Param,Servidor)end),crearHilos(Tablero,Poblacion,Mut,Param,Hilos-1,Servidor).

servidor(0)->io:format("Todos los hilos han finalizado~n");
servidor(Hilos)->
	receive
		sin->io:format("Se alcanzo el limite de Generaciones~n"),servidor(Hilos-1);
		X->io:format("~n"),imprimir(X),servidor(Hilos-1)
	end.

nReinasH(Poblacion,Mut,{limit,Generaciones,Elitismo},Servidor)->generaciones(Poblacion,Generaciones,Elitismo,Mut,Servidor);
nReinasH(Poblacion,Mut,{unlimit,Elitismo},Servidor)->generaciones(Poblacion,-1,Elitismo,Mut,Servidor).