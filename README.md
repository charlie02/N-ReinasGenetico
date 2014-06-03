N-ReinasGenetico
================

El problema de N-Reinas Resuelto con un algoritmo genético.
El lenguaje utilizado en este programa es Erlang

Funcionamiento del Programa
===========================

Para ejecutar el programa primero se debe tener instalado Erlang.
Se debe de compilar el archivo reinas.erl en Erlang de la siguiente manera
  c(reinas).

Una vez hecho esto se debe de llamar a la función nReinas(TamTablero,TamPoblación,Mutacion,Param). Dónde:
   - TamTablero: es el tamaño del tablero a utilizar en el problema.
   - TamPoblación: es el tamaño de la población a utilizar en el problema.
   - Mutacion: es la probabilidad de que ocurra una mutación, debe ser un número entre 0 y 1.
   - Param: es una tupla la cual va a modificar el comportamiento de la función. Debe ser de alguna de las siguientes formas:
           - {unlimit,Elitismo}: en este caso se ejecuta de sin límite generaciones, es decir, únicamente se va a detener   hasta encontrar una solución. Elitismo, indica si el programa va a utilizar elitismo en la producción de generaciones, deber ser si o no
          - {limit,Generaciones,Elitismo}: en este caso se va a ejecutar con un límite de generaciones indicado en Generaciones, e igual que la forma anterior indica si se utiliza o no el elitismo.
          - {hilos,Cantidad,Param}: En este caso funciona con hilos, se van a hacer Cantidad hilos con los mismos parámetros que tienen el tercer elemento de la tupla,que es a su vez una tupla con los parámetros anteriores. Se van a obtener hasta Cantidad de resultados

Se debe llamar a la función de la siguiente manera:
  - reinas:nReinas(10,11,0.03,{hilos,50,{unlimit,no}}).
