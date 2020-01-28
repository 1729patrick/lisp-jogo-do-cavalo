 # Inteligência Artificial - Jogo do Cavalo

## Manual do Utilizador
---
### Autores:
* Henoch Mendes Vitureira Nº170221014
* Patrick Battisti Forsthofer Nº190200007

## Docentes:
* Prof. Joaquim Filipe
* Eng. Filipe Mariano

# Índice
1. [Introdução](#Introdução)
   <br>
   1.1 [Objetivos](#Objetivos)
     <br>
   1.2 [Descrição Geral do Funcionamento](#Descrição)
2. [Utilização do Programa](#Utilização)
     <br>
   2.1 [Utilização do Programa](#Utilização)
      <br>
   2.2 [Pré-requisitos](#Pré-requisitos)
     <br>
   2.3 [Carregamento de Ficheiros do Programa](#Ficheiros)
     <br>
   2.4 [Inicar o Jogo](#Inicar)
     <br>
   2.5 [Estatísticas](#Estatísticas)
3. [Limitações](#Limitações)


# Introdução <a name="Introdução"></a>
Este documento tem como o obetivo de identificar os vários objetivos do programa do jogo do cavalo, descrever geralmente o funcionamento do mesmo, explicar a forma como se utiliza o programa, dar a conhecer a informação necessário para o este funcionar de forma correta, assim como a informação que este desponibliza, acompanhado de exemplos dos ecrãs com que o utilizador pode vir a deparar-se.

Para finalizar, são apresentadas as limitações do programa.

## Objetivos: <a name="Objetivos"></a>
* Simular o jogo do cavalo de modo a mostrar sequências de jogadas que permitem terminar o jogo tendo em conta o tabuleiro utilizado, a utilização de um algoritmo, e algumas regras.  Os algoritmos dispoíveis são o Depth-First Search, Breadth-First Search e A*.
  <br>
* Percorrer um tabuleiro pontuado com uma peça que realiza os mesmos movimentos que um cavalo no xadrez de modo a chegar a uma pontuação desejada.
  <br>
* Atingir uma dada pontuação definida para o problema, no menor número possível de jogadas, de pendendo do algoritmo escolhido, deslocando o cavalo ao longo do tabuleiro, partindo de uma casa inicial, em jogadas sucessivas até não ser possível efetuar qualquer movimento ou até atingir o objetivo. No caso de o cavalo não puder efetuar mais movimentos, significa que não há solução para o problema em questão.


## Descrição Geral do Funcionamento<a name="Descrição"></a> 
As regras a serem aplicadas para atingir o objetivo do tabuleiro a ser resolvido são:
* Existe apenas um jogador.
* O jogador começa por colocar o cavalo numa casa da primeira linha do tabuleiro.
* O estado final é atingido quando o cavalo chega a uma casa que lhe permite obter uma pontuação igual ou superior ao objetivo definido.
* Se não for possível atingir o objetivo, o programa deverá informar o utilizador de que o problema não tem solução.

O programa começa pela escolha entre dois modos: Exercícios ou Problemas. Exercícios são os tabuleiros fornecidos no enunciado do projeto do jogo do cavalo, equanto os problemas são os tabuleiros inseridos num ficheiro problems.dat, na pasta do programa.

A imagem abaixo é um exemplo dos tabuleiros fornecidos pelo docente:

![alt text](https://github.com/patrickbattisti/lisp-passeio-do-cavalo/blob/dev/mdImages/problema2.PNG?raw=true)


O jogo começa com a colocação do cavalo numa casa da 1ª linha (A1-J1 do tabuleiro). Se a casa escolhida tiver um número com dois dígitos diferentes, por exemplo 57, então, em consequência, o número simétrico 75 é apagado do tabuleiro, tornando esta casa inacessível durante o resto do jogo. Ou seja, o cavalo não pode terminar outra jogada nessa casa. Se o cavalo for colocado numa casa com um número "duplo", por exemplo 66, então qualquer outro número duplo pode ser removido e o jogador deve escolher qual em função da sua estratégia (por default remover a de maior valor). Depois de um jogador deixar a casa para se movimentar para outra, a casa onde estava fica também inacessível para o jogo, ficando o numero da casa apagado.

As regras aplicadas ao resentolar do jogo são:
* Um cavalo não pode saltar para uma casa vazia (sem número).
  <br>
*  A cada jogada repete-se a regra do simétrico ou duplo. 
    <br>
*  O jogador ganha pontos por cada casa visitada pelo cavalo (igual ao valor da casa). 
    <br>
*  Os pontos são contabilizados apenas para as casas visitadas, não pelos números simétricos ou duplos removidos.
    <br>
*  O jogo termina quando não for possível movimentar o cavalo no tabuleiro, ou se uma solução para os pontos desejados for encontrada.
    <br>
*  Caso exista, a solução é apresentada  pela sequência de jogadas realizadas sobre as casas do tabuleiro. Cada jogada é identificada pela casa destino do cavalo (uma letra e um número).


# Utilização do Programa<a name="Utilização"></a> 

## Pré-requisitos<a name="Pré-requisitos"></a> 
Para além dos ficheiros do programa "project.lisp", "puzzle.lisp" e "search.lisp" :
* Deve haver um ficheiro statistics.dat na pasta do programa.
* Deve haver um ficheiro problems.dat com pelo menos um tabuleiro no formato de lista na linguagem Common Lisp.

A conteúdo da pasta do progrmaa deve ter o seguinte aspeto:

![alt text](https://github.com/patrickbattisti/lisp-passeio-do-cavalo/blob/dev/mdImages/pasta-programa.PNG?raw=true)

O Ficheiro "problems.dat" deverá ter o seguinte tipo de conteúdo:
```dat
((NIL NIL NIL NIL NIL NIL NIL NIL NIL 12)
(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL 55 NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL 22 NIL 45 NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL 88 NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL 67 NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
(NIL NIL NIL 44 NIL NIL NIL NIL NIL NIL))


((94 25 54 89 21 8 36 14 41 96) 
(78 47 56 23 5 49 13 12 26 60) 
(0 27 17 83 34 93 74 52 45 80) 
(69 9 77 95 55 39 91 73 57 30) 
(24 15 22 86 1 11 68 79 76 72) 
(81 48 32 2 64 16 50 37 29 71) 
(99 51 6 18 53 28 7 63 10 88) 
(59 42 46 85 90 75 87 43 20 31) 
(3 61 58 44 65 82 19 4 35 62) 
(33 70 84 40 66 38 92 67 98 97))
```
## Carregamento de Ficheiros do Programa<a name="Ficheiros"></a> 
Para o utilizador carregar os ficheiros necessários, necessita de compilar apenas o ficheiro "project.lisp" no IDE LispWorks, pois este irá carregar os restantes.  


## Inicar o Jogo<a name="Iniciar"></a> 
Depois de carregar os ficheiros necessários para o funcionamento do programa, o utilizador deve executar a função "start-game". Após esta execução, será apresentado um menu de escolha de modos.
```lisp

CL-USER 1 > (start-game)
    
---------------------HORSE GAME---------------------------
   
|                Choose a Mode:          		  |
   
|                1 - Exercises                            |   
|                2 - Problems                             |   
|                3 - LEAVE                                |
   
 --------------------------------------------------------
```
<br>
Caso o utilizador selecione "1" ou "2", isto é, "Exercises" ou "Problems", irá depar-se com um menu de escolha de algoritmo a ser utilizado.
<br>

```lisp 

---------------------HORSE GAME---------------------------
   
|                Choose an algorithm:          	          |
   
|                1 - Depth-First                          |   
|                2 - Breadth-First                        |   
|                3 - A*                                   |
   
 ---------------------------------------------------------
```

Após escolher um dos algoritmos apresentados, se escolheu a opção "Exercices", o utilizador irá deparar-se com um menu de escolha de um dos 7 tabuleiros fornecidos pelo docente, de A e F.

```lisp

---------------------HORSE GAME---------------------------
  
-|||||||||||||||||||||EXERCISES|||||||||||||||||||||||||||
  
|                    Choose a Board:          		    |
  
|              	  	 1 - Board A                        |  
|                 	 2 - Board B                        |  
|               	 3 - Board C                        |  
|              	  	 4 - Board D                        |  
|              		 5 - Board E                        |  
|              	         6 - Board F                        |  
|              	         OTHER - Leave                      |
   
 --------------------------------------------------------
```
Caso o utilizador tenha selecionado a opção "Problems, irá deparar-se com um menu de escolha de um dos tabuleiros presentes no ficheiro "problems.dat". os Tabuleiros são listados dinamicamente pela sua posição no ficheiro. Quantos mais problemas houverem, mais opções irão aparecer.

```lisp 
 ---------------------------------------
|               HORSE GAME             |

|            Choose a Problem          |

|         1 - Board number: 1          |
|         2 - Board number: 2          |
|         3 - Board number: 3          |
|         4 - Board number: 4          |
 ---------------------------------------
```

Depois do utilizador ter escolhido um tabuleiro, o programa irá perguntar qual a posição inicial do cavalo, por linha e coluna, qual a estratégia de remoção de número "duplo", e qual o número de pontos desejados.

```lisp
What is the initial line number? 
0
What is the initial column number? 
3
What is strategy for remove asymmetric numbers? 
1. MAX ASIMMETRIC
2. MIN ASIMMETRIC
2
What is the target point number? 
240
```

Caso o utilizador tenha selecionado o algoritmo Depth-First, o programa irá perguntar qual a profundidade máxima a percorrer.

```lisp
What is the limit depth? 
6
```
Após estas informaçãoes serem inseridas, o programa irá processor o tabuleiro com o algoritmo e informações inseridas pelo utilizador. Caso o programa tenha encontrado uma solução, irá apresentar a mesma no ecrã, assim como o número de pontos atingidos, a profundidade do nó em que se encontrou a solução e a sequência de jogadas necessárias para chegar ao objetivo.

```lisp
*********************** SOLUTION DFS *******
POINTS: 245

DEPTH: 5

SEQUENCE: 
("D" 1)
("C" 3)
("B" 1)
("A" 3)
("B" 5)
("A" 7)


BOARD:
(94 NIL 54 NIL 21 8 36 14 41 96)
(78 47 56 23 5 49 13 12 26 60)
(NIL 27 NIL 83 34 93 74 NIL 45 80)
(69 9 77 95 55 39 91 73 57 30)
(24 NIL 22 86 1 NIL 68 79 76 72)
(81 48 32 2 64 16 50 37 29 NIL)
(T NIL 6 18 53 28 7 63 10 88)
(59 42 46 85 90 75 87 43 20 31)
(3 61 58 44 65 82 19 4 35 62)
(33 70 84 40 66 38 92 67 NIL 97)
******************************************
```

Caso o tabuleiro em questão não tenha solução para a informação inserida pelo utilizador, será apresentada a mensagem que o informa do mesmo.
```lisp
What is the target point number? 
75
No solution found
```

## Estatísticas<a name="Estatísticas"></a>
Após a execução bem sucedida de um algoritmo, o programa irá escrever no ficheiro "statistics.dat" a análise estatística da execução do algoritmo para resolver o tabuleiro escolhido. As informações oferecidas passam por ser algoritmo utilizado, tempo de execução, número de nós gerados, números de nós expandidos, valor de penetrância, factor de ramificação média, profundidade máxima (no caso de Depth-First), tamanho da solução, pontos desejados, pontos alcançados, sequência de jogadas, e os tabuleiros inicial e final. Abaixo encontra-se um exemplo deste output.
```dat
 ----: Algorithm: DFS 
 ----:  Starting time: 17:55:31
 ----:  Ending Time: 17:55:46
 ----:  Number of Generated Nodes: 32
 ----:  Number of Expanded Nodes: 16
 ----:  Penetrance Level: 0.1875
 ----:  Median Branching Factor: 1.4831691
 ----:  Maximum Depth: 6
 ----:  Solution Length: 6
 ----:  Goal Points: 240
 ----:  Current Points: 245
 ----:  Solution Sequence: ((D 1) (C 3) (B 1) (A 3) (B 5) (A 7))
 ----:  Starting Board:
   (94 25 54 89 21 8 36 14 41 96)
   (78 47 56 23 5 49 13 12 26 60)
   (0 27 17 83 34 93 74 52 45 80)
   (69 9 77 95 55 39 91 73 57 30)
   (24 15 22 86 1 11 68 79 76 72)
   (81 48 32 2 64 16 50 37 29 71)
   (99 51 6 18 53 28 7 63 10 88)
   (59 42 46 85 90 75 87 43 20 31)
   (3 61 58 44 65 82 19 4 35 62)
   (33 70 84 40 66 38 92 67 98 97)
 ----:  Final Board:
   (94 NIL 54 NIL 21 8 36 14 41 96)
   (78 47 56 23 5 49 13 12 26 60)
   (NIL 27 NIL 83 34 93 74 NIL 45 80)
   (69 9 77 95 55 39 91 73 57 30)
   (24 NIL 22 86 1 NIL 68 79 76 72)
   (81 48 32 2 64 16 50 37 29 NIL)
   (T NIL 6 18 53 28 7 63 10 88)
   (59 42 46 85 90 75 87 43 20 31)
   (3 61 58 44 65 82 19 4 35 62)
   (33 70 84 40 66 38 92 67 NIL 97)
```

# Limitações<a name="Limitações"></a>

Em termos de limitações, o programa não consegue voltar a atrás nas ações do utilizador, nem começar o programa de novo sem chamar a função "start-game" de novo. Os inputs do utilizador não têm validações para todo o tipo de erros.