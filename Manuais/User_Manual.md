 # Inteligência Artificial - Jogo do Cavalo, 2 Jogadores

## Manual do Utilizador
---
### Autores:
* Henoch Mendes Vitureira Nº170221014
* Patrick Battisti Forsthofer Nº190200007

## Docentes:
* Prof. Joaquim Filipe
* Prof. Filipe Mariano

### 2019/2020


# Introdução 
Este documento tem como o objetivo documentar tecnicamente a implementação em Common Lisp do jogo do cavalo, uma variante do problema do cavalo, cuja finalidade é simular um jogo de duas pessoas baseado em turnos, tal como o xadrez, de modo a determinar um venceder. O tabuleiro do jogo em questão tem 10 linhas e 10 colunas, pontuadas de 0 a 99. O vencedor do jogo é aquele que consegue obter mais pontos, até os dois não conseguirem realizar mais movimentos. O jogo pode tem vertente de o utilizador humano contra o computador, e do computador contra computador. As jogadas do computador são decididas com recurso ao algoritmo implementado, o Negamax.

A primeira parte do documento dá a conhecer os objetivos e descrição do programa. A seguir é apresentada a estrutra de ficheiros necessária para o programa funcionar. Posteriormente, é demonstrada a utilização do programa assim como o decorrer de jogo, acompanhado por exemplos visuais.

Para finalizar, são apresentadas as limitações do programa.

## Objetivos: <a name="Objetivos"></a>
* Simular o jogo do cavalo, entre 2 jogadores, de modo a mostrar sequências de jogadas que permitem terminar o jogo tendo em conta o tabuleiro utilizado, a utilização do algoritmo NEGAMAX.
  <br>
* Percorrer um tabuleiro pontuado com uma peça que realiza os mesmos movimentos que um cavalo no xadrez de modo a obter mais pontos que o adversário.
  <br>
* Após o utilizador definir o modo de jogo, entre Humano vs Computador e Computador vs Computador, uma profundidade máxima do NEGAMAX e um tempo limite de jogada, o jogo inicia.


## Descrição Geral do Funcionamento<a name="Descrição"></a> 
As regras a serem aplicadas para atingir o objetivo do tabuleiro a ser resolvido são:
* Existem dois jogadores, 1 Humano e 1 Computador, ou ambos são o Computador.
* O jogador humano começa por colocar o cavalo numa casa da primeira linha do tabuleiro.
* O jogador Computador começa por colocar o cavalo numa casa da última linha do tabuleiro.
* A pontuação das casas do tabuleiro é atribuída aleatoriamente.

O programa começa pela escolha entre dois modos: Humano vs Computador e Computador vs Computador. Humano vs Computador é o mod de jogo onde o utilizador joga, por turnos, contra o computador, sendo possível visualizar as jogadas possíveis do jogador humano a cada turno. As jogadas do computador são decididas pelo algoritmo NEGEMAX.

O jogo começa com a colocação de cada cavalo numa casa da 1ª e última linha (A1-J1 e A10-J10 do tabuleiro). Se a casa escolhida tiver um número com dois dígitos diferentes, por exemplo 57, então, em consequência, o número simétrico 75 é apagado do tabuleiro, tornando esta casa inacessível durante o resto do jogo. Ou seja, o cavalo não pode terminar outra jogada nessa casa. Se o cavalo for colocado numa casa com um número "duplo", por exemplo 66, então qualquer outro número duplo pode ser removido e o jogador deve escolher qual em função da sua estratégia (por default remover a de maior valor). Depois de um jogador deixar a casa para se movimentar para outra, a casa onde estava fica inacessível para as restantes jogadas posteriores, ficando o numero da casa substituído pelo valor NIL.

As regras aplicadas ao resentolar do jogo são:
* Um cavalo não pode saltar para uma casa vazia (sem número).
  <br>
*  Cada jogador ganha pontos por cada casa visitada pelo cavalo (igual ao valor da casa). 
   <br>
*   A cada jogada repete-se a regra do simétrico ou duplo. 
    <br>
*  O jogador ganha pontos por cada casa visitada pelo cavalo (igual ao valor da casa). 
    <br>
*  Os pontos são contabilizados apenas para as casas visitadas, não pelos números simétricos ou duplos removidos.
    <br>
*  O jogo termina quando não for possível movimentar nenhum dos cavalos no tabuleiro.
    <br>
*  O vencedor o jogador que obteve o maior número de pontos.


# Utilização do Programa

## Pré-requisitos
Para além dos ficheiros do programa "algorithmn.lisp", "game.lisp" e "interact.lisp", deve haver um ficheiro log.dat na pasta do programa.

## Carregamento de Ficheiros do Programa
Para o utilizador carregar os ficheiros necessários, necessita de compilar apenas o ficheiro "interact.lisp" no IDE LispWorks, pois este irá carregar os restantes ficheiros.  


## Inicar o Jogo
Depois de carregar compilar o ficheiro "interact.lisp, o utilizador deve executar a função "start". Após esta execução, será apresentado um menu de escolha de modos.
```lisp
---------------------CHOOSE A MODE----------------------------
   
                 1 - Human vs Computer                           
                 2 - Computer vs Computer                  
   
 --------------------------------------------------------
```
<br>
Caso o utilizador selecione "1" ou "2", isto é, "Human vs Computer" ou "Computer vs Computer ", irá deparar-se com o pedido de inserção da profundidade máxima.

<br>
<br>

```lisp 
> 2
   
------ENTER THE MAXIMUM DEPTH------
1
```
<br>

Após a profundidade ser inserida, será solicitado um tempo limite para as jogadas do computador, em milisegundos, sendo pedido um valor entre 1000 e 5000.

<br>

```lisp  
------ENTER TIME FOR COMPUTER PLAY (1000 >= TIME [ms] >= 5000)------
2000
```

<br>
Caso o Utilizador tenha escolhido o modo Human vs Computer, após a definição do tempo de jogada limite, é apresentado o menu de escolha do primeiro jogador a jogar, entre humano e computador.

<br>
<br>

```lisp  
---------------------CHOOSE THE FIRST PLAYER----------------------------
   
                 1 - Human                                    
                 2 - Computer                              
   
 --------------------------------------------------------
```
Após escolher o primeiro jogador a jogar, é apresentado o tabuleiro inicial, que é aleatório, com valores de 0 a 100.

<br>

```lisp
----------------------START BOARD-----------------

(51 61 26 89 79 54 36 87 74 5)
(18 46 57 19 42 94 66 86 59 65)
(78 63 70 97 55 95 90 73 64 10)
(0 33 81 15 37 56 27 69 39 8)
(91 6 40 41 52 71 7 45 28 58)
(99 82 43 29 32 93 49 35 21 11)
(9 53 67 68 84 60 88 2 96 76)
(16 38 23 13 75 20 34 47 50 92)
(12 62 72 3 48 24 4 1 77 25)
(30 14 80 31 83 44 98 17 22 85)

----------------------START BOARD-----------------
```
<br>
De seguida, as jogadas do humano são pedidas, mostrando no ecrã as jogadas possíveis que o jogador pode realizar. As jogadas têm de ser inseridas no ecrã da mesma forma que é descrito no ecrã, com uma letra seguida de um número. Caso o utilizadore não insira um valor válido, o programa volta a pedir a jogada ao utilizador. Após a inserção de uma jogada válida, o tabuleiro atualizado com a jogada escolhida é mostrado no ecrã. As jogadas do computador são apresentadas da mesma forma, com a indicação da posição que o computador escolheu jogar. Abaixo é possível visualizar exemplos de esta interação.

<br>
<br>

```lisp
Enter your move (A1 A2 A3 A4 A5 A6 A7 A8 A9 A10): A5

(51 61 26 89 -1 54 36 87 74 5)
(18 46 57 19 42 94 66 86 59 65)
(78 63 70 97 55 95 90 73 64 10)
(0 33 81 15 37 56 27 69 39 8)
(91 6 40 41 52 71 7 45 28 58)
(99 82 43 29 32 93 49 35 21 11)
(9 53 67 68 84 60 88 2 96 76)
(16 38 23 13 75 20 34 47 50 92)
(12 62 72 3 48 24 4 1 77 25)
(30 14 80 31 83 44 98 17 22 85)

Computer -2 move: F1

(51 61 26 89 -1 54 36 87 74 5)
(18 46 57 19 42 94 66 86 59 65)
(78 63 70 97 55 95 90 73 64 10)
(0 33 81 15 37 56 27 69 39 8)
(91 6 40 41 52 71 7 45 28 58)
(-2 82 43 29 32 93 49 35 21 11)
(9 53 67 68 84 60 88 2 96 76)
(16 38 23 13 75 20 34 47 50 92)
(12 62 72 3 48 24 4 1 77 25)
(30 14 80 31 83 44 98 17 22 85)



Enter your move (C4 C6 B3 B7): B7

(51 61 26 89 NIL 54 36 87 74 5)
(18 46 57 19 42 94 -1 86 59 65)
(78 63 70 97 55 95 90 73 64 10)
(0 33 81 15 37 56 27 69 39 8)
(91 6 40 41 52 71 7 45 28 58)
(-2 82 43 29 32 93 49 35 21 11)
(9 53 67 68 84 60 88 2 96 76)
(16 38 23 13 75 20 34 47 50 92)
(12 62 72 3 48 24 4 1 77 25)
(30 14 80 31 83 44 98 17 22 85)
```
<br>
Caso o utilizador tenha escolhido o modo Computador vs Computador, as jogadas são mostradas à medida que são calculadas e executadas pelo programa, mostrando no ecrã o número correspondente ao jogador (-1 ou -2), a posição para onde jogou e o tabuleiro atualizado com a jogada.
<br><br>

```lisp
Computer -1 move: J2

(-2 28 42 NIL 18 29 21 NIL NIL NIL)
(10 NIL NIL 17 65 NIL NIL NIL NIL 19)
(36 56 53 NIL 25 48 NIL NIL NIL NIL)
(35 NIL NIL 14 NIL NIL 13 7 33 NIL)
(75 26 11 62 NIL 38 NIL NIL 2 20)
(47 NIL 79 NIL 59 16 NIL 30 4 NIL)
(39 41 54 NIL 23 NIL NIL NIL NIL 89)
(5 1 NIL 0 22 NIL NIL 27 3 50)
(52 12 NIL 40 NIL NIL 8 57 15 82)
(31 -1 49 45 NIL 63 NIL NIL 55 32)



Computer -2 move: C2

(NIL 28 42 NIL 18 29 21 NIL NIL NIL)
(10 NIL NIL 17 65 NIL NIL NIL NIL 19)
(36 -2 53 NIL 25 48 NIL NIL NIL NIL)
(35 NIL NIL 14 NIL NIL 13 7 33 NIL)
(75 26 11 62 NIL 38 NIL NIL 2 20)
(47 NIL 79 NIL 59 16 NIL 30 4 NIL)
(39 41 54 NIL 23 NIL NIL NIL NIL 89)
(5 1 NIL 0 22 NIL NIL 27 3 50)
(52 12 NIL 40 NIL NIL 8 57 15 82)
(31 -1 49 45 NIL 63 NIL NIL 55 32)
```

<br>
O programa vai mostrando no ecrã todas as jogadas, até nenhum jogador puder efetuar jogadas. Quando não é possível efetuar mais jogadas, os pontos de ambos os jogadores, Humano ou Computador, são mostrados no ecrã.
<br><br>

```lisp
Total points player -1: 2132
Total points player -2: 2035
```

# Limitações

Em termos de limitações, o programa não consegue validar os inputs do utilizador, pois não têm validações para todo o tipo de erros, excepto para a jogada escolhida pelo jogador Humano.