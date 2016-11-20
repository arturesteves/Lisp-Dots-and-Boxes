;;;; projecto.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Funções de interacção com o utilizador, de carregamento dos outros ficheiros do projecto e leitura e escrita em ficheiros

;;;; Tempo
;;;; Antes dia 19/11/2016 : 1 hora
;;;; 19/11/2016 : 13:13 - 14:57: 1 hora e 37 minutos
;;;; 20/11/2016 : 16:47 - 19:25: 2 horas e 38 minutos
;;;; 20/11/2016 : 20:32 - 21:12: 0 horas e 40 minutos

;;;; Total: 




;;; Inicialização do Programa
;; iniciar 
(defun iniciar ()	"Função que inicializa o programa, chamando a função que apresenta o menu inicial"
	(menu-inicial)
)


;; menu-inicial
(defun menu-inicial () "Apresenta o menu principal do programa na consola. Sendo possível iniciar uma procura ou sair do programa"
	;(loop	; Quero-o aqui, para quando acabar a procura de um problema eu conseguir executar outra procura
		(progn
			(format t "~%> ------------------------------------------------------")
			(format t "~%>|         Puzzle dos Pontos e das Caixas              |")
			(format t "~%>|                                                     |")
			(format t "~%>|            1. Iniciar Procura                       |")     
			(format t "~%>|            2. Sair                                  |")
			(format t "~%>|                                                     |")
			(format t "~%> ------------------------------------------------------")
			(format t "~%> Opcao:")
			(format t "~%> ")
			
			(let ((opcao (ler-teclado)))
					(cond
						((not (numberp opcao)) (menu-inicial))		; Analisar esta linha, quando colocar o (loop )
						((and (<= opcao 2) (>= opcao 1)) (cond
																				((= opcao 1) (iniciar-procura))
																				((= opcao 2) nil)	; substituir nil por -> (return) Quando descomentar (loop 
																			)
						)
					(T (progn
							(format t "~%> Opcao Invalida!")
							(format t "~%> Opcoes Validas: [1, 2]")
							(format t "~%  ")
						)
					)
				)
			)
		)
	;)
)
;

;; iniciar-procura 
(defun iniciar-procura () 
"Pede ao utilizador toda a informação necessário para começar uma procura no espaço de estados. 
Sendo necessário fornecer o estado inicial, o algoritmo de procura, talvez seja necessário pedir a profundidade máxima e a heurísticas, consoante o algoritmo escolhido"

	(let* ((tabuleiro 						(ler-tabuleiro))
			 (numero-objectivo-caixas  (ler-numero-objectivo-caixas))
			 (algoritmo 						(ler-algoritmo))
			 (profundidade (cond ((eql algoritmo 'dfs) (ler-profundidade)) (T 9999)))
			 (heuristica (cond ((not (or (eql algoritmo 'dfs) (eql algoritmo 'bfs))) (ler-heuristica)) (T 0)))
			 
			 ;(escreve-no (procura-generica no profundidade 'solucaop 'sucessores algoritmo (operadores))
			)	
			
			
			(list tabuleiro numero-objectivo-caixas algoritmo profundidade heuristica)
	)
)
;

;; ler-tabuleiro
(defun ler-tabuleiro () "Lista todos os estados inicias possíveis, recebe a escolha do utilizador e retorna a sua escolha caso esta seja válida"
	(progn
		(format t "~%>")
		(format t "~%> Escolha o estado/tabuleiro inicial do problema ")
		;(format t "~%> Possibilidades [a, b, c, d, e, f]")
		(format t "~%> Possibilidades: ") 
		(format t "~%> 	Tabuleiro A -> Caixas fechadas: 1   |   Dimensao: 3 x 3")
		(format t "~%> 	Tabuleiro B -> Caixas fechadas: 5   |   Dimensao: 4 x 4")
		(format t "~%> 	Tabuleiro C -> Caixas fechadas: 4   |   Dimensao: 4 x 4")
		(format t "~%> 	Tabuleiro D -> Caixas fechadas: 0   |   Dimensao: 5 x 4")
		(format t "~%> 	Tabuleiro E -> Caixas fechadas: 2   |   Dimensao: 6 x 6")
		(format t "~%> 	Tabuleiro F -> Caixas fechadas: 0   |   Dimensao: 7 x 7")
		(format t "~%> Estado inicial: ")
		(format t "~%> ")
		
		(let* ((opcao (ler-teclado))
				 (opcao-valida (existe-lista opcao '(a b c d e f))))		;Perguntar ao stor sobre esta listagem hardcoded aqui	-> deveria ter uma função que me retorna todos os tabuleiros possíveis?
			(cond
				(opcao-valida opcao)
				(T (progn
						(format t "~%> Opcao Invalida!")
						(format t "~%  ")
						(ler-tabuleiro)
					)
				)
			)
		)
	)
)
;

;; ler-numero-objectivo-caixas 
(defun ler-numero-objectivo-caixas () "Le do utilizador o número objectivo de caixas a fechar"
	(progn
		(format t "~%> Qual o numero de caixas a fechar, ou seja, o objectivo ?")
		(format t "~%> ")
		(let ((resposta (ler-teclado)))
			(cond 
				((not (numberp resposta)) (ler-numero-objectivo-caixas))
				((>= resposta 1) resposta)
				(T (ler-numero-objectivo-caixas))))
	)
)
;

;; ler-algoritmo
(defun ler-algoritmo () "Recebe do utilizador o nome do algoritmo a usar para efectuar a procura"
	(progn
		(format t "~%> Qual o algoritmo que pretende usar para efectuar a procura?")
		(format t "~%> Serao apresentadoss todos os algoritmos com e o nome a introduzir no sistema: ") ; [a, b, c, d, e, f]")
		(format t "~%> 	Breadth-first Search -> bfs")
		(format t "~%> 	Depth-first Search -> dfs")
		(format t "~%> 	A* Search -> a*")
		(format t "~%> 	A DEFINIR -> *********************")
		(format t "~%> Algoritmo a usar: ")
		(format t "~%> ")
		
		(let* ((resposta (ler-teclado))
				 (opcao-valida (existe-lista resposta '(bfs dfs a*))))		;Perguntar ao stor sobre esta listagem hardcoded aqui	-> deveria ter uma função que me retorna todos os tabuleiros possíveis?
			(cond
				(opcao-valida resposta)
				(T (progn
						(format t "~%> Opcao Invalida!")
						(format t "~%  ")
						(ler-algoritmo)
					)
				)
			)
		)
	)
)
;

;; ler-heurística
(defun ler-heuristica () "Recebe do utilizador a decisão de qual heurística usar"
	(progn
		(format t "~%> Qual a heuristica que pretende aplicar na procura?")
		(format t "~%> Possibilidades: ") 
		(format t "~%> 	1. Proposta - Beneficia os tabuleiros com o maior numero de caixas fechadas")
		(format t "~%> 	2. Desenvolvida - *********************")
		(format t "~%> Heuristica a usar: ")
		(format t "~%> ")
		
		(let* ((resposta (ler-teclado)))
			(cond
				((not (numberp resposta)) (ler-heuristica))		
				((and (<= resposta 2) (>= resposta 1)) resposta)
				(T (progn
						(format t "~%> Opcao Invalida!")
						(format t "~%  ")
						(ler-heuristica)
					)
				)
			)
		)
	)
)
;

;; definir noutro ficheiro 
(defun existe-lista (elemento lista) "Retorna [T] se o elemento recebido existe na lista recebida e devolve [NIL] se o elemento não existe"
	(cond
		((null lista) nil)
		((eql elemento (car lista)) T)
		(T (existe-lista elemento (cdr lista)))
	)
)
;

;; ler-teclado
(defun ler-teclado () "Ler do teclado algo do utilizador"
	(read)
)
;

;;; Problemas a Resolver 

(list estado profundidade heuristica no-pai)

(defun problema-a ()
"Devolver problema a)"
  (cria-no (tabuleiro-a) 0 1 NIL)	;(first (le-tabuleiro (problemas)))	-> Ler de um ficheiro
)

(defun problema-b ()
"Devolve problema b)"
  (cria-no (tabuleiro-b) 0 1 NIL)
)

(defun problema-c ()
"Devolve problema c)"
  (cria-no (tabuleiro-c) 0 1 NIL)
)

(defun problema-d ()
"Devolve problema d)"
  (cria-no (tabuleiro-d) 0 1 NIL)
)

(defun problema-e ()
"Devolver problema e)"
  (cria-no (tabuleiro-e) 0 2 NIL)
)

(defun problema-f ()
"Devolver problema f)"
  (cria-no (tabuleiro-f) 0 1 NIL)
)