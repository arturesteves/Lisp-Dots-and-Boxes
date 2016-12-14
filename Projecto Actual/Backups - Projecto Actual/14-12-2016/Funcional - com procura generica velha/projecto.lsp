 ;;;; projecto.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Funções de interacção com o utilizador, de carregamento dos outros ficheiros do projecto e leitura e escrita em ficheiros


;;; ******************************************************************
;;; Inicialização do Programa 
;; iniciar 
(defun iniciar ()	"Função que inicializa o programa, chamando a função que apresenta o menu inicial."
	(progn
		(compile-file (concatenate 'string (diretoria-atual)"puzzle.lsp"))  
		(compile-file (concatenate 'string (diretoria-atual)"procura.lsp"))
		(load (concatenate 'string (diretoria-atual)"puzzle.ofasl")) 
		(load (concatenate 'string (diretoria-atual)"procura.ofasl"))
		(menu-inicial)
	)
)

#|
(defun iniciar ()"Função que inicializa o programa, chamando a função que apresenta o menu inicial."
	(progn
		(let ((caminho (inserir-diretoria)))
			  (load-files caminho)
    	 )
	)
)


(defun inserir-diretoria() 
    (progn
        (format t "~%Introduza o caminho do ficheiro ~%")
        (format nil (read-line))
    )
)
;;load-files ()
(defun load-files (opcao) "Funcao que carrega os ficheiros de puzzle e procura para o programa e executa o menu"
                (progn 
                    (compile-file (concatenate 'string opcao "\\puzzle.lsp"))
                    (compile-file (concatenate 'string opcao "\\procura.lsp"))
                    (load (concatenate 'string opcao "\\puzzle.ofasl")) 
                    (load (concatenate 'string opcao "\\procura.ofasl"))
					(menu-inicial)
               )
)


|#

;; menu-inicial
(defun menu-inicial () "Apresenta o menu principal do programa na consola. Sendo possível iniciar uma procura ou sair do programa"
	(loop	
		(progn
			(format t "~%> ------------------------------------------------------")
			(format t "~%>|         Puzzle dos Pontos e das Caixas              |")
			(format t "~%>|                                                     |")
			(format t "~%>|            1. Iniciar Procura                       |")     
			(format t "~%>|            2. Regras do Jogo                        |")     
			(format t "~%>|            3. Sair                                  |")
			(format t "~%>|                                                     |")
			(format t "~%> ------------------------------------------------------")
			(format t "~%> Opcao")
			(format t "~%> ")
			
			(let ((opcao (ler-teclado)))
				(cond
					((not (numberp opcao)) (menu-inicial))		
					((and (<= opcao 3) (>= opcao 1)) (cond
														((= opcao 1) (iniciar-procura))
														((= opcao 2) (regras-jogo))	
														((= opcao 3) (return))	
													)
					)
					(T (progn
							(format t "~%> Opcao Invalida!")
							(format t "~%> Opcoes Validas: [1, 2]")
							(terpri)
							(format t "~%  ")
						)
					)
				)
			)
		)
	)
)


;; iniciar-procura 
(defun iniciar-procura () 
"Pede ao utilizador toda a informação necessário para começar uma procura no espaço de estados. 
Sendo necessário fornecer o estado inicial, o algoritmo de procura e consoante o algoritmo escolhido é indicada a profundidade máxima e a heurística"

	(let* 	(	 (no 							(cria-no (ler-tabuleiro)))
				 (numero-objectivo-caixas  		(ler-numero-objectivo-caixas))
				 (algoritmo 					(ler-algoritmo))
				 (profundidade 					(cond ((eql algoritmo 'dfs) (ler-profundidade)) (T 9999)))
				 (heuristica 					(cond ((not (or (eql algoritmo 'dfs) (eql algoritmo 'bfs))) (ler-heuristica)) (T nil)))
				 (tempo-inicial					(get-universal-time))
				 (solucao 						(procura-generica no profundidade 'solucaop 'sucessores algoritmo (operadores) heuristica numero-objectivo-caixas)))	
			
			(resultados no profundidade algoritmo heuristica solucao tempo-inicial) ; tempo-inicial
			

	)
)
;

#|
(defun escrever-caminho()
	(format t "~%>")
	(format t "~%> Escreve a diretoria onde se encontram os ficheiros! ")
	(let ((resposta (ler-teclado)))
	(concatenate 'string resposta "\\puzzle.lsp")
	)
)
|#

(defun escreve-solucao-teste-2 (lista)
	(format t "~%~%~A" lista)
)

(defun escreve-solucao-teste (lista)
	(cond
		((null lista) nil)
		(T (progn 
			(format t "~%~A" (car lista))
			(escreve-solucao-teste (cdr lista))
			)
		)
	)
)


;; ler-tabuleiro
(defun ler-tabuleiro () "Lista todos os estados inicias possíveis, recebe a escolha do utilizador e retorna a sua escolha caso esta seja válida"
	(progn
		(format t "~%>")
		(format t "~%> Escolha o estado/tabuleiro inicial do problema ")
		(format t "~%> Possibilidades: ") 
		(format t "~%> 	Tabuleiro A -> Caixas fechadas: 1   |   Dimensao: 3 x 3")
		(format t "~%> 	Tabuleiro B -> Caixas fechadas: 5   |   Dimensao: 4 x 4")
		(format t "~%> 	Tabuleiro C -> Caixas fechadas: 4   |   Dimensao: 4 x 4")
		(format t "~%> 	Tabuleiro D -> Caixas fechadas: 0   |   Dimensao: 5 x 4")
		(format t "~%> 	Tabuleiro E -> Caixas fechadas: 2   |   Dimensao: 6 x 6")
		(format t "~%> 	Tabuleiro F -> Caixas fechadas: 0   |   Dimensao: 7 x 7")
		(format t "~%> 	Tabuleiro G -> Discussão")
		(format t "~%> Estado inicial: ")
		(format t "~%> ")
		
		(let* ((opcao (ler-teclado))
			   (opcao-valida (existe-lista opcao '(a b c d e f))))		
					(with-open-file (ficheiro (concatenate 'string (diretoria-atual)"problemas.dat") :direction :input :if-does-not-exist :error)
						(cond
							((not opcao-valida) (progn
													(format t "~%> Opcao Invalida!")
													(format t "~%  ")
													(terpri)
													(ler-tabuleiro)))
							((equal opcao 'a) (nth 0 (read ficheiro)))
							((equal opcao 'b) (nth 1 (read ficheiro)))
							((equal opcao 'c) (nth 2 (read ficheiro)))
							((equal opcao 'd) (nth 3 (read ficheiro)))
							((equal opcao 'e) (nth 4 (read ficheiro)))
							((equal opcao 'f) (nth 5 (read ficheiro)))
							;((equal opcao 'g) (nth 6 (read ficheiro)))	; se for adicionado ao nosso ficheiro é o problema 6, se for adicionado num ficheiro novo é o problema 1
						)
					)
		)
	)
)


;; ler-numero-objectivo-caixas 
;; TODO: receber o tabuleiro e verificar o numero máximo de caixas a fechar!, para nao serem introduzidas por ex: 100000 caixas a fechar 
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


;; ler-algoritmo
(defun ler-algoritmo () "Recebe do utilizador o nome do algoritmo a usar para efectuar a procura"
	(progn
		(format t "~%> Qual o algoritmo que pretende usar para efectuar a procura?")
		(format t "~%> Serao apresentadoss todos os algoritmos com e o nome a introduzir no sistema: ")
		(format t "~%> 	Breadth-first Search ->  bfs")
		(format t "~%> 	Depth-first Search ->    dfs")
		(format t "~%> 	A* Search ->             a-asterisco")
		(format t "~%> 	IDA* Search ->           ida-asterisco")
		(format t "~%> Algoritmo a usar: ")
		(format t "~%> ")
		
		(let* ((resposta (ler-teclado))
				 (opcao-valida (existe-lista resposta '(bfs dfs a-asterisco ida-asterisco))))		
			(cond
				(opcao-valida resposta)
				(T (progn
						(format t "~%> Opcao Invalida!")
						(format t "~%  ")
						(terpri)
						(ler-algoritmo)
					)
				)
			)
		)
	)
)


;; ler-heurística
(defun ler-heuristica () "Recebe do utilizador a decisão de qual heurística usar"
	(progn
		(format t "~%> Qual a heuristica que pretende aplicar na procura?")
		(format t "~%> Possibilidades: ") 
		(format t "~%> 	1. Proposta pelos professores")
		(format t "~%> 	2. Proposta pelos alunos")
		(format t "~%> Heuristica a usar: ")
		(format t "~%> ")
		
		(let* ((resposta (ler-teclado)))
			(cond
				((or (not (numberp resposta)) (or (> resposta 2) (< resposta 1))) 
					(progn
						(format t "~%> Opcao Invalida!")
						(format t "~%  ")
						(terpri)
						(ler-heuristica)
					))		
				(T (cond
						((= resposta 1) 'heuristica1)
						((= resposta 2) 'heuristica2)
					)
				)
			)
		)
	)
)


;; ler profundidade do algoritmo dfs
(defun ler-profundidade () "Le do utilizador a profundidade para o algoritmo dfs"
	(progn
		(format t "~%> Qual a profundidade que pretende ?")
		(format t "~%> ")
		(let ((resposta (ler-teclado)))
			(cond 
				((or (not (numberp resposta)) (or (> resposta 99999) (<= resposta 0))) 
					(progn
						(format t "~%> Opcao Invalida! Valores compreendidos entre [0,9999]")
						(format t "~%  ")
						(terpri)
						(ler-profundidade)
					))
				(T resposta)
			)
		)
	)
)


;; regras-jogo
(defun regras-jogo() "Apresenta as regras do jogo dos pontos e das caixas"
	(format t "~%> 
		●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●● PUZZLE ●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●● 
		●●                                                   		            ●●
		●● O objetivo do puzzle consiste em fechar um determinado número de caixas  ●●
		●● a partir de uma configuração inicial do tabuleiro.                       ●●
		●● Quando o número de caixas por fechar é atingido, o puzzle está resolvido.●●
		●● A resolução do puzzle consiste portanto em executar a sucessão de traços ●●
		●● que permite chegar a um estado onde o número de caixas por fechar é 	    ●●
		●● alcançado.		                                                    ●●
		●●                                                                    	    ●●
		●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●"
	)
)


;;; Funções I/O
;; diretoria-atual
(defun diretoria-atual () "Função que define um caminho para leitura dos ficheiros."
	(let (
			;(path-daniel "C:\\Users\\Daniel's\\Documents\\Projecto_IA\\Projecto Actual\\"))
			(path-artur  "C:\\Users\\artur\\Documents\\Projectos\\Escola\\Projecto_IA\\Projecto Actual\\"))
			;(path-professor ""))
		path-artur
		;path-daniel
		;path-professor
	)
)



;;; Estatisticas

;; resultados
(defun resultados (no-inicial profundidade-maxima algoritmo heuristica solucao tempo-inicial) "Função que imprime num ficheiro do tipo .DAT as estatisticas do jogo."
	(let* ((tamanho-lista-abertos (car (cdr solucao)))
			(no-solucao (car solucao))
			(estado-solucao (get-no-estado no-solucao))
			(tamanho-lista-fechados (+ (car (cdr (cdr solucao))) 1)) ; é somado sempre +1, pq a procura generica apenas coloca na lista de fechados o nó quando este é solução, mas a verdade é que é necessário contar com ele			  
			(nos-gerados (- (+ tamanho-lista-abertos tamanho-lista-fechados) 1))	; porque o nó inicial não conta
			(profundidade (get-no-profundidade (get-no-estado solucao)))
			(no-final (get-no-estado solucao))
			(tempo (- (get-universal-time) tempo-inicial))
			(caminho (caminho-solucao no-solucao)))
						
		(with-open-file (ficheiro (concatenate 'string (diretoria-atual)"estatisticas.dat") 
							:direction :output
							:if-exists :append 
							:if-does-not-exist :create)
		
	;; Esta parte será escrita no ficheiro do tipo .DAT
		(format ficheiro "Gerado em ~s~%" (current-date-string))
		(format ficheiro "~%Estado inicial: ~s ~%" no-inicial)
		(format ficheiro "~%Estado final: ~s ~%" estado-solucao)
		(format ficheiro "~%Profundidade maxima ~s ~%" profundidade-maxima)
		(format ficheiro "~%Algoritmo: ~s ~%" algoritmo)
		(format ficheiro " ~s ~%" heuristica)
		(format ficheiro "~%Profundidade: ~s ~%" profundidade)
		(format ficheiro "~%Nos Gerados: ~s ~%" nos-gerados)
		(format ficheiro "~%Nos expandidos: ~s ~%" tamanho-lista-fechados)
		(format ficheiro "~%Penetrancia: ~a ~%"  (penetrancia no-final nos-gerados)); (float (/ (second (car abertos))(+ (length abertos) (length fechados)))))
		(format ficheiro "~%Fator de Ramificacao: ~%" (fator-ramificacao profundidade nos-gerados))
		;(format ficheiro "~%Solução: ~s ~%" solucao)	
		(format ficheiro "~%Caminho ate a solucao: ~s ~%" caminho)	
		(format ficheiro "~%Caixas Fechadas: ~s ~%" (caixas-fechadas (get-no-estado no-final)))
		(format ficheiro "~%Tempo decorrido: ~s segundos ~%" tempo)
		
		;(format ficheiro "Profundidade da Solução: ~s ~%" (second (car abertos)))
		(format ficheiro "___________________________________________________~%")
		
		)
	;;Esta parte será mostrada na consola
		(format t "Gerado em ~s~%" (current-date-string))
		(format t "~%Estado inicial: ~s ~%" no-inicial)
		(format t "~%Estado final: ~s ~%" estado-solucao)
		(format t "~%Profundidade maxima ~s ~%" profundidade-maxima)
		(format t "~%Algoritmo: ~s ~%" algoritmo)
		(format t "~%Heuristica: ~s ~%" heuristica)
		(format t "~%Profundidade: ~s ~%" profundidade)
		(format t "~%Nos Gerados: ~s ~%" nos-gerados)
		(format t "~%Nos expandidos: ~s ~%" tamanho-lista-fechados)
		(format t "~%Penetrancia: ~a ~%" (penetrancia no-final nos-gerados)) ;(float (/ (second (car abertos))(+ (length abertos) (length fechados)))))
		(format t "~%Fator de Ramificacao: ~%" (fator-ramificacao profundidade nos-gerados))	
		;(format t "~%Solução: ~s ~%" solucao)	
		(format t "~%Caminho ate a solucao: ~s ~%" caminho)	
		(format t "~%Caixas Fechadas: ~s ~%" (caixas-fechadas (get-no-estado no-final)))
		(format t "~%Tempo decorrido: ~s segundos ~%" tempo)
		(format t "___________________________________________________~%")
	)
)



;;; Funções Auxiliares
;; existe-lista
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

;; current-date-string [Data actual]
(defun current-date-string () "Retorna a data no formato de string"
	(multiple-value-bind (sec min hr day mon yr dow dst-p tz)
		(get-decoded-time)
		(declare (ignore dow dst-p tz))	
		(format nil "~A-~A-~A : ~A:~A:~A" yr mon day hr min sec)
	)
)