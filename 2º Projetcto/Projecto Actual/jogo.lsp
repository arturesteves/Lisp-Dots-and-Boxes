	 ;;;; projecto.lisp
	;;;; Disciplina de IA - 2016 / 2017
	;;;; Programador: Artur Esteves - 140221076
	;;;; Programador: Daniel Costa - 120221058
	;;;; Funções de interacção com o utilizador, de carregamento dos outros ficheiros do projecto e leitura e escrita em ficheiros


	;;; ******************************************************************
	;;; Inicialização do Programa 

	#|
	;;iniciar
	(defun iniciar ()"Função que inicializa o programa, chamando a função que apresenta o menu inicial."
		(progn
			(let ((caminho (inserir-diretoria)))
				  (load-files caminho)
			 )
		)
	)

	;;inserir-diretoria
	(defun inserir-diretoria() "Função que pede ao utilizador para colocar a raiz da pasta onde se encontram os ficheiros do projeto."
		(progn
			(format t "~%Introduza o caminho do ficheiro ~%")
			(format nil (read-line))
		)
	)

	;;load-files ()
	(defun load-files (caminho) "Funcao que carrega os ficheiros de puzzle e procura para o programa e executa o menu"
					(progn 
						(compile-file (concatenate 'string caminho "\\alfabeta.lsp"))
						(compile-file (concatenate 'string caminho "\\pontosecaixas.lsp"))
						(load (concatenate 'string caminho "\\alfabeta.ofasl")) 
						(load (concatenate 'string caminho "\\pontosecaixas.ofasl"))
						(menu-inicial caminho)
				   )
	)

	|#

	;; menu-inicial
	;(defun menu-inicial (caminho) "Apresenta o menu principal do programa na consola. Sendo possível iniciar uma procura ou sair do programa"
	(defun menu-inicial()
		(loop	
			(progn
				(format t "~%> ------------------------------------------------------")
				(format t "~%>|         Puzzle dos Pontos e das Caixas              |")
				(format t "~%>|                                                     |")
				(format t "~%>|            1. Iniciar Jogo	                        |")     
				(format t "~%>|            2. Regras do Jogo                        |")		
				(format t "~%>|            3. Sair                                  |")
				(format t "~%>|                                                     |")
				(format t "~%> ------------------------------------------------------")
				(format t "~%> Opcao")
				(format t "~%> ")
				
				(let ((opcao (ler-teclado)))
					(cond
						;((not (numberp opcao)) (menu-inicial caminho))		
						((not (numberp opcao)) (menu-inicial))		
						((and (<= opcao 3) (>= opcao 1)) (cond
															((= opcao 1) (menu-selecionar-jogo))
															((= opcao 2) (regras-jogo))	
															((= opcao 3) (progn (format t "PROGRAMA TERMINADO")) (return))
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
		)
	)

	;;menu-selecionar-jogo
(defun menu-selecionar-jogo()
		(loop	
			(progn
				(format t "~%> ------------------------------------------------------")
				(format t "~%>|         Puzzle dos Pontos e das Caixas              |")
				(format t "~%>|                                                     |")
				(format t "~%>|            1. Humano vs PC	                        |")     
				(format t "~%>|            2. PC	 vs PC	                        |")		
				(format t "~%>|            3. Sair                                  |")
				(format t "~%>|                                                     |")
				(format t "~%> ------------------------------------------------------")
				(format t "~%> Opcao")
				(format t "~%> ")
				
				(let ((opcao (ler-teclado)))
					(cond
						((not (numberp opcao)) (menu-selecionar-jogo))		
						((and (<= opcao 3) (>= opcao 1)) (cond
															((= opcao 1) (preparar-jogar-humano-pc))
															((= opcao 2) "jogar-pc-pc")	
															((= opcao 3) (progn (format t "PROGRAMA TERMINADO")) (return))
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
		)
)

(defun preparar-jogar-humano-pc() "Menu que apresenta as varias escolhas de jogar, que o humano tem para o jogo que ira realizar."
		(let* ((peca-humano			(progn
									(format t "Que peça pretende utilizar, 1 ou 2? (Selecionar 1 ou 2) ~%") (ler-teclado)))
				(primeiro-a-jogar 	(progn
									(format t "Pretende ser o primeiro a jogar? (s/n) ~%")(ler-teclado)))
			)
			(cond ((AND 
				(OR (equal primeiro-a-jogar 's) (equal primeiro-a-jogar 'n))
				(OR (equal peca-humano '1) (equal peca-humano '2))
			)
				(iniciar-jogo-humano peca-humano primeiro-a-jogar)
				)
			(t (preparar-jogar-humano-pc))
			)
		)
)

(defun iniciar-jogo-humano (peca primeiro-a-jogar) "Função que realiza a primeira jogada do humano num tabuleiro vazio 7x7"

)




;; regras-jogo
(defun regras-jogo() "Apresenta as regras do jogo dos pontos e das caixas"
	(format t "~%> 
		 ---------------- Regras do Puzzle dos Pontos e das Caixas ----------------- 
		| O objetivo do puzzle consiste em fechar um determinado número de caixas 	|
		| a partir de uma configuração inicial do tabuleiro.                      	|
		| Quando o número de caixas por fechar é atingido, o puzzle está resolvido.	|
		| A resolução do puzzle consiste portanto em executar a sucessão de traços 	|
		| que permite chegar a um estado onde o número de caixas por fechar é 	    |
		| alcançado.		                                                    	|
		 ---------------------------------------------------------------------------"
	)
)

	#|
	;;; Estatisticas
	;;sem-resultados
	(defun sem-resultados (no-inicial diretoria) "Função que imprime num ficheiro do tipo .DAT que não existe solução de determinado nó"
			
			(with-open-file (ficheiro (concatenate 'string diretoria "\\estatisticas.dat") 
								:direction :output
								:if-exists :append 
								:if-does-not-exist :create)
				(format ficheiro "~%Estado inicial: ~s ~%" no-inicial)
				(format ficheiro "Sem Solução")
				(format t "~%Estado inicial: ~s ~%" no-inicial)
				(format t "Sem Solução")
		)
	)
	;; resultados
	(defun resultados (no-inicial profundidade-maxima algoritmo heuristica solucao tempo-inicial diretoria) "Função que imprime num ficheiro do tipo .DAT as estatisticas do jogo."
		(let* 
			(
				(tamanho-lista-abertos (car (cdr solucao)))
				(no-solucao (car solucao))
				(estado-solucao (get-no-estado no-solucao))
				(tamanho-lista-fechados (+ (car (cdr (cdr solucao))) 1))
				(nos-gerados (- (+ tamanho-lista-abertos tamanho-lista-fechados) 1))
				(profundidade (get-no-profundidade (get-no-estado solucao)))
				(no-final (get-no-estado solucao))
				(tempo (- (get-universal-time) tempo-inicial))
				(caminho (caminho-solucao no-solucao))
				(valor-heuristico (get-no-heuristica no-solucao))
			)
							
			(with-open-file (ficheiro (concatenate 'string diretoria "\\estatisticas.dat") 
								:direction :output
								:if-exists :append 
								:if-does-not-exist :create)

								
				;; Esta parte será escrita no ficheiro do tipo .DAT
				(format ficheiro "Gerado em ~s~%" (current-date-string))
				(format ficheiro "~%Estado inicial: ~s ~%" no-inicial)
				(format ficheiro "~%Estado final: ~s ~%" estado-solucao)
				(format ficheiro "~%Profundidade maxima: ~s ~%" profundidade-maxima)
				(format ficheiro "~%Algoritmo: ~s ~%" algoritmo)
				(format ficheiro " ~s ~%" heuristica)
				(format ficheiro "~%Profundidade: ~s ~%" profundidade)
				(format ficheiro "~%Nos Gerados: ~s ~%" nos-gerados)
				(format ficheiro "~%Nos expandidos: ~s ~%" tamanho-lista-fechados)
				(format ficheiro "~%Penetrancia: ~s ~%"(penetrancia no-final nos-gerados));
				(format ficheiro "~%Fator de Ramificacao: ~s ~%" (fator-ramificacao profundidade tamanho-lista-fechados))	
				(format ficheiro "~%Valor Heuristico: ~s ~%" valor-heuristico)
				(format ficheiro "~%Caminho ate a solucao: ~s ~%" caminho)	
				(format ficheiro "~%Caixas Fechadas: ~s ~%" (caixas-fechadas (get-no-estado no-final)))
				(format ficheiro "~%Tempo decorrido: ~s segundos ~%" tempo)
				(format ficheiro "___________________________________________________~%")
			)

			;;Esta parte será mostrada na consola
			(format t "Gerado em ~s~%" (current-date-string))
			(format t "~%Estado inicial: ~s ~%" no-inicial)
			(format t "~%Estado final: ~s ~%" estado-solucao)
			(format t "~%Profundidade maxima: ~s ~%" profundidade-maxima)
			(format t "~%Algoritmo: ~s ~%" algoritmo)
			(format t "~%Heuristica: ~s ~%" heuristica)
			(format t "~%Profundidade: ~s ~%" profundidade)
			(format t "~%Nos Gerados: ~s ~%" nos-gerados)
			(format t "~%Nos expandidos: ~s ~%" tamanho-lista-fechados)
			(format t "~%Penetrancia: ~s ~%" (penetrancia no-final nos-gerados))
			(format t "~%Fator de Ramificacao: ~s ~%" (fator-ramificacao profundidade tamanho-lista-fechados))
			(format t "~%Valor Heuristico: ~s ~%" valor-heuristico)
			(format t "~%Caminho ate a solucao: ~s ~%" caminho)	
			(format t "~%Caixas Fechadas: ~s ~%" (caixas-fechadas (get-no-estado no-final)))
			(format t "~%Tempo decorrido: ~s segundos ~%" tempo)
			(format t "___________________________________________________~%")	
		)
	)
	|#



;;; Funções Auxiliares

;; ler-teclado
(defun ler-teclado () "Ler do teclado algo do utilizador"
	(read)
)
	;

	#|
	;; current-date-string [Data actual]
	(defun current-date-string () "Retorna a data no formato de string"
		(multiple-value-bind (sec min hr day mon yr dow dst-p tz)
			(get-decoded-time)
			(declare (ignore dow dst-p tz))	
			(format nil "~A-~A-~A : ~A:~A:~A" yr mon day hr min sec)
		)
	)
	|#
