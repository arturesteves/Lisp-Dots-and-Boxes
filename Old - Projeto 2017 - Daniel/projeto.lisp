;;;; Projeto Inteligência Artificial 2016-2017
;;;; Daniel Fernandes Costa
;;;; Data:01 Novembro de 2016
;;;; Projeto 1 - Menu

;;; Compile e Load dos restantes ficheiros
(defun compilar-ficheiros()
"Função que permite fazer a compilação dos ficheiros"
	(compile-file "C:/Users/Daniel's/Documents/Projeto 2017/puzzle.lisp")
	(compile-file "C:/Users/Daniel's/Documents/Projeto 2017/procura.lisp")
)

(defun ler-ficheiros()
"Função que permite fazer o load dos seguintes ficheiros"
	(load  "C:/Users/Daniel's/Documents/Projeto 2017/puzzle.ofasl")
	(load  "C:/Users/Daniel's/Documents/Projeto 2017/procura.ofasl")
)

;;; Menu Principal
(defun menu-principal()
compilar-ficheiros()
ler-ficheiros()
	"Menu principal com as diversas opções de escolha"
	(format t "~% ●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●")
	(format t "~% ●	 Puzzle dos Pontos e das Caixas   ●")
	(format t "~% ●		1- Resolver Puzzle          ●")
	(format t "~% ●		2- Ler Regras do Jogo	    ●")
	(format t "~% ●		3- Sair			    ●")
	(format t "~% ●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●")
	(format t "~%~% Escolher Opção:")
	(let 
		((opcao (read)))
		(cond
			((equal opcao '1) (menu-escolha-problema) )
			((equal opcao '2) (regras-jogo) )
			((equal opcao '3) "Jogo Terminado. Obrigado pela sua participação!")   
			(T 
				(progn  
					(format t "~%Opção Inválida. Escolher uma das opções do menu ~%~%Opção: ")
					(setf opcao (read))
				)
			)
		)
	)
)

;;; Menu para escolher o problema dado pelo docente.
(defun menu-escolha-problema()
	"Menu de escolha de um dos diversos problemas"
		(format t "~% ●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●")
	(format t "~% ●	    		Problemas 	    ●")
	(format t "~% ●		1- Problema A         	    ●")
	(format t "~% ●		2- Problema B         	    ●")
	(format t "~% ●		3- Problema C         	    ●")
	(format t "~% ●		4- Problema D         	    ●")
	(format t "~% ●		5- Problema E         	    ●")
	(format t "~% ●		6- Problema F         	    ●")
	(format t "~% ●		7- Voltar atrás		    ●")
	(format t "~% ●		8- Sair			    ●")
	(format t "~% ●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●")
	(format t "~%~% Escolher Opção:")
	(let 
		((opcao (read)))
		(cond
			((equal opcao '1) (menu-escolha-algoritmo 1) ) ; (nth 0 (leitura-problemas))
			((equal opcao '2) (menu-escolha-algoritmo 2) ) ; (nth 1 (leitura-problemas))
			((equal opcao '3) (menu-escolha-algoritmo 3) ) ; (nth 2 (leitura-problemas))
			((equal opcao '4) (menu-escolha-algoritmo 4) ) ; (nth 3 (leitura-problemas))
			((equal opcao '5) (menu-escolha-algoritmo 5) ) ; (nth 4 (leitura-problemas))
			((equal opcao '6) (menu-escolha-algoritmo 6) ) ; (nth 5 (leitura-problemas))
			((equal opcao '7) (menu-principal) )
			((equal opcao '8) "Jogo Terminado. Obrigado pela sua participação!")   
			(T 
				(progn  
					(format t "~%Opção Inválida. Escolher uma das opções do menu ~%~%Opção: ")
					(setf opcao (read))
				)
			)
		)
	)
)



;;; Menu para escolher o algoritmo para executar no problema anteriormente selecionado
(defun menu-escolha-algoritmo(problema)
	"Menu de escolha de um dos algoritmos"
	(format t "~% ●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●")
	(format t "~% ●	    		Algoritmo	     ●")
	(format t "~% ●		1- Depth-First (Profundidade)●")
	(format t "~% ●		2- Breath-First (Largura)    ●")
	(format t "~% ●		3- A *        		     ●")
	(format t "~% ●		4- SMA*        		     ●")
	(format t "~% ●		5- Sair			     ●")
	(format t "~% ●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●")
	(format t "~%~% Escolher Opção:")
	(let 
((opcao (read)))
		(cond
			((equal opcao '1)  (menu-escolha-profundidade))
			((equal opcao '2)  )
			((equal opcao '3)  )
			((equal opcao '4)  )
			((equal opcao '5) "Jogo Terminado. Obrigado pela sua participação!")   
			(T 
				(progn  
					(format t "~%Opção Inválida. Escolher uma das opções do menu ~%~%Opção: ")
					(setf opcao (read))
				)
			)
		)
	)
)

;;; Menu para escolher a profundidade do Depth-First (Procura em profundidade)
(defun menu-escolha-profundidade ()
  (format t "Escolhe a profundidade para o algoritmo Depth-First (0 - para não ter profundidade)~%")
	(let ((opcao (read)))
		(cond
		((not (numberp opcao)) 0)
		(t opcao)
		)
	)
  )



;;; Regras do Jogo
(defun regras-jogo()
	(format t "
	●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●● PUZZLE ●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●● 
	●●                                                   		            ●●
	●● O objetivo do puzzle consiste em fechar um determinado número de caixas  ●●
	●● a partir de uma configuração inicial do tabuleiro.                       ●●
	●● Quando o número de caixas por fechar é atingido, o puzzle está resolvido.●●
	●● A resolução do puzzle consiste portanto em executar a sucessão de traços ●●
	●● que permite chegar a um estado onde o número de caixas por fechar é 	    ●●
	●● alcançado.								    ●●
	●●                                                                    	    ●●
	●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●● 
	"
	)
	(menu-principal)
)


;;; Funções Auxiliar 
(defun leitura-problemas()
	"Função auxiliar para ler os problemas a partir de uma diretoria"
	(with-open-file
		(ficheiro "C:/Users/Daniel's/Documents/Projeto 2017/problemas.dat"
			:direction
			:input
			:if-does-not-exist
			:error
		)
		(read ficheiro)
	)
)