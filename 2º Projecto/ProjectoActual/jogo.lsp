;;;; jogo.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058


;;;; Constantes:
(defvar *jogador1* 1)
(defvar *jogador2* 2)


;;; ------------------------------------------------------------------------------
;;; Iniciação do Jogo
;;; ------------------------------------------------------------------------------

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
					(menu-inicial)
			   )
)


;; menu-inicial
(defun menu-inicial() "Apresenta o menu principal do programa na consola. Sendo possível iniciar uma procura ou sair do programa"
	(loop	
		(progn
			(format t "~%> ------------------------------------------------------")
			(format t "~%>|         Puzzle dos Pontos e das Caixas              |")
			(format t "~%>|                                                     |")
			(format t "~%>|            1. Iniciar Jogo	                    |")     	
			(format t "~%>|            2. Sair                                  |")
			(format t "~%>|                                                     |")
			(format t "~%> ------------------------------------------------------")
			(format t "~%> Opcao")
			(format t "~%> ")
			
			(let ((opcao (ler-teclado)))
				(cond	
					((not (numberp opcao)) (menu-inicial))		
					((and (<= opcao 2) (>= opcao 1)) (cond
														((= opcao 1) (iniciar-jogada))	
														((= opcao 2) (progn (format t "PROGRAMA TERMINADO")) (return))
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

;;iniciar-jogada
(defun iniciar-jogada()
		(loop	
			(progn
				(format t "~%> ------------------------------------------------------")
				(format t "~%>|         Puzzle dos Pontos e das Caixas              |")
				(format t "~%>|                                                     |")
				(format t "~%>|            1. Humano vs Humano (Proposta)           |")
				(format t "~%>|            2. Humano vs PC 			    		|") 
				(format t "~%>|            3. Voltar atrás	                    	|")		
				(format t "~%>|                                                     |")
				(format t "~%> ------------------------------------------------------")
				(format t "~%> Opcao")
				(format t "~%> ")	
				(let ((opcao (ler-teclado)))
					(cond
						((not (numberp opcao)) (menu-selecionar-jogo))		
						((and (<= opcao 3) (>= opcao 1)) (cond
															((= opcao 1) (fazer-uma-partida-humano-humano))
															((= opcao 2) (fazer-uma-partida-humano-pc))
															((= opcao 3) (return))
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

;;Utilizado para Humano vs Humano	
(defun fazer-uma-partida-humano-humano () "Caso pretende ser o primeiro a jogar. Passa o jogador1, caso contrario jogador2"
  (if (y-or-n-p "Quer ser o 1º a iniciar a partida como Jogador 1? (y/n)")
      (humano-humano-joga (tabuleiro-inicial) *jogador1* 0 0)
      (humano-humano-joga (tabuleiro-inicial) *jogador2* 0 0)))

;;Utilizado para Humano vs Computador	  
(defun fazer-uma-partida-humano-pc () "Caso pretende ser o primeiro a jogar. Passa o jogador1, caso contrario jogador2"
  (if (y-or-n-p "Quer ser o 1º a iniciar a partida como Jogador 1? (y/n)")
      (humano-joga (tabuleiro-inicial) *jogador1* 0 0)
	  (computador-joga (tabuleiro-vazio-3x3) *jogador2* 0 0)))
	  ;(computador-joga (tabuleiro-inicial) *jogador2* 0 0)))
	  ;(computador-joga (tabuleiro-teste1-fecha-1-caixa) *jogador2* 0 0)))	; pc começa
	  ;(computador-joga (tabuleiro-teste2) *jogador2* 0 0)))
	  


	  
	  
	  

;;; ------------------------------------------------------------------------------
;;; JOGADA HUMANO VS HUMANO			- APRESENTADA COMO PROPOSTA
;;; ------------------------------------------------------------------------------
;;humano-joga 
(defun humano-humano-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2)
	(let* 	(
				(jogada (le-jogada tabuleiro))
				(novo-tabuleiro (faz-jogada tabuleiro peca (first jogada) (second jogada) (third jogada)))		
				(numero-caixas-jogador (caixas-fechadas  novo-tabuleiro))
			)
				(cond				
					((vencedor-p numero-caixas-jogador peca numero-caixas-j1 numero-caixas-j2) 	(progn	
																																						(format t "~&Ganhou!")(jogar-de-novo)))
					((tabuleiro-preenchido-p novo-tabuleiro) (format t "~&Empatamos."))
					(	(and
							(= peca *jogador1*) 
							(> numero-caixas-jogador numero-caixas-j1)
						)
						(progn
						(imprime-tabuleiro novo-tabuleiro)
						(humano-humano-joga novo-tabuleiro peca numero-caixas-jogador numero-caixas-j2)
						)
					)
					(
						(and 
							(= peca *jogador2*)
							(> numero-caixas-jogador numero-caixas-j2)
						)
						(progn
						(imprime-tabuleiro novo-tabuleiro)
						(humano-humano-joga novo-tabuleiro peca numero-caixas-j1 numero-caixas-jogador)
						)
					)
					(t
						(progn
						(imprime-tabuleiro novo-tabuleiro)
						(humano-humano-joga novo-tabuleiro (trocar-peca peca) numero-caixas-j1 numero-caixas-j2)
						)
					)
				)
    )
)


;;; ------------------------------------------------------------------------------
;;; JOGADA HUMANO VS COMPUTADOR
;;; ------------------------------------------------------------------------------
#|
(defun faz-jogada (tabuleiro peca operador x y)
	(funcall operador x y peca tabuleiro)
)
|#
;;humano-joga 
(defun humano-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2)
(format t "~%~%~%Entrei humano-joga~%")

	(let* 	(
				(jogada (le-jogada tabuleiro)) ;; (INSERIR-ARCO-HORIZONTAL 1 1)
				(numero-caixas-fechadas-antigo (caixas-fechadas tabuleiro))
				(novo-tabuleiro (faz-jogada tabuleiro peca (first jogada) (second jogada) (third jogada)))		
				(numero-caixas-jogador (caixas-fechadas  novo-tabuleiro))
				;(continua-vez-do-humano (
			)
				;(format t "~%Numero-caixas-jogador: ~a"  numero-caixas-jogador)
				
				(format t "~%~%~%Numero Caixas: ~A~%" numero-caixas-jogador)
				(format t "~%~%~%Caixas J1: ~A~%" numero-caixas-j1)
				(format t "~%~%~%Caixas J2: ~A~%" numero-caixas-j2)
				(cond				
					((vencedor-p numero-caixas-jogador peca numero-caixas-j1 numero-caixas-j2) 	(progn	
																																						(format t "~&Ganhou!")
																																						(jogar-de-novo)))
					((tabuleiro-preenchido-p novo-tabuleiro) (format t "~&Empatamos."))
					
					((and (= peca *jogador1*) (> numero-caixas-jogador numero-caixas-j1))
					;((and (= peca *jogador1*) (> numero-caixas-jogador numero-caixas-fechadas-antigo))
						#||(progn
						(imprime-tabuleiro novo-tabuleiro)
						(humano-joga novo-tabuleiro peca numero-caixas-jogador  numero-caixas-j2)
						)||#
						(imprime-tabuleiro novo-tabuleiro)
						(humano-joga novo-tabuleiro peca numero-caixas-jogador numero-caixas-j2))
					
					((and (= peca *jogador2*) (> numero-caixas-jogador numero-caixas-j2))
					;((and (= peca *jogador2*) (> numero-caixas-jogador numero-caixas-fechadas-antigo))
						(progn
						(imprime-tabuleiro novo-tabuleiro)
						(humano-joga novo-tabuleiro peca numero-caixas-j1 numero-caixas-jogador)))
					(T 
						(progn
						;(imprime-tabuleiro novo-tabuleiro)
						(computador-joga novo-tabuleiro (trocar-peca peca) numero-caixas-j1 numero-caixas-j2)
						)
					)
				) ;;; NAO ESQUECER -> FECHOU CAIXA VOLTA A JOGAR!!!
    )
)

#|| VELHA
(defun computador-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2) 
		(let* 	(
				(tempo-inicial (get-universal-time)) ;; get tempo atual
				;(alfa-beta (no profundidade-limite peca f-utilidade &optional (alfa -1000) (beta 1000) &aux ((tempo-inicial (get-universal-time)))))
				
				;; em vez de 0 é o valor da prof. do tabuleiro + 1
				(valor-alfa-beta (alfa-beta (cria-no tabuleiro nil numero-caixas-j1 numero-caixas-j2) 5 peca 'funcao-utilidade -1000 1000 tempo-inicial))
				;(jogada (melhor-jogada-pc tabuleiro)) ;(INSERIR-ARCO-VERTICAL 7 6)
				
				(novo-tabuleiro (faz-jogada tabuleiro peca (first jogada) (second jogada) (third jogada))) ;; Retorna tabuleiro que executa o operador
				(numero-caixas-jogador (caixas-fechadas  novo-tabuleiro))
				
				(fechou-caixa  (cond((> numero-caixas-jogador (caixas-fechadas tabuleiro)) T) (T NIL)) )
				

				;;														1

				)
					(cond				
						((vencedor-p peca numero-caixas-jogador numero-caixas-j1 numero-caixas-j2) 	(progn	(format t "~&Ganhou!")(jogar-de-novo)))
						((tabuleiro-preenchido-p novo-tabuleiro) (format t "~&Empatamos."))
						(	(and
								(= peca *jogador1*) 
								(> numero-caixas-jogador numero-caixas-j1)
							)
							(progn
							(imprime-tabuleiro novo-tabuleiro)
							(computador-joga novo-tabuleiro peca numero-caixas-jogador numero-caixas-j2)
							)
						)
						(
							(and 
								(= peca *jogador2*)
								(> numero-caixas-jogador numero-caixas-j2)
							)
							(progn
							(imprime-tabuleiro novo-tabuleiro)
							(computador-joga novo-tabuleiro peca numero-caixas-j1 numero-caixas-jogador)
							)
						)
						(t
							(progn
							(imprime-tabuleiro novo-tabuleiro)
							;(humano-humano-joga novo-tabuleiro (trocar-peca peca) numero-caixas-j1 numero-caixas-j2)
							(humano-joga novo-tabuleiro (trocar-peca peca) numero-caixas-j1 numero-caixas-j2)
							)
						)
					)
		)
)
||#
;cria-no (estado &optional (profundidade 0) (utilidade nil) (caixas-jogador-1 0)(caixas-jogador-2 0)) 
;; -joga

(defun computador-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2) 
(format t "~%~%~%Entrei computador-joga~%")



		(let* 	(
				(tempo-inicial (get-universal-time)) ;; get tempo atual			
				;;;;;;alfa-beta (no profundidade-limite peca f-utilidade &optional (alfa -1000) (beta 1000) &aux ((tempo-inicial (get-universal-time))
				(valor-alfa-beta (alfa-beta (cria-no tabuleiro 0 nil numero-caixas-j1 numero-caixas-j2) 4 peca 'funcao-utilidade))
				;;;;; Este valor 'valor-alfa-beta' usar para escrever no log.
				(novo-tabuleiro (get-no-estado *jogada-pc*))	;; ESTA JOGADA-PC está a null!						
				(numero-caixas-jogador (caixas-fechadas  novo-tabuleiro))
				;(estatisticas (estatisticas-log *jogada-pc* peca numero-caixas-j1 numero-caixas-j2))

				)
				(progn				
				
				;(format t "~%~%~%Jogada PC guardada: ~A~%" *jogada-pc*) ;; Se passa nil automaticamente tabulero terá nil LOGO 
				(format t "~%~%~%TABuleiro inicial: ~A~%" tabuleiro)
				(format t "~%~%~%TABuleiro da Jogada PC guardada: ~A~%" novo-tabuleiro)
				(format t "~%Nó guardado: ~a~%" *jogada-pc*)
				(format t "~%~%~%Numero Caixas: ~A~%" numero-caixas-jogador)
					(cond				
						;((vencedor-p numero-caixas-jogador peca numero-caixas-j1 numero-caixas-j2) 	(progn	(format t "~&Ganhou!")(jogar-de-novo)))
						((tabuleiro-preenchido-p novo-tabuleiro) (format t "~&Empatamos."))
						(	(and
								(= peca *jogador1*) 
								(> numero-caixas-jogador numero-caixas-j1)
							)
							(progn
							(imprime-tabuleiro novo-tabuleiro)
							(computador-joga novo-tabuleiro peca numero-caixas-jogador numero-caixas-j2)
							)
						)
						(
							(and 
								(= peca *jogador2*)
								(> numero-caixas-jogador numero-caixas-j2)
							)
							(progn
							(imprime-tabuleiro novo-tabuleiro)
							(computador-joga novo-tabuleiro peca numero-caixas-j1 numero-caixas-jogador)
							)
						)
						(t
							(progn
							(imprime-tabuleiro novo-tabuleiro)
							(humano-joga novo-tabuleiro (trocar-peca peca) numero-caixas-j1 numero-caixas-j2)
							)
						)
					)
					
					)
		)
)

;;; Funções Auxiliares a Jogada
#|
;;le-jogada-pc
(defun melhor-jogada-pc (tabuleiro) "Le uma jogada fazendo a verificacao da sua legalidade. A jogada lida (arco-horizontal ou arco-vertical) e a posicao na no tabuleiro (entre 1 e 8)"
  (format t "~&A sua jogada: ~% ")
  (let* ( 
			(operador (random-set (operadores)))
			(x (random 9))
			(y (random 8))
		)
	(cond	
			((eq x 0)	(list (random-set (operadores)) (+ x 1) y))
			((eq y 0)	(list (random-set (operadores)) x (+ y 1)))
		  (	(or
			 (and	(equal operador 'inserir-arco-horizontal) 	(nth (- y 1) (nth (- x 1) (first tabuleiro))))
			 (and 	(equal operador 'inserir-arco-vertical) 	(nth (- y 1) (nth (- x 1) (second tabuleiro))))
			)
			(format t "~&Esta casa ja esta ocupada.")
			(melhor-jogada-pc tabuleiro)
			)
		  (t (list operador x y))
	)
  )
)


;funções auxiliares para melhor jogada do computador
(defun random-set(set) "Pick one element of set, and make a list of it."
  (append (random-elt set)))

(defun random-elt (element) "Choose an element from a list at random."
  (elt element (random (length element))))

  
;;jogar-de-novo
(defun jogar-de-novo()
	(let ((novo-jogo (progn (format t "~% Pretende jogar de novo? (s/n)")(ler-teclado))))
		(cond
			((equal novo-jogo 's)(menu-inicial))
			(t nil)
		)
	)
)
|#

(defun vencedor-p (novo-numero-caixas peca caixas-jogador1 caixas-jogador2) 
	(let ((resultado (>= novo-numero-caixas 25)))
		
	;	(format t "~%Entrei em Vencedorp~%")
	;	(format t "~%Peca: ~a~%" peca )
		; (format t "~%Caixas-jogador-1: ~a~%" caixas-jogador1)
		; (format t "~%Caixas-jogador-2: ~a~%" caixas-jogador2)
		(cond
			(resultado (cond
							(	(and
								(= peca *jogador1*)
								(> caixas-jogador1 caixas-jogador2)
								)
							*jogador1*
							)
							(	(and
								(= peca *jogador2*)
								(< caixas-jogador1 caixas-jogador2)
								)
							*jogador2*
							)
						)
			)
			(T nil)					
		)
	)
)

#|
;;vencedor-p 	novo-numero-caixas e o numero de caixas fechadas existentes
(defun vencedor-p (novo-numero-caixas peca caixas-jogador1 caixas-jogador2) "determina se existe um vencedor. Se existir devolve o jogador que venceu. Senao devolve NIL."
  (cond
    ((null novo-numero-caixas) nil)
	(
			(and 
			(= peca *jogador1*)
			(>= novo-numero-caixas 25)) 
			*jogador1*
	)
    
	(
		(and
			(= peca *jogador2*)
			(>= novo-numero-caixas 25)
		) *jogador2*
	)
    
	(T nil)
    )
)

|#

;;le-jogada
(defun le-jogada (tabuleiro) "Le uma jogada fazendo a verificacao da sua legalidade. A jogada lida (arco-horizontal ou arco-vertical) e a posicao na no tabuleiro (entre 1 e 8)"
  (format t "~&A sua jogada: ")
  (let*  ( 
		 (operador (le-operador))
		 (x (le-valor-x 'x))
		 (y (le-valor-y 'y)) 
		)
	(cond
		  (	(or
			 (and	(equal operador 'inserir-arco-horizontal) 	(nth (- y 1) (nth (- x 1) (first tabuleiro))))
			 (and 	(equal operador 'inserir-arco-vertical) 	(nth (- y 1) (nth (- x 1) (second tabuleiro))))
			)
			(format t "~&Esta casa ja esta ocupada.")
			(le-jogada tabuleiro)
			)
		  (t (list operador x y))
	)
  )
)


;;le-operador
(defun le-operador ()
	(format t "~%> ------------------------------------------------------")
	(format t "~%>|        			 Tipo de Jogada				       	|")
	(format t "~%>|                                                     |")
	(format t "~%>|            1.colocacao de um arco horizontal        |")     	
	(format t "~%>|            2.colocacao de um arco vertical          |")
	(format t "~%>|                                                     |")
	(format t "~%> ------------------------------------------------------")
	(format t "~%> Opcao")
	(format t "~%> ")
	  (let ((operador-lido (read)))
			(cond
			 ((or (not (integerp operador-lido))
						 (< operador-lido 1) (> operador-lido 2))
			   (format t "~&Entrada invalida.")(le-operador))
			   (T (cond
				   ((= 1 operador-lido) 'inserir-arco-horizontal)
				   (T 'inserir-arco-vertical)))
			)
		)
)

;;le-valor-x
(defun le-valor-x (valor)
  (format t "~&Valor de ~A [1 <= ~A <= 8]: " valor valor)
  (let ((valor-lido (read)))
		(cond
			(
				(or 
					(not(integerp valor-lido))
					(< valor-lido 1) (> valor-lido 8)	
				)			
				(format t "~&Entrada invalida.")(le-valor-x valor)
			)
		   (T valor-lido)
		)
	)
)

;; Esta função foi necessária para a coordenada Y, que será entre 1 e 7 
;;le-valor-y
(defun le-valor-y (valor)
  (format t "~&Valor de ~A [1 <= ~A <= 7]: " valor valor)
  (let ((valor-lido (read)))
		(cond
			(
				(or 
					(not(integerp valor-lido))
					(< valor-lido 1) (> valor-lido 7)	;; y so pode ir até 7 <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
				)			
				(format t "~&Entrada invalida.")(le-valor-y valor)
			)
		   (T valor-lido)
		)
	)
)

;;; faz-jogada
(defun faz-jogada (tabuleiro peca operador x y)"Faz uma jogada com base numa das duas operacoes posiveis, num tabuleiro, uma peca com a qual jogar e duas coordenadas recebidos por parametro."
	(funcall operador x y peca tabuleiro)
)



;;; ------------------------------------------------------------------------------
;;; IMPRIME TABULEIRO
;;; ------------------------------------------------------------------------------
#|
(defun imprime-tabuleiro (tabuleiro) "Imprime o tabuleiro, linha a linha"
  (let ((linhas (first tabuleiro)) (colunas (rodar (second tabuleiro))))
    (mapcar #'(lambda (linha coluna) (progn (imprime-linha linha) (imprime-coluna coluna) (imprime-coluna coluna))) linhas colunas)
  )
)
|#
;; NOTA: Ele continua a imprimir uma lista de nils devido a iteração do mapcar.
;;imprime-tabuleiro
(defun imprime-tabuleiro (tabuleiro) "Imprime o tabuleiro, linha a linha"
	(let ((linhas (first tabuleiro)) (colunas (append (rodar (second tabuleiro)) '(NIL))))
		(mapcar #'(lambda (linha coluna) (progn (imprime-linha linha) 
												(imprime-coluna coluna)
												(imprime-coluna coluna))) linhas colunas)
	)
)

;; converte-arco-horizontal
(defun converte-arco-horizontal (v)
  "Converte os inteiros dos arcos horizontais para os simbolos --- (jogador com peca 1) e ... (jogador com peca 2)"
  (cond ((equal v 1) "___")
		((equal v 2) "...")
		(t "   ")))

;; converte-arco-vertical
(defun converte-arco-vertical (v)
  "Converte os inteiros dos arcos verticais para os simbolos | (jogador com peca 1) e  . (jogador com peca 2) "
  (cond ((equal v 1) "|  ")
		((equal v 2) ".  ")
		(t "   ")))

;; rodar
(defun rodar (matriz) "Transposta de matriz"
  (apply #'mapcar #'list matriz))
  
;; trocar-peca
(defun trocar-peca (peca) "Troca a peca de um jogador para a peca de outro jogador."
  (cond
	((= peca *jogador1*) *jogador2*)
	((= peca *jogador2*) *jogador1*)
	)
)

;; imprime-linha
(defun imprime-linha (lista) "Imprime uma linha formatada do tabuleiro"
  (format t ". ~A . ~A . ~A . ~A . ~A . ~A . ~A . ~%"
          (converte-arco-horizontal (first lista))
          (converte-arco-horizontal (second lista))
          (converte-arco-horizontal (third lista))
          (converte-arco-horizontal (fourth lista))
          (converte-arco-horizontal (fifth lista))
          (converte-arco-horizontal (sixth lista))
          (converte-arco-horizontal (seventh lista))
          (converte-arco-horizontal (eighth lista))
))

;; imprime-coluna

(defun imprime-coluna (lista)"Imprime uma linha formatada do tabuleiro"
  (format t "~A   ~A   ~A   ~A   ~A   ~A   ~A   ~A  ~%"
          (converte-arco-vertical (first lista))
          (converte-arco-vertical (second lista))
          (converte-arco-vertical (third lista))
          (converte-arco-vertical (fourth lista))
          (converte-arco-vertical (fifth lista))
          (converte-arco-vertical (sixth lista))
          (converte-arco-vertical (seventh lista))
          (converte-arco-vertical (eighth lista))

))



;;; Auxiliares do Jogo
(defun tabuleiro-inicial (&optional stream)
  "Permite criar o tabuleiro inicial do jogo."
  (cond ((null stream) '(
	((NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL)
	(NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL)
	(NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL)
	(NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL))
	((NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL)
	(NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL)
	(NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL)
	(NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL))
))
		(t (read stream))))



		
;;; ------------------------------------------------------------------------------
;;; IMPRIME TABULEIRO
;;; ------------------------------------------------------------------------------
#|
	CONFIRMAR AINDA SE ESTA A CORRER TODAS AS LISTAS DO TABULEIRO!!!
|#


(defun tabuleiro-preenchido-p (tabuleiro)"Verifica se o tabuleiro ja esta completamente preenchido"
	(cond
		((null tabuleiro) 0)	
		( 	(= 	(contar-nils-sublistas tabuleiro) (length tabuleiro)
			) T)
		(t nil)
	)
)

(defun contar-nils-sublistas(lista)
	(cond
		((null lista) 0)
		(	(= (contar-nils-lista (first (get-arcos-horizontais lista))) 0) ;;contar-nils-lista do pontosecaixas
			(+ 1 (contar-nils-sublistas (cdr lista)))
		)
		(t (contar-nils-sublistas (cdr lista)))
	)
)


;ler-teclado
(defun ler-teclado () "Ler do teclado algo do utilizador"
	(read)
)


;;; ------------------------------------------------------------------------------
;;; Estatisticas			- PARA MAIS TARDE!!!!
;;; ------------------------------------------------------------------------------
(defun estatisticas-log (no peca caixas-fechadas-j1 caixas-fechadas-j2) "escreve as estatisticas num ficheiro"
	(let ((ficheiro (concatenate 'string (diretoria-atual) "\\log.dat")))
		(with-open-file (file ficheiro :diretion
										:output
										:if-exists
										:append
										:if-does-not-existe
										:create)
			;; Esta parte será escrita no ficheiro do tipo .DAT
			(format file "Data ~s~%" (current-date-string))
			(format file "~%Vencedor: ~s ~%"(vencedor-p (novo-numero-caixas peca caixas-j1 caixas-j2)) )
			(format file "~%Tabuleiro: ~s ~%"(get-no-estado no))
			(format file "~%Cortes Alfa: ~s ~%" *corte-alfa*)
			(format file "~%Cortes Beta: ~s ~%" *corte-beta*)
			(format file "~%Nos analisados: ~s ~%"*nos-analisados* )
			(format file "~%Tempo Máximo: ~s ~%" *tempo-despendido*)
			(format file "___________________________________________________~%")
		)
		;;Esta parte será mostrada na consola
			(format t "Data ~s~%" (current-date-string))
			(format t "~%Vencedor: ~s ~%" )
			(format t "~%Tabuleiro: ~s ~%" )
			(format file "~%Cortes Alfa: ~s ~%"*corte-alfa* )
			(format file "~%Cortes Beta: ~s ~%"*corte-beta* )
			(format t "~%Nos analisados: ~s ~%"*nos-analisados* )
			(format t "~%Tempo Máximo: ~s ~%" *tempo-despendido*)

	)
)