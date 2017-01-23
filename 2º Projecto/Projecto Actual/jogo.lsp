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
				;	(compile-file (concatenate 'string caminho "\\alfabeta.lsp"))
					(compile-file (concatenate 'string caminho "\\pontosecaixas.lsp"))
				;	(load (concatenate 'string caminho "\\alfabeta.ofasl")) 
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
			(format t "~%>|            1. Iniciar Jogo	                        |")     	
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

;;menu-selecionar-jogo
(defun iniciar-jogada()
		(loop	
			(progn
				(format t "~%> ------------------------------------------------------")
				(format t "~%>|         Puzzle dos Pontos e das Caixas              |")
				(format t "~%>|                                                     |")
				(format t "~%>|            1. Humano vs Humano                      |")
				(format t "~%>|            2. Humano vs PC 			                |")  
				(format t "~%>|            3. Voltar atrás	                        |")		
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
(defun fazer-uma-partida-humano-humano () "Caso pretende ser o primeiro a jogar. Passa o jogador1, caso contrario jogador2"
  (if (y-or-n-p "Quer iniciar a partida como Jogador 1? (y/n)")
      (humano-joga (tabuleiro-inicial) *jogador1* 0 0)
      (humano-joga (tabuleiro-inicial) *jogador2* 0 0)))
	  
(defun fazer-uma-partida-humano-pc () "Caso pretende ser o primeiro a jogar. Passa o jogador1, caso contrario jogador2"
  (if (y-or-n-p "Quer iniciar a partida? (y/n)")
      (humano-joga (tabuleiro-inicial) *jogador1* 0 0)
      (humano-joga (tabuleiro-inicial) *jogador2* 0 0)))



;;; ------------------------------------------------------------------------------
;;; JOGADA HUMANO VS HUMANO
;;; ------------------------------------------------------------------------------

;;humano-joga 
(defun humano-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2)
	(let* 	(
				(jogada (le-jogada tabuleiro))	
				(novo-tabuleiro (faz-jogada tabuleiro peca (first jogada) (second jogada) (third jogada)))		
				(numero-caixas-jogador (caixas-fechadas  novo-tabuleiro))
			)
				(cond
					((vencedor-p peca numero-caixas-jogador numero-caixas-j1 numero-caixas-j2) 	(progn
																									(format t "~&Ganhou!")
																									(jogar-de-novo)
																								)
					)
					((tabuleiro-preenchido-p novo-tabuleiro) (format t "~&Empatamos."))
					(	(and
							(= peca *jogador1*) 
							(> numero-caixas-jogador numero-caixas-j1)
						)
						(humano-joga novo-tabuleiro peca numero-caixas-jogador-j1 numero-caixas-j2)	;; antes: numero-caixas-jogador numero-caixas-j2
					)
					(
						(and 
							(= peca *jogador2*)
							(> numero-caixas-jogador numero-caixas-j2)
						)
						(humano-joga novo-tabuleiro peca numero-caixas-j1 numero-caixas-jogador)
					)
					(t 
						(progn
						(imprime-tabuleiro novo-tabuleiro)
						(format t "~A" novo-tabuleiro)
						(humano-joga novo-tabuleiro (trocar-peca peca) numero-caixas-j1 numero-caixas-j2)
						)
					)
				)
    )
)


;;; ------------------------------------------------------------------------------
;;; JOGADA HUMANO VS COMPUTADOR
;;; ------------------------------------------------------------------------------

;;humano-joga 
(defun humano-computador-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2)
	(let* 	(
				(jogada (le-jogada tabuleiro))	
				(novo-tabuleiro (faz-jogada tabuleiro peca (first jogada) (second jogada) (third jogada)))		
				(numero-caixas-jogador (caixas-fechadas  novo-tabuleiro))
			)
				(cond
					((vencedor-p peca numero-caixas-jogador numero-caixas-j1 numero-caixas-j2) 	(progn
																									(format t "~&Ganhou!")
																									(jogar-de-novo)
																								)
					)
					((tabuleiro-preenchido-p novo-tabuleiro) (format t "~&Empatamos."))
					(	(and
							(= peca *jogador1*) 
							(> numero-caixas-jogador numero-caixas-j1)
						)
						(humano-computador-joga novo-tabuleiro peca numero-caixas-jogador numero-caixas-j2)
					)
					(
						(and 
							(= peca *jogador2*)
							(> numero-caixas-jogador numero-caixas-j2)
						)
						(humano-computador-joga novo-tabuleiro peca numero-caixas-j1 numero-caixas-jogador)
					)
					(t 
						(progn
						(imprime-tabuleiro novo-tabuleiro)
						(humano-computador-joga novo-tabuleiro (trocar-peca peca) numero-caixas-j1 numero-caixas-j2)
						)
					)
				)
    )
)




#|
;;computador-joga
(defun computador-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2) 
"Retorna uma jogada da máquina (um tabuleiro) após verificar que o jogo ainda não terminou."
	(let* (
				(jogada (le-jogada tabuleiro))	
				(novo-tabuleiro (faz-jogada tabuleiro peca (first jogada) (second jogada) (third jogada)))		
				(numero-caixas-jogador (caixas-fechadas  novo-tabuleiro))
			)
			(cond
				((vencedor-p peca numero-caixas-jogador numero-caixas-j1 numero-caixas-j2) (format t "~&Ganhou!"))
				((tabuleiro-preenchido-p novo-tabuleiro) (format t "~&Empatamos."))
				(	(and
						(= peca *jogador1*) 
						(> numero-caixas-jogador numero-caixas-j1)
					)
					(computador-joga novo-tabuleiro peca numero-caixas-jogador numero-caixas-j2)
				)
				(
					(and 
						(= peca *jogador2*)
						(> numero-caixas-jogador numero-caixas-j2)
					)
					(computador-joga novo-tabuleiro peca numero-caixas-j1 numero-caixas-jogador)
				)
				(t (computador-joga novo-tabuleiro (trocar-peca peca) numero-caixas-j1 numero-caixas-j2))
			)
    )
)
|#



;;; Funções Auxiliares a Jogada

;;jogar-de-novo
(defun jogar-de-novo()
	(let ((novo-jogo (progn (format t "~% Pretende jogar de novo? (s/n)")(ler-teclado))))
		(cond
			((equal novo-jogo 's)(menu-inicial))
			(t nil)
		)
	)
)
;;vencedor-p
(defun vencedor-p (novo-numero-caixas peca caixas-jogador1 caixas-jogador2) "determina se existe um vencedor. Se existir devolve o jogador que venceu. Senao devolve NIL."
  (cond
    ((and (= peca *jogador1*) (>= novo-numero-caixas 25)) *jogador1*)
    ((and (= peca *jogador2*) (>= novo-numero-caixas 25)) *jogador2*)
    (T NIL)
    )
)

;;le-jogada
(defun le-jogada (tabuleiro) "Le uma jogada fazendo a verificacao da sua legalidade. A jogada lida (arco-horizontal ou arco-vertical) e a posicao na no tabuleiro (entre 1 e 8)"
  (format t "~&A sua jogada: ")
  (let  ( 
		 (operador (le-operador))
		 (x (le-valor 'x))
		 (y (le-valor 'y)) 
		)
	(cond
		  ((or
			 (and (equal operador 'inserir-arco-horizontal) (nth (1- x) (nth (1- y) (first tabuleiro))))
			 (and (equal operador 'inserir-arco-vertical) (nth (1- x) (nth (1- y) (second tabuleiro))))
			 )
			 (format t "~&Esta casa ja esta ocupada.")
			 (le-jogada tabuleiro))
		  (t (list operador x y)))
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

;;le-valor
(defun le-valor (valor)
  (format t "~&Valor de ~A [1 <= ~A <= 8]: " valor valor)
  (let ((valor-lido (read)))
		(cond
		 ((or 
			(not(integerp valor-lido))
			(< valor-lido 1) (> valor-lido 8))
		   (format t "~&Entrada invalida.")(le-valor valor))
		   (T valor-lido)
		)
	)
)

;;; faz-jogada
(defun faz-jogada (tabuleiro peca operador x y)
  "Faz uma jogada com base numa das duas operacoes posiveis, num tabuleiro, uma peca com a qual jogar e duas coordenadas recebidos por parametro."
  ;(funcall operador tabuleiro peca (1- x) (1- y) )
  (funcall operador x y tabuleiro peca)
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
  
 #|
;; imprime-arcos-horizontais
(defun imprime-arcos-horizontais (lista)
(mapcar #'(lambda (linha) (imprime-linha linha) ) lista)
)
|#

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



(defun ler-teclado () "Ler do teclado algo do utilizador"
	(read)
)








;;; Testes
(defun tabuleiro-teste ()
  '(
	((NIL NIL NIL 2 1 NIL NIL) (NIL NIL NIL NIL 2 1 2)
	(1 2 1 2 2 2 NIL) (2 NIL NIL NIL 1 2 NIL)
	(2 2 NIL NIL 2 1 NIL) (2 2 NIL 1 1 2 NIL)
	(2 2 NIL 2 1 1 NIL) (1 1 NIL 1 2 2 NIL))
	((NIL NIL NIL 1 1 1 1) (NIL NIL NIL 1 1 1 1)
	(NIL 1 NIL NIL 1 1 2) (NIL 2 1 NIL 1 2 1)
	(1 NIL NIL 1 NIL NIL NIL) (1 NIL NIL 1 1 NIL NIL)
	(NIL NIL 1 1 NIL NIL NIL) (NIL 2 1 2 1 NIL 2))
  )
)

(defun tabuleiro-teste2 ()
  '(
	(	(1 1 2 2 2 1 2) (2 2 1 1 1 2 2)
		(2 2 1 2 1 2 2) (2 2 1 1 1 2 2)
		(2 2 1 2 1 1 2) (1 1 1 1 2 2 1))
	(
		(1 1 1 1 1 1 1) (2 1 1 1 1 1 1)
		(1 1 2 1 1 1 2) (2 2 1 1 1 2 1)
		(1 1 1 1 1 1 1) (1 2 2 1 1 1 1)
		(2 1 1 1 2 1 1) (2 2 1 2 1 2 2))
  )
)