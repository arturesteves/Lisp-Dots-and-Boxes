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
				(format t "~%>|            1. Humano vs PC	                        |")
				;(format t "~%>|            1. PC vs PC (Campeonato)                 |")  
				(format t "~%>|            3. Voltar atrás	                        |")		
				(format t "~%>|                                                     |")
				(format t "~%> ------------------------------------------------------")
				(format t "~%> Opcao")
				(format t "~%> ")	
				(let ((opcao (ler-teclado)))
					(cond
						((not (numberp opcao)) (menu-selecionar-jogo))		
						((and (<= opcao 3) (>= opcao 1)) (cond
															((= opcao 1) (fazer-uma-partida))
															;((= opcao 2) "Campeonato")
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
(defun fazer-uma-partida () "Caso pretende ser o primeiro a jogar. Passa o jogador1, caso contrario jogador2"
  (if (y-or-n-p "Quer iniciar a partida? (y/n)")
      (humano-joga (tabuleiro-inicial) *jogador1* 0 0)
      (humano-joga (tabuleiro-inicial) *jogador2* 0 0)))


;;; ------------------------------------------------------------------------------
;;; JOGADAS
;;; ------------------------------------------------------------------------------

;;humano-joga c/ minimax
(defun humano-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2)
	(let* (
			(jogada (le-jogada tabuleiro))
			(novo-tabuleiro (faz-jogada (first jogada) peca (second jogada) (third jogada)))
			(numero-caixas-jogador (caixas-fechadas  novo-tabuleiro))); usar a funcao get-numero-caixas desenvolvida no 1ยบ projeto
			(cond
				((vencedor-p peca numero-caixas-jogador numero-caixas-j1 numero-caixas-j2)
				(format t "~&Ganhou!"))
				((tabuleiro-preenchido-p novo-tabuleiro)
				(format t "~&Empatamos."))
				((and (= peca *jogador1*) (> numero-caixas-jogador numero-caixas-j1))
				(humano-joga novo-tabuleiro peca numero-caixas-jogador numero-caixas-j2))
				((and (= peca *jogador2*) (> numero-caixas-jogador numero-caixas-j2))
				(humano-joga novo-tabuleiro peca numero-caixas-j1 numero-caixas-jogador))
				(t (humano-joga novo-tabuleiro (trocar-peca peca) numero-caixas-j1 numero-caixas-j2))
			)
    )
)


;;computador-joga
;(defun computador-joga(tabuleiro peca numero-caixas-j1 numero-caixas-j2)


;;; Funções Auxiliares a Jogada

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
			 (and (equal operador 'arco-horizontal) (nth (1- x) (nth (1- y) (first tabuleiro))))
			 (and (equal operador 'arco-vertical) (nth (1- x) (nth (1- y) (second tabuleiro))))
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
				   ((= 1 operador-lido) 'arco-horizontal)
				   (T 'arco-vertical)))
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
(defun faz-jogada (jogador x y tabuleiro) "Cria o novo tabuleiro que consiste numa jogada do jogador selecionado"
  (cond ((null tabuleiro) nil)
		((= pos 0) (cons jogador (rest tabuleiro)))
		(t (cons (first tabuleiro) (faz-jogada jogador (1- pos) (rest tabuleiro))))))
	

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
  
  
;; imprime-arcos-horizontais
(defun imprime-arcos-horizontais (lista)
(mapcar #'(lambda (linha) (imprime-linha linha) ) lista)
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


;;humano-joga
(defun humano-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2)
  (let* (	(jogada (le-jogada tabuleiro))
			(novo-tabuleiro (faz-jogada (first jogada) peca (second jogada) (third jogada)))
			(numero-caixas-jogador (caixas-fechadas novo-tabuleiro)))
		(cond
			((vencedor-p peca numero-caixas-jogador numero-caixas-j1 numero-caixas-j2)(format t "~&Ganhou!"))
			((tabuleiro-preenchido-p novo-tabuleiro)(format t "~&Empatamos."))
			((and (= peca *jogador1*) (> numero-caixas-jogador numero-caixas-j1))
			(humano-joga novo-tabuleiro peca numero-caixas-jogador numero-caixas-j2))
			((and (= peca *jogador2*) (> numero-caixas-jogador numero-caixas-j2))
			(humano-joga novo-tabuleiro peca numero-caixas-j1 numero-caixas-jogador))
			(t (humano-joga novo-tabuleiro (trocar-peca peca) numero-caixas-j1 numero-caixas-j2))
		)
	)
)

;;computador-joga
;(defun computador-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2)
;)


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
			VAI BUSCAR AO NOSSO FICHEIRO ALFA-BETA
			
(defun vencedor-p (novo-numero-caixas peca caixas-jogador1 caixas-jogador2)
"Determina se existe um vencedor. Se existir devolve o jogador que venceu. Senao devolve NIL."
  (cond
	((and (= peca *jogador1*) (>= novo-numero-caixas 25)) *jogador1*)
	((and (= peca *jogador2*) (>= novo-numero-caixas 25)) *jogador2*)
	(T NIL)
	)
)
|#

(defun trocar-peca (peca) "Troca a peca de um jogador para a peca de outro jogador."
  (cond
	((= peca *jogador1*) *jogador2*)
	((= peca *jogador2*) *jogador1*)
	)
)



;Teste: (defun tabuleiro-preenchido-p  (tabuleiro)
;Resultado: 0
(defun tabuleiro-preenchido-p (tabuleiro)"Verifica se o tabuleiro ja esta completamente preenchido"
	(cond
		((null tabuleiro) 0)
		( (= (contar-nils-sublistas tabuleiro) (+ (length(car tabuleiro)) (length(cadr tabuleiro))) ) T)
		(t nil)
	)
)
#|
	(defun tabuleiro-preenchido-p (tabuleiro)
		(member 0 tabuleiro))
|#

;;Teste: (contar-nils-sublistas (tabuleiro-teste))
;; Resultado: 0
(defun contar-nils-sublistas(lista)
	(cond
		((null lista)0)
		((= (contar-nils-lista (caar lista)) 0) (+ 1 (contar-nils-sublistas (cdr lista))))
		(t (contar-nils-sublistas(cdr lista)))
	)
)

;;Teste: (contar-nils-lista '(NIL NIL NIL 2 1 NIL NIL))
;; Resultado: 5
;; contar-nils-lista
(defun contar-nils-lista (lista) "Função que irá contar o numero de NILS's existentes numa lista.Se não existir dará valor 0, ou seja, teriamos uma lista so com T,o que resulta uma caixa fechada."
	(cond 
		((null lista) 0) 
		((equal 'NIL (car lista)) (+ 1 (contar-nils-lista (cdr lista)))) 
		(T (contar-nils-lista (cdr lista)))
	)
)







;;; Funções Auxiliares

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
	
