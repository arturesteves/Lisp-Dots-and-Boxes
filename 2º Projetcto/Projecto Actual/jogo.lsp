;;;; jogo.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058


;;;; Constantes:
(defvar *jogador1* 1)
(defvar *jogador2* 2)

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
				(format t "~%>|            2. Sair                                  |")
				(format t "~%>|                                                     |")
				(format t "~%> ------------------------------------------------------")
				(format t "~%> Opcao")
				(format t "~%> ")
				
				(let ((opcao (ler-teclado)))
					(cond
						;((not (numberp opcao)) (menu-inicial caminho))		
						((not (numberp opcao)) (menu-inicial))		
						((and (<= opcao 2) (>= opcao 1)) (cond
															((= opcao 1) (menu-selecionar-jogo))	
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


;;; Função de Impresso

(defun converte-arco-horizontal (v)
  "Converte os inteiros dos arcos horizontais para os simbolos --- (jogador com peca 1) e ... (jogador com peca 2)"
  (cond ((equal v 1) "___")
        ((equal v 2) "...")
        (t "   ")))

(defun converte-arco-vertical (v)
  "Converte os inteiros dos arcos verticais para os simbolos | (jogador com peca 1) e  . (jogador com peca 2) "
  (cond ((equal v 1) "|  ")
        ((equal v 2) ".  ")
        (t "   ")))

(defun rodar (matriz)
  (apply #'mapcar #'list matriz))
  
 
(defun imprime-linha (lista)
  "Imprime uma linha formatada do tabuleiro"
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

(defun imprime-arcos-horizontais (lista)
(mapcar #'(lambda (linha) (imprime-linha linha) ) lista)
)

(defun imprime-coluna (lista)
  "Imprime uma linha formatada do tabuleiro"
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
;; (imprime-tabuleiro (tabuleiro-teste))
(defun imprime-tabuleiro (tabuleiro)
  "Imprime o tabuleiro, linha a linha"
  (let ((linhas (first tabuleiro)) (colunas (rodar (second tabuleiro))))
    (mapcar #'(lambda (linha coluna) (progn (imprime-linha linha) (imprime-coluna coluna) (imprime-coluna coluna))) linhas colunas)
  )
)






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

(defun vencedor-p (novo-numero-caixas peca caixas-jogador1 caixas-jogador2)
  "determina se existe um vencedor. Se existir devolve o jogador que venceu. Senao devolve NIL."
  (cond
    ((and (= peca *jogador1*) (>= novo-numero-caixas 25)) *jogador1*)
    ((and (= peca *jogador2*) (>= novo-numero-caixas 25)) *jogador2*)
    (T NIL)
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
	
	
	
