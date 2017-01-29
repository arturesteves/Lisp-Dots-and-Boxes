;;;; jogo.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058


;;;; Constantes:
(defvar *jogador1* 1)
(defvar *jogador2* 2)




;_-------------------------------------------------------------------------------------------------------
;;;para eliminar
(defun tabuleiro-vazio-2x2 () "Retorna um tabuleiro sem caixas fechadas de dimensão 2x2"
	(list '((NIL NIL) (NIL NIL) (NIL NIL))
			'((NIL NIL) (NIL NIL) (NIL NIL))
	)
)	;; Próxima jogada é um 2, e é o pc a realizar
(defun no-teste-vazio-2x2()
	(cria-no (tabuleiro-vazio-2x2) 0 nil 0 0)
)


;;Tabuleiro Teste Fecha 3 Caixas
(defun tabuleiro-teste-fecha-3-caixa () "Retorna um tabuleiro sem caixas fechadas de dimensão 3 x 3"
    (list  '((1 NIL NIL) (2 NIL NIL) (1 NIL NIL) (1 NIL NIL))
            '((1 1 1) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))
    )
)
;;;para eliminar
;_-------------------------------------------------------------------------------------------------------









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
	  ;(humano-joga (tabuleiro-vazio-2x2) *jogador1* 0 0)
	  (humano-joga (tabuleiro-teste-fecha-3-caixa) *jogador1* 0 0)
	 
	  
	 (computador-joga (tabuleiro-teste-fecha-3-caixa) *jogador1* 0 0)))
	  ;(computador-joga (tabuleiro-inicial) *jogador2* 0 0)))
	  ;(computador-joga (tabuleiro-vazio-2x2) *jogador2* 0 0)))
	  
	  
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
					((vencedor-p tabuleiro numero-caixas-jogador peca numero-caixas-j1 numero-caixas-j2) 	(progn	
																											(format t "~&Ganhou!")
																											(jogar-de-novo)))
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
;;
;;humano-joga 
(defun humano-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2)
(format t "~%~%~%Entrei humano-joga~%")

	(let* ((jogada (le-jogada tabuleiro)) 
				(novo-tabuleiro (faz-jogada tabuleiro peca (first jogada) (second jogada) (third jogada)))		
				
				(numero-caixas-fechadas-tabuleiro-old (caixas-fechadas tabuleiro)) ;; 1 cf   1 cf pc
				
				
				(numero-caixas-fechadas-tabuleiro (caixas-fechadas  novo-tabuleiro)) ;; imaginemos que fechas caixas 2 
				
				(number-caixas-jogador1 (+ numero-caixas-j1 (- numero-caixas-fechadas-tabuleiro numero-caixas-fechadas-tabuleiro-old))) ; humano 3 ... pc 1
				)
				
				(format t "Num caixas Humano: ~a~%" number-caixas-jogador1)
				(format t "Num caixas PC: ~a~%" numero-caixas-j2)
				(format t "Avaliação: ~a~%" (and (= peca *jogador1*) (> numero-caixas-fechadas-tabuleiro numero-caixas-fechadas-tabuleiro-old)))
				;(format t "Tabule: ~a~%" number-caixas-jogador1)
				(format t "Novo Tabuleiro: ~a~%" novo-tabuleiro)
				(cond				
					((vencedor-p novo-tabuleiro numero-caixas-fechadas-tabuleiro peca number-caixas-jogador1 numero-caixas-j2) 	(progn (format t "~&Ganhou!")
																														(jogar-de-novo)))
																																										
					((tabuleiro-preenchido-p novo-tabuleiro) (progn 
																						(format t "~%&Empatamos.~%")	;~%TAB FINAL: ~a~%" novo-tabuleiro)
																						(jogar-de-novo)))
					
					((and (= peca *jogador1*) (> numero-caixas-fechadas-tabuleiro numero-caixas-fechadas-tabuleiro-old))
						(imprime-tabuleiro novo-tabuleiro)
						(humano-joga novo-tabuleiro peca number-caixas-jogador1 numero-caixas-j2))
						
					(T (progn
						;(imprime-tabuleiro novo-tabuleiro)
						(computador-joga novo-tabuleiro (trocar-peca peca) number-caixas-jogador1 numero-caixas-j2)))
				)
    )
)


(defun computador-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2) 
(format t "~%~%~%Entrei computador-joga~%")

	(let* (
			(tempo-inicial (get-universal-time)) ;; get tempo atual			
			(valor-alfa-beta (alfa-beta (cria-no tabuleiro 0 0 numero-caixas-j1 numero-caixas-j2) 3 peca 'funcao-utilidade)) ;;;;; Este valor 'valor-alfa-beta' usar para escrever no log.
			;(estatisticas (estatisticas-log *jogada-pc* peca numero-caixas-j1 numero-caixas-j2))
			
			(novo-tabuleiro (get-no-estado *jogada-pc*))
			
			(numero-caixas-fechadas-tabuleiro-old (caixas-fechadas tabuleiro))
			
			(numero-caixas-fechadas-tabuleiro (caixas-fechadas  novo-tabuleiro))
			
			(number-caixas-jogador2 (+ numero-caixas-j2 (- numero-caixas-fechadas-tabuleiro numero-caixas-fechadas-tabuleiro-old))))
			;(num-caixas-j2 (cond ((> numero-caixas-fechadas-tabuleiro numero-caixas-j2) numero-caixas-fechadas-tabuleiro) (T numero-caixas-j2))))
			
			(format t "Num caixas Humano: ~a~%" numero-caixas-j1)
			(format t "Num caixas PC: ~a~%" number-caixas-jogador2)
			;(format t "Antigo Tabuleiro: ~a~%" tabuleiro)
			;(format t "Novo Tabuleiro: ~a~%" novo-tabuleiro)
				
		(progn				
			(cond				
				((vencedor-p novo-tabuleiro numero-caixas-fechadas-tabuleiro peca numero-caixas-j1 number-caixas-jogador2) (progn	(format t "~&Ganhou!")(jogar-de-novo)))
				((tabuleiro-preenchido-p novo-tabuleiro) (progn (format t "~&Empatamos")(jogar-de-novo)))
				
				(T (progn
						(imprime-tabuleiro novo-tabuleiro)
						(humano-joga novo-tabuleiro (trocar-peca peca) numero-caixas-j1 number-caixas-jogador2)
					)
				)
			)
		)
	)
)



;;jogar-de-novo	
(defun jogar-de-novo()
	(let ((novo-jogo (progn (format t "~% Pretende jogar de novo? (s/n)")(ler-teclado))))
		(cond
			((equal novo-jogo 's)(menu-inicial))
			(t nil)
		)
	)
)

(defun vencedor-p (tabuleiro novo-numero-caixas peca caixas-jogador1 caixas-jogador2) 
;(format t "~%Entrei no vencedor-p~%")
	(let* ((num-linhas (numero-linhas-tabuleiro tabuleiro))
			(num-colunas (numero-colunas-tabuleiro tabuleiro))
			(numero-maximo-caixas (* num-linhas num-colunas))
			(num-caixas-para-vencer (cond ((evenp numero-maximo-caixas) (+ (/ numero-maximo-caixas 2) 1)) (T (/ (+ numero-maximo-caixas 1) 2))))
			(resultado (>= novo-numero-caixas num-caixas-para-vencer)))
#||
		(format t "~%numero-maximo-caixas: ~a" numero-maximo-caixas)
		(format t "~%numero-caixas-para-vencer: ~a" num-caixas-para-vencer)
		(format t "~%Resultado: ~a" resultado)
		(format t "~%Entrei no vencedor-p~%")
		(format t "~%Entrei no vencedor-p~%")
		 (format t "~%Caixas-jogador-1: ~a~%" caixas-jogador1)
		 (format t "~%Caixas-jogador-2: ~a~%" caixas-jogador2)
||#
		(cond
			(resultado (cond 
								((and (= peca *jogador1*) (> caixas-jogador1 caixas-jogador2)) *jogador1*)
								((and (= peca *jogador2*) (< caixas-jogador1 caixas-jogador2)) *jogador2*)))
			(T nil)					
		)
	)
)

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
#||
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
||#
;;;;;;;;;;
(defun alisa (lista) "Retorna uma lista com todos os átomos na lista principal"
	(cond
		((null lista) nil)
		(T (cond 
				((atom (car lista)) (cons (car lista) (alisa (cdr lista))))
				(T (append (alisa (car lista)) (alisa (cdr lista))))
			)
		)
	)
)

(defun tabuleiro-preenchido-p (lista)
	(let* ((lista-alisada (alisa lista))
			(tamanho (length lista-alisada))
			(resultado (apply '+ (mapcar #'(lambda (n)
							(cond
								((null n) 0)
								(T 1)
							)
			) lista-alisada))))
		(cond
			((= tamanho resultado) T)
			(T nil)
		)
	)
)
;;;;;;;;;


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