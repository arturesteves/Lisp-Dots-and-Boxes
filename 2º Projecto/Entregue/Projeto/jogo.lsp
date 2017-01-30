;;;; jogo.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Este ficheiro contÍm a interaÁ„o com o utilizador. Jogadas
;;;; Constantes:
(defvar *jogador1* 1)
(defvar *jogador2* 2)
(defvar *start-time* 0)

;;; InicializaÁ„o do Jogo

;;iniciar
(defun iniciar ()"FunÁ„o que inicializa o programa, chamando a funÁ„o que apresenta o menu inicial."
	(progn
		(let ((caminho (inserir-diretoria)))
			  (load-files caminho)
		 )
	)
)
;

;;inserir-diretoria
(defun inserir-diretoria() "FunÁ„o que pede ao utilizador para colocar a raiz da pasta onde se encontram os ficheiros do projeto."
	(progn
		(format t "~%Introduza o caminho do ficheiro ~%")
		(format nil (read-line))
	)
)
;

;;load-files ()
(defun load-files (caminho) "FunÁ„o que carrega os ficheiros de puzzle e procura para o programa e executa o menu"
				(progn 
					(compile-file (concatenate 'string caminho "\\alfabeta.lsp"))
					(compile-file (concatenate 'string caminho "\\pontosecaixas.lsp"))
					(load (concatenate 'string caminho "\\alfabeta.ofasl")) 
					(load (concatenate 'string caminho "\\pontosecaixas.ofasl"))
					(menu-inicial caminho)
			   )
)
;

;; menu-inicial
(defun menu-inicial(caminho) "Apresenta o menu principal do programa na consola. Sendo possÌvel iniciar uma procura ou sair do programa"
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
														((= opcao 1) (iniciar-jogada caminho))	
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
;

;;iniciar-jogada
(defun iniciar-jogada(caminho)
		(loop	
			(progn
				(format t "~%> ------------------------------------------------------")
				(format t "~%>|         Puzzle dos Pontos e das Caixas              |")
				(format t "~%>|                                                     |")
				(format t "~%>|            1. Humano vs Humano (Proposta)           |")
				(format t "~%>|            2. Humano vs PC 			    |") 
				(format t "~%>|            3. Voltar atr·s	                    |")		
				(format t "~%>|                                                     |")
				(format t "~%> ------------------------------------------------------")
				(format t "~%> Opcao")
				(format t "~%> ")	
				(let ((opcao (ler-teclado)))
					(cond
						((not (numberp opcao)) (menu-selecionar-jogo))		
						((and (<= opcao 3) (>= opcao 1)) (cond
															((= opcao 1) (fazer-uma-partida-humano-humano))
															((= opcao 2) (fazer-uma-partida-humano-pc caminho))
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
;

;;Utilizado para Humano vs Humano	
(defun fazer-uma-partida-humano-humano () "Caso pretende ser o primeiro a jogar. Passa o jogador1, caso contrario jogador2"
  (if (y-or-n-p "Pretende iniciar a  partida como Jogador 1? (y/n)")
      (humano-humano-joga (tabuleiro-inicial) *jogador1* 0 0)
      (humano-humano-joga (tabuleiro-inicial) *jogador2* 0 0)))
;	  

;;Utilizado para Humano vs Computador	  
(defun fazer-uma-partida-humano-pc (caminho) "Caso pretende ser o primeiro a jogar. Passa o jogador1, caso contrario jogador2"
  (setf *start-time* 0)
  (if (y-or-n-p "Pretende iniciar a partida como Jogador 1? (y/n)")
	   (humano-joga (tabuleiro-inicial) *jogador1* 0 0 caminho)
	   (computador-joga (tabuleiro-inicial) *jogador2* 0 0 caminho))	    
)
;
	  
;;; JOGADA HUMANO VS HUMANO			- APRESENTADA COMO PROPOSTA
;;humano-humano-joga (Player1 vs Player2)
(defun humano-humano-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2)"FunÁ„o que realiza a jogada do humano e do outro humano"
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
;

;;; JOGADA HUMANO VS COMPUTADOR
;;humano-joga 
(defun humano-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2 caminho)"Fun√ß√£o que realiza a jogada do humano"
	(let* (		(jogada (le-jogada tabuleiro)) 
				(novo-tabuleiro (faz-jogada tabuleiro peca (first jogada) (second jogada) (third jogada)))						
				(numero-caixas-fechadas-tabuleiro-old (caixas-fechadas tabuleiro))					
				(numero-caixas-fechadas-tabuleiro (caixas-fechadas  novo-tabuleiro))			
				(number-caixas-jogador1 (+ numero-caixas-j1 (- numero-caixas-fechadas-tabuleiro numero-caixas-fechadas-tabuleiro-old)))
				)
				(format t "~%Caixas Jogador1: ~A~%" numero-caixas-j1) ;;Apresenta o numero de caixas do Jogador 1
				(format t "~%Caixas Jogador2: ~A~%" numero-caixas-j2) ;;Apresenta o numero de caixas do Jogador 2
				(cond				
					((vencedor-p novo-tabuleiro numero-caixas-fechadas-tabuleiro peca number-caixas-jogador1 numero-caixas-j2) (format t "~&Ganhou!"))																																					
					((tabuleiro-preenchido-p novo-tabuleiro)(format t "~%&Empatamos.~%"))					
					(
						(and 
							(= peca *jogador1*) (> numero-caixas-fechadas-tabuleiro numero-caixas-fechadas-tabuleiro-old)
						)
							(imprime-tabuleiro novo-tabuleiro)
							(humano-joga novo-tabuleiro peca number-caixas-jogador1 numero-caixas-j2 caminho)
					)						
					(T (progn(computador-joga novo-tabuleiro (trocar-peca peca) number-caixas-jogador1 numero-caixas-j2 caminho)))
				)
    )
)
;

;;computador-joga
(defun computador-joga (tabuleiro peca numero-caixas-j1 numero-caixas-j2 caminho &optional (tempo-jogada (get-universal-time))) "Fun√ß√£o que realiza a jogada da m√°quina"
	(let* (		
			(valor-alfa-beta (alfa-beta (cria-no tabuleiro 0 0 numero-caixas-j1 numero-caixas-j2) 3 peca 'funcao-utilidade))
			(novo-tabuleiro (get-no-estado *jogada-pc*))
			(numero-caixas-fechadas-tabuleiro-old (caixas-fechadas tabuleiro))
			(numero-caixas-fechadas-tabuleiro (caixas-fechadas  novo-tabuleiro))
			(number-caixas-jogador2 (+ numero-caixas-j2 (- numero-caixas-fechadas-tabuleiro numero-caixas-fechadas-tabuleiro-old)))			
			(vencedor-imprimir(vencedor-p novo-tabuleiro numero-caixas-fechadas-tabuleiro peca numero-caixas-j1 number-caixas-jogador2))
			(estatisticas (estatisticas-log novo-tabuleiro valor-alfa-beta vencedor-imprimir caminho))
		)
		(progn				
			(cond				
				((vencedor-p novo-tabuleiro numero-caixas-fechadas-tabuleiro peca numero-caixas-j1 number-caixas-jogador2) (progn
																															(format t "~&Ganhou!")
																															(format t "~&Tempo demorado:~a"*start-time*)
																															estatisticas
																															)
																															)
				((tabuleiro-preenchido-p novo-tabuleiro)(progn (format t "~&Empatamos") (format t "~&Tempo demorado:~a"*start-time*) estatisticas))
				(
					(and 
						(= peca *jogador2*) (> numero-caixas-fechadas-tabuleiro numero-caixas-fechadas-tabuleiro-old)
					)
						(computador-joga novo-tabuleiro peca numero-caixas-j1 number-caixas-jogador2 caminho tempo-jogada)
				)						
				(T (progn
						(setf *start-time* (+ *start-time* (- (get-universal-time) tempo-jogada)))
						(format t "~%Tempo ~a:" *start-time*) (format t "segundos~%")
						(imprime-tabuleiro novo-tabuleiro)
						(humano-joga novo-tabuleiro (trocar-peca peca) numero-caixas-j1 number-caixas-jogador2 caminho)
					)
				)
			)
			
		)
	)
)
;

;;vencedor-p
(defun vencedor-p (tabuleiro novo-numero-caixas peca caixas-jogador1 caixas-jogador2)"Fun√ß√£o que faz a valida√ß√£o do vencedor"
	(let* (
			(num-linhas (numero-linhas-tabuleiro tabuleiro))
			(num-colunas (numero-colunas-tabuleiro tabuleiro))
			(numero-maximo-caixas (* num-linhas num-colunas))
			(num-caixas-para-vencer (cond ((evenp numero-maximo-caixas) (+ (/ numero-maximo-caixas 2) 1)) (T (/ (+ numero-maximo-caixas 1) 2))))
			(resultado (>= novo-numero-caixas num-caixas-para-vencer))
		)
		(cond
			(resultado (cond 
								((and (= peca *jogador1*) (> caixas-jogador1 caixas-jogador2)) *jogador1*)
								((and (= peca *jogador2*) (< caixas-jogador1 caixas-jogador2)) *jogador2*)))
			(T nil)					
		)
	)
)
;

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
;

;;le-operador
(defun le-operador () "L√© o operador que o utilizador pretende executar"
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
;

;;le-valor-x
(defun le-valor-x (valor) "Le a coordenada x que o utilizador pretende inserir o arco"
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
;

;;le-valor-y
(defun le-valor-y (valor) "Le a coordenada x que o utilizador pretende inserir o arco"
  (format t "~&Valor de ~A [1 <= ~A <= 7]: " valor valor)
  (let ((valor-lido (read)))
		(cond
			(
				(or 
					(not(integerp valor-lido))
					(< valor-lido 1) (> valor-lido 7)	;; y so pode ir at√© 7,>7 seria fora do tabuleiro
				)			
				(format t "~&Entrada invalida.")(le-valor-y valor)
			)
		   (T valor-lido)
		)
	)
)
;

;;; faz-jogada
(defun faz-jogada (tabuleiro peca operador x y)"Faz uma jogada com base numa das duas operacoes posiveis, num tabuleiro, uma peca com a qual jogar e duas coordenadas recebidos por parametro."
	(funcall operador x y peca tabuleiro)
)
;

;;; IMPRIME TABULEIRO
;; NOTA: Ele continua a imprimir uma lista de nils devido a itera√ß√£o do mapcar.
;;imprime-tabuleiro
(defun imprime-tabuleiro (tabuleiro) "Imprime o tabuleiro, linha a linha"
	(let ((linhas (first tabuleiro)) (colunas (append (rodar (second tabuleiro)) '(NIL))))
		(mapcar #'(lambda (linha coluna) (progn (imprime-linha linha) 
												(imprime-coluna coluna)
												(imprime-coluna coluna))) linhas colunas)
	)
)
;

;; converte-arco-horizontal
(defun converte-arco-horizontal (v)
  "Converte os inteiros dos arcos horizontais para os simbolos --- (jogador com peca 1) e ... (jogador com peca 2)"
  (cond ((equal v 1) "___")
		((equal v 2) "...")
		(t "   ")))
;

;; converte-arco-vertical
(defun converte-arco-vertical (v)
  "Converte os inteiros dos arcos verticais para os simbolos | (jogador com peca 1) e  . (jogador com peca 2) "
  (cond ((equal v 1) "|  ")
		((equal v 2) ".  ")
		(t "   ")))
;

;;rodar
(defun rodar (matriz) "Transposta de matriz"
  (apply #'mapcar #'list matriz))
;

;;imprime-linha
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
;

;;imprime-coluna
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
;

;;; Auxiliares do Jogo
;;tabuleiro-inicial
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
;		

;;alisa
(defun alisa (lista) "Retorna uma lista com todos os √°tomos na lista principal"
	(cond
		((null lista) nil)
		(T (cond 
				((atom (car lista)) (cons (car lista) (alisa (cdr lista))))
				(T (append (alisa (car lista)) (alisa (cdr lista))))
			)
		)
	)
)
;

;;tabuleiro-preenchido-p
(defun tabuleiro-preenchido-p (lista) "Verifica se o tabuleiro est√° preenchido"
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
;

;;ler-teclado
(defun ler-teclado () "Ler do teclado algo do utilizador"
	(read)
)


;;; Estatisticas

;;estatisticas-log
(defun estatisticas-log (tabuleiro alfabeta peca-vencedora caminho) "FunÁ„o que escreve as estatisticas num ficheiro e imprime na consola."	
		(with-open-file (file (concatenate 'string caminho "\\LOG.dat")
							:direction :output
							:if-exists :append 
							:if-does-not-exist :create)
			;; Esta parte ser√° escrita no ficheiro do tipo .DAT
			(format file "~%Vencedor: ~s ~%" peca-vencedora)
			(format file "~%Tabuleiro: ~s ~%" tabuleiro)
			(format file "~%Caixas Fechadas: ~s ~%" (caixas-fechadas tabuleiro))
			(format file "~%Cortes Alfa: ~s ~%" *corte-alfa*)
			(format file "~%Cortes Beta: ~s ~%" *corte-beta*)
			(format file "~%Nos analisados: ~s ~%"*nos-analisados* )
			(format file "~%Tempo Maximo ~s ~%" *tempo-despendido*)
			(format file "___________________________________________________~%~%~%")
		)	
		;;Esta parte ser√° mostrada na consola
			(format t "~%Vencedor: ~s ~%" peca-vencedora)
			(format t "~%Tabuleiro: ~s ~%" tabuleiro)
			(format t "~%Caixas Fechadas: ~s ~%" (caixas-fechadas tabuleiro))		
)
;	