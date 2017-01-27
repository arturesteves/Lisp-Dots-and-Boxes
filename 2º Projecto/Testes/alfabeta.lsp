;;;; procura.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Implementação dos algoritmos de procura

;;; Variáveis Globais
(defvar *cortes-alfa* 0)
(defvar *cortes-beta* 0)
(defvar *jogada-pc* nil) ;;variavel que guarda o tabuleiro que corresponde a melhor jogada que é atualizada pelo algoritmo cada vez que o valor de beta é atualizado.
(defvar *nos-analisados* 0) ;; variavel que guarda o numero de nos visitados
(defvar *tempo-despendido* 0)


	 
	 (defun ordena-sucessores (lista-sucessores f-utilidade peca)
	  (sort lista-sucessores #'> :key #'(lambda (no) (let ((caixas-j1 (get-caixas-jogador-1 no))
																					 (caixas-j2 (get-caixas-jogador-2 no)))
																					 (funcall f-utilidade no peca caixas-j1 caixas-j2)))))


	(defun alfa-beta (no prof-max peca f-utilidade &optional (horaLimite (+ (get-internal-real-time) (* 20 1000))) (alpha -1000) (beta 1000))
	  ; se o tempo limite foi ultrapassado, ou o no e' terminal ou a profundidade max foi atingida
		(let ((caixas-j1 (get-caixas-jogador-1 no))
				(caixas-j2 (get-caixas-jogador-2 no)))
		(cond ((or (> (get-internal-real-time) horaLimite) (no-folhap no) (= prof-max (get-no-profundidade no)))	
			 ; return the heuristic value of node
			(funcall f-utilidade no peca caixas-j1 caixas-j2))
			((eq (get-no-profundidade no) 0)
				(jogadorMax (ordena-sucessores (sucessores-alfabeta no (operadores) prof-max peca f-utilidade caixas-j1 caixas-j2)) prof-max peca f-utilidade horaLimite alpha beta))
			; if maximizingPlayer
			((eq 'MAX (verificar-profundidade-jogador no))	;; VER esta funcao
				(jogadorMax (ordena-sucessores (sucessores-alfabeta no (operadores) prof-max peca f-utilidade caixas-j1 caixas-j2)) prof-max peca f-utilidade horaLimite alpha beta))
			; else
			(T (jogadorMin (sucessores-alfabeta no (operadores) prof-max peca f-utilidade caixas-j1 caixas-j2) prof-max peca f-utilidade horaLimite alpha beta)))))
			
			
(defun jogadorMax (lista-sucessores prof-max peca f-utilidade horaLimite alpha beta)
	  ;; definimos o melhor valor como sendo o maximo entre o alpha e o AB(sucessor)
		(let ((melhor-valor (max alpha (alfa-beta (car lista-sucessores) prof-max (troca-peca peca) f-utilidade horaLimite alpha beta))))
		(progn
			(cond ((not (= melhor-valor alpha)) (setf *jogada-pc* (get-no-estado (car lista-sucessores)))) (t nil))
			(cond   ((>= melhor-valor beta) ;; se a >= ß, entao devolvemos beta (corte!)
						(and (corte-beta) beta))
							((null (cdr lista-sucessores)) ;; se ja nao existirem sucessores devolvemos o melhor-valor ate agora
			   melhor-valor)
			  ;;caso contrario chamamos a funçao para os restantes sucessores
			  (T (jogadorMax (cdr lista-sucessores) prof-max peca f-utildiade horaLimite melhor-valor beta))))))

			  
	(defun jogadorMin (lista-sucessores prof-max peca f-utilidade horaLimite alpha beta)
	  ;; definimos o melhor valor como sendo o minimo entre o beta e o AB(sucessor)
	  (let ((melhor-valor (min beta (alfa-beta (car lista-sucessores) prof-max (troca-peca peca) f-utilidade horaLimite alpha beta))))
		(cond ((<= melhor-valor alpha) ;; se ß <= a, entao devolvemos alpha (corte!)
			   (and (corte-alfa) alpha))
			  ((null (cdr lista-sucessores)) ;; se ja nao existirem sucessores devolvemos o melhor-valor ate agora
			   melhor-valor)
			  ;;caso contrario chamamos a funçao para os restantes sucessores
			  (T (jogadorMin (cdr lista-sucessores) prof-max peca f-utilidade horaLimite alpha melhor-valor)))))
			  
(defun corte-alfa ()
	  (setf *cortes-alfa* (1+ *cortes-alfa*)))

	(defun corte-beta ()
	  (setf *cortes-beta* (1+ *cortes-beta*)))
	  
	  
	 (defun e-no-maxp (no)
		(let ((profundidade (get-no-profundidade no)))
			(cond
				((or (= profundidade 0) (evenp profundidade)) T)
				(T nil)
			)
		)
	)
	 
	 (defun troca-peca (peca)
		(cond
			((= peca 1) 2)
			(T 1)
		)
	 )
	 
	 
		(defun no-folhap (no)
		(let* ((sucessores-no (sucessores no (operadores) 1 0 'funcao-utilidade 0 0)) 
			  (numero-sucessores (length sucessores-no)))
			(cond
				((> numero-sucessores 0) nil)	;; se nao tiver sucessores ou a profundidade do no, for igual a profundidade maxima
				(T T)
			)
		)
	)	
	
(defun funcao-utilidade (no peca caixas-fechadas-j1 caixas-fechadas-j2)
"A utility function (also called an objective function or payoff function), which gives
a numeric value for the terminal states. In chess, the outcome is a win, loss, or draw,
with values +1, -1, or 0. Some games have a wider ,variety of possible outcomes; the
payoffs in backgammon range from +I92 to -192. This chapter deals mainly with
zero-sum games, although we will briefly mention non-zero-sum games. "
(let* ((numero-caixas-fechadas (caixas-fechadas (get-no-estado no)))
		(vencedor-resultado (vencedor-p numero-caixas-fechadas peca caixas-fechadas-j1 caixas-fechadas-j2)))
(progn
	(format t "~%~%~A~%" numero-caixas-fechadas)
	(format t "~%~%~A~%" vencedor-resultado)
	(cond
		(
			(and
				(= peca *jogador1*)
				(and
					(and 
						(not (null vencedor-resultado))
						(= vencedor-resultado *jogador1*)
					)
					(equal (verificar-profundidade-jogador no) 'MAX)
					;(eql (vencedor-p numero-caixas-fechadas peca caixas-fechadas-j1 caixas-fechadas-j2) *jogador1*) ;; verificar numero-caixas
					;(equal (verificar-profundidade-jogador no) 'MAX)
				)
			)
		100
		)		
		(
			(and
				(= peca *jogador2*)
				(and
					(and 
						(not (null vencedor-resultado))
						(= vencedor-resultado *jogador2*)
					)
					(equal (verificar-profundidade-jogador no) 'MAX)
				(and
				(eql (vencedor-p numero-caixas-fechadas peca caixas-fechadas-j1 caixas-fechadas-j2) *jogador2*) ;; verificar numero-caixas
				(equal (verificar-profundidade-jogador no) 'MAX)
				)
				)
			)
		(- 0 100)
		)
	(t 0)
	)
	)
	)
)













;verificar-jogador
	(defun verificar-profundidade-jogador(no) "Função que verifica se o jogador encontra-se na profundidade de MAX ou MIN"
		(let ((profundidade (get-no-profundidade no)))
			(cond
				((or (evenp profundidade) (= profundidade 0)) 'MAX);;evenp returns true if integer is even (divisible by two); otherwise, returns false.
				(t 'MIN)
			)
		)
	)



	;;; Sucessores

	;; Tipo-jogador MAX e MIN
	;; Simbolo-jogador 1 e 2

	;;sucessores 
	; (no operadores algoritmo-procura profundidade funcao-heuristica numero-objectivo-caixas)
	;; 
	;; Tenho que receber a peça pq é a peça a aplicar a inserir nas varias posicoes.

	(defun sucessores-alfabeta (no operadores profundidade peca funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
		
		(let* ((numero-caixas-fechadas (caixas-fechadas (get-no-estado no)))
				 (sucessores_resultado (sucessores no operadores peca profundidade funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2))	;;retonra bem os sucessores
				 (novos-sucessores (apply 'append 	;; remove os nills da lista retornada
											(mapcar #'(lambda (node)
												(let ((fechou-caixa (verifica-se-fechou-caixa node numero-caixas-fechadas))
														(caixas-fechadas-jogador-1 (cond ((= peca *jogador-1*) (+ caixas-fechadas-j1 1)) (T caixas-fechadas-j1)))
														(caixas-fechadas-jogador-2 (cond ((= peca *jogador-1*) (+ caixas-fechadas-j1 1)) (T caixas-fechadas-j1))))
														(cond
															((null fechou-caixa) (list node))

															;;Se chegar ao T, significa que o computador vai jogar outra vez!
															(T 											;;aqui verificar qual e a peça e incrementar conforme a peça!
															;; actual
															;;;;;;;(sucessores node operadores peca (+ profundidade 1) funcao-utilidade (+ caixas-fechadas-j1 1) caixas-fechadas-j2)))
															(sucessores node operadores peca (+ profundidade 1) funcao-utilidade caixas-fechadas-jogador-1 caixas-fechadas-jogador-2)))
												)) sucessores_resultado)) )
			 )
			novos-sucessores
		)
	)

	(defun verifica-se-fechou-caixa (no numero-caixas-fechadas-anterior)
		(let ((caixas-actualmente-fechadas (caixas-fechadas (get-no-estado no))))
			(> caixas-actualmente-fechadas numero-caixas-fechadas-anterior)
		)
	)

	;;falta testar
	(defun sucessores (no operadores peca profundidade-maxima funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
		(let* ((operador (car operadores))
			   (numero-linhas (numero-linhas-tabuleiro (get-no-estado no)))
			   (numero-colunas (numero-colunas-tabuleiro (get-no-estado no)))
			   (lista-linhas-colunas-possiveis (cond 
												((eql operador 'inserir-arco-horizontal) (reverse (lista-combinacoes (+ numero-linhas 1) numero-colunas)))
												((eql operador 'inserir-arco-vertical)   (reverse (lista-combinacoes (+ numero-colunas 1) numero-linhas))))))
			(cond
				((null operadores) nil)
				((= (get-no-profundidade no) profundidade-maxima) nil) ; não procura mais
				(T (append 
					(sucessores-todas-possibilidades no operador peca lista-linhas-colunas-possiveis funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
					(sucessores no (cdr operadores) peca profundidade-maxima funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
					))
			)
		)
	)

	;; falta testar
	(defun sucessores-todas-possibilidades (no operador peca possibilidades funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
		(let* ((primeira-possibilidade (car possibilidades))
			   (possibilidades-validas (not (null possibilidades)))
			   (resultado (cond (possibilidades-validas (sucessores-aux no (list operador (append primeira-possibilidade (list peca))) peca funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)) (T nil)))
			   (resultado-avaliado (cond ((null resultado) nil) (T (list resultado)))))
			  
			(cond
				((null possibilidades) nil)
				
				(T (append resultado-avaliado (sucessores-todas-possibilidades no operador peca (cdr possibilidades) funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)))
			)
		)
	)

	;; falta testar 
	(defun sucessores-aux (no lista-operador-parametros peca funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
		(let* ((operador (car lista-operador-parametros))
				(tabuleiro (get-no-estado no))
				(parametros (append (cadr lista-operador-parametros) (list tabuleiro)))
				(resultado-operacao (apply operador parametros))
				(resultado (cria-no resultado-operacao (+ 1 (get-no-profundidade no)) (funcall funcao-utilidade no peca caixas-fechadas-j1 caixas-fechadas-j2) caixas-fechadas-j1 caixas-fechadas-j2 )))
			(cond
				((null resultado-operacao) nil)
				(T resultado)
			)
		)
	)

	;;lista-combinacoes
	(defun lista-combinacoes (maximo-linhas maximo-colunas) "Recebe uma lista e retorna um conjunto de listas que representa todas as combinacoes possiveis"
		(cond
			((zerop maximo-linhas) nil)
			(T (append (combinacoes-numero-lista maximo-linhas (criar-lista-numeros maximo-colunas)) (lista-combinacoes (- maximo-linhas 1) maximo-colunas)))
		)
	)

	;; combinacoes-numero-lista
	(defun combinacoes-numero-lista (numero lista) "Devolve uma lista com várias listas compostas pelo elemento recebido e um elemento da lista recebida"
		(let ((ultimo-elemento (car (last lista)))) 
			
			(cond
				((null lista) nil)
				(T (cons (list numero ultimo-elemento) (combinacoes-numero-lista numero (reverse (cdr (reverse lista))))))
			)
		)
	)

	;; criar-lista-numeros
	(defun criar-lista-numeros (tamanho &optional (valor-por-omissao 1)) "Devolve uma lista com o tamanho recebido como argumento e o valor dos elementos da lista é recebido se não por omissão têm todos o valor 1"
		(cond
			((zerop tamanho) nil)
			(T (cons valor-por-omissao (criar-lista-numeros (- tamanho 1) (+ valor-por-omissao 1))))
		)
	)

	 
	 
	 
	 
	;;; Funções de Verificações

	(defun existep (no lista-nos) "Retorna verdadeiro se o nó existir na lista"
		(cond
			((null lista-nos) nil)
			( (equal (get-no-estado no) (get-no-estado (car lista-nos))) T)
			;((equal no (car lista-nos)) T)
			(t
				(existep no (cdr lista-nos))
			)
		)
	)

	;; existe-solucao
	(defun existe-solucao (lista f-solucao numero-objectivo-caixas) "Verifica se existe uma solucao ao problema numa lista de sucessores"
		(cond
			((null lista) nil)
			((funcall f-solucao (car lista) numero-objectivo-caixas) (car lista))
			(T (existe-solucao (cdr lista) f-solucao numero-objectivo-caixas))
		)
	)

