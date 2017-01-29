;;;; procura.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Implementação dos algoritmos de procura

;;; Variáveis Globais
(defvar *corte-alfa* 0)
(defvar *corte-beta* 0)
(defvar *jogada-pc* nil) 
(defvar *nos-analisados* 0)
(defvar *tempo-despendido* 0)



(defun alfa-beta (no profundidade-limite peca f-utilidade &optional (alfa -999999) (beta 999999)  (tempo-inicial (get-universal-time))(tempo-maximo 5000))
	(let*(	
			(peca-a-jogar (cond ((= (get-no-profundidade no) 0) peca) (T (troca-peca peca)))) ;;Troca de peça
			(max-mix (verificar-profundidade-jogador no)) ;;MAX ou MIN
			(caixas-jogador-1	(get-caixas-jogador-1 no))
			(caixas-jogador-2 	(get-caixas-jogador-2 no))
			(tempo-actual 		(get-universal-time))
			(tempo-gasto 		(- tempo-actual tempo-inicial))
			(tempo-dispendido	(setf *tempo-despendido* tempo-gasto))
			;(nos-analisados 	(setf *nos-analisados* (+ *nos-analisados* 1)))
		)
		;(format t "~%NOOO:~a~%"no)
		(cond
			(	(or
					;(>= tempo-gasto tempo-maximo)
					;(no-folhap no) 	;; REVER
					(= profundidade-limite (get-no-profundidade no))	;; esta condicao fica no no-folhap
				)
				(progn
					(setf *nos-analisados* (+ *nos-analisados* 1))
										(get-no-utilidade no)			
				)	
			)

			(
				(eq max-mix 'MAX)
				(max-side (sucessores-alfabeta no (operadores) profundidade-limite peca f-utilidade caixas-jogador-1 caixas-jogador-2) profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo)
			)
			(T
				(min-side (sucessores-alfabeta no (operadores) profundidade-limite peca f-utilidade caixas-jogador-1 caixas-jogador-2) profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo)
			)
		)
	)
)



;;Função Alfa
(defun max-side (sucessores profundidade-limite peca f-utilidade alfa beta tempo-inicial tempo-maximo)
	(cond
		((null sucessores) alfa)
		(T (let*((nova-peca (trocar-peca peca))
					(valor-utilidade-no (alfa-beta (car sucessores) profundidade-limite nova-peca f-utilidade alfa beta tempo-inicial tempo-maximo))
					(novo-alfa (verifica-maior-sucessor alfa valor-utilidade-no (car sucessores))))
			(cond
				;(format t "~%~a~~%"*jogada-pc*)
				((<= beta novo-alfa) (setf *corte-beta* (+ *corte-beta* 1)) beta) ; houve corte alfa
				(T (max-side (cdr sucessores) profundidade-limite peca f-utilidade novo-alfa beta tempo-inicial tempo-maximo))
			)
		))
	)
)


;;Função Beta
(defun min-side (sucessores profundidade-limite peca f-utilidade alfa beta tempo-inicial tempo-maximo) "Função para descobrir o valor mais baixo entre o value e os valores dos sucessores"
;(format t "Entrei min-side~%")
;(format t "Intervalo : [~a, ~a] ~%" alfa beta)
	(cond
		((null sucessores) beta)
		(T (let*((nova-peca (trocar-peca peca))
					(valor-utilidade-no (alfa-beta (car sucessores) profundidade-limite nova-peca f-utilidade alfa beta tempo-inicial tempo-maximo))
					(novo-beta (verifica-menor-sucessor beta valor-utilidade-no)))
			(cond
				((<= novo-beta alfa) (setf *corte-alfa* (+ *corte-alfa* 1)) alfa) ; houve corte beta
				(T (min-side (cdr sucessores) profundidade-limite peca f-utilidade alfa novo-beta tempo-inicial tempo-maximo))
			)
		))
	)
)

(defun trocar-peca (peca) "Troca a peca de um jogador para a peca de outro jogador."
  (cond
	((= peca *jogador1*) *jogador2*)
	((= peca *jogador2*) *jogador1*)
	)
)

(defun verifica-maior-sucessor (alfa valor-utilidade sucessor)
	(cond
		((> valor-utilidade alfa) (setf *jogada-pc* sucessor) valor-utilidade)
		(t alfa)
	)
)

(defun verifica-menor-sucessor (beta valor-utilidade)
	(cond
		((< valor-utilidade beta) valor-utilidade)
		(t beta)
	)
)


(defun no-folhap (no)
	(let* (
			(sucessores-no (sucessores no (operadores) 1 (get-no-profundidade no) 'funcao-utilidade (get-caixas-jogador-1 no) (get-caixas-jogador-1 no)))
			(numero-sucessores (length sucessores-no))
)
		(cond
			
			((> numero-sucessores 0) nil)	;; se nao tiver sucessores ou a profundidade do no, for igual a profundidade maxima
			(T T)
		)
	)
)


(defun funcao-utilidade (no peca old-utilidade caixas-fechadas-j1 caixas-fechadas-j2 old-numero-caixas-j1 old-numero-caixas-j2)
	(let* ((numero-caixas-fechadas (caixas-fechadas (get-no-estado no)))
			 (tabuleiro (get-no-estado no))
			 (vencedor-resultado (vencedor-p tabuleiro numero-caixas-fechadas peca caixas-fechadas-j1 caixas-fechadas-j2)))

		(cond
			(vencedor-resultado (cond ((= vencedor-resultado *jogador2*) 2000) (T -2000)))	;; se o PC ganhar -> 2000 ; se for o Humano -> -2000
			(T 
				(cond
					((> caixas-fechadas-j2 old-numero-caixas-j2) (+ old-utilidade 50))	  ; PC fechou uma caixa 
					((> caixas-fechadas-j1 old-numero-caixas-j1) -50)  ; Humano fechou uma caixa 
					(T old-utilidade)
					;(T 0)	;; ninguem fechou caixa
				)
			)
		)
	)
)

(defun e-no-maxp (no)
	(let ((profundidade (get-no-profundidade no)))
		(cond
			((or (= profundidade 0) (evenp profundidade)) T)
			(T nil)
		)
	)
)

(defun no-folhap (no)
	(let* (
			;(sucessores-no (sucessores no (operadores) 1 0 'funcao-utilidade 0 0))
			(sucessores-no (sucessores no (operadores) 1 (get-no-profundidade no) 'funcao-utilidade (get-caixas-jogador-1 no) (get-caixas-jogador-1 no)))
			(numero-sucessores (length sucessores-no))
)
		(cond
			
			((> numero-sucessores 0) nil)	;; se nao tiver sucessores ou a profundidade do no, for igual a profundidade maxima
			(T T)
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

(defun verifica-se-fechou-caixa (no numero-caixas-fechadas-anterior)
	(let ((caixas-actualmente-fechadas (caixas-fechadas (get-no-estado no))))
		(> caixas-actualmente-fechadas numero-caixas-fechadas-anterior)
	)
)


(defun sucessores-alfabeta (no operadores profundidade peca funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
;(format t "Entrei sucessores-alfabeta~%")	

	(let* ((numero-caixas-fechadas (caixas-fechadas (get-no-estado no)))
		     (sucessores_resultado (sucessores no operadores peca profundidade funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2))	;;retorna os sucessores
		     (novos-sucessores (apply 'append 	;; remove os nills da lista retornada
											(mapcar #'(lambda (node)
																	(let ((fechou-caixa (verifica-se-fechou-caixa node numero-caixas-fechadas)))

																		(cond
																			((null fechou-caixa) (list node))
																			(T (let* ((caixas-fechadas-jogador-1 (cond ((= peca *jogador1*) (+ caixas-fechadas-j1 1)) (T caixas-fechadas-j1)))
																						 (caixas-fechadas-jogador-2 (cond ((= peca *jogador2*) (+ caixas-fechadas-j2 1)) (T caixas-fechadas-j2)))
																						 (new-sucessores (sucessores node operadores peca profundidade funcao-utilidade caixas-fechadas-jogador-1 caixas-fechadas-jogador-2)))
																				(cond
																					((null new-sucessores) (list node))
																					(T new-sucessores))))))
																)sucessores_resultado)))
		 )
		novos-sucessores
		;sucessores_resultado
	)
)

(defun sucessores (no operadores peca profundidade-maxima funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
;(format t "Entrei sucessores~%")

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
;(format t "Entrei sucessores-todas-possibilidades~%")

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

(defun sucessores-aux (no lista-operador-parametros peca funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
;(format t "Entrei sucessores-aux~%")

	(let* ((operador (car lista-operador-parametros))
			(tabuleiro-pai (get-no-estado no))
			(parametros (append (cadr lista-operador-parametros) (list tabuleiro-pai)))
			(tabuleiro-gerado (apply operador parametros))
			(old-utilidade (get-no-utilidade no))
			(old-numero-caixas-j1 (get-caixas-jogador-1 no))
			(old-numero-caixas-j2 (get-caixas-jogador-2 no)))

		(cond
			((null tabuleiro-gerado) nil)
			(T (let* ((numero-caixas-fechadas-tab-gerado (caixas-fechadas tabuleiro-gerado))
						 (numero-caixas-jogador-1 (cond ((= peca *jogador1*) (- numero-caixas-fechadas-tab-gerado caixas-fechadas-j2)) (T caixas-fechadas-j1)))
						 (numero-caixas-jogador-2 (cond ((= peca *jogador2*) (- numero-caixas-fechadas-tab-gerado caixas-fechadas-j1)) (T caixas-fechadas-j2)))
						 (profundidade  (+ 1 (get-no-profundidade no)))
						 (valor-utilidade (funcall funcao-utilidade no peca old-utilidade numero-caixas-jogador-1 numero-caixas-jogador-2 old-numero-caixas-j1 old-numero-caixas-j2)))
						 
				(cria-no tabuleiro-gerado profundidade valor-utilidade numero-caixas-jogador-1 numero-caixas-jogador-2))))
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