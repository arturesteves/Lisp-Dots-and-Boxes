;;;; Caixas

(defun numero-caixas-fechadas (tabuleiro)
	(conta-numero-caixas-fechadas (percorre-linhas tabuleiro 1))
)

#||


Existe um problema no percorre linhas ou percorre-colunas (mais provável) com vários tabuleiros



||#


;; Teste: (percorre-linhas (tabuleiro-teste2) 1)   -> Resultado: ((T T T NIL) (T T NIL NIL) (T NIL NIL NIL) (T NIL T T) (T NIL T NIL) (NIL NIL NIL T) (NIL NIL NIL NIL) (NIL NIL NIL NIL) (NIL NIL NIL NIL))
(defun percorre-linhas (tabuleiro linha)
	(let ((numero-linhas-horizontais (get-dimensao-linhas tabuleiro))) ; número de linhas horizontais
		(cond
			((= linha numero-linhas-horizontais) nil)
			(T (append (percorre-colunas linha tabuleiro) (percorre-linhas tabuleiro (+ linha 1))))
		)
	)
)

;;Teste: (percorre-colunas 1 (tabuleiro-teste2))   -> Resultado: ((T T T NIL) (T T NIL NIL) (T NIL NIL NIL))
(defun percorre-colunas (linha-h tabuleiro &optional (coluna-h 1) (linha-v 1) (coluna-v linha-h) (contador 1)) "Devolve uma lista com todas as caixas existentes numa linha"
	(let ((numero-linhas-verticais (get-dimensao-colunas tabuleiro)) ; número de linhas verticais
			
			)
			
	
	
		(cond
	
			((= contador numero-linhas-verticais) nil)		

	;;;;;;;; Correcto
			(T (cons (get-caixa linha-h coluna-h linha-v coluna-v tabuleiro) (percorre-colunas linha-h tabuleiro (+ contador (- contador 1)) (+ contador 1) coluna-v (+ contador 1))))
			
		;; Para testes -> ver os indices que foram gerados
		;;(T (list (list linha-h coluna-h linha-v coluna-v) (percorre-colunas linha-h tabuleiro (+ contador (- contador 1)) (+ contador 1) coluna-v (+ contador 1))))

		)
	)
)



;funcionar 


(defun get-caixa (linha-h coluna-h linha-v coluna-v tabuleiro)	; devolve a lista com os valores de uma caixa
	(let* ((lado-topo (get-valor-na-posicao-horizontal linha-h coluna-h (get-arcos-horizontais tabuleiro)))
			(lado-baixo (get-valor-na-posicao-horizontal (+ linha-h 1) coluna-h (get-arcos-horizontais tabuleiro)))
			(lado-esquerdo (get-valor-na-posicao-vertical linha-v coluna-v (get-arcos-verticais tabuleiro)))
			(lado-direito (get-valor-na-posicao-vertical (+ linha-v 1) coluna-v (get-arcos-verticais tabuleiro))))
			
		(list lado-topo lado-baixo lado-esquerdo lado-direito)
	)
)


;;; aux 

(defun numero-arcos-horizontais-linha (tabuleiro)
	(get-dimensao-aux (car (get-arcos-horizontais tabuleiro)))
)

(defun numero-arcos-verticais-coluna (tabuleiro)
	(get-dimensao-aux (car (get-arcos-verticais tabuleiro)))
)	


(defun get-dimensao-linhas (tabuleiro)
	(get-dimensao-aux (get-arcos-horizontais tabuleiro))
)


;; get-dimensao-colunas
;; Teste: (get-dimensao-colunas (tabuleiro-teste4))   -> Resultado: 3
(defun get-dimensao-colunas (tabuleiro)
	(get-dimensao-aux (get-arcos-verticais tabuleiro))
)


;; get-dimensao-aux
;; Teste: (get-dimensao-aux (get-arcos-horizontais (tabuleiro-teste1)))   -> Resultado: 4
(defun get-dimensao-aux (lista) "Dada uma lista de arcos devolve o número de listas existentes"
	(cond
		((null lista) 0)
		(T (+ 1 (get-dimensao-aux (cdr lista))))
	)
)


;; get-valor-na-posicao
;; Teste: (get-valor-na-posicao 2 '(5 6 4 2 3 1))   ->   6
(defun get-valor-na-posicao (indice lista) "Retorna o valor que está no índice da lista, sendo a lista e o índice passados como argumentos"
	(cond
		((null lista) nil)
		((= indice 1) (car lista))
		(T (get-valor-na-posicao (- indice 1) (cdr lista)))
	)
)


;; get-valor-na-posicao-horizontal
;; Teste: (get-valor-na-posicao-horizontal 1 3 (get-arcos-horizontais(tabuleiro-teste1)))   ->   T
(defun get-valor-na-posicao-horizontal (linha coluna lista) "Retorna o valor que está na índice linha e índice coluna recebidos de uma lista de arcos horizontais recebida"
	(cond
		((null lista) nil)
		((and (= linha 1)) (get-valor-na-posicao coluna (car lista)))
		(T (get-valor-na-posicao-horizontal (- linha 1) coluna (cdr lista)))
	)
)

;; get-valor-na-posicao-vertical
;; Teste: (get-valor-na-posicao-vertical 1 3 (get-arcos-verticais(tabuleiro-teste1)))   ->   T
(defun get-valor-na-posicao-vertical (linha coluna lista) "Retorna o valor que está na índice linha e índice coluna recebidos de uma lista de arcos verticais recebida"
	(cond
		((null lista) nil)
		((and (= linha 1)) (get-valor-na-posicao coluna (car lista)))
		(T (get-valor-na-posicao-vertical (- linha 1) coluna (cdr lista)))
	)
)




;;;A funcionar	
;; função 
;;; Recebe uma lista do tipo: ((T T NIL NIL) (T T T T) (NIL NIL NIL NIL) (T T T NIL))	-> nº de caixas fechadas: 1
;; Teste: (conta-numero-caixas-fechadas '((T T T T) (T T T T) (NIL NIL NIL NIL) (T T T NIL)))	-> 2
(defun conta-numero-caixas-fechadas (lista)
	
	(apply '+ (mapcar #'(lambda (caixa)
										(cond
											((null (todos-valores-iguais caixa)) 0)
											(T 1)
										)
									) lista
					)
	)
)
;

(defun todos-valores-iguais (lista)
	(let ((resultado (todos-valores-iguais-aux lista)))
		(cond
			((null resultado) nil)
			(T T)
		)
	)
)
;

(defun todos-valores-iguais-aux (lista)
	(cond
		((null (cdr lista)) (car lista))
		((not (eql (car lista) (todos-valores-iguais-aux (cdr lista)))) nil)
		(T (car lista))
	)
)