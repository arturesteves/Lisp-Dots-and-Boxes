;Novos Sucessores

; 1º Apenas listagem
; 2º Já com a heurística	(ver lab 8)	-> ter em atenção a lista de abertos



(defun tab-teste ()
	(list '((NIL NIL) (T NIL) (NIL NIL) (NIL T) (NIL T))
			'((NIL NIL NIL NIL) (NIL T T T) (NIL NIL NIL T))
	)
)

(defun no-teste ()	; profundidade: 0, pai: nil 
	(list (tab-teste) 0 nil)
)



(defun tab-teste1 ()
	'(((nil nil t nil) (t nil t t) (nil nil t t) (nil nil t t) (nil nil t t)) ((nil nil t t) (nil nil t t) (nil nil t t) (t nil t t) (nil t t t)))
)

(defun no-teste1 ()
	(list (tab-teste1) 0 nil)
)

;;; operadores 
(defun operadores () "Cria uma lista com todos os operadores do problema dos Pontos e das Caixas"
	(list 'inserir-arco-horizontal 'inserir-arco-vertical)
)



;; Teste: (numero-linhas-tabuleiro (tab-teste))   -> Resultado: 4

;; numero-linhas-tabuleiro
(defun numero-linhas-tabuleiro (tabuleiro) "Retorna o número de linhas horizontais de um tabuleiro"
	(get-dimensao-aux (car (get-arcos-verticais tabuleiro)))
)


;; Teste: (numero-colunas-tabuleiro (tab-teste))  -> Resultado: 2
(defun numero-colunas-tabuleiro (tabuleiro) "Retorna o número de linhas verticais de um tabuleiro"
	(get-dimensao-aux (car (get-arcos-horizontais tabuleiro)))
)	


;; get-dimensao-aux
;; Teste: (get-dimensao-aux (get-arcos-horizontais (tabuleiro-teste1)))   -> Resultado: 4
(defun get-dimensao-aux (lista) "Dada uma lista de arcos devolve o número de listas existentes"
	(cond
		((null lista) 0)
		(T (+ 1 (get-dimensao-aux (cdr lista))))
	)
)

;; get-arcos-horizontais
;; Teste: (get-arcos-horizontais (tabuleiro-teste1))   ->   ((NIL T T) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)) 
(defun get-arcos-horizontais (tabuleiro) "Retorna a lista dos arcos horizontais de um tabuleiro"
	(car tabuleiro)
)

;; get-arcos-verticais
;; Teste: (get-arcos-verticais (tabuleiro-teste1))   ->   ((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL))
(defun get-arcos-verticais (tabuleiro) "Retorna a lista dos arcos verticiais de um tabuleiro"
	(car (cdr tabuleiro))
)

(defun get-no-estado (no) "Retorna o estado do nó, que é representado pelo tabuleiro"
	(car no)
)

#|| Ja feitas

||#
(defun inserir-arco-na-posicao (indice lista) "Insere um arco (representado pelo valor [T]) no índice da lista recebida"
	(cond
		((null lista) nil)
		((= indice 1) (cons T (cdr lista)))
		(T (cons (car lista) (inserir-arco-na-posicao (- indice 1) (cdr lista))))
	)
)

;; inserir-arco-na-posicao-aux
;; Teste: (inserir-arco-na-posicao-aux 3 3 (get-arcos-verticais (tabuleiro-teste1)))   ->   ((NIL NIL NIL) (T T T) (NIL NIL T) (NIL T NIL))
(defun inserir-arco-na-posicao-aux (linha coluna lista) "Insere um arco (representado pelo valor [T]) numa lista que representa o conjunto de arcos dum tabuleiro. A posição representada pela linha e a coluna de destino são valores inteiros passados como argumentos"
	(cond
		((null lista) nil)
		((= linha 1) (cons (inserir-arco-na-posicao coluna (car lista)) (cdr lista)))
		(T (cons (car lista) (inserir-arco-na-posicao-aux (- linha 1) coluna (cdr lista))))
	)
)


#||

Onde está a condição (possivel-adicionar-arco)  -> deverá estar (not (possivel-adicionar-arco)) -> pq se nao for possivel adicionar devolve nil, e nil nao passa na condição or!!!



||#
;;; Operadores

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HOUVE ALTERACOES AQUI!!!! -> SUBSTITUIR ESTAS DUAS NO FICHEIRO ACTUAL

;; arco-vertical
;; Teste: (inserir-arco-vertical 3 3 (tabuleiro-teste1))   ->   (((NIL T T) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL NIL NIL) (T T T) (NIL NIL T) (NIL T NIL)))
(defun inserir-arco-vertical (linha coluna tabuleiro) "Insere um arco vertical (representado pelo valor [T]) num tabuleiro passado como argumento"
	(let ((arcos-horizontais (get-arcos-horizontais tabuleiro))
		  (arcos-verticais (get-arcos-verticais tabuleiro)))
	
		(cond
			((or (null tabuleiro) (not (possivel-adicionar-arco linha coluna arcos-verticais))) nil)
			(T (cons arcos-horizontais (list (inserir-arco-na-posicao-aux linha coluna arcos-verticais))))
		)
	)
)

;; arco-horizontal
;; Teste: (inserir-arco-horizontal 3 3 (tabuleiro-teste1))   ->   (((NIL T T) (NIL NIL NIL) (NIL NIL T) (NIL NIL NIL)) ((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL)))
(defun inserir-arco-horizontal (linha coluna tabuleiro) "Insere um arco horizontal (representado pelo valor [T]) num tabuleiro passado como argumento"
	(let ((arcos-horizontais (get-arcos-horizontais tabuleiro))
		  (arcos-verticais (get-arcos-verticais tabuleiro)))

		(cond
			((or (null tabuleiro) (not (possivel-adicionar-arco linha coluna arcos-horizontais))) nil)
			(T (cons (inserir-arco-na-posicao-aux linha coluna arcos-horizontais) (list arcos-verticais)))
		)
	)
)	


;; possivel-adicionar-arco
;; Teste: (possivel-adicionar-arco 1 1 (get-arcos-horizontais (tabuleiro-teste1)))   -> Resultado:   T
;; Teste: (possivel-adicionar-arco 1 3 (get-arcos-horizontais (tabuleiro-teste1)))   -> Resultado:   NIL
(defun possivel-adicionar-arco (linha coluna lista) "Recebe indices de linha e coluna e uma lista de arcos horizontais ou de arcos verticais e verifica se naquela posição o valor é [T], se for devolve [NIL], se for [NIL] devolve [T]"
	(cond
		((null lista) nil)
		((= linha 1) (possivel-adicionar-arco-aux coluna (car lista)))
		(T (possivel-adicionar-arco (- linha 1) coluna (cdr lista)))
	)
)


;; possivel-adicionar-arco-aux 
;; Teste: (possivel-adicionar-arco-aux 3 '(NIL T NIL NIL))   -> Resultado: T
(defun possivel-adicionar-arco-aux (indice lista)
	(cond
		((null lista) nil)
		((and (= indice 1) (eql (car lista) nil)) T)
		(T (possivel-adicionar-arco-aux (- indice 1) (cdr lista)))
	)
)




#||
Ou seja quando o operador for inserir-arco-horizontal a lista de combinações é: (lista-combinacoes NUMERO_LINHAS NUMERO_COLUNAS)
		quando o operador for inserir-arco-vertical   a lista de combinações é: (lista-combinacoes NUMERO_COLUNAS NUMERO_LINHAS)

||#		
		
		
		
; 1º por a funcionar com o bfs e dfs
; Estes sucessores estão apenas para o bfs e dfs 

; EM PRINCÍPIO SÓ FALTA RECEBER A LISTA DE ABERTOS E DEVOLVER TODOS OS QUE NÃO ESTÃO NA LISTA DE ABERTOS	

;; sucessores 
;; Teste: (sucessores (no-teste) (operadores) 'bfs 9999)


;(sucessores (no-teste) (operadores) 'bfs 9999)
(defun sucessores (no operadores algoritmo-procura profundidade) "Dado um nó é retornada uma lista com todos os sucessores desse mesmo nó"
	(let* ((operador (car operadores))
		   (numero-linhas (numero-linhas-tabuleiro (get-no-estado no)))
		   (numero-colunas (numero-colunas-tabuleiro (get-no-estado no)))
		   (lista-linhas-colunas-possiveis (cond 
											((eql operador 'inserir-arco-horizontal) (lista-combinacoes (+ numero-linhas 1) numero-colunas))
											((eql operador 'inserir-arco-vertical)   (lista-combinacoes (+ numero-colunas 1) numero-linhas)))))
		(cond
			((null operadores) nil)
			((and (equal 'dfs algoritmo-procura) (= (get-no-profundidade no) profundidade)) nil)
			;((not (null operadores)) (cons (sucessores-todas-possibilidades no operador lista-linhas-colunas-possiveis) (sucessores no (cdr operadores) algoritmo-procura profundidade)))
			(T (append (sucessores-todas-possibilidades no operador lista-linhas-colunas-possiveis) (sucessores no (cdr operadores) algoritmo-procura profundidade)))
		)
	)
)	



; (sucessores-todas-possibilidades (no-teste) 'inserir-arco-horizontal (reverse (lista-combinacoes (+ 1(numero-linhas-tabuleiro (get-no-estado (no-teste)))) (numero-colunas-tabuleiro (get-no-estado (no-teste))))))
; (sucessores-todas-possibilidades (no-teste) 'inserir-arco-vertical (reverse (lista-combinacoes (numero-linhas-tabuleiro (get-no-estado (no-teste))) (+ 1(numero-colunas-tabuleiro (get-no-estado (no-teste)))))))
(defun sucessores-todas-possibilidades (no operador possibilidades)
	(let* ((primeira-possibilidade (car possibilidades))
		   (possibilidades-validas (not (null possibilidades)))
		   (resultado (cond (possibilidades-validas (sucessores-aux no (list operador primeira-possibilidade))) (T nil)))
		   ;(resultado (sucessores-aux no (list operador primeira-possibilidade)))
		   (resultado-avaliado (cond ((null resultado) nil) (T (list resultado)))))
		  
		(cond
			((null possibilidades) nil)
			
			(T (append resultado-avaliado (sucessores-todas-possibilidades no operador (cdr possibilidades))))
			
			;;testes 
			;(T (list operador primeira-possibilidade "-- " (sucessores-todas-possibilidades no operador (cdr possibilidades))))
			
		)
	)
)
#||
(defun sucessores-todas-possibilidades (no operador possibilidades)
	(let* ((primeira-possibilidade (car possibilidades))
		  (possibilidades-validas (not (null possibilidades)))
		  (resultado (cond (possibilidades-validas (sucessores-aux no (list operador primeira-possibilidade))) (T nil)))
		(cond
			((null possibilidades) nil)
			(T (append (cond
						((null resultado) nil)
						(T (list resultado))
						)(sucessores-todas-possibilidades no operador (cdr possibilidades))))
			
			;;testes 
			;(T (list operador primeira-possibilidade "-- " (sucessores-todas-possibilidades no operador (cdr possibilidades))))
			
		)
	)
)||#

#|| ;WORKING
; (sucessores-todas-possibilidades (no-teste) 'inserir-arco-horizontal (reverse (lista-combinacoes (+ 1(numero-linhas-tabuleiro (get-no-estado (no-teste)))) (numero-colunas-tabuleiro (get-no-estado (no-teste))))))
(defun sucessores-todas-possibilidades (no operador possibilidades)
	(let ((primeira-possibilidade (car possibilidades)))
		(cond
			((null possibilidades) nil)
			(T (append (list (sucessores-aux no (list operador primeira-possibilidade))) (sucessores-todas-possibilidades no operador (cdr possibilidades))))
			
			;;testes 
			;(T (list operador primeira-possibilidade "-- " (sucessores-todas-possibilidades no operador (cdr possibilidades))))
			
		)
	)
)
||#


;; Teste: (sucessores-aux (no-teste) '(inserir-arco-horizontal (1 1)))   -> Resultado: ((((T NIL) (T NIL) (NIL NIL) (NIL T) (NIL T)) ((NIL NIL NIL NIL) (NIL T T T) (NIL NIL NIL T))) 1 ((((NIL NIL) (T NIL) (NIL NIL) (NIL T) (NIL T)) ((NIL NIL NIL NIL) (NIL T T T) (NIL NIL NIL T))) 0 NIL))
(defun sucessores-aux (no lista-operador-parametros)
	(let* ((operador (car lista-operador-parametros))
			(tabuleiro (get-no-estado no))
			(parametros (append (cadr lista-operador-parametros) (list tabuleiro)))
			(resultado-operacao (apply operador parametros))
			(resultado (list resultado-operacao (+ 1 (get-no-profundidade no)) no)))
		
		(cond
			((null resultado-operacao) nil)
			(T resultado)
		)
	)
)



(defun lista-combinacoes (maximo-linhas maximo-colunas) "Recebe uma lista e retorna um conjunto de listas que representa todas as combinacoes possiveis"
	(cond
		((zerop maximo-linhas) nil)
		(T (append (combinacoes-numero-lista maximo-linhas (criar-lista-numeros maximo-colunas)) (lista-combinacoes (- maximo-linhas 1) maximo-colunas)))
	)
)

(defun combinacoes-numero-lista (numero lista) "Devolve uma lista com várias listas compostas pelo elemento recebido e um elemento da lista recebida"
	(let ((ultimo-elemento (car (last lista)))) 
		
		(cond
			((null lista) nil)
			(T (cons (list numero ultimo-elemento) (combinacoes-numero-lista numero (reverse (cdr (reverse lista))))))
		)
	)
)

(defun criar-lista-numeros (tamanho &optional (valor-por-omissao 1)) "Devolve uma lista com o tamanho recebido como argumento e o valor dos elementos da lista é recebido se não por omissão têm todos o valor 1"
	(cond
		((zerop tamanho) nil)
		(T (cons valor-por-omissao (criar-lista-numeros (- tamanho 1) (+ valor-por-omissao 1))))
	)
)

(defun get-no-profundidade (no) "Retorna a profundidade em que o nó se encontra"
	(cadr no)   ; Igual a: (car (cdr no))
)


#||
;; vale a pena criar estas funções ? 
(defun possibilidades-horizontais (numero-linhas numero-colunas)
	(lista-combinacoes numero-linhas numero-colunas)
)

(defun possibilidades-verticais (numero-linhas numero-colunas)
	(lista-combinacoes numero-colunas numero-linhas)
)
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;
;Apresentação

(defun escreve-no (no) "Permite escrever no ecra um no do problema."
	#||(progn
		(format t (concatenate 'string "~%|>No: " (write-to-string no)))
	)||#
	(format t "~%Nó: ~A" no)
)
#||
(defun escreve-lista-nos(lista) "Permite escrever no ecra uma lista de nos do problema das vasilhas, e.g. um conjunto de sucessores, a lista de abertos etc."
	(cond
		((null lista) nil)
		(T (progn 
				(escreve-no (car lista)) 
				(escreve-lista-nos-aux (cdr lista))
			)
		)
	)
)||#


(defun escreve-lista-nos (lista) "Permite escrever no ecra uma lista de nos do problema das vasilhas, e.g. um conjunto de sucessores, a lista de abertos etc."
	(cond
		((null lista) nil)
		(T (progn 
				(escreve-lista-nos-aux (car lista)) 	; horizontais
				(escreve-lista-nos-aux (cadr lista)) ; verticais
			)
		)
	)
)

;; Este tem que escrever os nos horizontais e os verticais
(defun escreve-lista-nos-aux (lista) "Permite escrever no ecra uma lista de nos do problema das vasilhas, e.g. um conjunto de sucessores, a lista de abertos etc."
	(cond
		((null lista) nil)
		(T (progn 
				(escreve-no (car lista)) 
				(escreve-lista-nos-aux (cdr lista))
			)
		)
	)
)		