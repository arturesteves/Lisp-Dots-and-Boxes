;;;; puzzle.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Funções do domínio do problema


 

#||

Adaptar ao problema em mãos


A estrutura do meu nó será ((tabuleiro) profundidade (pai) (optional heuristica)) ? -> E aplicando os algoritmos de bfs e dfs a heuristica seria 'nil'. 

||#


;;; Métodos de Consulta

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


;;; Funcões auxiliares dos operadores

;; inserir-arco-na-posicao
;; Teste: (inserir-arco-na-posicao 2 '(T nil T T))   ->   (T T T T)
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


;;; Operadores

;; arco-vertical
;; Teste: (inserir-arco-vertical 3 3 (tabuleiro-teste1))   ->   (((NIL T T) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL NIL NIL) (T T T) (NIL NIL T) (NIL T NIL)))
(defun inserir-arco-vertical (linha coluna tabuleiro) "Insere um arco vertical (representado pelo valor [T]) num tabuleiro passado como argumento"
	(cond
		((null tabuleiro) nil)
		(T (cons (get-arcos-horizontais tabuleiro) (list (inserir-arco-na-posicao-aux linha coluna (get-arcos-verticais tabuleiro)))))
	)
)

;; arco-horizontal
;; Teste: (inserir-arco-horizontal 3 3 (tabuleiro-teste1))   ->   (((NIL T T) (NIL NIL NIL) (NIL NIL T) (NIL NIL NIL)) ((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL)))
(defun inserir-arco-horizontal (linha coluna tabuleiro) "Insere um arco horizontal (representado pelo valor [T]) num tabuleiro passado como argumento"
	(cond
		((null tabuleiro) nil)
		(T (cons (inserir-arco-na-posicao-aux linha coluna (get-arcos-horizontais tabuleiro)) (list (get-arcos-verticais tabuleiro))))
	)
)	



#|| CONTINUAR A AVALIAR DAQUI PARA BAIXO ||#



;; numero-caixas-fechadas
;; retorna o numero de caixas fechadas de um dado tabuleiro
;; (numero-caixas-fechadas (tabuleiro1))
;; 0
;(defun numero-caixas-fechadas (tabuleiro)
	
;)

;;,Acabar

;; get-valor-na-posicao
;; Teste: (get-valor-na-posicao 2 '(5 6 4 2 3 1))   ->   6
(defun get-valor-na-posicao (indice lista) "Retorna o valor que está no índice da lista, sendo a lista e o índice passados como argumentos"
	(cond
		((null lista) nil)
		((= indice 1) (car lista))
		(T (get-valor-na-posicao (- indice 1) (cdr lista)))
	)
)

;;;;;TODO, fazer uma função genérica para esta 2 funções a baixo
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


(defun numero-caixas-fechadas (tabuleiro)
	(cond
		((and (get-valor-na-posicao-horizontal 1 2 (get-arcos-horizontais tabuleiro)) (get-valor-na-posicao-horizontal 2 2 (get-arcos-horizontais tabuleiro))) 1)
		(T 0)
	)
)

(defun numero-caixas-fechadas-test (linha coluna tabuleiro)
	(cond
		((and (and (get-valor-na-posicao-horizontal linha coluna (get-arcos-horizontais tabuleiro)) (get-valor-na-posicao-horizontal (+ linha 1) coluna (get-arcos-horizontais tabuleiro)))
				  (and (get-valor-na-posicao-vertical linha coluna    (get-arcos-verticais tabuleiro))     (get-valor-na-posicao-vertical (+ linha 1) coluna (get-arcos-verticais tabuleiro)))) (+ 1 ))
		(T 0)
	)
)


#||
(defun get-arco-aux (linha coluna lista)
	(cond
		((null lista) nil)
		((= linha 1) (car lista))
		(T (arco-horizontal-aux (- linha 1) coluna (cdr lista)))
	)
)
||#




;;; Exercicios complementares	
;; tabuleiro-vazio			-> LAB 6
;; "constroi um tabuleiro vazio de dimensao n x m a partir de dois valores inteiros recebidos por parametro"
;; (tabuleiro-vazio 3 3)
;; (((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)))


