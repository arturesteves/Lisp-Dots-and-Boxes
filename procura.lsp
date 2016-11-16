;;;; procura.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: AE - 140221076
;;;; Implementação dos algoritmos de procura



;;; heuristica

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
;; Teste: (get-valor-na-posicao-horizontal 1 3 (get-arcos-horizontais(tabuleiroTeste1)))   ->   T
(defun get-valor-na-posicao-horizontal (linha coluna lista) "Retorna o valor que está na índice linha e índice coluna recebidos de uma lista de arcos horizontais recebida"
	(cond
		((null lista) nil)
		((and (= linha 1)) (get-valor-na-posicao coluna (car lista)))
		(T (get-valor-na-posicao-horizontal (- linha 1) coluna (cdr lista)))
	)
)

;; get-valor-na-posicao-vertical
;; Teste: (get-valor-na-posicao-vertical 1 3 (get-arcos-verticais(tabuleiroTeste1)))   ->   T
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

;; heuristica1
;; retorna a avaliação do tabuleiro, de acordo com a heurística definida no enunciado do projeto. A função recebe um tabuleiro e um valor inteiro que representa o objetivo: o número de caixas a fechar
;; (heuristica (tabuleiro1) 2)
;; 1

;;; Exercicios complementares
;; tabuleiro-vazio
;; "constroi um tabuleiro vazio de dimensao n x m a partir de dois valores inteiros recebidos por parametro"
;; (tabuleiro-vazio 3 3)
;; (((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)))


 