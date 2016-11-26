;;;; puzzle.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Funções do domínio do problema



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
		((or (null tabuleiro) (possivel-adicionar-arco linha coluna tabuleiro)) nil)
		(T (cons (get-arcos-horizontais tabuleiro) (list (inserir-arco-na-posicao-aux linha coluna (get-arcos-verticais tabuleiro)))))
	)
)

;; arco-horizontal
;; Teste: (inserir-arco-horizontal 3 3 (tabuleiro-teste1))   ->   (((NIL T T) (NIL NIL NIL) (NIL NIL T) (NIL NIL NIL)) ((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL)))
(defun inserir-arco-horizontal (linha coluna tabuleiro) "Insere um arco horizontal (representado pelo valor [T]) num tabuleiro passado como argumento"
	(cond
		((or (null tabuleiro) (possivel-adicionar-arco linha coluna tabuleiro)) nil)
		(T (cons (inserir-arco-na-posicao-aux linha coluna (get-arcos-horizontais tabuleiro)) (list (get-arcos-verticais tabuleiro))))
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

#|| CONTINUAR A AVALIAR DAQUI PARA BAIXO ||#



;; numero-caixas-fechadas
;; retorna o numero de caixas fechadas de um dado tabuleiro
;; (numero-caixas-fechadas (tabuleiro1))
;; 0
;(defun numero-caixas-fechadas (tabuleiro)
	
;)



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


;; get-dimensao-linhas
;; Teste: (get-dimensao-linhas (tabuleiro-teste4))   -> Resultado: 4
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





;;; heuristicas

;; heuristica1
;; Teste: (heuristica (tabuleiro1) 2)   -> Resultado: 1
(defun heuristica1 (tabuleiro numero-caixas-a-fechar) "Usada uma heurística que priveligia os tabuleiros com o maior número de caixas fechadas"
	(- numero-caixas-a-fechar (numero-caixas-fechadas tabuleiro)  1)
)


;;;;;;;;;;;; Necessário definir esta 2ª heurística
;;;;;;;; A definir ainda
;;; Usar nº de arcos ligados a um nó?
(defun heuristica2 (tabuleiro numero-caixas-a-fechar)
	nil
)



