;;;; puzzle.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: AE - 140221076
;;;; Funções do domínio do problema



;;; Tabuleiros de teste
;; tabuleiroTeste1
(defun tabuleiroTeste1 () "Retorna um tabuleiro sem caixas fechadas de dimensão 3 x 3"
	(list '((NIL T T) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))
			'((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL))
	)
)

;; tabuleiroTeste2
(defun tabuleiroTeste2 () "Retorna um tabuleiro com 1 caixa fechada de dimensão 3 x 3"
	(list '((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))
			'((NIL NIL NIL) (NIL T NIL) (NIL T NIL) (NIL NIL NIL))
	)
)

;; tabuleiroTeste3
(defun tabuleiroTeste3 () "Retorna um tabuleiro com 1 caixa fechada de dimensão 3 x 3"
	(list '((NIL T	 NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))
			'((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))
	)
)


;;; Métodos de Consulta

;; get-arcos-horizontais
;; Teste: (get-arcos-horizontais (tabuleiroTeste1))   ->   ((NIL T T) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)) 
(defun get-arcos-horizontais (tabuleiro) "Retorna a lista dos arcos horizontais de um tabuleiro"
	(car tabuleiro)
)

;; get-arcos-verticais
;; Teste: (get-arcos-verticais (tabuleiroTeste1))   ->   ((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL))
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
;; Teste: (inserir-arco-na-posicao-aux 3 3 (get-arcos-verticais (tabuleiroTeste1)))   ->   ((NIL NIL NIL) (T T T) (NIL NIL T) (NIL T NIL))
(defun inserir-arco-na-posicao-aux (linha coluna lista) "Insere um arco (representado pelo valor [T]) numa lista que representa o conjunto de arcos dum tabuleiro. A posição representada pela linha e a coluna de destino são valores inteiros passados como argumentos"
	(cond
		((null lista) nil)
		((= linha 1) (cons (inserir-arco-na-posicao coluna (car lista)) (cdr lista)))
		(T (cons (car lista) (inserir-arco-na-posicao-aux (- linha 1) coluna (cdr lista))))
	)
)


;;; Operadores

;; arco-vertical
;; Teste: (inserir-arco-vertical 3 3 (tabuleiroTeste1))   ->   (((NIL T T) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL NIL NIL) (T T T) (NIL NIL T) (NIL T NIL)))
(defun inserir-arco-vertical (linha coluna tabuleiro) "Insere um arco vertical (representado pelo valor [T]) num tabuleiro passado como argumento"
	(cond
		((null tabuleiro) nil)
		(T (cons (get-arcos-horizontais tabuleiro) (list (inserir-arco-na-posicao-aux linha coluna (get-arcos-verticais tabuleiro)))))
	)
)

;; arco-horizontal
;; Teste: (inserir-arco-horizontal 3 3 (tabuleiroTeste1))   ->   (((NIL T T) (NIL NIL NIL) (NIL NIL T) (NIL NIL NIL)) ((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL)))
(defun inserir-arco-horizontal (linha coluna tabuleiro) "Insere um arco horizontal (representado pelo valor [T]) num tabuleiro passado como argumento"
	(cond
		((null tabuleiro) nil)
		(T (cons (inserir-arco-na-posicao-aux linha coluna (get-arcos-horizontais tabuleiro)) (list (get-arcos-verticais tabuleiro))))
	)
)	

