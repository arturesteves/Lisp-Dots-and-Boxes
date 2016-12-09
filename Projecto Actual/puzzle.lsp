;;;; puzzle.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Funções do domínio do problema



;;; Construtor

;; cria-no
(defun cria-no (estado &optional (profundidade 0) (heuristica 0) (no-pai nil)) 
"Cria uma lista que representa um nó; Um nó é composto pelo estado que é o tabuleiro, este é um parâmetro obrigatório, é composto também por outros parâmetros, como
a profundidade a que se encontra, pela heurística deste mesmo nó e pelo nó pai, ou seja, o nó que o gerou. A profundidade e a heuróistica por defeito têm valor 0, enquanto que o nó pai por defeito é NIL"

	(list estado profundidade heuristica no-pai)
)


;;; Metodos seletores

;; get-no-estado
;; Teste: (get-no-estado (no-teste))   -> Resultado: (((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)))
(defun get-no-estado (no) "Retorna o estado do nó, que é representado pelo tabuleiro"
	(car no)
)

;; get-no-profundidade
;; Teste: (get-no-profundidade (no-teste))   -> Resultado: 0
(defun get-no-profundidade (no) "Retorna a profundidade em que o nó se encontra"
	(cadr no)   ; Igual a: (car (cdr no))
)

;; get-no-heuristica
;; Teste: (get-no-profundidade (no-teste))   -> Resultado: 0
(defun get-no-heuristica (no) "Retorna a heurística do nó"
	(caddr no)   ; Igual a: (car (cdr (cdr no)))
)

;; get-no-pai
;; Teste: (get-no-pai (no-teste))   -> Resultado: NIL
(defun get-no-pai (no) "Retorna o nó pai deste nó, ou seja, o nó que gerou este nó"
	(cadddr no)   ; Igual a: (car (cdr (cdr (cdr no))))
)




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



;; operadores 
(defun operadores () "Cria uma lista com todos os operadores do problema dos Pontos e das Caixas"
	(list 'inserir-arco-vertical 'inserir-arco-horizontal)
)


;; numero-linhas-tabuleiro
;; ATENÇÃO: Esta função devolve o número de linhas entre os pontos. Para devolver o número exacto de linhas é preciso somar 1 ao resultado da função."
(defun numero-linhas-tabuleiro (tabuleiro) "Retorna o número de linhas horizontais de um tabuleiro"
	(get-dimensao-aux (car (get-arcos-verticais tabuleiro)))
)


;; numero-colunas-tabuleiro
;; ATENÇÃO: Esta função devolve o número de colunas entre os pontos. Para devolver o número exacto de colunas é preciso somar 1 ao resultado da função."
(defun numero-colunas-tabuleiro (tabuleiro) "Retorna o número de linhas verticais de um tabuleiro"
	(get-dimensao-aux (car (get-arcos-horizontais tabuleiro)))
)




#||

	numero-caixas-fechadas 
||#


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


#||
		FALTA

Verificar se é nó objectivo

||#