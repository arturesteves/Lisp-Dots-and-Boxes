;;;; procura.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Implementação dos algoritmos de procura




;;; operadores 
(defun operadores () "Cria uma lista com todos os operadores do problema dos Pontos e das Caixas"
	(list 'inserir-arco-vertical 'inserir-arco-horizontal)
)


;;; Construtor

;;cria-no
(defun cria-no (estado &optional (profundidade 0) (heuristica 0) (no-pai nil)) 
"Cria uma lista que representa um nó; Um nó é composto pelo estado que é o tabuleiro, este é um parâmetro obrigatório, é composto também por outros parâmetros, como
a profundidade a que se encontra, pela heurística deste mesmo nó e pelo nó pai, ou seja, o nó que o gerou. A profundidade e a heuróistica por defeito têm valor 0, enquanto que o nó pai por defeito é NIL"

	(list estado profundidade heuristica no-pai)
)


;;; Metodos seletores

;; get-estado-no
;; Teste: (get-estado-no (no-teste))   -> Resultado: (((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)))
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




#||

Procura genérica
	(defun procura-generica (no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores &optional (abertos (list no-inicial)) (fechados nil))
Procuras concrectas


Sucessores
	(defun sucessores (no operadores algoritmo-procura profundidade) ... ) 	-> Funciona para os algoritmos 'bfs' e 'dfs'

Sucessores-aux

solução

(defun existep (no lista-nos algoritmo) ... )

(defun existe-solucao (lista f-solucao f-algoritmo) ...) 

||#

;;; Procura

;;;procura-generica

;;; Algoritmos

;; bfs   -> Breadth First Search
(defun bfs (abertos sucessores)	"Cria uma lista com a lista dos nós abertos em primeiro e os nós sucessores em último. Esta ordenação irá resultar numa procura em largura"
	(append abertos sucessores)
)

;; dfs   -> Depth First Search
(defun dfs (abertos sucessores)	"Cria uma lista com a lista dos nós sucessores em primeiro e os nós abertos em último. Esta ordenação irá resultar numa procura em profundidade"
	(append sucessores abertos)
)


;;; 
;; Teste: (solucaop (no-teste))   -> Resultado: NIL
#||
	
	Descomentar quando souber como guardar o número de caixas a fechar recebido como argumento

(defun solucaop (no)	
"Recebe um nó e verifica se este este é um nó objectivo, verificando o estado. Um nó é objectivo se o número de caixas fechadas 
do seu estado for igual ao número de caixas a fechar lido no início do programa"

	(= (numero-caixas-fechadas (get-estado-no no)) NUMERO CAIXAS A FECHAR DEFINIDAS NO INICIO DO PROGRAMA))
)

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


 