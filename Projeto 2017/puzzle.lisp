;;;; Projeto Inteligência Artificial 2016-2017
;;;; Daniel Fernandes Costa
;;;; Data:01 Novembro de 2016
;;;; Projeto 1 - Puzzle

;; tabuleiro de Teste
(defun tabuleirolab ()
"retorna um tabuleiro vazio de dimensão 3 x 3"
	(list '((NIL T T) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))
		  '((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL))
	)
)

(defun tabuleirolab2 ()
"retorna um tabuleiro vazio de dimensão 3 x 3"
	(list '((T T T) (T T NIL) (NIL NIL NIL) (NIL NIL NIL))
		  '((T T NIL) (NIL T NIL) (NIL NIL NIL) (NIL T NIL))
	)
)

;;; Métodos selectores / de consulta ●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●
;; Consulta dos arcos horizontais
(defun get-arcos-horizontais (tabuleiro)
"retorna a lista dos arcos horizontais de um determinado tabuleiro"
	(cond
		((null tabuleiro) nil)
		(T (car tabuleiro))
	)
)

;; Consulta dos arcos verticais
(defun get-arcos-verticais (tabuleiro)
"retorna a lista dos arcos verticais de um determinado tabuleiro"
	(cond
		((null tabuleiro) nil)
		(T (car (cdr tabuleiro)))
	)
)


;;; Operadores ●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●

;; Função de Inserir arco vertical

(defun arco-vertical(x y lista)
"Insere um arco vertical num tabuleiro numa posiçao dada pela linha e coluna."
	(cond
		((null lista) nil)
		(t (list (car lista) (arco-vertical-aux x y (get-arcos-verticais lista))))
	)
)

;; Função de Inserir arco horizontal
(defun arco-horizontal(x y lista)
"Insere um arco horizontal num tabuleiro numa posiçao dada pela linha e coluna."
	(cond
		((null lista) nil)
		(t (list (arco-vertical-aux x y (get-arcos-horizontais lista)) (cdr lista)))
	)
)

;;; Heuristica ●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●

;; Numero de caixas fechadas de um determinado tabuleiro
;; (numero-caixas-fechadas(get-arcos-horizontais (tabuleirolab2)))
;; (((T T T) (T T T) (NIL NIL NIL) (NIL NIL NIL))
;; => 2

(defun numero-caixas-fechadas(tabuleiro)
"retorna o numero de caixas fechadas de um tabuleiro"
	(cond
		((null tabuleiro)nil)
		(T 
			(numero-caixas-fechadas-aux (get-arcos-verticais tabuleiro)
										(get-arcos-horizontais tabuleiro)
			)
		)			  
	)
)



(defun numero-caixas-fechadas2(tabuleiro)
	(cond
		((null tabuleiro)nil)
		(
			(AND
				(eq (caar(get-arcos-horizontais tabuleiro)) T)
				(eq (caar (get-arcos-verticais tabuleiro)) T)
				(AND
					(eq (car (car (cdr (get-arcos-horizontais tabuleiro)))) T)
					(eq (car (car (cdr (get-arcos-verticais tabuleiro)))) T)
				)				
			)
			1
		)
		(t (+ 1 numero-caixas-fechadas2 (cdr tabuleiro)))
	)
)

;; filipe.mariano@estsetubal....
;; <-------------------------


;; Tentativas
(defun get-caixas-lista(tabuleiro)
	(cond
		((null tabuleiro) nil)
		(t
			 (list  (caar(get-arcos-horizontais(tabuleiro))) 
					(caar(get-arcos-verticais(tabuleiro)))
					(caadr (get-arcos-horizontais(tabuleiro)))
					(caadr (get-arcos-verticais(tabuleiro)))
			)
		)
	)
)


;; Função que verifica se o objetivo do problema foi atingido com sucesso 
;;((NIL T T) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))
;;((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL))
;; Objetivo é fechar 5 caixas 

(defun objetivo (tabuleiro)
"retorna o valor booleano do objetivo do problema"
	(cond
		((null tabuleiro) nil)
	)
)



;; Heuristica Dada
(defun heuristica (tabuleiro objetivo &optional (heu 0))
"retorna a avaliação do tabuleiro, de acordo com a heurística definida no enunciado do projeto. A função recebe um tabuleiro e um valor inteiro que representa o objetivo: o número de caixas a fechar"
	(cond
		((null tabuleiro) heu)
		((> (length (car tabuleiro))1) (heuristica (cdr tabuleiro) (+ 1 heu)))
		(t (heuristica (cdr tabuleiro) heu))
	)
)




;;;; CONSTRUTORES - NÓS E SUCESSORES  <---------------------------------

;; ler-vasilhas
(defun ler-vasilhas ()
	(let  ((estado (progn (format t "Estado:~%") (read)))
		(profundidade (progn (format t "Profundidade:~%") (read)))
		(no-pai (progn (format t "No pai:~%") (read))))

		(cria-no estado profundidade no-pai)
	)
)

;; cria nó
(defun cria-no (estado profundidade no-pai)
  (list estado profundidade no-pai)
)








;;; Funções Auxiliares●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●●

;;Funções auxiliares para calcualr o numero de caixas numero de caixas
(defun numero-caixas-fechadas-aux (vertical horizontal)
	(cond 
		((AND (null vertical) (null horizontal)) 0)
		((eval (list 'AND (car vertical) (segundo-auxiliar vertical) (car horizontal) (segundo-auxiliar horizontal))) (+ 1 (numero-caixas-fechadas-aux (cdr vertical) (cdr horizontal))))
		(T (+ 0 (numero-caixas-fechadas-aux (cdr vertical) (cdr horizontal))))  
	)
)
(defun segundo-auxiliar(lista)
	(car (cdr lista))
)

;; Funções Auxiliares dos Operadores
(defun arco-vertical-na-posicao (pos lista)
"Insere um arco vertical numa linha da lista que representa um conjunto de arcos verticais (coluna do tabuleiro)"
	(cond
		((null lista) (cons 'T lista))
		(
			(or 
				(= pos 0)
				(> pos (length lista))
			)
			(princ "Posicao compreendido entre 1 e 4")
		)
		((= pos 1) (cons 'T (cdr lista)))
		(t (cons (car lista) (arco-vertical-na-posicao(- pos 1) (cdr lista))))
	)
)


(defun arco-vertical-aux(x y lista)
"Insere um arco vertical numa lista que representa o conjunto de arcos verticais dum tabuleiro. A posicao representada pela linha e a coluna de destino sao valores inteiros recebidos por parametro." 
 	(cond
		((null lista) nil)
		((= y 1) (cons (arco-vertical-na-posicao x (car lista)) (cdr lista)))
		(t (cons (car lista) (arco-vertical-aux x (- y 1) (cdr lista))))
	)
)

