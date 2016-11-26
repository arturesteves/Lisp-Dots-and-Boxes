;;;; Projeto Inteligência Artificial 2016-2017
;;;; Daniel Fernandes Costa
;;;; Data:01 Novembro de 2016
;;;; Projeto 1 - Procura

;;; Função procura genérica
(defun procura-generica (no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores &optional (abertos (list no-inicial)) (fechados nil))
"Permite procurar a solucao de um problema usando a procura no espaço de estados. A partir de um estado inicial,
 de uma funcao que gera os sucessores e de um dado algoritmo. De acordo com o algoritmo pode ser usada um limite
 de profundidade, uma heuristica e um algoritmo de ordenacao
"
	(cond
		((null abertos) nil); nao existe solucao ao problema
		((funcall f-solucao (car abertos))  (car abertos)); se o primeiro dos abertos e solucao este no e devolvido
		((existep (first abertos) fechados f-algoritmo) (procuraGenerica no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores (cdr abertos) fechados)); se o no ja existe nos fechados e ignorado
		(T 
			(let* ((lista-sucessores (funcall f-sucessores (first abertos)  lista-operadores f-algoritmo prof-max));lista dos sucessores do primeiro dos abertos
			      (solucao (existe-solucao lista-sucessores f-solucao f-algoritmo)));verifica se existe uma solucao nos sucessores para o dfs
		          (cond
		            (solucao solucao); devolve a solucao
					(T (procuraGenerica no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores (funcall f-algoritmo (rest abertos) lista-sucessores) (cons (car abertos) fechados))); expande a arvore se o primeiro dos abertos nao for solucao
					)
			)
		)
	)
)


;;; Algoritmos

;; Breadht-First (Procura em largura)
(defun breadth-first(abertos sucessores maxDepth)
	(append abertos sucessores)
)
;; Depth-First (Procura em profundidade)
(defun depth-first(abertos sucessores maxDepth)
	(append sucessores abertos)
)
;; A*
(defun a-ast (abertos sucessores maxDepth)
	(sort
		(append abertos sucessores)
		#'< :key #'custo
	)
)

;; IDA* 				<------------------------------------------------ VERIFICAR --------------------
;; IDA* 				<------------------------------------------------ VERIFICAR --------------------
;; IDA* 				<------------------------------------------------ VERIFICAR --------------------
;; IDA* 				<------------------------------------------------ VERIFICAR --------------------
;; IDA* 				<------------------------------------------------ VERIFICAR --------------------
(defun procuraIdaStar (abertos fechados proxF gerados expandidos heuristica)                                                                          
  (cond
   ((null abertos) (format t "não existe solução"))
   ((verificaObjectivo (caar abertos)) (list (car abertos) gerados expandidos 'IDASTAR))
   ((visitado (car abertos) fechados) (procuraIdaStar (cdr abertos) fechados 
                                                      proxF gerados expandidos heuristica))
   (t (let* ((sucessores (gerarSucessores '(colocar esquerda direita) (car abertos) 1 1 '(preto bege castanho) 
                                         heuristica)) 
             (novaListaAbertos (idastar sucessores (cdr abertos) proxF 999)))
        (procuraIdaStar
         (car novaListaAbertos)
         (cons (car abertos) fechados)
         (cadr novaListaAbertos)
         (+ gerados (length sucessores))
         (1+ expandidos)
         heuristica
         )
        );reinicia a procura adicionando os sucessores segundo um dos 3 algoritmos suportados         
      )
   )
  )

(defun idastar (sucessores abertos f proxF) ;gestão do sucessores e lista de abertos segundo a lógica do IDA*
  (cond 
   ((null sucessores) (list  (sort abertos #'< :key #'custo) proxF))
   ((<= (nth 1 (car sucessores)) f)
    (cond 
     ((verificaObjectivo (caar abertos)) (list (cons (car sucessores) abertos) f proxF))
     (t   (idastar (cdr sucessores) (cons (car sucessores) abertos) f proxF)) ;só adiciona caso o f seja menor que o limiar
     )
    )
     (t 
      (cond
       ((and (< (nth 1 (car sucessores)) proxF) (> (nth 1 (car sucessores)) f)) ;caso seja menor calcula o próximo limiar (caso contrário mantém o mesmo próximo limiar)
        (idastar (cdr sucessores) abertos f (nth 1 (car sucessores))))
       (t (idastar (cdr sucessores) abertos f proxF))
       )
      )
    )
   )


















;;; Função de Calculo da Profundidade
(defun profundidade (estado)
"retorna a profundidade de determinado estado"
	(cond
		((null estado)0)
		(t (nth 1 estado))
	)
)

;;; Função do calculo do custo
(defun custo (no)
"retorna o valor do custo do nó"
	(+ (profundidade no)(nth 2 no))
)


;;; Função calculo da penetrância
(defun penetrancia (no gerados)
"retorna o valor da penetrância dos nos gerados até o nó objetivo sobre dos nos totais gerados"
	(cond
		((not (equal gerados 0)) (float (/ profundidade no) gerados))
	)
)

;;; Função de calculo do fator de ramificação
(defun fator-ramificacao (no expandidos)
"retorna o valor do fator de ramificaçao medio do numero de nos expandidos por cada no pai"
	(cond
		((not (equal expandidos 0)) (float (expt expandidos (/ 1 profundidade no))))
	)
)