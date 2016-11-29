;;;; procura.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Implementação dos algoritmos de procura




;;; operadores 
(defun operadores () "Cria uma lista com todos os operadores do problema dos Pontos e das Caixas"
	(list 'inserir-arco-vertical 'inserir-arco-horizontal)
)



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


#|| 

VERIFICAR se está a funcionar bem

||#
(defun procura-generica (no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores f-heuristica &optional (abertos (list no-inicial)) (fechados nil) (tempo-inicial (get-universal-time)))
"Permite procurar a solucao de um problema usando a procura no espaÃ§o de estados. A partir de um estado inicial,
 de uma funcao que gera os sucessores e de um dado algoritmo. De acordo com o algoritmo pode ser usada um limite
 de profundidade, uma heuristica e um algoritmo de ordenacao
"
	(cond
		((null abertos) nil); nao existe solucao ao problema
		((funcall f-solucao (car abertos))  (list (car abertos) (- (get-universal-time) tempo-inicial))); se o primeiro dos abertos e solucao este no e devolvido com o tempo de exe
		((existep (first abertos) fechados f-algoritmo) (procura-generica no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores (cdr abertos) fechados)); se o no ja existe nos fechados e ignorado
		(T 
			(let* ((lista-sucessores (funcall f-sucessores (first abertos)  lista-operadores f-algoritmo prof-max f-heuristica))
			      (solucao (existe-solucao lista-sucessores f-solucao f-algoritmo)));verifica se existe uma solucao nos sucessores para o dfs
		          (cond
		            (solucao (list solucao (- (get-universal-time) tempo-inicial))); devolve a solucao, com o tempo de execucao
					(T (procura-generica no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores f-heuristica (funcall f-algoritmo (rest abertos) lista-sucessores) (cons (car abertos) fechados))); expande a arvore se o primeiro dos abertos nao for solucao
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



   
   
#||

Sucessores

||#



 #||
 
 
 Rever / Adaptar ao problema 
 
 
 
 ||#

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


