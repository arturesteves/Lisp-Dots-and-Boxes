;;;; procura.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Implementação dos algoritmos de procura





(defun procura-generica (no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores f-heuristica numero-objectivo-caixas &optional (abertos (list no-inicial)) (fechados nil) (tempo-inicial (get-universal-time)))
"Permite procurar a solucao de um problema usando a procura no espaÃ§o de estados. A partir de um estado inicial,
 de uma funcao que gera os sucessores e de um dado algoritmo. De acordo com o algoritmo pode ser usada um limite
 de profundidade, uma heuristica e um algoritmo de ordenacao
"
	(cond
		((null abertos) nil); nao existe solucao ao problema
		((funcall f-solucao (car abertos) numero-objectivo-caixas)  (list (car abertos) (- (get-universal-time) tempo-inicial))); se o primeiro dos abertos e solucao este no e devolvido com o tempo de exe
		((existep (first abertos) fechados f-algoritmo) (procura-generica no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores f-heuristica numero-objectivo-caixas (cdr abertos) fechados)); se o no ja existe nos fechados e ignorado
		(T 
			(let* ((lista-sucessores (funcall f-sucessores (first abertos)  lista-operadores f-algoritmo prof-max f-heuristica numero-objectivo-caixas))
			      (solucao (existe-solucao lista-sucessores f-solucao f-algoritmo numero-objectivo-caixas)));verifica se existe uma solucao nos sucessores para o dfs
		          (cond
		            (solucao (list solucao (- (get-universal-time) tempo-inicial))); devolve a solucao, com o tempo de execucao
					(T (procura-generica no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores f-heuristica numero-objectivo-caixas (funcall f-algoritmo (rest abertos) lista-sucessores) (cons (car abertos) fechados))); expande a arvore se o primeiro dos abertos nao for solucao
					)
			)
		)
	)
)

;;;;; para desaparecer 
(defun no-teste-tab-a ()
	(list '(
		((NIL NIL NIL) (NIL NIL T) (NIL T T) (NIL NIL T)) 
		((NIL NIL NIL) (NIL T NIL) (NIL NIL T) (NIL T T))
		)
		0 nil
	)
)
;(procura-generica (no-teste-tab-a) 999 'solucaop 'sucessores 'bfs (operadores) 3)

;;;;;;;;; PROCURA-GENERICA mais desactualizada  -> funciona apenas com o bfs e dfs -> to be removed 
#||
(defun procura-generica (no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores numero-objectivo-caixas &optional (abertos (list no-inicial)) (fechados nil))
"Permite procurar a solucao de um problema usando a procura no espaço de estados. A partir de um estado inicial,
 de uma funcao que gera os sucessores e de um dado algoritmo. De acordo com o algoritmo pode ser usada um limite
 de profundidade, uma heuristica e um algoritmo de ordenacao
"
	(cond
		((null abertos) nil); nao existe solucao ao problema
		((funcall f-solucao (car abertos) numero-objectivo-caixas)  (car abertos)); se o primeiro dos abertos e solucao este no e devolvido
		((existep (first abertos) fechados f-algoritmo) (procura-generica no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores numero-objectivo-caixas (cdr abertos) fechados)); se o no ja existe nos fechados e ignorado
		(T 
			(let* ((lista-sucessores (funcall f-sucessores (first abertos)  lista-operadores f-algoritmo prof-max));lista dos sucessores do primeiro dos abertos
			      (solucao (existe-solucao lista-sucessores f-solucao f-algoritmo numero-objectivo-caixas)));verifica se existe uma solucao nos sucessores para o dfs
		          (cond
		            (solucao solucao); devolve a solucao
					(T (procura-generica no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores numero-objectivo-caixas (funcall f-algoritmo (rest abertos) lista-sucessores) (cons (car abertos) fechados))); expande a arvore se o primeiro dos abertos nao for solucao
					)
			)
		)
	)
)
||#

;;; Algoritmos

;; Breadht-First (Procura em largura)
(defun bfs (abertos sucessores)
	(append abertos sucessores)
)

;; Depth-First (Procura em profundidade)
(defun dfs (abertos sucessores)
	(append sucessores abertos)
)

;; A*
(defun a-asterisco (abertos sucessores)
	(sort (append abertos sucessores) #'< :key #'custo)
)

;; IDA* 				<------------------------------------------------ VERIFICAR --------------------
;; IDA* 				<------------------------------------------------ VERIFICAR --------------------
;; IDA* 				<------------------------------------------------ VERIFICAR --------------------
;; IDA* 				<------------------------------------------------ VERIFICAR --------------------
;; IDA* 				<------------------------------------------------ VERIFICAR --------------------
(defun procuraIdaStar (abertos fechados proxF gerados expandidos heuristica)                                                                          
	(cond
		((null abertos) (format t "não existe solução"))
		((verificaObjectivo (caar abertos)) (list (car abertos) gerados expandidos 'ida-asterisco))
		((visitado (car abertos) fechados) (procuraIdaStar (cdr abertos) fechados proxF gerados expandidos heuristica))
		(T (let* ((sucessores (gerarSucessores '(colocar esquerda direita) (car abertos) 1 1 '(preto bege castanho) heuristica)) 
					(novaListaAbertos (ida-asterisco sucessores (cdr abertos) proxF 999)))
			(procuraIdaStar (car novaListaAbertos) (cons (car abertos) fechados) (cadr novaListaAbertos) (+ gerados (length sucessores))
							(1+ expandidos) heuristica)
			);reinicia a procura adicionando os sucessores segundo um dos 3 algoritmos suportados         
		)
	)
)


;; ida-asterisco
(defun ida-asterisco (sucessores abertos f proxF) ;gestão do sucessores e lista de abertos segundo a lógica do IDA*
	(cond 
		((null sucessores) (list  (sort abertos #'< :key #'custo) proxF))
		((<= (nth 1 (car sucessores)) f)
			(cond 
				((verificaObjectivo (caar abertos)) (list (cons (car sucessores) abertos) f proxF))
				(T (ida-asterisco (cdr sucessores) (cons (car sucessores) abertos) f proxF)) ;só adiciona caso o f seja menor que o limiar
			)
		)
		(T (cond
			((and (< (nth 1 (car sucessores)) proxF) (> (nth 1 (car sucessores)) f)) ;caso seja menor calcula o próximo limiar (caso contrário mantém o mesmo próximo limiar)
				(ida-asterisco (cdr sucessores) abertos f (nth 1 (car sucessores))))
			(T (ida-asterisco (cdr sucessores) abertos f proxF))
			)
		)
	)
)


;;;;;;;;;;;;;;;;;; Sucessores
   
;; sucessores
(defun sucessores (no operadores algoritmo-procura profundidade funcao-heuristica numero-objectivo-caixas) "Dado um nó é retornada uma lista com todos os sucessores desse mesmo nó"
	(let* ((operador (car operadores))
		   (numero-linhas (numero-linhas-tabuleiro (get-no-estado no)))
		   (numero-colunas (numero-colunas-tabuleiro (get-no-estado no)))
		   (lista-linhas-colunas-possiveis (cond 																;;; Acho que tenho que chamar sempre aqui +1 +1 ?
											((eql operador 'inserir-arco-horizontal) (reverse (lista-combinacoes (+ numero-linhas 1) numero-colunas)))
											((eql operador 'inserir-arco-vertical)   (reverse (lista-combinacoes (+ numero-colunas 1) numero-linhas))))))
		(cond
			((null operadores) nil)
			((and (equal 'dfs algoritmo-procura) (= (get-no-profundidade no) profundidade)) nil)
			;((not (null operadores)) (cons (sucessores-todas-possibilidades no operador lista-linhas-colunas-possiveis) (sucessores no (cdr operadores) algoritmo-procura profundidade)))
			(T (append (sucessores-todas-possibilidades no operador lista-linhas-colunas-possiveis funcao-heuristica numero-objectivo-caixas) (sucessores no (cdr operadores) algoritmo-procura profundidade funcao-heuristica numero-objectivo-caixas)))
		)
	)
)


;; sucessores-todas-possibilidades
(defun sucessores-todas-possibilidades (no operador possibilidades funcao-heuristica numero-objectivo-caixas)
	(let* ((primeira-possibilidade (car possibilidades))
		   (possibilidades-validas (not (null possibilidades)))
		   (resultado (cond (possibilidades-validas (sucessores-aux no (list operador primeira-possibilidade) funcao-heuristica numero-objectivo-caixas)) (T nil)))
		   ;(resultado (sucessores-aux no (list operador primeira-possibilidade)))
		   (resultado-avaliado (cond ((null resultado) nil) (T (list resultado)))))
		  
		(cond
			((null possibilidades) nil)
			
			(T (append resultado-avaliado (sucessores-todas-possibilidades no operador (cdr possibilidades) funcao-heuristica numero-objectivo-caixas)))
			
			;;testes 
			;(T (list operador primeira-possibilidade "-- " (sucessores-todas-possibilidades no operador (cdr possibilidades))))
			
		)
	)
)


;; sucessores-aux
(defun sucessores-aux (no lista-operador-parametros funcao-heuristica numero-objectivo-caixas)
	(let* ((operador (car lista-operador-parametros))
			(tabuleiro (get-no-estado no))
			(parametros (append (cadr lista-operador-parametros) (list tabuleiro)))
			(resultado-operacao (apply operador parametros))
			(resultado (list resultado-operacao (+ 1 (get-no-profundidade no)) (cond ((not (null funcao-heuristica)) (funcall funcao-heuristica tabuleiro numero-objectivo-caixas)) (T nil)) no)))
		
		(cond
			((null resultado-operacao) nil)
			(T resultado)
		)
	)
)

;; lista-combinacoes
(defun lista-combinacoes (maximo-linhas maximo-colunas) "Recebe uma lista e retorna um conjunto de listas que representa todas as combinacoes possiveis"
	(cond
		((zerop maximo-linhas) nil)
		(T (append (combinacoes-numero-lista maximo-linhas (criar-lista-numeros maximo-colunas)) (lista-combinacoes (- maximo-linhas 1) maximo-colunas)))
	)
)

;; combinacoes-numero-lista
(defun combinacoes-numero-lista (numero lista) "Devolve uma lista com várias listas compostas pelo elemento recebido e um elemento da lista recebida"
	(let ((ultimo-elemento (car (last lista)))) 
		
		(cond
			((null lista) nil)
			(T (cons (list numero ultimo-elemento) (combinacoes-numero-lista numero (reverse (cdr (reverse lista))))))
		)
	)
)

;; criar-lista-numeros
(defun criar-lista-numeros (tamanho &optional (valor-por-omissao 1)) "Devolve uma lista com o tamanho recebido como argumento e o valor dos elementos da lista é recebido se não por omissão têm todos o valor 1"
	(cond
		((zerop tamanho) nil)
		(T (cons valor-por-omissao (criar-lista-numeros (- tamanho 1) (+ valor-por-omissao 1))))
	)
)

 
 
;; Teste: (existep '((0 0) 2 1 nil) '(((1 1) 1 1 nil) ((1 0) 2 1 nil) ((0 0) 2 1 nil) ((2 2) 1 1 nil)) 'dfs)
;; Resultado: NIL
;;existep
 (defun existep (no lista-nos algoritmo)"Retorna verdadeiro se o nó existir na lista.Para o algoritmo dfs,o conceito de nó repetido é particular."
    (let* ((no-comparado (existep-aux no lista-nos))
             (valido (not (null no-comparado))))
        (cond 
            ((and (eql algoritmo 'dfs) valido (= (get-no-profundidade no) (get-no-profundidade no-comparado))) T) ; algoritmo dfs
            ((and (eql algoritmo 'bfs) valido) T)
            ((and (eql algoritmo 'a-asterisco) valido) T) ;; confirmar se nao existe alguma limitaçao
            ((and (eql algoritmo 'ida-asterisco) valido) T) ;; confirmar se nao existe alguma limitaçao
            (T nil)
        )
    )
)


;; Teste: (existep-aux '((0 0) 2 1 nil) '(((1 1) 1 1 nil) ((1 0) 1 1 nil) ((0 0) 2 1 nil) ((2 2) 1 1 nil)))
;; Resultado: T
;existep-aux **
(defun existep-aux (no lista-nos)"Verifica se um nó existe numa lista de nos"
    (cond
        ((null lista-nos) nil)
        ;((equal no (car lista-nos)) T)
        ((equal (get-no-estado no) (get-no-estado (car lista-nos)))(car lista-nos))
        (T (existep-aux no (cdr lista-nos)))
    )
) 
 
 
;; existe-solucao
(defun existe-solucao (lista f-solucao f-algoritmo numero-objectivo-caixas)
"Verifica se existe uma solucao ao problema numa lista de sucessores para o algoritmo dfs"
	(cond
		((not (eql f-algoritmo 'dfs)) nil)
		((null lista) nil)
		((funcall f-solucao (car lista) numero-objectivo-caixas) (car lista))
		(T (existe-solucao (cdr lista) f-solucao f-algoritmo numero-objectivo-caixas))
	)
)

;;; Função do calculo do custo

;;; Funções de Cálculo


;; penetrancia
(defun penetrancia (no nos-gerados) "Retorna o valor da penetrância dos nos gerados até o nó objetivo sobre dos nos totais gerados"
    (cond
        ((not (equal  nos-gerados 0)) (float (/ (get-no-profundidade no)  nos-gerados)))
    )
)

;;; Função de calculo do fator de ramificação
#||
;; fator-ramificacao
(defun fator-ramificacao (no expandidos)
"retorna o valor do fator de ramificaçao medio do numero de nos expandidos por cada no pai"
	(cond
		((not (equal expandidos 0)) (float (expt expandidos (/ 1 (profundidade no)))))
	)
)||#