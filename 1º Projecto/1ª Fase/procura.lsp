;;;; procura.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Implementação dos algoritmos de procura

;;; Implementação das Procuras

;; procura-generica
(defun procura-generica (no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores f-heuristica numero-objectivo-caixas &aux (tempo-inicial (get-universal-time)))
"Permite procurar a solucao de um problema usando a procura no espaÃ§o de estados. A partir de um estado inicial,
 de uma funcao que gera os sucessores e de um dado algoritmo. De acordo com o algoritmo pode ser usada um limite
 de profundidade, uma heuristica e um algoritmo de ordenacao
" 
	(let ( (abertos (list no-inicial)) (fechados nil) )
		(loop
		  (let ((no-atual (first abertos)))
				 (cond
					   ((null abertos) (return nil))
					  ((funcall f-solucao no-atual numero-objectivo-caixas) (return (list no-atual (length abertos) (length fechados) (- (get-universal-time) tempo-inicial) )))
					  ((existep no-atual fechados f-algoritmo) (setf abertos (rest abertos))); se o no ja existe nos fechados e ignorado
					   (T 
							(let* ((lista-sucessores (funcall f-sucessores no-atual lista-operadores f-algoritmo prof-max f-heuristica numero-objectivo-caixas))
					   
					   (solucao (existe-solucao lista-sucessores f-solucao f-algoritmo numero-objectivo-caixas)));verifica se existe uma solucao nos sucessores para o dfs
							  (cond
								(solucao (return (append (list solucao) (list (+ (length abertos) (length lista-sucessores)) (length fechados) (- (get-universal-time) tempo-inicial))))); devolve a solucao, com o tempo de execucao
									(T (progn
										(setf abertos (funcall f-algoritmo (rest abertos) lista-sucessores))
										(setf fechados (cons no-atual fechados))
										)                
									)            
								)          
							)         
						)      
					)     
			)     
		)    
	)
)

;;procura-ida-asterisco
(defun procura-ida-asterisco  (no-inicial prof-max f-solucao f-sucessores  f-algoritmo lista-operadores f-heuristica numero-objectivo-caixas &aux (tempo-inicial (get-universal-time)))
	 "Permite procurar a solucao de um problema usando procura no espaço de estados. A partir de um estado inicial,
	gera os sucessores e do algoritmo IDA* a partir de um estado inicial e um limiar.
" 
   (let ( 	(abertos (list no-inicial)) 
			(fechados nil) 
			(limiar (funcall f-heuristica (get-no-estado no-inicial) numero-objectivo-caixas)))
		(loop
            (let ((no-atual (first abertos)))
                (cond
                    ((null abertos) (return nil))
                    ((and (funcall f-solucao no-atual numero-objectivo-caixas) (>= limiar (custo no-atual))) (return (list no-atual (length abertos) (length fechados))))
                    ((existep no-atual fechados f-algoritmo) (setf abertos (rest abertos))); se o no ja existe nos fechados e ignorado
                    (T 
                        (let* ((lista-sucessores (funcall f-sucessores no-atual lista-operadores f-algoritmo prof-max f-heuristica numero-objectivo-caixas))
                                (solucao (existe-solucao lista-sucessores f-solucao f-algoritmo numero-objectivo-caixas)));verifica se existe uma solucao nos sucessores para o dfs						
                            (cond
								((null lista-sucessores) (return nil)) ;; retorna nil quando esta a null a lista de sucessores
								((and solucao (>= limiar (custo solucao))) (return (append (list solucao) (list (+ (length abertos) (length lista-sucessores)) (+ 1 (length fechados)) (- (get-universal-time) tempo-inicial)))))
								(T
									(progn
										(setf limiar (novo-limiar limiar lista-sucessores))
										(setf abertos (funcall f-algoritmo (rest abertos) lista-sucessores limiar))
										(setf fechados (cons no-atual fechados))
									)
								)
                            )
                        )
                    )
				)
			)
        )
    )
)

;;; Funções auxiliares dos Sucessores

;;get-novo-limiar
(defun novo-limiar (limiar sucessores) "Função auxiliar da procura do algoritmo IDA*, que serve para implementar um novo limiar caso seja maior que o custo."
    (let ((min-custo (custo (first (sort sucessores #'< :key #'custo)))))
        (cond
            ((null sucessores) limiar)
            ((> min-custo limiar) min-custo)
            (T limiar)
        )
    )
)

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

;; IDA*
(defun ida-asterisco (abertos sucessores limiar)
	(cond
		((null sucessores) abertos)
		(
			(> (custo (car sucessores))limiar) ;; se o custo do sucessor for <= que o limiar ele volta a fazer
			(ida-asterisco abertos (cdr sucessores) limiar)
		)
		(T
			(cons
				(car sucessores)
				(ida-asterisco abertos (cdr sucessores) limiar)
			)
		)
	)
)


;;; Sucessores
   
;;sucessores
(defun sucessores (no operadores algoritmo-procura profundidade funcao-heuristica numero-objectivo-caixas) "Dado um nó é retornada uma lista com todos os sucessores desse mesmo nó"
	(let* ((operador (car operadores))
		   (numero-linhas (numero-linhas-tabuleiro (get-no-estado no)))
		   (numero-colunas (numero-colunas-tabuleiro (get-no-estado no)))
		   (lista-linhas-colunas-possiveis (cond 
											((eql operador 'inserir-arco-horizontal) (reverse (lista-combinacoes (+ numero-linhas 1) numero-colunas)))
											((eql operador 'inserir-arco-vertical)   (reverse (lista-combinacoes (+ numero-colunas 1) numero-linhas))))))
		(cond
			((null operadores) nil)
			((and (equal 'dfs algoritmo-procura) (= (get-no-profundidade no) profundidade)) nil)
			(T (append (sucessores-todas-possibilidades no operador lista-linhas-colunas-possiveis funcao-heuristica numero-objectivo-caixas) (sucessores no (cdr operadores) algoritmo-procura profundidade funcao-heuristica numero-objectivo-caixas)))
		)
	)
)


;;; Funções auxiliares dos Sucessores
;;sucessores-todas-possibilidades
(defun sucessores-todas-possibilidades (no operador possibilidades funcao-heuristica numero-objectivo-caixas)
	(let* ((primeira-possibilidade (car possibilidades))
		   (possibilidades-validas (not (null possibilidades)))
		   (resultado (cond (possibilidades-validas (sucessores-aux no (list operador primeira-possibilidade) funcao-heuristica numero-objectivo-caixas)) (T nil)))
		   (resultado-avaliado (cond ((null resultado) nil) (T (list resultado)))))
		  
		(cond
			((null possibilidades) nil)
			
			(T (append resultado-avaliado (sucessores-todas-possibilidades no operador (cdr possibilidades) funcao-heuristica numero-objectivo-caixas)))
		)
	)
)


;;sucessores-aux
(defun sucessores-aux (no lista-operador-parametros funcao-heuristica numero-objectivo-caixas)
	(let* ((operador (car lista-operador-parametros))
			(tabuleiro (get-no-estado no))
			(parametros (append (cadr lista-operador-parametros) (list tabuleiro)))
			(resultado-operacao (apply operador parametros))
			(resultado (cria-no resultado-operacao (+ 1 (get-no-profundidade no)) (cond ((not (null funcao-heuristica)) (funcall funcao-heuristica tabuleiro numero-objectivo-caixas)) (T nil)) no)))
		(cond
			((null resultado-operacao) nil)
			(T resultado)
		)
	)
)

;;lista-combinacoes
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

 
;;; Funções de Verificações

;;existep
 (defun existep (no lista-nos algoritmo)"Retorna verdadeiro se o nó existir na lista.Para o algoritmo dfs,o conceito de nó repetido é particular."
    (let* ((no-comparado (existep-aux no lista-nos))
             (valido (not (null no-comparado))))
        (cond 
            ((and (eql algoritmo 'dfs) valido (= (get-no-profundidade no) (get-no-profundidade no-comparado))) T) ; algoritmo dfs
            ((and (eql algoritmo 'bfs) valido) T)
            ((and (eql algoritmo 'a-asterisco) valido) T)
            ((and (eql algoritmo 'ida-asterisco) valido) T)
            (T nil)
        )
    )
)



;existep-aux 
(defun existep-aux (no lista-nos)"Verifica se um nó existe numa lista de nos"
    (cond
        ((null lista-nos) nil)
        ((equal (get-no-estado no) (get-no-estado (car lista-nos)))(car lista-nos))
        (T (existep-aux no (cdr lista-nos)))
    )
) 
 
 
;; existe-solucao
(defun existe-solucao (lista f-solucao f-algoritmo numero-objectivo-caixas) "Verifica se existe uma solucao ao problema numa lista de sucessores para o algoritmo dfs"
	(cond
		((not (eql f-algoritmo 'dfs)) nil)
		((null lista) nil)
		((funcall f-solucao (car lista) numero-objectivo-caixas) (car lista))
		(T (existe-solucao (cdr lista) f-solucao f-algoritmo numero-objectivo-caixas))
	)
)


;;; Funções de Cálculo

;;penetrancia
(defun penetrancia (no nos-gerados) "Retorna o valor da penetrância dos nos gerados até o nó objetivo sobre dos nos totais gerados"
    (cond
        ((not (equal  nos-gerados 0)) (float (/ (get-no-profundidade no)  nos-gerados)))
    )
)

;;fator-ramificacao 
; B+B^2+B^3+...+B^L=T [L comprimento do caminho até ao objetivo, T numero total de nós gerados]
(defun fator-ramificacao (L valor-t  &optional (margem-erro 0.5) (bmin 1) (bmax 10e11)) "retorna o valor do fator de ramificaçao do no"
    (let* ((bmedio (/ (+ bmin bmax) 2)))
        (cond 
            ((< (- bmax bmin) margem-erro) (/ (+ bmax bmin) 2))
            ((> (- (f-polinomial L bmedio) valor-t) margem-erro) (fator-ramificacao L valor-t margem-erro bmin bmedio))
            (T 
                (float(fator-ramificacao L valor-t margem-erro bmedio bmax))
            )
        )
    )
)

;;; Funções auxiliares
(defun f-polinomial (polinomio x)
    (cond 
        ((= polinomio 1) x)
        (t (+ (expt x polinomio) (f-polinomial (- polinomio 1) x)))
    )
)