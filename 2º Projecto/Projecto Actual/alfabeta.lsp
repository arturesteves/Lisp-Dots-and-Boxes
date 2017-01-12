;;;; procura.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Implementação dos algoritmos de procura

;;; Constantes
(defvar *corte-alfa* 0)
(defvar *corte-beta* 0)

;;Teste		:(vencedorp (no-teste2) 5 4) || (vencedorp (no-teste2) 1 8)
;;Resultado :1 || 2
(defun vencedorp(no caixas-fechadas-j1 caixas-fechadas-j2) "retorna o jogador que venceu a partir. Caso empate retorna 0"
	(let ((resultado (= (+ caixas-fechadas-j1 caixas-fechadas-j2) (verifica-todas-caixas-fechadas no))))
		(cond
			(resultado (cond
						((> caixas-fechadas-j1 caixas-fechadas-j2) 1)
						((< caixas-fechadas-j1 caixas-fechadas-j2) 2)
						))
			(T nil)
					
		)
	)
)

(defun verifica-todas-caixas-fechadas(no)
	(* 	(length (car (get-arcos-horizontais (get-no-estado no))))
		(length (car (get-arcos-verticais (get-no-estado no))))
	)
)



(defun avaliar-folha(no) "Retorna o valor [100] em caso de vitoria do jogador MAX, o valor [-100] em caso de vitoria do jogador MIN ou o valor [0] em caso de empate."
	(cond
		((equal (verificar-profundidade-jogador no) 'MAX) 100)
		(t -100)
	)
)

;verificar-jogador
(defun verificar-profundidade-jogador(no) "Função que verifica se o jogador encontra-se na profundidade de MAX ou MIN"
	(let ((profundidade (get-no-profundidade no)))
		(cond
			((or
				(evenp profundidade) ;;evenp returns true if integer is even (divisible by two); otherwise, returns false.
				(= profundidade 0)) 'MAX)
				(t 'MIN)
		)
	)
)


;;Algoritmo alfa-beta
(defun alfa-beta (no profundidade-limite peca caixas-fechadas-j1 caixas-fechadas-j2 &aux (tempo-inicial (get-universal-time)))

)




;;; Sucessores

;; Tipo-jogador MAX e MIN
;; Simbolo-jogador 1 e 2

;;sucessores
(defun sucessores (no peca caixas-fechadas-j1 caixas-fechadas-j2)
	(let* (
		   (numero-linhas (numero-linhas-tabuleiro (get-no-estado no)))
		   (numero-colunas (numero-colunas-tabuleiro (get-no-estado no)))
		   (lista-linhas-colunas-possiveis (cond 
											((eql operador 'inserir-arco-horizontal) (reverse (lista-combinacoes (+ numero-linhas 1) numero-colunas)))
											((eql operador 'inserir-arco-vertical)   (reverse (lista-combinacoes (+ numero-colunas 1) numero-linhas))))))
		(cond
			((equal peca 1))
			((equal peca 2))
		)
	)
)


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
	

#|
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
|#

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

(defun existep (no lista-nos) "Retorna verdadeiro se o nó existir na lista"
	(cond
		((null lista-nos) nil)
		( (equal (get-no-estado no) (get-no-estado (car lista-nos))) T)
		;((equal no (car lista-nos)) T)
		(t
			(existep no (cdr lista-nos))
		)
	)
)

;; existe-solucao
(defun existe-solucao (lista f-solucao numero-objectivo-caixas) "Verifica se existe uma solucao ao problema numa lista de sucessores"
	(cond
		((null lista) nil)
		((funcall f-solucao (car lista) numero-objectivo-caixas) (car lista))
		(T (existe-solucao (cdr lista) f-solucao numero-objectivo-caixas))
	)
)



#|
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
|#
