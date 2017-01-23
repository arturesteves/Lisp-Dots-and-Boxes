;;;; procura.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Implementação dos algoritmos de procura

;;; Variáveis Globais
(defvar *corte-alfa* 0)
(defvar *corte-beta* 0)



;avaliar-folha
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

;;guardo numa variavel global a melhor jogada? -> mesmo o no?

;;Algoritmo alfa-beta
;; Old: (no profundidade-limite peca caixas-fechadas-j1 caixas-fechadas-j2 &optional (alfa 2) (beta 2) &aux (tempo-inicial (get-universal-time)))

;; Aplicar esta condição mo alfabeta ou nos sucessores? 
	;; ((and (equal 'dfs algoritmo-procura) (= (get-no-profundidade no) profundidade)) nil)
(defun alfa-beta (no profundidade-limite peca max-or-min &optional (alfa 2) (beta 2) &aux (tempo-inicial (get-universal-time)))
	(cond
		((or (= profundidade-limite 0) (no-folhap no)) (funcao-utilidade no))
		((eql 'MAX) (max-side (sucessores ...) profundidade-limite alfa beta)) 
		(T (min-side (sucessores ...) profundidade-limite alfa beta))  ;; (eql 'MIN) 
	)
)

(defun max-side (sucessores profundidade-limite alfa beta)
	(cond
		((null sucessores) nil)
		(T (let ((valor-utilidade-no (alfa-beta (car sucessores) (- profundidade-limite 1) 'MIN alfa beta)))
			(cond
				((> valor-utilidade-no alfa) (max-side (cdr sucessores) (- profundidade-limite 1) valor-utilidade-no beta)) 
				((>= alfa beta) (setf *corte-alfa* (+ *corte-alfa* 1)) beta) ; houve corte alfa
				(T alfa)
			)
		))
	)
)

(defun min-side (sucessores profundidade-limite alfa beta)
	(cond
		((null sucessores) nil)
		(T (let ((valor-utilidade-no (alfa-beta (car sucessores) (- profundidade-limite 1) 'MAX alfa beta)))
			(cond
				((> valor-utilidade-no beta) (min-side (cdr sucessores) (- profundidade-limite 1) alfa valor-utilidade-no)) 
				((<= beta alfa) (setf *corte-beta* (+ *corte-beta* 1)) alfa) ; houve corte beta
				(T beta)
			)
		))
	)
)

;;; tenho que ter uma função sucessores-alem que verifica se um no gerado fechou uma caixa ou nao. se fechou vou gerar os sucessores desse no
;; para depois adicionar a lista de sucessores actuais

;;(= (get-no-utilidade no) alfa)	(max-side )

#||
(* the minimax value of n, searched to depth d.
 * If the value is less than min, returns min.
 * If greater than max, returns max. *)
 
 fun minimax(n: node, d: int, min: int, max: int): int =
   if leaf(n) or depth=0 return evaluate(n)
   if n is a max node
      v := min
      for each child of n
         v' := minimax (child,d-1,...,...)
         if v' > v, v:= v'
         if v > max return max
      return v
   if n is a min node
      v := max
      for each child of n
         v' := minimax (child,d-1,...,...)
         if v' < v, v:= v'
         if v < min return min
      return v  
||#


;; modificar a função, calcular sempre os sucessores demora muito tempo-inicial
;; ou verifico se o nº de caixas fechadas é igual a 49 ou a numero que ja nao permita o outro ganhar.
;; verificar se a profundidade é a máxima, pq se for e mais rapido
(defun no-folhap (no)
	(let* ((sucessores-no (sucessores no))
		  (numero-sucessores (length sucessoes-no)))
		(cond
			((> numero-sucessores 0) nil)
			(T T)
		)
	)
)	

;; os nós folha finais são os que ainda têm sucessores
;; os nós folha são os que já não tem mais sucessores


; inteligência -> Ver artigos 
; caso seja utilizada uma função de utilidade de outra pessoa, esta deve ser mencionada no projecto
(defun funcao-utilidade (no)

)























;;; Sucessores

;; Tipo-jogador MAX e MIN
;; Simbolo-jogador 1 e 2

;;sucessores 
; (no operadores algoritmo-procura profundidade funcao-heuristica numero-objectivo-caixas)
;; 
;; Tenho que receber a peça pq é a peça a aplicar a inserir nas varias posicoes.

(defun sucessores-alfabeta (no operadores profundidade peca caixas-fechadas-j1 caixas-fechadas-j2)
	(let ((sucessores (no operadores profundidade peca caixas-fechadas-j1 caixas-fechadas-j2))
		 )
		 ;; verificar 
		 ;; se eu gerar os sucessores todos nunca faço os cortes alfa beta!!!!!
		
		;; comoé que sei que é um no folha?	-> todos os do sucessores saoum no folha certo? -> ou depende da profundidade?
		;;
		(cond
			
		)
	)
)

;; pq preciso do numero de caixas fechadas por cada jogador??????
(defun sucessores (no operadores profundidade peca caixas-fechadas-j1 caixas-fechadas-j2)
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
