;;;; procura.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Implementação dos algoritmos de procura

;;; Variáveis Globais
(defvar *corte-alfa* 0)
(defvar *corte-beta* 0)
(defvar *jogada-pc* nil) ;;variavel que guarda o tabuleiro que corresponde a melhor jogada que é atualizada pelo algoritmo cada vez que o valor de beta é atualizado.
(defvar *nos-analisados* 0) ;; variavel que guarda o numero de nos visitados
(defvar *tempo-despendido*)


;;Algoritmo alfa-beta
;; Old: (no profundidade-limite peca caixas-fechadas-j1 caixas-fechadas-j2 &optional (alfa 2) (beta 2) &aux (tempo-inicial (get-universal-time)))

;; Aplicar esta condição mo alfabeta ou nos sucessores? 
	;; ((and (equal 'dfs algoritmo-procura) (= (get-no-profundidade no) profundidade)) nil)
(defun alfa-beta (no profundidade-limite peca f-utilidade &optional (alfa -1000) (beta 1000)  (tempo-inicial (get-universal-time)) 	;; tempo de quando começou a 1ª procura no alfabeta
																																	(tempo-maximo 5000)) ; 5 milisegundos
	(let* ((peca-a-jogar (cond ((= (get-no-profundidade no) 0) peca) (T (troca-peca peca))))
			(no-max (e-no-maxp no))
			(caixas-jogador-1 (get-caixas-jogador-1 no))
			(caixas-jogador-2 (get-caixas-jogador-2 no)) 
			;(tempo-limite-margem (* tempo-maximo 0.9))	; 90% do tempo limite	-> aplicar se for usado o campeonato
			(tempo-actual (get-universal-time))	;; tempo de quando começou a n pesquisa
			(tempo-gasto (- tempo-actual tempo-inicial))
			(tempo-dispendido (setf *tempo-despendido* tempo-gasto))
			
			(nos-analisados (setf *nos-analisados* (+ *nos-analisados* 1)))
			)
			
		(cond
			((or  (>= tempo-gasto tempo-maximo) 
					(no-folhap no) 	;; REVER QUE ESTA MERDA TA A FALHAR
					(= profundidade-limite (get-no-profundidade no))) 
																									;;(setf *nos-analisados* (+ *nos-analisados* 1))	
																									(funcall f-utilidade no peca caixas-jogador-1 caixas-jogador-2))
																						
			(no-max (max-side (sucessores-alfabeta no (operadores) profundidade-limite peca-a-jogar f-utilidade caixas-jogador-1 caixas-jogador-2) profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo))		
			; equivalente a ((not no-max) ())
			(T (min-side (sucessores-alfabeta no (operadores) profundidade-limite peca-a-jogar f-utilidade caixas-jogador-1 caixas-jogador-2) profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo))
		)
	)
)	

(defun max-side (sucessores profundidade-limite peca f-utilidade alfa beta tempo-inicial tempo-maximo)
	(cond
		((null sucessores) nil)
		(T (let ((valor-utilidade-no (alfa-beta (car sucessores) profundidade-limite (troca-peca peca) f-utilidade alfa beta tempo-inicial tempo-maximo)))
			(cond
			
			;; check 	Aqui algures é preciso fazer update da melhor jogada
			;;; Tenho que ir buscar o maior dos sucessores? e esse fica a melhor jogada?
				#||
				Old: ((> valor-utilidade-no alfa) (max-side (cdr sucessores) profundidade-limite peca f-utilidade valor-utilidade-no beta tempo-inicial tempo-maximo))
				||#
				((> valor-utilidade-no alfa) (progn
															(setf *jogada-pc* (car sucessores))		;; Actualiza a melhor jogada!
															(max-side (cdr sucessores) profundidade-limite peca f-utilidade valor-utilidade-no beta tempo-inicial tempo-maximo)))
				((>= alfa beta) (setf *corte-beta* (+ *corte-beta* 1)) beta) ; houve corte alfa
				(T alfa)
			)
		))
	)
)


 
 (defun min-side (sucessores profundidade-limite peca f-utilidade alfa beta tempo-inicial tempo-maximo)
	(cond
		((null sucessores) nil)
		(T (let ((valor-utilidade-no (alfa-beta (car sucessores) profundidade-limite (troca-peca peca) f-utilidade alfa beta tempo-inicial tempo-maximo)))
			(cond
			
			;; check	Aqui algures é preciso fazer update da melhor jogada
			;;; Tenho que ir buscar o menor dos sucessores? e esse fica a melhor jogada?
				((> valor-utilidade-no beta) (progn
																;(setf *jogada-pc* (car sucessores))
																(min-side (cdr sucessores) profundidade-limite peca f-utilidade alfa valor-utilidade-no tempo-inicial tempo-maximo)))
				((<= beta alfa) (setf *corte-alfa* (+ *corte-alfa* 1)) alfa) ; houve corte beta
				(T beta)
			)
		))
	)
)
 
 
 
 (defun e-no-maxp (no)
	(let ((profundidade (get-no-profundidade no)))
		(cond
			((or (= profundidade 0) (evenp profundidade)) T)
			(T nil)
		)
	)
)
 
 (defun troca-peca (peca)
	(cond
		((= peca 1) 2)
		(T 1)
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
;;;;;;;;; (sucessores no operadores peca profundidade funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2))	;;retonra bem os sucessores
(defun no-folhap (no)
	; os valores, peça, profundidade, caixas-fechadas-j1 e caixas-fechadas-j2 são valores dummy neste caso, pq nao e necessário usar
	(let* ((sucessores-no (sucessores no (operadores) 1 0 'funcao-utilidade 0 0)) 
		  (numero-sucessores (length sucessores-no)))
		(cond
			((> numero-sucessores 0) nil)	;; se nao tiver sucessores ou a profundidade do no, for igual a profundidade maxima
			(T T)
		)
	)
)	

;; os nós folha finais são os que ainda têm sucessores
;; os nós folha são os que já não tem mais sucessores


; inteligência -> Ver artigos 
; caso seja utilizada uma função de utilidade de outra pessoa, esta deve ser mencionada no projecto
#|(defun funcao-utilidade (no peca caixas-fechadas-j1 caixas-fechadas-j2)
		(cond
		((= peca *jogador1*)(cond ((> caixas-fechadas-j1 caixas-fechadas-j2) 100) (T -100)))
		((= peca *jogador2*)(cond ((> caixas-fechadas-j2 caixas-fechadas-j1) 100) (T -100)))
		(T 0)
	)
)
|#

#|
; http://www.dcc.fc.up.pt/~INES/aulas/1112/SI/jogos.pdf
; http://www.cs.colostate.edu/~anderson/cs440/01fall/assignments/ttt.lisp
; http://people.eecs.berkeley.edu/~russell/code/search/algorithms/minimax.lisp
; http://www.sti-innsbruck.at/sites/default/files/Knowledge-Representation-Search-and-Rules/Russel-&-Norvig-Inference-and-Logic-Sections-6.pdf
|#
(defun funcao-utilidade (no peca caixas-fechadas-j1 caixas-fechadas-j2 &optional (numero-caixas 25))
"A utility function (also called an objective function or payoff function), which gives
a numeric value for the terminal states. In chess, the outcome is a win, loss, or draw,
with values +1, -1, or 0. Some games have a wider ,variety of possible outcomes; the
payoffs in backgammon range from +I92 to -192. This chapter deals mainly with
zero-sum games, although we will briefly mention non-zero-sum games. "
	(cond
		(
			(and
				(= peca *jogador1*)
				(and
					(= (vencedor-p numero-caixas peca caixas-fechadas-j1 caixas-fechadas-j2) *jogador1*) ;; verificar numero-caixas
					(equal (verificar-profundidade-jogador no) 'MAX)
				)
			)
		100
		)		
		(
			(and
				(= peca *jogador2*)
				(and
				(= (vencedor-p numero-caixas peca caixas-fechadas-j1 caixas-fechadas-j2) *jogador2*) ;; verificar numero-caixas
				(equal (verificar-profundidade-jogador no) 'MAX)
				)
			)
		(- 0 100)
		)
	(t 0)
	)
)


;verificar-jogador
(defun verificar-profundidade-jogador(no) "Função que verifica se o jogador encontra-se na profundidade de MAX ou MIN"
	(let ((profundidade (get-no-profundidade no)))
		(cond
			((or (evenp profundidade) (= profundidade 0)) 'MAX);;evenp returns true if integer is even (divisible by two); otherwise, returns false.
			(t 'MIN)
		)
	)
)
	
	
#|
;avaliar-folha
(defun avaliar-folha(no) "Retorna o valor [100] em caso de vitoria do jogador MAX, o valor [-100] em caso de vitoria do jogador MIN ou o valor [0] em caso de empate."
	(cond
		((equal (verificar-profundidade-jogador no) 'MAX) 100)
		(t -100)
	)
)
|#




;;; Sucessores

;; Tipo-jogador MAX e MIN
;; Simbolo-jogador 1 e 2

;;sucessores 
; (no operadores algoritmo-procura profundidade funcao-heuristica numero-objectivo-caixas)
;; 
;; Tenho que receber a peça pq é a peça a aplicar a inserir nas varias posicoes.

(defun sucessores-alfabeta (no operadores profundidade peca funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
	
	(let* ((numero-caixas-fechadas (caixas-fechadas (get-no-estado no)))
		   (sucessores_resultado (sucessores no operadores peca profundidade funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2))	;;retonra bem os sucessores
		   (novos-sucessores (apply 'append 	;; remove os nills da lista retornada
										(mapcar #'(lambda (node)
											(let ((fechou-caixa (verifica-se-fechou-caixa node numero-caixas-fechadas)))
													;(sucessores node operadores peca profundidade funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
													(cond
														((null fechou-caixa) (list node))
														;((null fechou-caixa) (list 1))
														;;Se chegar ao T, significa que o computador vai jogar outra vez!
														(T 											;;aqui verificar qual e a peça e incrementar conforme a peça!
														(sucessores node operadores peca (+ profundidade 1) funcao-utilidade (+ caixas-fechadas-j1 1) caixas-fechadas-j2)))
														;(list (length (sucessores node operadores peca (+ profundidade 1) funcao-utilidade (+ caixas-fechadas-j1 1) caixas-fechadas-j2)))))
											)) sucessores_resultado)) )
		 )
		novos-sucessores
	)
)

(defun verifica-se-fechou-caixa (no numero-caixas-fechadas-anterior)
	(let ((caixas-actualmente-fechadas (caixas-fechadas (get-no-estado no))))
		(> caixas-actualmente-fechadas numero-caixas-fechadas-anterior)
	)
)

(defun verifica-se-fechou-caixa (no numero-caixas-fechadas-anterior)
	(let ((caixas-actualmente-fechadas (caixas-fechadas (get-no-estado no))))
		(> caixas-actualmente-fechadas numero-caixas-fechadas-anterior)
	)
)

;;falta testar
(defun sucessores (no operadores peca profundidade-maxima funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
	(let* ((operador (car operadores))
		   (numero-linhas (numero-linhas-tabuleiro (get-no-estado no)))
		   (numero-colunas (numero-colunas-tabuleiro (get-no-estado no)))
		   (lista-linhas-colunas-possiveis (cond 
											((eql operador 'inserir-arco-horizontal) (reverse (lista-combinacoes (+ numero-linhas 1) numero-colunas)))
											((eql operador 'inserir-arco-vertical)   (reverse (lista-combinacoes (+ numero-colunas 1) numero-linhas))))))
		(cond
			((null operadores) nil)
			((= (get-no-profundidade no) profundidade-maxima) nil) ; não procura mais
			(T (append 
				(sucessores-todas-possibilidades no operador peca lista-linhas-colunas-possiveis funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
				(sucessores no (cdr operadores) peca profundidade-maxima funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
				))
		)
	)
)

;; falta testar
(defun sucessores-todas-possibilidades (no operador peca possibilidades funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
	(let* ((primeira-possibilidade (car possibilidades))
		   (possibilidades-validas (not (null possibilidades)))
		   (resultado (cond (possibilidades-validas (sucessores-aux no (list operador (append primeira-possibilidade (list peca))) peca funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)) (T nil)))
		   (resultado-avaliado (cond ((null resultado) nil) (T (list resultado)))))
		  
		(cond
			((null possibilidades) nil)
			
			(T (append resultado-avaliado (sucessores-todas-possibilidades no operador peca (cdr possibilidades) funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)))
		)
	)
)

;; falta testar 
(defun sucessores-aux (no lista-operador-parametros peca funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
	(let* ((operador (car lista-operador-parametros))
			(tabuleiro (get-no-estado no))
			(parametros (append (cadr lista-operador-parametros) (list tabuleiro)))
			(resultado-operacao (apply operador parametros))
			(resultado (cria-no resultado-operacao (+ 1 (get-no-profundidade no)) (funcall funcao-utilidade tabuleiro peca caixas-fechadas-j1 caixas-fechadas-j2) caixas-fechadas-j1 caixas-fechadas-j2 )))
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
