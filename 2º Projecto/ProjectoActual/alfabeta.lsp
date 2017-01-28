;;;; procura.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Implementação dos algoritmos de procura

;;; Variáveis Globais
(defvar *corte-alfa* 0)
(defvar *corte-beta* 0)
(defvar *jogada-pc* nil) ;;variavel que guarda o tabuleiro que corresponde a melhor jogada que é atualizada pelo algoritmo cada vez que o valor de beta é atualizado.


(defvar *no-escolhido-pc* nil)

(defvar *nos-analisados* 0) ;; variavel que guarda o numero de nos visitados
(defvar *tempo-despendido* 0)


;;Algoritmo alfa-beta
;; Old: (no profundidade-limite peca caixas-fechadas-j1 caixas-fechadas-j2 &optional (alfa 2) (beta 2) &aux (tempo-inicial (get-universal-time)))
;; Aplicar esta condição mo alfabeta ou nos sucessores? 
	;; ((and (equal 'dfs algoritmo-procura) (= (get-no-profundidade no) profundidade)) nil)

	
;; novo alfa-beta
;;Teste: (alfa-beta (no-teste-1-fecha-1-caixa) (get-no-profundidade (no-teste-1-fecha-1-caixa)) 1 'funcao-utilidade)	
;;Resul: 0
(defun alfa-beta (no profundidade-limite peca f-utilidade &optional (alfa -1000) (beta 1000)  (tempo-inicial (get-universal-time))(tempo-maximo 5000)) 	;; tempo de quando começou a 1ª procura no alfabeta
(format t "Entrei Alfa-beta~%")	
(format t "Intervalo : [~a, ~a] ~%" alfa beta)
	
	(let*(	
			;(peca-a-jogar (cond ((= (get-no-profundidade no) 0) peca) (T (troca-peca peca)))) ;;Troca de peça
			(max-mix (verificar-profundidade-jogador no)) ;;MAX ou MIN
			(caixas-jogador-1	(get-caixas-jogador-1 no))
			(caixas-jogador-2 	(get-caixas-jogador-2 no))
			(tempo-actual 		(get-universal-time))
			(tempo-gasto 		(- tempo-actual tempo-inicial))
			(tempo-dispendido	(setf *tempo-despendido* tempo-gasto))
			
			;(nos-analisados 	(setf *nos-analisados* (+ *nos-analisados* 1)))
		)
		;(format t "~%NOOO:~a~%"no)
		(cond
			(	(or
					(>= tempo-gasto tempo-maximo)
					;(no-folhap no) 	;; REVER
					(= profundidade-limite (get-no-profundidade no))	;; esta condicao fica no no-folhap
				)
				(progn
					(setf *nos-analisados* (+ *nos-analisados* 1))
					;(format t "Nó avaliado: ~a~%" no)
					;(format t "Caixas J.1: ~a~%" caixas-jogador-1)
					;(format t "Caixas J.2: ~a~%" caixas-jogador-2)
											
											;;DEIXEI DE RETORNAR O VALOR DA FUNCAO UTILIDADE calculado aqui, MAS VOU RETORNAR O VAlor da utilidaed ja calculado anteriormente!
											;(funcall f-utilidade no peca caixas-jogador-1 caixas-jogador-2)
										(get-no-utilidade no)
											
				;	(format t "~%NOS ANALISADOS~%~a~%"*nos-analisados*)
				;	(format t "~%TEMPO DISPENDIDO~%~a~%"tempo-dispendido)
				)	
			)

			(
				(eq max-mix 'MAX)
				(max-side (sucessores-alfabeta no (operadores) profundidade-limite peca f-utilidade caixas-jogador-1 caixas-jogador-2) profundidade-limite peca f-utilidade alfa beta tempo-inicial tempo-maximo)
			)
			(T
				(min-side (sucessores-alfabeta no (operadores) profundidade-limite peca f-utilidade caixas-jogador-1 caixas-jogador-2) profundidade-limite peca f-utilidade alfa beta tempo-inicial tempo-maximo)
			)
		)
	)
)

;;Função Alfa
(defun max-side (sucessores profundidade-limite peca f-utilidade alfa beta tempo-inicial tempo-maximo)
(format t "Entrei max-side~%")
(format t "Intervalo : [~a, ~a] ~%" alfa beta)
	(cond
		((null sucessores) alfa)
		(T (let* ((nova-peca (troca-peca peca))
				(valor-utilidade-no (alfa-beta (car sucessores) profundidade-limite nova-peca f-utilidade alfa beta tempo-inicial tempo-maximo))
				)
			(cond
				((> valor-utilidade-no alfa)	(progn
																	(format t "Nova jogada guardada: ~a~%" (car sucessores))
																	;(format t "Valor utilidade: ~a~%" valor-utilidade-no)
																	;(format t "Valor alfa: ~a~%~%" alfa)
																	(setf *jogada-pc* (car sucessores))	;; Actualiza a melhor jogada!
																	valor-utilidade-no
												))
				;(format t "~%~a~~%"*jogada-pc*)
				((>= alfa beta) (setf *corte-beta* (+ *corte-beta* 1)) beta) ; houve corte alfa
				(T (max-side (cdr sucessores) profundidade-limite peca f-utilidade valor-utilidade-no beta tempo-inicial tempo-maximo))
			)
		))
	)
)



;;Função Beta
(defun min-side (sucessores profundidade-limite peca f-utilidade alfa beta tempo-inicial tempo-maximo)
(format t "Entrei min-side~%")
(format t "Intervalo : [~a, ~a] ~%" alfa beta)
	(cond
		((null sucessores) beta)
		(T (let* ((nova-peca (troca-peca peca))
				(valor-utilidade-no (alfa-beta (car sucessores) profundidade-limite nova-peca f-utilidade alfa beta tempo-inicial tempo-maximo))
				)
			(cond
				((< valor-utilidade-no beta) valor-utilidade-no)
				((<= beta alfa) (setf *corte-alfa* (+ *corte-alfa* 1)) alfa) ; houve corte beta
				(T (min-side (cdr sucessores) profundidade-limite peca f-utilidade alfa beta tempo-inicial tempo-maximo))
			)
		))
	)
)


#||
;;Teste: (alfa-beta (no-teste-1-fecha-1-caixa) (get-no-profundidade (no-teste-1-fecha-1-caixa)) 1 'funcao-utilidade)
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
			(format t "Jogada-pc: ~a"  *jogada-pc*)
		(cond
			(	(or  ;(>= tempo-gasto tempo-maximo) 
					(no-folhap no) 	;; REVER QUE ESTA MERDA TA A FALHAR
					(= profundidade-limite (get-no-profundidade no))) 
					;;(setf *nos-analisados* (+ *nos-analisados* 1))
						(print "1")
					(funcall f-utilidade no peca caixas-jogador-1 caixas-jogador-2))
				
			(no-max (print "2")(max-side (sucessores-alfabeta no (operadores) profundidade-limite peca-a-jogar f-utilidade caixas-jogador-1 caixas-jogador-2) profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo))		
			((not no-max) (print "3")(min-side (sucessores-alfabeta no (operadores) profundidade-limite peca-a-jogar f-utilidade caixas-jogador-1 caixas-jogador-2) profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo))
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
				
				;Old: ((> valor-utilidade-no alfa) (max-side (cdr sucessores) profundidade-limite peca f-utilidade valor-utilidade-no beta tempo-inicial tempo-maximo))
				
				((> valor-utilidade-no alfa); (progn
												(setf *jogada-pc* (car sucessores))		;; Actualiza a melhor jogada!
												;(max-side (cdr sucessores) profundidade-limite peca f-utilidade valor-utilidade-no beta tempo-inicial tempo-maximo))
												)
				((>= alfa beta) (setf *corte-beta* (+ *corte-beta* 1)) beta) ; houve corte alfa
				;(T alfa)
				(T (max-side (cdr sucessores) profundidade-limite peca f-utilidade valor-utilidade-no beta tempo-inicial tempo-maximo))
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
||#

#||
(defun f-min (sucessores profundidade-max f-utilidade f-sucessores lista-operadores simbolo caixas-jogador1 caixas-jogador2 alfa beta)
	(let ((value-linha (alfabeta (car sucessores) (1- profundidade-max) f-utilidade f-sucessores lista-operadores (trocar-peca simbolo) 1 caixas-jogador1 caixas-jogador2 alfa beta)))
		(cond
			((null (cdr sucessores)) value-linha)
			((<= value-linha alfa) (progn (setf *numero-cortes-alfa* (1+ *numero-cortes-alfa*)) alfa))
			(T (f-min (cdr sucessores) profundidade-max f-utilidade f-sucessores lista-operadores simbolo caixas-jogador1 caixas-jogador2 alfa value-linha))
		)
	)
)
||#
 
 
 ;; ESTAS DUAS FUNÇÕES JA EXISTEM. ELIMINAR!!!! (trocar-peca) e (verificar-profundidade-jogador) <------------------------------------
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
		((= peca *jogador1*) *jogador2*)
		(T *jogador1*)
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
;;PROFUNDIDADE NO = PROF.MAX
(defun no-folhap (no)
	(let* (
			;(sucessores-no (sucessores no (operadores) 1 0 'funcao-utilidade 0 0))
			(sucessores-no (sucessores no (operadores) 1 (get-no-profundidade no) 'funcao-utilidade (get-caixas-jogador-1 no) (get-caixas-jogador-1 no)))
			(numero-sucessores (length sucessores-no))
)
		(cond
			
			((> numero-sucessores 0) nil)	;; se nao tiver sucessores ou a profundidade do no, for igual a profundidade maxima
			(T T)
		)
	)
)	




;; NOVA FUNÇÃO
(defun funcao-utilidade (no peca caixas-fechadas-j1 caixas-fechadas-j2 old-numero-caixas-j1 old-numero-caixas-j2)
	(let* ((numero-caixas-fechadas (caixas-fechadas (get-no-estado no)))
			 (vencedor-resultado (vencedor-p numero-caixas-fechadas peca caixas-fechadas-j1 caixas-fechadas-j2)))

			 
		(cond
			(vencedor-resultado (cond ((= vencedor-resultado *jogador2*) 2000) (T -2000)))	;; se o PC ganhar -> 2000 ; se for o Humano -> -2000
			(T 
				;(j1-fechou-caixa ((cond ((> caixas
				
				(cond
					((> caixas-fechadas-j2 old-numero-caixas-j2) 50)	  ; PC fechou uma caixa 
					((> caixas-fechadas-j1 old-numero-caixas-j1) -50)  ; Humano fechou uma caixa 
					(T 0)	;; ninguem fechou caixa
				)
			)
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
#||
;;Dando mais valor a medida que se fecha caixas. Aqui sei essa diferença através do num. de caixas-j1 e caixas-j2
;VERIFICAR SE O NO ACABA O JOGO, SE ACABAR DA 500
(defun funcao-utilidade (no peca caixas-fechadas-j1 caixas-fechadas-j2)
"A utility function (also called an objective function or payoff function), which gives
a numeric value for the terminal states. In chess, the outcome is a win, loss, or draw,
with values +1, -1, or 0. Some games have a wider ,variety of possible outcomes; the
payoffs in backgammon range from +I92 to -192. This chapter deals mainly with
zero-sum games, although we will briefly mention non-zero-sum games. "
	(let* (	
		(numero-caixas-fechadas (caixas-fechadas (get-no-estado no)))
		(vencedor-resultado (vencedor-p numero-caixas-fechadas peca caixas-fechadas-j1 caixas-fechadas-j2)))
		(progn
		;(format t "~%caixa fechadas ~%~A~%" numero-caixas-fechadas)
		;(format t "~%vencedor F-U: ~%~A~%" vencedor-resultado) 
			(cond
				((null vencedor-resultado) (cond
															((> caixas-fechadas-j2 caixas-fechadas-j1) 50)
															((= caixas-fechadas-j2 caixas-fechadas-j1) 0)
															(t -50)))
				((and (= vencedor-resultado *jogador1*) (equal (verificar-profundidade-jogador no) 'MAX)) 100)
				((and (= vencedor-resultado *jogador2*) (equal (verificar-profundidade-jogador no) 'MAX)) -100)
				(T 0))))
)
||#




				
			#|
				(and	
					(= peca *jogador1*)
					(and
						(not (null vencedor-resultado))
						(= vencedor-resultado *jogador1*)
						(equal (verificar-profundidade-jogador no) 'MAX)
						;(eql (vencedor-p numero-caixas-fechadas peca caixas-fechadas-j1 caixas-fechadas-j2) *jogador1*) ;; verificar numero-caixas
						;(equal (verificar-profundidade-jogador no) 'MAX)
					)
				)
			100
			)
			
			(
				(and
					(= peca *jogador2*)
					(and 
							(not (null vencedor-resultado))
							(= vencedor-resultado *jogador2*)
							(equal (verificar-profundidade-jogador no) 'MAX)

					)
				)
			-100
			)
			
		; (t 0)
		(t
			(cond
				( (> caixas-fechadas-j1 caixas-fechadas-j2) 50)
				(t
					-50
				)
			
			)
		)
		)
	)
)
)
;
|#
#|
(defun funcao-utilidade (no peca caixas-fechadas-j1 caixas-fechadas-j2)
"A utility function (also called an objective function or payoff function), which gives
a numeric value for the terminal states. In chess, the outcome is a win, loss, or draw,
with values +1, -1, or 0. Some games have a wider ,variety of possible outcomes; the
payoffs in backgammon range from +I92 to -192. This chapter deals mainly with
zero-sum games, although we will briefly mention non-zero-sum games. "
	(let* ((numero-caixas-fechadas (caixas-fechadas (get-no-estado no)))
		(vencedor-resultado (vencedor-p numero-caixas-fechadas peca caixas-fechadas-j1 caixas-fechadas-j2)))
		
		
	)
)
||#

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
;Teste: (sucessores-alfabeta (no-teste-1) (operadores) 1 1 'funcao-utilidade 0 0)

(defun sucessores-alfabeta (no operadores profundidade peca funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
(format t "Entrei sucessores-alfabeta~%")	
	(let* ((numero-caixas-fechadas (caixas-fechadas (get-no-estado no)))
		     (sucessores_resultado (sucessores no operadores peca profundidade funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2))	;;retonra bem os sucessores
		     (novos-sucessores (apply 'append 	;; remove os nills da lista retornada
										(mapcar #'(lambda (node)
											(let ((fechou-caixa (verifica-se-fechou-caixa node numero-caixas-fechadas))
													(caixas-fechadas-jogador-1 (cond ((= peca *jogador1*) (+ caixas-fechadas-j1 1)) (T caixas-fechadas-j1)))
													(caixas-fechadas-jogador-2 (cond ((= peca *jogador2*) (+ caixas-fechadas-j2 1)) (T caixas-fechadas-j2))))
													
												;	(format t "~%~% Sucessor com sucessores:")
												;	(format t "Fechou caixas: ~a~%" fechou-caixa)
												
													;(format t "~%~% Node:~a" node)
													;(format t "~%~% Node:~a" fechou-caixa)
													
													(cond
														((null fechou-caixa) (list node))

														;;Se chegar ao T, significa que o computador vai jogar outra vez!
														#|| Aqui ir buscar o valor utilidade do 'node', se for == 1000, significa que o jogo acabou, logo nao e preciso ir gerar sucessores?
																Pq, se nao tiver algo do género, o que vai acontecer é que vou gerar sucessores quando o tab. estiver cheio.
																
														||#
														(T 											;;aqui verificar qual e a peça e incrementar conforme a peça!
														;; actual
														;;;;;;;(sucessores node operadores peca (+ profundidade 1) funcao-utilidade (+ caixas-fechadas-j1 1) caixas-fechadas-j2)))
															(let ((new-sucessores (sucessores node operadores peca (+ profundidade 1) funcao-utilidade caixas-fechadas-jogador-1 caixas-fechadas-jogador-2)))
																(cond
																	((null new-sucessores) (list node))
																	(T new-sucessores)
																)
															)))
											)) sucessores_resultado)) )
		 )
		novos-sucessores
		
		;sucessores_resultado
	)
)
;

(defun verifica-se-fechou-caixa (no numero-caixas-fechadas-anterior)
	(let ((caixas-actualmente-fechadas (caixas-fechadas (get-no-estado no))))
		(> caixas-actualmente-fechadas numero-caixas-fechadas-anterior)
	)
)
#||
(defun verifica-se-fechou-caixa (no numero-caixas-fechadas-anterior)
	(let ((caixas-actualmente-fechadas (caixas-fechadas (get-no-estado no))))
		(> caixas-actualmente-fechadas numero-caixas-fechadas-anterior)
	)
)||#

;;falta testar
(defun sucessores (no operadores peca profundidade-maxima funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
(format t "Entrei sucessores~%")

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
;(format t "Entrei sucessores-todas-possibilidades~%")

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
;(format t "Entrei sucessores-aux~%")

	(let* ((operador (car lista-operador-parametros))
			(tabuleiro-pai (get-no-estado no))
			(parametros (append (cadr lista-operador-parametros) (list tabuleiro-pai)))
			(tabuleiro-gerado (apply operador parametros))
			
			
			(old-numero-caixas-j1 (get-caixas-jogador-1 no))
			(old-numero-caixas-j2 (get-caixas-jogador-2 no)))
			;(numero-caixas-fechadas-tab-pai (caixas-fechadas tabuleiro-pai))	-> chamado na funcao novo-numero-caixas
			
			;(numero-caixas-fechadas (caixas-fechadas (get-no-estado no)))
			;(ta (format t "~%Resultado: ~a"resultado-operacao))
			;(tua (format t "~%Peca: ~a"peca))
			;(novo-numero-caixas-fechadas (caixas-fechadas resultado-operacao))
			;(num-caixas-fechadas-j1 (- novo-numero-caixas-fechadas caixas-fechadas-j2))
			; (num-caixas-fechadas-j2 (- novo-numero-caixas-fechadascaixas-fechadas-j1))
																										;;Aqui no funcall nao é 'no', é 'resultado-operacao', e nao é peça é troca peça
;(resultado (cria-no resultado-operacao (+ 1 (get-no-profundidade no)) (funcall funcao-utilidade no peca caixas-fechadas-j1 caixas-fechadas-j2) caixas-fechadas-j1 caixas-fechadas-j2 )))
		
		;(format t "~%Num caixas fechadas tab pai: ~a" numero-caixas-fechadas-tab-pai)
		;(format t "Resultado Operação:~a" tabuleiro-gerado)
		
		(cond
			((null tabuleiro-gerado) nil)
			;(T resultado)
			(T (let* ((numero-caixas-fechadas-tab-gerado (caixas-fechadas tabuleiro-gerado))
			
					;;MUDAR ESTA PARTE DAS CAIXAS, QUE ESTA MAL, se a peça for 1, as peças do 2 nao muda! se a peça for 2 as peças 1 nao mudam!
						(numero-caixas-jogador-1 (cond ((= peca *jogador1*) (- numero-caixas-fechadas-tab-gerado caixas-fechadas-j2)) (T caixas-fechadas-j1)))
						(numero-caixas-jogador-2 (cond ((= peca *jogador2*) (- numero-caixas-fechadas-tab-gerado caixas-fechadas-j1)) (T caixas-fechadas-j2)))
						;(numero-caixas-jogador-1 (- numero-caixas-fechadas-tab-gerado caixas-fechadas-j2))
						;(numero-caixas-jogador-2 (- numero-caixas-fechadas-tab-gerado caixas-fechadas-j1))
						
					;;MUDAR
						(profundidade  (+ 1 (get-no-profundidade no)))
						(valor-utilidade (funcall funcao-utilidade no peca numero-caixas-jogador-1 numero-caixas-jogador-2 old-numero-caixas-j1 old-numero-caixas-j2))
						)
			
	;(format t "~%~%SUCESSORES-AUX~%~%")
		#||(format t "~%Profundidade: ~a" profundidade)||#

#||		
	(format t "~%~%~%~%~%~%JOGADA:~%")
	(format t "TAB Gerado: ~a~%" tabuleiro-gerado)
	(imprime-tabuleiro tabuleiro-gerado)

	(format t "Valor utilidade: ~a~%" valor-utilidade)
	(format t "Antigo num. caixas jogador 1: ~a~%" caixas-fechadas-j1)
	(format t "Antigo num. caixas jogador 2: ~a~%" caixas-fechadas-j2)
	(format t "Novo num. caixas jogador 1: ~a~%" numero-caixas-jogador-1)
	(format t "Novo num. caixas jogador 2: ~a~%~%" numero-caixas-jogador-2)
	;(imprime-tabuleiro tabuleiro-gerado)
	;(format t "fim tab")
	||#
				(cria-no tabuleiro-gerado
											 profundidade
											 valor-utilidade
											 numero-caixas-jogador-1 
											 numero-caixas-jogador-2)))
		)
	)
)
#||
(defun novo-numero-caixas (tabuleiro-pai tabuleiro-gerado)
	(let ((numero-caixas-fechadas-tab-pai (caixas-fechadas tabuleiro-pai))
			(numero-caixas-fechadas-tab-gerado (caixas-fechadas tabuleiro-gerado)))
		
		(cond
			((null tabuleiro-gerado) (list 0 0))	; list -> (numero-caixas-j1 numero-caixas-j2)	-> será de um sucessor não válido, tabuleiro a NIL
			(T (let (
		)
	)
)
||#

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
