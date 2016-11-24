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

Procura genérica
	(defun procura-generica (no-inicial prof-max f-solucao f-sucessores f-algoritmo lista-operadores &optional (abertos (list no-inicial)) (fechados nil))
Procuras concrectas


Sucessores
	(defun sucessores (no operadores algoritmo-procura profundidade) ... ) 	-> Funciona para os algoritmos 'bfs' e 'dfs'

Sucessores-aux

solução

(defun existep (no lista-nos algoritmo) ... )

(defun existe-solucao (lista f-solucao f-algoritmo) ...)	



Passos:		(Até Sábado)
	1. Desenvolver sucessores-aux
	2. Desenvolver sucessores
	3. Desenvolver numero-caixas-fechadas
	4. Desenvolver solucaop
	5. Implementar A*
	6. Adaptar/Transferir procura genérica para aplicar o A*
	7. Testar
	
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


;;; Sucessores
#|| 
OLD

(defun sucessores (no operadores algoritmo-procura profundidade)
	(cond
		((and (equal 'dfs algoritmo-procura) (= (get-profundidade-no no) profundidade)) nil)
		((not (null operadores)) (cons (sucessores-aux no (car operadores)) (sucessores no (cdr operadores) algoritmo-procura profundidade)))
	)
)


||#

;; sucessores-aux
;; Teste: (sucessores-aux (no-teste) 'inserir-arco-horizontal)			
;;					-> Tabuleiro do no-teste: (((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))'((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)))
;;
;;		A CONFIRMAR
;; Resultado: (
;;					(((T T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)))
;;					(((NIL T T) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)))
;;					(((NIL T NIL) (T T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)))
;;					(((NIL T NIL) (NIL T T) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)))
;;					(((NIL T NIL) (NIL T NIL) (T NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)))
;;					(((NIL T NIL) (NIL T NIL) (NIL T NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)))
;;					(((NIL T NIL) (NIL T NIL) (NIL NIL T) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)))
;;					(((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (T NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)))
;;					(((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL T NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)))
;;					(((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL T)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)))
;;					)
#||

OLD
(defun sucessores-aux (no operador) 
	(list (funcall operador (get-vasilhas-no no)) (+ 1 (get-profundidade-no no)) no)
)


||#

#||
(defun sucessores (no operadores algoritmo-procura profundidade)
	(cond
		((and (equal 'dfs algoritmo-procura) (= (get-profundidade-no no) profundidade)) nil)
		((not (null operadores)) (cons (sucessores-aux no (car operadores)) (sucessores no (cdr operadores) algoritmo-procura profundidade)))
	)
)
||#

#||
COLOCAR NO FICHEIRO Projecto.lisp

||#
(defun escreve-no (no) "Permite escrever no ecra um no do problema."
	(progn
		(format t (concatenate 'string "~%|>No: " (write-to-string no)))
	)
)
#||
(defun escreve-lista-nos(lista) "Permite escrever no ecra uma lista de nos do problema das vasilhas, e.g. um conjunto de sucessores, a lista de abertos etc."
	(cond
		((null lista) nil)
		(T (progn 
				(escreve-no (car lista)) 
				(escreve-lista-nos-aux (cdr lista))
			)
		)
	)
)||#


(defun escreve-lista-nos (lista) "Permite escrever no ecra uma lista de nos do problema das vasilhas, e.g. um conjunto de sucessores, a lista de abertos etc."
	(cond
		((null lista) nil)
		(T (progn 
				(escreve-lista-nos-aux (car lista)) 	; horizontais
				(escreve-lista-nos-aux (cadr lista)) ; verticais
			)
		)
	)
)

;; Este tem que escrever os nos horizontais e os verticais
(defun escreve-lista-nos-aux (lista) "Permite escrever no ecra uma lista de nos do problema das vasilhas, e.g. um conjunto de sucessores, a lista de abertos etc."
	(cond
		((null lista) nil)
		(T (progn 
				(escreve-no (car lista)) 
				(escreve-lista-nos-aux (cdr lista))
			)
		)
	)
)



;;TODO
;;
;;
;; SÓ FALTA RECEBER DIFERENTES VALORES PARA O Nº DE LINHAS E COLUNAS   de cada lista de arco horizontais e verticais
;;
;;
;;	>>>>>>>>>>>>>>>>DÚVIDA<<<<<<<<<<<<<<<<<<<

;; (sucessores (no-teste) (operadores) 'bfs 0)
;; Teste: (escreve-lista-nos (sucessores (no-teste) (operadores) 'bfs 0))
#||
RESULTADO:			EXISTE UM ERRO COM ESTA GERAÇAO, APÓS PASSAR DE UMA LINHA PARA A OUTRA É ADICIONADO O TABULEIRO PAI, O QUE NAO FAZ SENTIDO, PRECISA DE SER REMOVIDO! / NAO GERADO


	|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL T))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL T NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (T NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL T) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL T NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (T NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T T) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (T T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T T) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((T T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL T)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL T NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (T NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL T) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL T NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (T NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T T) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (T T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T T) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))
|>No: ((((T T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1 (((# # # #) (# # # #)) 0 0 NIL))

||#

(defun sucessores (no operadores algoritmo-procura profundidade)
	(let ;;((lista-linhas-colunas-possiveis (cond ((eql (car operadores) 'inserir-arco-horizontal) ()))(lista-combinacoes 4 3)))
		   ((lista-linhas-colunas-possiveis (lista-combinacoes (get-dimensao-linhas (get-no-estado no)) (get-dimensao-colunas (get-no-estado no)))))
		#||
		((lista-linhas-colunas-possiveis (lista-combinacoes 4 3)))	;;;; se inserir-arco-horizontal entre 1 e (get-dimensao-linhas tabuleiro) -> ja devolve n + 1
																							;;;; se inserir-arco-vertical entre 1 e (get-dimensao-colunas tabuleiro) -> ja devolve m + 1
	||#
		(cond
			((and (equal 'dfs algoritmo-procura) (= (get-profundidade-no no) profundidade)) nil)
			((not (null operadores)) (cons (sucessores-todas-possibilidades no (car operadores) lista-linhas-colunas-possiveis) (sucessores no (cdr operadores) algoritmo-procura profundidade)))
		)
	)
)



#||

ACHO QUE NAO ESTA A DEVOLVER OS TABULEIROS COM OS NOVOS ARCOS	

||#
;; (sucessores-todas-possibilidades (no-teste) 'inserir-arco-horizontal (lista-combinacoes (get-dimensao-linhas (get-no-estado (no-teste))) (get-dimensao-colunas (get-no-estado (no-teste)))))
(defun sucessores-todas-possibilidades (no operador possibilidades)
	(cond
		((null possibilidades) nil)
		(T (cons (sucessores-aux no (list operador (car possibilidades))) (sucessores-todas-possibilidades no operador (cdr possibilidades))))
	)
)
#||

	Tenho 2 operadores, por cada arco tenho várias possibilidades:
	
		- inserir-arco-horizontal
			- inserir-arco-horizontal 1 1 tabuleiro
			- inserir-arco-horizontal 1 2 tabuleiro
			- inserir-arco-horizontal 1 3 tabuleiro
			- inserir-arco-horizontal 2 1 tabuleiro
			
		- inserir-arco-vertical
			- inserir-arco-vertical 1 1 tabuleiro
			- inserir-arco-vertical 1 2 tabuleiro
			- inserir-arco-vertical 1 3 tabuleiro
			- inserir-arco-vertical 2 1 tabuleiro
||#

;; to be used
;; (sucessores-aux (no-teste) '(inserir-arco-horizontal (1 1)))

;; Tabuleiro que está no nó (tab3) : 
;;						'((NIL T	 NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))
;;						'((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))
;;
;; Resultado: 	((((T T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 1
;;							((((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))) 0 0 NIL))

;;
(defun sucessores-aux (no lista-operador-parametros)
	(let* ((operador (car lista-operador-parametros))
			(tabuleiro (get-no-estado no))
			(parametros (append (cadr lista-operador-parametros) (list tabuleiro))))

		;; old
		;(apply operador parametros)
		;;(list (apply operador parametros) (+ 1 (get-no-profundidade no)) #|| no ||# "PAI")
		;;;; DEVOLVE-ME VALORES COM ###### -> rEINICIAR O lisp works
		
		;;CORRECTO
		;(list (apply operador parametros) (+ 1 (get-no-profundidade no)) no)
		(list (cadr lista-operador-parametros) (apply operador parametros) (+ 1 (get-no-profundidade no)) no)
	)
)
;;,TESTE: (sucessores-todas-possibilidades (no-teste) 'inserir-arco-horizontal (lista-combinacoes (get-dimensao-linhas (get-no-estado (no-teste))) (get-dimensao-colunas (get-no-estado (no-teste)))))








;(inserir-arco-horizontal 3 3 (tabuleiro-teste1))

;; lista-combinacoes
;; Teste: (lista-combinacoes  4 3)   -> Resultado: ((4 3) (4 2) (4 1) (3 3) (3 2) (3 1) (2 3) (2 2) (2 1) (1 3) (1 2) (1 1))
;; Teste: (reverse (lista-combinacoes 4 3)) -> Resultado: ((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3) (4 1) (4 2) (4 3))
(defun lista-combinacoes (maximo-linhas maximo-colunas) "Recebe uma lista e retorna um conjunto de listas que representa todas as combinacoes possiveis"
	(cond
		((zerop maximo-linhas) nil)
		(T (append (combinacoes-numero-lista maximo-linhas (criar-lista-numeros maximo-colunas)) (lista-combinacoes (- maximo-linhas 1) maximo-colunas)))
	)
)
;

;; criar-lista
;; Teste: (criar-lista-numeros 5)   -> Resultado: (1 2 3 4 5)
(defun criar-lista-numeros (tamanho &optional (valor-por-omissao 1)) "Devolve uma lista com o valor por defeito [NIL] se não for indicado qual o valor a preencher a lista. O tamanho da lista é passado como argumento"
	(cond
		((zerop tamanho) nil)
		(T (cons valor-por-omissao (criar-lista-numeros (- tamanho 1) (+ valor-por-omissao 1))))
	)
)
;

;; função auxiliar
;; combinacoes
;; Teste: (combinacoes 4 '(1 2 3))   -> Resultado: ((4 3) (4 2) (4 1))
(defun combinacoes-numero-lista (numero lista) 
	(let ((ultimo-elemento (car (last lista)))) 
		
		(cond
			((null lista) nil)
			(T (cons (list numero ultimo-elemento) (combinacoes-numero-lista numero (reverse (cdr (reverse lista))))))
		)
	)
)
;

#||

	Dúvida Sucessores:
	
		- (defun inserir-arco-horizontal (linha coluna tabuleiro) 
			-> Quais são realmente os sucessores do meu nó teste?
			
				- Primeiro aplico o operador, 'inserir-arco-horizontal' 
					-> As funções que estão no operador têm que verificar se já existe algo na posicao -> Ou se é possível adicionar.
					
				- Segundo aplico o operador, 'inserir-arco-vertical'

				-> Esperado: 
					-> A função sucessores-aux retornará uma lista com todas possibilidades aplicando o 1º operador.
						-> Para isso tenho que ter a linha e a coluna.
						-> Tenho que passar como argumento à função operador a LINHA e a COLUNA??? -> Problema
						
					-> Função sucessores retornará todas as possibilidades, quando aplicado os 2 operadores.
			
	Resultado esperado ao aplicar o operador '
			(defun tabuleiro-teste3 () "Retorna um tabuleiro com 1 caixa fechada de dimensão 3 x 3"
	(list '((NIL T	 NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))
			'((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))
	)
)
||#

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


 