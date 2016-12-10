;;;; puzzle.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Funções do domínio do problema



;;; Construtor

;; cria-no

(defun cria-no (estado &optional (profundidade 0) (heuristica nil) (no-pai nil)) 
"Cria uma lista que representa um nó; Um nó é composto pelo estado que é o tabuleiro, este é um parâmetro obrigatório, é composto também por outros parâmetros, como
a profundidade a que se encontra, pela heurística deste mesmo nó e pelo nó pai, ou seja, o nó que o gerou. A profundidade e a heurística por omissão têm valor nil, enquanto que o nó pai por defeito é NIL"

	(list estado profundidade heuristica no-pai)
)

;;; SUBSTITUIR PELA DE CIMA, esta (a de baixo) funciona apenas para a procura bfs e dfs
#||
(defun cria-no (estado &optional (profundidade 0) (no-pai nil)) 
"Cria uma lista que representa um nó; Um nó é composto pelo estado que é o tabuleiro, este é um parâmetro obrigatório, é composto também por outros parâmetros, como
a profundidade a que se encontra, pela heurística deste mesmo nó e pelo nó pai, ou seja, o nó que o gerou. A profundidade e a heuróistica por defeito têm valor 0, enquanto que o nó pai por defeito é NIL"

	(list estado profundidade no-pai)
)
||#

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



;; Função do calculo do custo
;; custo
(defun custo (no)"retorna o valor do custo do nó (f). Soma do valor da profundidade com o valor heuristico."
    (+ (get-no-profundidade no)(get-no-heuristica no))
)
;

;;; Métodos de Consulta

;; get-arcos-horizontais
;; Teste: (get-arcos-horizontais (tabuleiro-teste1))   ->   ((NIL T T) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)) 
(defun get-arcos-horizontais (tabuleiro) "Retorna a lista dos arcos horizontais de um tabuleiro"
	(car tabuleiro)
)

;; get-arcos-verticais
;; Teste: (get-arcos-verticais (tabuleiro-teste1))   ->   ((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL))
(defun get-arcos-verticais (tabuleiro) "Retorna a lista dos arcos verticiais de um tabuleiro"
	(car (cdr tabuleiro))
)


;;; Funcões auxiliares dos operadores

;; inserir-arco-na-posicao
;; Teste: (inserir-arco-na-posicao 2 '(T nil T T))   ->   (T T T T)
(defun inserir-arco-na-posicao (indice lista) "Insere um arco (representado pelo valor [T]) no índice da lista recebida"
	(cond
		((null lista) nil)
		((= indice 1) (cons T (cdr lista)))
		(T (cons (car lista) (inserir-arco-na-posicao (- indice 1) (cdr lista))))
	)
)

;; inserir-arco-na-posicao-aux
;; Teste: (inserir-arco-na-posicao-aux 3 3 (get-arcos-verticais (tabuleiro-teste1)))   ->   ((NIL NIL NIL) (T T T) (NIL NIL T) (NIL T NIL))
(defun inserir-arco-na-posicao-aux (linha coluna lista) "Insere um arco (representado pelo valor [T]) numa lista que representa o conjunto de arcos dum tabuleiro. A posição representada pela linha e a coluna de destino são valores inteiros passados como argumentos"
	(cond
		((null lista) nil)
		((= linha 1) (cons (inserir-arco-na-posicao coluna (car lista)) (cdr lista)))
		(T (cons (car lista) (inserir-arco-na-posicao-aux (- linha 1) coluna (cdr lista))))
	)
)


;;; Operadores

;; arco-vertical
;; Teste: (inserir-arco-vertical 3 3 (tabuleiro-teste1))   ->   (((NIL T T) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL NIL NIL) (T T T) (NIL NIL T) (NIL T NIL)))
(defun inserir-arco-vertical (linha coluna tabuleiro) "Insere um arco vertical (representado pelo valor [T]) num tabuleiro passado como argumento"
	(let ((arcos-horizontais (get-arcos-horizontais tabuleiro))
		  (arcos-verticais (get-arcos-verticais tabuleiro)))
	
		(cond
			((or (null tabuleiro) (not (possivel-adicionar-arco linha coluna arcos-verticais))) nil)
			(T (cons arcos-horizontais (list (inserir-arco-na-posicao-aux linha coluna arcos-verticais))))
		)
	)
)

;; arco-horizontal
;; Teste: (inserir-arco-horizontal 3 3 (tabuleiro-teste1))   ->   (((NIL T T) (NIL NIL NIL) (NIL NIL T) (NIL NIL NIL)) ((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL)))
(defun inserir-arco-horizontal (linha coluna tabuleiro) "Insere um arco horizontal (representado pelo valor [T]) num tabuleiro passado como argumento"
	(let ((arcos-horizontais (get-arcos-horizontais tabuleiro))
		  (arcos-verticais (get-arcos-verticais tabuleiro)))

		(cond
			((or (null tabuleiro) (not (possivel-adicionar-arco linha coluna arcos-horizontais))) nil)
			(T (cons (inserir-arco-na-posicao-aux linha coluna arcos-horizontais) (list arcos-verticais)))
		)
	)
)		


;; possivel-adicionar-arco
;; Teste: (possivel-adicionar-arco 1 1 (get-arcos-horizontais (tabuleiro-teste1)))   -> Resultado:   T
;; Teste: (possivel-adicionar-arco 1 3 (get-arcos-horizontais (tabuleiro-teste1)))   -> Resultado:   NIL
(defun possivel-adicionar-arco (linha coluna lista) "Recebe indices de linha e coluna e uma lista de arcos horizontais ou de arcos verticais e verifica se naquela posição o valor é [T], se for devolve [NIL], se for [NIL] devolve [T]"
	(cond
		((null lista) nil)
		((= linha 1) (possivel-adicionar-arco-aux coluna (car lista)))
		(T (possivel-adicionar-arco (- linha 1) coluna (cdr lista)))
	)
)


;; possivel-adicionar-arco-aux 
;; Teste: (possivel-adicionar-arco-aux 3 '(NIL T NIL NIL))   -> Resultado: T
(defun possivel-adicionar-arco-aux (indice lista)
	(cond
		((null lista) nil)
		((and (= indice 1) (eql (car lista) nil)) T)
		(T (possivel-adicionar-arco-aux (- indice 1) (cdr lista)))
	)
)



;; operadores 
(defun operadores () "Cria uma lista com todos os operadores do problema dos Pontos e das Caixas"
	(list 'inserir-arco-vertical 'inserir-arco-horizontal)
)


;; numero-linhas-tabuleiro
;; ATENÇÃO: Esta função devolve o número de linhas entre os pontos. Para devolver o número exacto de linhas é preciso somar 1 ao resultado da função."
(defun numero-linhas-tabuleiro (tabuleiro) "Retorna o número de linhas horizontais de um tabuleiro"
	(get-dimensao-aux (car (get-arcos-verticais tabuleiro)))
)


;; numero-colunas-tabuleiro
;; ATENÇÃO: Esta função devolve o número de colunas entre os pontos. Para devolver o número exacto de colunas é preciso somar 1 ao resultado da função."
(defun numero-colunas-tabuleiro (tabuleiro) "Retorna o número de linhas verticais de um tabuleiro"
	(get-dimensao-aux (car (get-arcos-horizontais tabuleiro)))
)

;; get-dimensao-aux
;; Teste: (get-dimensao-aux (get-arcos-horizontais (tabuleiro-teste1)))   -> Resultado: 4
(defun get-dimensao-aux (lista) "Dada uma lista de arcos devolve o número de listas existentes"
	(cond
		((null lista) 0)
		(T (+ 1 (get-dimensao-aux (cdr lista))))
	)
)



#||

	numero-caixas-fechadas 
||#


;; Teste : (caixas-fechadas (tabuleiro-teste))
;; Resultado: 0
;;caixas-fechadas
(defun caixas-fechadas (tabuleiro) "retorna o numero de caixas fechadas de um tabuleiro."
	(cond
		((null tabuleiro) nil)
		(t
			(contar-objetivo (caixas-fechadas-aux (get-arcos-horizontais tabuleiro)(get-cabecas-por-coluna tabuleiro)))
		)
	)
)


;;Teste: (caixas-fechadas-aux (get-arcos-horizontais (tabuleiro-teste)) (get-arcos-verticais (tabuleiro-teste)))
;;Resultado: ((NIL NIL NIL NIL) (T NIL NIL NIL) (T NIL NIL NIL) (NIL NIL T T) (NIL NIL T T) (NIL NIL T NIL) (NIL NIL NIL NIL) (NIL NIL NIL NIL) (NIL NIL NIL NIL))
;; Esta função vai buscar os arcos horizontais e verticais e aplica uma função auxiliar de modo a juntar todas as caixas numa só lista.
;;caixas-fechadas-aux
(defun caixas-fechadas-aux (horizontais verticais) "retorna a lista completa das caixas verticais e horizontais de um tabuleiro"
	(cond
		(
			(or (null horizontais)
				(null verticais)
			) 
		nil
		)
		(t
			(append (caixas-fechadas-aux2 (car horizontais) (cadr horizontais)  (car verticais))
					;(caixas-fechadas-aux2 (cdr horizontais) (caddr verticais) (cdr verticais))
					(caixas-fechadas-aux (cdr horizontais) (cdr verticais))
			)
		)
	)
)
  
 ;; Teste: (caixas-fechadas-aux2(car(get-arcos-horizontais (tabuleiro-teste)))(cadr(get-arcos-horizontais (tabuleiro-teste)))(car(get-arcos-verticais (tabuleiro-teste))))
 ;; Resultado: ((NIL NIL NIL NIL) (T NIL NIL NIL) (T NIL NIL NIL))
 ;; Esta função cria uma lista com a cabeça da 1ºlinha e da 2ºlinha e ainda a cabeça do resto da coluna.
(defun caixas-fechadas-aux2(linha1 linha2 coluna)"retorna uma lista com as caixas das duas linhas e da duas colunas."
	(cond
		(
			(or
				(null linha1)
				(null linha2)
				(null coluna)
			)
		nil)
		(t
			(cons
				(list (car linha1)(car linha2)(car coluna)(cadr coluna))
				(caixas-fechadas-aux2 (cdr linha1)(cdr linha2)(cdr coluna))
			)
		)
	)
)

;; Teste: (get-cabecas-lista-aux  (get-arcos-horizontais (tabuleiro-teste)) 0)
;; Resultado:(NIL NIL NIL NIL)
;; Teste: (get-cabecas-lista-aux  (get-arcos-verticais (tabuleiro-teste)) 0)
;; Resultado:(NIL T NIL NIL)
;;get-cabecas-lista-aux
(defun get-cabecas-lista-aux(lista n)"Função que vai buscar as cabeças de uma linha vertical ou horizontal,escolhida pelo utilizador, e cria numa nova lista."
	(cond
		((null lista)nil)
		(t
			(cons
				(nth n (car lista))
				(get-cabecas-lista-aux (cdr lista)n)
			)
		)
	)
)


;;get-n-arcos-horizontais
(defun get-n-arcos-horizontais (tabuleiro)"retorna o numero de arcos horizontais"
	(length (car (get-arcos-horizontais tabuleiro)))
)
;;get-n-arcos-verticais
(defun get-n-arcos-verticais (tabuleiro)"retorna o numero de arcos verticais"
	(length (car (get-arcos-verticais tabuleiro)))
)



;; Teste: (get-cabecas-por-coluna(tabuleiro-teste))
;; Resultado:((NIL T NIL NIL) (NIL T NIL T) (NIL T NIL NIL))
;;(get-arcos-verticais (tabuleiro-teste)) =>((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL)) 
(defun get-cabecas-por-coluna(tabuleiro)"retorna as listas de cada cabeca de cada coluna" 
	(contador 0 (get-n-arcos-verticais tabuleiro) 'get-cabecas-lista-aux (get-arcos-verticais  tabuleiro)) 
)



;; Função de iteração
(defun contador (i tamanhoMax funcao &rest argumentos)
  (cond 
		((= i tamanhoMax) nil) ;; a função para quando chegar ao tamanho máximo do nº de arcos 
		(T (ecase funcao
			(get-cabecas-lista-aux (cons 
									(get-cabecas-lista-aux (car argumentos) i)
									(contador (+ i 1) tamanhoMax funcao (car argumentos))))
			)
		 )
   )
)
;





;; Teste: (contar-objetivo'((NIL NIL NIL T) (T NIL NIL T) (T NIL NIL T)))
;; Resultado: 0
;; Teste: (contar-objetivo'((T T T T) (T T T T) (T NIL NIL T)))
;; Resultado: 2

;; Esta função irá contar se existe uma caixa fechada na lista que recebe.
(defun contar-objetivo(lista) "Função que irá contar se a caixa está fechada ou não, isto é, se a função auxiliar conta-caixa-fechada "
(cond
((null lista)0)
((= (contar-nils-lista (car lista)) 0) (+ 1 (contar-objetivo (cdr lista))))
(t (contar-objetivo(cdr lista)))
)
)

;; Teste: (contar-nils-lista '(NIL NIL NIL T))
;; Resultado: 3
;; Esta função irá contar se existe NIL na lista. Se não existir dará valor 0, ou seja, teriamos uma lista so com T,o que resulta uma caixa fechada.
(defun contar-nils-lista (lista) "Função que irá contar o numero de Nill's existentes numa lista."
	(cond ((null lista) 0) 
	((equal 'NIL (car lista)) (+ 1 (contar-nils-lista (cdr lista)))) 
	(T (contar-nils-lista (cdr lista)))
	)
)



;;; heuristicas

;; heuristica1
;; Teste: (heuristica (tabuleiro1) 2)   -> Resultado: 1
(defun heuristica1 (tabuleiro numero-caixas-a-fechar) "Usada uma heurística que priveligia os tabuleiros com o maior número de caixas fechadas"
	(- numero-caixas-a-fechar (caixas-fechadas tabuleiro)  1)
)


(defun heuristica2 (tabuleiro numero-caixas-a-fechar) ""
	(- numero-caixas-a-fechar  (caixas-fechadas tabuleiro)) ;; =-1 nao é admissivel devia ser 0
)


#||
		FALTA

Verificar se é nó objectivo

||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SOLUÇAO


;;; Precisa de ser testada
(defun solucaop (no numero-caixas-a-fechar)	"Devolve [T] se o número de caixas a fechar for igual ao número de caixas fechadas do nó, e devolve [NIL] se não for"
	(= (caixas-fechadas (get-no-estado no)) numero-caixas-a-fechar)
)



#||

;; Tem que receber o numero de caixas a fechar 
(defun solucaop (no)	
"Recebe um nó e verifica se este este é um nó objectivo, verificando o estado. Um nó é objectivo se o número de caixas fechadas 
do seu estado for igual ao número de caixas a fechar lido no início do programa"

	(= (caixas-fechadas (get-estado-no no)) NUMERO CAIXAS A FECHAR DEFINIDAS NO INICIO DO PROGRAMA))
)
||#
#|| Ir buscar o nó objectivo da solução -> ver o lab 8 -> pq é ir buscar o nó final que é o 1º tabuleiro e o nó inicial é o último estado)

||#