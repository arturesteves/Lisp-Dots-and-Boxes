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


;;; Metodos de Consulta

;; get-no-estado
(defun get-no-estado (no) "Retorna o estado do nó, que é representado pelo tabuleiro"
	(car no)
)

;; get-no-profundidade
(defun get-no-profundidade (no) "Retorna a profundidade em que o nó se encontra"
	(cadr no)   ; Igual a: (car (cdr no))
)

;; get-no-heuristica
(defun get-no-heuristica (no) "Retorna a heurística do nó"
	(caddr no)   ; Igual a: (car (cdr (cdr no)))
)

;; get-no-pai
(defun get-no-pai (no) "Retorna o nó pai deste nó, ou seja, o nó que gerou este nó"
	(cadddr no)   ; Igual a: (car (cdr (cdr (cdr no))))
)

;; custo
(defun custo (no)"retorna o valor do custo do nó (f). Soma do valor da profundidade com o valor heuristico."
    (+ (get-no-profundidade no)(get-no-heuristica no))
)


;; get-arcos-horizontais
(defun get-arcos-horizontais (tabuleiro) "Retorna a lista dos arcos horizontais de um tabuleiro"
	(car tabuleiro)
)

;; get-arcos-verticais
(defun get-arcos-verticais (tabuleiro) "Retorna a lista dos arcos verticiais de um tabuleiro"
	(car (cdr tabuleiro))
)

;;; Funcões auxiliares dos operadores

;; inserir-arco-na-posicao
(defun inserir-arco-na-posicao (indice lista) "Insere um arco (representado pelo valor [T]) no índice da lista recebida"
	(cond
		((null lista) nil)
		((= indice 1) (cons T (cdr lista)))
		(T (cons (car lista) (inserir-arco-na-posicao (- indice 1) (cdr lista))))
	)
)

;; inserir-arco-na-posicao-aux
(defun inserir-arco-na-posicao-aux (linha coluna lista) "Insere um arco (representado pelo valor [T]) numa lista que representa o conjunto de arcos dum tabuleiro. A posição representada pela linha e a coluna de destino são valores inteiros passados como argumentos"
	(cond
		((null lista) nil)
		((= linha 1) (cons (inserir-arco-na-posicao coluna (car lista)) (cdr lista)))
		(T (cons (car lista) (inserir-arco-na-posicao-aux (- linha 1) coluna (cdr lista))))
	)
)


;;; Operadores

;; arco-vertical
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
(defun possivel-adicionar-arco (linha coluna lista) "Recebe indices de linha e coluna e uma lista de arcos horizontais ou de arcos verticais e verifica se naquela posição o valor é [T], se for devolve [NIL], se for [NIL] devolve [T]"
	(cond
		((null lista) nil)
		((= linha 1) (possivel-adicionar-arco-aux coluna (car lista)))
		(T (possivel-adicionar-arco (- linha 1) coluna (cdr lista)))
	)
)


;; possivel-adicionar-arco-aux 
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

;; Teste : 

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

;; Teste: (get-cabecas-por-coluna(tabuleiro-teste))
;; Resultado:((NIL T NIL NIL) (NIL T NIL T) (NIL T NIL NIL))
;;(get-arcos-verticais (tabuleiro-teste)) =>((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL)) 
(defun get-cabecas-por-coluna(tabuleiro)"retorna as listas de cada cabeca de cada coluna" 
	(contador 0 (numero-colunas-tabuleiro tabuleiro) 'get-cabecas-lista-aux (get-arcos-verticais  tabuleiro)) 
)

;; contador (Função de iteração)
(defun contador (i tamanhoMax funcao &rest argumentos) "contador que executa determinada ação ao longo de uma lista. Para quando indice for = tamanho da lista."
  (cond 
		((= i tamanhoMax) nil)
		(T (ecase funcao
			(get-cabecas-lista-aux (cons 
									(get-cabecas-lista-aux (car argumentos) i)
									(contador (+ i 1) tamanhoMax funcao (car argumentos))))
			)
		 )
   )
)

;; Teste: (contar-objetivo'((NIL NIL NIL T) (T NIL NIL T) (T NIL NIL T)))
;; Resultado: 0
;; contar-objetivo
(defun contar-objetivo(lista) "Função que irá contar se a caixa está fechada ou não, isto é, se a função auxiliar conta-caixa-fechada. "
	(cond
		((null lista)0)
		((= (contar-nils-lista (car lista)) 0) (+ 1 (contar-objetivo (cdr lista))))
		(t (contar-objetivo(cdr lista)))
	)
)

;; Teste: (contar-nils-lista '(NIL NIL NIL T))
;; Resultado: 3
;; contar-nils-lista
(defun contar-nils-lista (lista) "Função que irá contar o numero de NILS's existentes numa lista.Se não existir dará valor 0, ou seja, teriamos uma lista so com T,o que resulta uma caixa fechada."
	(cond 
		((null lista) 0) 
		((equal 'NIL (car lista)) (+ 1 (contar-nils-lista (cdr lista)))) 
		(T (contar-nils-lista (cdr lista)))
	)
)


;;; Heuristicas

;; heuristica1
(defun heuristica1 (tabuleiro numero-caixas-a-fechar) "Usada uma heurística que priveligia os tabuleiros com o maior número de caixas fechadas"
	(- numero-caixas-a-fechar (caixas-fechadas tabuleiro)  1)
)

;; heuristica2
(defun heuristica2 (tabuleiro numero-caixas-a-fechar) ""
	(- numero-caixas-a-fechar  (caixas-fechadas tabuleiro)) ;; =-1 nao é admissivel devia ser 0
)

;; Solução

;; solucaop
(defun solucaop (no numero-caixas-a-fechar)	"Devolve [T] se o número de caixas a fechar for igual ao número de caixas fechadas do nó, e devolve [NIL] se não for"
	(= (caixas-fechadas (get-no-estado no)) numero-caixas-a-fechar)
)

#|| Ir buscar o nó objectivo da solução -> ver o lab 8 -> pq é ir buscar o nó final que é o 1º tabuleiro e o nó inicial é o último estado)

||#

;; caminho-solucao
(defun caminho-solucao (no &optional (solucao nil)) "Retorna o caminho até à solução. Ou seja todos os estados desde o inicial à solução."
	(cond
		((null (get-no-estado no)) solucao)
		(T (caminho-solucao (get-no-pai no) (cons (get-no-estado no) solucao)))
	) 
)