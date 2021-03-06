;;;; pontosecaixas.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Programador: Daniel Costa - 120221058
;;;; Ficheiro que contem todos os operadores e funções seletoras e modificadoras deste projeto.

;;; Construtor
;; cria-no
(defun cria-no (estado &optional (profundidade 0) (utilidade 0) (caixas-jogador-1 0)(caixas-jogador-2 0)) 
"Cria uma lista que representa um nó; Um nó é composto pelo estado que é o tabuleiro, este é um parâmetro obrigatório, é composto também por outros parâmetros, como
a profundidade a que se encontra, pela heurística deste mesmo nó, ou seja, o nó que o gerou. A utilidade por omissão tem valor 0"
	(list estado profundidade utilidade caixas-jogador-1 caixas-jogador-2)
)


;;; Metodos de Consulta

;;get-no-estado
(defun get-no-estado (no) "Retorna o estado do nó, que é representado pelo tabuleiro"
	(first no)
)
;

;;get-no-profundidade
(defun get-no-profundidade (no) "Retorna a profundidade em que o nó se encontra"
	(second no)   ; Igual a: (car (cdr no))
)
;

;;get-no-utilidade
(defun get-no-utilidade (no) "Retorna a heurística do nó"
	(third no)   ; Igual a: (car (cdr (cdr no)))
)
;

;;get-caixas-jogador-1
(defun get-caixas-jogador-1(no) "Retorna o numero de caixas do jogador 1"
	(fourth no)
)
;

;;get-caixas-jogador-2
(defun get-caixas-jogador-2(no) "Retorna o numero de caixas do jogador 2"
	(fifth no)
)
;

;;get-arcos-horizontais
(defun get-arcos-horizontais (tabuleiro) "Retorna a lista dos arcos horizontais de um tabuleiro"
	(car tabuleiro)
)
;

;;get-arcos-verticais
(defun get-arcos-verticais (tabuleiro) "Retorna a lista dos arcos verticiais de um tabuleiro"
	(car (cdr tabuleiro))
)
;


;;inserir-arco-na-posicao
(defun inserir-arco-na-posicao (indice lista peca) "Insere um arco (representado pelo valor [T]) no índice da lista recebida"
	(cond
		((null lista) nil)
		((= indice 1) (cons peca (cdr lista)))
		(T (cons (car lista) (inserir-arco-na-posicao (- indice 1) (cdr lista) peca)))
	)
)
;

;;inserir-arco-na-posicao-aux
(defun inserir-arco-na-posicao-aux (linha coluna lista peca) "Insere um arco (representado pelo valor [T]) numa lista que representa o conjunto de arcos dum tabuleiro. A posição representada pela linha e a coluna de destino são valores inteiros passados como argumentos"
	(cond
		((null lista) nil)
		((= linha 1) (cons 
						(inserir-arco-na-posicao coluna (car lista) peca)
						(cdr lista)
					 )
		)
		(T (cons 	(car lista)
					(inserir-arco-na-posicao-aux (- linha 1) coluna (cdr lista) peca)
			))
	)
)
;


;;; Operadores

;;inserir-arco-vertical	
(defun inserir-arco-vertical (linha coluna peca tabuleiro) "Insere um arco vertical (representado pelo valor [T]) num tabuleiro passado como argumento"
	(let ((arcos-horizontais (get-arcos-horizontais tabuleiro))
		  (arcos-verticais (get-arcos-verticais tabuleiro)))
	
		(cond
			((or (null tabuleiro) (not (possivel-adicionar-arco linha coluna arcos-verticais))) nil)
			(T (cons arcos-horizontais (list (inserir-arco-na-posicao-aux linha coluna arcos-verticais peca))))
		)
	)
)
;

;;inserir-arco-horizontal
(defun inserir-arco-horizontal (linha coluna peca tabuleiro) "Insere um arco horizontal (representado pelo valor [T]) num tabuleiro passado como argumento"
	(let ((arcos-horizontais (get-arcos-horizontais tabuleiro))
		  (arcos-verticais (get-arcos-verticais tabuleiro)))

		(cond
			((or (null tabuleiro) (not (possivel-adicionar-arco linha coluna arcos-horizontais))) nil)
			(T (cons (inserir-arco-na-posicao-aux linha coluna arcos-horizontais peca) (list arcos-verticais)))
		)
	)
)		
;

;;possivel-adicionar-arco
(defun possivel-adicionar-arco (linha coluna lista) "Recebe indices de linha e coluna e uma lista de arcos horizontais ou de arcos verticais e verifica se naquela posição o valor é [T], se for devolve [NIL], se for [NIL] devolve [T]"
	(cond
		((null lista) nil)
		((= linha 1) (possivel-adicionar-arco-aux coluna (car lista)))
		(T (possivel-adicionar-arco (- linha 1) coluna (cdr lista)))
	)
)
;

;;possivel-adicionar-arco-aux 
(defun possivel-adicionar-arco-aux (indice lista)
	(cond
		((null lista) nil)
		((and (= indice 1) (eql (car lista) nil)) T)
		(T (possivel-adicionar-arco-aux (- indice 1) (cdr lista)))
	)
)	
;

;;operadores 
(defun operadores () "Cria uma lista com todos os operadores do problema dos Pontos e das Caixas"
	(list 'inserir-arco-horizontal 'inserir-arco-vertical)
)
;

;;numero-linhas-tabuleiro
;; ATENÇÃO: Esta função devolve o número de linhas entre os pontos. Para devolver o número exacto de linhas é preciso somar 1 ao resultado da função."
(defun numero-linhas-tabuleiro (tabuleiro) "Retorna o número de linhas horizontais de um tabuleiro"
	(get-dimensao-aux (car (get-arcos-verticais tabuleiro)))
)
;

;;numero-colunas-tabuleiro
;; ATENÇÃO: Esta função devolve o número de colunas entre os pontos. Para devolver o número exacto de colunas é preciso somar 1 ao resultado da função."
(defun numero-colunas-tabuleiro (tabuleiro) "Retorna o número de linhas verticais de um tabuleiro"
	(get-dimensao-aux (car (get-arcos-horizontais tabuleiro)))
)
;

;;get-dimensao-aux
(defun get-dimensao-aux (lista) "Dada uma lista de arcos devolve o número de listas existentes"
	(cond
		((null lista) 0)
		(T (+ 1 (get-dimensao-aux (cdr lista))))
	)
)
;

;;; Função Validação de Caixas

;;caixas-fechadas
(defun caixas-fechadas (tabuleiro) "retorna o numero de caixas fechadas de um tabuleiro."
	(cond
		((null tabuleiro) nil)
		(t
			(contar-objetivo (caixas-fechadas-aux (get-arcos-horizontais tabuleiro)(get-cabecas-por-coluna tabuleiro)))
		)
	)
)
;

;;caixas-fechadas-aux
;; Esta função vai buscar os arcos horizontais e verticais e aplica uma função auxiliar de modo a juntar todas as caixas numa só lista.
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
;
 
;;caixas-fechadas-aux2
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
;

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
;

;;get-cabecas-por-coluna
(defun get-cabecas-por-coluna(tabuleiro)"retorna as listas de cada cabeca de cada coluna" 
	(contador 0 (numero-colunas-tabuleiro tabuleiro) 'get-cabecas-lista-aux (get-arcos-verticais  tabuleiro)) 
)
;

;; contador
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
;

;; contar-objetivo
(defun contar-objetivo(lista) "Função que irá contar se a caixa está fechada ou não, isto é, se a função auxiliar conta-caixa-fechada. "
	(cond
		((null lista)0)
		((= (contar-nils-lista (car lista)) 0) (+ 1 (contar-objetivo (cdr lista))))
		(t (contar-objetivo(cdr lista)))
	)
)
;

;; contar-nils-lista
(defun contar-nils-lista (lista) "Função que irá contar o numero de NILS's existentes numa lista.Se não existir dará valor 0, ou seja, teriamos uma lista so com T,o que resulta uma caixa fechada."
	(cond 
		((null lista) 0) 
		((equal 'NIL (car lista)) (+ 1 (contar-nils-lista (cdr lista)))) 
		(T (contar-nils-lista (cdr lista)))
	)
)
;


;; trocar-peca
(defun troca-peca (peca) "Troca a peca de um jogador para a peca de outro jogador."
	(cond
		((= peca *jogador1*) *jogador2*)
		((= peca *jogador2*) *jogador1*)
	)
)
;