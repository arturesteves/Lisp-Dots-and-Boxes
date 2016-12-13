(in-package :projecto1)

(defstruct estado ;estrutura do estado
tabuleiro         ;tabuleiro
f                 ;custo total (f + g)
g				  ;custo até ao nível actual
h				  ;custo estimado até à solução (heurística)
movActual		  ;movimento que levou até ao estado actual
noPai             ;nó a partir do qual o estado foi gerado
)

(defun semheuristica (tabuleiro) ;uma função com uma heurística sem significado para uso nas pesquisas cegas
  0
)

(defun heuristica1 (tabuleiro) ;heurística 1 (dada no enunciado) - soma do número de peças no tabuleiro - 1
  (1- (conta1s tabuleiro))
)

(defun heuristica2 (tabuleiro) ;heuristica 2 (criada no decorrer do projecto) - (soma do número de peças - 1) + soma da distância ao centro do tabuleiro
  (+ (heuristica1 tabuleiro)   
     (heuristica2-aux tabuleiro 1 1 0)
     )
)

(defun heuristica2-aux (tabuleiro x y soma) ;aplica a distância ao centro do tabuleiro e efectura a sua acumulação para todas as coordenadas
(cond
((> x 7) soma)
((> y 7) (heuristica2-aux tabuleiro (1+ x) 1 soma))
(T
(heuristica2-aux tabuleiro x (1+ y) (+ soma (distancia-ao-centro tabuleiro x y)))
)
)
)


(defun distancia-ao-centro (tabuleiro x y) ;calcula a distancia ao centro do tabuleiro de uma determinada peça
(if (equal (valor-coordenada x y tabuleiro) 1)
    (+ (abs (- 4 x)) (abs (- 4 y)))
    0
)  
)


(defun nos-gerados (la lf &optional (prevNosGerados 0)) ;retorna o número de nós gerados na procura
   (+ prevNosGerados (length la) (length lf))
)

(defun nos-expandidos (lf &optional (prevNosExpandidos 0)) ;retorna o número de nós expandidos na procura
   (+ prevNosExpandidos (length lf))
)

(defun penetrancia (la lf no &optional (prevNosGerados 0)) ;retorna a penetrância
   (float (/ (profundidade-solucao no) (nos-gerados la lf prevNosGerados)))
)

(defun profundidade-solucao (no) ;retorna a profundidade a que a solução foi encontrada
   (estado-g no)
)

(defun factor-ramificacao-medio (la lf no &optional (prevNosGerados 0)) ;calcula o número médio de nós por cada nível
  (float (/ (nos-gerados la lf prevNosGerados) (profundidade-solucao no)))
)

(defun procura-ida-star (la lf operadores heuristica limiar proxlimiar &optional (tempo (get-universal-time)) (funcaosucessores 'sucessores) (prevNosGerados 0) (prevNosExpandidos 0))
(cond
((null la)
(if (= limiar proxlimiar)
(solucao-nao-encontrada)
(procura-ida-star (last lf) nil operadores heuristica proxlimiar 9999 tempo funcaosucessores (nos-gerados la lf prevNosGerados) (nos-expandidos lf prevNosExpandidos))
)
)	
((objectivo (estado-tabuleiro (car la))) (devolver (car la) la lf 'IDA-star heuristica tempo limiar prevNosGerados prevNosExpandidos))
((visitado (car la) lf)(procura-ida-star (cdr la) lf operadores heuristica limiar proxlimiar tempo funcaosucessores prevNosGerados prevNosExpandidos))
(T (let ((retorno(ida-star (funcall funcaosucessores (car la) operadores nil heuristica) (cdr la) limiar proxlimiar)))
	(procura-ida-star
         (car retorno) (cons (car la) lf) operadores heuristica limiar (cadr retorno) tempo funcaosucessores prevNosGerados prevNosExpandidos
	)
    )
)
)
)

(defun ida-star (sucessores restla limiar proxlimiar) ;gestão do sucessores e lista de abertos segundo a lógica do IDA*
  (cond ((null sucessores) (list (ordenar-sucessores restla) proxlimiar))
    ((<= (estado-f (car sucessores)) limiar)
	(ida-star (cdr sucessores) (cons (car sucessores) restla) limiar proxlimiar)) ;só adiciona caso o f seja menor que o limiar
    (T 
     (if
         (and (< (estado-f (car sucessores)) proxlimiar) (> (estado-f (car sucessores)) limiar)) ;caso seja menor calcula o próximo limiar (caso contrário mantém o mesmo próximo limiar)
         (ida-star (cdr sucessores) restla limiar (estado-f (car sucessores)))
         (ida-star (cdr sucessores) restla limiar proxlimiar)
         )
     )
    )
)

(defun procura-generica (la lf algoritmo operadores heuristica &optional limite (tempo (get-universal-time)) (funcaosucessores 'sucessores)) ;função de procura genérica (para Breadth-first, Depth-first e A*)
(cond
((null la) (solucao-nao-encontrada)) ;lista abertos vazia -> solução não encontrada
((objectivo (estado-tabuleiro (car la))) (devolver (car la) la lf algoritmo heuristica tempo limite)) ;verifica se é nós objectivo e caso seja retorna a solução
((visitado (car la) lf)(procura-generica (cdr la) lf algoritmo operadores heuristica limite tempo funcaosucessores)) ;verifica se o nó já foi visitada anteriormente e caso tenha sido descarta-o
(T (procura-generica
    (funcall algoritmo (funcall funcaosucessores (car la) operadores nil heuristica) (cdr la) limite) (cons (car la) lf) algoritmo operadores heuristica limite tempo funcaosucessores
    );reinicia a procura adicionando os sucessores segundo um dos 3 algoritmos suportados                
)
)
) 

(defun visitado (no lf) ;verifica se um nó já foi anteriormente expandido
(cond
  ((null lf) nil)
  ((equal (estado-tabuleiro no) (estado-tabuleiro (car lf))) T)
  (T (visitado no (cdr lf)))
)
)

(defun largura (sucessores restla limite) ;procura Breadth-first
  (append restla sucessores)
)

(defun profundidade (sucessores restla limite) ;procura Depth-first
  (append sucessores restla)
)

(defun profundidade-limitada (sucessores restla limite) ;procura Depth-limited (Depth-first com limite de profundidade)
  (cond ((null sucessores) restla)
        ((<= (estado-g (car sucessores)) limite) (append sucessores restla))
        (T restla)
  )
)

(defun a-star (sucessores restla limite) ;procura A*
  (ordenar-sucessores (append sucessores restla))
)

(defun insere-ordenado (no la) ;insere um nó ordenado por f na lista de abertos
  (cond ((null la) (list no))
    ((< (estado-f no) (estado-f (car la))) (cons no la))
    (T (cons (car la) (insere-ordenado no (cdr la)))
    )
 ) 
 )

(defun ordenar-sucessores (sucessores) ;ordena uma lista segundo f
(cond ((null sucessores) nil)
       (T (insere-ordenado (car sucessores) (ordenar-sucessores (cdr sucessores))))                  
)
)

(defun movimentos-solucao (no solucoes) ;retorna os movimentos necessários para chegar à solução
	(cond
		((null (estado-noPai no)) solucoes)
		(T  (movimentos-solucao (estado-noPai no) (cons (estado-movActual no) solucoes)))
	) 
)

(defun tabuleiro-inicial (no) ;retorna o tabuleiro inicial
	(cond
		((null (estado-noPai no)) (estado-tabuleiro no))
		(T  (tabuleiro-inicial (estado-noPai no)))
	) 
)