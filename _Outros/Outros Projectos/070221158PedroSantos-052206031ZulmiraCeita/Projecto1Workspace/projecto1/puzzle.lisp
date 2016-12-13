(in-package :projecto1)

(defun valor-coordenada (x y tabuleiro) ;;retorna o valor de uma dada coordenada no tabuleiro
(let ((a (1- x)) (b (- 7 y)))
(if (or (< a 0) (< b 0))
nil
(nth a (nth b tabuleiro))
) 
)
)

(defun substituir-nth (lista n elem) ;função que dada uma lista substituí a posição n por elem
  (cond
    ((null lista) nil)
    ((= n 1) (cons elem (cdr lista)))
    (T (cons (car lista) (substituir-nth (cdr lista) (1- n) elem)))))

(defun mudar-coordenada (x y valor tabuleiro) ;altera o valor de uma coordenada no tabuleiro (útil para mover peças)
(substituir-nth tabuleiro (- 8 y) (substituir-nth (nth (- 7 y) tabuleiro) x valor))
)

(defun conta1s (L) ;conta o número de 1s no tabuleiro
  (cond ((null L) 0)
   ((numberp L) (if (= L 1) 1 0))
   (T (apply '+ (mapcar 'conta1s L)))))



(defun c-mover (x y tabuleiro) ;move uma peça para cima
 (mudar-coordenada x (1+ y) 1 (mudar-coordenada x y 0 tabuleiro))
)

(defun b-mover (x y tabuleiro) ;move uma peça para baixo
 (mudar-coordenada x (1- y) 1 (mudar-coordenada x y 0 tabuleiro))
)

(defun e-mover (x y tabuleiro) ;move uma peça para a esquerda (retorna o tabuleiro correspondente)
 (mudar-coordenada (1- x) y 1 (mudar-coordenada x y 0 tabuleiro))
)

(defun d-mover (x y tabuleiro) ;move uma peça para a direita (retorna o tabuleiro correspondente)
 (mudar-coordenada (1+ x) y 1 (mudar-coordenada x y 0 tabuleiro))
)

(defun cc-mover (x y tabuleiro) ;captura uma peça para cima (retorna o tabuleiro correspondente)
 (mudar-coordenada x (+ y 2) 1 (mudar-coordenada x (1+ y) 0 (mudar-coordenada x y 0 tabuleiro)))
)

(defun cb-mover (x y tabuleiro) ;captura uma peça para baixo (retorna o tabuleiro correspondente)
 (mudar-coordenada x (- y 2) 1 (mudar-coordenada x (1- y) 0 (mudar-coordenada x y 0 tabuleiro)))
)

(defun ce-mover (x y tabuleiro) ;captura uma peça para a esquerda (retorna o tabuleiro correspondente)
 (mudar-coordenada (- x 2) y 1 (mudar-coordenada (1- x) y 0 (mudar-coordenada x y 0 tabuleiro)))
)

(defun cd-mover (x y tabuleiro) ;captura uma peça para a direita (retorna o tabuleiro correspondente)
 (mudar-coordenada (+ x 2) y 1 (mudar-coordenada (1+ x) y 0 (mudar-coordenada x y 0 tabuleiro)))
)



(defun c (x y tabuleiro) ;função que verifica se é possível mover uma peça para cima e caso seja possível move e retorna o tabuleiro correspondente (caso contrário retorna nil)
(cond
  ((and (equal (valor-coordenada x y tabuleiro) 1)(equal (valor-coordenada x (1+ y) tabuleiro) 0)) (c-mover x y tabuleiro))
  (T nil)
  )
)

(defun b (x y tabuleiro) ;função que verifica se é possível mover uma peça para baixo e caso seja possível move e retorna o tabuleiro correspondente (caso contrário retorna nil)
(cond
  ((and (equal (valor-coordenada x y tabuleiro) 1)(equal (valor-coordenada x (1- y) tabuleiro) 0)) (b-mover x y tabuleiro))
  (T nil)
  )
)

(defun e (x y tabuleiro) ;função que verifica se é possível mover uma peça para a esquerda e caso seja possível move e retorna o tabuleiro correspondente (caso contrário retorna nil)
(cond
  ((and (equal (valor-coordenada x y tabuleiro) 1)(equal (valor-coordenada (1- x) y tabuleiro) 0)) 
   (e-mover x y tabuleiro)
   )
  (T nil)
  )
)

(defun d (x y tabuleiro) ;função que verifica se é possível mover uma peça para a direita e caso seja possível move e retorna o tabuleiro correspondente (caso contrário retorna nil)
(cond
  ((and (equal (valor-coordenada x y tabuleiro) 1)(equal (valor-coordenada (1+ x) y tabuleiro) 0)) 
  (d-mover x y tabuleiro)
   )
  (T nil)
  )
)

(defun cc (x y tabuleiro) ;função que verifica se é possível capturar uma peça para cima e caso seja possível move e retorna o tabuleiro correspondente (caso contrário retorna nil)
(cond
  ((and (equal (valor-coordenada x y tabuleiro) 1)(equal (valor-coordenada x (1+ y) tabuleiro) 1)(equal (valor-coordenada x (+ y 2) tabuleiro) 0)) (cc-mover x y tabuleiro))
  (T nil)
)
)

(defun cb (x y tabuleiro) ;função que verifica se é possível capturar uma peça para baixo e caso seja possível move e retorna o tabuleiro correspondente (caso contrário retorna nil)
(cond
  ((and (equal (valor-coordenada x y tabuleiro) 1)(equal (valor-coordenada x (1- y) tabuleiro) 1)(equal (valor-coordenada x (- y 2) tabuleiro) 0)) (cb-mover x y tabuleiro))
  (T nil)
)
)

(defun ce (x y tabuleiro) ;função que verifica se é possível capturar uma peça para a esquerda e caso seja possível move e retorna o tabuleiro correspondente (caso contrário retorna nil)
(cond
  ((and (equal (valor-coordenada x y tabuleiro) 1)(equal (valor-coordenada (1- x) y tabuleiro) 1)(equal (valor-coordenada (- x 2) y tabuleiro) 0)) (ce-mover x y tabuleiro))
  (T nil)
)
)

(defun cd (x y tabuleiro) ;função que verifica se é possível capturar uma peça para a direita e caso seja possível move e retorna o tabuleiro correspondente (caso contrário retorna nil)
(cond
  ((and (equal (valor-coordenada x y tabuleiro) 1)(equal (valor-coordenada (1+ x) y tabuleiro) 1)(equal (valor-coordenada (+ x 2) y tabuleiro) 0)) (cd-mover x y tabuleiro))
  (T nil)
)
)

(defun objectivo (tabuleiro) ;verifica se um tabuleiro é estado final (se tem apenas uma peça)
  (if (= (conta1s tabuleiro) 1) t nil)
)

(defun sucessores-aux (no operadores gerados x y heuristica) ;aplica a uma dada coordenada todos os operadores
  (cond
    ((null operadores) gerados) ;retorna gerados quando não existirem mais operadores
    (T 
     (let ((tabGerado (funcall (car operadores) x y (estado-tabuleiro no)))) ;gera o tabuleiro
       (if (null tabGerado) 												 ;se for nulo (operador não válido para uma dada posição) não o adiciona ao nós gerados
           (sucessores-aux no (cdr operadores) gerados x y heuristica)      
           (let ((heur (funcall heuristica tabGerado)) (nivel (1+ (estado-g no)))) ;caso seja válido calcula o nível e a heurisitca e adiciona o nó aos gerados
           (sucessores-aux no (cdr operadores) (append gerados (list (make-estado 
                                                      :tabuleiro tabGerado
                                                      :f (+ nivel heur)
                                                      :g nivel
                                                      :h heur
                                                      :movActual (list (car operadores) x y)
                                                      :noPai no
                                                      ) 
                                                     )) x y heuristica) ;continua a gerar nós para um determinada coordenada até se esgotarem
           )
           )
       )
     )
    )
  )

(defun sucessores-coord (no operadores gerados x y heuristica) ;aplica a cada coordenada os operadores
  (cond
    ((> x 7) gerados)
    ((> y 7) (sucessores-coord no operadores gerados (1+ x) 1 heuristica))
    (T (sucessores-coord no operadores (sucessores-aux no operadores gerados x y heuristica) x (1+ y) heuristica)
    )
    )   
 )

(defun sucessores (no operadores gerados heuristica) ;função que gera os sucessores de um nó segundo uma certa heurística e operadores
	(sucessores-coord no operadores gerados 1 1 heuristica)
)