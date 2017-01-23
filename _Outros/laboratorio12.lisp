;;;;       Programa do jogo do galo (adaptado de Touretzky, 1990)
;;;;       Apos ter desenvovlido as funções pedidas no enunciado
;;;;       utilize a funcao fazer-uma-partida para jogar (as casas são numeradas de 0 a 8).

;;;;
;;;; Constantes:
;;;;
(defvar *computador* 10)
(defvar *humano* 1)
; o conjunto de 8 tripletos que contem a lista dos indices para  representar as 3 linhas, 3 colunas e 2 diagonais do tabuleiro
(defvar *triplets*
  '((0 1 2) (3 4 5) (6 7 8)
    (0 3 6) (1 4 7) (2 5 8)
    (0 4 8) (2 4 6)))

;;;;
;;;; Variavel global:
;;;;
; variavel que guarda o tabuleiro que corresponde a jogada a fazer pelo PC, atualizada pelo algoritmo alfabeta
(defvar *jogada* nil)

;;;;
;;;; Funcoes auxiliares:
;;;;

;;; Construtores
(defun tabuleiro-inicial (&optional stream)
  "Permite criar o tabuleiro inicial do jogo."
  (cond ((null stream) (list 0 0 0 0 0 0 0 0 0))
        (t (read stream))))

(defun no-inicial (tabuleiro)
"Retorna o no inicial com o tabuleiro vazio e o valor da profundidade igual a zero"
  (list tabuleiro 0)
)

(defun cria-no (tabuleiro profundidade)
"cria um novo no com base num tabuleiro recebido por parametro e o valor da profundidade recebido.
No novo no, a profunidade e incrementada de um valor."
  (list tabuleiro (1+ profundidade))
)
;;; Getters
(defun no-tabuleiro (no)
  "Retorna o tabuleiro de um no"
  (car no)
)

(defun no-profundidade (no)
  "Retorna o tabuleiro de um no"
  (cadr no)
)

(defun no-tipo (no)
"Retorna o tipo do no em funcao da sua profundidade" 
  (cond 
   ((evenp (no-profundidade no)) 'MAX)
   (T 'MIN)
   )
)

(defun peca-jogador (tipo)
"Retorna a pca do jogador em funcao do seu tipo MAX ou MIN" 
  (cond
   ((equal tipo 'MAX) 10)
   (T 1)
))

;;; sucessores
(defun sucessores (no)
"Retorna a lista de sucessores de um no, em funcao do seu tipo."
(sucessores-aux 0 (no-tabuleiro no) (peca-jogador (no-tipo no)) (no-profundidade no))
)

(defun sucessores-aux (indice tabuleiro peca profundidade)
"Retorna a lista de sucessores de um no, em funcao do tabuleiro, da peca a jogar e da profundidade do no."
  (cond 
   ((> indice 8) nil)
   ((= 0 (nth indice tabuleiro)) (cons (cria-no (faz-jogada peca indice tabuleiro) profundidade) (sucessores-aux (1+ indice) tabuleiro peca profundidade)))
   (T (sucessores-aux (1+ indice) tabuleiro peca profundidade))
   )
)

;;; funcoes para a saida no ecra
(defun converte-para-letra (v)
  "Converte os inteiros do tabuleiro para os simbolos 0 e X "
  (cond ((equal v 1) "O")
        ((equal v 10) "X")
        (t " ")))

(defun imprime-linha (x y z)
  "Imprime uma linha formatada do tabuleiro"
  (format t "~&   ~A | ~A | ~A"
          (converte-para-letra x)
          (converte-para-letra y)
          (converte-para-letra z)))

(defun imprime-tabuleiro (tabuleiro)
  "Imprime o tabuleiro, linha a linha"
  (format t "~%")
  (imprime-linha
    (nth 0 tabuleiro) (nth 1 tabuleiro) (nth 2 tabuleiro))
  (format t "~& -----------")
  (imprime-linha
    (nth 3 tabuleiro) (nth 4 tabuleiro) (nth 5 tabuleiro))
  (format t "~& -----------")
  (imprime-linha
    (nth 6 tabuleiro) (nth 7 tabuleiro) (nth 8 tabuleiro))
  (format t "~%~%"))

(defun faz-jogada (jogador pos tabuleiro)
  "Cria o novo tabuleiro que consiste na jogada de um dos jogadores."
  (cond ((null tabuleiro) nil)
        ((= pos 0) (cons jogador (rest tabuleiro)))
        (t (cons (first tabuleiro) (faz-jogada jogador (1- pos) (rest tabuleiro)))))) 

(defun soma-triplet (tabuleiro triplet)
  (+ (nth (first triplet) tabuleiro)
     (nth (second triplet) tabuleiro)
     (nth (third triplet) tabuleiro)))

(defun calcula-soma (tabuleiro)
  (mapcar #'(lambda (triplet)
              (soma-triplet tabuleiro triplet))
          *triplets*))

(defun vencedor-p (tabuleiro)
  "determina se existe um vencedor. Se existir devolve a lista com o valor 3 ou 30 encontrado para indicar quem é o vencedor. Senão devolve NIL."
  (let ((soma (calcula-soma tabuleiro)))
    (or (member (* 3 *computador*) soma)
        (member (* 3 *humano*) soma))))

(defun fazer-uma-partida ()
  "Funcao que permite inicar o jogo. O jogador humano comeca quando responde Y e o PC comeca quando o utilizador responde N."
  (if (y-or-n-p "Quer iniciar a partida?")
      (humano-joga (tabuleiro-inicial))
      (computador-joga (tabuleiro-inicial))))

(defun le-jogada (tabuleiro)
  "Le uma jogada fazendo a verificacao da sua legalidade. A jogada lida e o indice na lista (entre 0 e 8)"
  (format t "~&A sua jogada [0 <= x <= 8]: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 0 pos 8)))
           (format t "~&Entrada invalida.")
           (le-jogada tabuleiro))
          ((not (zerop (nth pos tabuleiro)))
           (format t
             "~&Esta casa ja esta ocupada.")
           (le-jogada tabuleiro))
          (t pos))))

(defun tabuleiro-preenchido-p  (tabuleiro)
  "Verifica se o tabuleiro ja esta completamente preenchido"
  (not (member 0 tabuleiro)))


;;;;
;;;; funções de jogo (humano e computador c/minimax)
;;;;
(defun humano-joga (tabuleiro)
  "Retorna a jogada do humano sob a forma de um novo tabuleiro.
  Verifica se existe um vencedor ou um empate. Se nao for o caso, pede a jogada do PC"
  (let* ((pos (le-jogada tabuleiro))
         (novo-tabuleiro (faz-jogada *humano* pos tabuleiro))
		 )
    (imprime-tabuleiro novo-tabuleiro)
    (cond ((vencedor-p novo-tabuleiro)
           (format t "~&Ganhou!"))
          ((tabuleiro-preenchido-p novo-tabuleiro)
           (format t "~&Empatamos."))
          (t (computador-joga novo-tabuleiro)))))


(defun computador-joga (tabuleiro)
  "Retorna a jogada do PC sob a forma de um novo tabuleiro.
  Verifica se existe um vencedor ou um empate. Se nao for o caso, pede a jogada do humano"
  (let* ((pos (escolhe-melhor-jogada tabuleiro))
         (novo-tabuleiro (faz-jogada *computador* pos tabuleiro)))
    (format t "~&A minha jogada: ~S" pos)
    (imprime-tabuleiro novo-tabuleiro)
    (cond ((vencedor-p novo-tabuleiro)
           (format t "~&Ganhei!"))
          ((tabuleiro-preenchido-p novo-tabuleiro)
           (format t "~&Empatamos."))
          (t (humano-joga novo-tabuleiro)))))

(defun escolhe-melhor-jogada (tabuleiro)
  "Escolhe a melhor jogada com base ou numa estrategia ou no algoritmo minimax"
  ;(estrategia-aleatoria tabuleiro)
  (alfabeta (no-inicial tabuleiro) 4)
)

(defun estrategia-aleatoria (tabuleiro)
  "escolhe um valor aleatorio entre 0 e 8 para escolher a jogada a fazer. Faz a verificacao da existencia de uma peca no valor escolhido. Se existir ja uma peça, gera um novo valor aleatorio."
  (let ((pos (random 9)))
    (cond
     ((zerop (nth pos tabuleiro)) pos)
     (T (estrategia-aleatoria tabuleiro))))
)

;;; Exercicios
;; no-tipo
(defun no-tipo (no)
  (cond ((OR (evenp (second no)) (= (second no) 0)) 'MAX)
        ((oddp (second no)) 'MIN))
)
;; no-sucessores
(defun no-sucessores (no)
  (let ((tipo (no-tipo no)) (tabuleiro (no-tabuleiro no)))
    (cond ((eq tipo 'MIN) (gera-sucessores tabuleiro 10))
          (t (gera-sucessores tabuleiro 1))
    )
  )
)
;; avaliar-no
(defun avaliar-no (tabuleiro)
  (let ((vencedor (car (vencedor-p tabuleiro))))
    (cond ((= vencedor 30) 1)
          ((= vencedor 3) -1)
          ((tabuleiro-preenchido-p tabuleiro) 0)
          (t NIL)
    )
  )
)
;; no-sucessores
;; alfabeta
(defun alfabeta (no profMax &optional (alfa -1000) (beta 1000))
  (cond ((= (no-profundidade no) profMax) no) 
)
)
;; ordenar-nos



(defun alfabeta (no alfa beta maximizingPlayer solucao &optional (prof-max max-profundidade))
 (cond 
  ((OR (no-folha-p no prof-max) (eq solucao T)) no)
  ((equal maximizingPlayer T) (maximizer (sucessores no prof-max 'heuristica-projeto) alfa beta no))
  (T (minimizer (sucessores no prof-max 'heuristica-projeto) alfa beta no))
 )
)

(defun maximizer (sucessores alfa beta no)
  (cond
   ((AND (no-solucao sucessores)) (alfabeta sucessores alfa beta nil (no-solucao sucessores)))
   ((null sucessores) no)
   (T (let ((no-sucessores (car sucessores)))
        (cond 
         ((null (get-tabuleiro-no no-sucessores)) (maximizer (cdr sucessores) alfa beta no))
         (T (let ((newAlfa (max alfa (get-heuristica (alfabeta no-sucessores alfa beta nil (no-solucao no-sucessores))))))
              (cond
               ((>= newAlfa beta) no-sucessores)
               (T (maximizer (cdr sucessores) newAlfa beta no-sucessores))               
               ))))
     ))
  )
)

(defun minimizer (sucessores alfa beta no)
  (cond
   ((AND (no-solucao sucessores)) (alfabeta sucessores alfa beta t (no-solucao sucessores)))
   ((null sucessores) no)
   (T (let ((no-sucessores (car sucessores)))
        (cond 
          ((null (get-tabuleiro-no no-sucessores)) (minimizer (cdr sucessores) alfa beta no))
            (T (let ((newBeta (min beta (get-heuristica (alfabeta no-sucessores alfa beta t (no-solucao no-sucessores))))))
                 (cond
                  ((<= newBeta alfa) no-sucessores)
                  (t (minimizer (cdr sucessores) alfa newBeta no-sucessores))
                  )
             )) 
           )
     ))
	)
)