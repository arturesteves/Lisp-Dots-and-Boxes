;;;; alfabeta.lisp
;;;; Projeto 2 - IA 1 Semestre Ano Lectivo 2015-2016
;;;; Deve conter a implementacao do algoritmo alfa-beta
;;;; Autor: João Silva (120221053) & Joao Guerreiro (110221049)

(defvar *nos-analisados* 0)
(defvar *cortes-alfa* 0)
(defvar *cortes-beta* 0)

(defun repoe-estatisticas ()
  (setf *nos-analisados* 0)
  (setf *cortes-beta* 0)
  (setf *cortes-alfa* 0))

(defun analisa-no (no peca)
  (and (setf *nos-analisados* (1+ *nos-analisados*)) (avaliar-no no peca)))

(defun corte-alfa ()
  (setf *cortes-alfa* (1+ *cortes-alfa*)))

(defun corte-beta ()
  (setf *cortes-beta* (1+ *cortes-beta*)))

(defun get-nos-analisados ()
  *nos-analisados*)

(defun get-cortes-alfa ()
  *cortes-alfa*)

(defun get-cortes-beta ()
  *cortes-beta*)

(defun ordena-sucessores (lista-sucessores)
  (sort lista-sucessores #'> :key #'(lambda (no) (avaliar-no no *computador*))))

(defun alfabeta (no prof-max &optional (horaLimite (+ (get-internal-real-time) (* 20 1000))) (alpha -1000) (beta 1000))
  ; se o tempo limite foi ultrapassado, ou o no e' terminal ou a profundidade max foi atingida
  (cond ((or (> (get-internal-real-time) horaLimite) (no-terminal-p no) (= prof-max (no-profundidade no)))
         ; return the heuristic value of node
         (analisa-no no *computador*))
        ((eq (no-profundidade no) 0)
         (jogadorMax (ordena-sucessores (no-sucessores no *computador*)) prof-max horaLimite alpha beta))
        ; if maximizingPlayer
        ((eq 'MAX (no-tipo no))
         (jogadorMax (no-sucessores no *computador*) prof-max horaLimite alpha beta))
        ; else
        (t (jogadorMin (no-sucessores no *humano*) prof-max horaLimite alpha beta))))

(defun jogadorMax (lista-sucessores prof-max horaLimite alpha beta)
  ;; definimos o melhor valor como sendo o maximo entre o alpha e o AB(sucessor)
  (let ((melhor-valor (max alpha (alfabeta (car lista-sucessores) prof-max horaLimite alpha beta))))
    (progn
      (cond ((not (= melhor-valor alpha)) (setf *jogada* (no-tabuleiro (car lista-sucessores)))) (t nil))
    (cond ((>= melhor-valor beta) ;; se a >= ß, entao devolvemos beta (corte!)
             (and (corte-beta) beta))
          ((null (cdr lista-sucessores)) ;; se ja nao existirem sucessores devolvemos o melhor-valor ate agora
           melhor-valor)
          ;;caso contrario chamamos a funçao para os restantes sucessores
          (t (jogadorMax (cdr lista-sucessores) prof-max horaLimite melhor-valor beta))))))

(defun jogadorMin (lista-sucessores prof-max horaLimite alpha beta)
  ;; definimos o melhor valor como sendo o minimo entre o beta e o AB(sucessor)
  (let ((melhor-valor (min beta (alfabeta (car lista-sucessores) prof-max horaLimite alpha beta))))
    (cond ((<= melhor-valor alpha) ;; se ß <= a, entao devolvemos alpha (corte!)
           (and (corte-alfa) alpha))
          ((null (cdr lista-sucessores)) ;; se ja nao existirem sucessores devolvemos o melhor-valor ate agora
           melhor-valor)
          ;;caso contrario chamamos a funçao para os restantes sucessores
          (t (jogadorMin (cdr lista-sucessores) prof-max horaLimite alpha melhor-valor)))))