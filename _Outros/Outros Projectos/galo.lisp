;;;; galo.lisp
;;;; Projeto 2 - IA 1 Semestre Ano Lectivo 2015-2016
;;;; Codigo relacionado com o problema 
;;;; Autor: João Silva (120221053) & Joao Guerreiro (110221049)

(defparameter *no-teste* '((((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))
                            ((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))
                            ((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)))
                           0))

(defun cria-tabuleiro ()
  (list '((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))
        '((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))
        '((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))))

;;; GETTER - TABULEIRO
;; Get Nivel

(defun get-nivel (nivel tabuleiro)
  "Retorna o nivel do estado"
  (cond ((= nivel 3) (car tabuleiro))
        ((= nivel 2) (cadr tabuleiro))
        ((= nivel 1) (caddr tabuleiro))
        (t (format t "Nao existe nivel ~d" nivel))))

;; Get Linha

(defun get-linha (x nivel)
  "Retonra a linha do estado"
  (cond ((= x 1) (car nivel))
        ((= x 2) (cadr nivel))
        ((= x 3) (caddr nivel))
        (t (format t "Nao existe linha ~d" x))))

;; Get Elemento Linha

(defun get-elemento-linha (y linha)
 "Retorna o Elemento de uma Linha"
  (cond ((= y 1) (car linha))
        ((= y 2) (cadr linha))
        ((= y 3) (caddr linha))
        (t (format t "Nao existe elemento ~d" y))))

;; Get Casa

(defun get-casa (x y z tabuleiro)
 "Retorna Elemento de uam Posicao"
  (get-elemento-linha y (get-linha x (get-nivel z tabuleiro))))

;;; SETTER
;; Colocar Simbolo

(defun colocar-simbolo(x y z tabuleiro simbolo) 
  "Coloca o Simbolo na Posicao Desejada"
  (cond ((null tabuleiro) nil)
        ;;((verifica-casa (get-casa x y z tabuleiro)) (format t "Posição Invalida")) 
        ((= z 3) (cons (set-nivel x y (car tabuleiro) simbolo) (colocar-simbolo x y (+ z 1) (cdr tabuleiro) simbolo)))
        (t (cons (car tabuleiro) (colocar-simbolo x y (+ z 1) (cdr tabuleiro) simbolo)))))

;; Set Nivel

(defun set-nivel (x y nivel simbolo)
 "Altera o Nivel Onde se Pretende Inserir a Peça"
  (cond ((null nivel) nil)
        ((= x 1) (cons (set-linha y (car nivel) simbolo) (set-nivel (- x 1) y (cdr nivel) simbolo)))
        (t (cons (car nivel) (set-nivel (- x 1) y (cdr nivel) simbolo)))))

;; Set Linha

(defun set-linha (y linha simbolo)
  "Altera a Posicao onde se Pretende Inserir a Peça"
   (cond ((= y 1) (list simbolo (get-elemento-linha 2 linha) (get-elemento-linha 3 linha)))
         ((= y 2) (list (get-elemento-linha 1 linha) simbolo (get-elemento-linha 3 linha)))
         ((= y 3) (list (get-elemento-linha 1 linha) (get-elemento-linha 2 linha) simbolo))))

;;; LOGICAS
;; E Simbolo

(defun e-simbolo (casa simbolo)
      (eq casa simbolo))

;; E Cruz

(defun e-cruz (casa)
  (e-simbolo casa 'X))

;; E Bola

(defun e-bola (casa)
  (e-simbolo casa 'O))

;; E Vazio

(defun e-vazio (casa)
  (e-simbolo casa nil))

;; Verificar Casa

(defun verifica-casa (casa)
   (cond ((null casa) nil)
         (t)))

;;; ESTRUTURA NO

;;; CONSTRUTOR
;; cria-no
(defun cria-no (estado &optional (profundidade 0))
  (let ((novo-no (list estado profundidade)))
    (cond ((null estado)
           novo-no)
          (t (list estado profundidade)))))

;; GETTERS
(defun no-tabuleiro (no)
  "Retorna o tabuleiro de um no"
  (car no))

(defun no-profundidade (no)
  "Retorna o tabuleiro de um no"
  (cadr no))

(defun no-tipo (no)
"Retorna o tipo do no em funcao da sua profundidade" 
  (cond 
   ((evenp (no-profundidade no)) 'MAX)
   (T 'MIN)))

;;; OPERADORES
;; colocar-cruz
(defun colocar-cruz (x y z tabuleiro)
  (colocar-simbolo x y z tabuleiro 'X))

;; colocar-bola
(defun colocar-bola (x y z tabuleiro)
  (colocar-simbolo x y z tabuleiro 'O))

;;; SOLUCAO
;; solucaop
(defun solucaop (no)
  (let ((tabuleiro (no-tabuleiro no)))
    (or (nivel-final? 1 tabuleiro)
        (nivel-final? 2 tabuleiro)
        (nivel-final? 3 tabuleiro)
        (seccao-final? 1 tabuleiro)
        (seccao-final? 2 tabuleiro)
        (seccao-final? 3 tabuleiro)
        (diagonal-final? tabuleiro))))

;; nivel-final?
(defun nivel-final? (z tabuleiro)
  (or (colunas-solucao? z tabuleiro)
      (linhas-solucao? z tabuleiro)
      (diagonal-solucao? z tabuleiro)))

;;colunas-solucao?
(defun colunas-solucao? (z tabuleiro &optional (y 1))
  (cond ((= y 4) nil)
        (t (or (tres-iguais? (get-casa 1 y z tabuleiro)
                             (get-casa 2 y z tabuleiro)
                             (get-casa 3 y z tabuleiro))
               (colunas-solucao? z tabuleiro (1+ y))))))

;;linhas-solucao?
(defun linhas-solucao? (z tabuleiro &optional (x 1))
  (cond ((= x 4) nil)
        (t (or (tres-iguais? (get-casa x 1 z tabuleiro)
                             (get-casa x 2 z tabuleiro)
                             (get-casa x 3 z tabuleiro))
               (linhas-solucao? z tabuleiro (1+ x))))))

;;diagonal-solucao?
(defun diagonal-solucao? (z tabuleiro)
  (or (tres-iguais? (get-casa 1 1 z tabuleiro)
                    (get-casa 2 2 z tabuleiro)
                    (get-casa 3 3 z tabuleiro))
      (tres-iguais? (get-casa 1 3 z tabuleiro)
                    (get-casa 2 2 z tabuleiro)
                    (get-casa 3 1 z tabuleiro))))

;;seccao-final?
(defun seccao-final? (y tabuleiro)
  (or (colunas-h-solucao? y tabuleiro)
      (diagonal-y-solucao? y tabuleiro)
      (diagonal-x-solucao? y tabuleiro)))

;;colunas-h-solucao?
(defun colunas-h-solucao? (y tabuleiro &optional (x 1))
  (cond ((= x 4) nil)
        (t (or (tres-iguais? (get-casa x y 1 tabuleiro)
                             (get-casa x y 2 tabuleiro)
                             (get-casa x y 3 tabuleiro))
               (colunas-h-solucao? y tabuleiro (1+ x))))))

;;diagonal-y-solucao?
(defun diagonal-y-solucao? (y tabuleiro)
  (or (tres-iguais? (get-casa 1 y 1 tabuleiro)
                    (get-casa 2 y 2 tabuleiro)
                    (get-casa 3 y 3 tabuleiro))
      (tres-iguais? (get-casa 1 y 3 tabuleiro)
                    (get-casa 2 y 2 tabuleiro)
                    (get-casa 3 y 1 tabuleiro))))

;;diagonal-x-solucao?
(defun diagonal-x-solucao? (x tabuleiro)
  (or (tres-iguais? (get-casa x 1 1 tabuleiro)
                    (get-casa x 2 2 tabuleiro)
                    (get-casa x 3 3 tabuleiro))
      (tres-iguais? (get-casa x 1 3 tabuleiro)
                    (get-casa x 2 2 tabuleiro)
                    (get-casa x 3 1 tabuleiro))))

;;diagonal-final?
(defun diagonal-final? (tabuleiro)
  (or (tres-iguais? (get-casa 1 1 3 tabuleiro)
                    (get-casa 2 2 2 tabuleiro)
                    (get-casa 3 3 1 tabuleiro))
      (tres-iguais? (get-casa 3 3 3 tabuleiro)
                    (get-casa 2 2 2 tabuleiro)
                    (get-casa 1 1 1 tabuleiro))
      (tres-iguais? (get-casa 3 1 3 tabuleiro)
                    (get-casa 2 2 2 tabuleiro)
                    (get-casa 1 3 1 tabuleiro))
      (tres-iguais? (get-casa 1 3 3 tabuleiro)
                    (get-casa 2 2 2 tabuleiro)
                    (get-casa 3 1 1 tabuleiro))))

;;tres-iguais?
(defun tres-iguais? (a b c)
  (cond ((and (eq a 'O) (eq a b) (eq a c))
         'O)
        ((and (eq a 'X) (eq a b) (eq a c))
         'X)
        (t nil)))

;; avaliar-folha
(defun avaliar-folha (no peca)
  "Recebe um no final e uma peca e devolve a avaliacao do no"
  (let ((vencedor (solucaop no)))
    (cond ((null vencedor) 0)
          ((eq vencedor peca) 100)
          (t -100))))

;; avaliar-folha-limite
(defun avaliar-folha-limite (no peca)
  "Recebe um no nao final, devolve 50 ou -50 se igual a peca na posicao (2 2 2)"
  (let ((casa-central (get-casa 2 2 2 (no-tabuleiro no))))
    (cond ((null casa-central) 0)
          ((eq casa-central peca) 50)
          (t -50))))

;; avaliar-no
(defun avaliar-no (no peca)
  (cond ((no-terminal-p no) (avaliar-folha no peca))
        (t (avaliar-folha-limite no peca))))

;; no-terminal-p
(defun no-terminal-p (no)
  (or (solucaop no)
      (tabuleiro-preenchido-p (no-tabuleiro no))))

;; tabuleiro-preenchido-p
(defun tabuleiro-preenchido-p  (tabuleiro &optional (x 1) (y 1) (z 1))
  "Verifica se o tabuleiro ja esta completamente preenchido"
  (cond ((= x 4) (tabuleiro-preenchido-p tabuleiro 1 (1+ y) z))
        ((= y 4) (tabuleiro-preenchido-p tabuleiro x 1 (1+ z)))
        ((= z 4) t)
        (t (and  (get-casa x y z tabuleiro)
                 (tabuleiro-preenchido-p tabuleiro (1+ x) y z)))))

;; no-sucessores
(defun no-sucessores (no peca)
  "Recebe um no e uma peca, devolve os sucessores do tabuleiro para a peca"
  (let ((f-operador 
         ((lambda (peca) (cond ((eq peca 'O) 'colocar-bola) (t 'colocar-cruz))) 
          peca)))
    (sucessores no f-operador)))

;; sucessores
(defun sucessores (no f-operador &optional (x 1) (y 1) (z 1))
  (cond ((null f-operador) nil) ; verifica se ja percorreu as casas todas
        ((= z 4)
         (sucessores no f-operador x (1+ y) 1)) ;verifica a coordenada z
        ((= y 4)
         (sucessores no f-operador (1+ x) 1 z)) ;verifica a coordenada y
        ((= x 4) ; se foram percorridas todas as posicoes
         (sucessores no nil 1 1 1)) ; vamos colocar o f-operador a nil para interromper a iteracao
        (t (let ((posicao (get-casa x y z (no-tabuleiro no))))
             ;; se ja tem alguma coisa entao chamamos novamente o sucessores
             (cond ((not (e-vazio posicao)) (sucessores no f-operador x y (1+ z)))
                   (t ;; vamos gerar o sucessor e passar ao proximo
                      (let ((sucessor (sucessores-aux no f-operador x y z)))
                        (cons sucessor (sucessores no f-operador x y (1+ z))))))))))

;; sucessores-aux
(defun sucessores-aux (no nome-operador x y z)
  (cria-no (funcall nome-operador x y z (no-tabuleiro no))
           (1+ (no-profundidade no))))

;; escolhe-melhor-jogada
(defun escolhe-melhor-jogada (tabuleiro hora-limite)
  "Escolhe a melhor jogada com base ou numa estrategia ou no algoritmo minimax"
  (progn
    (setf *jogada* nil)
    (repoe-estatisticas)
    (alfabeta (cria-no tabuleiro) 10 hora-limite)
    (devolve-jogada tabuleiro *jogada*))
)

;; devolve-jogada
(defun devolve-jogada (tabA tabB &optional (x 1) (y 1) (z 1))
  (cond ((= x 4) (devolve-jogada tabA tabB 1 (1+ y) z))
        ((= y 4) (devolve-jogada tabA tabB x 1 (1+ z)))
        ((= z 4) nil) ;; nao deve ocorrer
        ((not (eq (get-casa x y z tabA)
                  (get-casa x y z tabB)))
         (list x y z))
        (t (devolve-jogada tabA tabB (1+ x) y z))))

;; faz-jogada
(defun faz-jogada (peca pos tabuleiro)
  "Cria o novo tabuleiro que consiste na jogada de um dos jogadores."
  (let ((x (first pos))
        (y (second pos))
        (z (third pos)))
    (colocar-simbolo x y z tabuleiro peca)))