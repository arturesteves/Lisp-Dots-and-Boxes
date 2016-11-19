;;;; projecto.lisp
;;;; Disciplina de IA - 2016 / 2017
;;;; Programador: Artur Esteves - 140221076
;;;; Funções de interacção com o utilizador, de carregamento dos outros ficheiros do projecto e leitura e escrita em ficheiros

;;;; Tempo
;;;; Antes dia 19/11/2016 : 1 hora
;;;; 19/11/2016: 13:13 - 14:57: 1 hora e 37 minutos

#||

LAB 7



ADAPTAR PARA O PROBLEMA
||#



;;; Inicializacao do programa
;; iniciar
(defun iniciar ()
"Permite iniciar o programa, fazendo a leitura do teclado do estado inicial e do algoritmo a utilizar para procurar a solução (neste caso a procura na profundidade ou na largura)"
  (let* ((no (cria-no (ler-vasilhas)))
         (algoritmo (ler-algoritmo))
         (profundidade (cond ((eql algoritmo 'dfs) (ler-profundidade)) (T 9999))) )
      (escreve-no (procura-generica no profundidade 'solucaop 'sucessores algoritmo (operadores))
  )))

;;; Input - interface que permite ler os valores inicias das vasilhas junto do utilizador.
(defun ler-no-inicial (&optional (f t))
  (read f))

(defun ler-vasilhas ()
"Permite ler do teclado o estado inicial do problema das vasilhas."
  (let (( get-vasilha-a (ler-vasilha "A")) (vasilha-b (ler-vasilha "B")))
    (list  get-vasilha-a vasilha-b)
    )
)

(defun ler-vasilha (vasilha)
"Permite ler do teclado o valor inicial de uma vasilha.
A função verifica que os valores lidos pertencem ao intervale esperado para cada vasilha."
(progn
    (format t "Insere o valor da vasilha ~A ~%" vasilha)
    (let ((valor (read)))
      (cond
        ((AND (equal vasilha "A") (OR (> valor 3) (< valor 0))) (progn (format t "Valor invalido ~%") (ler-vasilha vasilha)))
        ((AND (equal vasilha "B") (OR (> valor 5) (< valor 0))) (progn (format t "Valor invalido ~%") (ler-vasilha vasilha)))
        (T valor)
      )
  )
))

;; ler-algoritmo
(defun ler-algoritmo ()
"Permite fazer a leitura do algoritmo a utilizar."
  (progn
    (format t "Que algoritmo quer usar para procurar? ~%")
    (format t "1- Procura na largura ~%")
    (format t "2- Procura na profundidade ~%")
    (let ((resposta (read)))
      (cond ((= resposta 1) 'bfs)
            (T 'dfs)))
    )
  )
;; ler-profundidade
(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite para o algoritmo dfs."
    (progn
    (format t "Qual a profundidade limite? ~%")
    (read)
    ))


;;; Output - escrita do estado do problema
;;
(defun escrever-no (no &optional (g t))
"Permite escrever um no, por defeito no ecra."
  (format g "~A" no))

 (defun escreve-no (lista)
 "Permite escrever no ecra um no do problema."
  (progn
     (format t "| A: ~a | B: ~a | G: ~a |~%" ( get-vasilha-a (car lista)) (get-vasilha-b (car lista)) (get-profundidade-no (car lista)))
     (format t "Pai: ~a ~%" ( get-pai-no (car lista)))
  ))

  (defun escreve-lista-nos (lista)
  "Permite escrever no ecra uma lista de nos do problema das vasilhas, e.g. um conjunto de sucessores, a lista de abertos etc."
  (cond
   ((null lista) nil)
   (T (progn (escreve-no (car lista)) (escreve-lista-nos (cdr lista))))))
