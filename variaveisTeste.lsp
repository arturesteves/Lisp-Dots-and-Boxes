;;; Variáveis de Teste

;;; Tabuleiros de teste


;;
(defun tab-teste ()
	(list '((NIL NIL) (T NIL) (NIL NIL) (NIL T) (NIL T))
			'((NIL NIL NIL NIL) (NIL T T T) (NIL NIL NIL T))
	)
)


;; tabuleiro-teste0
(defun tabuleiro-teste0 () "Retorna um tabuleiro sem caixas fechadas de dimensão 3 x 1"
	(list '((NIL T T) (NIL NIL T))
			'((NIL) (NIL) (T) (T))
	)
)

;; tabuleiro-teste1
(defun tabuleiro-teste1 () "Retorna um tabuleiro sem caixas fechadas de dimensão 3 x 3"
	(list '((NIL T T) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))
			'((NIL NIL NIL) (T T T) (NIL NIL NIL) (NIL T NIL))
	)
)

;; tabuleiro-teste2
(defun tabuleiro-teste2 () "Retorna um tabuleiro com 1 caixa fechada de dimensão 3 x 3"
	(list '((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))
			'((NIL NIL NIL) (NIL T NIL) (NIL T NIL) (NIL NIL NIL))
	)
)

;; tabuleiro-teste3
(defun tabuleiro-teste3 () "Retorna um tabuleiro com 1 caixa fechada de dimensão 3 x 3"
	(list '((NIL T	 NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))
			'((NIL T NIL) (NIL T NIL) (NIL NIL NIL) (NIL NIL NIL))
	)
)

;; tabuleiro-teste4
(defun tabuleiro-teste4 () "Retorna um tabuleiro com 1 caixa fechada de dimensão 3 x 2"
	(list '((NIL T) (NIL T) (NIL NIL) (NIL T))
			'((NIL T T) (T NIL NIL) (T NIL NIL))
	)
)

;; tabuleiro-teste5
(defun tabuleiro-teste5 () "Retorna um tabuleiro com 3 caixas fechadas de dimensão 3 x 3"
	(list '((T T	 NIL) (NIL T T) (NIL T T) (NIL T T))
			'((NIL T T) (NIL T NIL) (NIL T T) (NIL T T))
	)
)


;;; Tabuleiros Enunciado

;; tabuleiro-a
(defun tabuleiro-a () "Tabuleiro A, com 1 caixa fechada de dimensão 3 x 3"
	(list '((NIL NIL NIL) (NIL NIL T) (NIL T T) (NIL NIL T))
			'((NIL NIL NIL) (NIL T NIL) (NIL NIL T) (NIL T T))
	)
)


;; tabuleiro-b
(defun tabuleiro-b () "Tabuleiro B, com 5 caixas fechadas de dimensão 4 x 4"
	(list '((NIL NIL T NIL) (T T T T) (NIL NIL T T) (NIL NIL T T) (NIL NIL T T))
			'((NIL NIL T T) (NIL NIL T T) (T T T T) (T NIL T T) (NIL T T T))
	)
)

;; tabuleiro-c
(defun tabuleiro-c () "Tabuleiro C, com 4 caixas fechadas de dimensão 4 x 4 "
	(list '((NIL NIL T NIL) (T NIL T T) (NIL NIL T T) (NIL NIL T T) (NIL NIL T T))
			'((NIL NIL T T) (NIL NIL T T) (NIL NIL T T) (T NIL T T) (NIL T T T ))			
	)
)


;; tabuleiro-d
(defun tabuleiro-d () "Tabuleiro D, com 0 caixas fechadas de dimensão 4 x 5"
	(list '((NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL))
			'((NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL))
	)
)


;; tabuleiro-e
(defun tabuleiro-e () "Tabuleiro E, com 2 caixas fechadas de dimensão 6 x 6"
	(list '((NIL NIL NIL T NIL NIL) (NIL NIL NIL T T T) (T T T T T NIL) (NIL NIL NIL T T NIL) (NIL NIL NIL T T NIL) (NIL NIL T T T T) (NIL NIL T NIL NIL NIL))
			'((NIL NIL NIL T T T ) (NIL T NIL NIL T T ) (NIL T T NIL T T ) (NIL NIL T T NIL NIL) (T NIL T NIL T NIL) (NIL NIL T T NIL NIL) (NIL T T T T T))
	)
)


;; tabuleiro-f
(defun tabuleiro-f () "Tabuleiro F, com 0 caixas fechadas de dimensão 7 x 7"
	(list '((NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL) (NIL T NIL NIL NIL NIL NIL) (NIL T NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL))
			'((NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL T NIL NIL) (NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL) (NIL NIL NIL NIL NIL NIL NIL))
	)
)





;;; no-teste
(defun no-teste () "Define um no teste do problema dos Pontos e das Caixas em que Tabuleiro = tabuleiro-teste3, profundidade=0, heuristica = 0 e no-pai=NIL"
	(list (tabuleiro-teste3) 0 0 nil)
)


