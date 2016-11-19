;;; Variáveis de Teste

;;; Tabuleiros de teste

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
(defun tabuleiro-teste4 () "Retorna um tabuleiro com 1 caixa fechada de dimensão 2 x 3"
	(list '((NIL T) (NIL T) (NIL NIL) (NIL T))
			'((NIL T T) (T NIL NIL) (NIL T NIL))
	)
)

;; tabuleiro-teste5
(defun tabuleiro-teste5 () "Retorna um tabuleiro com 3 caixa fechada de dimensão 3 x 3"
	(list '((T T	 NIL) (NIL T T) (NIL T T) (NIL T T))
			'((NIL T T) (NIL T NIL) (NIL T T) (NIL T T))
	)
)



;;; no-teste
(defun no-teste () "Define um no teste do problema dos Pontos e das Caixas em que Tabuleiro = tabuleiro-teste3, profundidade=0, heuristica = 0 e no-pai=NIL"
	(list (tabuleiro-teste3) 0 0 nil)
)
