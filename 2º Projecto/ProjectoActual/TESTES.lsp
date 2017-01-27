
;; tabuleiro-teste1
(defun tabuleiro-teste1 () "Retorna um tabuleiro sem caixas fechadas de dimensão 3 x 3"
	(list '((NIL 1 1) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))
			'((NIL NIL NIL) (1 2 2) (NIL NIL NIL) (NIL 2 NIL))
	)
)
(defun no-teste-1 ()
	(cria-no (tabuleiro-teste1) 0 nil 0 0)
)


(defun tabuleiro-teste1-fecha-1-caixa () "Retorna um tabuleiro sem caixas fechadas de dimensão 3 x 3"
	(list '((1 1 NIL) (NIL NIL NIL) (NIL NIL NIL) (NIL NIL 1))
			'((1 NIL NIL) (2 1 2) (NIL NIL NIL) (NIL 2 1))
	)
)

(defun no-teste-1-fecha-1-caixa()
	(cria-no (tabuleiro-teste1-fecha-1-caixa) 0 nil 0 0)
)

;;Testar os sucessores:
;; sem fechar caixas

;(defun sucessores-alfabeta (no operadores profundidade peca funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
(sucessores-alfabeta (no-teste-1) (operadores) 1 1 'funcao-utilidade 0 0)


;;a fechar caixas:
(sucessores-alfabeta (no-teste-1-fecha-1-caixa) (operadores) 1 1 'funcao-utilidade 0 0)


#|
> (sucessores-alfabeta (no-teste-1-fecha-1-caixa) (operadores) 1 1 'funcao-utilidade 0 0)
((((# # # #) (# # # #)) 1 NIL 0 0) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 2 NIL 1 1) 
 (((# # # #) (# # # #)) 1 NIL 0 0) 
 (((# # # #) (# # # #)) 1 NIL 0 0) 
 (((# # # #) (# # # #)) 1 NIL 0 0) 
 (((# # # #) (# # # #)) 1 NIL 0 0) 
 (((# # # #) (# # # #)) 1 NIL 0 0) 
 (((# # # #) (# # # #)) 1 NIL 0 0) 
 (((# # # #) (# # # #)) 1 NIL 0 0) 
 (((# # # #) (# # # #)) 1 NIL 0 0) 
 (((# # # #) (# # # #)) 1 NIL 0 0) 
 (((# # # #) (# # # #)) 1 NIL 0 0) 
 (((# # # #) (# # # #)) 1 NIL 0 0) 
 (((# # # #) (# # # #)) 1 NIL 0 0) 
 (((# # # #) (# # # #)) 1 NIL 0 0)
)
|#





;;alfa-beta
(alfa-beta (no-teste-1) 4 1 'funcao-utilidade)

(alfa-beta (no-teste-1-fecha-1-caixa) 4 1 'funcao-utilidade)

(imprime-tabuleiro (tabuleiro-teste1))
