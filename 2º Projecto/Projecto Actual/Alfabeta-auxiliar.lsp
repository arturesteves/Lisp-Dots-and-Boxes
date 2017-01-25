 ;;; Variáveis Globais
(defvar *corte-alfa* 0)
(defvar *corte-beta* 0)
(defvar *jogada-pc* nil) ;;variavel que guarda o tabuleiro que corresponde a melhor jogada que é atualizada pelo algoritmo cada vez que o valor de beta é atualizado.
(defvar *nos-analisados* 0) ;; variavel que guarda o numero de nos visitados
(defvar *tempo-despendido*)

 

 #||
 Confirmar a função: no-folhap (apenas verifico se não tem sucessores, se não tiver é porque é terminal)
 
 ; o valor 5000 é recebido!
													;;; (alfa -10e11) (beta 10e11)
 ||#												;;;Ou então [-1000, +1000]
(defun alfa-beta (no profundidade-limite peca f-utilidade &optional (alfa -1000) (beta 1000) &aux ((tempo-inicial (get-universal-time)) 	;; tempo de quando começou a 1ª procura no alfabeta
																																	(tempo-maximo (5000)))) ; 5 milisegundos
	(let* ((peca-a-jogar (cond ((= (get-no-profundidade no) 0) peca) (T (troca-peca peca))))
			(no-max (e-no-maxp no))
			(caixas-jogador-1 (get-caixas-jogador-1 no))
			(caixas-jogador-2 (get-caixas-jogador-2 no)) 
			;(tempo-limite-margem (* tempo-maximo 0.9))	; 90% do tempo limite	-> aplicar se for usado o campeonato
			(tempo-actual (get-universal-time))	;; tempo de quando começou a n pesquisa
			(tempo-gasto (- tempo-actual tempo-inicial))
			(tempo-dispendido (setf *tempo-despendido* tempo-gasto))
			)
			
		(cond
			((or  (>= tempo-gasto tempo-actual) 
					(no-folhap no) 
					(= profundidade-limite (get-no-profundidade no))) (progn 
																									(setf *nos-analisados* (+ *nos-analisados* 1))	
																									(funcall f-utilidade no)))
																						
			(no-max (max-side (sucessores-alfabeta-teste no (operadores) profundidade-limite peca-a-jogar f-utilidade caixas-jogador-1 caixas-jogador-2) profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo))		
			; equivalente a ((not no-max) ())
			(T (min-side (sucessores-alfabeta-teste no (operadores) profundidade-limite peca-a-jogar f-utilidade caixas-jogador-1 caixas-jogador-2) profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo))
		)
	)
)

(defun max-side (sucessores profundidade-limite peca f-utilidade alfa beta tempo-inicial tempo-maximo)
	(cond
		((null sucessores) nil)
		(T (let ((valor-utilidade-no (alfa-beta (car sucessores) profundidade-limite peca f-utilidade alfa beta tempo-inicial tempo-maximo)))
			(cond
			
			;; check 	Aqui algures é preciso fazer update da melhor jogada
			;;; Tenho que ir buscar o maior dos sucessores? e esse fica a melhor jogada?
				#||
				Old: ((> valor-utilidade-no alfa) (max-side (cdr sucessores) profundidade-limite peca f-utilidade valor-utilidade-no beta tempo-inicial tempo-maximo))
				||#
				((> valor-utilidade-no alfa) (progn 
															(setf *jogada-pc* (car sucessores))		;; Actualiza a melhor jogada!
															(max-side (cdr sucessores) profundidade-limite peca f-utilidade valor-utilidade-no beta tempo-inicial tempo-maximo)))
				((>= alfa beta) (setf *corte-beta* (+ *corte-beta* 1)) beta) ; houve corte alfa
				(T alfa)
			)
		))
	)
)


 
(defun min-side (sucessores profundidade-limite peca f-utilidade alfa beta tempo-inicial tempo-maximo)
	(cond
		((null sucessores) nil)
		(T (let ((valor-utilidade-no (alfa-beta (car sucessores) profundidade-limite peca f-utilidade alfa beta tempo-inicial tempo-maximo)))
			(cond
			
			;; check	Aqui algures é preciso fazer update da melhor jogada
			;;; Tenho que ir buscar o menor dos sucessores? e esse fica a melhor jogada?
				((> valor-utilidade-no beta) (progn
																(setf *jogada-pc* (car sucessores))
																(min-side (cdr sucessores) profundidade-limite peca f-utilidade alfa valor-utilidade-no tempo-inicial tempo-maximo)))
				((<= beta alfa) (setf *corte-alfa* (+ *corte-alfa* 1)) alfa) ; houve corte beta
				(T beta)
			)
		))
	)
)
 
 
 
(defun e-no-maxp (no)
	(let ((profundidade (get-no-profundidade no)))
		(cond
			((or (= profundidade 0) (evenp profundidade)) T)
			(T nil)
		)
	)
)
 
(defun troca-peca (peca)
	(cond
		((= peca 1) 2)
		(T 1)
	)
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alfa-beta (no profundidade-limite peca max-or-min &optional (alfa 2) (beta 2) &aux (tempo-inicial (get-universal-time)))
	(cond
		((or (= profundidade-limite 0) (no-folhap no)) (funcao-utilidade no))
		((eql max-or-min 'MAX) (max-side (sucessores no (operadores) profundidade-limite peca (get-caixas-jogador-1 no) (get-caixas-jogador-2 no)) profundidade-limite alfa beta))
		(T (min-side (sucessores no (operadores) profundidade-limite peca (get-caixas-jogador-1 no) (get-caixas-jogador-2 no)) profundidade-limite alfa beta))  ;; (eql 'MIN) 
	)
)


(defun max-side (sucessores profundidade-limite alfa beta)
	(cond
		((null sucessores) nil)
		(T (let ((valor-utilidade-no (alfa-beta (car sucessores) (- profundidade-limite 1) 'MIN alfa beta)))
			(cond
				((> valor-utilidade-no alfa) (max-side (cdr sucessores) (- profundidade-limite 1) valor-utilidade-no beta)) 
				((>= alfa beta) (setf *corte-alfa* (+ *corte-alfa* 1)) beta) ; houve corte alfa
				(T alfa)
			)
		))
	)
)

(defun min-side (sucessores profundidade-limite alfa beta)
	(cond
		((null sucessores) nil)
		(T (let ((valor-utilidade-no (alfa-beta (car sucessores) (- profundidade-limite 1) 'MAX alfa beta)))
			(cond
				((> valor-utilidade-no beta) (min-side (cdr sucessores) (- profundidade-limite 1) alfa valor-utilidade-no)) 
				((<= beta alfa) (setf *corte-beta* (+ *corte-beta* 1)) alfa) ; houve corte beta
				(T beta)
			)
		))
	)
)