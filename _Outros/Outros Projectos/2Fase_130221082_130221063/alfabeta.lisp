;;;; projeto.lisp
;;;; Disciplina de IA - 2015 / 2016
;;;; vai conter a implementacao dos varios algoritmos de procura
;; Luis Serrano e David Mealha


(defparameter *no-teste* '((((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL)) ((NIL NIL NIL) (0 X X) (NIL NIL NIL)) ((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))) 0 0 nil X))
(defparameter *operadores* '(colocar-cruz colocar-circulo))
(defparameter *a-cut-off* 0) ;numero de cortes alfa
(defparameter *b-cut-off* 0) ;numero de corte beta
(defparameter *nos-analisados* 0) ;numero de nos visitados 
(defparameter *tempo-gasto* 0)
(defparameter *jogada* NIL)


;;; Construtores
;; cria
(defun cria-no (estado &optional(profundidade 0) (valor-heuristico 0) (no-pai NIL) (no-peca 'X))
"Fun��o que cria um novo no, recebendo 4 paramentros, onde 3 deles s�o opcionais, 
o estado representa o valor desse estado, a profundidade o nivel a que se encontra o no na arvore, 
o no pai do no que se pretende criar, caso seja um no inicial, o no pai � NIL e e por fim o seu valor
heuristico que e dado por uma funcao heuristica"
  (list estado profundidade valor-heuristico no-pai no-peca))

;;; Getters
;; no-estado
(defun no-estado (no)
  "Permite ler o estado, ou seja, o tabuleiro desse no"
  (first no)
)

;; no-profundidade
(defun no-profundidade (no)
  "Permite ler a profundidade do no"
  (second no)
)

;; no-valor-heuristico 
(defun no-valor-heuristico (no)
  "Permite saber o valor heuristico do no passado como parametro"
  (third no)
)

;; no-pai
(defun no-pai (no)
  "Permite saber o no-pai do no passado como parametro"
  (fourth no)
)

;; no-peca
(defun no-peca (no)
  "Permite ir buscar a pe�a utilizada no n�"
  (fifth no)
)

;; existep
(defun existep (no lista)
  "Permite verificar se um n� existe numa lista."
  (cond ((eq no (car lista)) T)  ;verifica se o no encontra-se na cabe�a da lista, se for retorna true
        ((null lista) nil)  ;caso n�o tenha sido encontrado o no na lista retorna nil
        (t () (existep no (cdr lista))) ;condi��o recursiva para percorrer a lista 
   )
)
;;;Algoritmo Alfa-Beta
;;(alfa-beta no-inicial profundidadeMax -inf +inf) 
(defun alfa-beta (no profMax &optional(alfa -10e11) (beta 10e11) (tempo-inicial 0) (tempo-max 5))
  "Fun��o alfabeta, com cortes"
  (let* ((peca-trocada (troca-peca (no-peca no)))
        (peca (no-peca no))
        (jogador (isMaxOrMin no))
        (aux (setf *nos-analisados* (1+ *nos-analisados*)))
        (tempo-atual (get-universal-time))
        (tempo-gasto (setf *tempo-gasto* (- tempo-atual tempo-inicial)))
        )
    (cond ((>= (- tempo-atual tempo-inicial) tempo-max) (avaliar-folha no peca jogador)) 
          ;((= (no-profundidade no) profMax) (avaliar-folha no peca jogador))
          ((solucao-p no peca) (avaliar-folha no peca jogador))
          ((= (no-profundidade no) 0)
           (alfa-value profMax (sucessores no peca) alfa beta tempo-inicial tempo-max) ;no 1� nivel nao precisa de trocar de pe�a, mas nos restantes precisa
           )
          ((eq jogador 'MAX)
           (alfa-value profMax (sucessores no peca-trocada) alfa beta tempo-inicial tempo-max) ;Maximizer sem defparameter
           )
          (T
           (beta-value profMax (sucessores no peca-trocada) alfa beta tempo-inicial tempo-max)
           )
          )
    )
)

;; Fun��o Maximizer
(defun alfa-value (profMax lista-sucessores alfa beta tempo-inicial tempo-max)
  "Fun��o para descobrir o valor mais alto entre o value e a lista de sucessores recebidos"
  (cond ((null lista-sucessores) alfa) ;condi��o de paragem
        (t (let* ((no-filho (car lista-sucessores))
                  (value-sucessor (alfa-beta no-filho profMax alfa beta tempo-inicial tempo-max)) ;vai invocar o alfabeta para o primeiro sucessor da lista
                  (new-value-alfa (verifica-sucessor-maior alfa value-sucessor no-filho))) ;vai buscar o maximo entre o alfa e o valor anteriormente calculado
             (cond ((<= beta new-value-alfa) (setf *b-cut-off* (1+ *b-cut-off*)) beta)
                   (t (alfa-value profMax (cdr lista-sucessores) new-value-alfa beta tempo-inicial tempo-max)) ;Faz set � jogada com o first dos sucessores?
                   )
             )
           )
        )
)

(defun verifica-sucessor-maior (alfa value-sucessor sucessor)
  "Fun��o que verifica se o valor devolvido pelo sucessor � maior, se sim, atualiza a jogada"
  (cond ((> value-sucessor alfa) (setf *jogada* sucessor) value-sucessor)
        (t alfa)
        )
)

(defun verifica-sucessor-menor (beta value-sucessor sucessor)
  "Fun��o que verifica se o valor devolvido pelo sucessor � menos, se sim atualiza a jogada"
  (cond ((< value-sucessor beta) value-sucessor)
        (t beta)
        )
)

;; Fun��o auxiliar troca-peca
(defun troca-peca (peca)
  "Fun��o para mudar de pe�a, caso o no do atual nivel tenha a pe�a x, o proximo
nivel tem de ter uma pe�a 0"
  (cond ((eq peca 'X) '0)
        (t 'X)
        )
)

;; Fun��o Minimizer
(defun beta-value (profMax lista-sucessores alfa beta tempo-inicial tempo-max)
  "Fun��o para descobrir o valor mais baixo entre o value e os valores dos sucessores"
  (cond ((null lista-sucessores) beta) ;condi��o de paragem
        (t (let* ((no-filho (car lista-sucessores)) 
                  ;vai invocar o alfabeta para o primeiro sucessor da lista
                  (value-sucessor (alfa-beta no-filho profMax alfa beta tempo-inicial tempo-max))
                  (new-value-beta (verifica-sucessor-menor beta value-sucessor no-filho)) ;vai buscar o minimo entre o alfa e o valor anteriormente calculado
                  )
             (cond ((<= new-value-beta alfa) (setf *a-cut-off* (1+ *a-cut-off*)) alfa)
                   (t (beta-value profMax (cdr lista-sucessores) alfa new-value-beta tempo-inicial tempo-max))
                   )
             )
           )
        )
  )

;; Fun��o isMax (FUNCAO ADICIONADA PARA 2 FASE)
(defun isMaxOrMin (no)
"Verificar se um no � MIN ou MAX, faz o mesmo que o isMax, s� que retorn MAX ou MIN, em vez de um valor l�gico"
  (let ((profundidade (second no)))
    (cond ((OR (evenp profundidade) (= profundidade 0)) 'MAX)
          (t 'MIN)
          )
    )
)

;;;Avaliar-folha, ir� servir como a fun��o de avalia��o para o alfabeta (FUNCAO ADICIONADA PARA 2 FASE)
;;
(defun avaliar-folha (no-final peca jogador)
  "Fun��o que calcula o valor a devolver de acordo com o jogador"
  (let* ((valor-f-avaliacao (f-avaliacao no-final peca))
        ;(x (format t "vou avaliar o no: ~a com a pe�a: ~a com o valor: ~a ~%" no-final peca valor-f-avaliacao))
        )
    (cond ;((AND (= (no-profundidade no-final) 1)(solucao-p no-final peca)) 100)
           ((= (calcula-simbolos-alinhados (no-estado no-final) peca) 3) (+ 25 valor-f-avaliacao))
           ;peca na casa central do tabuleiro central, casa com mais op��es de alinhamento de todo os tabuleiros
           ((equal (get-casa 2 2 2 (no-estado no-final)) peca) (+ 15 valor-f-avaliacao)) ;soma ao valor da fun��o
           ;pe�a na casa central do primeiro e terceiro tabuleiro, a seguir � casa central do tabuleiro central, s�o as que tem mais op��es de alinhamento 

          (t valor-f-avaliacao)
          )
    )
)

;;Fun��o-avalia��o (FUNCAO ADICIONADA PARA 2 FASE)
(defun f-avaliacao (no peca)
  "Fun��o de avalia��o que ter� em conta as pe�as alinhadas nas linhas, colunas e diagonais.
   A formula a seguir, ser� a fornecida nas aulas, que consiste nas linhas/colunas/diagonais 
   com 2 pe�as iguais alinhadas, vezes 1.5, mais linhas/colunas/diagonais com 1 pe�a alinhas, 
   isto tudo menos a mesma coisa para a pe�a contr�ria"
  (let* ((estado (no-estado no))
         (peca-adversario (troca-peca peca))
         (lista-valores-alinhados (append 
                                   (verifica-estado estado 0 peca) 
                                   (verifica-linhas (inverte-linhas-para-linhas3D estado) 0 peca)
                                   (verifica-linhas (inverte-linhas-para-colunas-diagonais estado) 0 peca) 
                                   (verifica-linhas (inverte-linhas-para-colunas estado) 0 peca ) 
                                   (verifica-linhas (inverte-linhas-para-colunas3D estado) 0 peca) 
                                   (verifica-linhas (inverte-linhas-para-diagonais estado) 0 peca) 
                                   (verifica-linhas (inverte-linhas-para-diagonais3D estado) 0 peca)))
        (lista-valores-alinhados-oponente (append 
                                           (verifica-estado estado 0 peca-adversario) 
                                           (verifica-linhas (inverte-linhas-para-linhas3D estado) 0 peca-adversario)
                                           (verifica-linhas (inverte-linhas-para-colunas-diagonais estado) 0 peca-adversario) 
                                           (verifica-linhas (inverte-linhas-para-colunas estado) 0 peca-adversario) 
                                           (verifica-linhas (inverte-linhas-para-colunas3D estado) 0 peca-adversario) 
                                           (verifica-linhas (inverte-linhas-para-diagonais estado) 0 peca-adversario) 
                                           (verifica-linhas (inverte-linhas-para-diagonais3D estado) 0 peca-adversario)))
        (nr-pecas-alinhadas (numero-pecas-alinhadas lista-valores-alinhados))
        (qtd-duas-pecas (first nr-pecas-alinhadas)) ;primeiro valor da lista tem quantidade de duas pe�as alinhadas
        (qtd-uma-peca (second nr-pecas-alinhadas)) ;quantidade de uma pe�a alinhada
        (qtd-tres-pecas (third nr-pecas-alinhadas))

        ;contagem do oponente
        (nr-pecas-alinhadas-oponente (numero-pecas-alinhadas lista-valores-alinhados-oponente))
        (qtd-duas-pecas-op (first nr-pecas-alinhadas-oponente)) 
        (qtd-uma-peca-op (second nr-pecas-alinhadas-oponente))
        (qtd-tres-pecas-op (third nr-pecas-alinhadas-oponente))
        )
    (- (+ (* 2 qtd-tres-pecas)(* 1.5 qtd-duas-pecas) qtd-uma-peca) (+ (* 2 qtd-tres-pecas-op)(* 1.5 qtd-duas-pecas-op) qtd-uma-peca-op))
    )
)

;;Numero-pecas-alinhadas (FUNCAO ADICIONADA PARA 2 FASE)
(defun numero-pecas-alinhadas (lista-valores &optional(nr-2 0)(nr-1 0)(nr-3 0))
  "Fun��o que devolve uma lista, onde o primeiro membro s�o a quantidade de alinhamentos com duas pe�as, e o segundo o alinhamentos com 1 pe�a"
  (cond ((null lista-valores) (list nr-2 nr-1 nr-3))
        ((= (car lista-valores) 2) (numero-pecas-alinhadas (cdr lista-valores) (1+ nr-2) nr-1))
        ((= (car lista-valores) 1) (numero-pecas-alinhadas (cdr lista-valores) nr-2 (1+ nr-1)))
        ((= (car lista-valores) 3) (numero-pecas-alinhadas (cdr lista-valores) nr-2 nr-1 (1+ nr-3)))
        (t (numero-pecas-alinhadas (cdr lista-valores) nr-2 nr-1 nr-3))
        )
) 

;;Verifica-tabuleiro-preenchido-p (FUNCAO ADICIONADA PARA 2 FASE)
(defun verifica-tabuleiro-preenchido-p (tabuleiro &optional(lista-coord (incrementa-z)))
  "Fun��o para ver se os 3 tabuleiros est�o preenchidos"
  (cond ((null lista-coord) t)
        ((verifica-casa-vazia (first (car lista-coord)) (second (car lista-coord)) (third (car lista-coord)) tabuleiro) NIL) ;quer dizer que ha uma casa vazia, ent�o retorna NIL
        (t (verifica-tabuleiro-preenchido-p tabuleiro (cdr lista-coord)))
        )
)

;;; Geracao dos sucessores (NECESS�RIA PARA 2 FASE)
;; (sucessores *no-teste* *operadores* 3 nil)
;; sucessores
(defun sucessores (no peca)
  "Permite gerar os sucessores de um no e recebe 4 parametros: um no, a lista de operadores,
   o nome do algoritmo de procura e a profundidade maxima para expansao da arvore.
   Retorna a lista de sucessores do no recebido como parametro. Se o algoritmo for a profundidade e
   o no recebido tiver uma profundidade igual a profundidade maxima nao e gerado nenhum sucessor.
   A fun��o sucessores retorna somente os sucessores que n�o existem na lista de abertos.
"
  ;(load "/Users/luisserrano/Dropbox/Universidade/3� Ano/IA/projeto/puzzle.lisp")
  ;(load "C:\\Users\\David\\Desktop\\School\\2015-2016\\1� Semestre\\IA\\Projeto\\1Fase\\2Fase\\galo3d.lisp") ;faz load ao puzzle.lisp para que se possam verificar as coordenadas vazias
  (let ((lista-sucessores NIL) ;variavel onde ser�o guardados todos os sucessores
        (lista-xyz (incrementa-z));variavel onde ser�o guardadas todas as coordenadas do tabuleiro, come�ando pelo nivel mais acima
        )
    (cond ((equal peca 'X) (gera-sucessores no 'colocar-cruz lista-xyz lista-sucessores peca))
          ((equal peca '0) (gera-sucessores no 'colocar-circulo lista-xyz lista-sucessores peca))
          )
    )
)
 
(defun gera-sucessores (no operador lista-xyz lista-sucessores peca)
  "Fun��o que ir� gerar os sucessores usando a lista de coordenadas"
  (cond ((null (car lista-xyz)) lista-sucessores)
        ((eq (verifica-casa-vazia (first (car lista-xyz)) (second (car lista-xyz)) (third (car lista-xyz)) (no-estado no)) NIL) 
         (gera-sucessores no operador (cdr lista-xyz) lista-sucessores peca)) 
         ;se a casa xyz tiver ocupada, chama recursivamente a fun��o gera-sucessores

        (t (gera-sucessores no operador (cdr lista-xyz) 
                            (append (list (sucessores-aux no operador (first (car lista-xyz)) (second (car lista-xyz)) (third (car lista-xyz)) peca))
                                  lista-sucessores) peca))
        ;caso contr�rio chamar recursivamente gera-sucessores, mas adiciona a lista de sucessores os nos gerados para essas coordenadad
        )
)

;;Fun��es de gera��o de coordenadas do tabuleiro (NECESS�RIA PARA 2 FASE)
;incrementa-z
(defun incrementa-z (&optional (x 1) (y 1) (z 3) (lista NIL))
  "Fun��o recursiva que incrementa a coordenada z, para que depois possa ser adiciona � lista de coordenadas xy, atraves da fun��o add-z"
  (cond ((< z 1) lista)        
        (t (incrementa-z x y (1- z) (append (add-z z (incrementa-x x y)) lista))) ;append ou cons, com cons divide por tabuleiros
        ) 
)

;add-z
(defun add-z (z lista-xy &optional(lista-xyz NIL))
  "Fun��o que adiciona a coordenada z a cada sub lista xy"
  (cond ((null (car lista-xy)) lista-xyz)
        (t (add-z z (cdr lista-xy) (cons (append (car lista-xy) (list z)) lista-xyz))) ;� necess�rio fazer (list z) sen�o tornar-se-a numa dotted list, devido ao cons
        )
)

;incrementa-x
(defun incrementa-x (x y &optional(lista NIL))
  "Fun��o que gera uma lista de coordenadas xy"
  (cond ((> x 2) (append (list (list x (third (incrementa-y y))) (list x (second (incrementa-y y))) (list x (first (incrementa-y y)))) lista))
        (t (incrementa-x  (1+ x) y (append (list (list x (third (incrementa-y y))) (list x (second (incrementa-y y))) (list x (first (incrementa-y y)))) lista)))
        )
)

;incrementa-y
(defun incrementa-y (y &optional(lista NIL))
  "Fun��o que gera uma lista de coordenadas y"
  (cond ((> y 2) (cons y lista))
        (t (incrementa-y (1+ y) (cons y lista))) 
  )
)
  
;; sucessores-aux (NECESS�RIA PARA 2 FASE)
(defun sucessores-aux (no operador x y z peca)
  "Permite gerar um sucessor de um no. A func recebe um no, o operador a aplicar e a profundidade-maxima
   da arvore de procura. A func retorna um no sucessor na qual o estado foi actualizado pela aplicacao do
   operador, a profundidade foi incrementada um valor e o no pai actualizado com a insercao de no recebido
   como parametro."
  (let* ((estado (funcall operador x y z (no-estado no))) ;aplica o 1� operador ao estado do no recebido
         (no-gerado (cria-no estado (1+ (no-profundidade no)) 0 no peca))) ;cria o no j� com o novo estado e calcula a heuristica
    no-gerado
    )
)

;;; solucao-p (NECESS�RIA PARA 2 FASE)
(defun solucao-p (no peca)
  "Fun��o que verifica se um n� � solu��o"
  (cond ((= (calcula-simbolos-alinhados (no-estado no) peca) 3) T) ;caso estejam 3 simbolos alinhados � solu��o e retorna true
        (t () NIL) ;caso contr�rio retorna NIL
        )
)

;;; Simbolos Alinhados (NECESS�RIA PARA 2 FASE)
(defun calcula-simbolos-alinhados (estado simbolo &optional(heuristica 0))
  "Fun��o que retorna o valor m�ximo de simbolos alinhos, em linha, coluna, ou diagonal"
  (get-max (list 
            (get-max (verifica-estado estado heuristica simbolo)) ;valor maximo alinhado em todas as linhas
            (get-max (verifica-linhas (inverte-linhas-para-linhas3D estado) heuristica simbolo)) ;valor maximo alinhado em todas as linhas 3D, como todas as linhas3D est�o numa s� lista, chama-se o verifica-linhas
            (get-max (verifica-linhas (inverte-linhas-para-colunas-diagonais estado) heuristica simbolo)) ;valor maximo alinhado em todas as colunas diagonais
            (get-max (verifica-linhas (inverte-linhas-para-colunas estado) heuristica simbolo)) ;todas as possibilidades de colunas num s� tabuleiro. est�o s� numa lista com sublista, portanto chama-se o verifica-linhas 
            (get-max (verifica-linhas (inverte-linhas-para-colunas3D estado) heuristica simbolo)) ;valor maximo alinhado em colunas ao longo dos 3 tabuleiros(3D)
            (get-max (verifica-linhas (inverte-linhas-para-diagonais estado) heuristica simbolo)) ;valor maximo alinhado em diagonal por tabuleiro
            (get-max (verifica-linhas (inverte-linhas-para-diagonais3D estado) heuristica simbolo)) ;valor maximo de simbolos alinhados em diagonal ao longo dos tabuleiros
            )
           )
)

;;; Heuristica
(defun heuristica (estado f-heuristica)
  "Vai chamar a fun��o que verifica todas as colunas, linhas e diagonais, para saber quantos simbolos   estao alinhados, de modo a determinar o valor heuristico, atraves da fun��o heuristica 3-p(x)"
  (- 3 (calcula-simbolos-alinhados estado f-heuristica))
)

;(NECESS�RIA PARA 2 FASE)
(defun verifica-estado (estado f-heuristica simbolo &optional(lista-valores-no nil))
  "Fun��o que percorre o estado, e chama as fun��es para verificar as linhas, colunas e diagonais para cada tabuleiro"
  (cond ((null (car estado)) lista-valores-no)
         (t (verifica-estado (cdr estado) f-heuristica simbolo (append (verifica-linhas (car estado) f-heuristica simbolo) lista-valores-no)))
  )
)

;; Inverter linhas para linhas ao longo dos tabuleiros (NECESS�RIA PARA 2 FASE)
(defun inverte-linhas-para-linhas3D (estado &optional(lista-linhas3D NIL) (lista-operadores-esq '(first second third)) (lista-operadores-dir '(third second first)))
  "Fun��o que inverte o estado atual que esta por linhas, e cada sub lista passa a representar uma linha nos 3 tabuleiros(3D)"
  (let ((linha-1 (first lista-linhas3D))     
        (linha-2 (second lista-linhas3D))
        (linha-3 (third lista-linhas3D))
        (linha-4 (fourth lista-linhas3D))
        (linha-5 (fifth lista-linhas3D))
        (linha-6 (sixth lista-linhas3D)))
    (cond ((OR (null estado) (null lista-operadores-esq)) lista-linhas3D)
          (t (inverte-linhas-para-linhas3D (cdr estado) 
                                           (list (append  linha-1 (list (funcall (car lista-operadores-esq) (first (car estado))))) ;primeira linha 3D a partir da esquerda
                                                 (append  linha-2 (list (funcall (car lista-operadores-esq) (second (car estado))))) ;segunda linha 3D a partir da esquerda
                                                 (append  linha-3 (list (funcall (car lista-operadores-esq) (third (car estado)))))  ;terceira linha 3D a partir da esquerda
                                                 (append  linha-4 (list (funcall (car lista-operadores-dir) (first (car estado)))))  ;primeira linha 3D a partir da direita
                                                 (append  linha-5 (list (funcall (car lista-operadores-dir) (second (car estado)))))  ;terceira linha 3D a partir da direita
                                                 (append  linha-6 (list (funcall (car lista-operadores-dir) (third (car estado))))))  ;terceira linha 3D a partir da direita
                                           (cdr lista-operadores-esq)
                                           (cdr lista-operadores-dir)))
          )
    )
)

;; Inverter linhas para diagonais com y constante, onde s� varia x e z, ou seja colunas diagonais (NECESS�RIA PARA 2 FASE)
(defun inverte-linhas-para-colunas-diagonais (estado)
  "Fun��o que interte o estado atual para colunas diagonais, estas diagonais s�o iguais as linhas 3D s� que em vez de ser ao longo da linha � ao longo de uma coluna"
  (let* ((lista-colunas (inverte-linhas-para-colunas estado))
         (tabuleiro-colunas (list (list (first lista-colunas) (second lista-colunas) (third lista-colunas))
                                   (list (fourth lista-colunas) (fifth lista-colunas) (sixth lista-colunas))
                                   (list (seventh lista-colunas) (eighth lista-colunas) (ninth lista-colunas))))
         )
    (inverte-linhas-para-linhas3D tabuleiro-colunas) ;devolve lista com diagonais por coluna, usando a mesma fun��o que devolve as linhas 3D
    )
)

;; Inverter linhas para colunas no mesmo tabuleiro (NECESS�RIA PARA 2 FASE)
(defun inverte-linhas-para-colunas (estado)
  "Fun��o que inverte o estado atual que esta por linhas, e cada sub lista passa a representar uma coluna"
  (cond ((null estado) estado)
        (t () (append (inverte-linhas-colunas-aux (car estado)) (inverte-linhas-para-colunas (cdr estado))))
  )
)

(defun inverte-linhas-colunas-aux (tabuleiro)
  (list (list (first (car tabuleiro)) (car (car (cdr tabuleiro))) (car (car (cdr (cdr tabuleiro)))))
        (list (second (car tabuleiro)) (second (car (cdr tabuleiro))) (second (car (cdr (cdr tabuleiro)))))
        (list (third (car tabuleiro)) (third (car (cdr tabuleiro))) (third (car (cdr (cdr tabuleiro)))))
  )
)

;; Inverter linhas para colunas ao longo dos tabuleiros (NECESS�RIA PARA 2 FASE)
(defun inverte-linhas-para-colunas3D (estado &optional(list-invertida NIL))
   "Fun��o que inverte o estado atual que esta por linhas, e cada sub lista passa a representar uma coluna nos 3 tabuleiros(3D)"
   (let ((nivel-1 (append (first (first estado)) (second (first estado)) (third (first estado))))
         (nivel-2 (append (first (second estado)) (second (second estado)) (third (second estado))))
         (nivel-3 (append (first (third estado)) (second (third estado)) (third (third estado))))
         )
     (append (inverte-linhas-para-colunas3D-aux nivel-1 nivel-2 nivel-3) list-invertida)
     )
)

(defun inverte-linhas-para-colunas3D-aux (nivel-1 nivel-2 nivel-3 &optional(lista-invertida NIL))
  (cond ((null nivel-1) lista-invertida)
        (t (inverte-linhas-para-colunas3D-aux (cdr nivel-1) (cdr nivel-2) (cdr nivel-3) (cons (list (car nivel-1) (car nivel-2) (car nivel-3)) lista-invertida)))
        )
)

;; Inverter linhas para diagonais no mesmo tabuleiro (NECESS�RIA PARA 2 FASE)
(defun inverte-linhas-para-diagonais (estado &optional(lista-diagonais NIL))
  "Fun��o que inverte o estado atual que esta por linhas, e cada sub lista passa a representar uma diagonal"
  (cond ((null estado) lista-diagonais) 
        (t (inverte-linhas-para-diagonais (cdr estado) (append (inverte-linhas-para-diagonais-aux (car estado)) lista-diagonais)))
        )
)

(defun inverte-linhas-para-diagonais-aux (tabuleiro)
  (list (list (first (first tabuleiro)) (second (second tabuleiro)) (third (third tabuleiro)))
          (list (third (first tabuleiro)) (second (second tabuleiro)) (first (third tabuleiro)))
          )
)

;; Inverter linhas para diagonais ao longo dos tabuleiros (NECESS�RIA PARA 2 FASE)
(defun inverte-linhas-para-diagonais3D (estado)
  "Fun��o que inverte o estado atual que esta por linhas, e cada sublista passa a representar uma diagonal nos 3 tabuleiros(3D)"
  (list (list (caaar estado) (second (second (second estado))) (third (third (third estado))))
        (list (third (caar estado)) (second (second (second estado))) (first (third (third estado))))
        (list (first (third (first estado))) (second (second (second estado))) (third (first (third estado))))
        (list (third (third (first estado))) (second (second (second estado))) (caar (third estado)))
        );cadr = second | caddr = third | caar = (first (first | caaar = (first (first (first |
)

;; Valor maximo de uma lista 
(defun get-max (lista-valores)
  "Fun��o auxiliar que vai buscar o maior valor n�merico de uma lista"
  (reduce #'max lista-valores))

;; Verifica��o de numeros de simbolos alinhados (NECESS�RIA PARA 2 FASE)
(defun verifica-linhas (tabuleiro f-heuristica simbolo &optional(lista-valores-tabuleiro nil))
  "Fun��o que verifica a quantidade X's e 0's em todas as linhas de um tabuleiro "
  (cond ((null (car tabuleiro)) lista-valores-tabuleiro)
        ((AND (> (first (verifica-linha (car tabuleiro) simbolo))0) ;fica esta verifica��o pois pode ajudar na fun��o de avalia��o
              (> (second (verifica-linha (car tabuleiro) simbolo))0) 
              (equal f-heuristica '2))
         (verifica-linhas (cdr tabuleiro) f-heuristica simbolo lista-valores-tabuleiro)) ;caso seja a segunda heuristica e haja dois simbolos diferentes na mesma linha, n�o conta essa linha 
        (t (verifica-linhas (cdr tabuleiro) f-heuristica simbolo (append (verifica-linha (car tabuleiro) simbolo) lista-valores-tabuleiro)))
  )
)

(defun verifica-linha (linha simbolo &optional(nr-x 0) (nr-0 0) (lista nil) )
  "Fun��o que verifica a quantidade X's e 0's em cada linha de um tabuleiro"
  (cond ((null linha) lista)
        ((equal (verifica-simbolo (car linha) simbolo) 'X) (verifica-linha (cdr linha) simbolo (1+ nr-x) nr-0 (list (1+ nr-x) nr-0)))
        ((equal (verifica-simbolo (car linha) simbolo) '0) (verifica-linha (cdr linha) simbolo nr-x (1+ nr-0) (list nr-x (1+ nr-0))))
        (t () (verifica-linha (cdr linha) simbolo nr-x nr-0 (list nr-x nr-0))))
)

(defun verifica-simbolo (elem simbolo)
  "Fun��o que devolve o proprio valor de um elemento se for X ou 0, caso contr�rio retorna NIL"
  (cond ((equal elem simbolo) simbolo)
        )
)

