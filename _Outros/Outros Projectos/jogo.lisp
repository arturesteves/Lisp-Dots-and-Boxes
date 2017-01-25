;;;; jogo.lisp
;;;; Projeto 2 - IA 1 Semestre Ano Lectivo 2015-2016
;;;; Carrega os outros ficheiros de codigo, escreve e le de ficheiros e trata com a interaccao do utilizador
;;;; Autor: João Silva (120221053) & Joao Guerreiro (110221049)


(defvar *computador* nil)
(defvar *humano* nil)
(defvar *jogada* nil)
(defvar *tempo-jogada* 10) ;; colocar o tempo em segundos que o PC tem para jogar


;;; EXECUTAR PROGRAMA

(defun comecar-jogo (plataforma)
      (carregar-ficheiros plataforma)
      (menu-principal))

;;; MENU JOGO

(defun menu-principal ()
     (format t "~% ------------------------------------------------------")
     (format t "~%|                  JOGO DO GALO 3D                     |")
     (format t "~%|                                                      |")
     (format t "~%|               1- JOGAR                               |")
     (format t "~%|               2- SOBRE O JOGO                        |")      
     (format t "~%|               3- SAIR                                |")
     (format t "~%|                                                      |")
     (format t "~% ------------------------------------------------------")
     (format t "~%~%Escolha uma Opção:")

     (let ((escolha (read)))
       (cond ((= escolha 1) (escolher-peca))
             ((= escolha 2) (sobre-jogo))
             ((= escolha 3) (progn (format t "~% ------------------------------------------------------")
                                   (format t "~%|                                                      |")
                                   (format t "~%|                    ATÉ BREVE!!!!                     |")
                                   (format t "~%|                                                      |")
                                   (format t "~%|    João Silva                      João Guerreiro    |")
                                   (format t "~%|                                                      |")
                                   (format t "~% ------------------------------------------------------")))
             (t (progn (format t "~%ESCOLHA INVALIDA~%~%Escolha: ")
                  (menu-principal))))))


(defun sobre-jogo ()
  "Informação Sobre o Jogo"
     (format t "~% ------------------------------------------------------")
     (format t "~%|                   SOBRE O JOGO                       |")
     (format t "~%|                                                      |")
     (format t "~%|                                                      |")
     (format t "~%|   -> JOGO DO GALO 3D                                 |")
     (format t "~%|                                                      |")
     (format t "~%|      O jogo tem as mesmas regras que o jogo do       |")
     (format t "~%|      galo tradicional, mas neste caso temos          |")
     (format t "~%|      3 tabuleiros alinhados na vertical,             |")
     (format t "~%|      permitindo assim tambem combinações de          |")
     (format t "~%|      3 simbolos iguais em altura                     |")
     (format t "~%|      (linhas e diagonais), utilizando os             |")
     (format t "~%|      3 tabuleiros.                                   |")
     (format t "~%|                                                      |")
     (format t "~%|   -> COMO FUNCIONA                                   |")
     (format t "~%|                                                      |")
     (format t "~%|      Para comecar a jogar, basta escolher a          |")
     (format t "~%|      opcao jogar, escolher a peca que pretende       |")
     (format t "~%|      ser e quem vai começar primeiro a jogar         |")
     (format t "~%|      (jogador ou computador                          |")
     (format t "~%|                                                      |")
     (format t "~%|                                                      |")
     (format t "~%|                           PARA VOLTAR ESCOLHA 'B'    |")
     (format t "~%|                                                      |")
     (format t "~% ------------------------------------------------------")
     (format t "~%~%Escolha uma Opção:")

     (let ((escolha (read)))
       (cond ((equal escolha 'B) (menu-principal))
             (t (progn (format t "~%ESCOLHA INVALIDA~%~%Escolha: ")
                  (sobre-jogo))))))


(defun escolher-peca ()
     (format t "~% ------------------------------------------------------")
     (format t "~%|                  ESCOLHA A SUA PECA                  |")
     (format t "~%|                                                      |")
     (format t "~%|               1- X (CRUZ)                            |")
     (format t "~%|               2- O (BOLA)                            |")
     (format t "~%|                                                      |")
     (format t "~% ------------------------------------------------------")
     (format t "~%~%Escolha uma Peca:")

     (let ((escolha (read)))
       (cond ((= escolha 1) (progn (setf *humano* 'X) 
                                   (setf *computador* 'O)
                                   (quem-joga)))
             ((= escolha 2) (progn (setf *humano* 'O) 
                                   (setf *computador* 'X)
                                   (quem-joga)))
              (t (progn (format t "~%ESCOLHA INVALIDA~%~%Escolha: ")
                  (escolher-peca))))))


(defun quem-joga ()
     (format t "~% ------------------------------------------------------")
     (format t "~%|                  QUEM COMECA A JOGAR                 |")
     (format t "~%|                                                      |")
     (format t "~%|               1- JOGADOR                             |")
     (format t "~%|               2- COMPUTADOR                          |")
     (format t "~%|                                                      |")
     (format t "~% ------------------------------------------------------")
     (format t "~%~%Escolha uma Opção:")

     (let ((escolha (read)))
       (cond ((= escolha 1) (humano-joga (cria-tabuleiro)))
             ((= escolha 2) (computador-joga (cria-tabuleiro)))
              (t (progn (format t "~%ESCOLHA INVALIDA~%~%Escolha: ")
                  (quem-joga))))))


;;; CODIGO JOGO

(defun vencedor-p (tabuleiro)
  (solucaop (cria-no tabuleiro)))

(defun humano-joga (tabuleiro)
  "Retorna a jogada do humano sob a forma de um novo tabuleiro.
  Verifica se existe um vencedor ou um empate. Se nao for o caso, pede a jogada do PC"
  (let* ((pos (le-jogada tabuleiro))
         (novo-tabuleiro (faz-jogada 
                          *humano*
                          pos
                          tabuleiro)))
    (imprime-tabuleiro novo-tabuleiro)
    (cond ((vencedor-p novo-tabuleiro)
           (format t "~&Ganhou!"))
          ((tabuleiro-preenchido-p novo-tabuleiro)
           (format t "~&Empatamos."))
          (t (computador-joga novo-tabuleiro)))))

(defun computador-joga (tabuleiro)
  "Retorna a jogada do PC sob a forma de um novo tabuleiro.
  Verifica se existe um vencedor ou um empate. Se nao for o caso, pede a jogada do humano"
  (let* ((tempo-inicial (get-internal-real-time))
         (pos (escolhe-melhor-jogada tabuleiro (+ tempo-inicial (* 1000 (- *tempo-jogada* 1)))))
         (novo-tabuleiro (faz-jogada *computador* pos tabuleiro))
         (tempo-gasto-sec (/ (- (get-internal-real-time) tempo-inicial) 1000.0)))
    (format t "~&A minha jogada: ~S" pos)
    (format t "~&Tempo Gasto: ~S segundos" tempo-gasto-sec)
    (registo-jogada (imprime-tabuleiro novo-tabuleiro)
                     pos
                     (get-nos-analisados)
                     (get-cortes-alfa)
                     (get-cortes-beta)
                     tempo-gasto-sec)
    (imprime-tabuleiro novo-tabuleiro t)
    (cond ((vencedor-p novo-tabuleiro)
           (format t "~&Ganhei!"))
          ((tabuleiro-preenchido-p novo-tabuleiro)
           (format t "~&Empatamos."))
          (t (humano-joga novo-tabuleiro)))))

;;; FUNCOES DE IMPRESSAO

(defun imprime-tabuleiro (tabuleiro &optional (stream nil))
  "Imprime o tabuleiro, linha a linha"
  (let ((str (concatenate 'string
  (format stream "~%~&")
  (format stream "~%~&      NIVEL 3        NIVEL 2        NIVEL 1 ")
  (format stream "~%~%~&")
  (format stream "~%~&     ~A | ~A | ~A      ~A | ~A | ~A      ~A | ~A | ~A "
          (imprimir-peca 1 1 3 tabuleiro) (imprimir-peca 1 2 3 tabuleiro) (imprimir-peca 1 3 3 tabuleiro)
          (imprimir-peca 1 1 2 tabuleiro) (imprimir-peca 1 2 2 tabuleiro) (imprimir-peca 1 3 2 tabuleiro)
          (imprimir-peca 1 1 1 tabuleiro) (imprimir-peca 1 2 1 tabuleiro) (imprimir-peca 1 3 1 tabuleiro))
  (format stream "~%~&    -----------    -----------    -----------")
  (format stream "~%~&     ~A | ~A | ~A      ~A | ~A | ~A      ~A | ~A | ~A "
          (imprimir-peca 2 1 3 tabuleiro) (imprimir-peca 2 2 3 tabuleiro) (imprimir-peca 2 3 3 tabuleiro)
          (imprimir-peca 2 1 2 tabuleiro) (imprimir-peca 2 2 2 tabuleiro) (imprimir-peca 2 3 2 tabuleiro)
          (imprimir-peca 2 1 1 tabuleiro) (imprimir-peca 2 2 1 tabuleiro) (imprimir-peca 2 3 1 tabuleiro))
  (format stream "~%~&    -----------    -----------    -----------")
  (format stream "~%~&     ~A | ~A | ~A      ~A | ~A | ~A      ~A | ~A | ~A "
          (imprimir-peca 3 1 3 tabuleiro) (imprimir-peca 3 2 3 tabuleiro) (imprimir-peca 3 3 3 tabuleiro)
          (imprimir-peca 3 1 2 tabuleiro) (imprimir-peca 3 2 2 tabuleiro) (imprimir-peca 3 3 2 tabuleiro)
          (imprimir-peca 3 1 1 tabuleiro) (imprimir-peca 3 2 1 tabuleiro) (imprimir-peca 3 3 1 tabuleiro))
  (format stream "~%~%"))))
  str))



(defun imprimir-peca (x y z tabuleiro)
  (let ((peca (get-casa x y z tabuleiro)))
    (cond ((eq peca nil) " ")
          (t peca))))


(defun converte-num-xyz (nivel escolha)
  (cond ((or (< nivel 1) (> nivel 3) (< escolha 0) (> nivel 8)) nil)
        ((= escolha 0) (list 1 1 nivel))
        ((= escolha 1) (list 1 2 nivel))
        ((= escolha 2) (list 1 3 nivel))
        ((= escolha 3) (list 2 1 nivel))
        ((= escolha 4) (list 2 2 nivel))
        ((= escolha 5) (list 2 3 nivel))
        ((= escolha 6) (list 3 1 nivel))
        ((= escolha 7) (list 3 2 nivel))
        ((= escolha 8) (list 3 3 nivel))))


(defun le-jogada (tabuleiro)
  (format t "~&Qual Dos 3 Tabuleiro Pretende [1 a 3] :~&")
  (let ((tab (read)))
    (format t "~&Qual a Posicao do Tabuleiro que Pretende [0 a 8] :~&")
    (let ((pos (read)))
      (cond ((eq (converte-num-xyz tab pos) nil) 
             (format t "~&Dados Invalidos!!")
             (le-jogada tabuleiro))
            ((verifica-casa (get-casa (first (converte-num-xyz tab pos))
                                      (second (converte-num-xyz tab pos))
                                      (third (converte-num-xyz tab pos)) tabuleiro))
             (format t"~&Esta Casa Ja Esta Ocupada!!")
             (le-jogada tabuleiro))
            (t (converte-num-xyz tab pos))))))

;;; FICHEIROS

;; Carregar Ficheiros

(defun carregar-ficheiros (plataforma)
   "Carrega os ficheiro através da directoria que recebe"
          (compile-file (concatenate 'string (diretoria-ficheiros plataforma) "galo.lisp") :load t)
          (compile-file (concatenate 'string (diretoria-ficheiros plataforma) "alfabeta.lisp") :load t))


;; Directoria Ficheiros

(defun diretoria-ficheiros (plataforma)
  "Devolve a directoria a onde se encontra os ficheiros"
  (cond ((equal 'mac plataforma)
         (let ((mac-caminho "/Users/JoaoGuerreiro/Google Drive/IA 2015/2-Projecto/")) mac-caminho))
        ((equal 'windows plataforma)
         (let ((windows-caminho "C:\\Users\\joaoj\\Google Drive\\IA 2015\\2-Projecto\\")) windows-caminho))))


;; Criar Ficheiros

(defun criar-ficheiro (plataforma conteudo)
  (with-open-file (stream (concatenate 'string (diretoria-ficheiros plataforma) "LOG.dat")
                          :direction :output :if-exists :append :if-does-not-exist :create)
    (princ conteudo stream)))


;;; RELATORIO "LOG.DAT"

;; (criar-ficheiro 'mac (registo-jogada (jogadaRealizada posJogada nosAnalisados corteAlfa corteBeta tempoGasto)))

(defun registo-jogada (jogadaRealizada posJogada nosAnalisados corteAlfa corteBeta tempoGasto) 
  "Metodo que Constroi o Relatorio"
  (let ((registo (concatenate 'string
  (format nil "~%~% |------------------------------------------------------------------------|~%~%")
  (format nil "Posicao Jogada: ~a ~&" posJogada)
  (format nil "Nos Analisados: ~a ~&" nosAnalisados)
  (format nil "Cortes Alfa ~a ~&" corteAlfa)
  (format nil "Cortes Beta ~a ~&" corteBeta)
  (format nil "Tempo Gasto: ~a Segundos ~&" tempoGasto)
  (format nil "~%Jogada Realizada: ~a ~&" jogadaRealizada)
  (format nil "~%~% |------------------------------------------------------------------------|~%~%"))))
    (criar-ficheiro 'mac registo)))


;;;; FUNCAO PARA O CAMPEONATO

(defun jogar (estado simbolo tempo)
  (setf *computador* simbolo)
  (setf *humano* ((lambda (simbolo) (cond ((eq simbolo 'X) 'O) (t 'X))) simbolo))
  (let* ((tempo-inicial (get-internal-real-time))
         (pos (escolhe-melhor-jogada estado (+ tempo-inicial (- tempo 1000))))
         (novo-tabuleiro (faz-jogada *computador* pos estado)))
  novo-tabuleiro))