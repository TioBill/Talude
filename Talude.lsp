(setq
  acadObj (vlax-get-acad-object)
  aDoc (vla-get-activedocument acadObj)
  modelSpace (vla-get-modelspace aDoc)
)

(setq LAYER_PNT-TALUDE "EMPRESAOSM_PONTOS-TALUDE")

(defun createLayer ()
  (entmake
    '(
      (0 . "LAYER") (2 . "EMPRESAOSM_PONTOS-TALUDE") (62 . 8)
      )
  )
)

(defun desenhaLinhas ( / pontos-talude curvas-nivel-1
                      curvas-nivel-2
                      direcao direcao-position length current-point
                      point-position elevacao-curva-1 elevacao-curva-1
                      elevacao-curva-2 distancia-ponto-menor menor-elevacao distancia-curvas media-ponderada tamanho-linha endPoint continuar
                      )  
  (setq continuar "S")
  
  (alert "Apartir de agora, voce deve selecionar os pontos que serao trabalhados, as curvas que devem ser utilizadas e a direcao!\n\n
    Lembrando que na hora de selecionar as curvas de niveis, a ordem NAO importa, a posicao SIM!")
  
  (while (eq continuar "S")
    (alert "Selecione os pontos do talude.")
  
    (setq pontos-talude (ssget '(
                                 ( 0 . "POINT") (8 . "EMPRESAOSM_PONTOS-TALUDE")
                                 )
                        )
    )
    
    (alert "Seleciona primeira curva")
    (setq curvas-nivel-1 (entsel))
    (alert "Seleciona segunda curva")
    (setq curvas-nivel-2 (entsel))
    
    (initget "X Y AMBOS")
      
    (setq direcao (strcase (getstring "Considerar qual direcao? (X/Y/AMBOS) [AMBOS]")))
    
    (if (eq direcao "") (setq direcao "AMBOS"))
    
    (alert "Clique na direcao que o talude deve ser desenhado")
    (setq direcao-position (getpoint))
    
    (repeat (setq length (sslength pontos-talude))
      (setq current-point (vlax-ename->vla-object (ssname pontos-talude (setq length (1- length)))))   
      (setq point-position (vlax-get current-point 'coordinates))
      
      (setq elevacao-curva-1 (vla-get-elevation (vlax-ename->vla-object (car curvas-nivel-1))))
      (setq elevacao-curva-2 (vla-get-elevation (vlax-ename->vla-object (car curvas-nivel-2))))
      
      (if (>= elevacao-curva-1 elevacao-curva-2)
        (progn
          (setq distancia-ponto-menor (distance point-position (car (cdr curvas-nivel-2))))
          (setq menor-elevacao elevacao-curva-2)
        )
        (progn
          (setq distancia-ponto-menor (distance point-position (car (cdr curvas-nivel-1))))
          (setq menor-elevacao elevacao-curva-1)
        )
      )
      
      (setq distancia-curvas (distance (car (cdr curvas-nivel-1)) (car (cdr curvas-nivel-2))))
      
      (setq media-ponderada (/ distancia-ponto-menor distancia-curvas))
      (setq media-ponderada (+ media-ponderada menor-elevacao))
      
      (setq tamanho-linha (abs (- media-ponderada nivel-projetado)))
      
      (setq endPoint (list))
      
      (cond
        ((eq direcao "X")
          (if (>= (car direcao-position) (car point-position))
            (setq endPoint (list 
                            (+ (car point-position) tamanho-linha)
                                (cadr point-position)
                                (caddr point-position)
                            )
            )
          
            (setq endPoint (list
                            (- (car point-position) tamanho-linha)
                            (cadr point-position)
                            (caddr point-position)
                            )
            )
          )          
        )
        
        ((eq direcao "Y")
          (if (>= (cadr direcao-position) (cadr point-position))
            (setq endPoint (list
                            (car point-position)
                             (+ (cadr point-position) tamanho-linha)
                             (caddr point-position)
                           )
            )
            
            (setq endPoint (list
                            (car point-position)
                             (- (cadr point-position) tamanho-linha)
                             (caddr point-position)
                           )
            )
          )
        )
      
        ((eq direcao "AMBOS")
          (if (>= (car direcao-position) (car point-position))
            (setq endPoint (list 
                            (+ (car point-position) tamanho-linha)
                                (cadr point-position)
                                (caddr point-position)
                            )
            )
          
            (setq endPoint (list
                            (- (car point-position) tamanho-linha)
                            (cadr point-position)
                            (caddr point-position)
                            )
            )
          )    
         
          (if (>= (cadr direcao-position) (cadr point-position))
            (setq endPoint (list
                            (car endPoint)
                             (+ (cadr endPoint) tamanho-linha)
                             (caddr endPoint)
                           )
            )
            
            (setq endPoint (list
                            (car endPoint)
                             (- (cadr endPoint) tamanho-linha)
                             (caddr endPoint)
                           )
            )
          )
        )
      )
      
      (vla-addline modelSpace (vlax-3d-point point-position) (vlax-3d-point endPoint))
    )
    
    (initget "S N")
    
    (setq continuar (strcase (getstring "Deseja continuar? (S/N) [S]")))

    (if (eq continuar "") (setq continuar "S"))
    
  )    
)


(defun c:GerarTalude ( / 
                          crista pontos-cAux-cMes
                          nivel-projetado       
                          *error* 
                          pontos-talude
                          )
  (defun *error* (msg)
    (or 
      (wcmatch (strcase msg t) "*break,*cancel*,*exit*") 
      (alert (strcat "ERROR: " msg "**"))
    )
    
    (vla-endundomark aDoc)
    (setvar 'cmdecho 1) ;; Ativa o CMD feedback
  )
  
  (setvar 'cmdecho 0) ;; Desativa CMD feedback (Melhora de desempenho)

  (setq
    acadObj (vlax-get-acad-object)
    aDoc (vla-get-activedocument acadObj)
    modelSpace (vla-get-modelspace aDoc)
  )
  
  (vla-startundomark aDoc)
  (createLayer)
  
  (alert "Selecione a crista e aperte \"ENTER\" ou \"SPACE\"")
  
  (setq crista (ssname 
                   (ssget 
                     '(
                       (0 . "LWPOLYLINE,LINE")
                       )
                   )
                 
                 0
               )
  )
  
  (setvar 'clayer LAYER_PNT-TALUDE)
  (setvar 'cecolor "256") ;; Set color to "ByLayer"
  
  (alert "Selecione a quantidade de pontos:\n
    [1] - Media (Bom para maioria dos casos)\n
    [2] - Boa (Bom para terrenos extremamente irregulares)\n
  ")
    
  (initget (+ 1 2 4))
  
  (setq precisao (getint "Selecione o nivel de precisao (1/2): "))

  (cond 
    ((<= precisao 1) 
      (command "_measure" crista 1)
    )
    ((>= precisao 2)
     (command "_measure" crista .1)
    )
  )
  
  ;; Isola as layers "EMPRESAOSM_PONTOS-TALUDE", "EMPRESAOSM_CURVAS_AUXILIARES" e "EMPRESAOSM_CURVAS_MESTRAS"
  (setq pontos-cAux-cMes (ssget "_X" '((8 . "EMPRESAOSM_PONTOS-TALUDE,EMPRESAOSM_CURVAS_AUXILIARES,EMPRESAOSM_CURVAS_MESTRAS"))))
  
  (initget (+ 1 2 4))
  (setq nivel-projetado (getreal "Digite o nivel projetado da crista: "))
  
  (command "_layiso" pontos-cAux-cMes "")
  
  (desenhaLinhas)

  (command "_layuniso")

  (prompt "Deletando pontos gerados...")
  
  (setq pontos-talude (ssget "_X" '((0 . "POINT") (8 . "EMPRESAOSM_PONTOS-TALUDE"))))
  (command "_erase" pontos-talude "")


  (alert "Caso deseje conectar os pontos, digite: \"OSM:conectartPontos\"")
  
  (vla-endundomark aDoc)
  
  (princ)
)

(defun c:OSM:conectarPontos( / linhas-talude endPoints i currentLine safearray-talude )
  (setvar 'clayer LAYER_PNT-TALUDE)
  (setvar 'cecolor "256") ;; Set color to "ByLayer"

  (setq linhas-talude (ssget "_X" '((0 . "LINE") (8 . "EMPRESAOSM_PONTOS-TALUDE"))))

  (setq endPoints (list)) 
  
  (repeat (setq i (sslength linhas-talude))
    (setq currentLine (ssname linhas-talude (setq i (1- i))))
    (setq currentLine (vlax-ename->vla-object currentLine))
    
    (setq endPoints (append endPoints (vlax-get currentLine 'endPoint)))
  )
  
  (setq safearray-talude (vlax-make-safearray vlax-vbDouble (cons '0 (- (length endPoints) 1))))
  (vlax-safearray-fill safearray-talude endPoints)
  (vla-addpolyline modelSpace safearray-talude)
)


(alert 
  "Lisp carregada! Digite \"GerarTalude\" para comecar! IMPORTANTE! Esse programa somente funcionara corretamente para o UCS setado para WORLD!\n\n
  Se for sua primeira vez utilizando esse programa, recomendo fortemente que voce digite \"TaludeHelp\".\n\nEu explico o funcionamento da LISP por ali! Atencao, que o texto e longo, entao para melhor visualizacao, sugiro que aperte a tecla \"F2\" do seu teclado (ZwCad command) e aumente a tela do console.;)"
)

(defun c:TaludeHelp ()
  (prompt
    "O programa funciona da seguinte maneira:\n
    1: Ele pega toda a Polyline/linha que o usuario seleciona no comeco e cria diversos pontos em cima desse objeto. A distancia entre cada ponto eh definido pelo proprio usuario, apos a selecao do objeto!\n
    2: A partir desses pontos o programa isola, TODOS os objetos que estejam na layers \"EMPRESAOSM_CURVAS_AUXILIARES\", \"EMPRESAOSM_CURVAS_MESTRAS\" e \"EMPRESAOSM_PONTOS-TALUDE\". Essa ultima criada pelo proprio programa.\n
    3: Apos isso o programa vai pedir os pontos gerados pelo programa. As curvas de nivel que devem ser consideras (2 curvas no maximo). E a direcao a qual as linhas do talude deve ser desenhadas.\n\n
    Nota que, essa acao vai ser repetidas quantas vezes o usuario aceitar, a cada talude gerado o usuario deve responder se deseja continuar.\n
    
    OPTIONAL: Existem a opcao de conectar as linhas, que foram geradas. Tecnicamente, ele junta todas os ENDPOINTS de toda as lines que esteja na layer EMPRESAOSM_PONTOS-TALUDE.
    
    Lembrando, que o proposito do programa nao e ser 100% preciso, o proposito e servir como referencia na hora de realizar os projetos arquitetonicos.
    ESSE PROGRAMA NAO SUBSTITUI O TRABALHO DE UM PROFISSIONAL.
   ")
  
  (prompt
    "
    FAQ:
    Q: Como e feito o calculo do talude?
    R: O metodo utilizado para o calculo da posicao do talude e feito da seguinte maneira.\n\n
    
    Quando o usuario escolhe as curvas de nivel, o programa tambem pega a exata posicao a qual o usuario cliquou quando selecionou as curvas de nivel.\n
    A partir desses pontos o programa calcula a media ponderada das curvas.\n
    A media ponderada e feita da seguinte maneira:
    
    (Distancia da menor curva ate o ponto / Distancia da menor curva ate a proxima curva) + elevacao da menor curva.\n
    
    Com esse valor, eh pego o valor absoluto da subtracao do nivel de terreno projetado com a media ponderada. Esse valor eh entao usado para determinar o comprimento da linha.
    
    Nota: Por questao de praticidade, os pontos das curvas de nivel serao sempre os mesmo para todos os pontos, so mudando obviamento a posicao do ponto.\n
    Ou quando o talude e gerado, e o programa solicita novamente os proximos niveis de curvas a ser considerado. O mesmo se aplica para a direcao da linha.\n
    
    Q: Quando selecione o metodo que considera ambas as direcoes, as linhas nao seguem todas as mesma direcoes.\N
    R: Isso, muito provavelmente aconteceu pois o ponto de selecao que voce selecionou estava em paralelo com os pontos que estao alinhados na direcao que voce escolheu.
    por exemplo, os pontos estava na HORIZONTAL, e voce escolheu a direcao AMBOS, e a direcao estava em paralelo com os pontos. (frente/tras).\n
    Isso se deve, que na hora de desenhar a linha, cada linha e desenhado para seguir a direcao do mouse em relacao a posicao do ponto sendo trabalhado.
    ")
  (princ)
)