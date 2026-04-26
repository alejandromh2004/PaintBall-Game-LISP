;; ======================================================================
;; PRÀCTICA FINAL LLENGUATGES DE PROGRAMACIÓ - LISP - PAINTBALL
;; ======================================================================
;; Estudiants: Alejandro Martinez Hermosa, Javier Vivo Samaniego
;; Data: 30/04/2026
;; Assignatura: Llenguatges de Programació (LP)
;; Grup: <Indicar Grup>
;; Professors: <Indicar Professors>
;; Convocatòria: Primera Convocatòria (Ordinària)

;; Inicialitza el programa, carregant tots els fitxers necessaris
(cond ((not (boundp '*features*)) (setq *features* nil))) 
(load "proyectos/projecte_inicial/common.lsp") 
(load "proyectos/projecte_inicial/tco.lsp")    
(load "proyectos/projecte_inicial/funciones_auxiliares.lsp")
(load "proyectos/projecte_inicial/grafics.lsp")
(load "proyectos/projecte_inicial/agent-abc123.lsp")
(load "proyectos/projecte_inicial/agent-xyz999.lsp")

;; Punt d'entrada del programa
(defun inici (&optional (fitxer "proyectos/projecte_inicial/maps/bait.map"))
  (BLACK) 
  (mode 0 0 640 400) 
  
  ;; Prepara el mapa i inicia la partida
  (let ((mapa-inicial (carrega-mapa fitxer)))
    (inicia-partida mapa-inicial))
  
  (BLACK) 
  t)

;; Carga mapa
(defun carrega-mapa (nom-fitxer)
  (let* ((canal (open nom-fitxer :direction :input))
         (mapa (read canal)))
    (close canal)
    mapa)
)

;; Prepara el mapa inicial
(defun prepara-casella-inicial (casella x y)
;; Si es terra base o bolla inicialitzam colors inicials i ID
  (cond ((and (eq (car casella) 'terra) 
              (or (eq (caddr casella) 'base) (eq (caddr casella) 'bolla)))
         (let* ((tipus (caddr casella)) ;; Tipus casella(base o bolla)
                (color-propi (nth 5 casella)) ;; Color propi de la unitat
                (colors-inicials (cond ((eq tipus 'base) nil) ;; Bases sin pintar
                                       (t (list color-propi)))) ;; Bolles pintades del seu color
                (id-unitat (+ (* y 1000) x))) ;; ID de la unitat\
          ;; Declara la nova casella amb tots els camps
           (list 'terra (cadr casella) tipus (cadddr casella) 
                 colors-inicials color-propi (nth 6 casella) (nth 7 casella) 
                 id-unitat)))
        (t casella)))

;; Prepara una fila sencera de caselles
(defun prepara-fila-inicial (fila x y)
  (cond ((null fila) nil)
        (t (cons (prepara-casella-inicial (car fila) x y)
                 (prepara-fila-inicial (cdr fila) (+ x 1) y)))))

;; Prepara el mapa inicial
(defun prepara-mapa-inicial (mapa y)
  (cond ((null mapa) nil)
        (t (cons (prepara-fila-inicial (car mapa) 0 y)
                 (prepara-mapa-inicial (cdr mapa) (+ y 1))))))


;; ======================================================================
;; CONTROLADOR GENERAL
;; ======================================================================

;; Inicialitza el joc
(defun inicia-partida (mapa-inicial)
  (let ((dx (random 1000)) ;; Desplaçament horitzontal aleatori per a les unitats
        (dy (random 1000)) ;; Desplaçament vertical aleatori per a les unitats
        (mapa-prep (prepara-mapa-inicial mapa-inicial 0)))
    ;; Inicialitza el bucle de la partida
    (bucle-partida 1 mapa-prep 200 200 nil nil dx dy nil nil 0 nil))) ;; Valors inicials del bucle

;; Bucle principal del joc amb fletxes, gestió de l'historial de rondes i gestió de la memòria.
(defun-tco bucle-partida (ronda mapa pint-e1 pint-e2 mem-e1 mem-e2 dx dy historia futur skip-visual fletxes-prev)
  ;; Dibuixem només si no estem saltant torns visuals
  (cond ((<= skip-visual 0)
       (dibuixa-mapa mapa ronda
                     (cond ((= (mod ronda 2) 1) 'e1) (t 'e2))
                     pint-e1 pint-e2 fletxes-prev)))

  (cond 
    ;; Comprova si l'equip 1 ha perdut (la base ha desaparegut)
    ((= (compta-bases-mapa mapa 'e1) 0)
     (format t "~%==================================================~%")
     (format t "    VICTORIA! LA BASE DE L'EQUIP 1 HA EXPLOTAT    ~%")
     (format t "               GUANYA L'EQUIP 2!                  ~%")
     (format t "==================================================~%")
     'fi-de-partida)

    ;; Comprova si l'equip 2 ha perdut (la base ha desaparegut)
    ((= (compta-bases-mapa mapa 'e2) 0)
     (format t "~%==================================================~%")
     (format t "    VICTORIA! LA BASE DE L'EQUIP 2 HA EXPLOTAT    ~%")
     (format t "               GUANYA L'EQUIP 1!                  ~%")
     (format t "==================================================~%")
     'fi-de-partida)

    ;; Comprova si s'ha arribat al límit de rondes
    ((> ronda 1500)
     (determina-guanyador-empat mapa pint-e1 pint-e2)
     'fi-de-partida)
        
    (t 
      ;; Determinem si hem de mostrar el menú o processar automàticament
      (let* ((cmd (cond 
                    ((> skip-visual 0) 'f) ;; Si estem saltant, el comando és forward
                    ;; Aquí mostram la consola
                    (t (progn
                        (format t "~%Ronda ~A/1500" ronda)
                        (format t "~%E1: ~A" pint-e1)
                        (format t "~%E2: ~A" pint-e2)
                        (format t "~%---------------")
                        (format t "~%Controls")
                        (format t "~%Continuar: ENTER")
                        (format t "~%Saltar: s")
                        (format t "~%Enrere: b")
                        (format t "~%Sortir: q")
                        (format t "~%-----------------")
                        (format t "~%Tecla: ")
                        ;; Aquí llegim la tecla
                        (let ((input (read-line)))
                          (cond ((string-equal input "b") 'b)
                                ((string-equal input "q") 'q)
                                ((string-equal input "s") 's)
                                (t 'f)))))))
             ;; Calculem el skip per a la següent iteració
             (proxim-skip (cond ((eq cmd 's) 
                                 (format t "Quantes rondes vols saltar? ")
                                 (let ((n (read))) (max 0 (- n 1)))) ;; Escogit saltar: n-1 rondes (no se processa la ronda actual)
                                ((> skip-visual 0) (- skip-visual 1)) ;; Continuam saltant
                                (t 0)))) ;; No hem saltat cap ronda
       
       (cond 
         ;; Si es q, sortim del joc
         ((eq cmd 'q)
          (cls)
          (format t "~%[SISTEMA] Partida aturada per l'usuari.~%")
          'fi-de-partida)

         ;; Si es b i hi ha historia, tornam a la ronda anterior
         ((and (eq cmd 'b) historia)
          (let ((estat-ant (car historia))
                (estat-actual (list ronda mapa pint-e1 pint-e2 mem-e1 mem-e2)))
            (bucle-partida (car estat-ant) 
                           (cadr estat-ant) 
                           (caddr estat-ant) 
                           (nth 3 estat-ant) 
                           (nth 4 estat-ant) 
                           (nth 5 estat-ant) 
                           dx dy (cdr historia) (cons estat-actual futur) 0 nil)))
         
         ;; Si es forward (f) i hi ha futur, anarem endavant
         (t 
          (cond
            ((and (eq cmd 'f) futur (<= skip-visual 0))
             (let ((estat-seg (car futur)) ;; Següent estat (no es processa, tan sols se recupera)
                   (estat-act (list ronda mapa pint-e1 pint-e2 mem-e1 mem-e2)))
               (bucle-partida (car estat-seg) ;; Següent ronda (ja processada)
                              (cadr estat-seg) 
                              (caddr estat-seg) 
                              (nth 3 estat-seg) 
                              (nth 4 estat-seg) 
                              (nth 5 estat-seg) 
                              dx dy 
                              (cons estat-act historia) 
                              (cdr futur) 
                              0 nil)))
            
            ;; Si es forward (f) i no hi ha futur processam el torn
            (t
             (let* ((equip-actiu (cond ((= (mod ronda 2) 1) 'e1) (t 'e2))) ;; Rota els torns entre e1 i e2
                    (pint-e1-inici (cond ((eq equip-actiu 'e1) (+ pint-e1 2 (compta-labs-mapa mapa 'e1))) (t pint-e1))) ;; Recarrega la pintura de l'equip actiu i suma els labs
                    (pint-e2-inici (cond ((eq equip-actiu 'e2) (+ pint-e2 2 (compta-labs-mapa mapa 'e2))) (t pint-e2))) ;; Recarrega la pintura de l'equip actiu i suma els labs
                    (pintura-actual-equip (cond ((eq equip-actiu 'e1) pint-e1-inici) (t pint-e2-inici))) ;; Pinta-equip es la pintura que s'utilitza en el torn
                    (memoria-actual-equip (cond ((eq equip-actiu 'e1) mem-e1) (t mem-e2))) ;; La memoria del equip actiu
                    (mapa-descansat (redueix-temps-mapa mapa equip-actiu)) ;; Redueix el temps de les unitats 
                    (unitats-actuants (busca-unitats-mapa mapa-descansat equip-actiu 0)) ;; Cerca les unitats de l'equip actiu
                    (estat-resultant (processa-totes-les-unitats unitats-actuants mapa-descansat ronda pintura-actual-equip equip-actiu memoria-actual-equip dx dy nil)) ;; Processa totes les unitats de l'equip actiu
                    (nou-mapa (car estat-resultant)) ;; Nou mapa
                    (nova-pintura-equip (cadr estat-resultant)) ;; Nova pintura del equip actiu
                    (nova-memoria-equip (caddr estat-resultant)) ;; Nova memoria del equip actiu
                    (nova-pint-e1 (cond ((eq equip-actiu 'e1) nova-pintura-equip) (t pint-e1-inici))) ;; Nova pintura de l'equip 1
                    (nova-pint-e2 (cond ((eq equip-actiu 'e2) nova-pintura-equip) (t pint-e2-inici))) ;; Nova pintura de l'equip 2
                    (nova-mem-e1 (cond ((eq equip-actiu 'e1) nova-memoria-equip) (t mem-e1))) ;; Nova memoria de l'equip 1
                    (nova-mem-e2 (cond ((eq equip-actiu 'e2) nova-memoria-equip) (t mem-e2))) ;; Nova memoria de l'equip 2
                    (nova-historia (cons (list ronda mapa pint-e1 pint-e2 mem-e1 mem-e2) historia)) ;; Nova historia
                    (nova-fletxes (nth 3 estat-resultant))) ;; Fletxes del torn actual

               (bucle-partida (+ ronda 1) 
                              nou-mapa 
                              nova-pint-e1 
                              nova-pint-e2 
                              nova-mem-e1 
                              nova-mem-e2
                              dx
                              dy
                              nova-historia
                              nil ;; El futur es perd si processem un torn nou
                              proxim-skip nova-fletxes))))))))))

;; ======================================================================
;; CONDICIÓ DE VICTÒRIA: COMPTAR BASES
;; ======================================================================

;; Conta quantes bases té un equip en una fila
(defun compta-bases-fila (fila equip)
  (cond ((null fila) 0)
        (t (let ((casella (car fila)))
             (cond ((and (eq (car casella) 'terra)
                         (eq (caddr casella) 'base)
                         (eq (cadddr casella) equip))
                    (+ 1 (compta-bases-fila (cdr fila) equip)))
                   (t (compta-bases-fila (cdr fila) equip)))))))

;; Conta quantes bases té un equip en tot el mapa
(defun compta-bases-mapa (mapa equip)
  (cond ((null mapa) 0)
        (t (+ (compta-bases-fila (car mapa) equip)
              (compta-bases-mapa (cdr mapa) equip)))))

;; ======================================================================
;; LÒGICA DE DESEMPAT
;; ======================================================================

;; Conta quantes bolles té un equip en una fila
(defun compta-bolles-fila (fila equip)
  (cond ((null fila) 0)
        (t (let ((casella (car fila)))
             (cond ((and (eq (car casella) 'terra)
                         (eq (caddr casella) 'bolla)
                         (eq (cadddr casella) equip))
                    (+ 1 (compta-bolles-fila (cdr fila) equip)))
                   (t (compta-bolles-fila (cdr fila) equip)))))))

;; Conta quantes bolles té un equip en tot el mapa
(defun compta-bolles-mapa (mapa equip)
  (cond ((null mapa) 0)
        (t (+ (compta-bolles-fila (car mapa) equip)
              (compta-bolles-mapa (cdr mapa) equip)))))

;; Determina el guanyador segons el criteri de desempat
(defun determina-guanyador-empat (mapa pint-e1 pint-e2)
  (let ((bolles-e1 (compta-bolles-mapa mapa 'e1))
        (bolles-e2 (compta-bolles-mapa mapa 'e2)))
    (format t "~%==================================================~%")
    (format t "   FINAL PER LIMIT DE TORNS (1500) - DESEMPAT     ~%")
    (format t "   Equip 1: ~A bolles | Equip 2: ~A bolles        ~%" bolles-e1 bolles-e2)
    (format t "   Pintura 1: ~A    | Pintura 2: ~A               ~%" pint-e1 pint-e2)
    (format t "==================================================~%")
    (cond 
      ;; Guanya l'equip amb mes bolles vives
      ((> bolles-e1 bolles-e2) (format t "           GUANYA L'EQUIP 1 PER BOLLES!           ~%"))
      ((> bolles-e2 bolles-e1) (format t "           GUANYA L'EQUIP 2 PER BOLLES!           ~%"))
      ;; Guanya l'equip amb mes reserva de pintura
      ((> pint-e1 pint-e2) (format t "          GUANYA L'EQUIP 1 PER PINTURA!           ~%"))
      ((> pint-e2 pint-e1) (format t "          GUANYA L'EQUIP 2 PER PINTURA!           ~%"))
      ;; Guanya un equip aleatòriament
      (t (let ((guanyador (nth (random 2) '(e1 e2))))
           (format t "          GUANYA L'EQUIP ~A PER SORT!             ~%" (cond ((eq guanyador 'e1) 1) (t 2))))))))



;; ======================================================================
;; CERCA D'UNITATS
;; ======================================================================

;; Comprova si una casella conté una base o bolla de l'equip indicat
(defun es-unitat-equip (casella equip)
  (and (eq (car casella) 'terra)            ;; Ha de ser terra
       (or (eq (caddr casella) 'base)       ;; Ha de ser una base
           (eq (caddr casella) 'bolla))     ;; o una bolla
       (eq (cadddr casella) equip)))        ;; I ha de pertànyer a l'equip

;; Recorre una fila sencera i retorna una llista amb les coordenades (x y) de les unitats
(defun busca-unitats-fila (fila equip x y)
  (cond ((null fila) nil)
        ;; Si la casella actual és una unitat nostra, afegim (x y) i seguim buscant
        ((es-unitat-equip (car fila) equip)
         (cons (list x y) (busca-unitats-fila (cdr fila) equip (+ x 1) y)))
        ;; Si no, simplement seguim buscant a la següent casella (sumant 1 a la x)
        (t
         (busca-unitats-fila (cdr fila) equip (+ x 1) y))))

;; Recorre totes les files del mapa i n'ajunta els resultats
(defun busca-unitats-mapa (mapa equip y)
  (cond ((null mapa) nil)
        (t
        ;; Rebem una llista amb les unitats de la fila actual
        ;; i la concatenem amb les unitats de les files inferiors
         (append (busca-unitats-fila (car mapa) equip 0 y)
                 (busca-unitats-mapa (cdr mapa) equip (+ y 1))))))


;; ======================================================================
;; SISTEMA DE VISIÓ
;; ======================================================================

;; Adapta la informació d'una casella del mapa al format que demana l'enunciat per a l'agent
(defun formateja-casella (coord casella dx dy)
  (let ((tipus-casella (car casella))
        (coord-despla (list (+ (car coord) dx) (+ (cadr coord) dy))))
    (cond ((eq tipus-casella 'aigua)
           ;; L'aigua només necessita coordenada i tipus
           (list coord-despla 'aigua))
          (t
           ;; La terra necessita tota la informació de l'element
           (let ((color-casella (cadr casella))
                 (element (caddr casella))
                 (equip (cadddr casella))
                 (colors-pintat (nth 4 casella))
                 (color-propi (nth 5 casella))
                 (tr-p (nth 6 casella)) ;; Temps per pintar
                 (tr-m (nth 7 casella))) ;; Temps per moure
             (list coord-despla tipus-casella color-casella element equip colors-pintat color-propi tr-p tr-m))))))

;; Recorre una fila i retorna només les caselles que estan dins del rang de visió
(defun visio-fila (fila origen-x origen-y rang x y dx dy)
  (cond ((null fila) nil)
        ;; Si la distància al quadrat és menor o igual al rang, la casella és visible
        ((<= (distancia-quadrada origen-x origen-y x y) rang)
         (cons (formateja-casella (list x y) (car fila) dx dy)
               (visio-fila (cdr fila) origen-x origen-y rang (+ x 1) y dx dy)))
        ;; Si no és visible, la ignorem i seguim amb la següent
        (t (visio-fila (cdr fila) origen-x origen-y rang (+ x 1) y dx dy))))

;; Recorre tot el mapa i ajunta les caselles visibles en una única llista
(defun visio-mapa (mapa origen-x origen-y rang y dx dy)
  (cond ((null mapa) nil)
        (t (append (visio-fila (car mapa) origen-x origen-y rang 0 y dx dy)
                   (visio-mapa (cdr mapa) origen-x origen-y rang (+ y 1) dx dy)))))


;; ======================================================================
;; EMPAQUETATGE I COMUNICACIÓ AMB ELS AGENTS
;; ======================================================================

;; Construeix la llista d'estat que necessita l'agent
(defun empaqueta-dades-unitat (mapa ronda equip pintura memoria x y dx dy)
  (let* ((casella (indexa-matriu mapa y x))
         (tipus-unitat (caddr casella))  ; Extraiem si és base o bolla
         (colors-pintat (nth 4 casella)) ; Llista de colors dels quals està pintada
         (color-propi (nth 5 casella))   ; Color de la bolla (r, g, b) o nil
         (tr-pintar (nth 6 casella))     ; Temps de recuperació per pintar
         (tr-moure (nth 7 casella))      ; Temps de recuperació per moure
         
         ;; Assignem el rang de visió segons el tipus d'unitat
         (rang-visio (cond ((eq tipus-unitat 'base) 64)
                           ((eq tipus-unitat 'bolla) 20)
                           (t 0)))
                            
         ;; Calculem què veu aquesta unitat des de la seva posició
         (visio (visio-mapa mapa x y rang-visio 0 dx dy))
         (id-unitat (nth 8 casella)))
    
    ;; Retornem la llista
    (list ronda 
          equip 
          pintura 
          id-unitat
          tipus-unitat 
          (list (+ x dx) (+ y dy)) ; Coordenada desplaçada
          colors-pintat 
          color-propi 
          tr-pintar 
          tr-moure 
          visio 
          memoria)))

;; Funció que crida a l'agent corresponent segons l'equip
(defun demana-accions-agent (dades-empaquetades)
  (let ((equip (nth 1 dades-empaquetades))) ;; Comprova a quí s'ha de cridar
    (cond 
      ((eq equip 'e1) (agent-xyz999 dades-empaquetades))
      ((eq equip 'e2) (agent-abc123 dades-empaquetades))
      (t nil))))

;; ======================================================================
;; PROCESSADOR D'ACCIONS
;; ======================================================================

;; Processe una llista d'accions
(defun-tco aplica-accions (accions mapa pintura memoria equip coord-origen ronda dx dy fletxes)
  (cond ((null accions) (list mapa pintura memoria fletxes))
        (t
         (let* ((accio (car accions))
                (tipus-accio (car accio))
                (args (cadr accio)))
           
           (cond 
             ;; ---------------------------------------------------------
             ;; ACCIÓ: CREA-BOLLA
             ;; ---------------------------------------------------------
             ((eq tipus-accio 'crea-bolla)
             ;; Treu la informació de l'acció
              (let* ((color-bolla (car args))
                     (coord-desti (cadr args))
                     (dest-x (- (car coord-desti) dx))
                     (dest-y (- (cadr coord-desti) dy))
                     (orig-x (car coord-origen))
                     (orig-y (cadr coord-origen)))
                (cond ((and (<= (distancia-quadrada orig-x orig-y dest-x dest-y) 2)
                            (>= pintura 50)) ;; Comprovam que sigui vàlida (diagonals, eixos i disponibilitat de pintura)
                       (let* ((casella-vella (indexa-matriu mapa dest-y dest-x)) ;; Treu la casella a on es vol crear la bolla
                              (color-terra (cadr casella-vella)) ;; Treu el color de la casella on es vol crear la bolla
                              (id-unitat (+ (* ronda 100000) (+ (* dest-y 1000) dest-x))) ;; Assigna un identificador únic
                              (nova-casella (list 'terra color-terra 'bolla equip nil color-bolla 0 0 id-unitat)) ;; Crea la nova casella
                              (nou-mapa (posa-dins-matriu mapa dest-y dest-x nova-casella)) ;; Crea el nou mapa
                              (nova-pintura (- pintura 50))) ;; Li treu 50 punts de pintura
                         (aplica-accions (cdr accions) nou-mapa nova-pintura memoria equip coord-origen ronda dx dy fletxes))) ;; Procesa la següent acció
                      (t (aplica-accions (cdr accions) mapa pintura memoria equip coord-origen ronda dx dy fletxes))))) ;; Procesa la següent acció si no es pot crear la bolla
             
             ;; ---------------------------------------------------------
             ;; ACCIÓ: MOU
             ;; ---------------------------------------------------------
             ((eq tipus-accio 'mou)
             ;; Treu la informació de l'acció
              (let* ((coord-desti-des (car args))
                     (dest-x (- (car coord-desti-des) dx))
                     (dest-y (- (cadr coord-desti-des) dy))
                     (orig-x (car coord-origen))
                     (orig-y (cadr coord-origen))
                     (casella-origen (indexa-matriu mapa orig-y orig-x))
                     (casella-desti (indexa-matriu mapa dest-y dest-x)))
                
                (cond ((and (< (cond ((nth 7 casella-origen) (nth 7 casella-origen)) (t 0)) 1) ;; Comprova que hagi passat temps de cooldown
                            (<= (distancia-quadrada orig-x orig-y dest-x dest-y) 2) ;; Comprova que la distància sigui menor o igual a 2
                            (eq (car casella-desti) 'terra) ;; Comprova que la casella de destí sigui terra
                            (null (caddr casella-desti))) ;; Comprova que la casella de destí no sigui una unitat
                       (let* ((color-terra-orig (cadr casella-origen))
                              (color-terra-dest (cadr casella-desti))
                              (equip-bolla (cadddr casella-origen))
                              (colors-pintat (nth 4 casella-origen))
                              (color-propi (nth 5 casella-origen))
                              (tr-pintar (nth 6 casella-origen))
                              (id-unitat (nth 8 casella-origen))
                              
                              (es-diagonal (= (+ (* (- dest-x orig-x) (- dest-x orig-x))
                                                 (* (- dest-y orig-y) (- dest-y orig-y))) 2)) ;; Calcula si el moviment és diagonal
                              (tr-base (cond (es-diagonal 1.4142) (t 1))) ;; Assigna el temps de base segons si el moviment és diagonal o no
                              (nou-tr-moure (cond ((eq color-terra-dest color-propi) tr-base) (t (* tr-base 3)))) ;; Assigna el temps de base segons si el moviment és diagonal o no i el color de la casella de destí
                              
                              (origen-buit (list 'terra color-terra-orig nil nil nil nil nil nil)) ;; Assigna la casella de origen com terra
                              (mapa-mig (posa-dins-matriu mapa orig-y orig-x origen-buit)) ;; Crea el nou mapa amb la casella de origen modificada
                              
                              (desti-ocupat (list 'terra color-terra-dest 'bolla equip-bolla colors-pintat color-propi tr-pintar nou-tr-moure id-unitat)) ;; Assigna la casella de desti com a bolla amb les característiques corresponents
                              (nou-mapa (posa-dins-matriu mapa-mig dest-y dest-x desti-ocupat))) ;; Crea el nou mapa amb la casella de destí modificada
                         
                         (aplica-accions (cdr accions) nou-mapa pintura memoria equip (list dest-x dest-y) ronda dx dy (cons (list 'mou coord-origen (list dest-x dest-y)) fletxes)))) ;; Procesa la següent acció
                      (t (aplica-accions (cdr accions) mapa pintura memoria equip coord-origen ronda dx dy fletxes))))) ;; Procesa la següent acció si no es pot moure la bolla
             
             ;; ---------------------------------------------------------
             ;; ACCIÓ: PINTA
             ;; ---------------------------------------------------------
             ((eq tipus-accio 'pinta)
             ;; Treu la informació de l'acció
              (let* ((coord-desti-des (car args)) 
                     (dest-x (- (car coord-desti-des) dx))
                     (dest-y (- (cadr coord-desti-des) dy))
                     (orig-x (car coord-origen))
                     (orig-y (cadr coord-origen))
                     (casella-origen (indexa-matriu mapa orig-y orig-x))
                     (casella-desti (indexa-matriu mapa dest-y dest-x)))
                
                (cond ((and (< (cond ((nth 6 casella-origen) (nth 6 casella-origen)) (t 0)) 1) ;; Comprova que hagi passat temps de cooldown
                            (<= (distancia-quadrada orig-x orig-y dest-x dest-y) 5) ;; Comprova que la distància sigui menor o igual a 5
                            (eq (car casella-desti) 'terra)) ;; Comprova que la casella de destí sigui terra
                       (let* ((color-terra-orig (cadr casella-origen))
                              (equip-tirador (cadddr casella-origen))
                              (color-tirador (nth 5 casella-origen))
                              
                              (nou-tr-pintar (cond ((eq color-terra-orig color-tirador) 3) (t 9))) ;; Assigna el temps de base segons el color de la casella de destí
                              
                              (origen-actualitzat (list 'terra color-terra-orig 'bolla equip-tirador
                                                        (nth 4 casella-origen) color-tirador 
                                                        nou-tr-pintar (nth 7 casella-origen)
                                                        (nth 8 casella-origen))) ;; Actualitza la casella d'origen amb el nou temps de cooldown de pintada
                              (mapa-mig (posa-dins-matriu mapa orig-y orig-x origen-actualitzat)) ;; Crea el mapa amb la casella d'origen actualitzada
                              
                              (element-desti (caddr casella-desti))
                              (equip-desti (cadddr casella-desti))
                              (colors-desti (nth 4 casella-desti))
                              (color-propi-desti (nth 5 casella-desti))
                              (tr-p-desti (nth 6 casella-desti))
                              (tr-m-desti (nth 7 casella-desti))
                              
                              (nous-colors-desti 
                               (cond ((and element-desti (not (eq element-desti 'lab))) ;; Comprova que la casella de destí no sigui un laboratori
                                      (cond ((member color-tirador colors-desti) colors-desti) ;; Comprova si el color del tirador ja esta en la llista de colors
                                            (t (cons color-tirador colors-desti)))) ;; Si el color del tirador no esta en la llista de colors, s'afegeix
                                     (t colors-desti))) ;; Actualitza els colors de la casella de destí
                              
                              (colors-totals (cons color-propi-desti nous-colors-desti)) ;; Crea la llista de colors totals
                              
                              (explota (and (member 'r colors-totals)
                                            (member 'g colors-totals)
                                            (member 'b colors-totals))) ;; Comprova si la casella de destí ha explotat
                              
                              (desti-actualitzat 
                               (cond 
                                  (explota 
                                   (list 'terra color-tirador nil nil nil nil nil nil)) ;; Si la casella de destí ha explotat, s'afegeix com a terra amb el color del tirador
                                  ((eq element-desti 'lab)
                                   (list 'terra color-tirador 'lab equip-tirador nil nil nil nil)) ;; Si la casella de destí es un laboratori, s'afegeix com a laboratori amb el color del tirador
                                  (t
                                   (list 'terra color-tirador element-desti equip-desti nous-colors-desti color-propi-desti tr-p-desti tr-m-desti (nth 8 casella-desti))))) ;; Si la casella de destí no es un laboratori, s'afegeix com a terra amb el color del tirador i els colors actualitzats
                              
                              (nou-mapa (posa-dins-matriu mapa-mig dest-y dest-x desti-actualitzat))) ;; Crea el mapa amb la casella de destí actualitzada
                         
                         (aplica-accions (cdr accions) nou-mapa pintura memoria equip coord-origen ronda dx dy (cons (list 'pinta coord-origen (list dest-x dest-y)) fletxes)))) ;; Procesa la següent acció
                      (t (aplica-accions (cdr accions) mapa pintura memoria equip coord-origen ronda dx dy fletxes)))))
             
             ;; ---------------------------------------------------------
             ;; ACCIÓ: ESCRIU-MEMORIA
             ;; ---------------------------------------------------------
             ((eq tipus-accio 'escriu-memoria)
              (let ((nova-mem (car args))) ;; Guarda la nova memòria
                (aplica-accions (cdr accions) mapa pintura nova-mem equip coord-origen ronda dx dy fletxes)))

             ;; ---------------------------------------------------------
             ;; IGNORAR ALTRES ACCIONS
             ;; ---------------------------------------------------------
             (t (aplica-accions (cdr accions) mapa pintura memoria equip coord-origen ronda dx dy fletxes)))))))

;; Demana accions a cada unitat i les aplica seqüencialment
(defun-tco processa-totes-les-unitats (unitats mapa ronda pintura equip memoria dx dy fletxes)
  (cond ((null unitats) (list mapa pintura memoria fletxes))
        (t
         (let* ((coord (car unitats)) ;; Empaquetem les dades de la unitat actual
                (x (car coord)) ;; X de la unitat actual
                (y (cadr coord)) ;; Y de la unitat actual
                (dades (empaqueta-dades-unitat mapa ronda equip pintura memoria x y dx dy)) ;; Dades de la unitat actual
                
                (accions (demana-accions-agent dades)) ;; Accions de la unitat actual
                
                (resultat-accions (aplica-accions accions mapa pintura memoria equip coord ronda dx dy fletxes)) ;; Accions aplicades
                (mapa-post-accions (car resultat-accions)) ;; Mapa post accions
                (pintura-post-accions (cadr resultat-accions)) ;; Pintura post accions
                (memoria-post-accions (caddr resultat-accions)) ;; Memoria post accions
                (fletxes-post-accions (nth 3 resultat-accions))) ;; Fletxes post accions

           (processa-totes-les-unitats (cdr unitats) ;; Següent unitat
                                       mapa-post-accions ;; Mapa amb accions aplicades
                                       ronda 
                                       pintura-post-accions ;; Pintura amb accions aplicades
                                       equip 
                                       memoria-post-accions ;; Memoria amb accions aplicades
                                       dx
                                       dy fletxes-post-accions)))))

;; ======================================================================
;; ACTUALITZACIÓ DELS TEMPS DE RECUPERACIÓ
;; ======================================================================

;; Decrementa el temps de recuperació d'una unitat
(defun decrementa-temps (t-recup)
  (cond ((null t-recup) nil)     ; Les bases tenen nil
        ((<= t-recup 1) 0)       ; Si és 1, 0.5 o 0, es queda en 0 (a punt per actuar)
        (t (- t-recup 1))))      ; Si és major que 1, li restam 1

;; Redueix els temps de recuperació de les unitats d'un equip
(defun redueix-temps-casella (casella equip)
  (cond ((eq (car casella) 'aigua) casella) ;; Si es aigua, no es fa res
        ((eq (car casella) 'terra) ;; Si es terra
         (let ((color-terra (cadr casella)) ;; Color de la terra
               (element (caddr casella)) ;; Element a la terra
               (equip-casella (cadddr casella)) ;; Equip a la terra
               (colors-pintat (nth 4 casella)) ;; Colors de la terra
               (color-propi (nth 5 casella)) ;; Color propi de la terra
               (tr-pintar (nth 6 casella)) ;; Temps de recuperació de pintar
               (tr-moure (nth 7 casella))) ;; Temps de recuperació de moure
           (cond ((and element (eq equip-casella equip)) ;; Si hi ha una unitat i és de l'equip actiu, reduïm els seus temps
                  (list 'terra color-terra element equip-casella colors-pintat color-propi 
                        (decrementa-temps tr-pintar) ;; Redueix el temps de recuperació de pintar
                        (decrementa-temps tr-moure) ;; Redueix el temps de recuperació de moure
                        (nth 8 casella))) ;; ID de la unitat
                 (t casella)))) ;; Si no és de l'equip o està buida, no la toquem
        (t casella)))

;; Recorre la fila actualitzant els temps de les unitats de l'equip
(defun redueix-temps-fila (fila equip)
  (cond ((null fila) nil)
        (t (cons (redueix-temps-casella (car fila) equip)
                 (redueix-temps-fila (cdr fila) equip)))))

;; Redueix els temps de recuperació de les unitats d'un equip
(defun redueix-temps-mapa (mapa equip)
  (cond ((null mapa) nil)
        (t (cons (redueix-temps-fila (car mapa) equip)
                 (redueix-temps-mapa (cdr mapa) equip)))))

;; ======================================================================
;; ECONOMIA: COMPTAR LABORATORIOS
;; ======================================================================

;; Compta laboratoris d'un equip en una fila
(defun compta-labs-fila (fila equip)
  (cond ((null fila) 0)
        (t (let ((casella (car fila)))
             (cond ((and (eq (car casella) 'terra)
                         (eq (caddr casella) 'lab)
                         (eq (cadddr casella) equip))
                    (+ 1 (compta-labs-fila (cdr fila) equip)))
                   (t (compta-labs-fila (cdr fila) equip)))))))

;; Compta laboratoris d'un equip en el mapa
(defun compta-labs-mapa (mapa equip)
  (cond ((null mapa) 0)
        (t (+ (compta-labs-fila (car mapa) equip)
              (compta-labs-mapa (cdr mapa) equip)))))