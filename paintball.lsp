;; ======================================================================
;; PRÀCTICA FINAL LLENGUATGES DE PROGRAMACIÓ - LISP - PAINTBALL
;; ======================================================================
;; Estudiants: ABC, XYZ
;; Data: 25/04/2026
;; Assignatura: Llenguatges de Programació (LP)
;; Grup: <Indicar Grup>
;; Professors: <Indicar Professors>
;; Convocatòria: Primera Convocatòria (Ordinària)
;;
;; ----------------------------------------------------------------------
;; FITXER: paintball.lsp
;; DESCRIPCIÓ: Controlador principal del joc. Gestiona el bucle de 
;; partida, l'alternança de torns, l'execució d'accions dels agents,
;; la validació de regles i les condicions de victòria.
;;
;; INSTRUCCIONS D'ÚS:
;; 1. Carregar el fitxer: (load "paintball.lsp")
;; 2. Iniciar la partida: (inici)
;; 
;; DISSENY FUNCIONAL:
;; S'ha seguit un enfocament purament funcional. L'estat del joc es
;; passa com a paràmetre en les crides recursives. S'ha fet ús de
;; l'optimització de crides en posició final (TCO) per permetre 
;; partides de llarga durada sense desbordament de pila.
;;
;; ASPECTES OPCIONALS IMPLEMENTATS:
;; - Sistema d'identificadors únics (id-unitat) per a cada unitat.
;; - Validació estricta de rangs i cooldowns en el controlador.
;; - Coordenades desplazades aleatòriament (anti-deducció de mapa).
;; - Memòria compartida funcional entre unitats del mateix equip.
;; - Límit de 1500 torns amb sistema de finalització.
;; ----------------------------------------------------------------------

;; Necessari per a l'optimització de crides recursives.
(cond ((not (boundp '*features*)) (setq *features* nil)))
(load "proyectos/projecte_inicial/common.lsp") ; https://almy.us/files/xl305req.zip
(load "proyectos/projecte_inicial/tco.lsp")    ; https://github.com/antoni-oliver/defun-tco

;; Altres fitxers de la pràctica:
(load "proyectos/projecte_inicial/funciones_auxiliares.lsp")
(load "proyectos/projecte_inicial/grafics.lsp")
(load "proyectos/projecte_inicial/agent-abc123")
(load "proyectos/projecte_inicial/agent-xyz999")


(defun inici ()
  "Punt d'entrada del programa."
  (BLACK) 
  (mode 0 0 640 375) 
  
  ;; Llegim el mapa del fitxer i el guardam localment
  (let ((mapa-inicial (carrega-mapa "proyectos/projecte_inicial/maps/bait.map")))
    
    ;; Li passam el mapa al mòdul gràfic perquè el pinti
    (inicia-partida mapa-inicial)
    
  )

  (BLACK) 
  t)

(defun carrega-mapa (nom-fitxer)
  "Llegeix el mapa des d'un fitxer de text i retorna la llista."
  (let* ((canal (open nom-fitxer :direction :input))
         (mapa (read canal)))  ; Llegeix la llista directament
    (close canal)              ; És molt important tancar el fitxer!
    mapa)
)


;; ======================================================================
;; GESTIÓ D'IDENTIFICADORS ÚNICS (Bug 3)
;; ======================================================================

(defun prepara-casella-inicial (casella x y)
  "Afegeix un ID únic (0 x y) a les unitats inicials del mapa."
  (cond ((and (eq (car casella) 'terra) 
              (or (eq (caddr casella) 'base) (eq (caddr casella) 'bolla)))
         (list 'terra (cadr casella) (caddr casella) (cadddr casella) 
               (nth 4 casella) (nth 5 casella) (nth 6 casella) (nth 7 casella) 
               (list 0 x y)))
        (t casella)))

(defun prepara-fila-inicial (fila x y)
  (cond ((null fila) nil)
        (t (cons (prepara-casella-inicial (car fila) x y)
                 (prepara-fila-inicial (cdr fila) (+ x 1) y)))))

(defun prepara-mapa-inicial (mapa y)
  (cond ((null mapa) nil)
        (t (cons (prepara-fila-inicial (car mapa) 0 y)
                 (prepara-mapa-inicial (cdr mapa) (+ y 1))))))


;; ======================================================================
;; CONTROLADOR GENERAL
;; ======================================================================

(defun inicia-partida (mapa-inicial)
  "Prepara l'estat inicial i llança el bucle principal de la partida."
  ;; Generem el desplazamiento aleatori fix per a tota la partida (Bug 6)
  (let ((dx (random 1000))
        (dy (random 1000)))
    (format t "~%[SISTEMA] Coordenades desplazades per dx=~A, dy=~A~%" dx dy)
    (bucle-partida 1 (prepara-mapa-inicial mapa-inicial 0) 200 200 nil nil dx dy)))


(defun-tco bucle-partida (ronda mapa pint-e1 pint-e2 mem-e1 mem-e2 dx dy)
  "El motor principal del joc. S'executa recursivament a cada torn."
  
  (dibuixa-mapa mapa)
  (BLACK)
  (format t "~%--- RONDA ~A ---~%" ronda)
  (format t "Pintura E1: ~A | Pintura E2: ~A~%" pint-e1 pint-e2)
  
  (cond 
    ;; 1. VICTÒRIA EQUIP 2 (La base de l'E1 ha desaparegut)
    ((= (compta-bases-mapa mapa 'e1) 0)
     (format t "~%==================================================~%")
     (format t "    VICTORIA! LA BASE DE L'EQUIP 1 HA EXPLOTAT    ~%")
     (format t "               GUANYA L'EQUIP 2!                  ~%")
     (format t "==================================================~%")
     'fi-de-partida)

    ;; 2. VICTÒRIA EQUIP 1 (La base de l'E2 ha desaparegut)
    ((= (compta-bases-mapa mapa 'e2) 0)
     (format t "~%==================================================~%")
     (format t "    VICTORIA! LA BASE DE L'EQUIP 2 HA EXPLOTAT    ~%")
     (format t "               GUANYA L'EQUIP 1!                  ~%")
     (format t "==================================================~%")
     'fi-de-partida)

    ;; 3. EMPAT PER LÍMIT DE TORNS
    ((> ronda 1500)
     (format t "~%Final de la partida: Límit de 1500 torns assolit!~%")
     ;; Aquí més endavant podem cridar a la funció de desempat
     'fi-de-partida)
        
        (t 
         (let* ((equip-actiu (cond ((= (mod ronda 2) 1) 'e1)
                                   (t 'e2)))
                
                ;; 1. Sumem la pintura passiva
                (pint-e1-inici (cond ((eq equip-actiu 'e1) (+ pint-e1 2 (compta-labs-mapa mapa 'e1))) (t pint-e1)))
                (pint-e2-inici (cond ((eq equip-actiu 'e2) (+ pint-e2 2 (compta-labs-mapa mapa 'e2))) (t pint-e2)))
                
                (pintura-actual-equip (cond ((eq equip-actiu 'e1) pint-e1-inici) (t pint-e2-inici)))
                (memoria-actual-equip (cond ((eq equip-actiu 'e1) mem-e1) (t mem-e2)))
                
                ;; 1.5. APLIQUEM EL DESCANS!
                (mapa-descansat (redueix-temps-mapa mapa equip-actiu))
                
                ;; 2. Busquem unitats
                (unitats-actuants (busca-unitats-mapa mapa-descansat equip-actiu 0))
                
                ;; 3. Processem accions
                (estat-resultant (processa-totes-les-unitats unitats-actuants mapa-descansat ronda pintura-actual-equip equip-actiu memoria-actual-equip dx dy))
                
                (nou-mapa (car estat-resultant))
                (nova-pintura-equip (cadr estat-resultant))
                (nova-memoria-equip (caddr estat-resultant))
                
                (nova-pint-e1 (cond ((eq equip-actiu 'e1) nova-pintura-equip) (t pint-e1-inici)))
                (nova-pint-e2 (cond ((eq equip-actiu 'e2) nova-pintura-equip) (t pint-e2-inici)))
                (nova-mem-e1 (cond ((eq equip-actiu 'e1) nova-memoria-equip) (t mem-e1)))
                (nova-mem-e2 (cond ((eq equip-actiu 'e2) nova-memoria-equip) (t mem-e2))))
           
           ;; 4. PAUSA MANUAL CLÀSSICA
           (BLACK)
           (format t ">> TECLA 'Enter' PER PASSAR AL SEGUENT TORN")
           (read-line)
           
           ;; 5. Crida recursiva neta (ara amb dx i dy)
           (bucle-partida (+ ronda 1) 
                          nou-mapa 
                          nova-pint-e1 
                          nova-pint-e2 
                          nova-mem-e1 
                          nova-mem-e2
                          dx
                          dy)))))

;; ======================================================================
;; CONDICIÓ DE VICTÒRIA: COMPTAR BASES
;; ======================================================================

(defun compta-bases-fila (fila equip)
  "Compta quantes bases té un equip en una fila."
  (cond ((null fila) 0)
        (t (let ((casella (car fila)))
             (cond ((and (eq (car casella) 'terra)
                         (eq (caddr casella) 'base)
                         (eq (cadddr casella) equip))
                    (+ 1 (compta-bases-fila (cdr fila) equip)))
                   (t (compta-bases-fila (cdr fila) equip)))))))

(defun compta-bases-mapa (mapa equip)
  "Compta quantes bases té un equip en tot el mapa."
  (cond ((null mapa) 0)
        (t (+ (compta-bases-fila (car mapa) equip)
              (compta-bases-mapa (cdr mapa) equip)))))

;; ======================================================================
;; CERCA D'UNITATS
;; ======================================================================

(defun es-unitat-equip (casella equip)
  "Comprova si una casella conté una 'base' o 'bolla' de l'equip indicat."
  ;; A LISP, fer (caddr '(terra g)) retorna NIL sense donar error, el que és perfecte.
  (and (eq (car casella) 'terra)            ;; Ha de ser terra
       (or (eq (caddr casella) 'base)       ;; Ha de ser una base
           (eq (caddr casella) 'bolla))     ;; ... o una bolla
       (eq (cadddr casella) equip)))        ;; I ha de pertànyer a l'equip

(defun busca-unitats-fila (fila equip x y)
  "Recorre una fila sencera i retorna una llista amb les coordenades (x y) de les unitats."
  (cond ((null fila) nil)
        ;; Si la casella actual és una unitat nostra, afegim (x y) i seguim buscant
        ((es-unitat-equip (car fila) equip)
         (cons (list x y) (busca-unitats-fila (cdr fila) equip (+ x 1) y)))
        ;; Si no, simplement seguim buscant a la següent casella (sumant 1 a la x)
        (t
         (busca-unitats-fila (cdr fila) equip (+ x 1) y))))

(defun busca-unitats-mapa (mapa equip y)
  "Recorre totes les files del mapa i n'ajunta els resultats."
  (cond ((null mapa) nil)
        (t
         ;; Usam 'append' per unir la llista de coordenades de la fila actual 
         ;; amb les llistes de les files inferiors.
         (append (busca-unitats-fila (car mapa) equip 0 y)
                 (busca-unitats-mapa (cdr mapa) equip (+ y 1))))))


;; ======================================================================
;; SISTEMA DE VISIÓ
;; ======================================================================

(defun distancia-quadrada (ax ay bx by)
  "Calcula la distància euclidiana al quadrat entre dos punts (ax, ay) i (bx, by)."
  (+ (* (- ax bx) (- ax bx))
     (* (- ay by) (- ay by))))

(defun formateja-casella (coord casella dx dy)
  "Adapta la informació d'una casella del mapa al format que demana l'enunciat per a la visió."
  (let ((tipus-casella (car casella))
        (coord-despla (list (+ (car coord) dx) (+ (cadr coord) dy))))
    (cond ((eq tipus-casella 'aigua)
           ;; L'aigua només necessita coordenada i tipus
           (list coord-despla 'aigua))
          (t
           ;; La terra necessita tota la informació de l'element que hi ha a sobre
           (let ((color-casella (cadr casella))
                 (element (caddr casella))
                 (equip (cadddr casella)))
             ;; Retornem: (coord-despla tipus color element equip colors-pintat color-propi tr-pintar tr-moure)
             (list coord-despla tipus-casella color-casella element equip nil nil nil nil))))))

(defun visio-fila (fila origen-x origen-y rang x y dx dy)
  "Recorre una fila i retorna només les caselles que estan dins del rang de visió."
  (cond ((null fila) nil)
        ;; Si la distància al quadrat és menor o igual al rang, la casella és visible
        ((<= (distancia-quadrada origen-x origen-y x y) rang)
         (cons (formateja-casella (list x y) (car fila) dx dy)
               (visio-fila (cdr fila) origen-x origen-y rang (+ x 1) y dx dy)))
        ;; Si no és visible, la ignorem i seguim amb la següent
        (t (visio-fila (cdr fila) origen-x origen-y rang (+ x 1) y dx dy))))

(defun visio-mapa (mapa origen-x origen-y rang y dx dy)
  "Recorre tot el mapa i ajunta les caselles visibles en una única llista."
  (cond ((null mapa) nil)
        (t (append (visio-fila (car mapa) origen-x origen-y rang 0 y dx dy)
                   (visio-mapa (cdr mapa) origen-x origen-y rang (+ y 1) dx dy)))))


;; ======================================================================
;; EMPAQUETATGE I COMUNICACIÓ AMB ELS AGENTS
;; ======================================================================

(defun empaqueta-dades-unitat (mapa ronda equip pintura memoria x y dx dy)
  "Construeix la llista d'estat exacta que necessita l'agent per prendre decisions."
  ;; Recorda que a la teva funció indexa-matriu, el segon paràmetre és la fila (y) i el tercer la columna (x)
  (let* ((casella (indexa-matriu mapa y x))
         (tipus-unitat (caddr casella))  ; Extraiem si és 'base o 'bolla
         
         ;; Extraiem la resta de dades (si la casella encara no té aquesta info extensa, seran nil)
         (colors-pintat (nth 4 casella)) ; Llista de colors dels quals està pintada
         (color-propi (nth 5 casella))   ; Color de la bolla ('r, 'g, 'b) o nil
         (tr-pintar (nth 6 casella))     ; Temps de recuperació per pintar
         (tr-moure (nth 7 casella))      ; Temps de recuperació per moure
         
         ;; Assignem el rang de visió correcte segons el tipus d'unitat
         (rang-visio (cond ((eq tipus-unitat 'base) 64)
                           ((eq tipus-unitat 'bolla) 20)
                           (t 0)))
                           
         ;; Calculem què veu aquesta unitat des de la seva posició (aplicant dx/dy a la visió)
         (visio (visio-mapa mapa x y rang-visio 0 dx dy))
         
         ;; Extraiem l'ID únic emmagatzemat a la casella (Bug 3)
         (id-unitat (nth 8 casella)))
    
    ;; Retornem la llista estructurada exactament com demana l'enunciat
    (list ronda 
          equip 
          pintura 
          id-unitat
          tipus-unitat 
          (list (+ x dx) (+ y dy)) ; coordenada desplazada
          colors-pintat 
          color-propi 
          tr-pintar 
          tr-moure 
          visio 
          memoria)))

(defun demana-accions-agent (dades-empaquetades)
  "Crida a l'agent i unifica el format perquè tots retornin (accions memòria)."
  (let ((equip (cadr dades-empaquetades))
        (memoria-actual (nth 11 dades-empaquetades))) ; Recuperem la memòria actual (índex corregit)
    
    (cond 
      ;; Si és l'agent ANTIC (abc123), només retorna accions, així que li peguem la memòria nosaltres
      ((eq equip 'e1) 
       (list (agent-abc123 dades-empaquetades) memoria-actual))
      
      ;; Si és el NOU agent (xyz999), ell ja retorna (accions nova-memòria)
      ((eq equip 'e2) 
       (agent-xyz999 dades-empaquetades))
      
      (t (list nil memoria-actual)))))

;; ======================================================================
;; PROCESSADOR D'ACCIONS
;; ======================================================================

(defun-tco aplica-accions (accions mapa pintura equip coord-origen ronda dx dy)
  "Aplica recursivament una llista d'accions retornant el nou (mapa pintura)."
  (cond ((null accions) (list mapa pintura))
        (t
         (let* ((accio (car accions))
                (tipus-accio (car accio))
                (args (cadr accio)))
           
           (cond 
             ;; ---------------------------------------------------------
             ;; ACCIÓ: CREA-BOLLA
             ;; ---------------------------------------------------------
             ((eq tipus-accio 'crea-bolla)
              (let* ((color-bolla (car args))
                     (coord-desti (cadr args))
                     (dest-x (- (car coord-desti) dx))
                     (dest-y (- (cadr coord-desti) dy))
                     (orig-x (car coord-origen))
                     (orig-y (cadr coord-origen)))
                (cond ((and (<= (distancia-quadrada orig-x orig-y dest-x dest-y) 2)
                            (>= pintura 50))
                       (let* ((casella-vella (indexa-matriu mapa dest-y dest-x))
                              (color-terra (cadr casella-vella))
                              (nova-casella (list 'terra color-terra 'bolla equip nil color-bolla 0 0 (list ronda dest-x dest-y)))
                              (nou-mapa (posa-dins-matriu mapa dest-y dest-x nova-casella))
                              (nova-pintura (- pintura 50)))
                         (aplica-accions (cdr accions) nou-mapa nova-pintura equip coord-origen ronda dx dy)))
                      (t (aplica-accions (cdr accions) mapa pintura equip coord-origen ronda dx dy)))))
             
             ;; ---------------------------------------------------------
             ;; ACCIÓ: MOU
             ;; ---------------------------------------------------------
             ((eq tipus-accio 'mou)
              (let* ((coord-desti-des args)
                     (dest-x (- (car coord-desti-des) dx))
                     (dest-y (- (cadr coord-desti-des) dy))
                     (orig-x (car coord-origen))
                     (orig-y (cadr coord-origen))
                     (casella-origen (indexa-matriu mapa orig-y orig-x))
                     (casella-desti (indexa-matriu mapa dest-y dest-x)))
                
                (cond ((and (< (cond ((nth 7 casella-origen) (nth 7 casella-origen)) (t 0)) 1)
                            (<= (distancia-quadrada orig-x orig-y dest-x dest-y) 2)
                            (eq (car casella-desti) 'terra)
                            (null (caddr casella-desti)))
                       (let* ((color-terra-orig (cadr casella-origen))
                              (color-terra-dest (cadr casella-desti))
                              (equip-bolla (cadddr casella-origen))
                              (colors-pintat (nth 4 casella-origen))
                              (color-propi (nth 5 casella-origen))
                              (tr-pintar (nth 6 casella-origen))
                              (id-unitat (nth 8 casella-origen))
                              
                              (es-diagonal (= (+ (* (- dest-x orig-x) (- dest-x orig-x))
                                                 (* (- dest-y orig-y) (- dest-y orig-y))) 2))
                              (tr-base (cond (es-diagonal 1.4142) (t 1)))
                              (nou-tr-moure (cond ((eq color-terra-dest color-propi) tr-base) (t (* tr-base 3))))
                              
                              (origen-buit (list 'terra color-terra-orig nil nil nil nil nil nil))
                              (mapa-mig (posa-dins-matriu mapa orig-y orig-x origen-buit))
                              
                              (desti-ocupat (list 'terra color-terra-dest 'bolla equip-bolla colors-pintat color-propi tr-pintar nou-tr-moure id-unitat))
                              (nou-mapa (posa-dins-matriu mapa-mig dest-y dest-x desti-ocupat)))
                         
                         (aplica-accions (cdr accions) nou-mapa pintura equip (list dest-x dest-y) ronda dx dy)))
                      (t (aplica-accions (cdr accions) mapa pintura equip coord-origen ronda dx dy)))))
             
             ;; ---------------------------------------------------------
             ;; ACCIÓ: PINTA
             ;; ---------------------------------------------------------
             ((eq tipus-accio 'pinta)
              (let* ((coord-desti-des args) 
                     (dest-x (- (car coord-desti-des) dx))
                     (dest-y (- (cadr coord-desti-des) dy))
                     (orig-x (car coord-origen))
                     (orig-y (cadr coord-origen))
                     (casella-origen (indexa-matriu mapa orig-y orig-x))
                     (casella-desti (indexa-matriu mapa dest-y dest-x)))
                
                (cond ((and (< (cond ((nth 6 casella-origen) (nth 6 casella-origen)) (t 0)) 1)
                            (<= (distancia-quadrada orig-x orig-y dest-x dest-y) 5)
                            (eq (car casella-desti) 'terra))
                       (let* ((color-terra-orig (cadr casella-origen))
                              (equip-tirador (cadddr casella-origen))
                              (color-tirador (nth 5 casella-origen))
                              
                              (nou-tr-pintar (cond ((eq color-terra-orig color-tirador) 1) (t 3)))
                              
                              (origen-actualitzat (list 'terra color-terra-orig 'bolla equip-tirador
                                                        (nth 4 casella-origen) color-tirador 
                                                        nou-tr-pintar (nth 7 casella-origen)
                                                        (nth 8 casella-origen)))
                              (mapa-mig (posa-dins-matriu mapa orig-y orig-x origen-actualitzat))
                              
                              (element-desti (caddr casella-desti))
                              (equip-desti (cadddr casella-desti))
                              (colors-desti (nth 4 casella-desti))
                              (color-propi-desti (nth 5 casella-desti))
                              (tr-p-desti (nth 6 casella-desti))
                              (tr-m-desti (nth 7 casella-desti))
                              
                              (nous-colors-desti 
                               (cond ((and element-desti (not (eq element-desti 'lab)))
                                      (cond ((member color-tirador colors-desti) colors-desti)
                                            (t (cons color-tirador colors-desti))))
                                     (t colors-desti)))
                              
                              (colors-totals (cons color-propi-desti nous-colors-desti))
                              
                              (explota (and (member 'r colors-totals)
                                            (member 'g colors-totals)
                                            (member 'b colors-totals)))
                              
                              (desti-actualitzat 
                               (cond 
                                 (explota 
                                  (list 'terra color-tirador nil nil nil nil nil nil))
                                 ((eq element-desti 'lab)
                                  (list 'terra color-tirador 'lab equip-tirador nil nil nil nil))
                                 (t
                                  (list 'terra color-tirador element-desti equip-desti nous-colors-desti color-propi-desti tr-p-desti tr-m-desti (nth 8 casella-desti)))))
                              
                              (nou-mapa (posa-dins-matriu mapa-mig dest-y dest-x desti-actualitzat)))
                         
                         (aplica-accions (cdr accions) nou-mapa pintura equip coord-origen ronda dx dy)))
                      (t (aplica-accions (cdr accions) mapa pintura equip coord-origen ronda dx dy)))))
             
             ;; ---------------------------------------------------------
             ;; IGNORAR ALTRES ACCIONS
             ;; ---------------------------------------------------------
             (t (aplica-accions (cdr accions) mapa pintura equip coord-origen ronda dx dy)))))))


(defun-tco processa-totes-les-unitats (unitats mapa ronda pintura equip memoria dx dy)
  "Demana accions a cada unitat i les aplica seqüencialment. Retorna (nou-mapa nova-pintura nova-memoria)."
  (cond ((null unitats) (list mapa pintura memoria))
        (t
         (let* ((coord (car unitats))
                (x (car coord))
                (y (cadr coord))
                ;; 1. Empaquetem el que veu aquesta unitat (amb desplazamiento)
                (dades (empaqueta-dades-unitat mapa ronda equip pintura memoria x y dx dy))
                
                ;; 2. Cridem l'agent intel·ligent (que ara sempre retorna una llista de 2 elements)
                (resposta-agent (demana-accions-agent dades))
                (accions (car resposta-agent))        ;; El primer element són les accions
                (nova-memoria (cadr resposta-agent))  ;; El segon element és la llibreta actualitzada!
                
                ;; 3. Apliquem les accions al mapa (des-desplaçant abans)
                (resultat-accions (aplica-accions accions mapa pintura equip coord ronda dx dy))
                (mapa-post-accions (car resultat-accions))
                (pintura-post-accions (cadr resultat-accions)))
           
           ;; 4. Crida recursiva per a la següent unitat! 
           ;; ATENCIÓ: Li passem la NOVA-MEMORIA perquè la següent bolla ja sàpiga el que ha vist aquesta!
           (processa-totes-les-unitats (cdr unitats) 
                                       mapa-post-accions 
                                       ronda 
                                       pintura-post-accions 
                                       equip 
                                       nova-memoria
                                       dx
                                       dy)))))


;; ======================================================================
;; ACTUALITZACIÓ DELS TEMPS DE RECUPERACIÓ (COOLDOWNS)
;; ======================================================================

(defun decrementa-temps (t-recup)
  "Resta 1 al temps de recuperació, amb un mínim de 0."
  (cond ((null t-recup) nil)     ; Les bases tenen nil
        ((<= t-recup 1) 0)       ; Si és 1, 0.5 o 0, es queda en 0 (a punt per actuar)
        (t (- t-recup 1))))      ; Si és major que 1, li restam 1

(defun redueix-temps-casella (casella equip)
  "Retorna una casella nova amb els temps reduïts si pertany a l'equip."
  (cond ((eq (car casella) 'aigua) casella)
        ((eq (car casella) 'terra)
         (let ((color-terra (cadr casella))
               (element (caddr casella))
               (equip-casella (cadddr casella))
               (colors-pintat (nth 4 casella))
               (color-propi (nth 5 casella))
               (tr-pintar (nth 6 casella))
               (tr-moure (nth 7 casella)))
           ;; Si hi ha una unitat i és de l'equip actiu, reduïm els seus temps
           (cond ((and element (eq equip-casella equip))
                  (list 'terra color-terra element equip-casella colors-pintat color-propi 
                        (decrementa-temps tr-pintar) 
                        (decrementa-temps tr-moure)
                        (nth 8 casella)))
                 (t casella)))) ; Si no és de l'equip o està buida, no la toquem
        (t casella)))

(defun redueix-temps-fila (fila equip)
  "Recorre la fila actualitzant els temps de les unitats de l'equip."
  (cond ((null fila) nil)
        (t (cons (redueix-temps-casella (car fila) equip)
                 (redueix-temps-fila (cdr fila) equip)))))

(defun redueix-temps-mapa (mapa equip)
  "Recorre el mapa sencer per actualitzar els temps de recuperació."
  (cond ((null mapa) nil)
        (t (cons (redueix-temps-fila (car mapa) equip)
                 (redueix-temps-mapa (cdr mapa) equip)))))

;; ======================================================================
;; ECONOMIA: COMPTAR LABORATORIOS
;; ======================================================================

(defun compta-labs-fila (fila equip)
  "Compta quants laboratoris té un equip en una fila."
  (cond ((null fila) 0)
        (t (let ((casella (car fila)))
             (cond ((and (eq (car casella) 'terra)
                         (eq (caddr casella) 'lab)
                         (eq (cadddr casella) equip))
                    (+ 1 (compta-labs-fila (cdr fila) equip)))
                   (t (compta-labs-fila (cdr fila) equip)))))))

(defun compta-labs-mapa (mapa equip)
  "Compta quants laboratoris té un equip en tot el mapa."
  (cond ((null mapa) 0)
        (t (+ (compta-labs-fila (car mapa) equip)
              (compta-labs-mapa (cdr mapa) equip)))))
