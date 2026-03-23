;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del controlador principal.
;; <Descripció de les funcions d'aquest fitxer>

;; Necessari per a l'optimització de crides recursives.
(load "proyectos/projecte_inicial/common.lsp") ; https://almy.us/files/xl305req.zip
(load "proyectos/projecte_inicial/tco.lsp")    ; https://github.com/antoni-oliver/defun-tco

;; Altres fitxers de la pràctica:
(load "proyectos/projecte_inicial/funciones_auxiliares.lsp")
(load "proyectos/projecte_inicial/grafics.lsp")
(load "proyectos/projecte_inicial/agent-abc123")
(load "proyectos/projecte_inicial/agent-xyz999")


(defun inici ()
  "Punt d'entrada del programa."
  (color 0 0 0 255 255 255) 
  (mode 0 0 640 375) 
  
  ;; Llegim el mapa del fitxer i el guardam localment
  (let ((mapa-inicial (carrega-mapa "proyectos/projecte_inicial/maps/tiny.map")))
    
    ;; Li passam el mapa al mòdul gràfic perquè el pinti
    (inicia-partida mapa-inicial)
    
  )

  (color 0 0 0 255 255 255) 
  t)

(defun carrega-mapa (nom-fitxer)
  "Llegeix el mapa des d'un fitxer de text i retorna la llista."
  (let* ((canal (open nom-fitxer :direction :input))
         (mapa (read canal)))  ; Llegeix la llista directament
    (close canal)              ; És molt important tancar el fitxer!
    mapa)
)


;; ======================================================================
;; CONTROLADOR GENERAL
;; ======================================================================

(defun inicia-partida (mapa-inicial)
  "Prepara l'estat inicial i llança el bucle principal de la partida."
  ;; L'estat inicial és:
  ;; - Ronda: 1
  ;; - Mapa: el que hem carregat del fitxer
  ;; - Pintura E1: 200
  ;; - Pintura E2: 200
  ;; - Memòria E1: nil (buida)
  ;; - Memòria E2: nil (buida)
  (bucle-partida 1 mapa-inicial 200 200 nil nil 'manual 1))


(defun bucle-partida (ronda mapa pint-e1 pint-e2 mem-e1 mem-e2 mode velocitat)
  "El motor principal del joc. S'executa recursivament a cada torn."
  
  (dibuixa-mapa mapa)
  (BLACK)
  (format t "~%--- RONDA ~A ---~%" ronda)
  (format t "Pintura E1: ~A | Pintura E2: ~A~%" pint-e1 pint-e2)
  
  (cond ((> ronda 1500)
         (RED)
         (format t "Final de la partida: Límit de 1500 torns assolit!~%")
         'fi-de-partida)
        
        (t 
         (let* ((equip-actiu (cond ((= (mod ronda 2) 1) 'e1)
                                   (t 'e2)))
                
                ;; 1. Sumem la pintura passiva
                (pint-e1-inici (if (eq equip-actiu 'e1) (+ pint-e1 2 (compta-labs-mapa mapa 'e1)) pint-e1))
                (pint-e2-inici (if (eq equip-actiu 'e2) (+ pint-e2 2 (compta-labs-mapa mapa 'e2)) pint-e2))
                
                (pintura-actual-equip (if (eq equip-actiu 'e1) pint-e1-inici pint-e2-inici))
                (memoria-actual-equip (if (eq equip-actiu 'e1) mem-e1 mem-e2))
                
                ;; 1.5. APLIQUEM EL DESCANS!
                (mapa-descansat (redueix-temps-mapa mapa equip-actiu))
                
                ;; 2. Busquem unitats
                (unitats-actuants (busca-unitats-mapa mapa-descansat equip-actiu 0))
                
                ;; 3. Processem accions
                (estat-resultant (processa-totes-les-unitats unitats-actuants mapa-descansat ronda pintura-actual-equip equip-actiu memoria-actual-equip))
                
                (nou-mapa (car estat-resultant))
                (nova-pintura-equip (cadr estat-resultant))
                (nova-memoria-equip (caddr estat-resultant))
                
                (nova-pint-e1 (if (eq equip-actiu 'e1) nova-pintura-equip pint-e1-inici))
                (nova-pint-e2 (if (eq equip-actiu 'e2) nova-pintura-equip pint-e2-inici))
                (nova-mem-e1 (if (eq equip-actiu 'e1) nova-memoria-equip mem-e1))
                (nova-mem-e2 (if (eq equip-actiu 'e2) nova-memoria-equip mem-e2))
                
                ;; ==========================================================
                ;; 4. CONTROL DE MODE I VELOCITAT
                ;; ==========================================================
                (controls (processa-controls mode velocitat))
                (nou-mode (car controls))
                (nova-velocitat (cadr controls)))
           
           ;; Si el mode és Automàtic, imprimim instruccions i fem la pausa de temps
           (when (eq nou-mode 'auto)
             (BLACK)
             (format t ">> AUTO (Velocitat x~A). Escriu 'm' + ENTER per aturar.~%" nova-velocitat)
             (sleep 1))
           
           ;; Crida recursiva amb el mode i velocitat actualitzats
           (bucle-partida (+ ronda 1) 
                          nou-mapa 
                          nova-pint-e1 
                          nova-pint-e2 
                          nova-mem-e1 
                          nova-mem-e2
                          nou-mode
                          nova-velocitat)))))
;; ======================================================================
;; CONTROLS DE LA PARTIDA (Manual / Automàtic i Velocitat)
;; ======================================================================

(defun processa-controls (mode velocitat)
  "Llegeix l'entrada de l'usuari per canviar de mode i velocitat."
  (let ((entrada (cond
                   ;; Si estem en manual, ens quedem bloquejats esperant
                   ((eq mode 'manual)
                    (BLACK)
                    (format t ">> MANUAL. Prem ENTER per continuar, o escriu 'a' + ENTER per mode Automatic: ")
                    (read-line))
                   
                   ;; Si estem en auto i no hi ha res escrit, retornem nil
                   (t nil))))
    
    ;; Avaluem què ha escrit l'usuari i retornem una llista (nou-mode nova-velocitat)
    (cond ((equal entrada "m") (list 'manual velocitat))
          ((equal entrada "a") (list 'auto velocitat))
          ;; Si no ha escrit res o ha posat una altra cosa, mantenim l'estat actual
          (t (list mode velocitat)))))
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

(defun formateja-casella (coord casella)
  "Adapta la informació d'una casella del mapa al format que demana l'enunciat per a la visió."
  (let ((tipus-casella (car casella)))
    (cond ((eq tipus-casella 'aigua)
           ;; L'aigua només necessita coordenada i tipus
           (list coord 'aigua))
          (t
           ;; La terra necessita tota la informació de l'element que hi ha a sobre
           (let ((color-casella (cadr casella))
                 (element (caddr casella))
                 (equip (cadddr casella)))
             ;; Retornem: (coord tipus color element equip colors-pintat color-propi tr-pintar tr-moure)
             ;; Nota: Deixem els últims 4 valors a NIL temporalment fins que implementem els cooldowns i danys.
             (list coord tipus-casella color-casella element equip nil nil nil nil))))))

(defun visio-fila (fila origen-x origen-y rang x y)
  "Recorre una fila i retorna només les caselles que estan dins del rang de visió."
  (cond ((null fila) nil)
        ;; Si la distància al quadrat és menor o igual al rang, la casella és visible
        ((<= (distancia-quadrada origen-x origen-y x y) rang)
         (cons (formateja-casella (list x y) (car fila))
               (visio-fila (cdr fila) origen-x origen-y rang (+ x 1) y)))
        ;; Si no és visible, la ignorem i seguim amb la següent
        (t (visio-fila (cdr fila) origen-x origen-y rang (+ x 1) y))))

(defun visio-mapa (mapa origen-x origen-y rang y)
  "Recorre tot el mapa i ajunta les caselles visibles en una única llista."
  (cond ((null mapa) nil)
        (t (append (visio-fila (car mapa) origen-x origen-y rang 0 y)
                   (visio-mapa (cdr mapa) origen-x origen-y rang (+ y 1))))))


;; ======================================================================
;; EMPAQUETATGE I COMUNICACIÓ AMB ELS AGENTS
;; ======================================================================

(defun empaqueta-dades-unitat (mapa ronda equip pintura memoria x y)
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
                           
         ;; Calculem què veu aquesta unitat des de la seva posició
         (visio (visio-mapa mapa x y rang-visio 0)))
    
    ;; Retornem la llista estructurada exactament com demana l'enunciat
    (list ronda 
          equip 
          pintura 
          tipus-unitat 
          (list x y) 
          colors-pintat 
          color-propi 
          tr-pintar 
          tr-moure 
          visio 
          memoria)))

(defun demana-accions-agent (dades-empaquetades)
  "Crida a la funció de l'agent corresponent (hardcoded com demana l'enunciat) i en retorna les accions."
  (let ((equip (cadr dades-empaquetades))) ; El segon element és l'equip
    (cond ((eq equip 'e1) (agent-abc123 dades-empaquetades))
          ((eq equip 'e2) (agent-xyz999 dades-empaquetades))
          (t nil))))

;; ======================================================================
;; PROCESSADOR D'ACCIONS
;; ======================================================================

(defun aplica-accions (accions mapa pintura equip coord-origen)
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
                     (dest-x (car coord-desti))
                     (dest-y (cadr coord-desti)))
                (cond ((>= pintura 50)
                       (let* ((casella-vella (indexa-matriu mapa dest-y dest-x))
                              (color-terra (cadr casella-vella))
                              (nova-casella (list 'terra color-terra 'bolla equip nil color-bolla 0 0))
                              (nou-mapa (posa-dins-matriu mapa dest-y dest-x nova-casella))
                              (nova-pintura (- pintura 50)))
                         (aplica-accions (cdr accions) nou-mapa nova-pintura equip coord-origen)))
                      (t (aplica-accions (cdr accions) mapa pintura equip coord-origen)))))
             
             ;; ---------------------------------------------------------
             ;; ACCIÓ: MOU
             ;; ---------------------------------------------------------
             ((eq tipus-accio 'mou)
              (let* ((coord-desti args)
                     (dest-x (car coord-desti))
                     (dest-y (cadr coord-desti))
                     (orig-x (car coord-origen))
                     (orig-y (cadr coord-origen))
                     (casella-origen (indexa-matriu mapa orig-y orig-x))
                     (casella-desti (indexa-matriu mapa dest-y dest-x)))
                
                (cond ((and (eq (car casella-desti) 'terra)
                            (null (caddr casella-desti)))
                       (let* ((color-terra-orig (cadr casella-origen))
                              (color-terra-dest (cadr casella-desti))
                              (equip-bolla (cadddr casella-origen))
                              (colors-pintat (nth 4 casella-origen))
                              (color-propi (nth 5 casella-origen))
                              (tr-pintar (nth 6 casella-origen))
                              
                              (es-diagonal (= (+ (* (- dest-x orig-x) (- dest-x orig-x))
                                                 (* (- dest-y orig-y) (- dest-y orig-y))) 2))
                              (tr-base (if es-diagonal 1.4142 1))
                              (nou-tr-moure (if (eq color-terra-dest color-propi) tr-base (* tr-base 3)))
                              
                              (origen-buit (list 'terra color-terra-orig nil nil nil nil nil nil))
                              (mapa-mig (posa-dins-matriu mapa orig-y orig-x origen-buit))
                              
                              (desti-ocupat (list 'terra color-terra-dest 'bolla equip-bolla colors-pintat color-propi tr-pintar nou-tr-moure))
                              (nou-mapa (posa-dins-matriu mapa-mig dest-y dest-x desti-ocupat)))
                         
                         (aplica-accions (cdr accions) nou-mapa pintura equip coord-desti)))
                      (t (aplica-accions (cdr accions) mapa pintura equip coord-origen)))))
             
             ;; ---------------------------------------------------------
             ;; ACCIÓ: PINTA
             ;; ---------------------------------------------------------
             ((eq tipus-accio 'pinta)
              (let* ((coord-desti args) ; Coordenada on disparem
                     (dest-x (car coord-desti))
                     (dest-y (cadr coord-desti))
                     (orig-x (car coord-origen))
                     (orig-y (cadr coord-origen))
                     (casella-origen (indexa-matriu mapa orig-y orig-x))
                     (casella-desti (indexa-matriu mapa dest-y dest-x)))
                
                ;; Només podem pintar si el destí és terra
                (cond ((eq (car casella-desti) 'terra)
                       (let* ((color-terra-orig (cadr casella-origen))
                              (equip-tirador (cadddr casella-origen))
                              (color-tirador (nth 5 casella-origen))
                              
                              ;; 1. Calculem el cooldown (1 normal, 3 si trepitjam color enemic)
                              (nou-tr-pintar (if (eq color-terra-orig color-tirador) 1 3))
                              
                              ;; Actualitzem l'origen amb el cooldown gastat
                              (origen-actualitzat (list 'terra color-terra-orig 'bolla equip-tirador
                                                        (nth 4 casella-origen) color-tirador 
                                                        nou-tr-pintar (nth 7 casella-origen)))
                              (mapa-mig (posa-dins-matriu mapa orig-y orig-x origen-actualitzat))
                              
                              ;; 2. Analitzem què hi ha al destí
                              (element-desti (caddr casella-desti))
                              (equip-desti (cadddr casella-desti))
                              (colors-desti (nth 4 casella-desti))
                              (color-propi-desti (nth 5 casella-desti))
                              (tr-p-desti (nth 6 casella-desti))
                              (tr-m-desti (nth 7 casella-desti))
                              
                              ;; Si hi ha base/bolla enemiga, afegim el color del tret (evitant duplicats)
                              (nous-colors-desti 
                               (if (and element-desti (not (eq element-desti 'lab)))
                                   (if (member color-tirador colors-desti)
                                       colors-desti
                                       (cons color-tirador colors-desti))
                                   colors-desti))
                              
                              ;; Comprovem si ha acumulat els 3 colors per explotar
                              (explota (and nous-colors-desti
                                            (member 'r nous-colors-desti)
                                            (member 'g nous-colors-desti)
                                            (member 'b nous-colors-desti)))
                              
                              ;; 3. Construïm la nova casella de destí
                              (desti-actualitzat 
                               (cond 
                                 (explota 
                                  ;; BOOM! L'element explota, però el terra queda del nostre color
                                  (list 'terra color-tirador nil nil nil nil nil nil))
                                 
                                 ((eq element-desti 'lab)
                                  ;; LAB CAPTURAT! Ara és del nostre equip i canviem el terra
                                  (list 'terra color-tirador 'lab equip-tirador nil nil nil nil))
                                 
                                 (t
                                  ;; NO EXPLOTA o TERRA BUIDA. Terra pintada, la resta igual
                                  (list 'terra color-tirador element-desti equip-desti nous-colors-desti color-propi-desti tr-p-desti tr-m-desti))))
                              
                              ;; Inserim el destí actualitzat
                              (nou-mapa (posa-dins-matriu mapa-mig dest-y dest-x desti-actualitzat)))
                         
                         (aplica-accions (cdr accions) nou-mapa pintura equip coord-origen)))
                      (t 
                       ;; Moviment invàlid (aigua), s'ignora
                       (aplica-accions (cdr accions) mapa pintura equip coord-origen)))))
             
             ;; ---------------------------------------------------------
             ;; IGNORAR ALTRES ACCIONS
             ;; ---------------------------------------------------------
             (t (aplica-accions (cdr accions) mapa pintura equip coord-origen)))))))

(defun processa-totes-les-unitats (unitats mapa ronda pintura equip memoria)
  "Demana accions a cada unitat i les aplica seqüencialment. Retorna (nou-mapa nova-pintura nova-memoria)."
  (cond ((null unitats) (list mapa pintura memoria))
        (t
         (let* ((coord (car unitats))
                (x (car coord))
                (y (cadr coord))
                ;; 1. Empaquetem el que veu aquesta unitat en concret
                (dades (empaqueta-dades-unitat mapa ronda equip pintura memoria x y))
                ;; 2. Cridem l'agent intel·ligent
                (accions (demana-accions-agent dades))
                ;; 3. Apliquem les accions al mapa
                (resultat-accions (aplica-accions accions mapa pintura equip coord))
                (mapa-post-accions (car resultat-accions))
                (pintura-post-accions (cadr resultat-accions)))
           
           ;; 4. Crida recursiva per a la següent unitat, passant-li l'estat ja actualitzat
           (processa-totes-les-unitats (cdr unitats) 
                                       mapa-post-accions 
                                       ronda 
                                       pintura-post-accions 
                                       equip 
                                       memoria)))))


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
                        (decrementa-temps tr-moure)))
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
