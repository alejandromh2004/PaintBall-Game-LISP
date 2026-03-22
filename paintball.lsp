;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del controlador principal.
;; <Descripció de les funcions d'aquest fitxer>

;; Necessari per a l'optimització de crides recursives.
; (load 'common) ; https://almy.us/files/xl305req.zip
; (load 'tco)    ; https://github.com/antoni-oliver/defun-tco

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
  (let ((mapa-inicial (carrega-mapa "proyectos/projecte_inicial/maps/two.map")))
    
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
  (bucle-partida 1 mapa-inicial 200 200 nil nil))


(defun bucle-partida (ronda mapa pint-e1 pint-e2 mem-e1 mem-e2)
  "El motor principal del joc. S'executa recursivament a cada torn."
  
  ;; 1. Dibuixem l'estat actual al principi del torn
  (dibuixa-mapa mapa)
  ;; Mostrem informació per consola temporalment per fer debug
  (format t "~%--- RONDA ~A ---~%" ronda)
  (format t "Pintura E1: ~A | Pintura E2: ~A~%" pint-e1 pint-e2)
  
  ;; 2. Comprovem condicions de finalització (Límit de torns)
  (cond ((> ronda 1500) ; [cite: 25]
         (format t "Final de la partida: Límit de 1500 torns assolit!~%")
         ;; Aquí més endavant cridarem a la funció de desempat
         'fi-de-partida)
        
        ;; Aquí afegirem més endavant la comprovació de si una base ha explotat
        
        ;; 3. Si la partida continua, processem el torn
        (t 
         (let* (;; Determinem de quin equip és el torn actual
                ;; Un equip juga els torns senars i l'altre els torns parells[cite: 117].
                (equip-actiu (cond ((= (mod ronda 2) 1) 'e1)
                                   (t 'e2)))
                
                ;; ==========================================================
                ;; LA MÀGIA DEL TORN
                ;; ==========================================================
                ;; 1. Sumem la pintura passiva del torn (2 unitats base) [cite: 31]
                (pint-e1-inici (if (eq equip-actiu 'e1) (+ pint-e1 2) pint-e1))
                (pint-e2-inici (if (eq equip-actiu 'e2) (+ pint-e2 2) pint-e2))
                
                (pintura-actual-equip (if (eq equip-actiu 'e1) pint-e1-inici pint-e2-inici))
                (memoria-actual-equip (if (eq equip-actiu 'e1) mem-e1 mem-e2))
                
                ;; 2. Busquem on són les nostres unitats al començament del torn
                (unitats-actuants (busca-unitats-mapa mapa equip-actiu 0))
                
                ;; 3. Processem totes les unitats i n'obtenim l'estat resultant
                (estat-resultant (processa-totes-les-unitats unitats-actuants 
                                                             mapa 
                                                             ronda 
                                                             pintura-actual-equip 
                                                             equip-actiu 
                                                             memoria-actual-equip))
                
                ;; 4. Extraiem el resultat per passar-lo a la següent ronda
                (nou-mapa (car estat-resultant))
                (nova-pintura-equip (cadr estat-resultant))
                (nova-memoria-equip (caddr estat-resultant))
                
                ;; Reassignem la pintura i la memòria a l'equip correcte per la següent crida
                (nova-pint-e1 (if (eq equip-actiu 'e1) nova-pintura-equip pint-e1-inici))
                (nova-pint-e2 (if (eq equip-actiu 'e2) nova-pintura-equip pint-e2-inici))
                (nova-mem-e1 (if (eq equip-actiu 'e1) nova-memoria-equip mem-e1))
                (nova-mem-e2 (if (eq equip-actiu 'e2) nova-memoria-equip mem-e2)))
           
           ;; Fem una petita pausa per poder veure el joc torn a torn (Prem Enter)
           (format t "Torn de l'equip ~A. Prem ENTER per continuar..." equip-actiu)
           (read-line)
           
           ;; 4. Crida recursiva per al següent torn amb l'estat actualitzat
           (bucle-partida (+ ronda 1) 
                          nou-mapa 
                          nova-pint-e1 
                          nova-pint-e2 
                          nova-mem-e1 
                          nova-mem-e2)))))
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
             ;; ACCIÓ: CREA-BOLLA
             ((eq tipus-accio 'crea-bolla)
              (let* ((color-bolla (car args))
                     (coord-desti (cadr args))
                     (dest-x (car coord-desti))
                     (dest-y (cadr coord-desti)))
                (cond ((>= pintura 50) ; Comprovem que podem pagar-ho
                       (let* ((casella-vella (indexa-matriu mapa dest-y dest-x))
                              (color-terra (cadr casella-vella))
                              ;; Construïm la nova casella amb la bolla a sobre
                              ;; (terra color-terra bolla equip colors-pintat color-propi tr-pintar tr-moure)
                              (nova-casella (list 'terra color-terra 'bolla equip nil color-bolla 0 0))
                              ;; Inserim la casella al mapa sense mutar l'original!
                              (nou-mapa (posa-dins-matriu mapa dest-y dest-x nova-casella))
                              (nova-pintura (- pintura 50)))
                         ;; Passem a la següent acció amb el mapa i la pintura actualitzats
                         (aplica-accions (cdr accions) nou-mapa nova-pintura equip coord-origen)))
                      (t ; Si no hi ha pintura, ignorem l'acció
                       (aplica-accions (cdr accions) mapa pintura equip coord-origen)))))
             
             ;; ACCIÓ: MOU (Estructura base per afegir-ho després)
             ((eq tipus-accio 'mou)
              ;; Aquí anirà la teva lògica per esborrar l'origen i escriure al destí
              (aplica-accions (cdr accions) mapa pintura equip coord-origen))
             
             ;; IGNORAR ALTRES ACCIONS (pinta, escriu-memoria) de moment
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
