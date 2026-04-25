;; ======================================================================
;; PRÀCTICA FINAL LLENGUATGES DE PROGRAMACIÓ - LISP - PAINTBALL
;; ======================================================================
;; Estudiant: XYZ (Versió Avançada - Torneig)
;;
;; DESCRIPCIÓ: Agent intel·ligent avançat 2.0. 
;; Paradigma estrictament funcional (Sense if, setq, when, loop).
;; Incorpora memòria global (A-lists), Swarm Intelligence per l'exploració,
;; i avaluació heurística del terreny per moure's i atacar de manera letal.
;; ======================================================================

;; ----------------------------------------------------------------------
;; FUNCIONS MATEMÀTIQUES I BÀSIQUES
;; ----------------------------------------------------------------------

(defun agent-xyz999-dist-q (c1 c2)
  "Calcula la distància euclidiana al quadrat entre dues coordenades."
  (+ (* (- (car c1) (car c2)) (- (car c1) (car c2)))
     (* (- (cadr c1) (cadr c2)) (- (cadr c1) (cadr c2)))))

;; ----------------------------------------------------------------------
;; GESTIÓ DE LA MEMÒRIA (Associative Lists Pures)
;; ----------------------------------------------------------------------

(defun agent-xyz999-get-mem (clau memoria)
  "Recupera un valor de la memòria usant la clau."
  (cond ((null memoria) nil)
        ((eq (caar memoria) clau) (cdar memoria))
        (t (agent-xyz999-get-mem clau (cdr memoria)))))

(defun agent-xyz999-set-mem (clau valor memoria)
  "Actualitza o insereix un valor a la memòria, retornant la nova memòria."
  (cond ((null memoria) (list (cons clau valor)))
        ((eq (caar memoria) clau) (cons (cons clau valor) (cdr memoria)))
        (t (cons (car memoria) (agent-xyz999-set-mem clau valor (cdr memoria))))))

(defun agent-xyz999-esborra-elem (elem llista)
  "Esborra una instància d'un element d'una llista (per treure labs conquerits)."
  (cond ((null llista) nil)
        ((equal elem (car llista)) (agent-xyz999-esborra-elem elem (cdr llista)))
        (t (cons (car llista) (agent-xyz999-esborra-elem elem (cdr llista))))))

(defun agent-xyz999-afegir-lab (coord mem)
  "Afegeix un laboratori a la llista de labs objectiu si no hi és."
  (let ((labs (agent-xyz999-get-mem 'labs mem)))
    (cond ((member coord labs :test #'equal) mem)
          (t (agent-xyz999-set-mem 'labs (cons coord labs) mem)))))

(defun agent-xyz999-treure-lab (coord mem)
  "Treballa conjuntament amb la visió per ignorar els laboratoris que ja són nostres."
  (let ((labs (agent-xyz999-get-mem 'labs mem)))
    (cond ((member coord labs :test #'equal)
           (agent-xyz999-set-mem 'labs (agent-xyz999-esborra-elem coord labs) mem))
          (t mem))))

;; ----------------------------------------------------------------------
;; PROCESSAMENT DE LA VISIÓ
;; ----------------------------------------------------------------------

(defun agent-xyz999-actualitza-memoria (visio mem-actual el-meu-equip)
  "Recorre la visió per actualitzar el coneixement global (Base i Labs)."
  (cond ((null visio) mem-actual)
        (t (let* ((casella (car visio))
                  (coord (car casella))
                  (element (cadddr casella))
                  (equip-element (nth 4 casella))
                  
                  ;; Detectar Base enemiga
                  (mem-1 (cond ((and (eq element 'base) (not (eq equip-element el-meu-equip)))
                                (agent-xyz999-set-mem 'base coord mem-actual))
                               (t mem-actual)))
                  
                  ;; Detectar Labs (i treure els que ja hàgim capturat)
                  (mem-2 (cond ((and (eq element 'lab) (not (eq equip-element el-meu-equip)))
                                (agent-xyz999-afegir-lab coord mem-1))
                               ((and (eq element 'lab) (eq equip-element el-meu-equip))
                                (agent-xyz999-treure-lab coord mem-1))
                               (t mem-1))))
             
             (agent-xyz999-actualitza-memoria (cdr visio) mem-2 el-meu-equip)))))

;; ----------------------------------------------------------------------
;; SISTEMA TÀCTIC DE COMBAT (PUNTERIA)
;; ----------------------------------------------------------------------

(defun agent-xyz999-avalua-tret (casella el-meu-equip el-meu-color coord-actual)
  "Retorna una puntuació de prioritat per disparar a aquesta casella."
  (let* ((coord-obj (car casella))
         (dist (agent-xyz999-dist-q coord-actual coord-obj))
         (element (cadddr casella))
         (equip-obj (nth 4 casella))
         (colors-pintats (nth 5 casella)))
    (cond ((> dist 25) -1) ; Massa lluny (5^2 = 25)
          ((null element) -1) ; Res a disparar
          ((eq equip-obj el-meu-equip) -1) ; Mai disparar a aliats
          
          ;; Base enemiga: Prioritat absoluta, excepte si ja té el nostre color!
          ((eq element 'base)
           (cond ((member el-meu-color colors-pintats) -1)
                 (t 1000))) 
          
          ;; Laboratoris neutrals o enemics
          ((eq element 'lab) 500)
          
          ;; Bolles enemigues: Ara amb més prioritat si estan a prop
          ((eq element 'bolla) 800)
          (t -1))))

(defun agent-xyz999-millor-tret (visio equip color coord-actual millor-coord millor-punt)
  "Cerca el millor objectiu per disparar recursivament."
  (cond ((null visio) millor-coord)
        (t (let* ((casella (car visio))
                  (punt (agent-xyz999-avalua-tret casella equip color coord-actual)))
             (cond ((> punt millor-punt)
                    (agent-xyz999-millor-tret (cdr visio) equip color coord-actual (car casella) punt))
                   (t
                    (agent-xyz999-millor-tret (cdr visio) equip color coord-actual millor-coord millor-punt)))))))

;; ----------------------------------------------------------------------
;; SISTEMA DE NAVEGACIÓ (GREEDY HEURÍSTIC I EXPLORACIÓ)
;; ----------------------------------------------------------------------

(defun agent-xyz999-es-movible (casella coord-actual)
  "Comprova si la casella està buida, és de terra i és adient per moure's (distància <= 2)."
  (let ((coord (car casella))
        (tipus (cadr casella))
        (element (cadddr casella)))
    (and (eq tipus 'terra)
         (null element)
         (<= (agent-xyz999-dist-q coord coord-actual) 2)
         (not (equal coord coord-actual)))))

(defun agent-xyz999-filtra-movibles (visio coord-actual)
  "Retorna una llista amb totes les caselles admeses per caminar."
  (cond ((null visio) nil)
        ((agent-xyz999-es-movible (car visio) coord-actual)
         (cons (car visio) (agent-xyz999-filtra-movibles (cdr visio) coord-actual)))
        (t (agent-xyz999-filtra-movibles (cdr visio) coord-actual))))

(defun agent-xyz999-punt-exploracio (coord id)
  "Força les bolles a dispersar-se als 4 punts cardinals segons el seu ID."
  (let* ((id-segur (cond (id id) (t (random 1000)))) ; Prevenció per si l'entorn falla
         (quadrant (rem id-segur 4))
         (cx (car coord))
         (cy (cadr coord)))
    (cond ((= quadrant 0) (list (+ cx 1000) (+ cy 1000)))
          ((= quadrant 1) (list (- cx 1000) (+ cy 1000)))
          ((= quadrant 2) (list (+ cx 1000) (- cy 1000)))
          (t              (list (- cx 1000) (- cy 1000))))))

(defun agent-xyz999-busca-proxim (visio equip)
  "Busca el primer objectiu d'interès (Base o Lab no propi) en el camp visual."
  (cond ((null visio) nil)
        (t (let* ((casella (car visio))
                  (coord (car casella))
                  (element (cadddr casella))
                  (equip-el (nth 4 casella)))
             (cond ((and element (not (eq equip-el equip)) 
                         (or (eq element 'lab) (eq element 'base)))
                    coord)
                   (t (agent-xyz999-busca-proxim (cdr visio) equip)))))))

(defun agent-xyz999-tria-desti (coord-actual mem id-unitat visio equip)
  "Elegeix cap a on ha de marxar la unitat (Visió -> Base -> Lab -> Explorar)."
  (let ((proper (agent-xyz999-busca-proxim visio equip))
        (base-enemic (agent-xyz999-get-mem 'base mem))
        (base-aliada (agent-xyz999-get-mem 'base-aliada mem))
        (labs (agent-xyz999-get-mem 'labs mem))
        (es-defensor (and id-unitat (= (rem id-unitat 5) 0))))
    (cond (proper proper)                           ; 1. Si veig algo ara mateix, hi vaig.
          ((and es-defensor base-aliada) base-aliada) ; 2. Si soc defensor, guardo la base.
          (base-enemic base-enemic)                  ; 3. Prioritat atacar base coneguda.
          (labs                                      ; 4. Repartir labs coneguts.
           (nth (rem id-unitat (length labs)) labs))
          (t (agent-xyz999-punt-exploracio coord-actual id-unitat)))))

(defun agent-xyz999-cost-pas (casella-desti desti-final el-meu-color)
  "Heurística de cost: Combina la distància al destí final amb el perill de caminar on no toca."
  (let* ((coord (car casella-desti))
         (color-casella (caddr casella-desti)) ; color de la pintura a terra
         (dist-al-desti (agent-xyz999-dist-q coord desti-final))
         ;; Donem preferència a caminar pel propi color per mantenir temps_recuperacio baix
         (pena-color (cond ((eq color-casella el-meu-color) 0) (t 300)))) 
    (+ (* dist-al-desti 100) pena-color)))

(defun agent-xyz999-millor-pas (movibles desti-final el-meu-color millor-coord millor-cost)
  "Troba la casella amb menor cost per apropar-nos al destí heurísticament."
  (cond ((null movibles) millor-coord)
        (t (let* ((casella (car movibles))
                  (cost (agent-xyz999-cost-pas casella desti-final el-meu-color)))
             (cond ((< cost millor-cost)
                    (agent-xyz999-millor-pas (cdr movibles) desti-final el-meu-color (car casella) cost))
                   (t
                    (agent-xyz999-millor-pas (cdr movibles) desti-final el-meu-color millor-coord millor-cost)))))))

;; ----------------------------------------------------------------------
;; CERVELL BASE I BOLLA
;; ----------------------------------------------------------------------

(defun agent-xyz999-decisio-base (pintura coord visio mem equip)
  "Crea bolles intel·ligentment posant-les al millor flanc envers els enemics."
  (cond ((>= pintura 50)
         (let ((movibles (agent-xyz999-filtra-movibles visio coord)))
           (cond ((null movibles) nil)
                 (t
                  (let* ((desti (agent-xyz999-tria-desti coord mem 0 visio equip))
                         (millor-casella (agent-xyz999-millor-pas movibles desti 'cap nil 1000000000))
                         ;; Correcció del bug: millor-casella JA ÉS la coordenada
                         (coord-spawn (cond (millor-casella millor-casella) (t (car (car movibles)))))
                         (color-nou (nth (random 3) '(r g b))))
                    (list (list 'crea-bolla (list color-nou coord-spawn))))))))
        (t nil)))

(defun agent-xyz999-intentar-moure (coord temps-moure visio mem color-propi id-unitat equip)
  "Aplica l'heurística A* Greedy de moviment."
  (cond ((< temps-moure 1)
         (let ((movibles (agent-xyz999-filtra-movibles visio coord)))
           (cond ((null movibles) nil)
                 (t (let* ((desti (agent-xyz999-tria-desti coord mem id-unitat visio equip))
                           (millor (agent-xyz999-millor-pas movibles desti color-propi nil 1000000000)))
                      ;; Correcció del bug: No fer car a millor
                      (cond (millor (list (list 'mou (list millor))))
                            (t nil)))))))
        (t nil)))

(defun agent-xyz999-decisio-bolla (coord equip color-propi tr-pintar tr-moure visio mem id-unitat)
  "Decideix si disparar a l'objectiu més crític, o avançar."
  (let ((temps-pintar (cond (tr-pintar tr-pintar) (t 0)))
        (temps-moure (cond (tr-moure tr-moure) (t 0))))
    (cond
      ;; Prioritat Absoluta: Disparar (si cooldown ho permet)
      ((< temps-pintar 1)
       (let ((tret (agent-xyz999-millor-tret visio equip color-propi coord nil -1)))
         (cond (tret (list (list 'pinta (list tret))))
               ;; Si no hi ha res per disparar, ens movem
               (t (agent-xyz999-intentar-moure coord temps-moure visio mem color-propi id-unitat equip)))))
      
      ;; Si no podem disparar, intentem moure'ns
      (t (agent-xyz999-intentar-moure coord temps-moure visio mem color-propi id-unitat equip)))))

;; ----------------------------------------------------------------------
;; PUNT D'ENTRADA PRINCIPAL (INTERFÍCIE)
;; ----------------------------------------------------------------------

(defun agent-xyz999 (dades)
  "Llegeix la llista d'estat de Paintball i orquestra la decisió."
  (let* ((ronda (nth 0 dades))
         (equip (nth 1 dades))
         (pintura (nth 2 dades))
         (id-unitat (nth 3 dades))
         (tipus-unitat (nth 4 dades))
         (coord (nth 5 dades))
         (colors-pintat (nth 6 dades))
         (color-propi (nth 7 dades))
         (tr-pintar (nth 8 dades))
         (tr-moure (nth 9 dades))
         (visio (nth 10 dades))
         (mem-antiga (nth 11 dades))
         
         ;; Actualitzar la intel·ligència de xarxa amb el que veu aquesta unitat
         (mem-1 (agent-xyz999-actualitza-memoria visio mem-antiga equip))
         
         ;; Si soc la base, registro la meva posició a la llibreta per als defensors
         (mem-nova (cond ((eq tipus-unitat 'base) (agent-xyz999-set-mem 'base-aliada coord mem-1))
                         (t mem-1)))
         
         ;; Prendre l'acció
         (accio (cond ((eq tipus-unitat 'base)
                       (agent-xyz999-decisio-base pintura coord visio mem-nova equip))
                      ((eq tipus-unitat 'bolla)
                       (agent-xyz999-decisio-bolla coord equip color-propi tr-pintar tr-moure visio mem-nova id-unitat))
                      (t nil))))
    
    ;; Retornar SEMPRE la memòria compartida primer, seguida de les accions
    (append (list (list 'escriu-memoria (list mem-nova)))
            accio)))