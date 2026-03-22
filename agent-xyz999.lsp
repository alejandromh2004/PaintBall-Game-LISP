;; ======================================================================
;; AGENT INTEL·LIGENT XYZ999
;; ======================================================================

;; ----------------------------------------------------------------------
;; FUNCIONS AUXILIARS (Totes amb el prefix agent-xyz999-)
;; ----------------------------------------------------------------------

(defun agent-xyz999-distancia-q (c1 c2)
  "Calcula la distància euclidiana al quadrat entre dues coordenades."
  (+ (* (- (car c1) (car c2)) (- (car c1) (car c2)))
     (* (- (cadr c1) (cadr c2)) (- (cadr c1) (cadr c2)))))

(defun agent-xyz999-es-terra-buida (casella)
  "Comprova si una casella de la visió és terra i no té cap element a sobre."
  ;; Format visió: (coord tipus-casella color-casella tipus-element ...)
  (and (eq (cadr casella) 'terra)
       (null (cadddr casella))))

(defun agent-xyz999-es-enemic-o-lab (casella el-meu-equip)
  "Comprova si hi ha un element disparable (enemic o laboratori no propi)."
  (let ((element (cadddr casella))
        (equip-casella (nth 4 casella)))
    (and element
         (not (eq equip-casella el-meu-equip)))))

(defun agent-xyz999-busca-caselles-buides-adj (coord visio)
  "Retorna una llista de coordenades buides a distància <= 2."
  (cond ((null visio) nil)
        ((and (agent-xyz999-es-terra-buida (car visio))
              (<= (agent-xyz999-distancia-q coord (car (car visio))) 2)
              (not (equal coord (car (car visio))))) ; Que no sigui on som ara
         (cons (car (car visio)) (agent-xyz999-busca-caselles-buides-adj coord (cdr visio))))
        (t (agent-xyz999-busca-caselles-buides-adj coord (cdr visio)))))

(defun agent-xyz999-busca-objectius (coord el-meu-equip visio)
  "Retorna coordenades d'objectius a distància de tret (<= 5)."
  (cond ((null visio) nil)
        ((and (agent-xyz999-es-enemic-o-lab (car visio) el-meu-equip)
              (<= (agent-xyz999-distancia-q coord (car (car visio))) 5))
         (cons (car (car visio)) (agent-xyz999-busca-objectius coord el-meu-equip (cdr visio))))
        (t (agent-xyz999-busca-objectius coord el-meu-equip (cdr visio)))))

;; ----------------------------------------------------------------------
;; CERVELL DE LA BASE
;; ----------------------------------------------------------------------

(defun agent-xyz999-decisio-base (pintura coord visio)
  "Lògica per a les bases: Crear bolles si hi ha pintura suficient."
  (cond ((>= pintura 50) ; Costa 50 de pintura
         (let ((buides (agent-xyz999-busca-caselles-buides-adj coord visio)))
           (cond ((null buides) nil) ; No hi ha espai
                 (t 
                  ;; Cream una bolla vermella ('r) a la primera casella lliure
                  (list (list 'crea-bolla (list 'r (car buides))))))))
        (t nil)))

;; ----------------------------------------------------------------------
;; CERVELL DE LA BOLLA
;; ----------------------------------------------------------------------

(defun agent-xyz999-decisio-bolla (coord equip tr-pintar tr-moure visio)
  "Lògica per a les bolles: Disparar si pot, o moure's."
  ;; Els cooldowns poden venir com a nil al principi, els tractem com a 0
  (let ((temps-pintar (if tr-pintar tr-pintar 0))
        (temps-moure (if tr-moure tr-moure 0)))
    
    (cond 
      ;; 1. Prioritat: Disparar a un objectiu si el cooldown és < 1
      ((< temps-pintar 1)
       (let ((objectius (agent-xyz999-busca-objectius coord equip visio)))
         (cond ((not (null objectius))
                ;; Disparem al primer objectiu que veiem
                (list (list 'pinta (car objectius))))
               (t 
                ;; Si no hi ha objectius, intentem moure'ns
                (agent-xyz999-intentar-moure coord temps-moure visio)))))
      
      ;; 2. Si no pot disparar, intenta moure's
      (t (agent-xyz999-intentar-moure coord temps-moure visio)))))

(defun agent-xyz999-intentar-moure (coord temps-moure visio)
  "Sub-lògica per moure una bolla."
  (cond ((< temps-moure 1)
         (let ((buides (agent-xyz999-busca-caselles-buides-adj coord visio)))
           (cond ((not (null buides))
                  ;; Ens movem a la primera casella buida disponible
                  (list (list 'mou (car buides))))
                 (t nil))))
        (t nil)))

;; ----------------------------------------------------------------------
;; PUNT D'ENTRADA PRINCIPAL
;; ----------------------------------------------------------------------

(defun agent-xyz999 (dades)
  "Retorna la jugada que realitza l'agent XYZ999 en un torn."
  ;; Extraiem la informació empaquetada pel controlador
  (let ((equip (nth 1 dades))
        (pintura (nth 2 dades))
        (tipus-unitat (nth 3 dades))
        (coord (nth 4 dades))
        (tr-pintar (nth 7 dades))
        (tr-moure (nth 8 dades))
        (visio (nth 9 dades)))
    
    (cond ((eq tipus-unitat 'base)
           (agent-xyz999-decisio-base pintura coord visio))
          ((eq tipus-unitat 'bolla)
           (agent-xyz999-decisio-bolla coord equip tr-pintar tr-moure visio))
          (t nil))))