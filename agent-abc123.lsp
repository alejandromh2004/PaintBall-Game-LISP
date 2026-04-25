;; ======================================================================
;; PRÀCTICA FINAL LLENGUATGES DE PROGRAMACIÓ - LISP - PAINTBALL
;; ======================================================================
;; Estudiant: ABC
;; Data: 25/04/2026
;; Assignatura: Llenguatges de Programació (LP)
;; Grup: <Indicar Grup>
;; Professors: <Indicar Professors>
;; Convocatòria: Primera Convocatòria (Ordinària)
;;
;; ----------------------------------------------------------------------
;; FITXER: agent-abc123.lsp
;; DESCRIPCIÓ: Agent intel·ligent bàsic. Implementa una lògica reactiva
;; de disparar a objectius visibles o moure's aleatòriament si no n'hi ha.
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; FUNCIONS AUXILIARS (Totes amb el prefix agent-abc123-)
;; ----------------------------------------------------------------------

(defun agent-abc123-distancia-q (c1 c2)
  "Calcula la distància euclidiana al quadrat entre dues coordenades."
  (+ (* (- (car c1) (car c2)) (- (car c1) (car c2)))
     (* (- (cadr c1) (cadr c2)) (- (cadr c1) (cadr c2)))))

(defun agent-abc123-es-terra-buida (casella)
  "Comprova si una casella de la visió és terra i no té cap element a sobre."
  ;; Format visió: (coord tipus-casella color-casella tipus-element ...)
  (and (eq (cadr casella) 'terra)
       (null (cadddr casella))))

(defun agent-abc123-es-enemic-o-lab (casella el-meu-equip)
  "Comprova si hi ha un element disparable (enemic o laboratori no propi)."
  (let ((element (cadddr casella))
        (equip-casella (nth 4 casella)))
    (and element
         (not (eq equip-casella el-meu-equip)))))

(defun agent-abc123-busca-caselles-buides-adj (coord visio)
  "Retorna una llista de coordenades buides a distància <= 2."
  (cond ((null visio) nil)
        ((and (agent-abc123-es-terra-buida (car visio))
              (<= (agent-abc123-distancia-q coord (car (car visio))) 2)
              (not (equal coord (car (car visio))))) ; Que no sigui on som ara
         (cons (car (car visio)) (agent-abc123-busca-caselles-buides-adj coord (cdr visio))))
        (t (agent-abc123-busca-caselles-buides-adj coord (cdr visio)))))

(defun agent-abc123-busca-objectius (coord el-meu-equip visio)
  "Retorna coordenades d'objectius a distància de tret (<= 5)."
  (cond ((null visio) nil)
        ((and (agent-abc123-es-enemic-o-lab (car visio) el-meu-equip)
              (<= (agent-abc123-distancia-q coord (car (car visio))) 5))
         (cons (car (car visio)) (agent-abc123-busca-objectius coord el-meu-equip (cdr visio))))
        (t (agent-abc123-busca-objectius coord el-meu-equip (cdr visio)))))

;; ----------------------------------------------------------------------
;; CERVELL DE LA BASE (Agent ABC123)
;; ----------------------------------------------------------------------

(defun agent-abc123-decisio-base (pintura coord visio)
  "Lògica per a les bases: Crear bolles de colors aleatoris si hi ha pintura."
  (cond ((>= pintura 50)
         (let ((buides (agent-abc123-busca-caselles-buides-adj coord visio)))
           (cond ((null buides) nil)
                 (t 
                  ;; Triem un color a l'atzar entre r, g i b
                  (let ((color-aleatori (nth (random 3) '(r g b))))
                    (list (list 'crea-bolla (list color-aleatori (car buides)))))))))
        (t nil)))

;; ----------------------------------------------------------------------
;; CERVELL DE LA BOLLA
;; ----------------------------------------------------------------------

(defun agent-abc123-decisio-bolla (coord equip tr-pintar tr-moure visio)
  "Lògica per a les bolles: Disparar si pot, o moure's."
  ;; Els cooldowns poden venir com a nil al principi, els tractem com a 0
  (let ((temps-pintar (cond (tr-pintar tr-pintar) (t 0)))
        (temps-moure (cond (tr-moure tr-moure) (t 0))))
    
    (cond 
      ;; 1. Prioritat: Disparar a un objectiu si el cooldown és < 1
      ((< temps-pintar 1)
       (let ((objectius (agent-abc123-busca-objectius coord equip visio)))
         (cond ((not (null objectius))
                ;; Disparem al primer objectiu que veiem (l'argument ha de ser una llista)
                (list (list 'pinta (list (car objectius)))))
               (t 
                ;; Si no hi ha objectius, intentem moure'ns
                (agent-abc123-intentar-moure coord temps-moure visio)))))
      
      ;; 2. Si no pot disparar, intenta moure's
      (t (agent-abc123-intentar-moure coord temps-moure visio)))))

(defun agent-abc123-intentar-moure (coord temps-moure visio)
  "Sub-lògica per moure una bolla de forma aleatòria per explorar el mapa."
  (cond ((< temps-moure 1)
         (let ((buides (agent-abc123-busca-caselles-buides-adj coord visio)))
           (cond ((not (null buides))
                  ;; TRUC D'INTEL·LIGÈNCIA: Triem una casella buida a l'atzar! (L'argument ha de ser una llista)
                  (let ((casella-aleatoria (nth (random (length buides)) buides)))
                    (list (list 'mou (list casella-aleatoria)))))
                 (t nil))))
        (t nil)))

;; ----------------------------------------------------------------------
;; PUNT D'ENTRADA PRINCIPAL
;; ----------------------------------------------------------------------

(defun agent-abc123 (dades)
  "Retorna la jugada que realitza l'agent ABC123 en un torn."
  ;; Extraiem la informació empaquetada pel controlador
  (let ((equip (nth 1 dades))
        (pintura (nth 2 dades))
        (tipus-unitat (nth 4 dades))
        (coord (nth 5 dades))
        (tr-pintar (nth 8 dades))
        (tr-moure (nth 9 dades))
        (visio (nth 10 dades)))
    
    (cond ((eq tipus-unitat 'base)
           (agent-abc123-decisio-base pintura coord visio))
          ((eq tipus-unitat 'bolla)
           (agent-abc123-decisio-bolla coord equip tr-pintar tr-moure visio))
          (t nil))))