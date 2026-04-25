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
;; CERVELL DE LA BASE (Agent XYZ999)
;; ----------------------------------------------------------------------

(defun agent-xyz999-decisio-base (pintura coord visio)
  "Lògica per a les bases: Crear bolles de colors aleatoris si hi ha pintura."
  (cond ((>= pintura 50)
         (let ((buides (agent-xyz999-busca-caselles-buides-adj coord visio)))
           (cond ((null buides) nil)
                 (t 
                  ;; Triem un color a l'atzar entre r, g i b
                  (let ((color-aleatori (nth (random 3) '(r g b))))
                    (list (list 'crea-bolla (list color-aleatori (car buides)))))))))
        (t nil)))

;; ----------------------------------------------------------------------
;; CERVELL DE LA BOLLA
;; ----------------------------------------------------------------------

(defun agent-xyz999-decisio-bolla (coord equip tr-pintar tr-moure visio memoria)
  "Lògica per a les bolles: Disparar si pot, o moure's intel·ligentment."
  (let ((temps-pintar (if tr-pintar tr-pintar 0))
        (temps-moure (if tr-moure tr-moure 0)))
    (cond 
      ;; 1. Disparar (Prioritat 1)
      ((< temps-pintar 1)
       (let ((objectius (agent-xyz999-busca-objectius coord equip visio)))
         (cond ((not (null objectius))
                (list (list 'pinta (car objectius))))
               (t (agent-xyz999-intentar-moure coord temps-moure visio memoria)))))
      
      ;; 2. Moure's
      (t (agent-xyz999-intentar-moure coord temps-moure visio memoria)))))

(defun agent-xyz999-intentar-moure (coord temps-moure visio memoria)
  "Es mou cap al primer objectiu de la memòria. Si no n'hi ha, explora a l'atzar."
  (cond ((< temps-moure 1)
         (let ((buides (agent-xyz999-busca-caselles-buides-adj coord visio)))
           (cond ((null buides) nil)
                 (memoria
                  ;; INTEL·LIGÈNCIA: Tenim un objectiu a la llibreta! Anem cap a ell.
                  (let ((millor-casella (agent-xyz999-millor-pas buides (car memoria))))
                    (list (list 'mou millor-casella))))
                 (t
                  ;; EXPLORACIÓ: La memòria està buida, busquem a l'atzar.
                  (let ((casella-aleatoria (nth (random (length buides)) buides)))
                    (list (list 'mou casella-aleatoria)))))))
        (t nil)))
;; ----------------------------------------------------------------------
;; PUNT D'ENTRADA PRINCIPAL
;; ----------------------------------------------------------------------

(defun agent-xyz999 (dades)
  "Retorna (accio nova-memoria) o (nil nova-memoria)."
  (let* ((equip (nth 1 dades))
         (pintura (nth 2 dades))
         (tipus-unitat (nth 4 dades))
         (coord (nth 5 dades))
         (tr-pintar (nth 8 dades))
         (tr-moure (nth 9 dades))
         (visio (nth 10 dades))
         (memoria-antiga (nth 11 dades)) 
         
         ;; 1. La unitat llegeix la visió i apunta/esborra coses a la llibreta
         (memoria-nova (agent-xyz999-actualitza-memoria visio memoria-antiga equip))
         
         ;; 2. Pren la decisió
         (accio (cond ((eq tipus-unitat 'base)
                       (agent-xyz999-decisio-base pintura coord visio))
                      ((eq tipus-unitat 'bolla)
                       (agent-xyz999-decisio-bolla coord equip tr-pintar tr-moure visio memoria-nova))
                      (t nil))))
    
    ;; 3. Retornem l'acció EXACTAMENT com la demana el controlador
    ;; Retornem una llista amb l'acció original i la llibreta actualitzada
    (list accio memoria-nova)))

;; ----------------------------------------------------------------------
;; FUNCIONS DE GESTIÓ DE MEMORIA
;; ----------------------------------------------------------------------

(defun agent-xyz999-esborra-coord (coord llista)
  "Esborra una coordenada de la memòria."
  (cond ((null llista) nil)
        ((equal coord (car llista)) (agent-xyz999-esborra-coord coord (cdr llista)))
        (t (cons (car llista) (agent-xyz999-esborra-coord coord (cdr llista))))))

(defun agent-xyz999-actualitza-memoria (visio memoria el-meu-equip)
  "Llegeix la visió i actualitza la llibreta (memòria) amb els objectius."
  (cond ((null visio) memoria)
        (t
         (let* ((casella (car visio))
                (coord (car casella))
                (element (cadddr casella))
                (equip-element (nth 4 casella))
                ;; Cridem recursivament per a la resta de la visió
                (mem-restant (agent-xyz999-actualitza-memoria (cdr visio) memoria el-meu-equip)))
           (cond
             ;; Si és un Lab neutral/enemic o una Base enemiga, l'afegim (si no hi és ja)
              ((and element (not (eq equip-element el-meu-equip)) 
                   (or (eq element 'lab) (eq element 'base)))
                (if (member coord mem-restant :test #'equal)
                    mem-restant ;;si ja està en memoria retorna la memoria que ja tenim
                (cons coord mem-restant)) ;;else retorna la cordenada nova + memoria que ja tenim
              )
             
             ;; Si és un Lab NOSTRE (ja capturat), l'esborrem de la memòria perquè deixin d'anar-hi
             ((and (eq element 'lab) (eq equip-element el-meu-equip))
              (agent-xyz999-esborra-coord coord mem-restant))
             
             (t mem-restant))))))

(defun agent-xyz999-millor-pas (buides desti)
  "Tria la casella buida que ens acosta més a la coordenada destí."
  (cond ((null buides) nil)
        ((null (cdr buides)) (car buides))
        (t (let ((millor-resta (agent-xyz999-millor-pas (cdr buides) desti)))
             (if (< (agent-xyz999-distancia-q (car buides) desti)
                    (agent-xyz999-distancia-q millor-resta desti))
                 (car buides)
                 millor-resta)))))