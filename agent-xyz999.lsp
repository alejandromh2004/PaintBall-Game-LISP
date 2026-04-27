;; ======================================================================
;; AGENT BÀSIC FUNCIONAL
;; ======================================================================
;; Aquest és un agent completament bàsic i funcional que només fa les
;; tasques fonamentals: guardar les posicions de les bases,
;; disparar a enemics/base quan els veu i moure's de forma pseudoaleatòria.
;; No utilitza cap variable global ni instruccions imperatives (setq, loop, etc).
;; Ideal com a punt de partida net per afegir lògica avançada.

;; ======================================================================
;; SECCIÓ 1: FUNCIONS BÀSIQUES
;; ======================================================================

(defun agent-xyz999-dist-q (c1 c2)
  "Calcula la distància al quadrat entre dues coordenades (x y)."
  (cond ((or (null c1) (null c2)) 100000)
        (t (+ (* (- (car c1) (car c2)) (- (car c1) (car c2)))
              (* (- (cadr c1) (cadr c2)) (- (cadr c1) (cadr c2)))))))

(defun agent-xyz999-longitud (lst)
  "Longitud de la llista lst. Recursió simple."
  (cond ((null lst) 0)
        (t (+ 1 (agent-xyz999-longitud (cdr lst))))))

(defun agent-xyz999-nth (n lst)
  "Element n-è d'una llista (0-indexat)."
  (cond ((null lst) nil)
        ((= n 0) (car lst))
        (t (agent-xyz999-nth (- n 1) (cdr lst)))))

(defun agent-xyz999-rem (num div)
  "Mòdul de divisió per evitar errors de signes."
  (cond ((< num 0) (agent-xyz999-rem (- num) div))
        (t (rem num div))))

;; ======================================================================
;; SECCIÓ 2: GESTIÓ DE LA MEMÒRIA (A-LIST)
;; ======================================================================

(defun agent-xyz999-get (clau mem)
  "Obté un valor de la llista d'associació mem."
  (cond ((null mem) nil)
        ((eq (caar mem) clau) (cdar mem))
        (t (agent-xyz999-get clau (cdr mem)))))

(defun agent-xyz999-set (clau val mem)
  "Retorna una nova llista d'associació amb el parell (clau . val) actualitzat."
  (cond ((null mem) (list (cons clau val)))
        ((eq (caar mem) clau) (cons (cons clau val) (cdr mem)))
        (t (cons (car mem) (agent-xyz999-set clau val (cdr mem))))))

;; ======================================================================
;; SECCIÓ 3: ACCESSORS PER A LA VISIÓ
;; ======================================================================

(defun agent-xyz999-c-coord  (c) (nth 0 c))
(defun agent-xyz999-c-tipus  (c) (nth 1 c))
(defun agent-xyz999-c-color  (c) (nth 2 c))
(defun agent-xyz999-c-elem   (c) (nth 3 c))
(defun agent-xyz999-c-equip  (c) (nth 4 c))
(defun agent-xyz999-c-colors (c) (nth 5 c))

;; ======================================================================
;; SECCIÓ 4: ACTUALITZACIÓ DE LA MEMÒRIA
;; ======================================================================

(defun agent-xyz999-actualitza-mem (vis mem equip)
  "Només guarda la posició de la nostra base i la base enemiga si les veu."
  (cond
    ((null vis) mem)
    (t (let* ((c     (car vis))
              (coord (agent-xyz999-c-coord c))
              (elem  (agent-xyz999-c-elem c))
              (eq-c  (agent-xyz999-c-equip c))
              (mem1  (cond
                       ((and (eq elem 'base) coord (not (eq eq-c equip)))
                        (agent-xyz999-set 'base-enemy coord mem))
                       ((and (eq elem 'base) coord (eq eq-c equip))
                        (agent-xyz999-set 'base-ally coord mem))
                       (t mem))))
         (agent-xyz999-actualitza-mem (cdr vis) mem1 equip)))))

;; ======================================================================
;; SECCIÓ 5: FUNCIONS AUXILIARS DE NAVEGACIÓ I DECISIÓ
;; ======================================================================

(defun agent-xyz999-movibles (vis coord)
  "Retorna llista de coordenades on ens podem moure (dist<=2 i buides)."
  (cond
    ((null vis) nil)
    (t (let* ((c  (car vis))
              (co (agent-xyz999-c-coord c)))
         (cond ((and (eq (agent-xyz999-c-tipus c) 'terra)
                     (null (agent-xyz999-c-elem c))
                     (<= (agent-xyz999-dist-q co coord) 2)
                     (not (equal co coord)))
                (cons co (agent-xyz999-movibles (cdr vis) coord)))
               (t (agent-xyz999-movibles (cdr vis) coord)))))))

(defun agent-xyz999-tria-tret (vis equip coord)
  "Busca el primer element enemic (base o bolla) a distància <=5."
  (cond ((null vis) nil)
        (t (let* ((c (car vis))
                  (elem (agent-xyz999-c-elem c))
                  (eq-c (agent-xyz999-c-equip c))
                  (dist (agent-xyz999-dist-q coord (agent-xyz999-c-coord c))))
             (cond ((and (<= dist 5)
                         (or (eq elem 'base) (eq elem 'bolla) (eq elem 'lab))
                         (not (eq eq-c equip)))
                    (list 'pinta (list (agent-xyz999-c-coord c))))
                   (t (agent-xyz999-tria-tret (cdr vis) equip coord)))))))

(defun agent-xyz999-random-element (lst var1 var2)
  "Selecciona un element de la llista utilitzant funcions matemàtiques bàsiques."
  (cond ((null lst) nil)
        (t (let ((len (agent-xyz999-longitud lst)))
             (agent-xyz999-nth (agent-xyz999-rem (+ var1 var2) len) lst)))))

;; ======================================================================
;; SECCIÓ 6: DECISIONS D'UNITAT (BASE I BOLLA)
;; ======================================================================

(defun agent-xyz999-decisio-bolla (coord equip tr-pintar tr-moure vis mem id ronda)
  "Decisió d'una bolla: disparar a la primera cosa que vegi i moure's pseudoaleatòriament."
  (let* ((tp (cond (tr-pintar tr-pintar) (t 0)))
         (tm (cond (tr-moure tr-moure) (t 0)))
         (tret (cond ((< tp 1) (agent-xyz999-tria-tret vis equip coord)) (t nil)))
         (movs (agent-xyz999-movibles vis coord))
         (desti (agent-xyz999-random-element movs id ronda))
         (mou (cond ((and (< tm 1) desti) (list 'mou (list desti))) (t nil))))
    (append (cond (tret (list tret)) (t nil))
            (cond (mou (list (list mou))) (t nil))))) ; <-- 'mou' de vegades cal embolicar-lo correctament

(defun agent-xyz999-decisio-base (coord vis mem pintura ronda id)
  "Decisió de la base: crear una bolla sempre que pugui i assignar-li un color i spawn lliures."
  (cond
    ((< pintura 50) nil)
    (t (let* ((colors '(r g b))
              (color (agent-xyz999-random-element colors id ronda))
              (movs (agent-xyz999-movibles vis coord))
              (spawn (agent-xyz999-random-element movs id ronda)))
         (cond ((and spawn color) (list (list 'crea-bolla (list color spawn))))
               (t nil))))))

;; ======================================================================
;; SECCIÓ 7: PUNT D'ENTRADA DE L'AGENT
;; ======================================================================

(defun agent-xyz999 (dades)
  "Funció principal de l'agent cridada pel simulador."
  (let* ((ronda       (nth 0  dades))
         (equip       (nth 1  dades))
         (pintura     (nth 2  dades))
         (id          (nth 3  dades))
         (tipus       (nth 4  dades))
         (coord       (nth 5  dades))
         (color-propi (nth 7  dades))
         (tr-pintar   (nth 8  dades))
         (tr-moure    (nth 9  dades))
         (vis         (nth 10 dades))
         (mem-old     (nth 11 dades))
         
         ;; 1. Actualitzar memòria
         (mem-nova    (agent-xyz999-actualitza-mem vis mem-old equip))
         
         ;; 2. Prendre decisió
         (accions     (cond
                        ((eq tipus 'base)
                         (agent-xyz999-decisio-base coord vis mem-nova pintura ronda id))
                        ((eq tipus 'bolla)
                         ;; Passem (list 'mou ...) correctament
                         (let* ((tp (cond (tr-pintar tr-pintar) (t 0)))
                                (tm (cond (tr-moure tr-moure) (t 0)))
                                (tret (cond ((< tp 1) (agent-xyz999-tria-tret vis equip coord)) (t nil)))
                                (movs (agent-xyz999-movibles vis coord))
                                (desti (agent-xyz999-random-element movs id ronda))
                                (mou (cond ((and (< tm 1) desti) (list 'mou (list desti))) (t nil))))
                           (append (cond (tret (list tret)) (t nil))
                                   (cond (mou (list mou)) (t nil)))))
                        (t nil))))
    
    ;; 3. Retornar escriptura de memòria i accions
    (append (list (list 'escriu-memoria (list mem-nova))) accions)))