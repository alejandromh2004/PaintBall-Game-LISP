;; ======================================================================
;; AGENT FUNCIONAL COMPLET - ESTRATÈGIA AMB ROLS I ANTI-ATASCOS
;; ======================================================================

(defun agent-xyz999-abs (n)
  (cond ((null n) 0) ((< n 0) (- n)) (t n)))

(defun agent-xyz999-dist-q (c1 c2)
  (cond ((or (null c1) (null c2)) 100000)
        (t (+ (* (- (car c1) (car c2)) (- (car c1) (car c2)))
              (* (- (cadr c1) (cadr c2)) (- (cadr c1) (cadr c2)))))))

(defun agent-xyz999-longitud (lst)
  (cond ((null lst) 0)
        (t (+ 1 (agent-xyz999-longitud (cdr lst))))))

(defun agent-xyz999-nth (n lst)
  (cond ((null lst) nil)
        ((= n 0) (car lst))
        (t (agent-xyz999-nth (- n 1) (cdr lst)))))

(defun agent-xyz999-membre (elem lst)
  (cond ((null lst) nil)
        ((equal (car lst) elem) t)
        (t (agent-xyz999-membre elem (cdr lst)))))

(defun agent-xyz999-elimina (elem lst)
  (cond ((null lst) nil)
        ((equal (car lst) elem) (agent-xyz999-elimina elem (cdr lst)))
        (t (cons (car lst) (agent-xyz999-elimina elem (cdr lst))))))

(defun agent-xyz999-take (n lst)
  (cond ((or (<= n 0) (null lst)) nil)
        (t (cons (car lst) (agent-xyz999-take (- n 1) (cdr lst))))))

;; ======================================================================
;; MEMÒRIA COMPARTIDA (A-LIST)
;; ======================================================================

(defun agent-xyz999-get (clau mem)
  (cond ((null mem) nil)
        ((eq (caar mem) clau) (cdar mem))
        (t (agent-xyz999-get clau (cdr mem)))))

(defun agent-xyz999-set (clau val mem)
  (cond ((null mem) (list (cons clau val)))
        ((eq (caar mem) clau) (cons (cons clau val) (cdr mem)))
        (t (cons (car mem) (agent-xyz999-set clau val (cdr mem))))))

(defun agent-xyz999-update-unit-path (id coord mem)
  "Afegeix la coord a l'historial de la unitat (màxim 4 posicions) per evitar cicles."
  (let* ((paths (agent-xyz999-get 'unit-paths mem))
         (path-actual (agent-xyz999-get id paths))
         (nou-path (agent-xyz999-take 4 (cons coord path-actual)))
         (nous-paths (agent-xyz999-set id nou-path paths)))
    (agent-xyz999-set 'unit-paths nous-paths mem)))

;; ======================================================================
;; ACCESSORS PER A LA VISIÓ
;; ======================================================================

(defun agent-xyz999-c-coord  (c) (nth 0 c))
(defun agent-xyz999-c-tipus  (c) (nth 1 c))
(defun agent-xyz999-c-color  (c) (nth 2 c))
(defun agent-xyz999-c-elem   (c) (nth 3 c))
(defun agent-xyz999-c-equip  (c) (nth 4 c))
(defun agent-xyz999-c-colors (c) (nth 5 c))

;; ======================================================================
;; ACTUALITZACIÓ DE LA MEMÒRIA (VISIÓ)
;; ======================================================================

(defun agent-xyz999-actualitza-mem (vis mem equip)
  (cond
    ((null vis) mem)
    (t (let* ((c     (car vis))
              (coord (agent-xyz999-c-coord c))
              (elem  (agent-xyz999-c-elem c))
              (eq-c  (agent-xyz999-c-equip c))
              (colors (agent-xyz999-c-colors c))
              
              ;; Base
              (m1 (cond ((and (eq elem 'base) (not (eq eq-c equip)))
                         (agent-xyz999-set 'colors-base-enemy colors 
                                           (agent-xyz999-set 'base-enemy coord mem)))
                        ((and (eq elem 'base) (eq eq-c equip))
                         (agent-xyz999-set 'base-ally coord mem))
                        (t mem)))
              
              ;; Labs
              (labs (agent-xyz999-get 'labs m1))
              (m2 (cond ((and (eq elem 'lab) (not (eq eq-c equip)))
                         (cond ((not (agent-xyz999-membre coord labs))
                                (agent-xyz999-set 'labs (cons coord labs) m1))
                               (t m1)))
                        ((and (eq elem 'lab) (eq eq-c equip))
                         (agent-xyz999-set 'labs (agent-xyz999-elimina coord labs) m1))
                        (t m1))))
         (agent-xyz999-actualitza-mem (cdr vis) m2 equip)))))

;; ======================================================================
;; LÒGICA DE COLORS
;; ======================================================================

(defun agent-xyz999-filtra-colors (tots pintats)
  (cond ((null tots) nil)
        ((agent-xyz999-membre (car tots) pintats)
         (agent-xyz999-filtra-colors (cdr tots) pintats))
        (t (cons (car tots) (agent-xyz999-filtra-colors (cdr tots) pintats)))))

;; ======================================================================
;; LÒGICA DE TRET
;; ======================================================================

(defun agent-xyz999-puntua-tret (c equip coord)
  "Retorna una puntuació per a l'objectiu. Com més alt, millor."
  (let* ((elem (agent-xyz999-c-elem c))
         (eq-c (agent-xyz999-c-equip c))
         (dist (agent-xyz999-dist-q coord (agent-xyz999-c-coord c))))
    (cond ((> dist 5) -1)
          ((eq eq-c equip) -1)
          ((eq elem 'base) 1000)
          ((eq elem 'bolla) 500)
          ((eq elem 'lab) 100)
          (t -1))))

(defun agent-xyz999-millor-tret (vis equip coord best-coord best-score)
  (cond ((null vis) best-coord)
        (t (let* ((c (car vis))
                  (score (agent-xyz999-puntua-tret c equip coord)))
             (cond ((> score best-score)
                    (agent-xyz999-millor-tret (cdr vis) equip coord (agent-xyz999-c-coord c) score))
                   (t (agent-xyz999-millor-tret (cdr vis) equip coord best-coord best-score)))))))

;; ======================================================================
;; NAVEGACIÓ I ANTI-ATASCOS
;; ======================================================================

(defun agent-xyz999-movibles (vis coord)
  (cond ((null vis) nil)
        (t (let* ((c  (car vis))
                  (co (agent-xyz999-c-coord c)))
             (cond ((and (eq (agent-xyz999-c-tipus c) 'terra)
                         (null (agent-xyz999-c-elem c))
                         (<= (agent-xyz999-dist-q co coord) 2)
                         (not (equal co coord)))
                    (cons co (agent-xyz999-movibles (cdr vis) coord)))
                   (t (agent-xyz999-movibles (cdr vis) coord)))))))

(defun agent-xyz999-cost-mov (co desti unit-path)
  "Calcula el cost d'anar a `co` per arribar a `desti`.
   Penalitza immensament si `co` està a l'historial (evita atacs/oscil·lacions)."
  (let ((dist (agent-xyz999-dist-q co desti))
        (tabu (cond ((agent-xyz999-membre co unit-path) 10000) (t 0))))
    (+ dist tabu)))

(defun agent-xyz999-millor-mov (movibles desti unit-path best-coord best-cost)
  (cond ((null movibles) best-coord)
        (t (let* ((co (car movibles))
                  (cost (agent-xyz999-cost-mov co desti unit-path)))
             (cond ((< cost best-cost)
                    (agent-xyz999-millor-mov (cdr movibles) desti unit-path co cost))
                   (t (agent-xyz999-millor-mov (cdr movibles) desti unit-path best-coord best-cost)))))))

;; ======================================================================
;; ROLS I DESTINACIONS
;; ======================================================================

(defun agent-xyz999-closest-lab (labs coord best-lab best-dist)
  (cond ((null labs) best-lab)
        (t (let ((dist (agent-xyz999-dist-q (car labs) coord)))
             (cond ((< dist best-dist)
                    (agent-xyz999-closest-lab (cdr labs) coord (car labs) dist))
                   (t (agent-xyz999-closest-lab (cdr labs) coord best-lab best-dist)))))))

(defun agent-xyz999-desti-bolla (id coord mem ronda)
  "Determina la coordenada destí basant-se en el rol i l'estat del joc."
  (let* ((base-enemy (agent-xyz999-get 'base-enemy mem))
         (base-ally (agent-xyz999-get 'base-ally mem))
         (labs (agent-xyz999-get 'labs mem))
         (abs-id (agent-xyz999-abs id))
         (is-attacker (< (rem abs-id 3) 2)) ; 0 o 1
         (is-defender (= (rem abs-id 6) 5))
         
         ;; Lògica d'Explorador: Punt llunyà basat en l'id
         (dirs-exp '((30 0) (20 20) (0 30) (-20 20) (-30 0) (-20 -20) (0 -30) (20 -20)))
         (d-exp (agent-xyz999-nth (rem abs-id 8) dirs-exp))
         (bx (cond ((and base-ally (car base-ally)) (car base-ally)) (t 500)))
         (by (cond ((and base-ally (cadr base-ally)) (cadr base-ally)) (t 500)))
         (dest-exp (list (+ bx (car d-exp)) (+ by (cadr d-exp))))
         
         ;; Lògica de Defensor (Patrulla)
         (dirs-pat '((5 0) (3 3) (0 5) (-3 3) (-5 0) (-3 -3) (0 -5) (3 -3)))
         (d-pat (agent-xyz999-nth (rem (agent-xyz999-abs ronda) 8) dirs-pat))
         (dest-pat (cond (base-ally (list (+ bx (car d-pat)) (+ by (cadr d-pat))))
                         (t dest-exp))))
         
    (cond
      ;; 1. Si no coneixem la base enemiga, tothom explora o captura labs.
      ((null base-enemy)
       (cond ((and labs (> (agent-xyz999-longitud labs) 0)) 
              (agent-xyz999-closest-lab labs coord nil 100000))
             (t dest-exp)))
             
      ;; 2. Atacantes (2/3 de les boles)
      (is-attacker base-enemy)
      
      ;; 3. Defensors (1/6 de les boles)
      (is-defender dest-pat)
      
      ;; 4. Exploradors restants (1/6 de les boles) van a per labs o exploren
      (t (cond ((and labs (> (agent-xyz999-longitud labs) 0)) 
                (agent-xyz999-closest-lab labs coord nil 100000))
               (t dest-exp))))))

;; ======================================================================
;; DECISIONS UNITÀRIES (BASE / BOLLA)
;; ======================================================================

(defun agent-xyz999-decisio-bolla (coord equip tr-pintar tr-moure vis mem id ronda)
  (let* ((tp (cond (tr-pintar tr-pintar) (t 0)))
         (tm (cond (tr-moure tr-moure) (t 0)))
         
         ;; Actualitzar historial de camins per aquesta bola
         (paths (agent-xyz999-get 'unit-paths mem))
         (unit-path (agent-xyz999-get id paths))
         
         ;; Tret
         (target-tret (cond ((< tp 1) (agent-xyz999-millor-tret vis equip coord nil -1)) (t nil)))
         (tret (cond (target-tret (list 'pinta (list target-tret))) (t nil)))
         
         ;; Moviment
         (desti (agent-xyz999-desti-bolla id coord mem ronda))
         (movs (agent-xyz999-movibles vis coord))
         (target-mov (cond ((< tm 1) (agent-xyz999-millor-mov movs desti unit-path nil 1000000)) (t nil)))
         (mou (cond (target-mov (list 'mou (list target-mov))) (t nil))))
         
    (append (cond (tret (list tret)) (t nil))
            (cond (mou (list mou)) (t nil)))))

(defun agent-xyz999-decisio-base (coord vis mem pintura ronda id)
  (cond
    ((< pintura 50) nil)
    (t (let* ((colors-enemy (agent-xyz999-get 'colors-base-enemy mem))
              (utils (agent-xyz999-filtra-colors '(r g b) colors-enemy))
              (colors (cond ((null utils) '(r g b)) (t utils)))
              (color (agent-xyz999-nth (rem (agent-xyz999-abs ronda) (agent-xyz999-longitud colors)) colors))
              
              ;; Spawn preferentment cap endavant
              (movs (agent-xyz999-movibles vis coord))
              (base-enemy (agent-xyz999-get 'base-enemy mem))
              (desti (cond (base-enemy base-enemy) 
                           (t (list (+ (car coord) 10) (+ (cadr coord) 10)))))
              (spawn (agent-xyz999-millor-mov movs desti nil nil 1000000)))
              
         (cond ((and spawn color) (list (list 'crea-bolla (list color spawn))))
               (t nil))))))

;; ======================================================================
;; PUNT D'ENTRADA DE L'AGENT
;; ======================================================================

(defun agent-xyz999 (dades)
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
         
         ;; 1. Actualitzar memòria amb la visió actual
         (mem-vis     (agent-xyz999-actualitza-mem vis mem-old equip))
         
         ;; 2. Guardar la nova posició a l'historial (només si és bolla)
         (mem-nova    (cond ((eq tipus 'bolla) 
                             (agent-xyz999-update-unit-path id coord mem-vis))
                            (t mem-vis)))
         
         ;; 3. Prendre decisió
         (accions     (cond
                        ((eq tipus 'base)
                         (agent-xyz999-decisio-base coord vis mem-nova pintura ronda id))
                        ((eq tipus 'bolla)
                         (agent-xyz999-decisio-bolla coord equip tr-pintar tr-moure vis mem-nova id ronda))
                        (t nil))))
    
    ;; 4. Retornar escriptura de memòria i accions
    (append (list (list 'escriu-memoria (list mem-nova))) accions)))