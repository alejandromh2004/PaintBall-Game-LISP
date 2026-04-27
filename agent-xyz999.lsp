;; ======================================================================
;; AGENT FUNCIONAL COMPLET v2 - MOVIMENT MILLORAT
;; Canvis clau:
;;   1. Destí d'exploració = posició ACTUAL + vector, no base + vector
;;   2. Tabu GRADUAT (600/250/80/20) en lloc de 10000 pla
;;   3. Penalització ANTI-RAMADA: repulsa per aliats propers a la visió
;;   4. Historial ampliat a 6 posicions
;;   5. Pertorbació per ronda: quan atascats, el cost s'escala per evitar
;;      que tots convergeixin al mateix candidat
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
  "Afegeix coord a l'historial de la unitat (màxim 6 posicions)."
  (let* ((paths    (agent-xyz999-get 'unit-paths mem))
         (path-act (agent-xyz999-get id paths))
         (nou-path (agent-xyz999-take 6 (cons coord path-act)))
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
    (t (let* ((c      (car vis))
              (coord  (agent-xyz999-c-coord c))
              (elem   (agent-xyz999-c-elem c))
              (eq-c   (agent-xyz999-c-equip c))
              (colors (agent-xyz999-c-colors c))

              (m1 (cond ((and (eq elem 'base) (not (eq eq-c equip)))
                         (agent-xyz999-set 'colors-base-enemy colors
                           (agent-xyz999-set 'base-enemy coord mem)))
                        ((and (eq elem 'base) (eq eq-c equip))
                         (agent-xyz999-set 'base-ally coord mem))
                        (t mem)))

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
        (t (let* ((c     (car vis))
                  (score (agent-xyz999-puntua-tret c equip coord)))
             (cond ((> score best-score)
                    (agent-xyz999-millor-tret (cdr vis) equip coord (agent-xyz999-c-coord c) score))
                   (t
                    (agent-xyz999-millor-tret (cdr vis) equip coord best-coord best-score)))))))

;; ======================================================================
;; ANTI-RAMADA: detectar aliats a la visió
;; ======================================================================

(defun agent-xyz999-allies-vis (vis equip)
  "Retorna una llista de coords dels aliats (bolles) visibles."
  (cond ((null vis) nil)
        (t (let* ((c    (car vis))
                  (elem (agent-xyz999-c-elem c))
                  (eq-c (agent-xyz999-c-equip c)))
             (cond ((and (eq elem 'bolla) (eq eq-c equip))
                    (cons (agent-xyz999-c-coord c)
                          (agent-xyz999-allies-vis (cdr vis) equip)))
                   (t (agent-xyz999-allies-vis (cdr vis) equip)))))))

(defun agent-xyz999-penalty-ramada (co allies)
  "Penalitza el moviment cap a posicions properes a aliats."
  (cond ((null allies) 0)
        (t (let ((dist (agent-xyz999-dist-q co (car allies))))
             (+ (cond ((= dist 0) 2000)   ; ocupat per aliat
                      ((< dist 3) 600)    ; distància 1 (adjacent)
                      ((< dist 8) 200)    ; distància 2
                      ((< dist 18) 60)    ; distància 3
                      (t 0))
                (agent-xyz999-penalty-ramada co (cdr allies)))))))

;; ======================================================================
;; TABU GRADUAT
;; ======================================================================

(defun agent-xyz999-tabu-penalty (co unit-path idx)
  "Penalitza posicions recents de forma decreixent:
   posició t-1=600, t-2=250, t-3=80, t-4=30, t-5+=10"
  (cond ((null unit-path) 0)
        ((equal co (car unit-path))
         (cond ((= idx 0) 600)
               ((= idx 1) 250)
               ((= idx 2) 80)
               ((= idx 3) 30)
               (t 10)))
        (t (agent-xyz999-tabu-penalty co (cdr unit-path) (+ idx 1)))))

;; ======================================================================
;; NAVEGACIÓ: MOVIBLES I COST MILLORAT
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

(defun agent-xyz999-cost-mov (co desti unit-path allies)
  "Cost total = distància al destí + penalització tabu + repulsió ramada."
  (let* ((dist   (agent-xyz999-dist-q co desti))
         (tabu   (agent-xyz999-tabu-penalty co unit-path 0))
         (ramada (agent-xyz999-penalty-ramada co allies)))
    (+ dist tabu ramada)))

(defun agent-xyz999-millor-mov (movibles desti unit-path allies best-coord best-cost)
  (cond ((null movibles) best-coord)
        (t (let* ((co   (car movibles))
                  (cost (agent-xyz999-cost-mov co desti unit-path allies)))
             (cond ((< cost best-cost)
                    (agent-xyz999-millor-mov (cdr movibles) desti unit-path allies co cost))
                   (t
                    (agent-xyz999-millor-mov (cdr movibles) desti unit-path allies best-coord best-cost)))))))

;; ======================================================================
;; DESTINS: BASATS EN POSICIÓ ACTUAL (no en coordenades absolutes!)
;; ======================================================================

(defun agent-xyz999-dir-vec (idx)
  "8 direccions cardinales/diagonals amb magnitud ~25.
   (18 ≈ 25/sqrt(2) per a diagonals)"
  (cond ((= idx 0) '(25  0))
        ((= idx 1) '(18  18))
        ((= idx 2) '(0   25))
        ((= idx 3) '(-18 18))
        ((= idx 4) '(-25 0))
        ((= idx 5) '(-18 -18))
        ((= idx 6) '(0   -25))
        ((= idx 7) '(18  -18))
        (t         '(25  0))))

(defun agent-xyz999-closest-lab (labs coord best-lab best-dist)
  (cond ((null labs) best-lab)
        (t (let ((dist (agent-xyz999-dist-q (car labs) coord)))
             (cond ((< dist best-dist)
                    (agent-xyz999-closest-lab (cdr labs) coord (car labs) dist))
                   (t (agent-xyz999-closest-lab (cdr labs) coord best-lab best-dist)))))))

(defun agent-xyz999-desti-bolla (id coord mem ronda)
  "Destí basat en POSICIÓ ACTUAL + vector per a l'exploració.
   Això evita el problema de coordenades absolutes desconegudes."
  (let* ((base-enemy (agent-xyz999-get 'base-enemy mem))
         (base-ally  (agent-xyz999-get 'base-ally mem))
         (labs       (agent-xyz999-get 'labs mem))
         (abs-id     (agent-xyz999-abs id))

         ;; Direcció fixa per unitat: 8 agents → 8 direccions, cap ramada
         (dir-idx  (rem abs-id 8))
         (d        (agent-xyz999-dir-vec dir-idx))

         ;; Destí d'exploració = posició ACTUAL + vector
         ;; Cada vegada que es mou, el destí avança en la mateixa direcció
         (dest-exp (list (+ (car coord)  (car d))
                         (+ (cadr coord) (cadr d))))

         ;; Patrulla defensiva: orbitar la base aliada
         ;; Si tenim base-ally, posar destí a ~8 unitats en una direcció rotant
         (dirs-pat '((8 0) (6 6) (0 8) (-6 6) (-8 0) (-6 -6) (0 -8) (6 -6)))
         (d-pat    (agent-xyz999-nth (rem (agent-xyz999-abs ronda) 8) dirs-pat))
         (dest-pat (cond (base-ally
                          (list (+ (car base-ally)  (car d-pat))
                                (+ (cadr base-ally) (cadr d-pat))))
                         (t dest-exp)))

         ;; Rol: 0,1 mod 3 → atacant; 2 mod 3 → explorador/defensor
         (rol (rem abs-id 3))
         (is-attacker (< rol 2))
         (is-defender (= (rem abs-id 6) 5)))

    (cond
      ;; Prioritat 1: si hi ha labs enemics visibles/recordats → captura'ls
      ((and labs (> (agent-xyz999-longitud labs) 0))
       (agent-xyz999-closest-lab labs coord nil 100000))

      ;; Prioritat 2: atacants → base enemiga (si la coneixem)
      ((and is-attacker base-enemy)
       base-enemy)

      ;; Prioritat 3: defensor → patrullar la base aliada
      (is-defender dest-pat)

      ;; Prioritat 4: explorar en la pròpia direcció des de la posició actual
      (t dest-exp))))

;; ======================================================================
;; DECISIONS UNITÀRIES (BASE / BOLLA)
;; ======================================================================

(defun agent-xyz999-decisio-bolla (coord equip tr-pintar tr-moure vis mem id ronda)
  (let* ((tp (cond (tr-pintar tr-pintar) (t 0)))
         (tm (cond (tr-moure tr-moure) (t 0)))

         ;; Historial d'aquesta unitat
         (paths     (agent-xyz999-get 'unit-paths mem))
         (unit-path (agent-xyz999-get id paths))

         ;; Aliats visibles (per penalitzar ramada)
         (allies (agent-xyz999-allies-vis vis equip))

         ;; Tret
         (target-tret (cond ((< tp 1)
                             (agent-xyz999-millor-tret vis equip coord nil -1))
                            (t nil)))
         (tret (cond (target-tret (list 'pinta (list target-tret))) (t nil)))

         ;; Moviment
         (desti      (agent-xyz999-desti-bolla id coord mem ronda))
         (movs       (agent-xyz999-movibles vis coord))
         (target-mov (cond ((< tm 1)
                            (agent-xyz999-millor-mov movs desti unit-path allies nil 1000000))
                           (t nil)))
         (mou (cond (target-mov (list 'mou (list target-mov))) (t nil))))

    (append (cond (tret (list tret)) (t nil))
            (cond (mou  (list mou))  (t nil)))))

(defun agent-xyz999-decisio-base (coord vis mem pintura ronda id)
  (cond
    ((< pintura 50) nil)
    (t (let* ((colors-enemy (agent-xyz999-get 'colors-base-enemy mem))
              (utils  (agent-xyz999-filtra-colors '(r g b) colors-enemy))
              (colors (cond ((null utils) '(r g b)) (t utils)))
              (color  (agent-xyz999-nth (rem (agent-xyz999-abs ronda)
                                             (agent-xyz999-longitud colors)) colors))

              ;; Spawn cap a la base enemiga si la coneixem
              (movs       (agent-xyz999-movibles vis coord))
              (base-enemy (agent-xyz999-get 'base-enemy mem))
              (desti (cond (base-enemy base-enemy)
                           (t (list (+ (car coord) 10) (+ (cadr coord) 10)))))
              (spawn (agent-xyz999-millor-mov movs desti nil nil nil 1000000)))

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
         (tr-pintar   (nth 8  dades))
         (tr-moure    (nth 9  dades))
         (vis         (nth 10 dades))
         (mem-old     (nth 11 dades))

         ;; 1. Actualitzar memòria amb la visió actual
         (mem-vis  (agent-xyz999-actualitza-mem vis mem-old equip))

         ;; 2. Guardar nova posició a l'historial (només bolles)
         (mem-nova (cond ((eq tipus 'bolla)
                          (agent-xyz999-update-unit-path id coord mem-vis))
                         (t mem-vis)))

         ;; 3. Prendre decisió
         (accions  (cond ((eq tipus 'base)
                          (agent-xyz999-decisio-base coord vis mem-nova pintura ronda id))
                         ((eq tipus 'bolla)
                          (agent-xyz999-decisio-bolla coord equip tr-pintar tr-moure vis mem-nova id ronda))
                         (t nil))))

    ;; 4. Retornar escriptura de memòria i accions
    (append (list (list 'escriu-memoria (list mem-nova))) accions)))