;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: Alejandro Martinez Hermosa, Javier Vivo Samaniego.
;; Professor: Miquel Cabot.
;; Lliurament: primera convocatòria.
;; Agent intel·ligent del programa equip 1.

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
;; DETECCIÓ DE BLOQUEIG I GESTIÓ DE DIRECCIONS
;; ======================================================================

(defun agent-xyz999-is-stuck (unit-path)
  "Retorna t si les últimes 4 posicions estan totes dins dist²<10 entre si.
   Indica que l'agent porta rodes girant al mateix lloc (dead-end o ramada)."
  (cond ((< (agent-xyz999-longitud unit-path) 4) nil)
        (t (let* ((p0 (car unit-path))
                  (p1 (agent-xyz999-nth 1 unit-path))
                  (p2 (agent-xyz999-nth 2 unit-path))
                  (p3 (agent-xyz999-nth 3 unit-path)))
             (cond ((or (null p0) (null p1) (null p2) (null p3)) nil)
                   (t (and (< (agent-xyz999-dist-q p0 p1) 10)
                           (< (agent-xyz999-dist-q p0 p2) 10)
                           (< (agent-xyz999-dist-q p0 p3) 10))))))))

(defun agent-xyz999-get-dir (id mem)
  "Obté la direcció actual d'una unitat (default: abs-id mod 8)."
  (let* ((dirs (agent-xyz999-get 'unit-dirs mem))
         (d    (cond (dirs (agent-xyz999-get id dirs)) (t nil))))
    (cond ((null d) (rem (agent-xyz999-abs id) 8))
          (t d))))

(defun agent-xyz999-set-dir (id dir mem)
  "Desa la nova direcció d'una unitat a la memòria."
  (let* ((dirs     (agent-xyz999-get 'unit-dirs mem))
         (safe-dirs (cond (dirs dirs) (t nil)))
         (new-dirs (agent-xyz999-set id dir safe-dirs)))
    (agent-xyz999-set 'unit-dirs new-dirs mem)))

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
              ;; CANVI v3: labs limitat a 15 entrades per evitar creixement O(n)
              (m2 (cond ((and (eq elem 'lab) (not (eq eq-c equip)))
                         (cond ((not (agent-xyz999-membre coord labs))
                                (agent-xyz999-set 'labs
                                  (agent-xyz999-take 15 (cons coord labs)) m1))
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
;; CANVI v3: limitem a 6 aliats per evitar cost O(n²) amb molts agents
;; ======================================================================

(defun agent-xyz999-allies-vis-acc (vis equip n)
  "Recull fins a n aliats (bolles) visibles. Limit per eficiència."
  (cond ((or (null vis) (<= n 0)) nil)
        (t (let* ((c    (car vis))
                  (elem (agent-xyz999-c-elem c))
                  (eq-c (agent-xyz999-c-equip c)))
             (cond ((and (eq elem 'bolla) (eq eq-c equip))
                    (cons (agent-xyz999-c-coord c)
                          (agent-xyz999-allies-vis-acc (cdr vis) equip (- n 1))))
                   (t (agent-xyz999-allies-vis-acc (cdr vis) equip n)))))))

(defun agent-xyz999-allies-vis (vis equip)
  (agent-xyz999-allies-vis-acc vis equip 6))

(defun agent-xyz999-penalty-ramada (co allies)
  (cond ((null allies) 0)
        (t (let ((dist (agent-xyz999-dist-q co (car allies))))
             (+ (cond ((= dist 0) 2000)
                      ((< dist 3) 600)
                      ((< dist 8) 200)
                      ((< dist 18) 60)
                      (t 0))
                (agent-xyz999-penalty-ramada co (cdr allies)))))))

;; ======================================================================
;; TABU GRADUAT
;; ======================================================================

(defun agent-xyz999-tabu-penalty (co unit-path idx)
  (cond ((null unit-path) 0)
        ((equal co (car unit-path))
         (cond ((= idx 0) 600)
               ((= idx 1) 250)
               ((= idx 2) 80)
               ((= idx 3) 30)
               (t 10)))
        (t (agent-xyz999-tabu-penalty co (cdr unit-path) (+ idx 1)))))

;; ======================================================================
;; NAVEGACIÓ: MOVIBLES I COST
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
;; DESTINS: BASATS EN POSICIÓ ACTUAL
;; CANVI v3: dir-idx ve de la memòria (pot rotar si stuck), no és fix
;; ======================================================================

(defun agent-xyz999-dir-vec (idx)
  "8 direccions cardinales/diagonals amb magnitud ~25."
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

(defun agent-xyz999-desti-bolla (id coord mem ronda dir-idx)
  "Destí basat en POSICIÓ ACTUAL + vector.
   dir-idx ve de la memòria i pot haver estat rotat si l'agent estava stuck."
  (let* ((base-enemy (agent-xyz999-get 'base-enemy mem))
         (base-ally  (agent-xyz999-get 'base-ally mem))
         (labs       (agent-xyz999-get 'labs mem))
         (abs-id     (agent-xyz999-abs id))

         ;; Direcció actual (pot haver rotat per stuck)
         (d        (agent-xyz999-dir-vec dir-idx))
         (dest-exp (list (+ (car coord)  (car d))
                         (+ (cadr coord) (cadr d))))

         ;; Patrulla defensiva
         (dirs-pat '((8 0) (6 6) (0 8) (-6 6) (-8 0) (-6 -6) (0 -8) (6 -6)))
         (d-pat    (agent-xyz999-nth (rem (agent-xyz999-abs ronda) 8) dirs-pat))
         (dest-pat (cond (base-ally
                          (list (+ (car base-ally)  (car d-pat))
                                (+ (cadr base-ally) (cadr d-pat))))
                         (t dest-exp)))

         (rol         (rem abs-id 3))
         (is-attacker (< rol 2))
         (is-defender (= (rem abs-id 6) 5)))

    (cond
      ((and labs (> (agent-xyz999-longitud labs) 0))
       (agent-xyz999-closest-lab labs coord nil 100000))
      ((and is-attacker base-enemy)
       base-enemy)
      (is-defender dest-pat)
      (t dest-exp))))

;; ======================================================================
;; DECISIONS UNITÀRIES
;; ======================================================================

(defun agent-xyz999-decisio-bolla (coord equip tr-pintar tr-moure vis mem id ronda dir-idx)
  "CANVI v3: rep dir-idx com a paràmetre (pot ser la direcció rotada)."
  (let* ((tp (cond (tr-pintar tr-pintar) (t 0)))
         (tm (cond (tr-moure tr-moure) (t 0)))

         (paths     (agent-xyz999-get 'unit-paths mem))
         (unit-path (agent-xyz999-get id paths))

         (allies (agent-xyz999-allies-vis vis equip))

         (target-tret (cond ((< tp 1)
                             (agent-xyz999-millor-tret vis equip coord nil -1))
                            (t nil)))
         (tret (cond (target-tret (list 'pinta (list target-tret))) (t nil)))

         (desti      (agent-xyz999-desti-bolla id coord mem ronda dir-idx))
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

         ;; 1. Actualitzar memòria amb la visió
         (mem-vis  (agent-xyz999-actualitza-mem vis mem-old equip))

         ;; 2. Guardar posició a l'historial (només bolles)
         (mem-path (cond ((eq tipus 'bolla)
                          (agent-xyz999-update-unit-path id coord mem-vis))
                         (t mem-vis)))

         ;; 3. CANVI v3: detectar bloqueig i rotar direcció si cal
         ;;    Obtenim l'historial actualitzat d'aquesta unitat
         (unit-path (let* ((paths (agent-xyz999-get 'unit-paths mem-path)))
                      (cond (paths (agent-xyz999-get id paths)) (t nil))))
         (stuck     (agent-xyz999-is-stuck unit-path))
         (cur-dir   (agent-xyz999-get-dir id mem-path))
         ;;    Si stuck, rotar +1 (fins a 7 rotacions possibles per sortir)
         (new-dir   (cond (stuck (rem (+ cur-dir 1) 8)) (t cur-dir)))
         ;;    Guardar nova direcció a memòria
         (mem-nova  (cond (stuck (agent-xyz999-set-dir id new-dir mem-path))
                          (t mem-path)))

         ;; 4. Decisió
         (accions  (cond ((eq tipus 'base)
                          (agent-xyz999-decisio-base coord vis mem-nova pintura ronda id))
                         ((eq tipus 'bolla)
                          (agent-xyz999-decisio-bolla coord equip tr-pintar tr-moure
                                                      vis mem-nova id ronda new-dir))
                         (t nil))))

    (append (list (list 'escriu-memoria (list mem-nova))) accions)))