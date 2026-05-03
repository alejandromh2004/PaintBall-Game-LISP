;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: Alejandro Martinez Hermosa, Javier Vivo Samaniego.
;; Professor: Miquel Cabot.
;; Lliurament: primera convocatòria.
;; Agent intel·ligent del programa equip 1 (Negre).

;; ======================================================================
;; SECCIÓ 1 – FUNCIONS AUXILIARS DE L'AGENT
;; ======================================================================

;; Retorna el valor absolut d'un nombre
(defun agent-jvs328-abs (n)
  (cond ((null n) 0) ((< n 0) (- n)) (t n)))

;; Calcula la distància euclidiana al quadrat entre dues coordenades
(defun agent-jvs328-dist-q (c1 c2)
  (cond ((or (null c1) (null c2)) 100000)
        (t (+ (* (- (car c1) (car c2)) (- (car c1) (car c2)))
              (* (- (cadr c1) (cadr c2)) (- (cadr c1) (cadr c2)))))))

;; Retorna la longitud d'una llista
(defun agent-jvs328-longitud (lst)
  (cond ((null lst) 0)
        (t (+ 1 (agent-jvs328-longitud (cdr lst))))))

;; Retorna l'element n-èssim d'una llista
(defun agent-jvs328-nth (n lst)
  (cond ((null lst) nil)
        ((= n 0) (car lst))
        (t (agent-jvs328-nth (- n 1) (cdr lst)))))

;; Comprova si un element pertany a una llista
(defun agent-jvs328-membre (elem lst)
  (cond ((null lst) nil)
        ((equal (car lst) elem) t)
        (t (agent-jvs328-membre elem (cdr lst)))))

;; Elimina totes les aparicions d'un element en una llista
(defun agent-jvs328-elimina (elem lst)
  (cond ((null lst) nil)
        ((equal (car lst) elem) (agent-jvs328-elimina elem (cdr lst)))
        (t (cons (car lst) (agent-jvs328-elimina elem (cdr lst))))))

;; Retorna els primers n elements d'una llista
(defun agent-jvs328-take (n lst)
  (cond ((or (<= n 0) (null lst)) nil)
        (t (cons (car lst) (agent-jvs328-take (- n 1) (cdr lst))))))

;; ======================================================================
;; SECCIÓ 2 – GESTIÓ DE LA MEMÒRIA COMPARTIDA
;; ======================================================================

;; Obté el valor associat a una clau en l'A-list de memòria
(defun agent-jvs328-get (clau mem)
  (cond ((null mem) nil)
        ((eq (caar mem) clau) (cdar mem))
        (t (agent-jvs328-get clau (cdr mem)))))

;; Estableix o actualitza un valor en l'A-list de memòria
(defun agent-jvs328-set (clau val mem)
  (cond ((null mem) (list (cons clau val)))
        ((eq (caar mem) clau) (cons (cons clau val) (cdr mem)))
        (t (cons (car mem) (agent-jvs328-set clau val (cdr mem))))))

;; Actualitza l'historial de posicions (màxim 6) per a la detecció de bloquejos
(defun agent-jvs328-update-unit-path (id coord mem)
  (let* ((paths    (agent-jvs328-get 'unit-paths mem))
         (path-act (agent-jvs328-get id paths))
         (nou-path (agent-jvs328-take 6 (cons coord path-act)))
         (nous-paths (agent-jvs328-set id nou-path paths)))
    (agent-jvs328-set 'unit-paths nous-paths mem)))

;; ======================================================================
;; SECCIÓ 3 – DETECCIÓ DE BLOQUEIG I GESTIÓ DE DIRECCIONS
;; ======================================================================

;; Determina si l'agent està atrapat o "rebotant" en una zona petita
(defun agent-jvs328-is-stuck (unit-path)
  (cond ((< (agent-jvs328-longitud unit-path) 4) nil)
        (t (let* ((p0 (car unit-path))
                  (p1 (agent-jvs328-nth 1 unit-path))
                  (p2 (agent-jvs328-nth 2 unit-path))
                  (p3 (agent-jvs328-nth 3 unit-path)))
             (cond ((or (null p0) (null p1) (null p2) (null p3)) nil)
                   (t (and (< (agent-jvs328-dist-q p0 p1) 10)
                           (< (agent-jvs328-dist-q p0 p2) 10)
                           (< (agent-jvs328-dist-q p0 p3) 10))))))))

;; Recupera la direcció d'exploració preferida per a una unitat
(defun agent-jvs328-get-dir (id mem)
  (let* ((dirs (agent-jvs328-get 'unit-dirs mem))
         (d    (cond (dirs (agent-jvs328-get id dirs)) (t nil))))
    (cond ((null d) (rem (agent-jvs328-abs id) 8))
          (t d))))

;; Desa la direcció actualitzada de la unitat
(defun agent-jvs328-set-dir (id dir mem)
  (let* ((dirs     (agent-jvs328-get 'unit-dirs mem))
         (safe-dirs (cond (dirs dirs) (t nil)))
         (new-dirs (agent-jvs328-set id dir safe-dirs)))
    (agent-jvs328-set 'unit-dirs new-dirs mem)))

;; ======================================================================
;; SECCIÓ 4 – ACCESSORS PER A LA VISIÓ
;; ======================================================================

(defun agent-jvs328-c-coord  (c) (nth 0 c)) ;; Coordenada (x y)
(defun agent-jvs328-c-tipus  (c) (nth 1 c)) ;; Tipus (terra/aigua)
(defun agent-jvs328-c-color  (c) (nth 2 c)) ;; Color de la casella
(defun agent-jvs328-c-elem   (c) (nth 3 c)) ;; Element (base/bolla/lab)
(defun agent-jvs328-c-equip  (c) (nth 4 c)) ;; Equip propietari
(defun agent-jvs328-c-colors (c) (nth 5 c)) ;; Llista de colors de pintura

;; ======================================================================
;; SECCIÓ 5 – ACTUALITZACIÓ DE LA MEMÒRIA (VISIÓ)
;; ======================================================================

;; Sincronitza el coneixement compartit basat en el que veu la unitat actual
(defun agent-jvs328-actualitza-mem (vis mem equip)
  (cond
    ((null vis) mem)
    (t (let* ((c      (car vis))
              (coord  (agent-jvs328-c-coord c))
              (elem   (agent-jvs328-c-elem c))
              (eq-c   (agent-jvs328-c-equip c))
              (colors (agent-jvs328-c-colors c))

              ;; Localització de bases
              (m1 (cond ((and (eq elem 'base) (not (eq eq-c equip)))
                         (agent-jvs328-set 'colors-base-enemy colors
                           (agent-jvs328-set 'base-enemy coord mem)))
                        ((and (eq elem 'base) (eq eq-c equip))
                         (agent-jvs328-set 'base-ally coord mem))
                        (t mem)))

              ;; Manteniment de la llista de laboratoris (limitat a 15 per eficiència)
              (labs (agent-jvs328-get 'labs m1))
              (m2 (cond ((and (eq elem 'lab) (not (eq eq-c equip)))
                         (cond ((not (agent-jvs328-membre coord labs))
                                (agent-jvs328-set 'labs
                                  (agent-jvs328-take 15 (cons coord labs)) m1))
                               (t m1)))
                        ((and (eq elem 'lab) (eq eq-c equip))
                         (agent-jvs328-set 'labs (agent-jvs328-elimina coord labs) m1))
                        (t m1))))
       (agent-jvs328-actualitza-mem (cdr vis) m2 equip)))))

;; ======================================================================
;; SECCIÓ 6 – LÒGICA DE TRET I COLORS
;; ======================================================================

;; Retorna els colors necessaris per completar la combinació RGB en un objectiu
(defun agent-jvs328-filtra-colors (tots pintats)
  (cond ((null tots) nil)
        ((agent-jvs328-membre (car tots) pintats)
         (agent-jvs328-filtra-colors (cdr tots) pintats))
        (t (cons (car tots) (agent-jvs328-filtra-colors (cdr tots) pintats)))))

;; Puntua objectius: base enemiga > bolles enemigues > laboratoris
(defun agent-jvs328-puntua-tret (c equip coord)
  (let* ((elem (agent-jvs328-c-elem c))
         (eq-c (agent-jvs328-c-equip c))
         (dist (agent-jvs328-dist-q coord (agent-jvs328-c-coord c))))
    (cond ((> dist 5) -1)
          ((eq eq-c equip) -1)
          ((eq elem 'base) 1000)
          ((eq elem 'bolla) 500)
          ((eq elem 'lab) 100)
          (t -1))))

;; Cerca la millor opció de tret dins el rang de visió
(defun agent-jvs328-millor-tret (vis equip coord best-coord best-score)
  (cond ((null vis) best-coord)
        (t (let* ((c     (car vis))
                  (score (agent-jvs328-puntua-tret c equip coord)))
             (cond ((> score best-score)
                    (agent-jvs328-millor-tret (cdr vis) equip coord (agent-jvs328-c-coord c) score))
                   (t
                    (agent-jvs328-millor-tret (cdr vis) equip coord best-coord best-score)))))))

;; ======================================================================
;; SECCIÓ 7 – ESTRATÈGIA ANTI-RAMADA
;; ======================================================================

;; Recull fins a 6 aliats visibles per evitar col·lisions i aglomeracions
(defun agent-jvs328-allies-vis-acc (vis equip n)
  (cond ((or (null vis) (<= n 0)) nil)
        (t (let* ((c    (car vis))
                  (elem (agent-jvs328-c-elem c))
                  (eq-c (agent-jvs328-c-equip c)))
             (cond ((and (eq elem 'bolla) (eq eq-c equip))
                    (cons (agent-jvs328-c-coord c)
                          (agent-jvs328-allies-vis-acc (cdr vis) equip (- n 1))))
                   (t (agent-jvs328-allies-vis-acc (cdr vis) equip n)))))))

(defun agent-jvs328-allies-vis (vis equip)
  (agent-jvs328-allies-vis-acc vis equip 6))

;; Aplica penalitzacions de cost segons la proximitat a altres aliats
(defun agent-jvs328-penalty-ramada (co allies)
  (cond ((null allies) 0)
        (t (let ((dist (agent-jvs328-dist-q co (car allies))))
             (+ (cond ((= dist 0) 2000)
                      ((< dist 3) 600)
                      ((< dist 8) 200)
                      ((< dist 18) 60)
                      (t 0))
                (agent-jvs328-penalty-ramada co (cdr allies)))))))

;; ======================================================================
;; SECCIÓ 8 – NAVEGACIÓ I COST DE MOVIMENT
;; ======================================================================

;; Penalització graduada per a les darreres posicions visitades
(defun agent-jvs328-tabu-penalty (co unit-path idx)
  (cond ((null unit-path) 0)
        ((equal co (car unit-path))
         (cond ((= idx 0) 600)
               ((= idx 1) 250)
               ((= idx 2) 80)
               ((= idx 3) 30)
               (t 10)))
        (t (agent-jvs328-tabu-penalty co (cdr unit-path) (+ idx 1)))))

;; Llista de caselles lliures adjacents
(defun agent-jvs328-movibles (vis coord)
  (cond ((null vis) nil)
        (t (let* ((c  (car vis))
                  (co (agent-jvs328-c-coord c)))
             (cond ((and (eq (agent-jvs328-c-tipus c) 'terra)
                         (null (agent-jvs328-c-elem c))
                         (<= (agent-jvs328-dist-q co coord) 2)
                         (not (equal co coord)))
                    (cons co (agent-jvs328-movibles (cdr vis) coord)))
                   (t (agent-jvs328-movibles (cdr vis) coord)))))))

;; Calcula el cost total d'un moviment (distància + tabú + ramada)
(defun agent-jvs328-cost-mov (co desti unit-path allies)
  (let* ((dist   (agent-jvs328-dist-q co desti))
         (tabu   (agent-jvs328-tabu-penalty co unit-path 0))
         (ramada (agent-jvs328-penalty-ramada co allies)))
    (+ dist tabu ramada)))

;; Selecciona la casella amb menor cost per apropar-se a l'objectiu
(defun agent-jvs328-millor-mov (movibles desti unit-path allies best-coord best-cost)
  (cond ((null movibles) best-coord)
        (t (let* ((co   (car movibles))
                  (cost (agent-jvs328-cost-mov co desti unit-path allies)))
             (cond ((< cost best-cost)
                    (agent-jvs328-millor-mov (cdr movibles) desti unit-path allies co cost))
                   (t
                    (agent-jvs328-millor-mov (cdr movibles) desti unit-path allies best-coord best-cost)))))))

;; ======================================================================
;; SECCIÓ 9 – ESTRATÈGIA DE DESTINACIONS I ROLS
;; ======================================================================

;; Direccions d'exploració de gran abast
(defun agent-jvs328-dir-vec (idx)
  (cond ((= idx 0) '(25  0))
        ((= idx 1) '(18  18))
        ((= idx 2) '(0   25))
        ((= idx 3) '(-18 18))
        ((= idx 4) '(-25 0))
        ((= idx 5) '(-18 -18))
        ((= idx 6) '(0   -25))
        ((= idx 7) '(18  -18))
        (t         '(25  0))))

;; Cerca el laboratori conegut més proper
(defun agent-jvs328-closest-lab (labs coord best-lab best-dist)
  (cond ((null labs) best-lab)
        (t (let ((dist (agent-jvs328-dist-q (car labs) coord)))
             (cond ((< dist best-dist)
                    (agent-jvs328-closest-lab (cdr labs) coord (car labs) dist))
                   (t (agent-jvs328-closest-lab (cdr labs) coord best-lab best-dist)))))))

;; Calcula el punt destí segons l'estat del joc i el rol de la unitat
(defun agent-jvs328-desti-bolla (id coord mem ronda dir-idx)
  (let* ((base-enemy (agent-jvs328-get 'base-enemy mem))
         (base-ally  (agent-jvs328-get 'base-ally mem))
         (labs       (agent-jvs328-get 'labs mem))
         (abs-id     (agent-jvs328-abs id))

         ;; Direcció base per a l'exploració
         (d        (agent-jvs328-dir-vec dir-idx))
         (dest-exp (list (+ (car coord)  (car d))
                         (+ (cadr coord) (cadr d))))

         ;; Patrulla de seguretat al voltant de la base pròpia
         (dirs-pat '((8 0) (6 6) (0 8) (-6 6) (-8 0) (-6 -6) (0 -8) (6 -6)))
         (d-pat    (agent-jvs328-nth (rem (agent-jvs328-abs ronda) 8) dirs-pat))
         (dest-pat (cond (base-ally
                          (list (+ (car base-ally)  (car d-pat))
                                (+ (cadr base-ally) (cadr d-pat))))
                         (t dest-exp)))

         (rol         (rem abs-id 3))
         (is-attacker (< rol 2))
         (is-defender (= (rem abs-id 6) 5)))

    (cond
      ;; 1. Captura de laboratoris (prioritat per a l'economia)
      ((and labs (> (agent-jvs328-longitud labs) 0))
       (agent-jvs328-closest-lab labs coord nil 100000))
      ;; 2. Atac directe a la base enemiga
      ((and is-attacker base-enemy)
       base-enemy)
      ;; 3. Mantenir posició defensiva
      (is-defender dest-pat)
      ;; 4. Continuar l'exploració del mapa
      (t dest-exp))))

;; ======================================================================
;; SECCIÓ 10 – GESTOR DE COMPORTAMENT (BASE I BOLLA)
;; ======================================================================

(defun agent-jvs328-decisio-bolla (coord equip tr-pintar tr-moure vis mem id ronda dir-idx)
  (let* ((tp (cond (tr-pintar tr-pintar) (t 0)))
         (tm (cond (tr-moure tr-moure) (t 0)))

         (paths     (agent-jvs328-get 'unit-paths mem))
         (unit-path (agent-jvs328-get id paths))
         (allies (agent-jvs328-allies-vis vis equip))

         ;; Acció de tret
         (target-tret (cond ((< tp 1)
                             (agent-jvs328-millor-tret vis equip coord nil -1))
                            (t nil)))
         (tret (cond (target-tret (list 'pinta (list target-tret))) (t nil)))

         ;; Acció de moviment
         (desti      (agent-jvs328-desti-bolla id coord mem ronda dir-idx))
         (movs       (agent-jvs328-movibles vis coord))
         (target-mov (cond ((< tm 1)
                             (agent-jvs328-millor-mov movs desti unit-path allies nil 1000000))
                            (t nil)))
         (mou (cond (target-mov (list 'mou (list target-mov))) (t nil))))

    (append (cond (tret (list tret)) (t nil))
            (cond (mou  (list mou))  (t nil)))))

(defun agent-jvs328-decisio-base (coord vis mem pintura ronda id)
  (cond
    ((< pintura 50) nil)
    (t (let* ((colors-enemy (agent-jvs328-get 'colors-base-enemy mem))
              (utils  (agent-jvs328-filtra-colors '(r g b) colors-enemy))
              (colors (cond ((null utils) '(r g b)) (t utils)))
              (color  (agent-jvs328-nth (rem (agent-jvs328-abs ronda)
                                             (agent-jvs328-longitud colors)) colors))
              (movs       (agent-jvs328-movibles vis coord))
              (base-enemy (agent-jvs328-get 'base-enemy mem))
              (desti (cond (base-enemy base-enemy)
                           (t (list (+ (car coord) 10) (+ (cadr coord) 10)))))
              (spawn (agent-jvs328-millor-mov movs desti nil nil nil 1000000)))

         (cond ((and spawn color) (list (list 'crea-bolla (list color spawn))))
               (t nil))))))

;; ======================================================================
;; SECCIÓ 11 – PUNT D'ENTRADA DE L'AGENT
;; ======================================================================

(defun agent-jvs328 (dades)
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

         ;; 1. Integració de la nova visió a la memòria de l'equip
         (mem-vis  (agent-jvs328-actualitza-mem vis mem-old equip))

         ;; 2. Actualització del rastre de moviment
         (mem-path (cond ((eq tipus 'bolla)
                          (agent-jvs328-update-unit-path id coord mem-vis))
                         (t mem-vis)))

         ;; 3. Detecció de bloquejos i recalibratge de la direcció
         (unit-path (let* ((paths (agent-jvs328-get 'unit-paths mem-path)))
                      (cond (paths (agent-jvs328-get id paths)) (t nil))))
         (stuck     (agent-jvs328-is-stuck unit-path))
         (cur-dir   (agent-jvs328-get-dir id mem-path))
         (new-dir   (cond (stuck (rem (+ cur-dir 1) 8)) (t cur-dir)))
         (mem-nova  (cond (stuck (agent-jvs328-set-dir id new-dir mem-path))
                          (t mem-path)))

         ;; 4. Determinació de les millors accions a realitzar
         (accions  (cond ((eq tipus 'base)
                          (agent-jvs328-decisio-base coord vis mem-nova pintura ronda id))
                         ((eq tipus 'bolla)
                          (agent-jvs328-decisio-bolla coord equip tr-pintar tr-moure
                                                      vis mem-nova id ronda new-dir))
                         (t nil))))

    ;; 5. Retorn de la nova memòria i el conjunt d'accions
    (append (list (list 'escriu-memoria (list mem-nova))) accions)))
