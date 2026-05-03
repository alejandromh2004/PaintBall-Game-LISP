;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: Alejandro Martinez Hermosa, Javier Vivo Samaniego.
;; Professor: Miquel Cabot.
;; Lliurament: primera convocatòria.
;; Agent intel·ligent del programa equip 2 (Blanc).

;; ======================================================================
;; SECCIÓ 1 – FUNCIONS AUXILIARS DE L'AGENT
;; ======================================================================

;; Retorna el valor absolut d'un nombre
(defun agent-amh976-abs (n)
  (cond ((null n) 0) ((< n 0) (- n)) (t n)))

;; Calcula la distància euclidiana al quadrat entre dues coordenades
(defun agent-amh976-dist-q (c1 c2)
  (cond ((or (null c1) (null c2)) 100000)
        (t (+ (* (- (car c1) (car c2)) (- (car c1) (car c2)))
              (* (- (cadr c1) (cadr c2)) (- (cadr c1) (cadr c2)))))))

;; Retorna la longitud d'una llista
(defun agent-amh976-longitud (lst)
  (cond ((null lst) 0)
        (t (+ 1 (agent-amh976-longitud (cdr lst))))))

;; Retorna l'element n-èssim d'una llista
(defun agent-amh976-nth (n lst)
  (cond ((null lst) nil)
        ((= n 0) (car lst))
        (t (agent-amh976-nth (- n 1) (cdr lst)))))

;; Comprova si un element pertany a una llista
(defun agent-amh976-membre (elem lst)
  (cond ((null lst) nil)
        ((equal (car lst) elem) t)
        (t (agent-amh976-membre elem (cdr lst)))))

;; Elimina totes les aparicions d'un element en una llista
(defun agent-amh976-elimina (elem lst)
  (cond ((null lst) nil)
        ((equal (car lst) elem) (agent-amh976-elimina elem (cdr lst)))
        (t (cons (car lst) (agent-amh976-elimina elem (cdr lst))))))

;; Retorna els primers n elements d'una llista
(defun agent-amh976-take (n lst)
  (cond ((or (<= n 0) (null lst)) nil)
        (t (cons (car lst) (agent-amh976-take (- n 1) (cdr lst))))))

;; ======================================================================
;; SECCIÓ 2 – GESTIÓ DE LA MEMÒRIA COMPARTIDA
;; ======================================================================

;; Obté el valor associat a una clau en l'A-list de memòria
(defun agent-amh976-get (clau mem)
  (cond ((null mem) nil)
        ((eq (caar mem) clau) (cdar mem))
        (t (agent-amh976-get clau (cdr mem)))))

;; Estableix o actualitza un valor en l'A-list de memòria
(defun agent-amh976-set (clau val mem)
  (cond ((null mem) (list (cons clau val)))
        ((eq (caar mem) clau) (cons (cons clau val) (cdr mem)))
        (t (cons (car mem) (agent-amh976-set clau val (cdr mem))))))

;; Actualitza l'historial de posicions d'una unitat per evitar bucles infinits
(defun agent-amh976-update-unit-path (id coord mem)
  (let* ((paths (agent-amh976-get 'unit-paths mem))
         (path-actual (agent-amh976-get id paths))
         (nou-path (agent-amh976-take 4 (cons coord path-actual)))
         (nous-paths (agent-amh976-set id nou-path paths)))
    (agent-amh976-set 'unit-paths nous-paths mem)))

;; ======================================================================
;; SECCIÓ 3 – ACCÉS A LES DADES DE VISIÓ
;; ======================================================================

(defun agent-amh976-c-coord  (c) (nth 0 c)) ;; Coordenada (x y)
(defun agent-amh976-c-tipus  (c) (nth 1 c)) ;; Tipus (terra/aigua)
(defun agent-amh976-c-color  (c) (nth 2 c)) ;; Color de la casella
(defun agent-amh976-c-elem   (c) (nth 3 c)) ;; Element (base/bolla/lab)
(defun agent-amh976-c-equip  (c) (nth 4 c)) ;; Equip propietari
(defun agent-amh976-c-colors (c) (nth 5 c)) ;; Llista de colors de pintura

;; ======================================================================
;; SECCIÓ 4 – ACTUALITZACIÓ DE LA MEMÒRIA (VISIÓ)
;; ======================================================================

;; Analitza les caselles visibles i actualitza la base de dades interna de l'equip
(defun agent-amh976-actualitza-mem (vis mem equip)
  (cond
    ((null vis) mem)
    (t (let* ((c     (car vis))
              (coord (agent-amh976-c-coord c))
              (elem  (agent-amh976-c-elem c))
              (eq-c  (agent-amh976-c-equip c))
              (colors (agent-amh976-c-colors c))
              
              ;; Identificació de bases aliades i enemigues
              (m1 (cond ((and (eq elem 'base) (not (eq eq-c equip)))
                         (agent-amh976-set 'colors-base-enemy colors 
                                            (agent-amh976-set 'base-enemy coord mem)))
                        ((and (eq elem 'base) (eq eq-c equip))
                         (agent-amh976-set 'base-ally coord mem))
                        (t mem)))
              
              ;; Seguiment de laboratoris pendents de capturar
              (labs (agent-amh976-get 'labs m1))
              (m2 (cond ((and (eq elem 'lab) (not (eq eq-c equip)))
                         (cond ((not (agent-amh976-membre coord labs))
                                (agent-amh976-set 'labs (cons coord labs) m1))
                               (t m1)))
                        ((and (eq elem 'lab) (eq eq-c equip))
                         (agent-amh976-set 'labs (agent-amh976-elimina coord labs) m1))
                        (t m1))))
         (agent-amh976-actualitza-mem (cdr vis) m2 equip)))))

;; ======================================================================
;; SECCIÓ 5 – LÒGICA DE COLORS I TRET
;; ======================================================================

;; Filtra els colors que falten per explotar un objectiu
(defun agent-amh976-filtra-colors (tots pintats)
  (cond ((null tots) nil)
        ((agent-amh976-membre (car tots) pintats)
         (agent-amh976-filtra-colors (cdr tots) pintats))
        (t (cons (car tots) (agent-amh976-filtra-colors (cdr tots) pintats)))))

;; Puntua un possible objectiu de tret segons prioritat i distància
(defun agent-amh976-puntua-tret (c equip coord)
  (let* ((elem (agent-amh976-c-elem c))
         (eq-c (agent-amh976-c-equip c))
         (dist (agent-amh976-dist-q coord (agent-amh976-c-coord c))))
    (cond ((> dist 5) -1)
          ((eq eq-c equip) -1)
          ((eq elem 'base) 1000)
          ((eq elem 'bolla) 500)
          ((eq elem 'lab) 100)
          (t -1))))

;; Selecciona la millor coordenada per disparar pintura
(defun agent-amh976-millor-tret (vis equip coord best-coord best-score)
  (cond ((null vis) best-coord)
        (t (let* ((c (car vis))
                  (score (agent-amh976-puntua-tret c equip coord)))
             (cond ((> score best-score)
                    (agent-amh976-millor-tret (cdr vis) equip coord (agent-amh976-c-coord c) score))
                   (t (agent-amh976-millor-tret (cdr vis) equip coord best-coord best-score)))))))

;; ======================================================================
;; SECCIÓ 6 – NAVEGACIÓ I EVITACIÓ D'OBSTACLES
;; ======================================================================

;; Filtra les caselles adjacents on la unitat es pot moure
(defun agent-amh976-movibles (vis coord)
  (cond ((null vis) nil)
        (t (let* ((c  (car vis))
                  (co (agent-amh976-c-coord c)))
             (cond ((and (eq (agent-amh976-c-tipus c) 'terra)
                         (null (agent-amh976-c-elem c))
                         (<= (agent-amh976-dist-q co coord) 2)
                         (not (equal co coord)))
                    (cons co (agent-amh976-movibles (cdr vis) coord)))
                   (t (agent-amh976-movibles (cdr vis) coord)))))))

;; Calcula el cost de moviment, penalitzant fortament les posicions recents (tabú)
(defun agent-amh976-cost-mov (co desti unit-path)
  (let ((dist (agent-amh976-dist-q co desti))
        (tabu (cond ((agent-amh976-membre co unit-path) 10000) (t 0))))
    (+ dist tabu)))

;; Cerca el moviment òptim cap a una destinació
(defun agent-amh976-millor-mov (movibles desti unit-path best-coord best-cost)
  (cond ((null movibles) best-coord)
        (t (let* ((co (car movibles))
                  (cost (agent-amh976-cost-mov co desti unit-path)))
             (cond ((< cost best-cost)
                    (agent-amh976-millor-mov (cdr movibles) desti unit-path co cost))
                   (t (agent-amh976-millor-mov (cdr movibles) desti unit-path best-coord best-cost)))))))

;; ======================================================================
;; SECCIÓ 7 – ROLS I ESTRATÈGIA DE DESTINACIONS
;; ======================================================================

;; Troba el laboratori més proper d'una llista
(defun agent-amh976-closest-lab (labs coord best-lab best-dist)
  (cond ((null labs) best-lab)
        (t (let ((dist (agent-amh976-dist-q (car labs) coord)))
             (cond ((< dist best-dist)
                    (agent-amh976-closest-lab (cdr labs) coord (car labs) dist))
                   (t (agent-amh976-closest-lab (cdr labs) coord best-lab best-dist)))))))

;; Decideix a on s'ha de dirigir la unitat segons el seu rol (Atacant/Defensor/Explorador)
(defun agent-amh976-desti-bolla (id coord mem ronda)
  (let* ((base-enemy (agent-amh976-get 'base-enemy mem))
         (base-ally (agent-amh976-get 'base-ally mem))
         (labs (agent-amh976-get 'labs mem))
         (abs-id (agent-amh976-abs id))
         (is-attacker (< (rem abs-id 3) 2)) ; Rol d'atac
         (is-defender (= (rem abs-id 6) 5))  ; Rol de defensa
         
         ;; Destinació per defecte (Exploració)
         (dirs-exp '((30 0) (20 20) (0 30) (-20 20) (-30 0) (-20 -20) (0 -30) (20 -20)))
         (d-exp (agent-amh976-nth (rem abs-id 8) dirs-exp))
         (bx (cond ((and base-ally (car base-ally)) (car base-ally)) (t 500)))
         (by (cond ((and base-ally (cadr base-ally)) (cadr base-ally)) (t 500)))
         (dest-exp (list (+ bx (car d-exp)) (+ by (cadr d-exp))))
         
         ;; Destinació de patrulla prop de la base aliada
         (dirs-pat '((5 0) (3 3) (0 5) (-3 3) (-5 0) (-3 -3) (0 -5) (3 -3)))
         (d-pat (agent-amh976-nth (rem (agent-amh976-abs ronda) 8) dirs-pat))
         (dest-pat (cond (base-ally (list (+ bx (car d-pat)) (+ by (cadr d-pat))))
                         (t dest-exp))))
         
    (cond
      ;; Prioritat 1: Capturar laboratoris si n'hi ha de visibles o coneguts
      ((and labs (> (agent-amh976-longitud labs) 0)) 
       (agent-amh976-closest-lab labs coord nil 100000))
      ;; Prioritat 2: Atacar base enemiga si es coneix la posició
      ((and is-attacker base-enemy) base-enemy)
      ;; Prioritat 3: Patrullar base aliada
      (is-defender dest-pat)
      ;; Per defecte: Explorar el mapa
      (t dest-exp))))

;; ======================================================================
;; SECCIÓ 8 – GESTOR DE DECISIONS (BASE I BOLLA)
;; ======================================================================

;; Lògica específica per a les unitats de tipus bolla
(defun agent-amh976-decisio-bolla (coord equip tr-pintar tr-moure vis mem id ronda)
  (let* ((tp (cond (tr-pintar tr-pintar) (t 0)))
         (tm (cond (tr-moure tr-moure) (t 0)))
         
         ;; Actualització d'historial
         (paths (agent-amh976-get 'unit-paths mem))
         (unit-path (agent-amh976-get id paths))
         
         ;; Decisió de tret
         (target-tret (cond ((< tp 1) (agent-amh976-millor-tret vis equip coord nil -1)) (t nil)))
         (tret (cond (target-tret (list 'pinta (list target-tret))) (t nil)))
         
         ;; Decisió de moviment
         (desti (agent-amh976-desti-bolla id coord mem ronda))
         (movs (agent-amh976-movibles vis coord))
         (target-mov (cond ((< tm 1) (agent-amh976-millor-mov movs desti unit-path nil 1000000)) (t nil)))
         (mou (cond (target-mov (list 'mou (list target-mov))) (t nil))))
         
    (append (cond (tret (list tret)) (t nil))
            (cond (mou (list mou)) (t nil)))))

;; Lògica específica per a la base (creació de noves bolles)
(defun agent-amh976-decisio-base (coord vis mem pintura ronda id)
  (cond
    ((< pintura 50) nil) ;; Cost de creació
    (t (let* ((colors-enemy (agent-amh976-get 'colors-base-enemy mem))
              (utils (agent-amh976-filtra-colors '(r g b) colors-enemy))
              (colors (cond ((null utils) '(r g b)) (t utils)))
              (color (agent-amh976-nth (rem (agent-amh976-abs ronda) (agent-amh976-longitud colors)) colors))
              
              (movs       (agent-amh976-movibles vis coord))
              (base-enemy (agent-amh976-get 'base-enemy mem))
              (desti (cond (base-enemy base-enemy) 
                           (t (list (+ (car coord) 10) (+ (cadr coord) 10)))))
              (spawn (agent-amh976-millor-mov movs desti nil nil 1000000)))
              
         (cond ((and spawn color) (list (list 'crea-bolla (list color spawn))))
               (t nil))))))

;; ======================================================================
;; SECCIÓ 9 – PUNT D'ENTRADA PRINCIPAL
;; ======================================================================

(defun agent-amh976 (dades)
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
         
         ;; 1. Sincronitzar coneixement de l'equip
         (mem-vis     (agent-amh976-actualitza-mem vis mem-old equip))
         
         ;; 2. Evitar retrocedir o quedar-se atrapat
         (mem-nova    (cond ((eq tipus 'bolla) 
                             (agent-amh976-update-unit-path id coord mem-vis))
                            (t mem-vis)))
         
         ;; 3. Executar lògica de comportament
         (accions     (cond
                        ((eq tipus 'base)
                         (agent-amh976-decisio-base coord vis mem-nova pintura ronda id))
                        ((eq tipus 'bolla)
                         (agent-amh976-decisio-bolla coord equip tr-pintar tr-moure vis mem-nova id ronda))
                        (t nil))))
    
    ;; 4. Retornar memòria actualitzada i llista d'accions
    (append (list (list 'escriu-memoria (list mem-nova))) accions)))
