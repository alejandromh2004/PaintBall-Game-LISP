;; ======================================================================
;; PRÀCTICA FINAL LLENGUATGES DE PROGRAMACIÓ - LISP - PAINTBALL
;; ======================================================================
;; Estudiant: XYZ999
;; Data: 27/04/2026
;; Assignatura: Llenguatges de Programació (LP)
;; Convocatòria: Primera Convocatòria (Ordinària)
;;
;; FITXER: agent-xyz999.lsp
;;
;; ESTRATÈGIA v3 ("Kill-First Strike"):
;;
;;   ROLS (id mod 3):
;;     0 → ATACANT:    Rush directe a base enemiga quan la coneix.
;;     1 → CHASSADOR:  Va al lab MÉS PROPER. Sense labs, ataca la base.
;;     2 → EXPLORADOR: Cobreix sectors. S'uneix a l'atac quan base visible.
;;
;;   NOVETATS v3 respecte v2:
;;     ✅ Kill-color override: si la base enemiga té 2 colors i soc el
;;        3r color, IGNORO el rol i rush directe a la base. Guanyem abans.
;;     ✅ Defensa activa: bonus de prioritat de tret (+300) per enemics
;;        dins dist²≤25 de la base aliada. Protegim la base.
;;     ✅ Spawn smart: la base passa l'id per un jitter consistent que evita
;;        col·lisions en els spawns consecutius.
;;     ✅ Cooldown nil segur a tots els llocs rellevants.
;;
;;   SISTEMA DE TRETS (prioritat descendent):
;;     9000 → Kill base (2 colors pintats + el nostre = destruïda!)
;;     5300 → 2n color base + bonus defensa activa
;;     5000 → 2n color base (sense bonus)
;;     2300 → 1r color base + bonus defensa activa  [rarament aplicable]
;;     2000 → 1r color base
;;     1100 → Kill shot bolla prop de base aliada
;;      800 → Kill shot bolla
;;      700 → Dany bolla prop de base aliada
;;      400 → Dany útil a bolla
;;      200 → Captura lab enemic/neutral
;;       -1 → No disparar
;;
;;   MEMÒRIA COMPARTIDA (~70 àtoms, molt per sota del límit de 100000):
;;     base-ally:          coord base pròpia
;;     base-enemy:         coord base enemiga (quan vista)
;;     colors-base-enemy:  llista colors pintats a la base enemiga
;;     enemy-last-seen:    última coord d'una bolla enemiga vista
;;     labs:               coords de labs enemics/neutrals (màx 15)
;;     labs-a:             coords de labs aliats (màx 10)
;;
;;   DISSENY FUNCIONAL: Sense reassignació ni mutació d'estructures.
;;   Tractament seqüencial exclusivament per recursió i funcions d'ordre superior.
;;   Totes les funcions prefixades amb "agent-xyz999-".
;;
;;   ÚS: (agent-xyz999 dades) - cridat pel controlador per cada unitat.
;; ======================================================================


;; ======================================================================
;; SECCIÓ 1: UTILITATS BÀSIQUES
;; ======================================================================

(defun agent-xyz999-dist-q (c1 c2)
  "Distància euclidiana al quadrat entre c1=(x y) i c2=(x y). Retorna 1000000 si nil."
  (cond ((or (null c1) (null c2)) 1000000)
        (t (let ((dx (- (car c1) (car c2)))
                 (dy (- (cadr c1) (cadr c2))))
             (+ (* dx dx) (* dy dy))))))

(defun agent-xyz999-abs (n)
  "Valor absolut. Retorna 0 si n és nil."
  (cond ((null n) 0) ((< n 0) (- n)) (t n)))

(defun agent-xyz999-longitud (lst)
  "Longitud de la llista lst."
  (cond ((null lst) 0) (t (+ 1 (agent-xyz999-longitud (cdr lst))))))

(defun agent-xyz999-nth-safe (n lst)
  "Element n-è (0-indexat) de lst, o nil si fora de rang."
  (cond ((null lst) nil)
        ((= n 0) (car lst))
        (t (agent-xyz999-nth-safe (- n 1) (cdr lst)))))

(defun agent-xyz999-membre (elem lst)
  "Cert si elem pertany a lst (comparació per equal)."
  (cond ((null lst) nil)
        ((equal (car lst) elem) t)
        (t (agent-xyz999-membre elem (cdr lst)))))

(defun agent-xyz999-elimina (elem lst)
  "Elimina totes les aparicions d'elem de lst (comparació per equal)."
  (cond ((null lst) nil)
        ((equal (car lst) elem) (agent-xyz999-elimina elem (cdr lst)))
        (t (cons (car lst) (agent-xyz999-elimina elem (cdr lst))))))


;; ======================================================================
;; SECCIÓ 2: MEMÒRIA COMPARTIDA (a-list pura, sense mutació)
;; ======================================================================

(defun agent-xyz999-get (clau mem)
  "Retorna el valor associat a clau en la a-list mem, o nil."
  (cond ((null mem) nil)
        ((eq (caar mem) clau) (cdar mem))
        (t (agent-xyz999-get clau (cdr mem)))))

(defun agent-xyz999-set (clau val mem)
  "Insereix o actualitza (clau . val) a la a-list mem (no muta)."
  (cond ((null mem) (list (cons clau val)))
        ((eq (caar mem) clau) (cons (cons clau val) (cdr mem)))
        (t (cons (car mem) (agent-xyz999-set clau val (cdr mem))))))

(defun agent-xyz999-afegir-coord (coord lst max-n)
  "Afegeix coord a lst si no hi és i la llista no supera max-n elements."
  (cond ((agent-xyz999-membre coord lst) lst)
        ((>= (agent-xyz999-longitud lst) max-n) lst)
        (t (cons coord lst))))


;; ======================================================================
;; SECCIÓ 3: ACCESSORS DE CASELLA (índexs 0..8)
;;   0=coord 1=tipus 2=color-casella 3=tipus-elem 4=equip
;;   5=colors-pintat 6=color-propi 7=tr-pintar 8=tr-moure
;; ======================================================================

(defun agent-xyz999-c-coord  (c) (nth 0 c))
(defun agent-xyz999-c-tipus  (c) (nth 1 c))
(defun agent-xyz999-c-color  (c) (nth 2 c))
(defun agent-xyz999-c-elem   (c) (nth 3 c))
(defun agent-xyz999-c-equip  (c) (nth 4 c))
(defun agent-xyz999-c-colors (c) (nth 5 c))


;; ======================================================================
;; SECCIÓ 4: ACTUALITZACIÓ DE MEMÒRIA A PARTIR DE LA VISIÓ
;; ======================================================================

(defun agent-xyz999-actualitza-mem (vis mem equip)
  "Recorre la visió vis i actualitza mem (base-ally, base-enemy,
   colors-base-enemy, enemy-last-seen, labs, labs-a).
   vis: llista caselles. mem: a-list. equip: símbol equip propi."
  (cond
    ((null vis) mem)
    (t (let* ((c     (car vis))
              (coord (agent-xyz999-c-coord c))
              (elem  (agent-xyz999-c-elem c))
              (eq-c  (agent-xyz999-c-equip c))
              (cp    (agent-xyz999-c-colors c))

              ;; Actualitza bases i posicions enemigues
              (mem1  (cond
                       ((and (eq elem 'base) coord (not (eq eq-c equip)))
                        (agent-xyz999-set 'colors-base-enemy cp
                          (agent-xyz999-set 'base-enemy coord mem)))
                       ((and (eq elem 'bolla) coord (not (eq eq-c equip)))
                        (agent-xyz999-set 'enemy-last-seen coord mem))
                       ((and (eq elem 'base) coord (eq eq-c equip))
                        (agent-xyz999-set 'base-ally coord mem))
                       (t mem)))

              ;; Actualitza labs: aliat→labs-a, enemic/neutral→labs
              (mem2  (cond
                       ((and (eq elem 'lab) coord)
                        (cond
                          ((eq eq-c equip)
                           (agent-xyz999-set 'labs-a
                             (agent-xyz999-afegir-coord coord
                               (agent-xyz999-get 'labs-a mem1) 10)
                             (agent-xyz999-set 'labs
                               (agent-xyz999-elimina coord
                                 (agent-xyz999-get 'labs mem1))
                               mem1)))
                          (t
                           (agent-xyz999-set 'labs
                             (agent-xyz999-afegir-coord coord
                               (agent-xyz999-get 'labs mem1) 15)
                             (agent-xyz999-set 'labs-a
                               (agent-xyz999-elimina coord
                                 (agent-xyz999-get 'labs-a mem1))
                               mem1)))))
                       (t mem1))))
         (agent-xyz999-actualitza-mem (cdr vis) mem2 equip)))))


;; ======================================================================
;; SECCIÓ 5: LÒGICA DE COLORS
;; ======================================================================

(defun agent-xyz999-filtra-falten (tots pintats)
  "Retorna els colors de tots que NO estan a pintats."
  (cond ((null tots) nil)
        ((agent-xyz999-membre (car tots) pintats)
         (agent-xyz999-filtra-falten (cdr tots) pintats))
        (t (cons (car tots)
                 (agent-xyz999-filtra-falten (cdr tots) pintats)))))

(defun agent-xyz999-tria-color (colors-be ronda)
  "Tria el color òptim per crear:
   - 2 colors a base enemiga → SEMPRE el kill color.
   - 0-1 colors → rota equilibradament."
  (let* ((falten (agent-xyz999-filtra-falten '(r g b) colors-be))
         (n      (agent-xyz999-longitud falten)))
    (cond ((= n 0) (agent-xyz999-nth-safe (rem ronda 3) '(r g b)))
          ((= n 1) (car falten))
          (t (agent-xyz999-nth-safe (rem ronda n) falten)))))

(defun agent-xyz999-kill-color (colors-be)
  "Retorna el color que destruirà la base si ja té 2 colors; nil en cas contrari."
  (let ((falten (agent-xyz999-filtra-falten '(r g b) colors-be)))
    (cond ((= (agent-xyz999-longitud falten) 1) (car falten))
          (t nil))))


;; ======================================================================
;; SECCIÓ 6: ROLS I DESTINS
;; ======================================================================

(defun agent-xyz999-rol (id)
  "Assigna un rol: 0=atacant, 1=chassador, 2=explorador (id mod 3)."
  (rem (agent-xyz999-abs id) 3))

(defun agent-xyz999-lab-mes-proper (labs coord best-coord best-dist)
  "Retorna la coord del lab de labs més proper a coord (recursió de cua)."
  (cond ((null labs) best-coord)
        (t (let ((d (agent-xyz999-dist-q (car labs) coord)))
             (cond ((< d best-dist)
                    (agent-xyz999-lab-mes-proper (cdr labs) coord (car labs) d))
                   (t
                    (agent-xyz999-lab-mes-proper (cdr labs) coord best-coord best-dist)))))))

(defun agent-xyz999-vector-exploracio (id ronda base-ally)
  "Punt d'exploració en una direcció que canvia cada 15 rondes.
   16 sectors basats en id+fase per dispersar les unitats."
  (let* ((dirs '((1 0)(1 1)(0 1)(-1 1)(-1 0)(-1 -1)(0 -1)(1 -1)
                 (2 1)(1 2)(-1 2)(-2 1)(-2 -1)(-1 -2)(1 -2)(2 -1)))
         (fase  (truncate (/ ronda 15)))
         (idx   (rem (+ (agent-xyz999-abs id) fase) 16))
         (dir   (agent-xyz999-nth-safe idx dirs))
         (bx    (cond ((and base-ally (car base-ally)) (car base-ally)) (t 500)))
         (by    (cond ((and base-ally (cadr base-ally)) (cadr base-ally)) (t 500))))
    (cond ((null dir) (list bx by))
          (t (list (+ bx (* (car dir) 1000))
                   (+ by (* (cadr dir) 1000)))))))

(defun agent-xyz999-desti-bolla (mem id ronda coord color-propi)
  "Determina el destí de la bolla:
   OVERRIDE KILL: si soc el color que falta a la base enemiga (2 colors pintats),
     ignoro el rol i rush directe. Maximitza la velocitat de victòria.
   ROL 0 (atacant):   base-enemy directe, o explora.
   ROL 1 (chassador): lab més proper, o base-enemy, o explora.
   ROL 2 (explorador): base-enemy si visible, o explora.
   mem: a-list. id: enter. ronda: enter. coord: (x y). color-propi: símbol."
  (let* ((rol        (agent-xyz999-rol id))
         (base-enemy (agent-xyz999-get 'base-enemy mem))
         (colors-be  (agent-xyz999-get 'colors-base-enemy mem))
         (labs       (agent-xyz999-get 'labs mem))
         (base-ally  (agent-xyz999-get 'base-ally mem))
         (n-labs     (agent-xyz999-longitud labs))
         ;; Kill-color: el color que destruirà la base si ja té 2 colors
         (kc         (agent-xyz999-kill-color colors-be))
         (es-kill    (and kc base-enemy (eq color-propi kc))))
    (cond
      ;; KILL-COLOR OVERRIDE: rush immediat si puc destruir la base!
      (es-kill base-enemy)

      ;; ROL 0 - ATACANT: rush directe si coneix la base
      ((and (= rol 0) base-enemy) base-enemy)

      ;; ROL 1 - CHASSADOR: lab més proper per acumular pintura
      ((and (= rol 1) (> n-labs 0))
       (agent-xyz999-lab-mes-proper labs coord nil 1000000))

      ;; Si s'ha vist un enemic recentment, anar-hi a investigar
      ((agent-xyz999-get 'enemy-last-seen mem)
       (agent-xyz999-get 'enemy-last-seen mem))

      ;; ROL 1 sense labs: s'uneix a l'atac
      ((and (= rol 1) base-enemy) base-enemy)

      ;; ROL 2: s'uneix a l'atac si base visible
      ((and (= rol 2) base-enemy) base-enemy)

      ;; Tots: explorar en direcció dinàmica (canvia cada 15 rondes)
      (t (agent-xyz999-vector-exploracio id ronda base-ally)))))


;; ======================================================================
;; SECCIÓ 7: NAVEGACIÓ
;; ======================================================================

(defun agent-xyz999-movibles (vis coord)
  "Caselles de vis on la bolla es pot moure:
   terra buides, dins dist²≤2 de coord, i distintes de la posició actual."
  (cond
    ((null vis) nil)
    (t (let* ((c  (car vis))
              (co (agent-xyz999-c-coord c)))
         (cond ((and (eq (agent-xyz999-c-tipus c) 'terra)
                     (null (agent-xyz999-c-elem c))
                     (<= (agent-xyz999-dist-q co coord) 2)
                     (not (equal co coord)))
                (cons c (agent-xyz999-movibles (cdr vis) coord)))
               (t (agent-xyz999-movibles (cdr vis) coord)))))))

(defun agent-xyz999-cost-mov (casella desti color-propi)
  "Cost d'un moviment a casella cap a desti.
   Penalitza lleugerament caselles de color aliè (+5) per preferir
   caselles del color propi i estalviar el triple cooldown del joc.
   Prou petit per no bloquejar l'exploració."
  (let ((coord (agent-xyz999-c-coord casella)))
    (cond ((or (null coord) (null desti)) 1000000)
          (t (+ (* (agent-xyz999-dist-q coord desti) 10)
                (cond ((eq (agent-xyz999-c-color casella) color-propi) 0)
                      (t 5)))))))

(defun agent-xyz999-millor-mov (movibles desti color-propi best-coord best-cost)
  "Cerca greedy la casella movible de menor cost cap a desti (recursió de cua).
   Retorna la coordenada (x y) de la millor casella, o nil si cap."
  (cond
    ((null movibles) best-coord)
    (t (let ((cost (agent-xyz999-cost-mov (car movibles) desti color-propi)))
         (cond ((< cost best-cost)
                (agent-xyz999-millor-mov (cdr movibles) desti color-propi
                                         (agent-xyz999-c-coord (car movibles)) cost))
               (t
                (agent-xyz999-millor-mov (cdr movibles) desti color-propi
                                         best-coord best-cost)))))))


;; ======================================================================
;; SECCIÓ 8: SISTEMA DE COMBAT
;; ======================================================================

(defun agent-xyz999-prioritat-tret (casella equip color-propi dist base-ally)
  "Calcula la prioritat d'un tret a la casella des de dist.
   Inclou bonus de +300 per objectius prop de la base aliada (defensa activa).
   Retorna -1 si no s'ha de disparar.
   casella: casella visió. equip: símbol. color-propi: símbol.
   dist: dist² a l'objectiu. base-ally: coord base pròpia o nil."
  (let* ((elem   (agent-xyz999-c-elem casella))
         (eq-c   (agent-xyz999-c-equip casella))
         (cp     (agent-xyz999-c-colors casella))
         (n-cp   (agent-xyz999-longitud cp))
         ;; Bonus defensa: enemic dins dist²≤25 de la nostra base (5 caselles)
         (d-ba   (agent-xyz999-dist-q (agent-xyz999-c-coord casella) base-ally))
         (bonus  (cond ((and (< d-ba 25) (not (eq eq-c equip))) 300) (t 0))))
    (cond
      ;; Filtres bàsics: fora rang, no terra, buida, aliat
      ((> dist 5) -1)
      ((not (eq (agent-xyz999-c-tipus casella) 'terra)) -1)
      ((null elem) -1)
      ((eq eq-c equip) -1)

      ;; BASE ENEMIGA: objectiu suprem
      ((eq elem 'base)
       (cond
         ((agent-xyz999-membre color-propi cp) -1)  ; ja té el nostre color
         ((= n-cp 2) 9000)                          ; kill shot!
         ((= n-cp 1) (+ 5000 bonus))                ; 2n color
         (t (+ 2000 bonus))))                       ; 1r color

      ;; BOLLA ENEMIGA
      ((eq elem 'bolla)
       (cond
         ((agent-xyz999-membre color-propi cp) -1)  ; ja té el nostre color
         ((= n-cp 2) (+ 800 bonus))                 ; kill shot bolla!
         (t (+ 400 bonus))))                        ; dany útil

      ;; LAB enemic o no capturat
      ((eq elem 'lab)
       (cond ((not (eq eq-c equip)) 200) (t -1)))

      (t -1))))

(defun agent-xyz999-millor-tret (vis equip color-propi coord base-ally best-coord best-punt)
  "Cerca el millor objectiu per disparar en rang 5u² a la visió vis.
   Retorna la coord de l'objectiu o nil.
   Recursió de cua sobre vis."
  (cond
    ((null vis) best-coord)
    (t (let* ((c    (car vis))
              (dist (agent-xyz999-dist-q coord (agent-xyz999-c-coord c)))
              (p    (agent-xyz999-prioritat-tret c equip color-propi dist base-ally)))
         (cond ((> p best-punt)
                (agent-xyz999-millor-tret (cdr vis) equip color-propi coord
                                          base-ally (agent-xyz999-c-coord c) p))
               (t
                (agent-xyz999-millor-tret (cdr vis) equip color-propi coord
                                          base-ally best-coord best-punt)))))))


;; ======================================================================
;; SECCIÓ 9: DECISIÓ DE LA BOLLA
;; ======================================================================

(defun agent-xyz999-decisio-bolla (coord equip color-propi tr-pintar tr-moure
                                    vis mem id ronda)
  "Cervell de la bolla: retorna la llista d'accions (tret i/o moviment).
   Tret i moviment són independents i es poden fer al mateix torn.
   coord: (x y). equip, color-propi: símbols.
   tr-pintar, tr-moure: cooldowns (nil tractat com 0 per bolles noves).
   vis: llista caselles visibles. mem: a-list. id: enter. ronda: enter."
  (let* ((tp         (cond (tr-pintar tr-pintar) (t 0)))
         (tm         (cond (tr-moure  tr-moure)  (t 0)))
         (base-ally  (agent-xyz999-get 'base-ally mem))

         ;; TRET: millor objectiu en rang 5u² si cooldown < 1
         (tret-coord (cond ((< tp 1)
                            (agent-xyz999-millor-tret vis equip color-propi
                                                      coord base-ally nil -1))
                           (t nil)))
         (acc-tret   (cond (tret-coord
                            (list (list 'pinta (list tret-coord))))
                           (t nil)))

         ;; MOVIMENT: greedy cap al destí de rol si cooldown < 1
         (desti      (agent-xyz999-desti-bolla mem id ronda coord color-propi))
         (acc-mou    (cond ((< tm 1)
                            (let* ((movs (agent-xyz999-movibles vis coord))
                                   (m    (agent-xyz999-millor-mov movs desti
                                                                  color-propi
                                                                  nil 1000000000)))
                              (cond (m (list (list 'mou (list m))))
                                    (t nil))))
                           (t nil))))
    (append acc-tret acc-mou)))


;; ======================================================================
;; SECCIÓ 10: DECISIÓ DE LA BASE
;; ======================================================================

(defun agent-xyz999-decisio-base (coord vis mem pintura ronda id)
  "La base crea una bolla per torn si té ≥ 50 pintura.
   Color: kill color si base enemiga té 2 colors; sinó, rotació equilibrada.
   Spawn: cap a base enemiga (o labs, o rotació de 8 dirs) per guanyar posicionament.
   coord: (x y). vis: visió. mem: a-list. pintura, ronda, id: enters."
  (cond
    ((< pintura 50) nil)
    (t (let* ((colors-be  (agent-xyz999-get 'colors-base-enemy mem))
              (base-enemy (agent-xyz999-get 'base-enemy mem))
              (labs       (agent-xyz999-get 'labs mem))
              (color      (agent-xyz999-tria-color colors-be ronda))
              ;; Destí de spawn: base enemiga > labs > rotació de 8 dirs
              (desti-sp   (cond
                            (base-enemy base-enemy)
                            ((> (agent-xyz999-longitud labs) 0)
                             (agent-xyz999-lab-mes-proper labs coord nil 1000000))
                            (t (let* ((dir8 '((1 0)(1 1)(0 1)(-1 1)
                                              (-1 0)(-1 -1)(0 -1)(1 -1)))
                                      (d    (agent-xyz999-nth-safe
                                              (rem (+ (agent-xyz999-abs id) ronda) 8)
                                              dir8)))
                                 (list (+ (cond ((car coord) (car coord)) (t 500))
                                          (* (car d) 5))
                                       (+ (cond ((cadr coord) (cadr coord)) (t 500))
                                          (* (cadr d) 5)))))))
              (movs       (agent-xyz999-movibles vis coord))
              (spawn      (agent-xyz999-millor-mov movs desti-sp nil nil 1000000000)))
         (cond
           ((and spawn color) (list (list 'crea-bolla (list color spawn))))
           (t nil))))))


;; ======================================================================
;; SECCIÓ 11: PUNT D'ENTRADA PRINCIPAL
;; ======================================================================

(defun agent-xyz999 (dades)
  "Punt d'entrada de l'agent. Cridat pel controlador per cada unitat cada torn.
   Dades: (ronda equip pintura id-unitat tipus-unitat coordenada colors-pintat
           color-propi tr-pintar tr-moure visio memoria-compartida).
   Retorna: (escriu-memoria mem-nova) + llista d'accions de la unitat."
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

         ;; 1. Actualitza memòria compartida amb el que veu aquesta unitat
         (mem-vis     (agent-xyz999-actualitza-mem vis mem-old equip))

         ;; 2. La base sempre registra la seva pròpia posició a base-ally
         (mem-final   (cond
                        ((eq tipus 'base)
                         (agent-xyz999-set 'base-ally coord mem-vis))
                        (t mem-vis)))

         ;; 3. Decideix les accions segons el tipus d'unitat
         (accions     (cond
                        ((eq tipus 'base)
                         (agent-xyz999-decisio-base coord vis mem-final
                                                    pintura ronda id))
                        ((eq tipus 'bolla)
                         (agent-xyz999-decisio-bolla coord equip color-propi
                                                      tr-pintar tr-moure vis
                                                      mem-final id ronda))
                        (t nil))))

    ;; 4. Retorna sempre l'escriptura de memòria + les accions
    (append (list (list 'escriu-memoria (list mem-final)))
            accions)))