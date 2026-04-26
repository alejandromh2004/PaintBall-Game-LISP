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
;; ESTRATÈGIA v4 ("Frontier Spiral Exploration")
;;
;;   PROBLEMA DETECTAT A v3:
;;     L'explorador usava (bx + dir*1000), però el mapa fa 20-60 caselles.
;;     Les bolles marxaven en una direcció, xocaven amb l'aigua o el límit,
;;     i quedaven encallades fins que la fase canviava (cada 15 torns).
;;     A més, enemy-last-seen aglomerava TOTES les bolles al mateix punt.
;;
;;   SOLUCIÓ v4 — FRONTIER SPIRAL EXPLORATION:
;;     ✅ 24 waypoints en 3 anells concèntrics a distàncies reals
;;        (radis ~10, ~20, ~28 caselles des de la base), calibrats per
;;        cobrir mapes de 20×20 fins a 60×60.
;;     ✅ Cada bolla manté una FASE en memòria compartida (unit-phases).
;;        Quan arriba al waypoint (dist²<16), avança automàticament.
;;     ✅ TIMEOUT anti-bloqueig: si (ronda mod 6 = id mod 6), avança
;;        la fase per força, evitant que es quedi eterns en water/mur.
;;     ✅ Dispersió per id: índex = (abs(id)+fase) mod 24 → cada bolla
;;        comença en un waypoint distint, cobertura paral·lela garantida.
;;     ✅ Eliminat enemy-last-seen com a override global (causava
;;        aglomeració de totes les bolles al mateix punt en mapes grans).
;;     ✅ Mantingut: Kill-color override, defensa activa (+300), rols.
;;
;;   ROLS (abs(id) mod 3):
;;     0 → ATACANT:    Rush directe a base enemiga; si no la coneix, explora.
;;     1 → CHASSADOR:  Va al lab MÉS PROPER; sense labs, ataca base.
;;     2 → EXPLORADOR: Avança waypoints sistemàticament; s'uneix si base visible.
;;
;;   SISTEMA DE TRETS (prioritat descendent):
;;     9000 → Kill base (2 colors pintats + el nostre = destruïda!)
;;     5300 → 2n color base + bonus defensa activa
;;     5000 → 2n color base
;;     2300 → 1r color base + bonus defensa activa
;;     2000 → 1r color base
;;     1100 → Kill shot bolla prop de base aliada
;;      800 → Kill shot bolla
;;      700 → Dany bolla prop de base aliada
;;      400 → Dany útil a bolla
;;      200 → Captura lab enemic/neutral
;;       -1 → No disparar
;;
;;   MEMÒRIA COMPARTIDA (estimació ~140 àtoms, molt per sota de 100000):
;;     base-ally:         coord base pròpia           (~4 àtoms)
;;     base-enemy:        coord base enemiga           (~4 àtoms)
;;     colors-base-enemy: colors pintats a base enemy (~4 àtoms)
;;     labs:              coords labs enemics/neutrals (~45 àtoms, màx 15)
;;     labs-a:            coords labs aliats           (~30 àtoms, màx 10)
;;     unit-phases:       a-list (id . fase)           (~40 àtoms, màx 20)
;;
;;   DISSENY FUNCIONAL: Sense reassignació ni mutació d'estructures.
;;   Tractament seqüencial exclusivament per recursió i funcions d'ordre
;;   superior (mapcar, reduce si escau). Cap crida iterativa.
;;   Totes les funcions prefixades amb "agent-xyz999-".
;;
;;   ÚS: (agent-xyz999 dades) — cridat pel controlador per cada unitat.
;; ======================================================================


;; ======================================================================
;; SECCIÓ 1: UTILITATS BÀSIQUES
;; ======================================================================

(defun agent-xyz999-dist-q (c1 c2)
  "Distància euclidiana al quadrat entre c1=(x y) i c2=(x y).
   Retorna 1000000 si algun dels arguments és nil."
  (cond ((or (null c1) (null c2)) 1000000)
        (t (let ((dx (- (car c1) (car c2)))
                 (dy (- (cadr c1) (cadr c2))))
             (+ (* dx dx) (* dy dy))))))

(defun agent-xyz999-abs (n)
  "Valor absolut d'un nombre enter. Retorna 0 si n és nil."
  (cond ((null n) 0)
        ((< n 0) (- n))
        (t n)))

(defun agent-xyz999-longitud (lst)
  "Longitud de la llista lst. Recursió simple."
  (cond ((null lst) 0)
        (t (+ 1 (agent-xyz999-longitud (cdr lst))))))

(defun agent-xyz999-nth-safe (n lst)
  "Element n-è (0-indexat) de lst, o nil si n és fora de rang."
  (cond ((null lst) nil)
        ((= n 0) (car lst))
        (t (agent-xyz999-nth-safe (- n 1) (cdr lst)))))

(defun agent-xyz999-membre (elem lst)
  "Cert si elem pertany a lst (comparació per equal)."
  (cond ((null lst) nil)
        ((equal (car lst) elem) t)
        (t (agent-xyz999-membre elem (cdr lst)))))

(defun agent-xyz999-elimina (elem lst)
  "Elimina totes les aparicions d'elem de lst (comparació per equal).
   No muta la llista original."
  (cond ((null lst) nil)
        ((equal (car lst) elem) (agent-xyz999-elimina elem (cdr lst)))
        (t (cons (car lst) (agent-xyz999-elimina elem (cdr lst))))))


;; ======================================================================
;; SECCIÓ 2: MEMÒRIA COMPARTIDA (a-list pura, sense mutació)
;; ======================================================================

(defun agent-xyz999-get (clau mem)
  "Retorna el valor associat a clau (símbol) en la a-list mem, o nil.
   Comparació per eq (claus simbòliques).
   mem: llista de parells (clau . valor)."
  (cond ((null mem) nil)
        ((eq (caar mem) clau) (cdar mem))
        (t (agent-xyz999-get clau (cdr mem)))))

(defun agent-xyz999-set (clau val mem)
  "Insereix o actualitza el parell (clau . val) a la a-list mem.
   No muta: retorna una nova a-list.
   clau: símbol. val: qualsevol estructura."
  (cond ((null mem) (list (cons clau val)))
        ((eq (caar mem) clau) (cons (cons clau val) (cdr mem)))
        (t (cons (car mem) (agent-xyz999-set clau val (cdr mem))))))

(defun agent-xyz999-afegir-coord (coord lst max-n)
  "Afegeix coord a lst si no hi és ja i la llista té menys de max-n elements.
   coord: (x y). lst: llista de coords. max-n: límit."
  (cond ((agent-xyz999-membre coord lst) lst)
        ((>= (agent-xyz999-longitud lst) max-n) lst)
        (t (cons coord lst))))

;; ---- Accessors per unit-phases (claus numèriques: usa = en comptes de eq) ----

(defun agent-xyz999-get-unit-phase (id unit-phases)
  "Retorna la fase d'exploració de la unitat id dins unit-phases, o 0 per defecte.
   unit-phases: a-list ((id . fase) ...) amb claus enteres.
   Comparació per = (nombres enters)."
  (cond ((null unit-phases) 0)
        ((= (caar unit-phases) id) (cdar unit-phases))
        (t (agent-xyz999-get-unit-phase id (cdr unit-phases)))))

(defun agent-xyz999-set-unit-phase (id fase unit-phases)
  "Actualitza o afegeix el parell (id . fase) a unit-phases.
   No muta: retorna nova a-list.
   id: enter. fase: enter (incremental). unit-phases: a-list."
  (cond ((null unit-phases) (list (cons id fase)))
        ((= (caar unit-phases) id) (cons (cons id fase) (cdr unit-phases)))
        (t (cons (car unit-phases)
                 (agent-xyz999-set-unit-phase id fase (cdr unit-phases))))))


;; ======================================================================
;; SECCIÓ 3: ACCESSORS DE CASELLA
;;   Estructura de casella visió:
;;   (coord tipus-casella color-casella tipus-elem equip colors-pintat
;;    color-propi tr-pintar tr-moure)
;;   Índexs: 0       1              2             3         4      5
;;           6           7           8
;; ======================================================================

(defun agent-xyz999-c-coord  (c) (nth 0 c)) ; (x y)
(defun agent-xyz999-c-tipus  (c) (nth 1 c)) ; 'terra o 'agua
(defun agent-xyz999-c-color  (c) (nth 2 c)) ; 'r 'g o 'b
(defun agent-xyz999-c-elem   (c) (nth 3 c)) ; 'base 'bolla 'lab o nil
(defun agent-xyz999-c-equip  (c) (nth 4 c)) ; 'e1 'e2 o nil
(defun agent-xyz999-c-colors (c) (nth 5 c)) ; llista de colors pintats


;; ======================================================================
;; SECCIÓ 4: ACTUALITZACIÓ DE MEMÒRIA A PARTIR DE LA VISIÓ
;; ======================================================================

(defun agent-xyz999-actualitza-mem (vis mem equip)
  "Recorre la visió vis i actualitza mem (sense mutar):
     base-ally:         posició de la pròpia base quan es veu.
     base-enemy:        posició de la base enemiga quan es veu.
     colors-base-enemy: colors pintats a la base enemiga.
     labs:              coords de labs enemics/neutrals (màx 15).
     labs-a:            coords de labs aliats (màx 10).
   vis: llista de caselles (estructura de la secció 3).
   mem: a-list actual. equip: símbol ('e1 o 'e2) de l'equip propi."
  (cond
    ((null vis) mem)
    (t (let* ((c     (car vis))
              (coord (agent-xyz999-c-coord c))
              (elem  (agent-xyz999-c-elem c))
              (eq-c  (agent-xyz999-c-equip c))
              (cp    (agent-xyz999-c-colors c))

              ;; Actualitza base-ally, base-enemy i colors-base-enemy
              (mem1  (cond
                       ((and (eq elem 'base) coord (not (eq eq-c equip)))
                        (agent-xyz999-set 'colors-base-enemy cp
                          (agent-xyz999-set 'base-enemy coord mem)))
                       ((and (eq elem 'base) coord (eq eq-c equip))
                        (agent-xyz999-set 'base-ally coord mem))
                       (t mem)))

              ;; Actualitza labs: si és aliat → labs-a; si és enemic/neutral → labs
              (mem2  (cond
                       ((and (eq elem 'lab) coord)
                        (cond
                          ((eq eq-c equip)
                           ;; Lab capturat per nosaltres: passa a labs-a, surt de labs
                           (agent-xyz999-set 'labs-a
                             (agent-xyz999-afegir-coord coord
                               (agent-xyz999-get 'labs-a mem1) 10)
                             (agent-xyz999-set 'labs
                               (agent-xyz999-elimina coord
                                 (agent-xyz999-get 'labs mem1))
                               mem1)))
                          (t
                           ;; Lab enemic o neutral: entra a labs, surt de labs-a
                           (agent-xyz999-set 'labs
                             (agent-xyz999-afegir-coord coord
                               (agent-xyz999-get 'labs mem1) 15)
                             (agent-xyz999-set 'labs-a
                               (agent-xyz999-elimina coord
                                 (agent-xyz999-get 'labs-a mem1))
                               mem1)))))
                       (t mem1))))
         ;; Continua amb la resta de la visió
         (agent-xyz999-actualitza-mem (cdr vis) mem2 equip)))))


;; ======================================================================
;; SECCIÓ 5: LÒGICA DE COLORS
;; ======================================================================

(defun agent-xyz999-filtra-falten (tots pintats)
  "Retorna la subllista de tots que NO estan a pintats.
   tots: llista de colors totals (p.ex. '(r g b)).
   pintats: llista de colors ja aplicats."
  (cond ((null tots) nil)
        ((agent-xyz999-membre (car tots) pintats)
         (agent-xyz999-filtra-falten (cdr tots) pintats))
        (t (cons (car tots)
                 (agent-xyz999-filtra-falten (cdr tots) pintats)))))

(defun agent-xyz999-tria-color (colors-be ronda)
  "Tria el color òptim per a la nova bolla:
     - 1 color falta a la base enemiga → kill color directe.
     - 2+ colors falten → rotació per ronda entre els que falten.
     - Cap color falta (cas degenerat) → rotació completa per ronda.
   colors-be: llista de colors pintats a la base enemiga. ronda: enter."
  (let* ((falten (agent-xyz999-filtra-falten '(r g b) colors-be))
         (n      (agent-xyz999-longitud falten)))
    (cond ((= n 0) (agent-xyz999-nth-safe (rem (agent-xyz999-abs ronda) 3) '(r g b)))
          ((= n 1) (car falten))
          (t (agent-xyz999-nth-safe (rem (agent-xyz999-abs ronda) n) falten)))))

(defun agent-xyz999-kill-color (colors-be)
  "Retorna el color que destruirà la base enemiga si ja en té 2, nil en cas contrari.
   colors-be: llista de colors pintats a la base enemiga."
  (let ((falten (agent-xyz999-filtra-falten '(r g b) colors-be)))
    (cond ((= (agent-xyz999-longitud falten) 1) (car falten))
          (t nil))))


;; ======================================================================
;; SECCIÓ 6: ROLS, EXPLORACIÓ PER FASES I DESTINS (v4 — Frontier Spiral)
;; ======================================================================

(defun agent-xyz999-rol (id)
  "Assigna un rol per abs(id) mod 3:
   0 = ATACANT  (rush base enemiga),
   1 = CHASSADOR (prioritza labs),
   2 = EXPLORADOR (cobertura sistemàtica per waypoints)."
  (rem (agent-xyz999-abs id) 3))

(defun agent-xyz999-lab-mes-proper (labs coord best-coord best-dist)
  "Retorna la coord del laboratori de labs més proper a coord.
   Recursió de cua sobre labs.
   labs: llista de coords. best-coord, best-dist: acumuladors."
  (cond ((null labs) best-coord)
        (t (let ((d (agent-xyz999-dist-q (car labs) coord)))
             (cond ((< d best-dist)
                    (agent-xyz999-lab-mes-proper (cdr labs) coord (car labs) d))
                   (t
                    (agent-xyz999-lab-mes-proper (cdr labs) coord best-coord best-dist)))))))

(defun agent-xyz999-waypoints ()
  "Llista de 24 desplaçaments relatius (dx dy) en espiral de 3 anells.
   Anell 1 (radi~25): cobreix espais oberts immediats.
   Anell 2 (radi~45): cobreix mapes mitjans i zones intermèdies.
   Anell 3 (radi~65): cobreix els extrems de mapes molt grans.
   Radi estès dràsticament per forçar l'allunyament de la base."
  '((25 0)  (18 18) (0 25)  (-18 18) (-25 0)  (-18 -18) (0 -25) (18 -18)
    (45 0)  (32 32) (0 45)  (-32 32) (-45 0)  (-32 -32) (0 -45) (32 -32)
    (65 15) (15 65) (-15 65)(-65 15) (-65 -15)(-15 -65) (15 -65)(65 -15)))

(defun agent-xyz999-waypoint-per-fase (id fase base-ally)
  "Retorna la coord absoluta del waypoint per la fase actual de la unitat id.
   Índex = (abs(id) + abs(fase)) mod 24.
   El sumand abs(id) actua com a offset estàtic: unitats d'id diferent
   comencen en waypoints distints i sempre van ~id posicions separades,
   garantint cobertura paral·lela del mapa sense coordinació explícita.
   id: enter. fase: enter (incremental). base-ally: (x y) o nil."
  (let* ((wps (agent-xyz999-waypoints))
         (idx (rem (+ (agent-xyz999-abs id) (agent-xyz999-abs fase)) 24))
         (rel (agent-xyz999-nth-safe idx wps))
         (bx  (cond ((and base-ally (car base-ally))  (car base-ally))  (t 500)))
         (by  (cond ((and base-ally (cadr base-ally)) (cadr base-ally)) (t 500))))
    (cond ((null rel) (list bx by))
          (t (list (+ bx (car rel)) (+ by (cadr rel)))))))

(defun agent-xyz999-avanca-fase-si-cal (coord mem id ronda)
  "Comprova si la bolla id ha de canviar de waypoint i, si cal,
   retorna la memòria actualitzada amb la fase incrementada.

   Condicions d'avanç (qualsevol de les dues):
     (1) Proximitat: dist² al waypoint actual < 16 (~4 caselles).
         La bolla ha assolit el punt d'exploració assignat.
     (2) Timeout anti-bloqueig: (ronda mod 35) = (id mod 35).
         Força l'avanç cada ~35 torns per donar temps a viatjar lluny
         sense quedar-se encallats indefinidament.
         Cada unitat té el seu propi timeout (id mod 6 distints).

   coord: (x y) posició actual. mem: a-list. id, ronda: enters."
  (let* ((base-ally   (agent-xyz999-get 'base-ally mem))
         (unit-phases (agent-xyz999-get 'unit-phases mem))
         (fase        (agent-xyz999-get-unit-phase id unit-phases))
         (wp          (agent-xyz999-waypoint-per-fase id fase base-ally))
         (dist-wp     (agent-xyz999-dist-q coord wp))
         (timeout     (= (rem (agent-xyz999-abs ronda) 35)
                         (rem (agent-xyz999-abs id)    35))))
    (cond
      ((or (< dist-wp 16) timeout)
       (let* ((nova-fase       (+ fase 1))
              (nou-unit-phases (agent-xyz999-set-unit-phase id nova-fase unit-phases)))
         (agent-xyz999-set 'unit-phases nou-unit-phases mem)))
      (t mem))))

(defun agent-xyz999-desti-bolla (mem id coord color-propi)
  "Determina el destí de la bolla a partir de la memòria ja actualitzada.
   Prioritat:
     1. KILL-COLOR OVERRIDE: soc el color que destrueix la base → rush!
     2. ROL 0 (atacant):   base-enemy si coneguda → waypoint actual.
     3. ROL 1 (chassador): lab més proper → base-enemy → waypoint.
     4. ROL 2 (explorador): base-enemy si coneguda → waypoint actual.
   mem: a-list (ja amb fase avançada si calia en aquest torn).
   id: enter. coord: (x y). color-propi: símbol ('r, 'g o 'b)."
  (let* ((rol         (agent-xyz999-rol id))
         (base-enemy  (agent-xyz999-get 'base-enemy mem))
         (colors-be   (agent-xyz999-get 'colors-base-enemy mem))
         (labs        (agent-xyz999-get 'labs mem))
         (base-ally   (agent-xyz999-get 'base-ally mem))
         (unit-phases (agent-xyz999-get 'unit-phases mem))
         (fase        (agent-xyz999-get-unit-phase id unit-phases))
         (n-labs      (agent-xyz999-longitud labs))
         (kc          (agent-xyz999-kill-color colors-be))
         (es-kill     (and kc base-enemy (eq color-propi kc)))
         (wp          (agent-xyz999-waypoint-per-fase id fase base-ally)))
    (cond
      ;; KILL-COLOR OVERRIDE: soc el color decisiu, rush immediat!
      (es-kill base-enemy)

      ;; ROL 0 - ATACANT: directe a la base enemiga si la coneix
      ((and (= rol 0) base-enemy) base-enemy)

      ;; ROL 1 - CHASSADOR: cap al lab més proper per acumular pintura
      ((and (= rol 1) (> n-labs 0))
       (agent-xyz999-lab-mes-proper labs coord nil 1000000))

      ;; ROL 1 sense labs: s'uneix a l'atac si la base és coneguda
      ((and (= rol 1) base-enemy) base-enemy)

      ;; ROL 2 - EXPLORADOR: ataca si base coneguda, si no explora
      ((and (= rol 2) base-enemy) base-enemy)

      ;; Fallback universal: waypoint actual per cobertura sistemàtica
      (t wp))))


;; ======================================================================
;; SECCIÓ 7: NAVEGACIÓ
;; ======================================================================

(defun agent-xyz999-movibles (vis coord)
  "Retorna la subllista de caselles de vis on la bolla es pot moure:
   han de ser terra buides, dins dist²≤2 de coord (8 adjacents), i ≠coord.
   vis: llista de caselles. coord: (x y) posició actual."
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
  "Cost d'un moviment a casella dirigint-se cap a desti.
   Cost = dist²(casella, desti)*10 + penalització de color + soroll.
   Penalitza +5 les caselles de color aliè. S'hi suma (random 20) per 
   trencar oscil·lacions infinites quan hi ha parets d'aigua en forma de U.
   casella: estructura casella. desti: (x y). color-propi: símbol."
  (let ((coord (agent-xyz999-c-coord casella)))
    (cond ((or (null coord) (null desti)) 1000000)
          (t (+ (* (agent-xyz999-dist-q coord desti) 10)
                (cond ((eq (agent-xyz999-c-color casella) color-propi) 0)
                      (t 5))
                (random 20))))))

(defun agent-xyz999-millor-mov (movibles desti color-propi best-coord best-cost)
  "Cerca greedy la casella de movibles amb menor cost cap a desti.
   Recursió de cua sobre movibles.
   Retorna la coord (x y) de la millor casella, o nil si no n'hi ha cap."
  (cond
    ((null movibles) best-coord)
    (t (let ((cost (agent-xyz999-cost-mov (car movibles) desti color-propi)))
         (cond ((< cost best-cost)
                (agent-xyz999-millor-mov (cdr movibles) desti color-propi
                                         (agent-xyz999-c-coord (car movibles)) cost))
               (t
                (agent-xyz999-millor-mov (cdr movibles) desti color-propi
                                         best-coord best-cost)))))))

(defun agent-xyz999-fallback-mov (movibles)
  "Retorna la primera casella disponible com a fallback si està encallada."
  (cond ((null movibles) nil)
        (t (agent-xyz999-c-coord (car movibles)))))


;; ======================================================================
;; SECCIÓ 8: SISTEMA DE COMBAT
;; ======================================================================

(defun agent-xyz999-prioritat-tret (casella equip color-propi dist base-ally)
  "Calcula la prioritat d'un tret a casella des de dist.
   Retorna -1 si no s'ha de disparar (fora rang, casella buida, aliat,
   ja té el nostre color, etc.).
   Bonus de +300 per objectius dins dist²<25 de la base aliada (defensa activa).
   casella: estructura casella. equip: símbol. color-propi: símbol.
   dist: dist² bolla→casella. base-ally: (x y) o nil."
  (let* ((elem  (agent-xyz999-c-elem casella))
         (eq-c  (agent-xyz999-c-equip casella))
         (cp    (agent-xyz999-c-colors casella))
         (n-cp  (agent-xyz999-longitud cp))
         (d-ba  (agent-xyz999-dist-q (agent-xyz999-c-coord casella) base-ally))
         (bonus (cond ((and (< d-ba 25) (not (eq eq-c equip))) 300) (t 0))))
    (cond
      ;; Filtres bàsics: fora rang, no terra, casella buida, element aliat
      ((> dist 5)                                    -1)
      ((not (eq (agent-xyz999-c-tipus casella) 'terra)) -1)
      ((null elem)                                   -1)
      ((eq eq-c equip)                               -1)

      ;; BASE ENEMIGA: objectiu suprem
      ((eq elem 'base)
       (cond
         ((agent-xyz999-membre color-propi cp) -1)   ; ja té el nostre color: no pintar
         ((= n-cp 2)        9000)                    ; kill shot: tercer color!
         ((= n-cp 1) (+ 5000 bonus))                 ; segon color útil
         (t          (+ 2000 bonus))))                ; primer color útil

      ;; BOLLA ENEMIGA
      ((eq elem 'bolla)
       (cond
         ((agent-xyz999-membre color-propi cp) -1)   ; ja té el nostre color
         ((= n-cp 2) (+ 800 bonus))                  ; kill shot de bolla!
         (t          (+ 400 bonus))))                 ; dany útil a bolla

      ;; LAB enemic o no capturat: captura'l
      ((eq elem 'lab)
       (cond ((not (eq eq-c equip)) 200)
             (t -1)))

      (t -1))))

(defun agent-xyz999-millor-tret (vis equip color-propi coord base-ally best-coord best-punt)
  "Cerca el millor objectiu per disparar (rang ≤5u²) dins la visió vis.
   Recursió de cua sobre vis. Retorna la coord de l'objectiu o nil si cap.
   equip, color-propi: símbols. coord: (x y) posició bolla.
   base-ally: (x y) o nil. best-coord, best-punt: acumuladors."
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
                                    vis mem id)
  "Cervell de la bolla: retorna la llista d'accions (tret i/o moviment).
   Tret i moviment tenen cooldowns independents: es poden combinar al
   mateix torn. El destí s'obté de desti-bolla (ja llegeix fase actualitzada).
   coord: (x y). equip, color-propi: símbols.
   tr-pintar, tr-moure: cooldowns actuals (nil tractat com 0, bolles noves).
   vis: llista caselles visibles. mem: a-list ja actualitzada. id: enter."
  (let* ((tp         (cond (tr-pintar tr-pintar) (t 0)))
         (tm         (cond (tr-moure  tr-moure)  (t 0)))
         (base-ally  (agent-xyz999-get 'base-ally mem))

         ;; TRET: millor objectiu en rang ≤5u² si cooldown < 1
         (tret-coord (cond ((< tp 1)
                            (agent-xyz999-millor-tret vis equip color-propi
                                                      coord base-ally nil -1))
                           (t nil)))
         (acc-tret   (cond (tret-coord (list (list 'pinta (list tret-coord))))
                           (t nil)))

         ;; MOVIMENT: cap al destí de rol/waypoint si cooldown < 1
         (desti      (agent-xyz999-desti-bolla mem id coord color-propi))
         (acc-mou    (cond ((< tm 1)
                            (let* ((movs  (agent-xyz999-movibles vis coord))
                                   (m     (agent-xyz999-millor-mov movs desti
                                                                   color-propi
                                                                   nil 1000000000))
                                   (m-fin (cond (m m) (t (agent-xyz999-fallback-mov movs)))))
                              (cond (m-fin (list (list 'mou (list m-fin))))
                                    (t nil))))
                           (t nil))))
    (append acc-tret acc-mou)))


;; ======================================================================
;; SECCIÓ 10: DECISIÓ DE LA BASE
;; ======================================================================

(defun agent-xyz999-decisio-base (coord vis mem pintura ronda id)
  "La base crea una bolla per torn si té ≥50 pintura.
   Color: kill color si la base enemiga té 2 colors; sinó, rotació equilibrada.
   Spawn: dirigit cap a base-enemy > lab més proper > rotació de 8 dirs.
   coord: (x y). vis: llista caselles. mem: a-list.
   pintura, ronda, id: enters."
  (cond
    ((< pintura 50) nil)
    (t (let* ((colors-be  (agent-xyz999-get 'colors-base-enemy mem))
              (base-enemy (agent-xyz999-get 'base-enemy mem))
              (labs       (agent-xyz999-get 'labs mem))
              (color      (agent-xyz999-tria-color colors-be ronda))

              ;; Direcció de spawn: base-enemy > labs > 8 dirs rotatives
              (desti-sp   (cond
                            (base-enemy base-enemy)
                            ((> (agent-xyz999-longitud labs) 0)
                             (agent-xyz999-lab-mes-proper labs coord nil 1000000))
                            (t (let* ((dir8 '((1 0)(1 1)(0 1)(-1 1)
                                              (-1 0)(-1 -1)(0 -1)(1 -1)))
                                      (d    (agent-xyz999-nth-safe
                                              (rem (+ (agent-xyz999-abs id)
                                                      (agent-xyz999-abs ronda))
                                                   8)
                                              dir8)))
                                 (list (+ (cond ((car coord)  (car coord))  (t 500))
                                          (* (car d) 5))
                                       (+ (cond ((cadr coord) (cadr coord)) (t 500))
                                          (* (cadr d) 5)))))))

              (movs  (agent-xyz999-movibles vis coord))
              (spawn (agent-xyz999-millor-mov movs desti-sp nil nil 1000000000)))
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
   Retorna: ((escriu-memoria mem-nova) accio1 accio2 ...).

   Flux intern (tot funcional, sense mutació):
     1. Actualitza memòria compartida amb la visió d'aquesta unitat.
     2. La base registra la seva pròpia coord a base-ally.
     3. La bolla avança la seva fase d'exploració si s'escau.
     4. Decideix accions (base o bolla) amb la memòria actualitzada.
     5. Retorna escriu-memoria + accions."
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

         ;; Pas 1: actualitza memòria amb la visió d'aquesta unitat
         (mem-vis     (agent-xyz999-actualitza-mem vis mem-old equip))

         ;; Pas 2: la base sempre refresca la seva posició a base-ally
         (mem-base    (cond
                        ((eq tipus 'base)
                         (agent-xyz999-set 'base-ally coord mem-vis))
                        (t mem-vis)))

         ;; Pas 3: les bolles avancen fase si han assolit el waypoint o timeout
         (mem-final   (cond
                        ((eq tipus 'bolla)
                         (agent-xyz999-avanca-fase-si-cal coord mem-base id ronda))
                        (t mem-base)))

         ;; Pas 4: decideix accions amb la memòria completament actualitzada
         (accions     (cond
                        ((eq tipus 'base)
                         (agent-xyz999-decisio-base coord vis mem-final
                                                    pintura ronda id))
                        ((eq tipus 'bolla)
                         (agent-xyz999-decisio-bolla coord equip color-propi
                                                      tr-pintar tr-moure vis
                                                      mem-final id))
                        (t nil))))

    ;; Pas 5: retorna sempre l'escriptura de memòria + les accions de la unitat
    (append (list (list 'escriu-memoria (list mem-final)))
            accions)))