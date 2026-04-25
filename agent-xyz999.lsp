;; ======================================================================
;; PRÀCTICA FINAL LLENGUATGES DE PROGRAMACIÓ - LISP - PAINTBALL
;; ======================================================================
;; Estudiant: XYZ999
;; Data: 25/04/2026
;; Assignatura: Llenguatges de Programació (LP)
;; Convocatòria: Primera Convocatòria (Ordinària)
;;
;; FITXER: agent-xyz999.lsp
;;
;; ESTRATÈGIA GENERAL v2 ("Coordinated Deep Strike"):
;;
;;   ROLS (per ID mod 3):
;;     0 → ATACANT:    Rush directe a base enemiga quan la coneix.
;;                     Si no la coneix, explora en el seu sector únic.
;;     1 → CHASSADOR:  Va al lab enemic/neutral MÉS PROPER per acumular
;;                     pintura. Si captura tots els labs, defen els aliats.
;;                     Si no hi ha labs, s'uneix a l'atac.
;;     2 → EXPLORADOR: Cobreix sectors del mapa sistemàticament.
;;                     Si la base és coneguda, s'uneix a l'atac.
;;                     → 2/3 de les unitats atacaran la base quan sigui visible.
;;                     → 1/3 manté el flux de pintura capturant labs.
;;
;;   TRET (prioritats estrictes per cooldown < 1):
;;     9000 → Kill shot base (té 2 colors, el nostre és el 3r → DESTRUÏDA!)
;;     5000 → Segon color a la base (té 1 color)
;;     2000 → Primer color a la base (0 colors)
;;      800 → Kill shot bolla enemiga (2 colors, el nostre és el 3r)
;;      400 → Dany a bolla enemiga (li falta el nostre color)
;;      200 → Captura lab enemic/neutral
;;       -1 → No disparar (aliat, buida, ja té el nostre color, fora rang)
;;
;;   MOVIMENT: Greedy cap al destí de rol. Penalitza caselles de color
;;             incorrecte per estalviar cooldown de moviment (+100 cost).
;;
;;   BASE: Crea una bolla per torn si té ≥ 50 pintura.
;;     Color: el que destruirà la base enemiga (o rota r/g/b per equilibrar).
;;     Quan la base enemiga té 2 colors, SEMPRE crea l'únic kill color.
;;     Spawn cap a la base enemiga (o labs) per guanyar temps.
;;
;;   MEMÒRIA COMPARTIDA (~70 àtoms, molt per sota del límit de 100000):
;;     base-ally:          coordenada de la base aliada
;;     base-enemy:         coordenada de la base enemiga
;;     colors-base-enemy:  colors ja pintats a la base enemiga
;;     labs:               coords de labs enemics/neutrals (objectius, màx 15)
;;     labs-a:             coords de labs aliats (per defensar, màx 10)
;;
;;   CORRECCIONS CRÍTIQUES respecte v1:
;;     ❌ v1: Exploració a distància 400u → unitats bloquejades al límit del mapa
;;     ✅ v2: Exploració a distància 15u → cobreix correctament el mapa
;;     ❌ v1: Fase d'exploració cada 300 rondes → adaptació massa lenta
;;     ✅ v2: Fase cada 30 rondes → gira de sector més sovint
;;     ❌ v1: Lab seleccionat aleatòriament per ID → moltes unitats al mateix lab
;;     ✅ v2: Lab MÉS PROPER a la unitat → resposta ràpida i sense col·lisions
;;     ❌ v1: Totes les unitats a base enemiga quan es coneix → 0 pintura extra
;;     ✅ v2: 1/3 chassadors mantenen labs per flux de pintura
;;
;;   DISSENY FUNCIONAL: Sense reassignació ni mutació d'estructures.
;;   Tractament seqüencial exclusivament per recursió.
;;   FUNCIONS: Totes prefixades amb "agent-xyz999-".
;;
;;   ÚS: (agent-xyz999 dades) - cridat pel controlador per cada unitat.
;; ======================================================================


;; ======================================================================
;; SECCIÓ 1: UTILITATS BÀSIQUES
;; ======================================================================

(defun agent-xyz999-dist-q (c1 c2)
  "Distància euclidiana al quadrat entre dues coordenades (x y).
   Robusta a nil: retorna 1000000 si alguna és nil.
   c1, c2: llistes (x y) o nil."
  (cond ((or (null c1) (null c2)) 1000000)
        (t (let ((dx (- (car c1) (car c2)))
                 (dy (- (cadr c1) (cadr c2))))
             (+ (* dx dx) (* dy dy))))))

(defun agent-xyz999-abs (n)
  "Valor absolut d'un enter. Robusta a nil (retorna 0).
   n: enter o nil."
  (cond ((null n) 0)
        ((< n 0) (- n))
        (t n)))

(defun agent-xyz999-longitud (lst)
  "Longitud d'una llista lst."
  (cond ((null lst) 0)
        (t (+ 1 (agent-xyz999-longitud (cdr lst))))))

(defun agent-xyz999-nth-safe (n lst)
  "Retorna el n-è element (0-indexat) de lst, o nil si fora de rang.
   n: enter >= 0. lst: llista."
  (cond ((null lst) nil)
        ((= n 0) (car lst))
        (t (agent-xyz999-nth-safe (- n 1) (cdr lst)))))

(defun agent-xyz999-elimina (elem lst)
  "Elimina totes les aparicions d'elem de lst (comparació per equal).
   elem: qualsevol. lst: llista."
  (cond ((null lst) nil)
        ((equal (car lst) elem)
         (agent-xyz999-elimina elem (cdr lst)))
        (t (cons (car lst)
                 (agent-xyz999-elimina elem (cdr lst))))))

(defun agent-xyz999-membre (elem lst)
  "Comprova si elem és a lst (comparació per equal). Retorna t o nil.
   Necessari perquè member en XLISP-PLUS pot usar eql per defecte.
   elem: qualsevol. lst: llista."
  (cond ((null lst) nil)
        ((equal (car lst) elem) t)
        (t (agent-xyz999-membre elem (cdr lst)))))


;; ======================================================================
;; SECCIÓ 2: MEMÒRIA COMPARTIDA (a-list pura, sense mutació)
;; ======================================================================

(defun agent-xyz999-get (clau mem)
  "Retorna el valor associat a clau en la memòria (a-list).
   clau: símbol. mem: a-list ((clau . valor) ...). Retorna nil si no existeix."
  (cond ((null mem) nil)
        ((eq (caar mem) clau) (cdar mem))
        (t (agent-xyz999-get clau (cdr mem)))))

(defun agent-xyz999-set (clau val mem)
  "Insereix o actualitza la parella (clau . val) a la memòria.
   Retorna la nova memòria (no muta l'original).
   clau: símbol. val: qualsevol. mem: a-list."
  (cond ((null mem) (list (cons clau val)))
        ((eq (caar mem) clau) (cons (cons clau val) (cdr mem)))
        (t (cons (car mem)
                 (agent-xyz999-set clau val (cdr mem))))))

(defun agent-xyz999-afegir-coord (coord llista max-n)
  "Afegeix coord a llista si no hi és i no supera max-n elements.
   Usa equal per comparar coordenades (llistes).
   coord: (x y). llista: llista de coords. max-n: enter."
  (cond ((agent-xyz999-membre coord llista) llista)
        ((>= (agent-xyz999-longitud llista) max-n) llista)
        (t (cons coord llista))))


;; ======================================================================
;; SECCIÓ 3: ACCESSORS DE CASELLA DE LA VISIÓ
;;
;; Estructura d'una casella a la visió (índexs 0-8):
;;   0 = coordenada (x y)
;;   1 = tipus-casella ('terra o 'aigua)
;;   2 = color-casella ('r 'g o 'b)       [només si terra]
;;   3 = tipus-element ('base 'bolla 'lab) [si hi ha element]
;;   4 = equip ('e1 'e2 o nil)
;;   5 = colors-pintat (llista de colors pintats, pot ser nil)
;;   6 = color-propi (color de la bolla, nil si és base/lab)
;;   7 = tr-pintar (cooldown pintar)
;;   8 = tr-moure  (cooldown moure)
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
  "Recorre la visió vis i actualitza la memòria mem:
     base-ally, base-enemy, colors-base-enemy: bases vistes.
     labs: labs enemics/neutrals (objectius d'atac).
     labs-a: labs aliats (a defensar).
   vis: llista de caselles. mem: a-list. equip: 'e1 o 'e2."
  (cond
    ((null vis) mem)
    (t (let* ((c     (car vis))
              (coord (cond (c (agent-xyz999-c-coord c)) (t nil)))
              (elem  (cond (c (agent-xyz999-c-elem c))  (t nil)))
              (eq-c  (cond (c (agent-xyz999-c-equip c)) (t nil)))
              (cp    (cond (c (agent-xyz999-c-colors c)) (t nil)))

              ;; Actualitza informació de bases i enemics
              (mem1  (cond
                       ;; Base enemiga vista: guardem posició i colors pintats
                       ((and (eq elem 'base) coord eq-c (not (eq eq-c equip)))
                        (agent-xyz999-set 'colors-base-enemy cp
                          (agent-xyz999-set 'base-enemy coord mem)))
                       ;; Bolla enemiga vista: guardem on l'hem vist per inferir on és la seva base
                       ((and (eq elem 'bolla) coord eq-c (not (eq eq-c equip)))
                        (agent-xyz999-set 'enemy-last-seen coord mem))
                       ;; Base aliada: guardem posició
                       ((and (eq elem 'base) coord (eq eq-c equip))
                        (agent-xyz999-set 'base-ally coord mem))
                       (t mem)))

              ;; Actualitza labs:
              ;; - Lab aliat → a labs-a (defensa), fora de labs (ja no és objectiu)
              ;; - Lab enemic/neutral → a labs (objectiu), fora de labs-a
              (mem2  (cond
                       ((and (eq elem 'lab) coord)
                        (cond
                          ;; Lab capturat per nosaltres
                          ((eq eq-c equip)
                           (agent-xyz999-set 'labs-a
                             (agent-xyz999-afegir-coord coord
                               (agent-xyz999-get 'labs-a mem1) 10)
                             (agent-xyz999-set 'labs
                               (agent-xyz999-elimina coord
                                 (agent-xyz999-get 'labs mem1))
                               mem1)))
                          ;; Lab enemic o no capturat
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
  "Retorna els colors de tots que NO estan a pintats.
   tots: llista de colors. pintats: llista de colors pintats."
  (cond ((null tots) nil)
        ((agent-xyz999-membre (car tots) pintats)
         (agent-xyz999-filtra-falten (cdr tots) pintats))
        (t (cons (car tots)
                 (agent-xyz999-filtra-falten (cdr tots) pintats)))))

(defun agent-xyz999-tria-color (colors-be ronda)
  "Tria el color de bolla a crear per destruir la base enemiga.
   Quan la base té 2 colors, l'únic color útil és el kill color.
   Quan té 1 o 0 colors, rota per crear un exèrcit divers i equilibrat.
   colors-be: colors que JA TÉ la base enemiga. ronda: enter."
  (let* ((falten (agent-xyz999-filtra-falten '(r g b) colors-be))
         (n      (agent-xyz999-longitud falten)))
    (cond
      ;; Cas impossible (base ja destruïda), fallback
      ((= n 0) (agent-xyz999-nth-safe (rem ronda 3) '(r g b)))
      ;; UN ÚNIC COLOR POSSIBLE: el kill color, sempre!
      ((= n 1) (car falten))
      ;; Múltiples colors: rota per ronda per crear exèrcit equilibrat
      (t (agent-xyz999-nth-safe (rem ronda n) falten)))))


;; ======================================================================
;; SECCIÓ 6: ROLS I DESTINS
;; ======================================================================

(defun agent-xyz999-rol (id)
  "Assigna un rol a la unitat per ID mod 3:
     0 → atacant   (rush base enemiga, explora si no la coneix)
     1 → chassador (captura el lab MÉS PROPER, defensa labs aliats)
     2 → explorador (cobreix el mapa, s'uneix a l'atac si la base és visible)
   id: enter (id-unitat)."
  (rem (agent-xyz999-abs id) 3))

(defun agent-xyz999-lab-mes-proper (labs coord best-coord best-dist)
  "Retorna la coordenada del lab de la llista MÉS PROPER a coord.
   labs: llista de coordenades de labs.
   coord: posició actual (x y).
   best-coord, best-dist: millor resultat acumulat (inicialitzar a nil, 1000000)."
  (cond ((null labs) best-coord)
        (t (let ((d (agent-xyz999-dist-q (car labs) coord)))
             (cond ((< d best-dist)
                    (agent-xyz999-lab-mes-proper
                      (cdr labs) coord (car labs) d))
                   (t
                    (agent-xyz999-lab-mes-proper
                      (cdr labs) coord best-coord best-dist)))))))

(defun agent-xyz999-vector-exploracio (id ronda base-ally)
  "Genera un punt d'exploració llunyà en una direcció que canvia periòdicament.
   Canvia cada 15 rondes per evitar quedar-se bloquejat en parets.
   id: enter. ronda: enter. base-ally: (x y) o nil."
  (let* (;; 16 direccions per a una millor dispersió
         (dirs  '((1 0)(1 1)(0 1)(-1 1)(-1 0)(-1 -1)(0 -1)(1 -1)
                  (2 1)(1 2)(-1 2)(-2 1)(-2 -1)(-1 -2)(1 -2)(2 -1)))
         (fase  (truncate (/ ronda 15)))
         (idx   (rem (+ (agent-xyz999-abs id) fase) 16))
         (dir   (agent-xyz999-nth-safe idx dirs))
         (bx    (cond ((and base-ally (car base-ally)) (car base-ally)) (t 500)))
         (by    (cond ((and base-ally (cadr base-ally)) (cadr base-ally)) (t 500))))
    (cond ((null dir) (list bx by))
          (t (list (+ bx (* (car dir) 1000))
                   (+ by (* (cadr dir) 1000)))))))

(defun agent-xyz999-desti-bolla (mem id ronda coord)
  "Determina el destí prioritari de la bolla segons el seu rol.
   Atacant (0):    base-enemy → explorar.
   Chassador (1):  lab MÉS PROPER (no pel ID) → labs-a (defensa) → base-enemy → explorar.
   Explorador (2): base-enemy (si visible) → explorar.
   mem: a-list. id: enter. ronda: enter. coord: (x y) posició actual."
  (let* ((rol        (agent-xyz999-rol id))
         (base-enemy (agent-xyz999-get 'base-enemy mem))
         (labs       (agent-xyz999-get 'labs mem))
         (labs-a     (agent-xyz999-get 'labs-a mem))
         (base-ally  (agent-xyz999-get 'base-ally mem))
         (n-labs     (agent-xyz999-longitud labs))
         (n-labs-a   (agent-xyz999-longitud labs-a)))
    (cond
      ;; ROL 0 - ATACANT: rush directe a base enemiga si la coneix
      ((and (= rol 0) base-enemy) base-enemy)

      ;; ROL 1 - CHASSADOR: va al lab MÉS PROPER (enemics/neutrals)
      ((and (= rol 1) (> n-labs 0))
       (agent-xyz999-lab-mes-proper labs coord nil 1000000))

      ;; PRIORITAT COMPARTIDA: Si algú ha vist un enemic, anem a investigar aquesta zona!
      ((agent-xyz999-get 'enemy-last-seen mem)
       (agent-xyz999-get 'enemy-last-seen mem))

      ;; ROL 1 - CHASSADOR sense labs ni enemics: s'uneix a l'atac o explora
      ((and (= rol 1) base-enemy) base-enemy)

      ;; ROL 2 - EXPLORADOR: si base visible, s'uneix a l'atac
      ((and (= rol 2) base-enemy) base-enemy)

      ;; TOTS: explorar en la direcció dinàmica assignada
      (t (agent-xyz999-vector-exploracio id ronda base-ally)))))


;; ======================================================================
;; SECCIÓ 7: NAVEGACIÓ
;; ======================================================================

(defun agent-xyz999-movibles (vis coord)
  "Retorna les caselles de vis on la bolla es pot moure:
   terra buides, dins dist²≤2 de coord, i distintes de la posició actual.
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
  "Cost d'un moviment a casella buscant arribar a desti.
   Prefereix lleugerament caselles del color propi (+5 si és aliè),
   però NO bloqueja el moviment: el joc ja penalitza amb cooldown x3.
   Una penalització massa alta (ex: 100) faria que les bolles quedessin
   atrapades dins la seva zona de color i no explorassin mai.
   casella: casella de visió. desti: (x y). color-propi: 'r 'g o 'b."
  (let ((coord (agent-xyz999-c-coord casella)))
    (cond ((or (null coord) (null desti)) 1000000)
          (t (+ (* (agent-xyz999-dist-q coord desti) 10)
                ;; Preferència suau per color propi: +5 si és aliè.
                ;; Prou petit per no bloquejar l'exploració.
                (cond ((eq (agent-xyz999-c-color casella) color-propi) 0)
                      (t 5)))))))

(defun agent-xyz999-millor-mov (movibles desti color-propi best-coord best-cost)
  "Cerca greedy la casella movible de menor cost cap a desti.
   Retorna la coordenada (x y) de la millor casella, o nil si cap.
   movibles: llista de caselles movibles. desti: (x y).
   color-propi: 'r 'g o 'b. best-coord: millor coord fins ara. best-cost: cost mínim."
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

(defun agent-xyz999-prioritat-tret (casella equip color-propi dist)
  "Calcula la prioritat d'un tret a una casella des de rang dist.
   Retorna -1 si no s'ha de disparar; un enter positiu com a prioritat.
   Escala de prioritats (de major a menor):
     9000: kill shot base (2 colors, el nostre és el 3r → DESTRUÏDA!)
     5000: segon color a la base (1 color ja pintat)
     2000: primer color a la base (0 colors)
      800: kill shot bolla (2 colors, el nostre és el 3r)
      400: dany útil a bolla (li falta el nostre color)
      200: captura lab enemic o neutral
       -1: no disparar (fora rang, aliat, ja té el nostre color, buida)
   casella: casella de visió. equip: 'e1/'e2. color-propi: 'r/'g/'b. dist: dist²."
  (let* ((elem  (agent-xyz999-c-elem casella))
         (eq-c  (agent-xyz999-c-equip casella))
         (cp    (agent-xyz999-c-colors casella))
         (n-cp  (agent-xyz999-longitud cp)))
    (cond
      ;; Filtres bàsics: fora rang, no és terra, buida, o és aliada
      ((> dist 5) -1)
      ((not (eq (agent-xyz999-c-tipus casella) 'terra)) -1)
      ((null elem) -1)
      ((eq eq-c equip) -1)

      ;; BASE ENEMIGA: objectiu màxim
      ((eq elem 'base)
       (cond
         ;; Ja té el nostre color: inútil, no gastem cooldown
         ((agent-xyz999-membre color-propi cp) -1)
         ;; Té 2 colors i el nostre és el 3r → KILL SHOT!
         ((= n-cp 2) 9000)
         ;; Té 1 color: afegim el segon
         ((= n-cp 1) 5000)
         ;; Té 0 colors: primer cop
         (t 2000)))

      ;; BOLLA ENEMIGA
      ((eq elem 'bolla)
       (cond
         ;; Ja té el nostre color: inútil
         ((agent-xyz999-membre color-propi cp) -1)
         ;; Té 2 colors i el nostre destruirà la bolla!
         ((= n-cp 2) 800)
         ;; Dany útil: li falta el nostre color
         (t 400)))

      ;; LAB enemic o no capturat: capturar per pintura addicional
      ((eq elem 'lab)
       (cond ((not (eq eq-c equip)) 200)
             (t -1)))

      (t -1))))

(defun agent-xyz999-millor-tret (vis equip color-propi coord best-coord best-punt)
  "Cerca el millor objectiu per disparar en rang 5u² dins la visió vis.
   Retorna la coordenada de l'objectiu o nil si cap tret és útil.
   vis: llista caselles. equip: 'e1/'e2. color-propi: 'r/'g/'b.
   coord: posició actual. best-coord: millor coord fins ara. best-punt: prioritat màxima."
  (cond
    ((null vis) best-coord)
    (t (let* ((c    (car vis))
              (dist (agent-xyz999-dist-q coord (agent-xyz999-c-coord c)))
              (p    (agent-xyz999-prioritat-tret c equip color-propi dist)))
         (cond ((> p best-punt)
                (agent-xyz999-millor-tret (cdr vis) equip color-propi coord
                                          (agent-xyz999-c-coord c) p))
               (t
                (agent-xyz999-millor-tret (cdr vis) equip color-propi coord
                                          best-coord best-punt)))))))


;; ======================================================================
;; SECCIÓ 9: DECISIÓ DE LA BOLLA
;; ======================================================================

(defun agent-xyz999-decisio-bolla (coord equip color-propi tr-pintar tr-moure
                                    vis mem id ronda)
  "Cervell de la bolla: retorna la llista d'accions d'aquest torn.
   Sempre intenta tret (si cooldown<1) I moviment (si cooldown<1) al mateix torn.
   Tret i moviment són independents: ambdós es poden fer al mateix torn.
   coord: (x y). equip: 'e1/'e2. color-propi: 'r/'g/'b.
   tr-pintar, tr-moure: cooldowns (real o nil → tractat com 0).
   vis: visió. mem: memòria. id: id-unitat. ronda: enter."
  (let* (;; Cooldowns (nil = 0 per unitats noves)
         (tp (cond (tr-pintar tr-pintar) (t 0)))
         (tm (cond (tr-moure  tr-moure)  (t 0)))

         ;; TRET: cerca el millor objectiu en rang 5u² si el cooldown ho permet
         (tret-coord (cond ((< tp 1)
                            (agent-xyz999-millor-tret vis equip color-propi
                                                      coord nil -1))
                           (t nil)))
         (acc-tret (cond (tret-coord
                          (list (list 'pinta (list tret-coord))))
                         (t nil)))

         ;; MOVIMENT: greedy cap al destí de rol si el cooldown ho permet
         (desti   (agent-xyz999-desti-bolla mem id ronda coord))
         (acc-mou (cond ((< tm 1)
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

(defun agent-xyz999-decisio-base (coord vis mem pintura ronda)
  "La base crea una bolla per torn si té ≥ 50 pintura.
   Tria el color que millor destruirà la base enemiga:
     - 2 colors a la base enemiga → SEMPRE el kill color (l'únic útil).
     - 1 o 0 colors → rota per crear exèrcit divers.
   Spawn cap a la base enemiga (o labs) per guanyar posicionament.
   coord: (x y) base. vis: visió. mem: memòria. pintura: enter. ronda: enter."
  (cond
    ;; Sense prou pintura: no fa res
    ((< pintura 50) nil)
    (t (let* ((colors-be  (agent-xyz999-get 'colors-base-enemy mem))
              (base-enemy (agent-xyz999-get 'base-enemy mem))
              (labs       (agent-xyz999-get 'labs mem))
              ;; Color que destruirà la base enemiga (o equilibra l'exèrcit)
              (color      (agent-xyz999-tria-color colors-be ronda))
              ;; Destí de spawn: rotació de 8 direccions per evitar col·lisions a la porta
              (desti-sp   (cond
                            ;; Si sabem on és l'enemic, spawn cap a ell
                            (base-enemy base-enemy)
                            ;; Si no, rotem la direcció de spawn cada torn per no bloquejar la sortida
                            (t (let* ((dir8 '((1 0)(1 1)(0 1)(-1 1)(-1 0)(-1 -1)(0 -1)(1 -1)))
                                      (d    (agent-xyz999-nth-safe (rem ronda 8) dir8)))
                                 (list (+ (cond ((and coord (car coord)) (car coord)) (t 500)) (* (car d) 5))
                                       (+ (cond ((and coord (cadr coord)) (cadr coord)) (t 500)) (* (cadr d) 5)))))))
              ;; Caselles buides adjacents (rang spawn = 2u²)
              (movs       (agent-xyz999-movibles vis coord))
              ;; Casella de spawn: la adjacent cap al destí de spawn
              (spawn      (agent-xyz999-millor-mov movs desti-sp nil nil 1000000000)))
         (cond
           ((and spawn color)
            (list (list 'crea-bolla (list color spawn))))
           (t nil))))))


;; ======================================================================
;; SECCIÓ 11: PUNT D'ENTRADA PRINCIPAL
;; ======================================================================

(defun agent-xyz999 (dades)
  "Punt d'entrada de l'agent. Cridat pel controlador per cada unitat cada torn.
   Dades rebudes:
     (ronda equip pintura id-unitat tipus-unitat coordenada colors-pintat
      color-propi tr-pintar tr-moure visio memoria-compartida)
   Retorna: llista d'accions que sempre inclou 'escriu-memoria al davant.
   Passos:
     1. Actualitza la memòria compartida amb la visió d'aquest torn.
     2. Si som la base, registrem la nostra posició a base-ally.
     3. Decidim les accions segons el tipus d'unitat i rol.
     4. Retornem la memòria actualitzada + les accions."
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

         ;; 1. Actualitza memòria: bases, labs enemics, labs aliats
         (mem-vis (agent-xyz999-actualitza-mem vis mem-old equip))

         ;; 2. La base sempre registra la seva pròpia posició
         (mem-final (cond
                      ((eq tipus 'base)
                       (agent-xyz999-set 'base-ally coord mem-vis))
                      (t mem-vis)))

         ;; 3. Decidim les accions per tipus d'unitat
         (accions (cond
                    ((eq tipus 'base)
                     (agent-xyz999-decisio-base coord vis mem-final pintura ronda))
                    ((eq tipus 'bolla)
                     (agent-xyz999-decisio-bolla coord equip color-propi
                                                  tr-pintar tr-moure vis
                                                  mem-final id ronda))
                    (t nil))))

    ;; 4. Sempre escrivim la memòria (propagació entre totes les unitats) + accions
    (append (list (list 'escriu-memoria (list mem-final)))
            accions)))