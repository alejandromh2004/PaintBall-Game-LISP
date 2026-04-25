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
;; ESTRATÈGIA GENERAL ("Dominació per Capes"):
;;   L'agent implementa una estratègia militar orgànica de tres capes:
;;
;;   1. EXPLORACIÓ INTEL·LIGENT: Les bolles noves exploren en direccions
;;      disperses (no al mateix punt), usant un sistema de dispersió
;;      basat en l'ID + ronda per no agrupar-se mai.
;;
;;   2. ECONOMIA PRIMER: La base no creï bolles fins tenir >50 pintura,
;;      i priorirza els colors que FALTEN a la base enemiga. Si la base
;;      enemiga no es coneix, equilibra els tres colors.
;;
;;   3. ROLS DINÀMICS AMB FASES:
;;      - Fase EARLY (rondes 1-200): Explorar, capturar labs, evitar combat.
;;      - Fase MID (rondes 200-800): Atac coordinat, capturar labs, 
;;        roles d'atac/explorar equilibrats.
;;      - Fase LATE (rondes 800+): Atac total a la base enemiga.
;;      - Roles per ID: atacants, exploradors, defensors roten dinàmicament.
;;
;;   4. COMBAT INTEL·LIGENT:
;;      - Prioritat de tret: base enemiga (colors que falten) > bolla
;;        enemiga quasi morta > bolla enemiga > lab enemic.
;;      - MAI disparar a aliats.
;;      - Pinta la casella pròpia si no és del propi color (elimina penalització).
;;      - Fuig si quasi morta (2 colors pintats).
;;
;;   5. NAVEGACIÓ EFICIENT:
;;      - Prefereix caminar per caselles del propi color (evita x3 cooldown).
;;      - Greedy heurística cap al destí amb penalització de color.
;;      - Exploració amb vectors de dispersió per ID+ronda (no s'agrupen).
;;
;;   6. MEMÒRIA COMPARTIDA:
;;      - Guarda: posició base aliada, posició base enemiga, llista de labs,
;;        colors que ja té pintats la base enemiga.
;;      - Cada unitat llegeix la memòria i la pot ampliar.
;;
;; FUNCIONS AUXILIARS: Totes prefixades amb "agent-xyz999-".
;; ======================================================================


;; ======================================================================
;; SECCIÓ 1: MATEMÀTIQUES I UTILITATS
;; ======================================================================

(defun agent-xyz999-dist-q (c1 c2)
  "Distància euclidiana al quadrat entre dues coordenades (llistes de 2 enters)."
  (+ (* (- (car c1) (car c2)) (- (car c1) (car c2)))
     (* (- (cadr c1) (cadr c2)) (- (cadr c1) (cadr c2)))))

(defun agent-xyz999-abs (n)
  "Valor absolut d'un nombre."
  (cond ((< n 0) (- n)) (t n)))

(defun agent-xyz999-max2 (a b)
  "Màxim de dos nombres."
  (cond ((>= a b) a) (t b)))

(defun agent-xyz999-min2 (a b)
  "Mínim de dos nombres."
  (cond ((<= a b) a) (t b)))

(defun agent-xyz999-signum (n)
  "Retorna -1, 0 o 1 segons el signe de n."
  (cond ((> n 0) 1) ((< n 0) -1) (t 0)))

(defun agent-xyz999-membre-igual (elem llista)
  "Cerca elem a llista usant equal (per coordenades)."
  (cond ((null llista) nil)
        ((equal elem (car llista)) t)
        (t (agent-xyz999-membre-igual elem (cdr llista)))))

(defun agent-xyz999-longitud (llista)
  "Longitud d'una llista."
  (cond ((null llista) 0)
        (t (+ 1 (agent-xyz999-longitud (cdr llista))))))

(defun agent-xyz999-nth-safe (n llista)
  "Com nth però retorna nil si fora de rang."
  (cond ((null llista) nil)
        ((= n 0) (car llista))
        (t (agent-xyz999-nth-safe (- n 1) (cdr llista)))))

(defun agent-xyz999-compta-membres (elem llista)
  "Compta quantes vegades apareix elem a llista (amb equal)."
  (cond ((null llista) 0)
        ((equal (car llista) elem) (+ 1 (agent-xyz999-compta-membres elem (cdr llista))))
        (t (agent-xyz999-compta-membres elem (cdr llista)))))

(defun agent-xyz999-limita-llista (llista n)
  "Retorna els primers n elements de la llista (per no saturar memòria)."
  (cond ((null llista) nil)
        ((<= n 0) nil)
        (t (cons (car llista) (agent-xyz999-limita-llista (cdr llista) (- n 1))))))

(defun agent-xyz999-esborra-elem (elem llista)
  "Elimina totes les instàncies d'elem a llista (equal)."
  (cond ((null llista) nil)
        ((equal elem (car llista)) (agent-xyz999-esborra-elem elem (cdr llista)))
        (t (cons (car llista) (agent-xyz999-esborra-elem elem (cdr llista))))))


;; ======================================================================
;; SECCIÓ 2: GESTIÓ DE MEMÒRIA COMPARTIDA (A-lists pures)
;; ======================================================================

(defun agent-xyz999-get-mem (clau mem)
  "Retorna el valor associat a clau en la memòria (a-list). Nil si no existeix."
  (cond ((null mem) nil)
        ((eq (caar mem) clau) (cdar mem))
        (t (agent-xyz999-get-mem clau (cdr mem)))))

(defun agent-xyz999-set-mem (clau valor mem)
  "Insereix o actualitza la parella (clau . valor) a la memòria. Retorna nova mem."
  (cond ((null mem) (list (cons clau valor)))
        ((eq (caar mem) clau) (cons (cons clau valor) (cdr mem)))
        (t (cons (car mem) (agent-xyz999-set-mem clau valor (cdr mem))))))

(defun agent-xyz999-afegir-a-llista-mem (clau elem mem)
  "Afegeix elem a la llista guardada sota clau (si no hi és ja, per equal)."
  (let ((llista-actual (agent-xyz999-get-mem clau mem)))
    (cond ((agent-xyz999-membre-igual elem llista-actual) mem)
          (t (agent-xyz999-set-mem clau (cons elem llista-actual) mem)))))

(defun agent-xyz999-treure-de-llista-mem (clau elem mem)
  "Elimina elem de la llista guardada sota clau."
  (let ((llista-actual (agent-xyz999-get-mem clau mem)))
    (agent-xyz999-set-mem clau (agent-xyz999-esborra-elem elem llista-actual) mem)))


;; ======================================================================
;; SECCIÓ 3: ACCESSORS DE CASELLA (estructura del controlador)
;;
;;  Estructura casella terra: 
;;   (terra color-casella tipus-elem equip colors-pintat color-propi
;;    tr-pintar tr-moure id-unitat)
;;  Índexs en la VISIÓ:
;;   0=coord 1=tipus-casella 2=color-casella 3=tipus-elem 4=equip
;;   5=colors-pintat 6=color-propi 7=tr-pintar 8=tr-moure
;; ======================================================================

(defun agent-xyz999-vis-coord (c)       (nth 0 c))
(defun agent-xyz999-vis-tipus (c)       (nth 1 c))
(defun agent-xyz999-vis-color-terra (c) (nth 2 c))
(defun agent-xyz999-vis-element (c)     (nth 3 c))
(defun agent-xyz999-vis-equip (c)       (nth 4 c))
(defun agent-xyz999-vis-colors-p (c)    (nth 5 c))
(defun agent-xyz999-vis-color-propi (c) (nth 6 c))
(defun agent-xyz999-vis-tr-pintar (c)   (nth 7 c))
(defun agent-xyz999-vis-tr-moure (c)    (nth 8 c))


;; ======================================================================
;; SECCIÓ 4: PROCESSAMENT DE LA VISIÓ → ACTUALITZACIÓ MEMÒRIA
;; ======================================================================

(defun agent-xyz999-actualitza-mem-visio (visio mem equip)
  "Recorre la visió i actualitza la memòria amb base enemiga, labs i colors enemics."
  (cond ((null visio) mem)
        (t (let* ((c (car visio))
                  (coord (agent-xyz999-vis-coord c))
                  (tipus (agent-xyz999-vis-tipus c))
                  (elem (agent-xyz999-vis-element c))
                  (eq-c (agent-xyz999-vis-equip c))
                  (colors-p (agent-xyz999-vis-colors-p c))
                  
                  ;; Base enemiga: guardem posició i colors que ja té pintats
                  (mem1 (cond ((and (eq elem 'base) (not (eq eq-c equip)))
                               (agent-xyz999-set-mem 'colors-base-enemy colors-p
                                (agent-xyz999-set-mem 'base-enemy coord mem)))
                              ;; Base aliada
                              ((and (eq elem 'base) (eq eq-c equip))
                               (agent-xyz999-set-mem 'base-ally coord mem))
                              (t mem)))
                  
                  ;; Labs: afegim si no és nostre, treiem si ja és nostre
                  (mem2 (cond ((and (eq elem 'lab) (not (eq eq-c equip)) (eq tipus 'terra))
                               (agent-xyz999-afegir-a-llista-mem 'labs coord mem1))
                              ((and (eq elem 'lab) (eq eq-c equip))
                               (agent-xyz999-treure-de-llista-mem 'labs coord mem1))
                              (t mem1))))
             
             (agent-xyz999-actualitza-mem-visio (cdr visio) mem2 equip)))))


;; ======================================================================
;; SECCIÓ 5: AVALUACIÓ DE FASE (early/mid/late)
;; ======================================================================

(defun agent-xyz999-fase (ronda)
  "Retorna la fase estratègica: 'early (0-200), 'mid (200-800), 'late (800+)."
  (cond ((< ronda 200) 'early)
        ((< ronda 800) 'mid)
        (t 'late)))

(defun agent-xyz999-rol (id-unitat ronda)
  "Assigna un rol a la bolla: 'atacant, 'explorador, 'defensor.
   Els rols roten per fase per evitar comportament fix."
  (let* ((fase (agent-xyz999-fase ronda))
         (mod3 (rem (agent-xyz999-abs id-unitat) 3))
         (mod5 (rem (agent-xyz999-abs id-unitat) 5)))
    (cond 
      ;; Early: majoria exploradors, pocs defensors
      ((eq fase 'early)
       (cond ((= mod5 0) 'defensor)
             (t 'explorador)))
      ;; Mid: mix equilibrat
      ((eq fase 'mid)
       (cond ((= mod3 0) 'defensor)
             ((= mod3 1) 'atacant)
             (t 'explorador)))
      ;; Late: quasi tot atacants
      (t
       (cond ((= mod5 0) 'defensor)
             (t 'atacant))))))


;; ======================================================================
;; SECCIÓ 6: SISTEMA DE COLORS (lògica de quins colors falten)
;; ======================================================================

(defun agent-xyz999-colors-falten (colors-pintats)
  "Retorna la llista de colors (r g b) que NO estan a colors-pintats."
  (let ((tots '(r g b)))
    (agent-xyz999-filtra-no-presents tots colors-pintats)))

(defun agent-xyz999-filtra-no-presents (tots pintats)
  "Filtra de tots els que no estan a pintats."
  (cond ((null tots) nil)
        ((member (car tots) pintats) (agent-xyz999-filtra-no-presents (cdr tots) pintats))
        (t (cons (car tots) (agent-xyz999-filtra-no-presents (cdr tots) pintats)))))

(defun agent-xyz999-color-mes-util (colors-base-enemy color-propi)
  "Retorna el color de bolla més útil per destruir la base enemiga.
   El color de la bolla JA el té la base (color-propi de bolla). 
   Però si la base enemiga no té el color de la nostra bolla, 
   la nostra bolla ES útil atacant."
  ;; Si la base enemiga no té el nostre color, el nostre color és útil
  (cond ((not (member color-propi colors-base-enemy)) color-propi)
        (t nil)))

(defun agent-xyz999-tria-color-bolla (colors-base-enemy pintura ronda)
  "Tria quin color de bolla crear per maximitzar el dany a la base enemiga.
   Si es coneix la base enemiga, crea el color que li falta.
   Sinó, alterna r-g-b cada torn per diversificar l'exèrcit."
  (let ((falten (agent-xyz999-colors-falten colors-base-enemy)))
    (cond 
      ;; Si hi ha colors que falten a la base enemiga, crea'n un dels que falten
      ((and colors-base-enemy (not (null falten))) (car falten))
      ;; Si la base enemiga és desconeguda, alternem r-g-b cada torn
      (t (nth (rem ronda 3) '(r g b))))))


;; ======================================================================
;; SECCIÓ 7: FILTRATGE DE CASELLES MOVIBLES
;; ======================================================================

(defun agent-xyz999-es-movible (casella coord-actual)
  "Retorna t si la casella és terra buida dins rang de moviment (dist² ≤ 2)."
  (and (not (null casella))
       (>= (agent-xyz999-longitud casella) 2)
       (eq (nth 1 casella) 'terra)
       (null (nth 3 casella))   ; Ha d'estar buida (cap element)
       (<= (agent-xyz999-dist-q (nth 0 casella) coord-actual) 2)
       (not (equal (nth 0 casella) coord-actual))))

(defun agent-xyz999-filtra-movibles (visio coord-actual)
  "Retorna les caselles de la visió on la bolla es pot moure."
  (cond ((null visio) nil)
        ((agent-xyz999-es-movible (car visio) coord-actual)
         (cons (car visio) (agent-xyz999-filtra-movibles (cdr visio) coord-actual)))
        (t (agent-xyz999-filtra-movibles (cdr visio) coord-actual))))

(defun agent-xyz999-casella-propi-color (casella color-propi)
  "Retorna t si la casella és del color propi de la bolla."
  (eq (agent-xyz999-vis-color-terra casella) color-propi))


;; ======================================================================
;; SECCIÓ 8: SISTEMA DE NAVEGACIÓ GREEDY
;; ======================================================================

(defun agent-xyz999-cost-pas (casella desti color-propi visited ignora-rastre)
  "Cost heurístic d'un pas: distància al destí + penalització si no és del propi color.
   Si ignora-rastre és nil, aplica penalització de visited per explorar."
  (let* ((coord (agent-xyz999-vis-coord casella))
         (color-terra (agent-xyz999-vis-color-terra casella))
         (dist (agent-xyz999-dist-q coord desti))
         ;; Penalització alta si terra d'altre color (x3 cooldown real)
         (pena-color (cond ((eq color-terra color-propi) 0) (t 500)))
         ;; Penalització de rastre: Només s'aplica si NO estem en "mode atac/rush"
         (pena-rastre (cond ((and (not ignora-rastre)
                                  (agent-xyz999-membre-igual coord visited)) 2000)
                            (t 0))))
    (+ (* dist 10) pena-color pena-rastre)))

(defun agent-xyz999-millor-pas (movibles desti color-propi visited ignora-rastre millor-coord millor-cost)
  "Cerca greedy la casella movible de menor cost cap al destí."
  (cond ((null movibles) millor-coord)
        (t (let* ((casella (car movibles))
                  (cost (agent-xyz999-cost-pas casella desti color-propi visited ignora-rastre)))
             (cond ((< cost millor-cost)
                    (agent-xyz999-millor-pas (cdr movibles) desti color-propi visited ignora-rastre
                                             (agent-xyz999-vis-coord casella) cost))
                   (t
                    (agent-xyz999-millor-pas (cdr movibles) desti color-propi visited ignora-rastre
                                             millor-coord millor-cost)))))))


;; ======================================================================
;; SECCIÓ 9: SISTEMA D'EXPLORACIÓ DISPERSA
;; ======================================================================

(defun agent-xyz999-vector-exploracio (id-unitat ronda coord-base)
  "Genera un vector d'exploració ÚNIC per a cada bolla en cada fase.
   Usa l'ID i la ronda per dispersar les bolles en 8 directions + variació.
   Això evita que totes les bolles vagin al mateix punt."
  (let* (;; 8 direccions base (octants)
         (dir8 (list (list  1  0) (list  1  1) (list  0  1) (list -1  1)
                     (list -1  0) (list -1 -1) (list  0 -1) (list  1 -1)))
         ;; Seleccionem una direcció combinant ID i fase de ronda
         (fase-idx (cond ((< ronda 200) 0) ((< ronda 500) 1) ((< ronda 800) 2) (t 3)))
         ;; Rotació per fase per forçar canvis d'estratègia
         (idx-dir (rem (+ (agent-xyz999-abs id-unitat) (* fase-idx 2)) 8))
         (dir (agent-xyz999-nth-safe idx-dir dir8))
         ;; Distància de 300-600 unitats per ser fora del rang de visió
         (dist (+ 300 (* (rem (agent-xyz999-abs id-unitat) 4) 100)))
         (bx (cond (coord-base (car coord-base)) (t 500)))
         (by (cond (coord-base (cadr coord-base)) (t 500))))
    (list (+ bx (* (car dir) dist))
          (+ by (* (cadr dir) dist)))))

(defun agent-xyz999-desti-bolla (coord mem id-unitat ronda equip visio rol)
  "Decideix cap a on ha d'anar una bolla.
   PRIORITAT: Base enemiga > Objectiu visible > Labs en memòria > Explorar."
  (let* ((base-enemy (agent-xyz999-get-mem 'base-enemy mem))
         (labs       (agent-xyz999-get-mem 'labs mem))
         (proper-vis (agent-xyz999-busca-objectiu-visio visio equip coord))
         (n-labs (agent-xyz999-longitud labs)))
    (cond
      ;; 1. Si coneixem la BASE ENEMIGA, anem-hi (Rush)!
      (base-enemy base-enemy)
      
      ;; 2. Si veiem quelcom interessant (Base o Lab) ara mateix
      (proper-vis proper-vis)
      
      ;; 3. Si coneixem laboratoris, anem-hi per ID per dispersar-nos
      ((and labs (> n-labs 0))
       (agent-xyz999-nth-safe (rem (agent-xyz999-abs id-unitat) n-labs) labs))
      
      ;; 4. Altrament, exploració dispersa
      (t (agent-xyz999-vector-exploracio id-unitat ronda coord)))))

(defun agent-xyz999-filtra-visio-tipus (visio tipus equip)
  "Filtra caselles de la visió per tipus d'element (enemic o neutral)."
  (cond ((null visio) nil)
        (t (let* ((c (car visio))
                  (elem (agent-xyz999-vis-element c))
                  (eq-c (agent-xyz999-vis-equip c)))
             (cond ((and (eq elem tipus) (not (eq eq-c equip)))
                    (cons c (agent-xyz999-filtra-visio-tipus (cdr visio) tipus equip)))
                   (t (agent-xyz999-filtra-visio-tipus (cdr visio) tipus equip)))))))

(defun agent-xyz999-busca-objectiu-visio (visio equip coord-actual)
  "Busca el millor objectiu rellevant en visió. Prioritat: BASE > LAB."
  (let ((bases (agent-xyz999-filtra-visio-tipus visio 'base equip))
        (labs  (agent-xyz999-filtra-visio-tipus visio 'lab equip)))
    (cond (bases (agent-xyz999-vis-coord (car bases)))
          (labs  (agent-xyz999-vis-coord (car labs)))
          (t nil))))


;; ======================================================================
;; SECCIÓ 10: SISTEMA DE COMBAT (TRETS)
;; ======================================================================

(defun agent-xyz999-punts-tret (casella equip color-propi coord-actual colors-base-enemy)
  "Calcula la prioritat d'un tret a una casella (rang ≤ 5u²).
   Retorna -1 si no s'ha de disparar, altrament un enter positiu."
  (let* ((coord (agent-xyz999-vis-coord casella))
         (dist (agent-xyz999-dist-q coord-actual coord))
         (elem (agent-xyz999-vis-element casella))
         (eq-c (agent-xyz999-vis-equip casella))
         (colors-p (agent-xyz999-vis-colors-p casella))
         (color-pr (agent-xyz999-vis-color-propi casella)))
    (cond
      ;; Fora de rang de tret (5u²)
      ((> dist 5) -1)
      ;; Casella d'aigua: no es pot pintar
      ((eq (agent-xyz999-vis-tipus casella) 'aigua) -1)
      ;; Casella pròpia: no disparar mai a la pròpia casella en combat
      ;; (excepció: pintar la pròpia casella per treure penalització, gestionat a part)
      ((null elem) -1)
      ;; MAI disparar a aliats
      ((eq eq-c equip) -1)
      
      ;; BASE ENEMIGA: prioritat màxima si el nostre color li falta
      ((eq elem 'base)
       (cond ((not (member color-propi colors-p))
              ;; Quants colors li falten? Més prop de morir = més prioritat
              (let ((n-colors-te (agent-xyz999-longitud colors-p)))
                (cond ((= n-colors-te 2) 10000) ; 3r color = mort imminent!
                      ((= n-colors-te 1) 5000)
                      (t 2000))))
             ;; La base ja té el nostre color: no cal pintar-la
             (t -1)))
      
      ;; BOLLA ENEMIGA: prioritat alta si quasi morta (2 colors = morirà amb 1 tret)
      ((eq elem 'bolla)
       (cond
         ;; Bolla quasi morta: 1 tret la destrueix
         ((and (member color-propi colors-p)
               (= (agent-xyz999-longitud colors-p) 1)) -1) ; ja té el nostre color
         ((= (agent-xyz999-longitud colors-p) 1) 800) ; li falta 1 color (pot ser el nostre)
         ((not (member color-propi colors-p)) 400)    ; li falta el nostre
         (t -1)))
      
      ;; LAB ENEMIC o neutral: prioritat mitja
      ((eq elem 'lab)
       (cond ((not (eq eq-c equip)) 300)
             (t -1)))
      
      (t -1))))

(defun agent-xyz999-millor-tret (visio equip color-propi coord-actual colors-base-enemy
                                  millor-coord millor-punt)
  "Cerca el millor objectiu per disparar. Retorna la coordenada o nil."
  (cond ((null visio) millor-coord)
        (t (let ((punt (agent-xyz999-punts-tret (car visio) equip color-propi
                                                 coord-actual colors-base-enemy)))
             (cond ((> punt millor-punt)
                    (agent-xyz999-millor-tret (cdr visio) equip color-propi coord-actual
                                              colors-base-enemy
                                              (agent-xyz999-vis-coord (car visio)) punt))
                   (t
                    (agent-xyz999-millor-tret (cdr visio) equip color-propi coord-actual
                                              colors-base-enemy millor-coord millor-punt)))))))


;; ======================================================================
;; SECCIÓ 11: LÒGICA DE PERILL I FUGIDA
;; ======================================================================

(defun agent-xyz999-es-en-perill (colors-pintat)
  "Retorna t si la bolla té 2 colors pintats (un tret més la destrueix)."
  (>= (agent-xyz999-longitud colors-pintat) 2))

(defun agent-xyz999-desti-fugida (coord mem equip)
  "Retorna cap on ha de fugir una bolla en perill: cap a la base aliada."
  (let ((base-ally (agent-xyz999-get-mem 'base-ally mem)))
    (cond (base-ally base-ally)
          ;; Si no coneix la base aliada, va cap al centre del mapa conegut
          (t (list (+ (car coord) (* -5 (agent-xyz999-signum (car coord))))
                   (+ (cadr coord) (* -5 (agent-xyz999-signum (cadr coord)))))))))


;; ======================================================================
;; SECCIÓ 12: DECISIÓ DE LA BASE
;; ======================================================================

(defun agent-xyz999-millor-spawn (movibles desti visited)
  "Troba la casella de spawn més propera al destí."
  (cond ((null movibles) nil)
        (t (agent-xyz999-millor-pas movibles desti 'cap visited t nil 1000000000))))

(defun agent-xyz999-decisio-base (coord visio mem equip pintura ronda)
  "Decisió de la base: crea una bolla si té prou pintura.
   Tria el color més útil contra la base enemiga.
   Spawn cap a la direcció de l'enemic o d'un lab."
  (cond
    ;; No prou pintura: no crea res
    ((< pintura 50) nil)
    
    (t (let* ((movibles (agent-xyz999-filtra-movibles visio coord))
              (base-enemy (agent-xyz999-get-mem 'base-enemy mem))
              (labs (agent-xyz999-get-mem 'labs mem))
              (colors-base-enemy (agent-xyz999-get-mem 'colors-base-enemy mem))
              
              ;; Destí preferit per posicionar el spawn
              (desti-spawn (cond (base-enemy base-enemy)
                                 (labs (car labs))
                                 (t (list (+ (car coord) 500) (+ (cadr coord) 500)))))
              
              ;; Color més útil per destruir la base enemiga
              (color-nou (agent-xyz999-tria-color-bolla colors-base-enemy pintura ronda))
              
              ;; Millor posició de spawn: cap al destí (la base no necessita evitar visited, però el paràmetre és necessari)
              (coord-spawn (agent-xyz999-millor-spawn movibles desti-spawn (agent-xyz999-get-mem 'visited mem))))
          
          (cond ((and coord-spawn color-nou)
                 (list (list 'crea-bolla (list color-nou coord-spawn))))
                (coord-spawn
                 ;; Fallback: color r si res més
                 (list (list 'crea-bolla (list 'r coord-spawn))))
                (t nil))))))


;; ======================================================================
;; SECCIÓ 13: DECISIÓ DE LA BOLLA
;; ======================================================================

(defun agent-xyz999-casella-actual-a-visio (visio coord-actual)
  "Cerca a la visió la casella que correspon a la posició actual de la bolla."
  (cond ((null visio) nil)
        ((equal (agent-xyz999-vis-coord (car visio)) coord-actual) (car visio))
        (t (agent-xyz999-casella-actual-a-visio (cdr visio) coord-actual))))

(defun agent-xyz999-decisio-bolla (coord equip color-propi colors-pintat
                                    tr-pintar tr-moure visio mem id-unitat ronda)
  "Cervell de la bolla. Decideix trets i moviment en un sol torn.
   Lògica:
   1. Si en perill: fuig prioritàriament.
   2. Tret: cerca el millor objectiu al rang 5u².
   3. Pinta la pròpia casella si no és del color propi (redueix penalitzacions).
   4. Moviment: cap al destí segons el rol (evitant zones visitades recents).
  "
  (let* ((temps-pintar (cond (tr-pintar tr-pintar) (t 0)))
         (temps-moure  (cond (tr-moure tr-moure) (t 0)))
         (en-perill    (agent-xyz999-es-en-perill colors-pintat))
         (colors-base-enemy (agent-xyz999-get-mem 'colors-base-enemy mem))
         (visited      (agent-xyz999-get-mem 'visited mem))
         (rol          (agent-xyz999-rol id-unitat ronda))
         
         ;; --- TRET (si cooldown de pintar < 1) ---
         (tret-coord (cond ((< temps-pintar 1)
                            (agent-xyz999-millor-tret visio equip color-propi coord
                                                      colors-base-enemy nil -1))
                           (t nil)))
         (acc-tret (cond (tret-coord (list (list 'pinta (list tret-coord)))) (t nil)))
         
         ;; --- PINTA LA PRÒPIA CASELLA (si no és del propi color i no hem ja disparat) ---
         (casella-actual (agent-xyz999-casella-actual-a-visio visio coord))
         (color-terra-actual (cond (casella-actual 
                                    (agent-xyz999-vis-color-terra casella-actual))
                                   (t color-propi)))
         (pinta-terra (cond ((and (null tret-coord)
                                  (< temps-pintar 1)
                                  casella-actual
                                  (not (eq color-terra-actual color-propi)))
                             (list (list 'pinta (list coord))))
                            (t nil)))
         (acc-pinta-terra (cond (pinta-terra pinta-terra) (t nil)))
         
         ;; --- MOVIMENT ---
         (desti (cond (en-perill (agent-xyz999-desti-fugida coord mem equip))
                      (t (agent-xyz999-desti-bolla coord mem id-unitat ronda equip visio rol))))
         
         ;; Si tenim un destí conegut (base enemiga o lab), ignorem el rastre per rushejar
         (ignora-rastre (cond (en-perill nil)
                              ((agent-xyz999-get-mem 'base-enemy mem) t)
                              ((agent-xyz999-busca-objectiu-visio visio equip coord) t)
                              (t nil)))
         
         (acc-mou (cond ((< temps-moure 1)
                         (let* ((movibles (agent-xyz999-filtra-movibles visio coord))
                                (millor (agent-xyz999-millor-pas movibles desti color-propi visited ignora-rastre nil 1000000000)))
                           (cond (millor (list (list 'mou (list millor))))
                                 (t nil))))
                        (t nil))))
    
    ;; Combina totes les accions: tret (o pinta terra) + moviment
    (append acc-tret acc-pinta-terra acc-mou)))


;; ======================================================================
;; SECCIÓ 14: PUNT D'ENTRADA PRINCIPAL
;; ======================================================================

(defun agent-xyz999 (dades)
  "Punt d'entrada de l'agent. Rep les dades del controlador i retorna accions.
   Dades: (ronda equip pintura id-unitat tipus-unitat coordenada colors-pintat
           color-propi tr-pintar tr-moure visio memoria-compartida)"
  (let* ((ronda      (nth 0 dades))
         (equip      (nth 1 dades))
         (pintura    (nth 2 dades))
         (id-unitat  (nth 3 dades))
         (tipus      (nth 4 dades))
         (coord      (nth 5 dades))
         (colors-p   (nth 6 dades))
         (color-propi (nth 7 dades))
         (tr-pintar  (nth 8 dades))
         (tr-moure   (nth 9 dades))
         (visio      (nth 10 dades))
         (mem-old    (nth 11 dades))
         
         ;; 1. Actualitzem la memòria amb el que veiem
         (mem-1 (agent-xyz999-actualitza-mem-visio visio mem-old equip))
         
         ;; 2. Afegim la nostra posició actual al rastre (visited) per no trepitjar-nos
         ;;    Limitem la llista a 100 elements per no matar el rendiment
         (visited-old (agent-xyz999-get-mem 'visited mem-1))
         (mem-2 (agent-xyz999-set-mem 'visited 
                                      (cons coord (agent-xyz999-limita-llista visited-old 100))
                                      mem-1))

         ;; 3. Si soc la base, registro la meva posició per als defensors
         (mem-nova (cond ((eq tipus 'base)
                          (agent-xyz999-set-mem 'base-ally coord mem-2))
                         (t mem-2)))
         
         ;; 3. Decidim les accions
         (accions (cond
                    ((eq tipus 'base)
                     (agent-xyz999-decisio-base coord visio mem-nova equip pintura ronda))
                    ((eq tipus 'bolla)
                     (agent-xyz999-decisio-bolla coord equip color-propi colors-p
                                                  tr-pintar tr-moure visio mem-nova
                                                  id-unitat ronda))
                    (t nil))))
    
    ;; 4. Sempre escrivim la memòria actualitzada, seguida de les accions
    (append (list (list 'escriu-memoria (list mem-nova)))
            accions)))