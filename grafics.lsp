;; ======================================================================
;; PRÀCTICA FINAL LLENGUATGES DE PROGRAMACIÓ - LISP - PAINTBALL
;; ======================================================================
;; Estudiants: ABC, XYZ
;; Data: 25/04/2026
;; Assignatura: Llenguatges de Programació (LP)
;; Grup: <Indicar Grup>
;; Professors: <Indicar Professors>
;; Convocatòria: Primera Convocatòria (Ordinària)
;;
;; ----------------------------------------------------------------------
;; FITXER: grafics.lsp
;; DESCRIPCIÓ: Mòdul de representació gràfica complet de la partida.
;;
;; ASPECTES OPCIONALS IMPLEMENTATS (apartat "Mòdul gràfic"):
;;   - Color del qual está pintada cada casella (fons tenyit per color-casella).
;;   - Barres de temps de recuperació (cooldown) de pintar i moure per bolles.
;;   - HUD visual superior: indicadors d'equip actiu i barres de pintura.
;;   - Optimització: cls + redibuix complet cada torn (adequat per la mida).
;;
;; DISSENY FUNCIONAL:
;;   Totes les funcions són pures. El dibuix s'obté passant l'estat com a
;;   paràmetre, sense mutació ni reassignació.
;;
;; ESTRUCTURA DE CASELLA (terra):
;;   (terra color-casella element equip colors-pintat color-propi
;;    tr-pintar tr-moure id-unitat)
;;   index: 0       1             2      3     4            5
;;          6        7       8
;;
;; CANVI NECESSARI EN paintball.lsp:
;;   Substituir:
;;     (cond ((<= skip-visual 0) (dibuixa-mapa mapa)))
;;   Per:
;;     (cond ((<= skip-visual 0)
;;            (dibuixa-mapa mapa ronda
;;                          (cond ((= (mod ronda 2) 1) 'e1) (t 'e2))
;;                          pint-e1 pint-e2)))
;; ======================================================================


;; ======================================================================
;; SECCIÓ 1 – PALETA DE COLORS
;; ======================================================================

(defun gr-color-vermell () (color 215 35 35))    ; Vermell pur (r)
(defun gr-color-verd ()    (color 35 155 45))    ; Verd natural (g)
(defun gr-color-blau ()    (color 45 75 220))    ; Blau pur (b)
(defun gr-color-negre ()   (color 0   0   0))   ; Negre (equip 1)
(defun gr-color-blanc ()   (color 255 255 255))  ; Blanc (equip 2)
(defun gr-color-gris ()    (color 128 128 128))  ; Gris neutre (lab lliure)
(defun gr-color-aigua ()   (color 55 120 210))   ; Blau de l'aigua
(defun gr-color-hud ()     (color 18 18 28))     ; Fons del HUD

;; Aplica el color de pintura (vermell/verd/blau reals, no neon).
;; Paràmetres:
;;   c - símbol 'r, 'g o 'b
(defun gr-aplica-color-pintura (c)
  (cond ((eq c 'r) (gr-color-vermell))
        ((eq c 'g) (gr-color-verd))
        ((eq c 'b) (gr-color-blau))
        (t         (gr-color-gris))))

;; Aplica el color de vora d'equip: e1=negre, e2=blanc.
;; Paràmetres:
;;   e - símbol 'e1 o 'e2
(defun gr-aplica-color-equip (e)
  (cond ((eq e 'e1) (gr-color-negre))
        ((eq e 'e2) (gr-color-blanc))
        (t          (gr-color-gris))))

;; Color contrastat per a símbols sobre el cos d'una unitat d'equip e.
;; Paràmetres:
;;   e - símbol 'e1 o 'e2
(defun gr-aplica-color-equip-contrast (e)
  (cond ((eq e 'e1) (gr-color-blanc))
        (t          (gr-color-negre))))

;; Fons tenyit d'una casella de terra pintada del color c.
;; Implementa la funcionalitat opcional de mostrar el color de la casella.
;; Paràmetres:
;;   c - símbol 'r, 'g, 'b o nil
(defun gr-color-fons-casella (c)
  (cond ((eq c 'r) (color 255 210 210))   ; Vermell suau
        ((eq c 'g) (color 210 245 210))   ; Verd suau
        ((eq c 'b) (color 210 220 255))   ; Blau suau
        (t         (color 225 225 225)))) ; Gris neutre (sense pintura)


;; ======================================================================
;; SECCIÓ 2 – PRIMITIVES DE DIBUIX
;; ======================================================================

;; Dibuixa un rectangle ple de w×h píxels a la posició absoluta (x, y).
;; Utilitza omple-linies (de funciones_auxiliares.lsp) passant h-1 perquè
;; aquella funció dibuixa files-restants+1 línies (condició < 0).
;; Paràmetres:
;;   x, y - cantonada superior-esquerra
;;   w, h - amplada i alçada en píxels
(defun gr-fill (x y w h)
  (cond ((or (<= w 0) (<= h 0)) nil)
        (t (move x y)
           (omple-linies w (- h 1)))))

;; Dibuixa el contorn d'un rectangle de w×h a la posició (x, y).
;; Paràmetres:
;;   x, y - cantonada superior-esquerra
;;   w, h - amplada i alçada en píxels
(defun gr-stroke (x y w h)
  (cond ((or (<= w 0) (<= h 0)) nil)
        (t (move x y)
           (drawrel w 0)
           (drawrel 0 h)
           (drawrel (- w) 0)
           (drawrel 0 (- h)))))

;; Dibuixa una línia horitzontal d'amplada w a (x, y).
;; Paràmetres:
;;   x, y - punt d'inici
;;   w    - amplada en píxels
(defun gr-linia-h (x y w)
  (cond ((<= w 0) nil)
        (t (move x y)
           (drawrel w 0))))

;; Dibuixa un triangle ple (△) a la posició (tx, ty) amb mides (tw × th).
;; Utilitza omple-tri de funciones_auxiliares.lsp.
;; Paràmetres:
;;   tx, ty   - cantonada superior-esquerra del bounding box
;;   tw, th   - amplada i alçada del triangle
(defun gr-triangle-ple (tx ty tw th)
  (cond ((or (<= tw 0) (<= th 0)) nil)
        (t (move tx ty)
           (omple-tri tw th 0))))

;; Dibuixa el contorn d'un triangle (△) a la posició (tx, ty).
;; Paràmetres: igual que gr-triangle-ple
(defun gr-triangle-contorn (tx ty tw th)
  (cond ((or (<= tw 0) (<= th 0)) nil)
        (t (move tx ty)
           (triangle tw th))))


;; ======================================================================
;; SECCIÓ 3 – MARKS DE DANY (COLORS PINTATS)
;; ======================================================================
;;
;; Cada color pintat es mostra com un quadradet de (md × md) píxels
;; a una cantonada fixada de la casella:
;;   r = cantonada superior-esquerra
;;   g = cantonada superior-dreta
;;   b = cantonada inferior-esquerra
;; ======================================================================

;; Calcula la mida del marc de dany en funció de la mida de casella m.
;; Paràmetres:
;;   m - mida de la casella en píxels
(defun gr-mida-marc (m)
  (max 2 (min 5 (round (/ m 3)))))

;; Dibuixa un únic marc de dany de color c a la cantonada corresponent.
;; Paràmetres:
;;   c      - símbol 'r, 'g o 'b
;;   bx, by - cantonada superior-esquerra de la casella
;;   m      - mida de la casella
;;   md     - mida del marc de dany
(defun gr-dibuixa-marc (c bx by m md)
  (gr-aplica-color-pintura c)
  (cond ((eq c 'r) (gr-fill bx by md md))
        ((eq c 'g) (gr-fill (- (+ bx m) md) by md md))
        ((eq c 'b) (gr-fill bx (- (+ by m) md) md md))))

;; Dibuixa tots els marks de dany d'una llista de colors pintats.
;; Paràmetres:
;;   colors - llista de símbols ('r 'g 'b)
;;   bx, by - cantonada superior-esquerra de la casella
;;   m      - mida de la casella
(defun gr-dibuixa-danys (colors bx by m)
  (let ((md (gr-mida-marc m)))
    (cond ((null colors) nil)
          (t (gr-dibuixa-marc (car colors) bx by m md)
             (gr-dibuixa-danys (cdr colors) bx by m)))))


;; ======================================================================
;; SECCIÓ 4 – BARRES DE COOLDOWN (ASPECTE OPCIONAL)
;; ======================================================================
;;
;; Cada bolla mostra dues barres a la part inferior de la seva casella:
;;   - Barra superior: cooldown de moure (tr-moure), color taronja
;;   - Barra inferior: cooldown de pintar (tr-pintar), color vermell
;; Quan el cooldown és 0, la barra no es dibuixa (a punt per actuar).
;; ======================================================================

;; Dibuixa una barra de cooldown horitzontal.
;; Paràmetres:
;;   tr     - valor actual del cooldown (real o nil)
;;   tr-max - valor màxim de referència per a escalar la barra
;;   px, py - posició de la barra
;;   bw, bh - amplada i alçada de la barra en píxels
;;   c-r, c-g, c-b - color RGB de la part plena
(defun gr-barra-cooldown (tr tr-max px py bw bh c-r c-g c-b)
  (cond ((or (null tr) (<= tr 0)) nil)
        (t
         (let ((ple (max 1 (round (/ (* bw (min tr (float tr-max)))
                                    (float tr-max))))))
           ;; Fons fosc de la barra (indica el màxim)
           (color 55 55 55)
           (gr-fill px py bw bh)
           ;; Part plena (proporcional al cooldown restant)
           (color c-r c-g c-b)
           (gr-fill px py ple bh)))))

;; Dibuixa les dues barres de cooldown d'una bolla.
;; Paràmetres:
;;   tr-pintar - temps de recuperació de l'acció pintar
;;   tr-moure  - temps de recuperació de l'acció moure
;;   bx, by    - cantonada superior-esquerra de la casella
;;   m         - mida de la casella
(defun gr-dibuixa-cooldowns (tr-pintar tr-moure bx by m)
  (cond ((< m 8) nil)  ; Caselles massa petites: no dibuixem barres
        (t
         (let ((bw (- m 2))
               (px (+ bx 1)))
           ;; Barra de moure: taronja, damunt, cooldown màx 4 (diagonal+penal)
           (gr-barra-cooldown tr-moure 4.3 px (- (+ by m) 5) bw 2
                              200 120 30)
           ;; Barra de pintar: vermell, a baix, cooldown màx 9 (triple penal)
           (gr-barra-cooldown tr-pintar 9 px (- (+ by m) 2) bw 2
                              200 30 30)))))


;; ======================================================================
;; SECCIÓ 5 – DIBUIX DE CADA TIPUS D'ELEMENT
;; ======================================================================

;; --- 5a. BASE ---
;;
;; Dibuixa una base com un quadrat gran amb una creu interior.
;; L'equip es diferencia pel color de farcit: e1=negre, e2=blanc.
;; La creu interior és del color contrastat.
;; Marks de dany als cantons externs.
;;
;; Paràmetres:
;;   bx, by       - cantonada superior-esquerra de la casella
;;   m            - mida de la casella
;;   equip        - 'e1 o 'e2
;;   colors-pintat - llista de colors de dany rebuts
(defun gr-dibuixa-base (bx by m equip colors-pintat)
  (let* ((pad  (max 1 (round (/ m 8))))   ; Marge exterior de la base
         (sz   (- m (* 2 pad)))           ; Mida del cos de la base
         (ix   (+ bx pad))
         (iy   (+ by pad))
         ;; Centre del cos per la creu
         (cx   (+ ix (round (/ sz 2))))
         (cy   (+ iy (round (/ sz 2))))
         ;; Gruix de la creu (mínim 1px, proporcional)
         (gc   (max 1 (round (/ sz 5)))))

    ;; Cos de la base: quadrat del color d'equip
    (gr-aplica-color-equip equip)
    (gr-fill ix iy sz sz)

    ;; Vora doble per a visibilitat: 2 línies de contorn
    (gr-aplica-color-equip-contrast equip)
    (gr-stroke ix iy sz sz)
    (cond ((>= sz 6)
           (gr-stroke (+ ix 1) (+ iy 1) (- sz 2) (- sz 2))))

    ;; Creu interior del color contrastat
    (gr-aplica-color-equip-contrast equip)
    (gr-fill (- cx (round (/ gc 2))) iy gc sz)   ; Vertical
    (gr-fill ix (- cy (round (/ gc 2))) sz gc)   ; Horitzontal

    ;; Marks de dany als cantons (DAMUNT del cos, per visibilitat)
    (gr-dibuixa-danys colors-pintat bx by m)))


;; --- 5b. BOLLA ---
;;
;; Dibuixa una bolla com un quadrat interior del color propi,
;; amb una vora exterior del color d'equip (e1=negre, e2=blanc).
;; Els marks de dany es dibuixen als cantons de la casella.
;; Les barres de cooldown es dibuixen a la part inferior.
;;
;; Paràmetres:
;;   bx, by        - cantonada superior-esquerra de la casella
;;   m             - mida de la casella
;;   equip         - 'e1 o 'e2
;;   color-propi   - 'r, 'g o 'b (color de la bolla)
;;   colors-pintat - llista de colors de dany rebuts
;;   tr-pintar     - cooldown de pintar (real o nil)
;;   tr-moure      - cooldown de moure (real o nil)
(defun gr-dibuixa-bolla (bx by m equip color-propi colors-pintat tr-pintar tr-moure)
  (let* ((pad   (max 2 (round (/ m 4)))) ; Marge per la vora d'equip
         (inner (max 2 (- m (* 2 pad)))) ; Mida interior del cos
         (ix    (+ bx pad))
         (iy    (+ by pad)))

    ;; Vora d'equip: quadrat 1px més gran que el cos (amb color d'equip)
    (gr-aplica-color-equip equip)
    (gr-fill (- ix 1) (- iy 1) (+ inner 2) (+ inner 2))

    ;; Cos de la bolla: color propi (vermell, verd o blau real)
    (gr-aplica-color-pintura color-propi)
    (gr-fill ix iy inner inner)

    ;; Barres de cooldown a la part inferior (aspecte opcional)
    (gr-dibuixa-cooldowns tr-pintar tr-moure bx by m)

    ;; Marks de dany als cantons (damunt de tot)
    (gr-dibuixa-danys colors-pintat bx by m)))


;; --- 5c. LABORATORI ---
;;
;; Dibuixa un laboratori com un triangle (△) centrat a la casella.
;; Gris=no capturat, negre=equip 1, blanc=equip 2.
;; El contorn és del color contrastat per a millor visibilitat.
;;
;; Paràmetres:
;;   bx, by - cantonada superior-esquerra de la casella
;;   m      - mida de la casella
;;   equip  - 'e1, 'e2 o nil (si no capturat)
(defun gr-dibuixa-lab (bx by m equip)
  (let* ((pad (max 1 (round (/ m 5))))   ; Marge del triangle
         (tw  (max 3 (- m (* 2 pad))))   ; Amplada del triangle
         (th  (max 3 (- m (* 2 pad))))   ; Alçada del triangle
         (tx  (+ bx pad))
         (ty  (+ by pad)))

    ;; Cos del triangle: color d'equip o gris si no capturat
    (cond ((eq equip 'e1) (gr-color-negre))
          ((eq equip 'e2) (gr-color-blanc))
          (t              (gr-color-gris)))
    (gr-triangle-ple tx ty tw th)

    ;; Contorn del triangle per a millor llegibilitat
    (cond ((eq equip 'e1) (gr-color-blanc))
          ((eq equip 'e2) (gr-color-negre))
          (t              (color 80 80 80)))
    (gr-triangle-contorn tx ty tw th)

    ;; Petit punt central per indicar que és un lab (si la casella és prou gran)
    (cond ((>= m 10)
           (cond ((eq equip 'e1) (gr-color-blanc))
                 ((eq equip 'e2) (gr-color-negre))
                 (t              (color 60 60 60)))
           (let ((cx (+ tx (round (/ tw 2)) -1))
                 (cy (+ ty (round (/ th 2)) -1)))
             (gr-fill cx cy 2 2))))))


;; ======================================================================
;; SECCIÓ 6 – DIBUIX D'UNA CASELLA COMPLETA
;; ======================================================================

;; Dibuixa completament una casella (fons + element + decoració).
;; Paràmetres:
;;   casella - la llista que representa la casella del mapa
;;   bx, by  - cantonada superior-esquerra en píxels
;;   m       - mida de la casella en píxels
(defun gr-dibuixa-casella (casella bx by m)
  (let ((tipus (car casella)))
    (cond

      ;; CASELLA D'AIGUA
      ((eq tipus 'aigua)
       ;; Fons blau ple
       (gr-color-aigua)
       (gr-fill bx by m m)
       ;; Vora fosca per a diferenciació clara
       (color 30 65 150)
       (gr-stroke bx by m m))

      ;; CASELLA DE TERRA
      ((eq tipus 'terra)
       (let* ((color-c      (cadr casella))       ; Color de la casella pintada
              (element      (caddr casella))       ; 'base, 'bolla, 'lab o nil
              (equip        (cadddr casella))      ; 'e1, 'e2 o nil
              (colors-pintat (nth 4 casella))      ; Llista de colors de dany
              (color-propi  (nth 5 casella))       ; Color de la bolla
              (tr-pintar    (nth 6 casella))       ; Cooldown pintar
              (tr-moure     (nth 7 casella)))      ; Cooldown moure

         ;; 1. Fons de la casella: tenyit pel color de la casella pintada
         ;;    (Aspecte opcional: "Color del qual está pintada cada casella")
         (gr-color-fons-casella color-c)
         (gr-fill bx by m m)

         ;; 2. Vora fina de la quadrícula (gris clar)
         (color 165 165 165)
         (gr-stroke bx by m m)

         ;; 3. Element dins la casella
         (cond
           ((eq element 'base)
            (gr-dibuixa-base bx by m equip colors-pintat))
           ((eq element 'bolla)
            (gr-dibuixa-bolla bx by m equip color-propi colors-pintat tr-pintar tr-moure))
           ((eq element 'lab)
            (gr-dibuixa-lab bx by m equip))))))))


;; ======================================================================
;; SECCIÓ 7 – RECORREGUT DEL MAPA
;; ======================================================================

;; Dibuixa totes les caselles d'una fila del mapa.
;; Paràmetres:
;;   fila      - llista de caselles d'una fila
;;   col       - índex de columna actual (comença a 0)
;;   fila-idx  - índex de fila actual
;;   m         - mida de casella en píxels
;;   offset-y  - desplaçament vertical per al HUD
(defun gr-dibuixa-columnes (fila col fila-idx m offset-y)
  (cond ((null fila) nil)
        (t
         (gr-dibuixa-casella (car fila)
                             (* col m)
                             (+ offset-y (* fila-idx m))
                             m)
         (gr-dibuixa-columnes (cdr fila) (+ col 1) fila-idx m offset-y))))

;; Dibuixa totes les files del mapa.
;; Paràmetres:
;;   mapa     - llista de files del mapa
;;   fila-idx - índex de fila actual (comença a 0)
;;   m        - mida de casella en píxels
;;   offset-y - desplaçament vertical per al HUD
(defun gr-dibuixa-files (mapa fila-idx m offset-y)
  (cond ((null mapa) nil)
        (t
         (gr-dibuixa-columnes (car mapa) 0 fila-idx m offset-y)
         (gr-dibuixa-files (cdr mapa) (+ fila-idx 1) m offset-y))))


;; ======================================================================
;; SECCIÓ 8 – HUD (PANELL D'INFORMACIÓ SUPERIOR)
;; ======================================================================
;;
;; El HUD mostra:
;;   - Indicador lateral de l'equip actiu (franja de color)
;;   - Quadrat de color d'equip (e1=negre, e2=blanc) per a cada equip
;;   - Barra horitzontal proporcional a la quantitat de pintura (màx 500)
;;   - Indicador central del torn actiu
;; Tota la informació numèrica es segueix mostrant per consola (format t).
;; ======================================================================

;; Dibuixa la barra de pintura d'un equip.
;; Paràmetres:
;;   px, py  - posició de la barra
;;   bw, bh  - amplada i alçada de la barra
;;   pintura - quantitat actual de pintura
;;   equip   - 'e1 o 'e2 (determina el color de la barra)
(defun gr-barra-pintura (px py bw bh pintura equip)
  (let ((ple (max 0 (round (/ (* bw (min pintura 500.0)) 500.0)))))
    ;; Fons fosc
    (color 40 40 40)
    (gr-fill px py bw bh)
    ;; Part plena: color propi de l'equip (negre/blanc)
    (gr-aplica-color-equip equip)
    (cond ((> ple 0) (gr-fill px py ple bh)))
    ;; Vora fina
    (color 90 90 90)
    (gr-stroke px py bw bh)))

;; Dibuixa el HUD complet a la part superior de la finestra.
;; Paràmetres:
;;   ronda       - número de ronda actual
;;   equip-actiu - símbol 'e1 o 'e2
;;   pint-e1     - pintura actual de l'equip 1
;;   pint-e2     - pintura actual de l'equip 2
;;   hud-h       - alçada del HUD en píxels
(defun gr-dibuixa-hud (ronda equip-actiu pint-e1 pint-e2 hud-h)

  ;; Fons fosc del HUD
  (gr-color-hud)
  (gr-fill 0 0 640 hud-h)

  ;; Franja lateral d'equip actiu (3px, tot l'alçada del HUD)
  (gr-aplica-color-equip equip-actiu)
  (gr-fill 0 0 3 hud-h)

  ;; --- Equip 1 ---
  ;; Quadrat indicador d'equip 1 (negre amb vora blanca)
  (gr-color-negre)
  (gr-fill 8 4 12 12)
  (gr-color-blanc)
  (gr-stroke 8 4 12 12)
  ;; Barra de pintura de l'equip 1
  (gr-barra-pintura 24 6 140 8 pint-e1 'e1)

  ;; --- Equip 2 ---
  ;; Quadrat indicador d'equip 2 (blanc amb vora negra)
  (gr-color-blanc)
  (gr-fill 178 4 12 12)
  (gr-color-negre)
  (gr-stroke 178 4 12 12)
  ;; Barra de pintura de l'equip 2
  (gr-barra-pintura 194 6 140 8 pint-e2 'e2)

  ;; --- Indicador de torn actiu (centre del HUD) ---
  ;; Quadradet del color de l'equip actiu com a indicador de torn
  (gr-aplica-color-equip equip-actiu)
  (gr-fill 348 3 14 14)
  (gr-aplica-color-equip-contrast equip-actiu)
  (gr-stroke 348 3 14 14)
  ;; Punt de torn: petit quadrat al centre
  (gr-aplica-color-equip-contrast equip-actiu)
  (gr-fill 353 8 4 4)

  ;; --- Llegenda de colors (dreta del HUD) ---
  ;; r=vermell
  (gr-color-vermell)
  (gr-fill 415 4 8 8)
  ;; g=verd
  (gr-color-verd)
  (gr-fill 428 4 8 8)
  ;; b=blau
  (gr-color-blau)
  (gr-fill 441 4 8 8)
  ;; Lab (gris)
  (gr-color-gris)
  (gr-fill 454 4 8 8)

  ;; Línia separadora inferior del HUD
  (color 55 55 75)
  (gr-linia-h 0 (- hud-h 1) 640))


;; ======================================================================
;; SECCIÓ 9 – FUNCIÓ PRINCIPAL
;; ======================================================================

;; Funció principal de gràfics. Calcula la mida de casella òptima,
;; esborra la pantalla i dibuixa el HUD + mapa complet.
;;
;; Paràmetres:
;;   mapa        - matriu de caselles (llista de llistes)
;;   ronda       - número de ronda actual (enter)
;;   equip-actiu - símbol 'e1 o 'e2
;;   pint-e1     - quantitat de pintura de l'equip 1 (enter)
;;   pint-e2     - quantitat de pintura de l'equip 2 (enter)
;;
;; RECORDATORI: Cal actualitzar la crida en paintball.lsp:
;;   (dibuixa-mapa mapa ronda
;;                 (cond ((= (mod ronda 2) 1) 'e1) (t 'e2))
;;                 pint-e1 pint-e2)
(defun dibuixa-mapa (mapa ronda equip-actiu pint-e1 pint-e2)
  (let* ((files    (length mapa))
         (cols     (length (car mapa)))
         (hud-h    20)                            ; Alçada del HUD en píxels
         (area-h   (- 400 hud-h))                 ; Zona disponible per al mapa
         (m-files  (floor (/ area-h (max 1 files))))
         (m-cols   (floor (/ 640     (max 1 cols))))
         (m        (max 1 (min m-files m-cols)))) ; Mida de casella final

    ;; Esborra tota la pantalla
    (cls)

    ;; Dibuixa el mapa (sota del HUD)
    (gr-dibuixa-files mapa 0 m hud-h)

    ;; Dibuixa el HUD (a sobre: cobreix qualsevol solapament)
    (gr-dibuixa-hud ronda equip-actiu pint-e1 pint-e2 hud-h)))