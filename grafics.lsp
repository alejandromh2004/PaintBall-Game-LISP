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
;;   - HUD visual INFERIOR: indicadors d'equip actiu, barres de pintura i
;;     barra de progrés de ronda (ronda/1500).
;;   - Laboratoris com a triangle gris/negre/blanc sense vora.
;;   - Fletxes de moviment (taronja) i d'atac (vermell) sobre el mapa.
;;   - Optimització: cls + redibuix complet cada torn.
;;
;; DISSENY FUNCIONAL:
;;   Totes les funcions són pures. El dibuix s'obté passant l'estat com a
;;   paràmetre, sense mutació ni reassignació.
;;
;; ESTRUCTURA DE CASELLA (terra):
;;   (terra color-casella element equip colors-pintat color-propi
;;    tr-pintar tr-moure id-unitat)
;;
;; CANVI NECESSARI EN paintball.lsp  (veure baix):
;;   - Signatura de bucle-partida ampliada amb fletxes-prev.
;;   - Crida a dibuixa-mapa ampliada amb fletxes-prev.
;;   - aplica-accions retorna 4 elements (mapa pintura mem fletxes).
;;   - Les cridades format t de ronda i pintura es consoliden en 1 línia.
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

;; Dibuixa una línia recta de (x1,y1) a (x2,y2).
;; Paràmetres:
;;   x1, y1 - punt d'origen (absolut)
;;   x2, y2 - punt de destinació (absolut)
(defun gr-linia (x1 y1 x2 y2)
  (move x1 y1)
  (drawrel (- x2 x1) (- y2 y1)))

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
;; Dibuixa un laboratori com un triangle ple i sense vora.
;; No capturat: gris.  Equip 1: negre.  Equip 2: blanc.
;; (No té contorn per disseny: es vol un triangle net i minimalista.)
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

    ;; Cos del triangle: color segons equip, sense cap vora
    (cond ((eq equip 'e1) (gr-color-negre))
          ((eq equip 'e2) (gr-color-blanc))
          (t              (gr-color-gris)))
    (gr-triangle-ple tx ty tw th)))


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
;;   offset-y  - desplaçament vertical (per reservar espai per la consola)
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
;;   offset-y - desplaçament vertical
(defun gr-dibuixa-files (mapa fila-idx m offset-y)
  (cond ((null mapa) nil)
        (t
         (gr-dibuixa-columnes (car mapa) 0 fila-idx m offset-y)
         (gr-dibuixa-files (cdr mapa) (+ fila-idx 1) m offset-y))))


;; ======================================================================
;; SECCIÓ 8 – FLETXES D'ACCIONS (ASPECTE OPCIONAL)
;; ======================================================================
;;
;; Les fletxes es dibuixen DAMUNT del mapa, un cop totes les caselles
;; han estat pintades.
;;
;; Cada element de la llista fletxes té forma:
;;   (tipus (col-orig row-orig) (col-dest row-dest))
;; on tipus és 'mou (taronja) o 'pinta (vermell).
;;
;; Les coordenades són índexs reals de la matriu (sense desplaçament).
;; ======================================================================

;; Dibuixa una fletxa des del centre de la casella (x1,y1) fins a (x2,y2).
;; Paràmetres:
;;   x1, y1   - casella d'origen (col, row)
;;   x2, y2   - casella de destinació (col, row)
;;   m        - mida de casella en píxels
;;   offset-y - desplaçament vertical del mapa
(defun gr-dibuixa-fletxa (x1 y1 x2 y2 m offset-y)
  (let* ((hm  (round (/ m 2)))
         ;; Centres de les dues caselles en coordenades de pantalla
         (px1 (+ (* x1 m) hm))
         (py1 (+ offset-y (* y1 m) hm))
         (px2 (+ (* x2 m) hm))
         (py2 (+ offset-y (* y2 m) hm))
         ;; Vector de la fletxa
         (ddx (- px2 px1))
         (ddy (- py2 py1)))
    ;; Eix de la fletxa
    (gr-linia px1 py1 px2 py2)
    ;; Punta de la fletxa: petit quadrat ple al destí
    (gr-fill (- px2 2) (- py2 2) 4 4)))

;; Dibuixa totes les fletxes de la llista.
;; Paràmetres:
;;   fletxes  - llista de (tipus coord-orig coord-dest)
;;   m        - mida de casella en píxels
;;   offset-y - desplaçament vertical del mapa
(defun gr-dibuixa-fletxes (fletxes m offset-y)
  (cond ((null fletxes) nil)
        (t
         (let* ((f      (car fletxes))
                (tipus  (car f))
                (orig   (cadr f))
                (desti  (caddr f))
                (x1     (car orig))
                (y1     (cadr orig))
                (x2     (car desti))
                (y2     (cadr desti)))
           ;; Color de la fletxa: taronja per moviment, vermell per atac
           (cond ((eq tipus 'mou)   (color 230 115 20))
                 ((eq tipus 'pinta) (color 220 40  40))
                 (t                 (gr-color-gris)))
           (gr-dibuixa-fletxa x1 y1 x2 y2 m offset-y)
           (gr-dibuixa-fletxes (cdr fletxes) m offset-y)))))


;; ======================================================================
;; SECCIÓ 9 – HUD (PANELL D'INFORMACIÓ INFERIOR)
;; ======================================================================
;;
;; El HUD es dibuixa a la PART INFERIOR de la finestra (hud-y = 400-hud-h).
;; D'aquesta manera, el text de la consola (a la part superior) no tapa
;; el mapa ni el panell d'informació.
;;
;; Contingut del HUD (d'esquerra a dreta):
;;   - Franja lateral de l'equip actiu (3 px, alçada completa del HUD)
;;   - E1: indicador quadrat (negre) + barra de pintura (amb marques cada 100)
;;   - E2: indicador quadrat (blanc) + barra de pintura (amb marques cada 100)
;;   - Llegenda de colors (R, G, B, Lab)
;;   - Barra de progrés de ronda (ronda/1500, de verd a vermell)
;;   - Punt indicador de l'equip actiu (extrem dret)
;;
;; La informació numèrica (ronda i pintura) s'escriu en una sola línia
;; de consola a paintball.lsp per minimitzar l'espai que ocupa el text.
;; ======================================================================

;; Dibuixa marques verticals a la barra de pintura cada 100 unitats.
;; Paràmetres:
;;   px, py - cantonada de la barra
;;   bw, bh - amplada i alçada de la barra
(defun gr-marques-pintura (px py bw bh)
  ;; Marques cada 100 unitats dins de rang 0..500
  (let ((pas (round (/ bw 5.0))))
    (color 80 80 80)
    (gr-linia-h (+ px pas)           (- py 1) 1)
    (gr-linia-h (+ px (* 2 pas))     (- py 1) 1)
    (gr-linia-h (+ px (* 3 pas))     (- py 1) 1)
    (gr-linia-h (+ px (* 4 pas))     (- py 1) 1)))

;; Dibuixa la barra de pintura d'un equip al HUD.
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
    ;; Marques cada 100 unitats
    (gr-marques-pintura px py bw bh)
    ;; Vora fina
    (color 90 90 90)
    (gr-stroke px py bw bh)))

;; Dibuixa la barra de progrés de ronda al HUD.
;; Passa de verd (ronda 0) a groc (ronda 500) a vermell (ronda 1000+).
;; Paràmetres:
;;   px, py  - posició de la barra
;;   bw, bh  - amplada i alçada de la barra
;;   ronda   - ronda actual (enter)
(defun gr-barra-ronda (px py bw bh ronda)
  (let ((ple (max 0 (round (/ (* bw (min ronda 1500.0)) 1500.0)))))
    ;; Fons fosc
    (color 40 40 40)
    (gr-fill px py bw bh)
    ;; Color progressiu: verd → groc → vermell
    (cond ((< ronda 500)  (color 50  180 80))
          ((< ronda 1000) (color 200 180 50))
          (t              (color 200  60 60)))
    (cond ((> ple 0) (gr-fill px py ple bh)))
    ;; Vora fina
    (color 90 90 90)
    (gr-stroke px py bw bh)))

;; Dibuixa el HUD complet a la part inferior de la finestra.
;; Paràmetres:
;;   ronda       - número de ronda actual
;;   equip-actiu - símbol 'e1 o 'e2
;;   pint-e1     - pintura actual de l'equip 1
;;   pint-e2     - pintura actual de l'equip 2
;;   hud-y       - coordenada y d'inici del HUD (baix de la pantalla)
;;   hud-h       - alçada del HUD en píxels
(defun gr-dibuixa-hud (ronda equip-actiu pint-e1 pint-e2 hud-y hud-h)
  (let* (;; Posicions verticals centrades dins el HUD
         (y4   (+ hud-y 4))          ; Y per a quadrats d'equip
         (y6   (+ hud-y 7))          ; Y per a barres de pintura
         (bh   8))                   ; Alçada de les barres

    ;; === Fons fosc del HUD ===
    (gr-color-hud)
    (gr-fill 0 hud-y 640 hud-h)

    ;; === Franja lateral d'equip actiu (3px, tot l'alçada del HUD) ===
    (gr-aplica-color-equip equip-actiu)
    (gr-fill 0 hud-y 3 hud-h)

    ;; === Equip 1 ===
    ;; Quadrat indicador (negre amb vora blanca)
    (gr-color-negre)
    (gr-fill 8 y4 12 12)
    (gr-color-blanc)
    (gr-stroke 8 y4 12 12)
    ;; Barra de pintura E1 amb marques cada 100
    (gr-barra-pintura 24 y6 150 bh pint-e1 'e1)

    ;; === Equip 2 ===
    ;; Quadrat indicador (blanc amb vora negra)
    (gr-color-blanc)
    (gr-fill 184 y4 12 12)
    (gr-color-negre)
    (gr-stroke 184 y4 12 12)
    ;; Barra de pintura E2 amb marques cada 100
    (gr-barra-pintura 200 y6 150 bh pint-e2 'e2)

    ;; === Separador vertical central ===
    (color 55 55 75)
    (gr-fill 360 (+ hud-y 2) 1 (- hud-h 4))

    ;; === Llegenda de colors ===
    ;; R (vermell)
    (gr-color-vermell)
    (gr-fill 368 y4 8 8)
    ;; G (verd)
    (gr-color-verd)
    (gr-fill 381 y4 8 8)
    ;; B (blau)
    (gr-color-blau)
    (gr-fill 394 y4 8 8)
    ;; Lab (gris = no capturat)
    (gr-color-gris)
    (gr-fill 407 y4 8 8)

    ;; === Separador vertical dret ===
    (color 55 55 75)
    (gr-fill 422 (+ hud-y 2) 1 (- hud-h 4))

    ;; === Barra de progrés de ronda ===
    (gr-barra-ronda 428 y6 190 bh ronda)

    ;; === Indicador de torn actiu (extrem dret) ===
    ;; Petit quadrat del color de l'equip actiu
    (gr-aplica-color-equip equip-actiu)
    (gr-fill 622 y4 12 12)
    (gr-aplica-color-equip-contrast equip-actiu)
    (gr-stroke 622 y4 12 12)

    ;; === Línia separadora superior del HUD ===
    (color 55 55 75)
    (gr-linia-h 0 hud-y 640)))


;; ======================================================================
;; SECCIÓ 10 – FUNCIÓ PRINCIPAL
;; ======================================================================

;; Funció principal de gràfics. Calcula la mida de casella òptima,
;; esborra la pantalla, dibuixa el mapa + fletxes d'accions + HUD.
;;
;; Paràmetres:
;;   mapa        - matriu de caselles (llista de llistes)
;;   ronda       - número de ronda actual (enter)
;;   equip-actiu - símbol 'e1 o 'e2
;;   pint-e1     - quantitat de pintura de l'equip 1 (enter)
;;   pint-e2     - quantitat de pintura de l'equip 2 (enter)
;;   fletxes     - llista de fletxes del torn anterior (pot ser nil)
;;                 Cada element: (tipus (col-orig row-orig) (col-dest row-dest))
;;                 tipus: 'mou (taronja) o 'pinta (vermell)
;;
;; CRIDA EN paintball.lsp (veure comentari al principi del fitxer):
;;   (dibuixa-mapa mapa ronda
;;                 (cond ((= (mod ronda 2) 1) 'e1) (t 'e2))
;;                 pint-e1 pint-e2
;;                 fletxes-prev)
(defun dibuixa-mapa (mapa ronda equip-actiu pint-e1 pint-e2 fletxes)
  (let* ((files     (length mapa))
         (cols      (length (car mapa)))
         ;; Espai reservat per al text de la consola (1 línia de prompt)
         (console-h 18)
         ;; Alçada del HUD inferior
         (hud-h     24)
         ;; Zona disponible per al mapa (entre consola i HUD)
         (area-h    (- 400 hud-h console-h))
         ;; Mida de casella: la mínima de les dues dimensions
         (m-files   (floor (/ area-h (max 1 files))))
         (m-cols    (floor (/ 640    (max 1 cols))))
         (m         (max 1 (min m-files m-cols)))
         ;; Posicions clau
         (hud-y     (- 400 hud-h))      ; El HUD comença aquí
         (offset-y  console-h))         ; El mapa comença aquí (sota la consola)

    ;; Esborra tota la pantalla
    (cls)

    ;; 1. Dibuixa el mapa (entre la zona de consola i el HUD)
    (gr-dibuixa-files mapa 0 m offset-y)

    ;; 2. Dibuixa les fletxes d'accions damunt del mapa
    (gr-dibuixa-fletxes fletxes m offset-y)

    ;; 3. Dibuixa el HUD (a la part inferior, sempre visible)
    (gr-dibuixa-hud ronda equip-actiu pint-e1 pint-e2 hud-y hud-h)))