;; ======================================================================
;; PRÀCTICA FINAL LLENGUATGES DE PROGRAMACIÓ - LISP - PAINTBALL
;; ======================================================================
;; Estudiants: Alejandro Martinez Hermosa, Javier Vivo Samaniego
;; Data: 30/04/2026
;; Assignatura: Llenguatges de Programació (LP)
;; Grup: <Indicar Grup>
;; Professors: <Indicar Professors>
;; Convocatòria: Primera Convocatòria (Ordinària)



;; ======================================================================
;; SECCIÓ 1 – PALETA DE COLORS
;; ======================================================================

(defun gr-color-vermell () (color 215 35 35))    ; Vermell(r)
(defun gr-color-verd ()    (color 35 155 45))    ; Verd (g)
(defun gr-color-blau ()    (color 45 75 220))    ; Blau (b)
(defun gr-color-negre ()   (color 0   0   0))   ; Negre (equip 1)
(defun gr-color-blanc ()   (color 255 255 255))  ; Blanc (equip 2)
(defun gr-color-gris ()    (color 128 128 128))  ; Gris neutre (lab lliure)
(defun gr-color-aigua ()   (color 55 120 210))   ; Blau de l'aigua
(defun gr-color-hud ()     (color 18 18 28))     ; Fons del HUD

;; Aplica el color de pintura
;; Paràmetres:
;;   c - símbol 'r, 'g o 'b
(defun gr-aplica-color-pintura (c)
  (cond ((eq c 'r) (gr-color-vermell))
        ((eq c 'g) (gr-color-verd))
        ((eq c 'b) (gr-color-blau))
        (t         (gr-color-gris))))

;; Aplica el color de vora d'equip: e1=negre, e2=blanc
;; Paràmetres:
;;   e - símbol 'e1 o 'e2
(defun gr-aplica-color-equip (e)
  (cond ((eq e 'e1) (gr-color-negre))
        ((eq e 'e2) (gr-color-blanc))
        (t          (gr-color-gris))))

;; Color contrastat per a símbols sobre el cos d'una unitat d'equip e
;; Paràmetres:
;;   e - símbol 'e1 o 'e2
(defun gr-aplica-color-equip-contrast (e)
  (cond ((eq e 'e1) (gr-color-blanc))
        (t          (gr-color-negre))))

;; Fons tenyit d'una casella de terra pintada del color c
;; Implementa la funcionalitat opcional de mostrar el color de la casella.
;; Paràmetres:
;;   c - símbol 'r, 'g, 'b o nil
(defun gr-color-fons-casella (c)
  (cond ((eq c 'r) (color 255 210 210))   ; Vermell suau
        ((eq c 'g) (color 210 245 210))   ; Verd suau
        ((eq c 'b) (color 210 220 255))   ; Blau suau
        (t         (color 225 225 225)))) ; Gris neutre


;; ======================================================================
;; SECCIÓ 2 – PRIMITIVES DE DIBUIX
;; ======================================================================

;; Dibuixa un rectangle ple de w×h píxels a la posició absoluta (x, y)
;; Paràmetres:
;;   x, y - cantonada superior-esquerra
;;   w, h - amplada i alçada en píxels
(defun gr-fill (x y w h)
  (cond ((or (<= w 0) (<= h 0)) nil)
        (t (move x y)
           (omple-linies w (- h 1)))))

;; Dibuixa el contorn d'un rectangle de w×h a la posició (x, y)
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

;; Dibuixa una línia horitzontal d'amplada w a (x, y)
;; Paràmetres:
;;   x, y - punt d'inici
;;   w    - amplada en píxels
(defun gr-linia-h (x y w)
  (cond ((<= w 0) nil)
        (t (move x y)
           (drawrel w 0))))

;; Dibuixa una línia recta de (x1,y1) a (x2,y2)
;; Paràmetres:
;;   x1, y1 - punt d'origen (absolut)
;;   x2, y2 - punt de destinació (absolut)
(defun gr-linia (x1 y1 x2 y2)
  (move x1 y1)
  (drawrel (- x2 x1) (- y2 y1)))

;; Dibuixa un triangle ple a la posició (tx, ty) amb mides (tw × th)
;; Paràmetres:
;;   tx, ty   - cantonada superior-esquerra
;;   tw, th   - amplada i alçada del triangle
(defun gr-triangle-ple (tx ty tw th)
  (cond ((or (<= tw 0) (<= th 0)) nil)
        (t (move tx ty)
           (omple-tri tw th 0))))

;; Dibuixa el contorn d'un triangle a la posició (tx, ty).
;; Paràmetres: igual que gr-triangle-ple
(defun gr-triangle-contorn (tx ty tw th)
  (cond ((or (<= tw 0) (<= th 0)) nil)
        (t (move tx ty)
           (triangle tw th))))


;; ======================================================================
;; SECCIÓ 3 – MARQUES DE DANY
;; ======================================================================

;; Calcula la mida de la marca de dany en funció de la mida de casella m
;; Paràmetres:
;;   m - mida de la casella en píxels
(defun gr-mida-marc (m)
  (max 2 (min 5 (round (/ m 3)))))

;; Dibuixa una única marca de dany de color c a la cantonada corresponent.
;; Paràmetres:
;;   c      - símbol 'r, 'g o 'b
;;   bx, by - cantonada superior-esquerra de la casella
;;   m      - mida de la casella
;;   md     - mida de la marca de dany
(defun gr-dibuixa-marc (c bx by m md)
  (gr-aplica-color-pintura c)
  (cond ((eq c 'r) (gr-fill bx by md md))
        ((eq c 'g) (gr-fill (- (+ bx m) md) by md md))
        ((eq c 'b) (gr-fill bx (- (+ by m) md) md md))))

;; Dibuixa totes les marques de dany d'una llista de colors pintats.
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
;; SECCIÓ 4 – BARRES DE COOLDOWN
;; ======================================================================

;; Dibuixa una barra de cooldown horitzontal
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

;; Dibuixa les dues barres de cooldown d'una bolla
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

;; Dibuixa una base
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

    (gr-aplica-color-equip equip)
    (gr-fill ix iy sz sz)
    (gr-stroke ix iy sz sz)
    (cond ((>= sz 6)
           (gr-stroke (+ ix 1) (+ iy 1) (- sz 2) (- sz 2))))

    (gr-fill (- cx (round (/ gc 2))) iy gc sz)   ; Vertical
    (gr-fill ix (- cy (round (/ gc 2))) sz gc)   ; Horitzontal
    (gr-dibuixa-danys colors-pintat bx by m)))



;; Dibuixa una bolla
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

    (gr-aplica-color-equip equip)
    (gr-fill (- ix 1) (- iy 1) (+ inner 2) (+ inner 2))

    ;; Cos de la bolla: color propi (vermell, verd o blau real)
    (gr-aplica-color-pintura color-propi)
    (gr-fill ix iy inner inner)

    ;; Barres de cooldown a la part inferior
    (gr-dibuixa-cooldowns tr-pintar tr-moure bx by m)
    (gr-dibuixa-danys colors-pintat bx by m)))


;; Dibuixa un laboratori
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
;; SECCIÓ 7 – RECORREGUT DEL MAPA (PINTAT PARCIAL OPTIMITZAT)
;; ======================================================================

(defun gr-dibuixa-columnes (fila fila-ant col y-visual m offset-x offset-y)
  (cond ((null fila) nil)
        (t
         ;; Només dibuixa si la casella actual és diferent de la del torn passat
         (cond ((not (equal (car fila) (cond (fila-ant (car fila-ant)) (t nil))))
                (gr-dibuixa-casella (car fila) (+ offset-x (* col m)) (+ offset-y (* y-visual m)) m)))
         (gr-dibuixa-columnes (cdr fila) (cond (fila-ant (cdr fila-ant)) (t nil)) (+ col 1) y-visual m offset-x offset-y))))

(defun gr-dibuixa-files (mapa mapa-ant fila-idx m offset-x offset-y files)
  (cond ((null mapa) nil)
        (t
         (gr-dibuixa-columnes (car mapa) (cond (mapa-ant (car mapa-ant)) (t nil)) 0 (- files 1 fila-idx) m offset-x offset-y)
         (gr-dibuixa-files (cdr mapa) (cond (mapa-ant (cdr mapa-ant)) (t nil)) (+ fila-idx 1) m offset-x offset-y files))))


;; ======================================================================
;; SECCIÓ 8 – FLETXES I NETEJA
;; ======================================================================

(defun gr-redibuixa-area (x y max-x max-y mapa m offset-x offset-y files curr-x)
  "Neteja l'àrea d'una fletxa vella redibuixant només aquest tros de mapa."
  (cond ((> y max-y) nil)
        ((> curr-x max-x) (gr-redibuixa-area x (+ y 1) max-x max-y mapa m offset-x offset-y files x))
        (t (let ((casella (indexa-matriu mapa y curr-x)))
             (cond (casella (gr-dibuixa-casella casella (+ offset-x (* curr-x m)) (+ offset-y (* (- files 1 y) m)) m)))
             (gr-redibuixa-area x y max-x max-y mapa m offset-x offset-y files (+ curr-x 1))))))

(defun gr-esborra-fletxes (fletxes mapa m offset-x offset-y files)
  (cond ((null fletxes) nil)
        (t (let* ((orig (cadr (car fletxes)))
                  (desti (caddr (car fletxes)))
                  (x1 (min (car orig) (car desti)))
                  (x2 (max (car orig) (car desti)))
                  (y1 (min (cadr orig) (cadr desti)))
                  (y2 (max (cadr orig) (cadr desti))))
             (gr-redibuixa-area x1 y1 x2 y2 mapa m offset-x offset-y files x1))
           (gr-esborra-fletxes (cdr fletxes) mapa m offset-x offset-y files))))

(defun gr-dibuixa-fletxa (x1 y1 x2 y2 m offset-x offset-y files)
  (let* ((hm  (round (/ m 2)))
         (px1 (+ offset-x (* x1 m) hm))
         (py1 (+ offset-y (* (- files 1 y1) m) hm))
         (px2 (+ offset-x (* x2 m) hm))
         (py2 (+ offset-y (* (- files 1 y2) m) hm)))
    (gr-linia px1 py1 px2 py2)
    (gr-fill (- px2 2) (- py2 2) 4 4)))

(defun gr-dibuixa-fletxes (fletxes m offset-x offset-y files)
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
            (cond ((eq tipus 'mou)   (color 230 115 20))
                  ((eq tipus 'pinta) (color 220 40  40))
                  (t                 (color 128 128 128)))
            (gr-dibuixa-fletxa x1 y1 x2 y2 m offset-x offset-y files)
            (gr-dibuixa-fletxes (cdr fletxes) m offset-x offset-y files)))))


;; ======================================================================
;; SECCIÓ 9 – FUNCIÓ PRINCIPAL DE DIBUIX
;; ======================================================================

(defun dibuixa-mapa (mapa mapa-ant fletxes-pantalla fletxes-noves ronda equip-actiu pint-e1 pint-e2)
  (let* ((files     (length mapa))
         (cols      (length (car mapa)))
         ;; 1. Reservem 40 píxels d'alçada per al HUD de text
         (console-h 40) 
         (area-h    (- 400 console-h))
         (m-files   (floor (/ area-h (max 1 files))))
         (m-cols    (floor (/ 640    (max 1 cols))))
         (m         (max 1 (min m-files m-cols)))
         (extra-w   (- 640 (* cols m)))
         (offset-x  (floor (/ extra-w 2)))
         ;; 2. Empenyem el mapa 40 píxels cap avall
         (offset-y  0))

    ;; 1. Neteja TOTAL NOMÉS si és el primer torn (no hi ha mapa antic)
    (cond ((null mapa-ant) 
           (cls)
           (gr-dibuixa-files mapa nil 0 m offset-x offset-y files))
          (t
           ;; 2. Esborra la brutícia de les fletxes del torn anterior
           (cond (fletxes-pantalla
                  (gr-esborra-fletxes fletxes-pantalla mapa m offset-x offset-y files)))
           ;; 3. Dibuixa NOMÉS el que ha canviat
           (gr-dibuixa-files mapa mapa-ant 0 m offset-x offset-y files)))

    ;; 4. Dibuixa les fletxes noves
    (gr-dibuixa-fletxes fletxes-noves m offset-x offset-y files)
    (BLACK)))