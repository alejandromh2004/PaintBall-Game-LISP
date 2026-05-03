;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: Alejandro Martinez Hermosa, Javier Vivo Samaniego.
;; Professor: Miquel Cabot.
;; Lliurament: primera convocatòria.
;; Controlador de les funcions gràfiques del programa.

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

;; Aplica el color de pintura segons el símbol 'r, 'g o 'b
(defun gr-aplica-color-pintura (c)
  (cond ((eq c 'r) (gr-color-vermell))
        ((eq c 'g) (gr-color-verd))
        ((eq c 'b) (gr-color-blau))
        (t         (gr-color-gris))))

;; Aplica el color de vora d'equip segons 'e1 o 'e2
(defun gr-aplica-color-equip (e)
  (cond ((eq e 'e1) (gr-color-negre))
        ((eq e 'e2) (gr-color-blanc))
        (t          (gr-color-gris))))

;; Retorna un color de contrast (blanc/negre) segons l'equip
(defun gr-aplica-color-equip-contrast (e)
  (cond ((eq e 'e1) (gr-color-blanc))
        (t          (gr-color-negre))))

;; Retorna un fons tenyit suau segons el color de pintura 'r, 'g, 'b o nil
(defun gr-color-fons-casella (c)
  (cond ((eq c 'r) (color 255 210 210))   ; Vermell suau
        ((eq c 'g) (color 210 245 210))   ; Verd suau
        ((eq c 'b) (color 210 220 255))   ; Blau suau
        (t         (color 225 225 225)))) ; Gris neutre

;; ======================================================================
;; SECCIÓ 2 – PRIMITIVES DE DIBUIX
;; ======================================================================

;; Dibuixa un rectangle ple de w x h píxels a (x, y)
(defun gr-fill (x y w h)
  (cond ((or (<= w 0) (<= h 0)) nil)
        (t (move x y)
           (omple-linies w (- h 1)))))

;; Dibuixa el contorn d'un rectangle de w x h a (x, y)
(defun gr-stroke (x y w h)
  (cond ((or (<= w 0) (<= h 0)) nil)
        (t (move x y)
           (drawrel w 0)
           (drawrel 0 h)
           (drawrel (- w) 0)
           (drawrel 0 (- h)))))

;; Dibuixa una línia horitzontal d'amplada w a (x, y)
(defun gr-linia-h (x y w)
  (cond ((<= w 0) nil)
        (t (move x y)
           (drawrel w 0))))

;; Dibuixa una línia recta des de (x1,y1) fins a (x2,y2)
(defun gr-linia (x1 y1 x2 y2)
  (move x1 y1)
  (drawrel (- x2 x1) (- y2 y1)))

;; Dibuixa un triangle ple a (tx, ty) amb mides (tw x th)
(defun gr-triangle-ple (tx ty tw th)
  (cond ((or (<= tw 0) (<= th 0)) nil)
        (t (move tx ty)
           (omple-tri tw th 0))))

;; Dibuixa el contorn d'un triangle a la posició (tx, ty)
(defun gr-triangle-contorn (tx ty tw th)
  (cond ((or (<= tw 0) (<= th 0)) nil)
        (t (move tx ty)
           (triangle tw th))))

;; ======================================================================
;; SECCIÓ 3 – MARQUES DE DANY (PINTURA REBUDA)
;; ======================================================================

;; Calcula la mida de les marques de dany segons la mida de la casella
(defun gr-mida-marc (m)
  (max 2 (min 5 (round (/ m 3)))))

;; Dibuixa una marca de dany individual (un dels tres possibles colors)
(defun gr-dibuixa-marc (c bx by m md)
  (gr-aplica-color-pintura c)
  (cond ((eq c 'r) (gr-fill bx by md md))
        ((eq c 'g) (gr-fill (- (+ bx m) md) by md md))
        ((eq c 'b) (gr-fill bx (- (+ by m) md) md md))))

;; Dibuixa totes les marques de dany acumulades en una casella
(defun gr-dibuixa-danys (colors bx by m)
  (let ((md (gr-mida-marc m)))
    (cond ((null colors) nil)
          (t (gr-dibuixa-marc (car colors) bx by m md)
             (gr-dibuixa-danys (cdr colors) bx by m)))))

;; ======================================================================
;; SECCIÓ 4 – BARRES DE RECUPERACIÓ (COOLDOWN)
;; ======================================================================

;; Dibuixa una barra de progrés horitzontal per als temps de recuperació
(defun gr-barra-cooldown (tr tr-max px py bw bh c-r c-g c-b)
  (cond ((or (null tr) (<= tr 0)) nil)
        (t
         (let ((ple (max 1 (round (/ (* bw (min tr (float tr-max)))
                                     (float tr-max))))))
           (color 55 55 55) ;; Fons de la barra
           (gr-fill px py bw bh)
           (color c-r c-g c-b) ;; Part plena
           (gr-fill px py ple bh)))))

;; Dibuixa les barres de moure i pintar sobre una bolla
(defun gr-dibuixa-cooldowns (tr-pintar tr-moure bx by m)
  (cond ((< m 8) nil)
        (t
         (let ((bw (- m 2))
               (px (+ bx 1)))
           ;; Barra moure: taronja
           (gr-barra-cooldown tr-moure 4.3 px (- (+ by m) 5) bw 2
                              200 120 30)
           ;; Barra pintar: vermell
           (gr-barra-cooldown tr-pintar 9 px (- (+ by m) 2) bw 2
                              200 30 30)))))

;; ======================================================================
;; SECCIÓ 5 – REPRESENTACIÓ DELS ELEMENTS DEL JOC
;; ======================================================================

;; Dibuixa l'element Base (quadrat amb creu central)
(defun gr-dibuixa-base (bx by m equip colors-pintat)
  (let* ((pad  (max 1 (round (/ m 8))))
         (sz   (- m (* 2 pad)))
         (ix   (+ bx pad))
         (iy   (+ by pad))
         (cx   (+ ix (round (/ sz 2))))
         (cy   (+ iy (round (/ sz 2))))
         (gc   (max 1 (round (/ sz 5)))))
    (gr-aplica-color-equip equip)
    (gr-fill ix iy sz sz)
    (gr-stroke ix iy sz sz)
    (cond ((>= sz 6) (gr-stroke (+ ix 1) (+ iy 1) (- sz 2) (- sz 2))))
    (gr-fill (- cx (round (/ gc 2))) iy gc sz)
    (gr-fill ix (- cy (round (/ gc 2))) sz gc)
    (gr-dibuixa-danys colors-pintat bx by m)))

;; Dibuixa l'element Bolla (cercle/quadrat de color d'equip amb nucli de pintura)
(defun gr-dibuixa-bolla (bx by m equip color-propi colors-pintat tr-pintar tr-moure)
  (let* ((pad   (max 2 (round (/ m 4))))
         (inner (max 2 (- m (* 2 pad))))
         (ix    (+ bx pad))
         (iy    (+ by pad)))
    (gr-aplica-color-equip equip)
    (gr-fill (- ix 1) (- iy 1) (+ inner 2) (+ inner 2))
    (gr-aplica-color-pintura color-propi)
    (gr-fill ix iy inner inner)
    (gr-dibuixa-cooldowns tr-pintar tr-moure bx by m)
    (gr-dibuixa-danys colors-pintat bx by m)))

;; Dibuixa l'element Laboratori (triangle representatiu)
(defun gr-dibuixa-lab (bx by m equip)
  (let* ((pad (max 1 (round (/ m 5))))
         (tw  (max 3 (- m (* 2 pad))))
         (th  (max 3 (- m (* 2 pad))))
         (tx  (+ bx pad))
         (ty  (+ by pad)))
    (cond ((eq equip 'e1) (gr-color-negre))
          ((eq equip 'e2) (gr-color-blanc))
          (t              (gr-color-gris)))
    (gr-triangle-ple tx ty tw th)))

;; ======================================================================
;; SECCIÓ 6 – RENDERITZAT DE CASELLA COMPLETA
;; ======================================================================

;; Dibuixa el fons i l'element d'una casella individual
(defun gr-dibuixa-casella (casella bx by m)
  (let ((tipus (car casella)))
    (cond
      ((eq tipus 'aigua)
       (gr-color-aigua) (gr-fill bx by m m)
       (color 30 65 150) (gr-stroke bx by m m))
      ((eq tipus 'terra)
       (let* ((color-c      (cadr casella))
              (element      (caddr casella))
              (equip        (cadddr casella))
              (colors-pintat (nth 4 casella))
              (color-propi  (nth 5 casella))
              (tr-pintar    (nth 6 casella))
              (tr-moure     (nth 7 casella)))
         (gr-color-fons-casella color-c) (gr-fill bx by m m)
         (color 165 165 165) (gr-stroke bx by m m)
         (cond ((eq element 'base)  (gr-dibuixa-base bx by m equip colors-pintat))
               ((eq element 'bolla) (gr-dibuixa-bolla bx by m equip color-propi colors-pintat tr-pintar tr-moure))
               ((eq element 'lab)   (gr-dibuixa-lab bx by m equip))))))))

;; ======================================================================
;; SECCIÓ 7 – RENDERITZAT OPTIMITZAT (DIFERENCIAL)
;; ======================================================================

;; Recorre les columnes dibuixant només les caselles que han canviat d'estat
(defun gr-dibuixa-columnes (fila fila-ant col y-visual m offset-x offset-y)
  (cond ((null fila) nil)
        (t (cond ((not (equal (car fila) (cond (fila-ant (car fila-ant)) (t nil))))
                  (gr-dibuixa-casella (car fila) (+ offset-x (* col m)) (+ offset-y (* y-visual m)) m)))
           (gr-dibuixa-columnes (cdr fila) (cond (fila-ant (cdr fila-ant)) (t nil)) (+ col 1) y-visual m offset-x offset-y))))

;; Recorre les files del mapa
(defun gr-dibuixa-files (mapa mapa-ant fila-idx m offset-x offset-y files)
  (cond ((null mapa) nil)
        (t (gr-dibuixa-columnes (car mapa) (cond (mapa-ant (car mapa-ant)) (t nil)) 0 (- files 1 fila-idx) m offset-x offset-y)
           (gr-dibuixa-files (cdr mapa) (cond (mapa-ant (cdr mapa-ant)) (t nil)) (+ fila-idx 1) m offset-x offset-y files))))

;; ======================================================================
;; SECCIÓ 8 – GESTIÓ DE FLETXES D'ACCIÓ
;; ======================================================================

;; Neteja l'àrea d'una fletxa vella redibuixant el mapa de sota
(defun gr-redibuixa-area (x y max-x max-y mapa m offset-x offset-y files curr-x)
  (cond ((> y max-y) nil)
        ((> curr-x max-x) (gr-redibuixa-area x (+ y 1) max-x max-y mapa m offset-x offset-y files x))
        (t (let ((casella (indexa-matriu mapa y curr-x)))
             (cond (casella (gr-dibuixa-casella casella (+ offset-x (* curr-x m)) (+ offset-y (* (- files 1 y) m)) m)))
             (gr-redibuixa-area x y max-x max-y mapa m offset-x offset-y files (+ curr-x 1))))))

;; Esborra les fletxes de la pantalla
(defun gr-esborra-fletxes (fletxes mapa m offset-x offset-y files)
  (cond ((null fletxes) nil)
        (t (let* ((orig (cadr (car fletxes))) (desti (caddr (car fletxes)))
                  (x1 (min (car orig) (car desti))) (x2 (max (car orig) (car desti)))
                  (y1 (min (cadr orig) (cadr desti))) (y2 (max (cadr orig) (cadr desti))))
             (gr-redibuixa-area x1 y1 x2 y2 mapa m offset-x offset-y files x1))
           (gr-esborra-fletxes (cdr fletxes) mapa m offset-x offset-y files))))

;; Dibuixa una fletxa individual representant un moviment o tret
(defun gr-dibuixa-fletxa (x1 y1 x2 y2 m offset-x offset-y files)
  (let* ((hm  (round (/ m 2)))
         (px1 (+ offset-x (* x1 m) hm)) (py1 (+ offset-y (* (- files 1 y1) m) hm))
         (px2 (+ offset-x (* x2 m) hm)) (py2 (+ offset-y (* (- files 1 y2) m) hm)))
    (gr-linia px1 py1 px2 py2)
    (gr-fill (- px2 2) (- py2 2) 4 4)))

;; Dibuixa totes les fletxes noves del torn
(defun gr-dibuixa-fletxes (fletxes m offset-x offset-y files)
  (cond ((null fletxes) nil)
        (t (let* ((f (car fletxes)) (tipus (car f)) (orig (cadr f)) (desti (caddr f))
                  (x1 (car orig)) (y1 (cadr orig)) (x2 (car desti)) (y2 (cadr desti)))
             (cond ((eq tipus 'mou)   (color 230 115 20))
                   ((eq tipus 'pinta) (color 220 40  40))
                   (t                 (color 128 128 128)))
             (gr-dibuixa-fletxa x1 y1 x2 y2 m offset-x offset-y files)
             (gr-dibuixa-fletxes (cdr fletxes) m offset-x offset-y files)))))

;; ======================================================================
;; SECCIÓ 9 – FUNCIÓ PRINCIPAL DE DIBUIX DEL MAPA
;; ======================================================================

;; Coordina tot el procés de dibuix del mapa, fletxes i gestió de píxels
(defun dibuixa-mapa (mapa mapa-ant fletxes-pantalla fletxes-noves ronda equip-actiu pint-e1 pint-e2)
  (let* ((files     (length mapa))
         (cols      (length (car mapa)))
         (console-h 40) ;; Espai reservat per al HUD de text
         (area-h    (- 400 console-h))
         (m-files   (floor (/ area-h (max 1 files))))
         (m-cols    (floor (/ 640    (max 1 cols))))
         (m         (max 1 (min m-files m-cols)))
         (extra-w   (- 640 (* cols m)))
         (offset-x  (floor (/ extra-w 2)))
         (offset-y  0)) ;; El mapa es dibuixa des del top

    ;; 1. Neteja inicial si no hi ha estat anterior
    (cond ((null mapa-ant) 
           (cls)
           (gr-dibuixa-files mapa nil 0 m offset-x offset-y files))
          (t
           ;; 2. Esborrat de fletxes antigues
           (cond (fletxes-pantalla (gr-esborra-fletxes fletxes-pantalla mapa m offset-x offset-y files)))
           ;; 3. Actualització incremental de caselles
           (gr-dibuixa-files mapa mapa-ant 0 m offset-x offset-y files)))

    ;; 4. Renderitzat d'accions del torn
    (gr-dibuixa-fletxes fletxes-noves m offset-x offset-y files)
    (BLACK)))