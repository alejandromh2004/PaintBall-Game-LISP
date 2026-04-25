;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.

;; Variables globals per al dibuix (mida de la casella)
(setq m 15) 

;; ======================================================================
;; FUNCIONS DE DIBUIX DE DANYS
;; ======================================================================

(defun dibuixa-marca (color x y)
  "Dibuixa un petit quadrat de 2x2 a la posició x,y per marcar dany."
  (cond ((eq color 'r) (RED))
        ((eq color 'g) (GREEN))
        ((eq color 'b) (BLUE)))
  (moverel x y)
  (quadrat 2) ; Un quadrat petitó
  (moverel (- x) (- y))) ; Tornem a l'origen

(defun dibuixa-danys (colors)
  "Dibuixa marques a les cantonades per cada color de dany rebut."
  (cond ((null colors) nil)
        (t
         (let ((c (car colors)))
           ;; Assignant cantonades segons el color
           (cond ((eq c 'r) (dibuixa-marca 'r 1 1))                 ; Dalt-Esquerra
                 ((eq c 'g) (dibuixa-marca 'g (- m 3) 1))           ; Dalt-Dreta
                 ((eq c 'b) (dibuixa-marca 'b 1 (- m 3))))          ; Baix-Esquerra
           (dibuixa-danys (cdr colors))))))

;; ======================================================================
;; PINTAR ELEMENTS
;; ======================================================================

(defun pinta-element (casella)
  "Tradueix la informació d'una casella a la pantalla"
  (let ((tipus (car casella)))
    (cond 
      ;; Si és aigua, la pintam de CYAN
      ((eq tipus 'aigua)
       (CYAN)
       (quadrat-ple m)) 
      
      ;; Si és terra, miram el color i què conté
      ((eq tipus 'terra)
       (let ((color-terra (cadr casella))
             (element (caddr casella))
             (equip (cadddr casella))
             (colors-pintat (nth 4 casella))  ; <--- Extraiem la llista de danys
             (color-bolla (nth 5 casella)))   ; Aquest és el color propi ('r, 'g o 'b)
         
         ;; 1. Pintam el fons (la terra)
         (cond ((eq color-terra 'r) (RED))
               ((eq color-terra 'g) (GREEN))
               ((eq color-terra 'b) (BLUE)))
         (quadrat (- m 1))
         
         ;; 2. Pintam l'element si n'hi ha (Base, Lab o Bolla)
         (cond 
           ;; --- BASE ---
           ((eq element 'base)
            (cond ((eq equip 'e1) (PINK)) (t (BLACK)))
            (moverel 2 2)
            (quadrat (- m 5))
            (moverel -2 -2))
           
           ;; --- LABORATORI ---
           ((eq element 'lab)
            (cond ((eq equip 'e1) (PINK))
                  ((eq equip 'e2) (BLACK))
                  (t (color 128 128 128))) ; Groc fosc / Gris
            (moverel 4 4)
            (triangle (- m 8) (- m 8))
            (moverel -4 -4))
           
           ;; --- BOLLA ---
           ((eq element 'bolla)
            ;; A. El contorn de la bolla és del seu color de pintura
            (cond ((eq color-bolla 'r) (RED))
                  ((eq color-bolla 'g) (GREEN))
                  ((eq color-bolla 'b) (BLUE)))
            (moverel 3 3)
            (quadrat (- m 7))
            
            ;; B. El centre de la bolla ens diu de quin equip és
            (cond ((eq equip 'e1) (PINK)) (t (BLACK)))
            (moverel 1 1)
            (quadrat (- m 9))
            
            ;; Retornem el cursor a lloc
            (moverel -4 -4)))
         
         ;; 3. Finalment, pintem els indicadors de dany si n'hi ha
         ;; Ho fem fora del cond anterior perquè es dibuixi per sobre de l'element
         (cond ((and element colors-pintat)
                (dibuixa-danys colors-pintat))))))))

;; ======================================================================
;; MOTORS DE DIBUIX
;; ======================================================================

(defun pinta-columnes (fila x y)
  "Recorre una fila (llista de caselles) d'esquerra a dreta"
  (cond ((null fila) nil)
        (t 
         (move (* x m) (* y m))     ;; Ens col·locam a la coordenada (x, y) de la pantalla
         (pinta-element (car fila)) ;; Pintam la casella actual
         (pinta-columnes (cdr fila) (+ x 1) y)))) ;; Crida recursiva per a la següent columna

(defun pinta-files (mapa y)
  "Recorre el mapa (llista de files) de dalt a baix"
  (cond ((null mapa) nil)
        (t 
         (pinta-columnes (car mapa) 0 y)  ;; Pintam tota la fila actual començant a x=0
         (pinta-files (cdr mapa) (+ y 1))))) ;; Crida recursiva per a la següent fila

(defun dibuixa-mapa (mapa)
  "Funció principal de gràfics que es cridarà des del controlador"
  (cls)
  (pinta-files mapa 0))