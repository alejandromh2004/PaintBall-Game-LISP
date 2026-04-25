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
;; DESCRIPCIÓ: Mòdul de representació gràfica de la partida. S'encarrega
;; de dibuixar el mapa, les unitats i la informació d'estat a la finestra.
;; ----------------------------------------------------------------------

;; Mida de la casella (es calcularà dinàmicament)
;; Eliminem la variable global per seguir un enfocament purament funcional.

;; ======================================================================
;; FUNCIONS DE DIBUIX DE DANYS
;; ======================================================================

(defun dibuixa-marca (color x y m)
  "Dibuixa un petit quadrat de 2x2 a la posició x,y per marcar dany."
  (cond ((eq color 'r) (RED))
        ((eq color 'g) (GREEN))
        ((eq color 'b) (BLUE)))
  (moverel x y)
  (quadrat 2) ; Un quadrat petitó
  (moverel (- x) (- y))) ; Tornem a l'origen

(defun dibuixa-danys (colors m)
  "Dibuixa marques a les cantonades per cada color de dany rebut."
  (cond ((null colors) nil)
        (t
          (let ((c (car colors)))
            ;; Assignant cantonades segons el color
            (cond ((eq c 'r) (dibuixa-marca 'r 1 1 m))                 ; Dalt-Esquerra
                  ((eq c 'g) (dibuixa-marca 'g (- m 3) 1 m))           ; Dalt-Dreta
                  ((eq c 'b) (dibuixa-marca 'b 1 (- m 3) m)))          ; Baix-Esquerra
            (dibuixa-danys (cdr colors) m)))))

;; ======================================================================
;; PINTAR ELEMENTS
;; ======================================================================

(defun pinta-element (casella m)
  "Representació neta: fons blanc, vora negra, i fons de color només quan es pinta."
  (let ((tipus (car casella)))
    (cond 
      ;; --- AIGUA (Sempre blava) ---
      ((eq tipus 'aigua)
       (color 100 200 255) (quadrat-ple m)
       (BLACK) (quadrat m))
      
      ;; --- TERRA ---
      ((eq tipus 'terra)
       (let ((color-terra (cadr casella))
             (element (caddr casella))
             (equip (cadddr casella))
             (colors-pintat (nth 4 casella))
             (color-bolla (nth 5 casella)))
         
         ;; Fons: Blanc per defecte, o color de l'última pintura si n'hi ha
         (cond ((null colors-pintat) (color 255 255 255)) ; Blanc nuclear
               ((member 'r colors-pintat) (color 255 200 200))
               ((member 'g colors-pintat) (color 200 255 200))
               ((member 'b colors-pintat) (color 200 200 255))
               (t (color 255 255 255)))
         (quadrat-ple m)
         (BLACK) (quadrat m) ; Vora negra
         
         ;; Elements (Bases, Labs, Bolles)
         (cond 
           ((eq element 'base)
            (cond ((eq equip 'e1) (color 255 0 255)) (t (BLACK)))
            (moverel 2 2) (quadrat-ple (max 1 (- m 4))) (moverel -2 -2))
           
           ((eq element 'lab)
            (cond ((eq equip 'e1) (color 255 0 255))
                  ((eq equip 'e2) (BLACK))
                  (t (color 220 220 220)))
            (moverel (round (/ m 4)) (round (/ m 4)))
            (triangle-ple (max 1 (- m (round (/ m 2)))) (max 1 (- m (round (/ m 2)))))
            (moverel (- (round (/ m 4))) (- (round (/ m 4)))))
           
           ((eq element 'bolla)
            (cond ((eq color-bolla 'r) (RED))
                  ((eq color-bolla 'g) (GREEN))
                  ((eq color-bolla 'b) (BLUE)))
            (moverel (round (/ m 3)) (round (/ m 3)))
            (quadrat-ple (max 1 (- m (round (* 2 (/ m 3))))))
            (cond ((eq equip 'e1) (color 255 255 255)) (t (BLACK)))
            (moverel 1 1) (quadrat-ple (max 1 (- m (round (* 2.5 (/ m 3))))))
            (moverel (- (+ (round (/ m 3)) 1)) (- (+ (round (/ m 3)) 1)))))
         
         (cond ((and element colors-pintat) (dibuixa-danys colors-pintat m))))))))

;; ======================================================================
;; MOTORS DE DIBUIX
;; ======================================================================

(defun pinta-columnes (fila x y m)
  "Recorre una fila (llista de caselles) d'esquerra a dreta"
  (cond ((null fila) nil)
        (t 
         (move (* x m) (* y m))     
         (pinta-element (car fila) m) 
         (pinta-columnes (cdr fila) (+ x 1) y m)))) 

(defun pinta-files (mapa y m)
  "Recorre el mapa (llista de files) de dalt a baix"
  (cond ((null mapa) nil)
        (t 
         (pinta-columnes (car mapa) 0 y m)  
         (pinta-files (cdr mapa) (+ y 1) m)))) 

(defun dibuixa-mapa (mapa)
  "Funció principal de gràfics. Calcula 'm' per fer cabre el mapa i evitar la consola."
  (let* ((files (length mapa))
         (cols (length (car mapa)))
         ;; Deixem més espai (margin) per la consola i les puntuacions: 280px d'alçada útil
         (m-files (floor (/ 280 files)))
         (m-cols (floor (/ 630 cols)))
         (m (max 1 (min m-files m-cols))))
    (cls)
    (pinta-files mapa 1 m)))