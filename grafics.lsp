;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.
;; <Descripció de les funcions d'aquest fitxer>

;; Variables globals per al dibuix (mida de la casella)
(setq m 13) 

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
             (element (caddr casella)))
         ;; 1. Pintam el fons (la terra)
         (cond ((eq color-terra 'r) (RED))
               ((eq color-terra 'g) (GREEN))
               ((eq color-terra 'b) (BLUE)))
         (quadrat (- m 1))
         
         ;; 2. Pintam l'element si n'hi ha (Base o Lab)
         (cond ((eq element 'base)
                (BLACK) ;; Una base pot ser un quadrat negre a dins
                (moverel 2 2)
                (quadrat (- m 5))
                (moverel -2 -2))
               ((eq element 'lab)
                (color 255 255 255) ;; Un lab pot ser blanc
                (moverel 3 3)
                (quadrat (- m 7))
                (moverel -3 -3))
         )
        )
        )
    )
  )
                
)

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