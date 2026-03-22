;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del controlador principal.
;; <Descripció de les funcions d'aquest fitxer>

;; Necessari per a l'optimització de crides recursives.
; (load 'common) ; https://almy.us/files/xl305req.zip
; (load 'tco)    ; https://github.com/antoni-oliver/defun-tco

;; Altres fitxers de la pràctica:
(load "proyectos/projecte_inicial/grafics.lsp")
(load "proyectos/pintar.lsp")
; (load 'agent-abc123)
; (load 'agent-xyz999)


(defun inici ()
  "Punt d'entrada del programa."
  (color 0 0 0 255 255 255) 
  (mode 0 0 640 375) 
  
  ;; Llegim el mapa del fitxer i el guardam localment
  (let ((mapa-inicial (carrega-mapa "proyectos/projecte_inicial/maps/xdd.map")))
    
    ;; Li passam el mapa al mòdul gràfic perquè el pinti
    (dibuixa-mapa mapa-inicial)
    
    ;; Més endavant, aquí cridarem a la funció recursiva que controla els torns,
    ;; passant-li aquest 'mapa-inicial' com a estat base.
  )
  t)

(defun carrega-mapa (nom-fitxer)
  "Llegeix el mapa des d'un fitxer de text i retorna la llista."
  (let* ((canal (open nom-fitxer :direction :input))
         (mapa (read canal)))  ; Llegeix la llista directament
    (close canal)              ; És molt important tancar el fitxer!
    mapa))