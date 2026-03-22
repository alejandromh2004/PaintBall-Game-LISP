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
(load "proyectos/projecte_inicial/funciones_auxiliares.lsp")
(load "proyectos/projecte_inicial/grafics.lsp")
(load "proyectos/projecte_inicialagent-abc123")
(load "proyectos/projecte_inicialagent-xyz999")


(defun inici ()
  "Punt d'entrada del programa."
  (color 0 0 0 255 255 255) 
  (mode 0 0 640 375) 
  
  ;; Llegim el mapa del fitxer i el guardam localment
  (let ((mapa-inicial (carrega-mapa "proyectos/projecte_inicial/maps/two.map")))
    
    ;; Li passam el mapa al mòdul gràfic perquè el pinti
    (inicia-partida mapa-inicial)
    
    ;; Més endavant, aquí cridarem a la funció recursiva que controla els torns,
    ;; passant-li aquest 'mapa-inicial' com a estat base.
  )

  (color 0 0 0 255 255 255) 
  t)

(defun carrega-mapa (nom-fitxer)
  "Llegeix el mapa des d'un fitxer de text i retorna la llista."
  (let* ((canal (open nom-fitxer :direction :input))
         (mapa (read canal)))  ; Llegeix la llista directament
    (close canal)              ; És molt important tancar el fitxer!
    mapa)
)


;; ======================================================================
;; CONTROLADOR GENERAL
;; ======================================================================

(defun inicia-partida (mapa-inicial)
  "Prepara l'estat inicial i llança el bucle principal de la partida."
  ;; L'estat inicial és:
  ;; - Ronda: 1
  ;; - Mapa: el que hem carregat del fitxer
  ;; - Pintura E1: 200
  ;; - Pintura E2: 200
  ;; - Memòria E1: nil (buida)
  ;; - Memòria E2: nil (buida)
  (bucle-partida 1 mapa-inicial 200 200 nil nil))


(defun bucle-partida (ronda mapa pint-e1 pint-e2 mem-e1 mem-e2)
  "El motor principal del joc. S'executa recursivament a cada torn."
  
  ;; 1. Dibuixem l'estat actual al principi del torn
  (dibuixa-mapa mapa)
  ;; Mostrem informació per consola temporalment per fer debug
  (format t "~%--- RONDA ~A ---~%" ronda)
  (format t "Pintura E1: ~A | Pintura E2: ~A~%" pint-e1 pint-e2)
  
  ;; 2. Comprovem condicions de finalització (Base destruïda o límit de torns)
  (cond ((> ronda 1500) 
         (format t "Final de la partida: Límit de 1500 torns assolit!~%")
         ;; Aquí més endavant cridarem a la funció de desempat
         'fi-de-partida)
        
        ;; Aquí afegirem més endavant la comprovació de si una base ha explotat
        
        ;; 3. Si la partida continua, processem el torn
        (t 
         (let* (;; Determinem de quin equip és el torn actual
                (equip-actiu (cond ((= (mod ronda 2) 1) 'e1)
                                   (t 'e2)))
                
                ;; ==========================================================
                ;; AQUESTA ÉS LA ZONA ON PASSARÀ LA MÀGIA MÉS ENDAVANT:
                ;; Aquí cridarem funcions que agafaran el mapa antic i
                ;; retornaran versions noves del mapa i la pintura basant-se 
                ;; en la recol·lecció i les accions dels agents.
                ;;
                ;; Per ara, simulem que l'estat no canvia perquè el bucle giri.
                ;; ==========================================================
                (nou-mapa mapa) 
                (nova-pint-e1 pint-e1)
                (nova-pint-e2 pint-e2)
                (nova-mem-e1 mem-e1)
                (nova-mem-e2 mem-e2))
           
           ;; Fem una petita pausa per poder veure el joc torn a torn (Prem Enter)
           (format t "Torn de l'equip ~A. Prem ENTER per continuar..." equip-actiu)
           (read-line)
           
           ;; 4. Crida recursiva per al següent torn amb l'estat actualitzat
           (bucle-partida (+ ronda 1) 
                          nou-mapa 
                          nova-pint-e1 
                          nova-pint-e2 
                          nova-mem-e1 
                          nova-mem-e2)))))
