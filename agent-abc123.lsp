;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer de l'agent intel·ligent ABC123.
;; <Descripció de les funcions d'aquest fitxer>

;; Documentació d'això...
(defun agent-abc123 (dades)
    "Retorna la jugada que realitza l'agent ABC123 en un torn donades les dades de la partida."
    ; (car dades) = ronda
    ; (cadr dades) = equip
    ; etc.
    ; Exemple: retornam les següents accions:
    ; 1. Crear una bolla a la posició (2, 3).
    ; 2. Moure a la posició (4, 5).
    ; 3. Pintar a la posició (6, 7).
    ; 4. Escriure a la posició 1 de la memòria compartida el valor 42.
    ; 5. Escriure a la posició 2 de la memòria compartida el valor (2 3).
    '((crea-bolla (2 3))
      (mou (4 5))
      (pinta (6 7))
      (escriu-memoria 1 42)
      (escriu-memoria 2 (2 3))))


;; Recordau que qualsevol funció no predefinida necessària per a l'agent, ha d'estar definida en aquest fitxer
;; i ha d'estar prefixada amb "agent-abc123-" per evitar conflictes amb altres agents o mòduls.
;; Per exemple:
(defun agent-abc123-longitud (llista)
    "Retorna la longitud d'una llista."
    (cond ((null llista) 0)
          (t (+ 1 (agent-abc123-longitud (cdr llista))))))