# Paintball - Pràctica Final de LISP

Aquest repositori conté la pràctica final de l'assignatura Llenguatges de Programació, curs 2025-26, de la Universitat de les Illes Balears.

El projecte consisteix en el desenvolupament d'un programa funcional en XLISP-PLUSque implementa una simulació anomenada "Paintball". En aquesta simulació experimental, dos equips s'enfronten utilitzant unitats (bases i bolles) i tres substàncies de pintura diferents (vermell, verd i blau). L'objectiu principal és aconseguir pintar la base enemiga dels tres colors per destruir-la i guanyar la partida.

L'arquitectura del codi es divideix en tres components principals:

* **Controlador general:** S'encarrega d'iniciar la partida, carregar el mapa, controlar el final de la partida, executar els torns alternant equips i mantenir l'estat actualitzat.
* **Mòdul gràfic:** Dibuixa l'estat del mapa en un torn, segons la indicació del controlador general.
* **Agents intel·ligents:** Són el "cervell" de cada equip. Hi ha dos agents (un per cada equip) que trien les accions que una unitat ha de dur a terme segons la seva limitada visió de l'estat de la partida.

Tot el projecte s'ha desenvolupat complint amb les restriccions habituals de disseny funcional. En concret, no es permet fer reassignacions ni mutació d'estructures. El tractament seqüencial es duu a terme a través de crides recursives o funcions d'ordre superior (com mapcar o reduce), i mai a través de funcions iteratives.

(load "proyectos/projecte_inicial/paintball.lsp")

(inici "proyectos/projecte_inicial/maps/two.map")
