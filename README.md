# Paintball - Pràctica Final de LISP

[cite_start]Aquest repositori conté la pràctica final de l'assignatura Llenguatges de Programació, curs 2025-26, de la Universitat de les Illes Balears[cite: 2].

[cite_start]El projecte consisteix en el desenvolupament d'un programa funcional en XLISP-PLUS [cite: 10] [cite_start]que implementa una simulació anomenada "Paintball"[cite: 3]. [cite_start]En aquesta simulació experimental, dos equips s'enfronten utilitzant unitats (bases i bolles) i tres substàncies de pintura diferents (vermell, verd i blau)[cite: 5]. [cite_start]L'objectiu principal és aconseguir pintar la base enemiga dels tres colors per destruir-la i guanyar la partida[cite: 8].

[cite_start]L'arquitectura del codi es divideix en tres components principals[cite: 10]:

* [cite_start]**Controlador general:** S'encarrega d'iniciar la partida, carregar el mapa, controlar el final de la partida, executar els torns alternant equips i mantenir l'estat actualitzat[cite: 11].
* [cite_start]**Mòdul gràfic:** Dibuixa l'estat del mapa en un torn, segons la indicació del controlador general[cite: 12].
* **Agents intel·ligents:** Són el "cervell" de cada equip. [cite_start]Hi ha dos agents (un per cada equip) que trien les accions que una unitat ha de dur a terme segons la seva limitada visió de l'estat de la partida[cite: 13].

[cite_start]Tot el projecte s'ha desenvolupat complint amb les restriccions habituals de disseny funcional[cite: 14]. [cite_start]En concret, no es permet fer reassignacions ni mutació d'estructures[cite: 111]. [cite_start]El tractament seqüencial es duu a terme a través de crides recursives o funcions d'ordre superior (com mapcar o reduce), i mai a través de funcions iteratives[cite: 112].
