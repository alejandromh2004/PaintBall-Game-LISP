# PAINTBALL LISP - Pràctica Final LP

## Instruccions

### Com carregar el joc

1. Obriu el vostre entorn LISP (fora de la carpeta del projecte)
2. Carregueu el controlador principal:
   ```lisp
   (load "amh976_jvs328_PAINTBALL_LISP/paintball.lsp")
   ```

### Com iniciar una partida

Podeu iniciar el joc amb el mapa per defecte (`bait.map`):

```lisp
(inici)
```

O especificar un mapa concret de la carpeta `maps/`:

```lisp
(inici "amh976_jvs328_PAINTBALL_LISP/maps/basic3.map")
```

---

## Controls de Joc

* **ENTER**: Següent torn (pas a pas).
* **b**: **Enrere** (Torna al torn anterior en la història, max 5).
* **s**: **Saltar** (Demana el número de rondes a saltar).
* **q**: **Sortir** de la partida.

---

## Funcionalitats Extra (Part no obligatòria)

Hem implementat els següents apartats opcionals per millorar la nota de la pràctica:

### Controlador General

* **Suport de mapes grans**: El motor gràfic s'ajusta automàticament per suportar mapes de fins a **60x60** caselles, calculant la mida de cel·la òptima.
* **Lògica de 1500 torns**: Implementació estricta del límit de temps amb missatges de final de partida.
* **Penalització per pintura**: Les boles pateixen un increment en el temps de recuperació (`tr-moure`) si intenten moure's per caselles que no són del seu color d'equip.
* **Memòria compartida**: Implementada i utilitzada activament pels agents per coordinar el descobriment de bases i laboratoris.
* **Millor control de torns**: Sistema que permet fer **endavant i endarrere** (tecla `b`), saltar N rondes o anar pas a pas.

### Mòdul Gràfic

* **Renderitzat Optimitzat**: Sistema de dibuix **diferencial** que només repinta les caselles que canvien d'estat entre torns, millorant el rendiment en mapes grans.
* **Matriu de caselles detallada**:
  * **Color de fons**: Tintat suau segons el color de pintura dominant a la casella.
  * **Accions (Fletxes)**: Representació visual dels moviments (taronja) i trets (vermell) de cada torn.
  * **Temps de recuperació**: Barres visuals de *cooldown* sobre cada unitat que indiquen quant falta per a la següent acció.

### Agents Intel·ligents

* **Estratègies Avançades**: Ús de la memòria compartida per evitar exploracions redundants i coordinar atacs a bases enemigues. Gestió dinàmica de rols (explorador vs atacant).

### Altres / Ergonomia

* **Tail Call Optimization (TCO)**: Inclusió del fitxer `tco.lsp` amb una macro que permet recursivitat infinita, evitant desbordaments de pila en partides llargues de 1500 torns.

---

## Mapes Provats

S'han verificat tots els mapes de la carpeta `maps/`. El joc és compatible amb totes les mides i gestiona correctament les condicions de victòria per eliminació o per temps.

**Estudiants:**

- Alejandro Martinez Hermosa (`amh976`)
- Javier Vivo Samaniego (`jvs328`)
