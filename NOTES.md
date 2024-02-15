# Notes

## spiegazione a Mirko

Esperimento attuale è così, Mirko. 

Due moduli:
* Channel1: computa G = gradient(source) + gradient(target)
* Channel2: compute distanceBetween(source,target) e completa l'espressione recuperando G

Tutti i nodi eseguono sia Channel1 sia Channel2, tranne il nodo X che fa offloading di Channel2 sul cloud.

Ora, se Channel2 non computasse distanceBetween ma solo valutasse l'espressione locale, la cosa già funzionerebbe, perché non dovrebbe eseguire una funzione che richiede comunicazione "per conto di X", semplicemente prenderebbe l'output di Channel1 e computerebbe un'espressione locale, e l'output finale lo rimanda al nodo X. Invece l'esperimento attuale richiede il forwarding dei contesti dei nodi.

## Experiment

- If device `X` offloads `Channel2` (which does `distanceBetween`) to the cloud, it means the cloud computes 
  it including `distanceBetween`, and therefore the cloud should share the `distanceBetween`'s export with `X`'s neighbours.

