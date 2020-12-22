# Vorlage für Wasserhaushaltssimulationen großer Punktkollektive

In diesem Repository wird Schritt für Schritt gezeigt, wie man den Wasserhaushalt großer Punktkollektive wie der BWI mithilfe des R-Pakets 'LWFBrook90R' (Schmidt-Walter et al., 2020) berechnen kann. Es kann so als Vorlage für die Bearbeitung ähnlicher Fragestellungen genutzt werden. 

Insbesondere wird gezeigt, wie man

- die Funktion `msiterunLWFB90` effektiv nutzt,
- externe Datenquellen einbindet um Klimadaten bereitzustellen, und
- Funktionen definiert und übergibt, mithilfe derer die Simulationsergebnisse in Dateien oder Datenbanken gespeichert werden können.

Als Beispiel dienen die Daten des [Supplementary](https://doi.org/10.5281/zenodo.1491520) der Publikation zur [NFIWADS-Datenbank](https://www.openagrar.de/receive/openagrar_mods_00044576) (Schmidt-Walter et al., 2019), mithilfe derer die dort veröffentlichten Simulationsergebnisse mit dem aktuellen LWFBrook90R-Paket reproduziert werden können.


# Literatur

Schmidt-Walter, P., Ahrends, B., Mette, T., Puhlmann, H., Meesenburg, H. (2019):
NFIWADS: the water budget, soil moisture, and drought stress indicator database for the German National Forest Inventory (NFI).
doi:10.1007/s13595-019-0822-2 
Annals of Forest Science 76:39

Schmidt-Walter, P. Trotsiuk, V., Meusburger, K., Zacios, M., Meesenburg, H. (2020):
Advancing simulations of water fluxes, soil moisture and drought stress by using the LWF-Brook90 hydrological model in R.
doi:10.1016/j.agrformet.2020.108023
Agricultural and Forest Meteorology 291:108023 
