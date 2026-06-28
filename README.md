# Comparaison-traces-et-traits-en-charbon-de-bois
**Created by**: 

- Luc Bouchaud (https://github.com/lucbouchaud)
- Frédéric Santos (https://github.com/frederic-santos)
- Iñaki Intxaurbe (https://github.com/inakiintxaurbe)

*PACEA UMR 5199* 

**(Université du Bordeaux)**  

**e-mail**: 

- luc.bouchaud@etu.u-bordeaux.fr
- frederic.santos@u-bordeaux.fr
- inaki.intxaurbe@u-bordeaux.fr  

**ORCID**: 

- https://orcid.org/0009-0003-0673-9578 (Luc Bouchaud)
- https://orcid.org/0000-0003-1445-3871 (Frédéric Santos)
- https://orcid.org/0000-0003-3643-3177 (Iñaki Intxaurbe)

**Date**: 2026-01-04  

*Copyright (C) 2026 Luc Bouchaud, Frédéric Santos et Iñaki Intxaurbe*

---

## Aperçu

Cette code fait partie de une mémoire fin de master apellée "Voir tout en noir : Approche expérimentale d’une production de crayons de charbon de bois en contexte contrôlé et comparaison au registre orné"

Mémoire de Master 2 Sciences et Technologies

Mention : Archéologie, Sciences pour l’archéologie

Parcours : Préhistoire – Géoarchéologie – Archéozoologie 

Université de Bordeaux

Encadré par : 

- Catherine Ferrier (orcid : https://orcid.org/0000-0003-2347-7170 ; email : catherine.ferrier@u-bordeaux.fr)
- Iñaki Intxaurbe (orcid : https://orcid.org/0000-0003-3643-3177 ; email : inaki.intxaurbe@u-bordeaux.fr)
- Jean Claude Leblanc (orcid : https://orcid.org/0009-0000-7058-8696 ; email : jean.leblanc82@orange.fr)
- Mª Ángeles Medina Alcaide (orcid : https://orcid.org/0000-0002-7983-5988 ; email : m.medina.alcaide@gmail.com)

---

## Fonctionnement

Les deux scripts R utilisent la commande `getwd()` pour rechercher automatiquement le jeu de données dans le répertoire de travail. Par conséquent, les scripts et le fichier de données (Comparaison traces et traits.ods) doivent être enregistrés dans **le même dossier** afin que les analyses puissent être exécutées correctement.

Structure du dépôt :

- `Analyse-qualitative.R` → R script
- `Analyse-quantitative.R` → R script
- `Comparaison traces et traits.ods` → fichier de données

Veillez à ce que les deux scripts et le fichier `.ods` se trouvent dans le même répertoire avant d'exécuter les analyses.

---

## Prérequis

- **R** (>= 4.0 recommandé)
- Paquets utilisés : `haven`, `dplyr`, `car`, `janitor`, `MASS`, `readODS`, `rpart`, `rpart.plot`, `vcd`, `psych`, `irr`, `FactoMineR`, `factoextra`, `ggpubr`, `multcomp`.

---

## Licence

AGPL-3.0 *(La citation de l'auteur et du dépôt est **obligatoire**)*

Veuillez citer ceci comme : 

- Bouchaud, L., Santos, F., & Intxaurbe, I. (2026). Comparaison traces et traits en charbon de bois [Data set]. GitHub. [https://github.com/inakiintxaurbe/Comparaison-traces-et-traits-en-charbon-de-bois]
