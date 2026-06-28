#   Code : "Analyse quantitative"
# 
#   Cette code fait partie de une mémoire fin de master apellée :
#               
#           "Voir tout en noir :
#
#   Approche expérimentale d’une production de crayons de charbon de bois 
#   en contexte contrôlé et comparaison au registre orné"
#
#   Mémoire de Master 2 Sciences et Technologies
#   Mention : Archéologie, Sciences pour l’archéologie
#   Parcours : Préhistoire – Géoarchéologie – Archéozoologie 
#
#   Encadré par : Catherine Ferrier, Iñaki Intxaurbe, Jean Claude Leblanc 
#   et Mª Ángeles Medina Alcaide
#
#   Authors: 
#   Luc Bouchaud : https://github.com/lucbouchaud
#   Frédéric Santos : https://github.com/frederic-santos
#   Iñaki Intxaurbe Alberdi : https://github.com/inakiintxaurbe
#
#   Copyright (C) 2025  Luc Bouchaud, Frédéric Santos & Iñaki Intxaurbe
#
#   SPDX-License-Identifier: AGPL-3.0 (citation mandatory)
# ======================================================

pkgs <- c(
  "haven", 
  "dplyr", 
  "FactoMineR", 
  "factoextra", 
  "ggpubr", 
  "multcomp", 
  "car",
  "janitor",
  "MASS",
  "readODS",
  "rpart",
  "rpart.plot",
  "vcd",
  "psych",
  "irr"
  )

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(haven)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggpubr)
library(multcomp)
library(car)
library(janitor)
library(MASS)
library(readODS)
library(rpart)
library(rpart.plot)
library(vcd)
library(psych)
library(irr)

# DATA

file_path <- file.path(getwd(), "Comparaison traces et traits.ods")

BAW <- read_ods(
  path = file_path,
  sheet = 5,
  progress = FALSE
) |>
  clean_names(case = "upper_camel") |>
  mutate(across(where(is.character), as.factor))

summary(BAW)

# View DATA (BAW) et tapply

View(BAW)

tapply(BAW$XArea,BAW$Combinaison,summary)

# ANOVA et XArea (1)

boxplot(XArea ~ Combinaison, data = BAW)

anovaXarea <- aov(
  XArea ~ Combinaison, 
  data = BAW
  )

summary(anovaXarea)

tapply(BAW$Mean,BAW$Combinaison,summary)

boxplot(Mean ~ Combinaison, data = BAW)

anovaPercentGris <- aov(
  PercentGris ~ Combinaison, 
  data = BAW
  )

summary(anovaPercentGris)

tapply(BAW$ScoreGlobal,BAW$Combinaison,summary)

boxplot(ScoreGlobal ~ Combinaison, data = BAW)

anovaScoretotalC<- aov(
  ScoreGlobal ~ Combinaison, 
  data = BAW
  )

summary(anovaScoretotalC)

# XArea moyenne et tapply (1)

XAreamoyenne <- aggregate(
  XArea ~ Combinaison, 
  data = BAW, mean
  )

XAreamoyenne[order(XAreamoyenne$XArea), ]

Grismoyenne <- aggregate(
  PercentGris ~ Combinaison, 
  data = BAW, mean
  )

Grismoyenne[order(Grismoyenne$PercentGris), ]

Scoretotalité <- aggregate(
  ScoreGlobal ~ Combinaison, 
  data = BAW, mean
  )

Scoretotalité [order(-Scoretotalité$ScoreGlobal), ]

tapply(BAW$XArea,BAW$Foyer,summary)

# ANOVA et XArea (2)

boxplot(XArea ~ Foyer, data = BAW)

anovaXarea <- aov(
  XArea ~ Foyer, 
  data = BAW
  )

summary(anovaXarea)

tapply(BAW$Mean,BAW$Foyer,summary)

boxplot(Mean ~ Foyer, data = BAW)

anovaPercentGris <- aov(
  PercentGris ~ Foyer, 
  data = BAW
  )

summary(anovaPercentGris)

tapply(BAW$ScoreGlobal,BAW$Foyer,summary)

# ANOVA et XArea (3)

boxplot(ScoreGlobal ~ Foyer, data = BAW)

anovaScoretotalF<- aov(
  ScoreGlobal ~ Foyer, 
  data = BAW
  )

summary(anovaScoretotalF)

# XArea moyenne et tapply (2)

XAreamoyenne <- aggregate(
  XArea ~ Foyer, 
  data = BAW, mean
  )

XAreamoyenne[order(XAreamoyenne$XArea), ]

Grismoyenne <- aggregate(
  PercentGris ~ Foyer, 
  data = BAW, mean
  )

Grismoyenne[order(Grismoyenne$PercentGris), ]

Scoretotalité <- aggregate(
  ScoreGlobal ~ Foyer, 
  data = BAW, mean
  )

Scoretotalité [order(Scoretotalité$ScoreGlobal), ]

summary(BAW$XArea)

tapply(BAW$XArea,BAW$Pg,summary)

boxplot(XArea ~ Pg, data = BAW)

# t test

t.test(XArea ~ Pg, data = BAW)

tapply(BAW$XArea,BAW$Ei,summary)

boxplot(XArea ~ Ei, data = BAW,
        xlab = "Contexte de combustion",  
        ylab = "Couverture",
        main = "Pouvoir de couverture en fonction du contexte de combustion",  
        type = "l",
        col = "grey"
        ) 

t.test(XArea ~ Ei, data = BAW)

tapply(BAW$XArea,BAW$Srar,summary)

boxplot(XArea ~ Srar, data = BAW)

t.test(XArea ~ Srar, data = BAW)

tapply(BAW$XArea,BAW$Cl,summary)

boxplot(XArea ~ Cl, data = BAW,
        xlab = "Lieu de stockage",  
        ylab = "Couverture",
        main = "Pouvoir de couverture en fonction du lieu de stockage",  
        type = "l",
        col = "grey"
        )             

t.test(XArea ~ Cl, data = BAW)

summary(BAW$PercentGris)

tapply(BAW$PercentGris,BAW$Pg,summary)

boxplot(PercentGris ~ Pg, data = BAW)

t.test(PercentGris ~ Pg, data = BAW)

tapply(BAW$PercentGris,BAW$Ei,summary)

boxplot(PercentGris ~ Ei, data = BAW,
        xlab = "Contexte de combustion",  
        ylab = "Couleur",
        main = "Nuances de gris en fonction du contexte de combustion",  
        type = "l",
        col = "grey"
        ) 

t.test(PercentGris ~ Ei, data = BAW)

tapply(BAW$PercentGris,BAW$Srar,summary)

boxplot(PercentGris ~ Srar, data = BAW)

t.test(PercentGris ~ Srar, data = BAW)

tapply(BAW$PercentGris,BAW$Cl,summary)

boxplot(PercentGris ~ Cl, data = BAW)

t.test(PercentGris ~ Cl, data = BAW)