#   Code : "Analyse qualitative"
# 
#   Cette code fait partie de une mémoire fin de master apellée :
#               
#           " Voir tout en noir :
#
#   Approche expérimentale d’une production de crayons de charbon de bois 
#   en contexte contrôlé et comparaison au registre orné "
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

library (haven)
library(car)
library(dplyr)
library(janitor)
library(MASS)
library(readODS)
library(rpart)
library(rpart.plot)
library(vcd)
library(psych)
library(irr)

# DAT 1

file_path <- file.path(getwd(), "Comparaison traces et traits.ods")

dat1 <- read_ods(
  path = file_path,
  sheet = 1,
  progress = FALSE
) |>
  clean_names(case = "upper_camel") |>
  mutate(across(where(is.character), as.factor))

summary(dat1)

# DAT 2

dat2 <- read_ods(
  path = file_path,
  sheet = 2,
  progress = FALSE
) |>
  clean_names(case = "upper_camel") |>
  mutate(across(where(is.character), as.factor))

summary(dat2)

# DAT 3

dat3<- read_ods(
  path = file_path,
  sheet = 3,
  progress = FALSE
) |>
  clean_names(case = "upper_camel") |>
  mutate(across(where(is.character), as.factor))

summary(dat3)

colnames(dat3)

# View "dat2" et kappa2

View(dat2)

kappa2(data.frame(dat1$ScorePouvoirCouvrant, dat2$ScorePouvoirCouvrant), weight = "squared")

kappa2(data.frame(dat1$ScoreHomogeneite, dat2$ScoreHomogeneite), weight = "squared")

kappa2(data.frame(dat1$ScoreMorphologie, dat2$ScoreMorphologie), weight = "squared")

kappa2(data.frame(dat1$ScoreMoyen, dat2$ScoreMoyen), weight = "squared")

# Test Kappa pondéré pour SCORE1 VS SCORE A l'AVEUGLE

kappa2(data.frame(dat1$ScorePouvoirCouvrant, dat3$ScorePouvoirCouvrant), weight = "squared")

kappa2(data.frame(dat1$ScoreHomogeneite, dat3$ScoreHomogeneite), weight = "squared")

kappa2(data.frame(dat1$ScoreMorphologie, dat3$ScoreMorphologie), weight = "squared")

kappa2(data.frame(dat1$ScoreMoyen, dat3$ScoreMoyen), weight = "squared")

# Test Kappa pondéré pour SCORE2 VS SCORE A l'AVEUGLE

kappa2(data.frame(dat2$ScorePouvoirCouvrant, dat3$ScorePouvoirCouvrant), weight = "squared")

kappa2(data.frame(dat2$ScoreHomogeneite, dat3$ScoreHomogeneite), weight = "squared")

kappa2(data.frame(dat2$ScoreMorphologie, dat3$ScoreMorphologie), weight = "squared")

kappa2(data.frame(dat2$ScoreMoyen, dat3$ScoreMoyen), weight = "squared")

## Régression logistique ordinale

dat2.polr <- dat2 |>
  mutate(
    across(starts_with("Score"), as.ordered)
    )

reg.pc <- polr(
  ScorePouvoirCouvrant ~ PG * EI * SrAr * CL,
  data = dat2.polr
) |>
  step(k = 6, trace = 0)

Anova(reg.pc)

reg.pc <- polr(
  ScoreMorphologie ~ PG * EI * SrAr * CL,
  data = dat2.polr
) |>
  step(k = 6, trace = 0)

Anova(reg.pc)

reg.pc <- polr(
  ScoreHomogeneite ~ PG * EI * SrAr * CL,
  data = dat2.polr
) |>
  step(k = 6, trace = 0)

Anova(reg.pc)

reg.pc <- polr(
  ScoreMoyen ~ PG * EI * SrAr * CL,
  data = dat2.polr
) |>
  step(k = 6, trace = 0)

Anova(reg.pc)

ScoreTot <- aggregate(
  ScoreMoyen ~ Combinaison, 
  data = dat2, 
  mean
  )

ScoreTot [order(-ScoreTot$ScoreMoyen), ]

## Résumés de données


GE <- filter(
  dat2, 
  PG == "G", 
  EI == "E"
  )

GI <- filter(
  dat2, 
  PG == "G", 
  EI == "I"
  )

PE <- filter(
  dat2, 
  PG == "P", 
  EI == "E"
  )

PI <- filter(
  dat2, 
  PG == "P", 
  EI == "I"
  )

##Moyenne par foyer

GE |>
  summarise(
    Moy_pouvoir_couvrant = mean(ScorePouvoirCouvrant),
    Moy_morphologie = mean(ScoreMorphologie),
    Moy_homogeneite = mean(ScoreHomogeneite),
    Moy_moyen = mean(ScoreMoyen),
    N = n()
  ) |>
  mutate(
    across(where(is.numeric), round, 2)
    )

GI |>
  summarise(
    Moy_pouvoir_couvrant = mean(ScorePouvoirCouvrant),
    Moy_morphologie = mean(ScoreMorphologie),
    Moy_homogeneite = mean(ScoreHomogeneite),
    Moy_moyen = mean(ScoreMoyen),
    N = n()
  ) |>
  mutate(
    across(where(is.numeric), round, 2)
    )

PE |>
  summarise(
    Moy_pouvoir_couvrant = mean(ScorePouvoirCouvrant),
    Moy_morphologie = mean(ScoreMorphologie),
    Moy_homogeneite = mean(ScoreHomogeneite),
    Moy_moyen = mean(ScoreMoyen),
    N = n()
  ) |>
  mutate(
    across(where(is.numeric), round, 2)
    )

PI |>
  summarise(
    Moy_pouvoir_couvrant = mean(ScorePouvoirCouvrant),
    Moy_morphologie = mean(ScoreMorphologie),
    Moy_homogeneite = mean(ScoreHomogeneite),
    Moy_moyen = mean(ScoreMoyen),
    N = n()
  ) |>
  mutate(
    across(where(is.numeric), round, 2)
    )

dat2 |>
  group_by(PG) |>
  summarise(
    Moy_pouvoir_couvrant = mean(ScorePouvoirCouvrant),
    Moy_morphologie = mean(ScoreMorphologie),
    Moy_homogeneite = mean(ScoreHomogeneite),
    Moy_moyen = mean(ScoreMoyen),
    N = n()
  ) |>
  mutate(
    across(where(is.numeric), round, 2)
    )

dat2 |>
  group_by(EI) |>
  summarise(
    Moy_pouvoir_couvrant = mean(ScorePouvoirCouvrant),
    Moy_morphologie = mean(ScoreMorphologie),
    Moy_homogeneite = mean(ScoreHomogeneite),
    Moy_moyen = mean(ScoreMoyen),
    N = n()
  ) |>
  mutate(
    across(where(is.numeric), round, 2)
    )

dat2 |>
  group_by(CL) |>
  summarise(
    Moy_pouvoir_couvrant = mean(ScorePouvoirCouvrant),
    Moy_morphologie = mean(ScoreMorphologie),
    Moy_homogeneite = mean(ScoreHomogeneite),
    Moy_moyen = mean(ScoreMoyen),
    N = n()
  ) |>
  mutate(
    across(where(is.numeric), round, 2)
    )

dat2 |>
  group_by(SrAr) |>
  summarise(
    Moy_pouvoir_couvrant = mean(ScorePouvoirCouvrant),
    Moy_morphologie = mean(ScoreMorphologie),
    Moy_homogeneite = mean(ScoreHomogeneite),
    Moy_moyen = mean(ScoreMoyen),
    N = n()
  ) |>
  mutate(
    across(where(is.numeric), round, 2)
    )

### Répartition des scores par croisement de 2 variables

##  Pour le pouvoir couvrant :

xtabs(
  formula = ~ ScorePouvoirCouvrant + EI + CL,
  data = dat2
)

xtabs(
  formula = ~ ScorePouvoirCouvrant + CL + EI,
  data = dat2
)

xtabs(
  formula = ~ ScoreMorphologie + PG + EI,
  data = dat2
)

xtabs(
  formula = ~ ScoreMorphologie + EI + PG,
  data = dat2
)

xtabs(
  formula = ~ ScoreMorphologie + EI + CL,
  data = dat2
)

xtabs(
  formula = ~ ScoreMorphologie + CL + EI,
  data = dat2
)

##Pour le score Homogénéité

xtabs(
  formula = ~ ScoreHomogeneite + PG + SrAr,
  data = dat2
)

xtabs(
  formula = ~ ScoreHomogeneite + SrAr + PG,
  data = dat2
)

xtabs(
  formula = ~ ScoreHomogeneite + CL + SrAr,
  data = dat2
)

xtabs(
  formula = ~ ScoreHomogeneite + SrAr + CL,
  data = dat2
)

### Moyenne des scores par croisement de 2 variables

## EI VS CL !

dat2 |>
  group_by(EI, CL) |>
  summarise(
    Moy_pouvoir_couvrant = mean(ScorePouvoirCouvrant),
    Moy_morphologie = mean(ScoreMorphologie),
    Moy_homogeneite = mean(ScoreHomogeneite),
    Moy_moyen = mean(ScoreMoyen),
    N = n()
  ) |>
  mutate(
    across(where(is.numeric), round, 2)
    )

## EI VS SrAr

dat2 |>
  group_by(EI, SrAr) |>
  summarise(
    Moy_pouvoir_couvrant = mean(ScorePouvoirCouvrant),
    Moy_morphologie = mean(ScoreMorphologie),
    Moy_homogeneite = mean(ScoreHomogeneite),
    Moy_moyen = mean(ScoreMoyen),
    N = n()
  ) |>
  mutate(
    across(where(is.numeric), round, 2)
    )

## EI VS PG

dat2 |>
  group_by(EI, PG) |>
  summarise(
    Moy_pouvoir_couvrant = mean(ScorePouvoirCouvrant),
    Moy_morphologie = mean(ScoreMorphologie),
    Moy_homogeneite = mean(ScoreHomogeneite),
    Moy_moyen = mean(ScoreMoyen),
    N = n()
  ) |>
  mutate(
    across(where(is.numeric), round, 2)
    )

## CL VS SrAr

dat2 |>
  group_by(CL, SrAr) |>
  summarise(
    Moy_pouvoir_couvrant = mean(ScorePouvoirCouvrant),
    Moy_morphologie = mean(ScoreMorphologie),
    Moy_homogeneite = mean(ScoreHomogeneite),
    Moy_moyen = mean(ScoreMoyen),
    N = n()
  ) |>
  mutate(
    across(where(is.numeric), round, 2)
    )

## 4. Arbre de décision

## L'arbre du pouvoir couvrant

arbre.pc <- rpart(
  ScorePouvoirCouvrant ~ EI + SrAr + CL + PG,
  data = dat2,
  minbucket = 5
)
rpart.plot(
  arbre.pc, 
  extra = 1
  )

## L'arbre de l'homogénéité

arbre.pc <- rpart(
  ScoreHomogeneite ~ EI + SrAr + CL + PG,
  data = dat2,
  minbucket = 5
)
rpart.plot(
  arbre.pc, 
  extra = 1
  )

## L'arbre de la morphologie

arbre.pc <- rpart(
  ScoreMorphologie ~ EI + SrAr + CL + PG,
  data = dat2,
  minbucket = 5
)
rpart.plot(
  arbre.pc, 
  extra = 1
  )

## L'arbre du score moyen

arbre.pc <- rpart(
  ScoreMoyen ~ EI + SrAr + CL + PG,
  data = dat2,
  minbucket = 5
)
rpart.plot(
  arbre.pc, 
  extra = 1
  )
