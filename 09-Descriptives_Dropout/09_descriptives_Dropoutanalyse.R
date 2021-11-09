# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#   Deskriptive Statistik und Analyse der Dropouts                            #
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(psych)
library(tidyverse)
library(skimr)

# 1. "Eyeballing" -------------------------------------------------------------

## Alle Daten:
skim(data)

## Interventionsgruppe:
data %>%
  filter(group == 0) %>%
  skim()

## Kontrollgruppe:
data %>%
  filter(group == 1) %>%
  skim()

## Histogramm des primären Outcomes (PSS-Stress)
multi.hist(data %>% select(pss.0, pss.1, pss.2), ncol = 3)

# 2. Dropout-Analyse ----------------------------------------------------------

## Gesamte Daten
with(data, {
  c(sum(is.na(pss.0)),
    sum(is.na(pss.1)),
    sum(is.na(pss.2)))
}) -> na.all

na.all.p <- na.all/nrow(data)

## Interventionsgruppe
data %>% 
  filter(group == 1) %>%
  with({
    c(sum(is.na(pss.0)),
      sum(is.na(pss.1)),
      sum(is.na(pss.2)))
  }) -> na.ig

na.ig.p <- na.ig/nrow(data %>% filter(group == 1))

## Kontrollgruppe
data %>% 
  filter(group == 0) %>%
  with({
    c(sum(is.na(pss.0)),
      sum(is.na(pss.1)),
      sum(is.na(pss.2)))
  }) -> na.cg

na.cg.p <- na.cg/nrow(data %>% filter(group == 0))

## Sammeln in Dataframe
na <- data.frame(na.all, na.all.p = na.all.p*100, 
                 na.ig, na.ig.p = na.ig.p*100, 
                 na.cg, na.cg.p = na.cg.p*100)

## Zeilennamen des Dataframe ändern
rownames(na) = c("t0", "t1", "t2")
na






