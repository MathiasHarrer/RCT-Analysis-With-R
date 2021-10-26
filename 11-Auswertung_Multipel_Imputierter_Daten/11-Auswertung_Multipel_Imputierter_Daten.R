# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#   AUSWERTUNG MULTIPEL IMPUTIERTER DATEN                                     #
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(tidyverse)
library(magrittr)
library(mice)
library(miceadds)
library(skimr)
library(mitml)
library(purrr)
library(writexl)

# 0. Notwendige Funktionen ----------------------------------------------------

skimReport = function(data, round = FALSE){
  require(skimr)
  require(dplyr)
  require(purrr)
  x = skim(data)
  N = nrow(data)
  with(x, {
    skim_type == "factor" -> fac.mask
    vars = skim_variable[fac.mask]
    n.unique = factor.n_unique[fac.mask]
    strsplit(factor.top_counts, "\\, |\\:")[fac.mask] %>% 
      purrr::map(function(x){
        as.numeric(x) -> x
        data.frame(category = x[seq(1, length(x), by = 2)],
                   count = x[seq(2, length(x), by = 2)]) %>% 
          dplyr::mutate(percent = count/N)
      }) %>% 
      {names(.) = vars;.} %>% 
      map_df(~as.data.frame(.), .id = "variable")
  }) -> factors
  with(x, {
    skim_type == "numeric" -> num.mask
    data.frame(variable = skim_variable[num.mask],
               mean = numeric.mean[num.mask],
               sd = numeric.sd[num.mask],
               n = N-x$n_missing[num.mask],
               n.missing = n_missing[num.mask],
               perc.missing = n_missing[num.mask]/N)
  }) -> numerics
  if (round == TRUE){
    within(factors, {
      percent = round(percent*100, 2)
    }) -> factors
    within(numerics, {
      mean = round(mean, 2)
      sd = round(sd, 2)
      perc.missing = round(perc.missing*100, 2)
    }) -> numerics
  }
  dat = list(factors = factors,
             numerics = numerics)
  class(dat) = c("list", "skimReport")
  return(dat)
}

print.skimReport = function(x){
  cat("Categorial Variables \n ------------------ \n")
  cat("\n")
  print(x$factors)
  cat("\n")
  cat("Numeric Variables \n ------------------ \n")
  cat("\n")
  print(x$numerics)
} 


# 1. Deskriptive Auswertung der Imputierten Daten -----------------------------

# Konvertiere Imputationen (Klasse 'mids') in eine 'mitml'-Liste
implist <- mids2mitml.list(imp)
class(implist)

# Definiere kategorische Variablen
catvars = c("group", "sex", "ethn", "prevtraining", "prevpsychoth",
            "ft.helps", "rel", "degree", "inc", "child")

# In allen Imputationssets:
# Konvertiere alle definierten Variablen zum factor
implist %>% 
  map(~mutate_at(., catvars, as.factor)) -> implist

# In allen Imputationssets:
# - Erstelle deskriptive Statistiken
# - Aggregiere alle numerischen Werte
implist %>% 
  map(~skimReport(.)) -> descriptives

descriptives %>%
  map(~.$numerics[,-1]) %>% 
  Reduce(`+`, .)/25 -> num.desc.full


# In der Interventionsgruppe:
# Aggregiere alle numerischen Werte
implist %>% 
  map(~filter(., group == 1)) %>% 
  map(~skimReport(.)) -> descriptives.ig

descriptives.ig %>%
  map(~.$numerics[,-1]) %>% 
  Reduce(`+`, .)/25 -> num.desc.ig


# In der Kontrollgruppe:
# Aggregiere alle numerischen Werte
implist %>% 
  map(~filter(., group == 0)) %>% 
  map(~skimReport(.)) -> descriptives.cg

descriptives.cg %>%
  map(~.$numerics[,-1]) %>% 
  Reduce(`+`, .)/25 -> num.desc.cg


# Extrahiere Variablennamen
variable = descriptives[[1]]$numerics[,1]


# Kombiniere alle Ergebnisse und speichere als Excel-Worksheet
cbind(variable, num.desc.full, num.desc.ig, num.desc.cg) %>% 
  write_xlsx("imp_numeric_descriptives.xlsx")



# 2. Analysis of Covariance ---------------------------------------------------

## 2.1 ANCOVA in einem Imputationsset -----------------------------------------

m.ancova <- lm(pss.1 ~ 1 + group + scale(pss.0), data = implist[[1]]) 
summary(m.ancova)

# Prüfe Voraussetzungen:
# - Varianzhomogenität: gleichförmige Streuung der Residuen um 0
#   (-> Residuals vs. Fitted Plot)
# - Normalverteilung der Residuen (-> Q-Q Plot)

# Residuals vs. Fitted
plot(m.ancova, 1)

# Q-Q Plot
plot(m.ancova, 2)

# Analysis of Variance des Modells
anova(m.ancova)

# Zum Vergleich: Fit ohne Kovariate
lm(pss.1 ~ 1 + group, data = implist[[1]]) %>% 
  anova()


## 2.2 ANCOVA in Multipel Imputierten Daten -----------------------------------

# Liste muss Klasse "mitml.list" besitzen!
class(implist) = c("mitml.list", "list")

# Fitte ANCOVA-Modell in allen MI-Sets
with(implist, lm(pss.1 ~ 1 + group + pss.0)) %>% 
  testEstimates() -> mi.ancova

# Extrahiere F-Werte
with(implist, lm(pss.1 ~ 1 + group + pss.0)) %>% 
  map_dbl(~anova(.)$`F value`[1]) -> Fvalues

# Kombiniere F-Werte mit Rubin-Regeln
micombine.F(Fvalues, 1)


## 2.3 ANCOVA bei sekundären Endpunkten ---------------------------------------

# Definiere Namen alles zu analysierenden Variablen
# (ohne Zeitangabe)
test.vars = c("pss", "cesd", "hadsa",
              "isi", "mbi", "pswq")

# Generiere Modellformeln für Post- und FU-Messzeitpunkt
formula.1 = paste(test.vars, ".1 ~ 1 + group + ", test.vars, ".0",
                  sep = "")
formula.2 = paste(test.vars, ".2 ~ 1 + group + ", test.vars, ".0",
                  sep = "")

# Nutze "map", um Funktion über alle _Formeln_(!) anzuwenden:
# 1. Berechne ANCOVA für spezifisches Outcome, unter Kontrolle der
#    Baselinemessung
# 2. Extrahiere Ergebnisse des Gruppenterms
# 3. Extrahiere F-Werte und aggregiere mit Rubin-Regeln
# 4. Gebe generiertem data.frame neue Variablennamen
# 5. Füge Spalten für Variable und MZP hinzu
as.list(c(formula.1, formula.2)) %>% 
  map_dfr(function(x){
    
    with(implist, lm(as.formula(x))) %>% 
      testEstimates() -> res
    res$estimates[2,] -> res.group
    
    with(implist, lm(as.formula(x))) %>% 
      map_dbl(~anova(.)$`F value`[1]) -> Fvalues
    micombine.F(Fvalues, 1, display = FALSE) -> Fvalue.mi
    
    c(res.group, Fvalue.mi)
  }) %>% 
  set_colnames(c("md", "SE", "t", "df.mi",
                 "p.t", "RIV", "FMI", "F", "p.F",
                 "df.1", "df.2")) %>% 
  bind_cols(variable = rep(test.vars, 2), 
            time = rep(1:2, each = 6), .) -> mi.ancova.full
  
# Speichere Ergebnisse als Excel-Sheet
write_xlsx(mi.ancova.full, "mi_ancova_full.xlsx")






