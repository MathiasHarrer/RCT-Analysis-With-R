# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#   MULTIPLE IMPUTATION                                                       #
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(mice)
library(miceadds)
library(tidyverse)
library(plot.matrix)
library(mitml)
data$X = NULL

# 1. Missing Data Pattern -----------------------------------------------------
md.pattern(data, rotate.names = TRUE)


# 2. Influx-Outflux-Plot ------------------------------------------------------
flux(data)
fluxplot(data)


# 3. Ausschluss von Variablen -------------------------------------------------

imp0 <- mice(data, maxit = 0)
imp0$loggedEvents

outlist <- c(imp0$loggedEvents$out, "sess", "zuf.1")

imp.data <- data %>% select(-all_of(outlist))


# 4. Imputationsmatrix --------------------------------------------------------

# Definiere eine "Inlist" (zwingend einzuschließende Variablen)
inlist <- c("sex", "age", "ethn", "child", "prevpyschoth", "ft.helps",
            "prevtraining", "rel", "degree", "inc", "pss.0")

# - mincor (Minimale Interkorrelation) = 0.01
# - minpuc (Proportion of Usable Cases) = 0.1
pred <- quickpred(imp.data,
                  mincor = 0.05,
                  minpuc = 0.1,
                  inc = inlist)

# Mittlere Anzahl an Prädiktoren
rowsums <- table(rowSums(pred))
rowsums

rowsums[-1] %>% 
  {as.numeric(names(.)) %*% . / sum(.)}


# Setze Prädiktor "group" auf 0
pred[,"group"] = 0


# Visualisiere die Prädiktormatrix
plot(pred, main = "Imputation Matrix",
     col = c("grey", "blue"),
     xlab = "predicts",
     ylab = "is predicted",
     las = 2, cex.axis = 0.5)


# Nochmalige Nullimputation; zeige gewählte Methode
imp0 = mice(imp.data, maxit = 0,
            predictorMatrix = pred)
imp0$method


# Finde Variablen ohne Missings
no.missings = imp0$method == ""

# Setze Imputationsmethode auf "bygroup"
imp0$method %>% 
  replace(., . != "", "bygroup") -> imp.method

# Definiere Imputationsfunktion für "bygroup" variablen
imp0$method[!no.missings] %>% as.list() -> imp.function

# Definiere Liste mit Gruppenvariable
rep("group", length(imp.method[!no.missings])) %>% 
  set_names(names(imp.method[!no.missings])) %>% 
  as.list() -> imp.group.variable


# 5. Imputation ---------------------------------------------------------------
  
set.seed(123)
mice(imp.data, 
     predictorMatrix = pred,
     method = imp.method,
     imputationFunction = imp.function,
     group = imp.group.variable,
     m = 25, maxit = 25) -> imp

save(imp, file = "data/imp.rda")


# 6. Diagnostik ---------------------------------------------------------------

# Trace plots
plot(imp, 
     layout = c(4, ceiling(sum(!no.missings)/2)))

# Kernel densities
densityplot(imp, ~ pss.1 + pss.2)
densityplot(imp, ~ pss.1 + pss.2 | as.factor(group))





