# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#   Eyeballing und Manipulation von Studiendaten                              #
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(tidyverse)

# 1. Eyeballing ---------------------------------------------------------------
glimpse(data)


# 2. Zuordnung Objektklassen --------------------------------------------------
data$group        <- as.factor(data$group)
data$sex          <- as.factor(data$sex)
data$ethn         <- as.factor(data$ethn)
data$prevtraining <- as.factor(data$prevtraining)
data$prevpsychoth <- as.factor(data$prevpsychoth)
data$ft.helps     <- as.factor(data$ft.helps)
data$rel          <- as.factor(data$rel)
data$degree       <- as.factor(data$degree)
data$employ       <- as.factor(data$employ)
data$inc          <- as.factor(data$inc)
data$child        <- as.factor(data$child)


# 3. Data Slicing und Nutzung der Pipe ----------------------------------------

# Extrahieren Sie einen Datensatz, der nur die Daten der Personen 1-5 und die 
# Depressionsvariable "cesd" zu allen drei Messzeitpuntken enthält.

data_depr <- data[1:5, c("cesd.0", "cesd.1", "cesd.2")]

# Wie hoch ist der mittlere Depressionswert "cesd.0" bei Personen, deren 
# Stresswert "pss.0" über dem Mittel von 25.5 liegt?

data  %>%
  filter(pss.0 > 25.5) %>%
  pull(cesd.0) %>%
  mean()




