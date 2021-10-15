# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#   SIMULATIONSBEISPIEL: RANDOMISIERUNG                                       #
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(tidyverse)
library(ggplot2)


# 0. Notwendige Funktionen ----------------------------------------------------

randomize = function(blocksize, N){
  block = rep(1:ceiling(N/blocksize), each = blocksize)
  a1 = data.frame(block, rand=runif(length(block)), envelope= 1: length(block))
  a2 = a1[order(a1$block,a1$rand),]
  a2$arm = rep(c("Arm 1", "Arm 2"),times = length(block)/2)
  assign = a2[order(a2$envelope),]
  return(assign$arm)
}

plotter = function(m1, m0, mui = 45){
  plot.data <- data.frame(M = c(m1, m0), Gruppe = rep(c("Gruppe 1", "Gruppe 2"), 
                                       each = length(m1)))
  ggplot(plot.data, aes(x = M, fill = Gruppe)) + geom_density(alpha = 0.5) +
    geom_vline(xintercept = mui, linetype = "dotted") + xlab(expression(hat(mu))) +
    ylab("") + theme_minimal()
}



# 1. Simulation (k=1) ---------------------------------------------------------

set.seed(123)

# Simuliere 10 Werte für Variable "Alter" (Populationsmittelwert: 45 Jahre)
alter <- rnorm(10, 45, 15) %>% round()

# Randomisiere die 10 Personen in zwei Gruppen
gruppe <- randomize(blocksize = 2, N = 10)

# Berechne den Mittelwert & Mittelwertsunterscheid beider Gruppen
m1 <- mean(alter[gruppe == "Arm 1"]) 
m0 <- mean(alter[gruppe == "Arm 2"])
m.diff <- mean(alter[gruppe == "Arm 1"]) - mean(alter[gruppe == "Arm 2"])

m.diff


# 2. Simulation (k=100) -------------------------------------------------------

# Wiederhole 1. 100-mal
set.seed(123)
for (i in 2:100){
  
  alter <- rnorm(10, 45, 15) %>% round()
  gruppe <- randomize(blocksize = 2, N = 10)
  
  m1[i] <- mean(alter[gruppe == "Arm 1"], na.rm = TRUE) 
  m0[i] <- mean(alter[gruppe == "Arm 2"], na.rm = TRUE)
  m.diff[i] <- mean(alter[gruppe == "Arm 1"]) - mean(alter[gruppe == "Arm 2"])
  
}

mean(m.diff)
plotter(m1, m0)



# 3. Simulation (k=10.000) ----------------------------------------------------

# Wiederhole 1. 10.000-mal
set.seed(123)
for (i in 101:10000){
  
  alter <- rnorm(10, 45, 15) %>% round()
  gruppe <- randomize(blocksize = 2, N = 10)
  
  m1[i] <- mean(alter[gruppe == "Arm 1"], na.rm = TRUE) 
  m0[i] <- mean(alter[gruppe == "Arm 2"], na.rm = TRUE)
  m.diff[i] <- mean(alter[gruppe == "Arm 1"]) - mean(alter[gruppe == "Arm 2"])
  
}

mean(m.diff)
plotter(m1, m0)








