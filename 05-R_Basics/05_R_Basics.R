# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#   R Basics                                                                  #
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# Übungsaufgaben -----------------------------------------------------------

data <- read.csv("data/data.csv")
library(tidyverse)

age.log <- log(data$age)
pss.1.squared <- data$pss.1^2

mean(data$cesd.2, na.rm = TRUE)
sd(data$cesd.2, na.rm = TRUE)

list(mean(data$cesd.2, na.rm = TRUE),
     sd(data$cesd.2, na.rm = TRUE))

class(data$mbi.0)

data$age.50plus <- data$age >= 50
data$pss.diff <- data$pss.0 - data$pss.1

data %>% 
  filter(group == 1, sex == 1) %>% 
  pull(age) %>% 
  mean()

data[3:4, "ft.helps"] <- c(NA, NA)

data[order(data$age),]







