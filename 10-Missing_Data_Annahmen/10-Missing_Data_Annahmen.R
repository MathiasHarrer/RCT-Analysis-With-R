# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#   MULTIPLE IMPUTATION                                                       #
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(mice)
library(miceadds)
library(tidyverse)


is.na(data) %>% 
  colSums() %>% 
  barplot(las = 2)


md.pattern(data, rotate.names = TRUE)

flux(data)
fluxplot(data)


