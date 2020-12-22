#Project
library(gdata)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
# How did Trump Presidency affect H-1B Visas
# 1. visa petition from 2011 to 2016 (Obama Presidency).
petBO <- read.csv("2011-2016 petition.csv")
petBOa<- petBO %>% filter(CASE_STATUS == "CERTIFIED")
write.csv(petBOa, "new 2011-16 petition.csv", row.names = FALSE)

#2. visa petition from 2017-2019(Trump)
pet17 <- read.xls("2017.xlsx")