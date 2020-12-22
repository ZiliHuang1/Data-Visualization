library(gdata)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)


#Question1 Create the interactive visualization
crime<-read.csv("crime incident report.csv")
data<-crime %>% group_by(DISTRICT, MONTH, .drop = FALSE) %>% count(name = "RECORDS") %>% drop_na()
data<- data[-c(1,2,3,4,5,6,7,8,9,10,11,12),]
newdata<- data %>%pivot_wider(names_from = DISTRICT, values_from =RECORDS)
write.csv(newdata, "new crime data.csv",row.names = FALSE)

#Question2
mob<- read.csv("2020_US_Region_Mobility_Report.csv")
usmob<- mob %>% select(country_region,date, retail_and_recreation_percent_change_from_baseline,
                       grocery_and_pharmacy_percent_change_from_baseline,
                       parks_percent_change_from_baseline,transit_stations_percent_change_from_baseline,
                       workplaces_percent_change_from_baseline,residential_percent_change_from_baseline) %>%
  group_by(country_region,date) %>%
  summarise_at(c("retail_and_recreation_percent_change_from_baseline",
                 "grocery_and_pharmacy_percent_change_from_baseline",
                 "parks_percent_change_from_baseline","transit_stations_percent_change_from_baseline",
                 "workplaces_percent_change_from_baseline", "residential_percent_change_from_baseline"), 
               mean, na.rm = TRUE)
write.csv(usmob, "usmob.csv", row.names = FALSE)

#Extra
datae<-read.xls("empincome.xlsx")
income<- datae %>% 
  mutate(date = as.Date(paste(year,month,day, sep = "-")))
newrow<-subset(income, date > "2020-09-01")
income<- income[,-c(1,2,3)]
income[income['date'] > "2020-09-01",2:4] <- NA
covidincome<-left_join(income, newrow, by = "date")

write.csv(covidincome, "covidincome.csv", row.names = FALSE)
