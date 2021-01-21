library(gdata)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

#Question1: Plot a scatter plot with number of patents on y-axis and population on x-axis.

#Citation1: US Patenting.csv derived from https://www.uspto.gov/web/offices/ac/ido/oeip/taf/countyall/usa_county_gd.htm
#Citation2: Population.xlsx derived from https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html

Patent<-read.csv("US Patenting.csv")
Population<-read.csv("Population.csv")
newpatent<- select(Patent, State =State.or.Territory, County=Regional.Area.Component, Patent=X2015)
newpop<- select(Population, Area=Geographic.Area, population=X2015) 
newpop$Area<- tolower(substr(newpop[,1],2,1000))
patentnew2<-unite(newpatent, Area, c(County, State), sep = ", ", remove=TRUE) 
patentnew2$Area<-tolower(patentnew2$Area)
data<-inner_join(patentnew2,newpop, by ="Area")
ggplot(data, aes(x=population, y=Patent)) +
  geom_point() +geom_text(aes(label =Area))+
  ggtitle("The relationship between patents number and their population by counties")


#Question 2: 
#1. Number of people living in urban and rural areas, World, 1960 to 2017
data21<- read.csv("urban-and-rural-population.csv")
data21new<- data21 %>% 
  pivot_longer(cols=c(Rural.population,Urban.population), names_to ="variables",values_to ="Population")%>%
  group_by(Year,variables) %>% summarise(population=sum(Population))
ggplot(data21new,aes(x=Year, y=population, colour=variables)) +geom_line() +geom_point() +
  ggtitle("Number of people living in urban and rural areas, World, 1960 to 2017")

#2. Urban and rural population projected to 2050, World, 1500 to 2050
data22<- read.csv("urban-and-rural-population-2050.csv")
data22new<- data22 %>% 
  pivot_longer(cols=c(Urban,Rural), names_to ="variables",values_to ="Population")%>%
  group_by(Year,variables) %>% summarise(population=sum(Population))
ggplot(data22new,aes(x=Year, y=population, fill=variables)) +geom_area() +
  ggtitle("Urban and rural population projected to 2050, World, 1500 to 2050")

#3. Urban Population vs GDP per capita for the year 2016
data23<-read.csv("urbanization-vs-gdp.csv")
a<- data23 %>% select(Entity,Year, Total.population) %>% filter(Year=="2013")
b<- data23 %>% filter(Year=="2015")
new<- inner_join(b, a, by="Entity")
new<-new[,-c(4,8)]
ggplot(new, aes(x=Real.GDP.per.capita.in.2011USD, y=Urban.population.long.run.to.2016, color =Continent))+
  geom_point(aes(size=Total.population.y))+
  scale_x_log10() +geom_text(aes(label =Entity))+
  xlab("GDP per capita in 2011US$")+ylab("Share of population living in urban area")+
  ggtitle("Urban Population vs GDP per capita for the year 2015")

#Question3: create a box plot for average SAT scores for reading/writing and math
score<-read.csv("2018-19 SAT score Mass.csv")
data3<-score%>% pivot_longer(cols = c(Reading.or.Writing,Math), names_to ="Subjects", values_to ="Scores")
data3<- data3 %>%select(Subjects, Scores)
data3<- data3[1:713,]
data3$Subjects <- as.factor(data3$Subjects)
data3$Scores <- as.numeric(as.character(data3$Scores))
ggplot(data3, aes(x=Subjects, y=Scores)) + geom_boxplot() +ggtitle("Average SAT scores for reading/writing and math")

#Extra Points
income<- read.csv("median household income Mass.csv")
income1<-income %>% separate(col = Distruct.Name, into = c('Weston', 'District'), sep = " School", remove = TRUE)
income<- income1 %>% select(Median.Household.Income=Median.Household.Income.., District.Name=Weston)
score2<-score%>% pivot_longer(cols = c(Reading.or.Writing,Math), names_to ="Subjects", values_to ="Scores") 
newdata<-inner_join(score2, income, by ="District.Name")
newdata$Scores<- as.numeric(as.character(newdata$Scores))
newdata$Median.Household.Income <- as.numeric(gsub('[$,]', '', newdata$Median.Household.Income))
newdata$Subjects<- as.factor(newdata$Subjects)
ggplot(newdata,aes(x=Median.Household.Income, y=Scores, colour=Subjects)) +geom_point()+
  ggtitle("Relationship between Avg. SAT scores and median household income in that school district")
  
#Conclusion：The Average SAT scores are positively correalted to median household income, namely higher household income leads to higher SAT scores.


