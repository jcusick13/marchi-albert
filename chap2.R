library(dplyr)
setwd("Documents/baseball/r/data")

Batting <- read.csv("Batting.csv")
players <- group_by(Batting, playerID)
dataframe.AB <- summarise(players, Career.AB=sum(AB, na.rm=TRUE))

Batting <- merge(Batting, dataframe.AB, by="playerID")
Batting.5000 <- subset(Batting, Career.AB >= 5000)

ab.hr.so <- function(d){
  c.AB <- sum(d$AB, na.rm=TRUE)
  c.HR <- sum(d$HR, na.rm=TRUE)
  c.SO <- sum(d$SO, na.rm=TRUE)
  
  data.frame(AB=c.AB, HR=c.HR, SO=c.SO)
}

aaron <- subset(Batting.5000, playerID=="aaronha01")
ab.hr.so(aaron)

d.5000 <- group_by(Batting.5000, playerID) %>% do(ab.hr.so(.))
head(d.5000)

with(d.5000, plot(HR/AB, SO/AB))
with(d.5000, lines(lowess(HR/AB, SO/AB)))
