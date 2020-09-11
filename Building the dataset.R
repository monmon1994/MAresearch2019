# This code is to replicate the analyses and findings from my 2019
# Masters Research Report. Code developed by Monique Bennett, Wits University.

## Required packages ##
#install.packages(c("car", "wbstats", "readr"))
library(car)
library(wbstats)
library(readr)
library(readxl)
library(dplyr)

## Load the data ##
load("MA_bennett.RData")

## Developing country list ##

allcountries <- read_csv("countryincomelist.csv")

developing <- subset(allcountries[, c("country_name", "iso3c", "region", 
                                      "income_class")], income_class != "High income")

## World Bank Data ##

wb_2018 <- wb(country = "all", indicator = c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD", 
                                             "NE.EXP.GNFS.CD", "NE.IMP.GNFS.CD", 
                                             "NY.GDP.TOTL.RT.ZS", "EN.ATM.CO2E.KT", 
                                             "CC.EST", "GE.EST","RQ.EST", 
                                             "PV.EST", "RL.EST", "VA.EST"), startdate = 2018, 
              enddate = 2018, return_wide = TRUE)

wb_developing <- merge(wb_2018, developing, by.x = "iso3c", by.y = "iso3c") 

## EPI data ##

epi_snap <- read_csv("epi2018countrysnapshotv01.csv")

wb_episnap <- merge(wb_developing, epi_snap, by.x = "iso3c", by.y = "iso")

wb_episnap <- select(wb_episnap, c(-iso2c, -code))

## Lexical Democracy data and dummy ##

lied <- read_excel("lied_v3.xls")

lied_2018 <- subset(lied[, c("countryn", "year", "lexical_index")], year == 2015)

lied_developing <- merge(lied_2018, developing, by.x = "countryn", 
                         by.y = "country_name")

lied_developing$lied_dummy <- NA

lied_developing$lied_dummy[lied_developing$lexical_index == 0] <- 0

lied_developing$lied_dummy[lied_developing$lexical_index == 1] <- 0

lied_developing$lied_dummy[lied_developing$lexical_index == 2] <- 0

lied_developing$lied_dummy[lied_developing$lexical_index == 3] <- 0

lied_developing$lied_dummy[lied_developing$lexical_index == 6] <- 1

lied <- select(lied_developing, c(-countryn, -region, -income_class, -year))

wb_episnap <- merge(wb_episnap, lied, by.x = "iso3c", by.y = "iso3c")

wb_episnap <- select(wb_episnap, c(-EPI2018Rank, -country.y, -country.x))

## Variable diagnostics ##

### Dependent variable: EPI

# The 2018 Environmental Performance Index (EPI) ranks 180 countries on 
# 24 performance indicators across ten issue categories covering environmental 
# health and ecosystem vitality. These metrics provide a gauge at a national 
# scale of how close countries are to established environmental policy goals.

# The EPI reveals a tension between two fundamental dimensions of sustainable 
# development: (1) environmental health, which rises with economic growth and 
# prosperity, and (2) ecosystem vitality, which comes under strain from 
# industrialization and urbanization. Good governance emerges as the critical 
# factor required to balance these distinct dimensions of sustainability.

summary(wb_episnap$EPI2018Score) # scale is 0 to 100 (worst to best)

hist(wb_episnap$EPI2018Score)

plot(wb_episnap$EPI2018Score, type="n")
text(wb_episnap$EPI2018Score, labels = wb_episnap$iso3c)

p1 <-  powerTransform(EPI2018Score ~ 1, data = wb_episnap, family = "bcPower")
summary(p1)

## Independent Variables

# Rule of Law
summary(wb_episnap$RL.EST)
hist(wb_episnap$RL.EST) # scale is -2 to 1

# Political Violence
summary(wb_episnap$PV.EST)
hist(wb_episnap$PV.EST) # scale is -3 to 1

# Voice and Acc
summary(wb_episnap$VA.EST)
hist(wb_episnap$VA.EST) # scale is -2 to 2  

# Gov effectiveness 
summary(wb_episnap$GE.EST)
hist(wb_episnap$GE.EST) # -2 to 2

# Regulatory Quality
summary(wb_episnap$RQ.EST) 
hist(wb_episnap$RQ.EST) 

# Lexical Democracy
summary(wb_episnap$lied_dummy)
plot(wb_episnap$lied_dummy) # 0 and 1

#### GDP per capita
hist(wb_episnap$NY.GDP.PCAP.CD)
hist(log(wb_episnap$NY.GDP.PCAP.CD))



