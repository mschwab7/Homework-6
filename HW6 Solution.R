happy = readRDS("data/HAPPY.rds")
str(happy)
library(tidyverse)
library(ggplot2)
library(ggmosaic)

temp = happy %in% c("IAP", "DK", "NA")
temp[1: 100]

#Replace values with NA
happy = replace(happy, happy == "IAP", NA)
happy = replace(happy, happy == "DK", NA)
happy = replace(happy, happy == "NA", NA)
View(happy)

#Replace "89 OR OLDER" with 89
happy = replace(happy, happy == "89 OR OLDER", 89)
happy$AGE = as.numeric(happy$AGE)
str(happy)

#Change categorical columns into factors
happy$YEAR = as.factor(happy$YEAR)

happy$HAPPY = as.factor(happy$HAPPY)
levels(happy$HAPPY)

happy = happy %>% mutate(SEX = as.factor(SEX), MARITAL = as.factor(MARITAL), 
                 DEGREE = as.factor(DEGREE), FINRELA = as.factor(FINRELA), 
                 HEALTH = as.factor(HEALTH), PARTYID = as.factor(PARTYID),
                 POLVIEWS = as.factor(POLVIEWS))

str(happy)

#Reorder the levels that have a natural ordering

levels(happy$YEAR)

levels(happy$MARITAL)
happy$MARITAL = reorder(happy$MARITAL, happy$AGE, fun = mean(), na.rm = TRUE)
levels(happy$MARITAL)

levels(happy$DEGREE)
levels(happy$DEGREE) = c("LT HIGH SCHOOL", "HIGH SCHOOL", "JUNIOR COLLEGE",
                         "BACHELOR", "GRADUATE")
levels(happy$DEGREE)

levels(happy$FINRELA)
levels(happy$FINRELA) = c("FAR BELOW AVERAGE", "BELOW AVERAGE", "AVERAGE",
                          "ABOVE AVERAGE", "FAR ABOVE AVERAGE")
levels(happy$FINRELA)

levels(happy$HEALTH)
levels(happy$HEALTH) = c("POOR", "FAIR", "GOOD", "EXCELLENT")
levels(happy$HEALTH)

happy %>% ggplot(aes(x = HAPPY)) + geom_bar() + facet_wrap(~DEGREE)
happy %>% ggplot(aes(x = HAPPY)) + geom_bar() + facet_wrap(~MARITAL)

happy %>% group_by(FINRELA, HEALTH) %>% summarise(m.happy = mean(as.numeric(HAPPY), na.rm = TRUE))
happy.fin = happy %>% group_by(FINRELA, HEALTH) %>% summarise(m.happy = mean(as.numeric(HAPPY), na.rm = TRUE))
happy.fin %>% ggplot(aes(x = FINRELA, fill = HEALTH)) + geom_bar(aes(weight = m.happy),
                                                              position = position_dodge())

happy %>% group_by(MARITAL, FINRELA) %>% summarise(m.happy2 = mean(as.numeric(HAPPY), na.rm = TRUE))
happy.mar = happy %>% group_by(MARITAL, FINRELA) %>% summarise(m.happy2 = mean(as.numeric(HAPPY), na.rm = TRUE))
happy.mar %>% ggplot(aes(x = FINRELA, fill = MARITAL)) + geom_bar(aes(weight = m.happy2),
                                                                  position = position_dodge())
