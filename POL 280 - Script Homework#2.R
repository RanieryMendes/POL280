#LIBRARIES USED
library(foreign)
library(tibble)
library(plyr) 
library(descr)
library(ggplot2)
library(dplyr)
library(moments)
library(stringr)
library(haven)

#read data
dataset_RevisedANES <- read_dta("revisedanes2016small.dta")


#Analisys of immigration level

immigration_level <- dataset_RevisedANES$K12...11

immigration_level <- mapvalues(immigration_level, from = c("1", "2", "3", "4"), to=c("1. Increased", "2. Kept the same", "3. Decreased a bit", "4. Decreased a lot"))

table(immigration_level)

ggplot(data= dataset_RevisedANES) + geom_bar(mapping = aes(immigration_level))+
  aes(y=stat(count)/sum(stat(count)))+
  labs(title = "Figure 1. Potential Voters’ Support for Increasing Immigration", caption = "Data Source: ANES 2016")+
  xlab("1 increase – 4 decrease a lot")+ ylab("Percentage")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


level_job <- dataset_RevisedANES$K12...6
table(level_job)

cor(dataset_RevisedANES$K12...11, dataset_RevisedANES$K12...6)


#question ii

#need to create subset
presidential_vote <- dataset_RevisedANES$A02
presidential_vote <- mapvalues(presidential_vote, from = c("1", "2", "3"), to=c("Clinton", "Trump", "Other"))

reg_new <- mapvalues(region, from = c("1", "2", "3", "4"), to=c("NE", "MW", "S", "W"))

region <- dataset_RevisedANES$R24

pres_sub <- subset(dataset_RevisedANES,select = "R24" )
trump <- 
table(pres_sub)

CrossTable(presidential_vote, reg_new,  prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE         )
