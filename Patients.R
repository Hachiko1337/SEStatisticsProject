leukemia <- read.csv("dbLeukemia.csv", TRUE, ",")
head(leukemia)

hist(leukemia$Success, main = "Success rate", ylab = "Number of treated patients", xlab = "0 = Failed, 1 = Successfully treated")

pie(leukemia$Age, main = "Ages", ylab = "age", xlab = "no.")

plot(leukemia$Age, leukemia$Success, ylab = "y", xlab = "x")





cancer <- read.csv("dbCancer.csv", TRUE, ",")
head(cancer)

hist(cancer$Success, main = "Success rate", ylab = "Number of treated patients", xlab = "0 = Failed, 1 = Successfully treated")

pie(cancer$Age, main = "Ages", ylab = "age", xlab = "no.")

plot(cancer$Age, cancer$Success, ylab = "y", xlab = "x")



# Simple Pie Chart
slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls, main="Pie Chart of Countries")

slices <- c(10, 12, 4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")


# 1
labs <- c(paste(0, 17, sep = "-"), paste(18, 24, sep = "-"), paste(seq(25, 65, by = 10), seq(0 + 35 - 1, 75 - 1, by = 10),
                sep = "-"), paste(75, "+", sep = ""))
labs

leukemia$AgeGroup <- cut(leukemia$Age, breaks = c(paste(0, 17, sep = "-"), paste(18, 24, sep = "-"), 
                                                  paste(seq(25, 65, by = 10), seq(0 + 35 - 1, 75 - 1, by = 10),
                                                  sep = "-"), 75), labels = labs, right = FALSE)

# 2
leukemia$age_grp <- leukemia$age
leukemia$age_grp <- ifelse((leukemia$age>=0 & leukemia$age<=18) , '0-18',leukemia$age_grp)
leukemia$age_grp <- ifelse((leukemia$age>18 & leukemia$age<=25) , '18-25',leukemia$age_grp)
leukemia$age_grp <- ifelse((leukemia$age>25 & leukemia$age<=35) , '25-35',leukemia$age_grp)
leukemia$age_grp <- ifelse((leukemia$age>35 & leukemia$age<=45) , '35-45',leukemia$age_grp)
leukemia$age_grp <- ifelse((leukemia$age>45 & leukemia$age<=55) , '45-55',leukemia$age_grp)
leukemia$age_grp <- ifelse((leukemia$age>55 & leukemia$age<=65) , '55-65',leukemia$age_grp)
leukemia$age_grp <- ifelse((leukemia$age>65 & leukemia$age<=75) , '65-75',leukemia$age_grp)
leukemia$age_grp <- ifelse((leukemia$age>75) , '75+',leukemia$age_grp)
leukemia$age_grp<-as.factor(leukemia$age_grp)
summary(leukemia$age_grp)
library(dplyr)
leukemia <- select(leukemia, -(age) )

# 3
library(data.table)
require(data.table)
agebreaks <- c(0,18,25,35,45,55,65,75,150)
agelabels <- c("0-17","18-24","25-35","35-45","45-55","55-65","65-75","75+")

setDT(data)[ , Agegroups := cut(age, 
                                breaks = agebreaks, 
                                right = FALSE, 
                                labels = agelabels)]

