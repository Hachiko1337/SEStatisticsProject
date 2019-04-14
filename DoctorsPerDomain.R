myData <- read.csv("database.csv",TRUE,",")

names(myData) <- c("No.","Last_Name","First_Name","Sex_M_F","Age","Domain","Years.Of.Practice","
                   Number_of_pacients","Succ_cases")

#head(myData)

x <- table(myData$Domain)

par(pch=22, col="black")
lbls <- paste(names(x), " = ", x, sep="")
pie(x,labels = lbls,main="Doctors per Domain")
