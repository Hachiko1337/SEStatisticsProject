#install.packages("RMySQL")

library("RMySQL")

mydb = dbConnect(RMySQL::MySQL(),
                 user='username',
                 password='password',
                 dbname='doctors',
                 host='192.168.64.2')

myData <- read.csv("TABLE_DOCTORS.csv",TRUE,",")

#----------/////// Years of practice

names(myData) <- c("No.","Last_Name","First_Name","Sex_M_F","Age","Domain","Years.Of.Practice","
                   Number_of_pacients","Succ_cases")

name <- iconv(myData$Last_Name, "utf-8", "ASCII", sub="")
firstName <- iconv(myData$First_Name, "utf-8", "ASCII", sub="")

barplot(myData$Years.Of.Practice,
        names.arg=paste(name,firstName),
        main="Doctor's years of practice",
        xlab="Years of practice",
        horiz=TRUE,
        las = 1,
        cex.names=0.7,
        border=1)

#------------/////// Age

myData <- read.csv("TABLE_DOCTORS.csv",TRUE,",")

names(myData) <- c("No.","Last_Name","First_Name","Sex_M_F","Age","Domain","Years.Of.Practice","
                   Number_of_pacients","Succ_cases")
x <- base::table(myData$Age)

as.vector(x)

par(pch=22, col="orange") # plotting symbol and color 

plot(x,type="s", 
     main="Doctor's ages",
     ylab="Number of Doctors",
     xlab="Age") 

#-------------/////// Doctors per domain

x <- base::table(myData$Domain)

par(pch=22, col="black")
lbls <- paste(names(x),
              " = ", 
              x,
              sep="")
pie(x,
    labels = lbls,
    main="Doctors per Domain")

#-------------///////  Male / Female

x <- base::table(myData$Sex_M_F)

#as.vector(x)

pie(x,names(x),main="Male / Female")

#-------------/////// Successful cases


names(myData) <- c("No.","Last_Name","First_Name","Sex_M_F","Age","Domain","Years.Of.Practice","
                   Number_of_pacients","Succ_cases")

name <- iconv(myData$Last_Name, "utf-8", "ASCII", sub="")
firstName <- iconv(myData$First_Name, "utf-8", "ASCII", sub="")

barplot(myData$Succ_cases,
        names.arg=paste(name,firstName),
        #names.arg=myData$Domain,
        main="The number of successful cases for each doctor",
        xlab="Successful cases",
        horiz=TRUE,
        las = 1,
        cex.names=0.7,
        border=1)