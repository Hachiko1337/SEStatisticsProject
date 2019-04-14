myData <- read.csv("database.csv",TRUE,",")

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