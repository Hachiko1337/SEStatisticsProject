myData <- read.csv("database.csv",TRUE,",")

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

# problema la barplot-ul asta este ca afiseaza foarte inghesuit,
# din cauza ca sunt foarte multe nume

# head(myData)
# 
# x <- table(myData$Domain)
# domainName <- names(x)
# domainName
# 
# y <- table(myData$Succ_cases)
# succCases <- as.vector(y)
# succCases
# 
# data <- data.frame(domainName,succCases)
# boxplot(myData$Age) average age graph
?barplot