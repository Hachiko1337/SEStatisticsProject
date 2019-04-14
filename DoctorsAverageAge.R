myData <- read.csv("database.csv",TRUE,",")

names(myData) <- c("No.","Last_Name","First_Name","Sex_M_F","Age","Domain","Years.Of.Practice","
                   Number_of_pacients","Succ_cases")

#head(myData)

x <- table(myData$Age)

as.vector(x)
#average <- weighted.mean(myData$Age)
#average

par(pch=22, col="orange") # plotting symbol and color 

#par(mfrow=c(2,4)) # daca vreau ca toate grafurile generate sa fie pe aceeasi pagina
plot(x,type="s", 
     main="Doctor's ages",
     ylab="Number of Doctors",
     xlab="Age") 