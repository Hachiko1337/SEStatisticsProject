myData <- read.csv("database.csv",TRUE,",")

names(myData) <- c("No.","Last_Name","First_Name","Sex_M_F","Age","Domain","Years.Of.Practice","
                   Number_of_pacients","Succ_cases")

#head(myData)

# PieChart for Male / Female

x <- table(myData$Sex_M_F) # table ia de cate ori se repeta valorile

#as.vector(x) # memoreaza doar valorile, fara denumiri

#labels <- c("Female","Male")
pie(x,names(x),main="Male / Female")