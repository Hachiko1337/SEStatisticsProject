library(sqldf)
library(plotrix)
predictions = read.csv("predictie.csv", TRUE, ";")

#NUMBER OF PACIENTS SUFFERING FROM THE TOP 5 MOST PREDICTED DISEASES (OR ATTEMPTED TO PREDICT)

diseasePredictionFrame = data.frame(disease = predictions$Disease)
diseasePredictionQuery = sqldf("select disease, count(*) from diseasePredictionFrame group by disease order by count(*) desc limit 5")
#svg("Top-5-most-predicted-diseases.svg")
par(bg = "bisque2")
text (as.array(diseasePredictionQuery$`count(*)`)
   , labels = as.array(diseasePredictionQuery$`count(*)`)
   , y = barplot(diseasePredictionQuery$`count(*)`
               , names.arg = as.array(diseasePredictionQuery$disease)
               , horiz = TRUE
               , main = "Occurrences for predicted diseases"
               , col = heat.colors(length(as.array(diseasePredictionQuery$`count(*)`)))
               , xlim = c(0, as.array(diseasePredictionQuery$`count(*)`)[1]*2)
               , cex.main = 1.5
               , xlab = "Number of patients"
               , las = 1
               , cex.names = 0.8)
   , pos = 4
   , font = 5
   , cex = 0.8)
#dev.off()


#SUCCESS AND FAILURE RATES FOR TOP 5 MOST USED TREATMENTS

treatmentSuccessRateFrame = data.frame(treatment = predictions$Treatment, success = predictions$Success)
treatmentSuccessRateQuery = sqldf("select treatment from treatmentSuccessRateFrame group by treatment order by count(*) desc limit 5")

for ( treatment in treatmentSuccessRateQuery$treatment ) {
     successQuery = sqldf(paste("select count(*) from treatmentSuccessRateFrame where treatment = '", treatment, "' and success = 0", sep = ""))
     failureQuery = sqldf(paste("select count(*) from treatmentSuccessRateFrame where treatment = '", treatment, "' and success = 1", sep = ""))
     totalQuery = sqldf(paste("select count(*) from treatmentSuccessRateFrame where treatment = '", treatment, "'", sep = ""))
     #svg(paste("Success-and-failure-rates-for-", treatment, ".svg", sep = ""))
     par(bg = "bisque2")
     pie(c(as.numeric(successQuery), as.numeric(failureQuery))
       , labels = paste (c(" Success ", " Failure "), round(c(as.numeric(successQuery), as.numeric(failureQuery))/sum(c(as.numeric(successQuery), as.numeric(failureQuery))) * 100), "%")
       , radius = 1
       , col = c("palegreen3", "red3")
       , main = treatment
       , cex.main = 1.5
       )
     title(sub = paste(treatment, " has been applied on ", as.numeric(totalQuery), " patients\n\n", "Based on predictions", sep = ""), cex.sub = 1.3)
     #dev.off()
     }
  
#RECURRENCE CHANCE FOR TOP 5 MOST PREDICTED DISEASES

diseasesRecurrenceRateFrame = data.frame(disease = predictions$Disease, recurrence = predictions$Recurrence)
topDiseasesRecurrenceQuery = sqldf("select disease from diseasesRecurrenceRateFrame group by disease order by count(*) desc limit 5")

for ( disease in topDiseasesRecurrenceQuery$disease ) {
     recurrentQuery = sqldf(paste("select count(*) from diseasesRecurrenceRateFrame where disease = '", disease, "' and recurrence = 'Yes'", sep = ""))
     notRecurrentQuery = sqldf(paste("select count(*) from diseasesRecurrenceRateFrame where disease = '", disease, "' and recurrence = 'No'", sep = ""))
     totalQuery = sqldf(paste("select count(*) from diseasesRecurrenceRateFrame where disease = '", disease, "'", sep = ""))
     #svg(paste("Recurrence-chance-for-", disease, ".svg", sep = ""))
     par(bg = "bisque2")
     pie3D(round(c(as.numeric(recurrentQuery), as.numeric(notRecurrentQuery))/sum(c(as.numeric(recurrentQuery), as.numeric(notRecurrentQuery))) * 100)
         , labels=paste(c("Positive ", "Negative "), round(c(as.numeric(recurrentQuery), as.numeric(notRecurrentQuery))/sum(c(as.numeric(recurrentQuery), as.numeric(notRecurrentQuery))) * 100), "%", sep = "")
         , explode=0.1
         , main=disease
         , col = c("red3", "palegreen3")
         , radius = 1
         , cex.main = 1.5
         , shade = 0.6
         , theta = 0.8)
     title(sub = paste("Patients suffering from ", disease, ":", as.numeric(totalQuery), "\n\nBased on predictions", sep = ""), cex.sub = 1.3)
     #dev.off()
     }

#TOP 5 MOST USED TREATMENTS

treatmentCountFrame = data.frame(treatment = predictions$Treatment)
treatmentCountQuery = sqldf("select treatment, count(*) from treatmentCountFrame group by treatment order by count(*) desc limit 5")

#svg("Top-5-most-used-treatments.svg")
par(bg = 'bisque2')
treatmentCountBarplot = barplot(treatmentCountQuery$`count(*)`
                              , col = rainbow(5)
                              , ylim = c(0, as.array(treatmentCountQuery$`count(*)`)[1] + as.array(treatmentCountQuery$`count(*)`)[1] / 3)
                              , xlab = "Treatments"
                              , main = "Top 5 most used treatments"
                              , ylab = "Uses"
                              , cex.main = 1.5)
text(x = treatmentCountBarplot, y  = treatmentCountQuery$`count(*)`
       , label = treatmentCountQuery$`count(*)`
       , pos = 3
       , col = "black")
axis(1, at = treatmentCountBarplot
   , labels = treatmentCountQuery$treatment
   , tick=FALSE
   , line=-0.5)
#dev.off()

#STAGES RATES FOR TOP 5 MOST PREDICTED DISEASES

topDiseasesFrame = data.frame(disease = predictions$Disease, stage = predictions$Stage)
topDiseasesQuery = sqldf("select disease, count(*) from topDiseasesFrame group by disease order by count(*) desc limit 5")

for ( disease in topDiseasesQuery$disease ) {
     numberOfEasyStagesQuery = sqldf(paste("select count(*) from topDiseasesFrame where disease = '", disease, "' and stage = 'Easy'", sep = ""))
     numberOfMediumStagesQuery = sqldf(paste("select count(*) from topDiseasesFrame where disease = '", disease, "' and stage = 'Medium'", sep = ""))
     numberOfAdvancedStagesQuery = sqldf(paste("select count(*) from topDiseasesFrame where disease = '", disease, "' and stage = 'Advanced'", sep = ""))
     totalQuery = sqldf(paste("select count(*) from topDiseasesFrame where disease = '", disease, "'", sep = ""))
     #svg(paste("Stages-for-disease-", disease, ".svg", sep = ""))
     par(bg = "bisque2")
     pie(c(as.numeric(numberOfEasyStagesQuery), as.numeric(numberOfMediumStagesQuery), as.numeric(numberOfAdvancedStagesQuery))
       , labels = paste (c(" Easy ", " Medium ", " Advanced "), round(c(as.numeric(numberOfEasyStagesQuery), as.numeric(numberOfMediumStagesQuery), as.numeric(numberOfAdvancedStagesQuery))/sum(c(as.numeric(numberOfEasyStagesQuery), as.numeric(numberOfMediumStagesQuery), as.numeric(numberOfAdvancedStagesQuery))) * 100), "%")
       , main = disease
       , radius = 1
       , col = c("palegreen3", "orange", "red3")
       , cex.main = 1.5)
     title(sub = paste("Patients suffering from ", disease, ": ", as.numeric(totalQuery), "\n\nBased on predictions", sep = ""), cex.sub = 1.3)
     #dev.off()
     }

#AGE RANGES FOR TOP 5 MOST PREDICTED DISEASES

topDiseasesAgeFrame = data.frame(disease = predictions$Disease, age = predictions$Age)
topDiseasesAgeQuery = sqldf("select disease from topDiseasesQuery group by disease order by count(*) desc limit 5")

for ( disease in topDiseasesAgeQuery$disease ) {
     #svg(paste("Age-ranges-for-", disease, ".svg", sep = ""))
     par(bg = "bisque2", mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
     plot(NULL
        , main = disease
        , type = "n"
        , xlab = "Age"
        , ylab = "Frequence"
        , xlim = c(0, 100)
        , x = c(0, as.numeric(sqldf("select count(*) from topDiseasesAgeFrame group by age order by count(*) desc limit 1")) + 1)
        , cex.main = 1.5)
     legend("topright"
          , inset=c(-0.3,0)
          , legend=c("0 - 18", "18 - 36", "36 - 54", "54 - 72", "72 - 90", "90 - 100")
          , pch = c(19, 19, 19, 19, 19, 19)
          , col = c("red", "blue", "#00CC00", "orange", "#9900CC", "black")
          , title="Range")
     between0and18Query = sqldf(paste("select age from topDiseasesAgeFrame where disease = '", disease, "' and age >= 0 and age <= 18", sep = ""))
     if ( ncol(as.data.frame(table(between0and18Query$age))) >= 2 )
          points( as.matrix(as.data.frame(table(between0and18Query$age)))[,1],as.matrix(as.data.frame(table(between0and18Query$age)))[,2], col= "red", pch = 19, cex = 1, lty = "solid", lwd = 1 )
     between18and36Query = sqldf(paste("select age from topDiseasesAgeFrame where disease = '", disease, "' and age > 18 and age <=36", sep = ""))
     if ( ncol(as.data.frame(table(between18and36Query$age))) >= 2 )
          points(as.matrix(as.data.frame(table(between18and36Query$age)))[,1], as.matrix(as.data.frame(table(between18and36Query$age)))[,2], col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 1 )
     between36and54Query = sqldf(paste("select age from topDiseasesAgeFrame where disease = '", disease, "' and age > 36 and age <=54", sep = ""))
     if ( ncol(as.data.frame(table(between36and54Query$age))) >= 2 )
          points(as.matrix(as.data.frame(table(between36and54Query$age)))[,1], as.matrix(as.data.frame(table(between36and54Query$age)))[,2], col= "#00CC00", pch = 19, cex = 1, lty = "solid", lwd = 1 )
     between54and72Query = sqldf(paste("select age from topDiseasesAgeFrame where disease = '", disease, "' and age > 54 and age <=72", sep = ""))
     if ( ncol(as.data.frame(table(between54and72Query$age))) >= 2 )
          points(as.matrix(as.data.frame(table(between54and72Query$age)))[,1], as.matrix(as.data.frame(table(between54and72Query$age)))[,2], col= "orange", pch = 19, cex = 1, lty = "solid", lwd = 1 )
     between72and90Query = sqldf(paste("select age from topDiseasesAgeFrame where disease = '", disease, "' and age > 72 and age <=90", sep = ""))
     if ( ncol(as.data.frame(table(between72and90Query$age))) >= 2 )
          points(as.matrix(as.data.frame(table(between72and90Query$age)))[,1], as.matrix(as.data.frame(table(between72and90Query$age)))[,2], col= "#9900CC", pch = 19, cex = 1, lty = "solid", lwd = 1 )
     between90and100Query = sqldf(paste("select age from topDiseasesAgeFrame where disease = '", disease, "' and age > 90 and age <=100", sep = ""))
     if (ncol(as.data.frame(table(between90and100Query$age))) >= 2 )
          points(as.matrix(as.data.frame(table(between90and100Query$age)))[,1], as.matrix(as.data.frame(table(between90and100Query$age)))[,2], col= "black", pch = 19, cex = 1, lty = "solid", lwd = 1 )
     #dev.off()
     }

#SUCCESS AND FAILURE RATES FOR EVERY STAGE FOR TOP 5 MOST USED TREATMENTS

topTreatmentsStageAndSuccessFrame = data.frame(treatment = predictions$Treatment, stage = predictions$Stage, success = predictions$Success)
topTreatmentsStageAndSuccessQuery = sqldf("select treatment from topTreatmentsStageAndSuccessFrame group by treatment order by count(*) desc limit 5")

for ( treatment in topTreatmentsStageAndSuccessQuery$treatment ) {
  successCountEasyQuery = sqldf(paste("select count(*) from topTreatmentsStageAndSuccessFrame where treatment = '", treatment, "' and stage = 'Easy' and success = 1", sep = "")) 
  failureCountEasyQuery = sqldf(paste("select count(*) from topTreatmentsStageAndSuccessFrame where treatment = '", treatment, "' and stage = 'Easy' and success = 0", sep = ""))
  successCountMediumQuery = sqldf(paste("select count(*) from topTreatmentsStageAndSuccessFrame where treatment = '", treatment, "' and stage = 'Medium' and success = 1", sep = "")) 
  failureCountMediumQuery = sqldf(paste("select count(*) from topTreatmentsStageAndSuccessFrame where treatment = '", treatment, "' and stage = 'Medium' and success = 0", sep = ""))
  successCountAdvancedQuery = sqldf(paste("select count(*) from topTreatmentsStageAndSuccessFrame where treatment = '", treatment, "' and stage = 'Advanced' and success = 1", sep = "")) 
  failureCountAdvancedQuery = sqldf(paste("select count(*) from topTreatmentsStageAndSuccessFrame where treatment = '", treatment, "' and stage = 'Advanced' and success = 0", sep = ""))
  #svg(paste("Stages-and-success-and-failure-rates-for-", treatment, ".svg", sep = ""))
  par(bg = "bisque2", mar = c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  barplot(matrix(c(round(c(as.numeric(successCountEasyQuery), as.numeric(failureCountEasyQuery))/sum(c(as.numeric(successCountEasyQuery), as.numeric(failureCountEasyQuery))) * 100), round(c(as.numeric(successCountMediumQuery), as.numeric(failureCountMediumQuery))/sum(c(as.numeric(successCountMediumQuery), as.numeric(failureCountMediumQuery))) * 100), round(c(as.numeric(successCountAdvancedQuery), as.numeric(failureCountAdvancedQuery))/sum(c(as.numeric(successCountAdvancedQuery), as.numeric(failureCountAdvancedQuery))) * 100)) , nrow = 2)
        , names.arg = c("Easy", "Medium", "Advanced")
        , main = treatment
        , xlab = "Stages"
        , ylab = "Percentage"
        , cex.main = 1.5
        , col = c("palegreen3", "red3"))
  color.legend(4.4, 66, 5, 90
             , rect.col = c("palegreen3", "red3") 
             , legend = c("Success", "Failure")
             , gradient = "y")
  mtext("Colors meaning", at = c(4.5))
  #dev.off();
  }
