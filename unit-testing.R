checkNotEmptyDataFrame = function(dataFrame) {
     
     writeLines(paste("-------------------------------------------------------------------------------------------------------------------------------\nTest: ", testCounter, sep = ""))
     isEmpty = TRUE
     tryCatch(
          expr = {
               test_that("Not empty data frame",
               {
                    expect_gt(nrow(dataFrame), 1)
               })
               writeLines("Success")
               isEmpty = FALSE
               successfulTests <<- successfulTests + 1
          },
          error = function(Error) {
               failedTests <<- failedTests + 1
               writeLines("Failure")
               print(Error)
          }
     )
     writeLines("-------------------------------------------------------------------------------------------------------------------------------\n")
     return(isEmpty)
     
}

checkColumnExistenceForDataFrame = function(dataFrame, column) {
     
     writeLines(paste("-------------------------------------------------------------------------------------------------------------------------------\nTest: ", testCounter, sep = ""))
     hasColumn = FALSE
     tryCatch(
          expr = {
               for ( Column in colnames(dataFrame) )
                    if ( Column == column ) {
                         hasColumn = TRUE
                         break
                    }
               test_that("Data frame has a column",
                         {
                              expect_equal(hasColumn, TRUE)
                         }
                         )
               writeLines("Success")
               successfulTests <<- successfulTests + 1
          },
          error = function(Error) {
               failedTests <<- failedTests + 1
               writeLines("Failure")
               print(Error)
          }
     )
     writeLines("-------------------------------------------------------------------------------------------------------------------------------\n")
     testCounter <<- testCounter + 1
     return(hasColumn)
     
}


checkNumberOfColumnsForDataFrame = function(dataFrame, numberOfColumns) {
     
     writeLines(paste("-------------------------------------------------------------------------------------------------------------------------------\nTest: ", testCounter, sep = ""))
     hasNumberOfColumns = FALSE
     tryCatch(
          expr = {
               test_that("Data frame has a number of columns",
                         {
                              expect_equal(ncol(dataFrame), numberOfColumns)
                         })
               writeLines("Success")
               hasNumberOfColumns = TRUE
               successfulTests <<- successfulTests + 1
          },
          error = function(Error) {
               failedTests <<- failedTests + 1
               writeLines("Failure")
               print(Error)
          }
     )
     writeLines("-------------------------------------------------------------------------------------------------------------------------------\n")
     testCounter <<- testCounter + 1
     return(hasNumberOfColumns)
     
}

checkDataFrame = function(object) {
     
     isDataFrame = FALSE
     writeLines(paste("-------------------------------------------------------------------------------------------------------------------------------\nTest: ", testCounter, sep = ""))
     tryCatch(
          expr = {
               test_that("Not a data frame",
                    {
                         expect_true(is.data.frame(object))
                    })
               writeLines("Success")
               isDataFrame = TRUE
               successfulTests <<- successfulTests + 1
          },
          error = function(Error) {
               failedTests <<- failedTests + 1
               writeLines("Failure")
               print(Error)
          }
     )
     writeLines("-------------------------------------------------------------------------------------------------------------------------------\n")
     testCounter <<- testCounter + 1
     return(isDataFrame)
     
}


checkNull = function(object) {
     
     writeLines(paste("-------------------------------------------------------------------------------------------------------------------------------\nTest: ", testCounter, sep = ""))
     isNull = TRUE
     tryCatch(
          expr = {
               test_that("Null object", 
                         {
                              expect_false(is.null(object))
                         })
               writeLines("Success")
               isNull = FALSE
               successfulTests <<- successfulTests + 1
          },
          error = function(Error) {
               failedTests <<- failedTests + 1
               writeLines("Failure")
               print(Error)
          }
     )
     writeLines("-------------------------------------------------------------------------------------------------------------------------------\n")
     testCounter <<- testCounter + 1
     return(isNull)
     
}

checkIfVectorContainsOnlyTheseValues = function(vector, values) {
     
     writeLines(paste("-------------------------------------------------------------------------------------------------------------------------------\nTest: ", testCounter, sep = ""))
     containsValues = TRUE
     tryCatch(
          expr = {
               for ( value in vector ) 
                    if ( value %in% values == FALSE ) {
                         containsValues = FALSE
                         break
                    }
               test_that("Vector contains only these values",
                         {
                              expect_equal(containsValues, TRUE)
                         })
               writeLines("Success")
               successfulTests <<- successfulTests + 1
          },
          error = function(Error) {
               failedTests <<- failedTests + 1
               writeLines("Failure")
               print(Error)
          }
     )
     writeLines("-------------------------------------------------------------------------------------------------------------------------------\n")
     testCounter <<- testCounter + 1
     return(containsValues)
}

top5MostPredictedDiseases = function (predictionsTable) {
     
     writeLines(paste("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\nFunction: ", functionCounter, sep = ""))
     validData = FALSE
     tryCatch(
          expr = {
               expect_false(checkNull(predictionsTable))
               expect_true(checkDataFrame(predictionsTable))
               expect_true(checkNotEmptyDataFrame(predictionsTable))
               expect_true(checkNumberOfColumnsForDataFrame(predictionsTable, 7))
               expect_true(checkColumnExistenceForDataFrame(predictionsTable, "Disease"))
               writeLines("Function terminated successfully\n")
               validData = TRUE
               successfulTests <<- successfulTests + 1
          },
          error = function(Error) {
               failedTests <<- failedTests + 1
               writeLines("An error occurred while executing the function.")
               print(Error)
          }
     )
     if ( validData ) {
          diseases = c()
          top5MostPredictedDiseasesFrame = data.frame(disease = predictionsTable$Disease)
          top5MostPredictedDiseasesQuery = sqldf("select disease from top5MostPredictedDiseasesFrame group by disease order by count(*) desc limit 5")
          for ( disease in top5MostPredictedDiseasesQuery$disease )
               diseases = c(diseases, disease)
          return(diseases)
          
     }
     writeLines("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
     functionCounter <<- functionCounter + 1
     
}



successAndFailureRatesForTop5Treatments = function (predictionsFrame) {
     
     writeLines(paste("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\nFunction: ", functionCounter, sep = ""))
     validData = FALSE
     tryCatch(
          expr = {
               expect_false(checkNull(predictionsFrame))
               expect_true(checkDataFrame(predictionsFrame))
               expect_true(checkNotEmptyDataFrame(predictionsFrame))
               expect_true(checkNumberOfColumnsForDataFrame(predictionsFrame, 7))
               expect_true(checkColumnExistenceForDataFrame(predictionsFrame, "Treatment"))
               expect_true(checkColumnExistenceForDataFrame(predictionsFrame, "Success"))
               expect_true(checkIfVectorContainsOnlyTheseValues(predictionsFrame$Treatment, c(0, 1)))
               writeLines("Function terminated successfully\n")
               validData = TRUE
               successfulTests <<- successfulTests + 1
          },
          error = function(Error) {
               failedTests <<- failedTests + 1
               writeLines("An error occurred while executing the function.")
               print(Error)
          }
     )
     if ( validData ) {
          top5TreatmentsFrame = data.frame(treatment = predictionsFrame$Treatment, success = predictionsFrame$Success)
          top5TreatmentsQuery = sqldf("select treatment from top5TreatmentsQuery group by treatment order by count(*) desc limit 5")
          # to do
     }
     writeLines("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
     functionCounter <<- functionCounter + 1
     
}

printFinalResults = function() {
     writeLines(paste("Failed tests: ", failedTests, "\nSuccessful tests: ", successfulTests, "\n", sep = ""))
}

library(sqldf)
library(testthat)
predictions = read.csv("predictions.csv", TRUE, ";")

outputFile = "results.txt"
testCounter = 1
functionCounter = 1
failedTests = 0
successfulTests = 0


sink(file = outputFile, append = FALSE, split = FALSE)
top5MostPredictedDiseases(NULL)
top5MostPredictedDiseases(2)
top5MostPredictedDiseases("String")
top5MostPredictedDiseases(as.array(1, 2, 3 ,4))
top5MostPredictedDiseases(as.matrix(1, 2, 3, 4, nrow = 2))
top5MostPredictedDiseases(predictions$Disease)
top5MostPredictedDiseases(as.numeric(12.22222222))
top5MostPredictedDiseases(predictions)
successAndFailureRatesForTop5Treatments(NULL)
successAndFailureRatesForTop5Treatments(123)
successAndFailureRatesForTop5Treatments("Treatment")
successAndFailureRatesForTop5Treatments(c(1, 2, 3, 4, 5, 6))
successAndFailureRatesForTop5Treatments(predictions$Treatment)
successAndFailureRatesForTop5Treatments(as.Date("18-01-1999"))
successAndFailureRatesForTop5Treatments(predictions)
printFinalResults()
sink()
