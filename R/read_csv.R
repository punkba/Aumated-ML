#' A read_csv function
#'
#' This function allows you to read a csv filename
#' @param filename has the filenamepath
#' @return success/failure
#' @export

read_csv<-function(filename)
{

#install.packages("EvaluationMeasures")
#install.packages("VIF")
#install.packages("pROC")
#install.packages("SDMTools")
#install.packages("party")
#install.packages("caret")
#install.packages("gbm")
#install.packages("randomForest")
#install.packages("ROSE")
#install.packages("e1071")
#install.packages("car")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("caTools")
#install.packages("glmnet")

  library(EvaluationMeasures)
  library(pROC)
  library(SDMTools)
  library(party)
  library(caret)
  library(randomForest)
  library(ROSE)
  library(e1071)
  library(car)
  library(plyr)
  library(dplyr)
  library(caTools)
  library(glmnet)
  library(VIF)


  library(utils)
  if(!grepl(".csv$", filename)){
  	stop("Uploaded filename must be a .csv filename!");
  }
  df_full<-utils::read.csv(filename, header = TRUE, stringsAsFactors = FALSE )


  list(
	message = paste("Read Successful" )
	)
	df_full
}
