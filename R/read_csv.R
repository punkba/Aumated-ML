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
  #filename1<-gsub("fakepath","opencpuapp_ip",filename)
	df_full<-utils::read.csv(filename, header = TRUE, stringsAsFactors = FALSE )
  #df_full<-utils::read.csv("c:/opencpuapp_ip/base_data.csv", header = TRUE, stringsAsFactors = FALSE )
  #df_full<-utils::read.csv(paste("c:/opencpuapp_ip/",substr(filename,13,nchar(filename)),sep=""), header = TRUE, stringsAsFactors = FALSE );
  print(getwd())

  list(
	#message = paste("hello ", "c:/opencpuapp_ip/",substr(filename,17,nchar(filename)),".csv",sep="")
	message = paste("Read Successful" )
	)
	df_full
}
