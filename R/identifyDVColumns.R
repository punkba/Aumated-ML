identifyDVColumns <- function(inputFile){

  library(utils)
  if(!grepl(".csv$", inputFile)){
    stop("Uploaded filename must be a .csv filename!");
  }

  file<-utils::read.csv(inputFile, header = TRUE, stringsAsFactors = FALSE )

  #get the column names
  uniqueValuesByColumn <- lapply(file,function(x) length(unique(x)))

  #filter column names only with 2 levels
  uniqueValuesByColumn <- uniqueValuesByColumn[uniqueValuesByColumn == 2]

  if(uniqueValuesByColumn == 0){
    stop('File does not contain a dependent variable columns with 2 categories')
  }
  else{
    return(names(uniqueValuesByColumn))
  }
}
