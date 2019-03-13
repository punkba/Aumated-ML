
scoringmodule <- function(filename,modelSel,prevSessionid) {

  library(VIF)
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
  library(plotly)
  library(woeBinning)
  library(tidyverse)

  loc <- getServerPath(prevSessionid,getwd())
  cleanedDataLoc <- paste0(loc,'/cleaned_data.csv')
  data<-read.csv(file=cleanedDataLoc,stringsAsFactors = FALSE)


  if(!grepl(".csv$", filename)){
    stop("Uploaded filename must be a .csv filename!");
  }

  df_full_bin<-utils::read.csv(filename, header = TRUE, stringsAsFactors = FALSE )

  list(message = paste("Read Successful" ))

  test<-data.frame(df_full_bin)

  thresholdLoc <- paste0(loc,'/threshold.csv')
  data_pts<-read.csv(file=thresholdLoc,stringsAsFactors = FALSE)
  target.var.name <- data_pts$DVName
  typeof(target.var.name)

  preprocessing <- function(df_data,dv){
    data <- df_data
    #add string for data summary
    Summary_df <- data.frame(unclass(summary(data)), check.names = FALSE, stringsAsFactors = FALSE)

    #consider target variable name given as input in HybridFS function as DV'
    n <- dv
    if(length(unique(data[[n]]))!=2)
    {
      stop("Error: Dependent variables should be 2 for binary classification!")
    }

    names(data)[names(data)==n] <- "DV"
    df_temp<-data

    #get the list of categorical variables
    cat_var=data.frame()

    df_temp<-df_temp[, names(df_temp) != "DV"]

    #get the list of character variables
    char <- df_temp[sapply(df_temp,is.character)]
    cat_var<-char

    #get the list of logical variables
    logcl <- df_temp[sapply(df_temp, is.logical)]
    cat_var<-cbind(cat_var,logcl)

    if(ncol(cat_var) == 0)
    {
      fact <- df_temp[sapply(df_temp, is.factor)]
      cat_var<-cbind(cat_var,fact)
    }

    #get the list of Factors
    #fact <- df_temp[sapply(df_temp, is.factor)]
    #cat_var<-cbind(cat_var,fact)

    #removing the categorical variables in df_temp
    df_temp<-df_temp[, !sapply(df_temp,is.logical)]
    df_temp<-df_temp[, !sapply(df_temp,is.character)]

    #determining other categorical variables with less than 52 levels
    unique_lvl_cnt<-df_temp[lengths(lapply(df_temp, unique)) <= 52]
    disc_var_names<-list()
    disc_var_names<-names(unique_lvl_cnt)
    discrete <- list(discrete=I(disc_var_names))
    cat_var<-cbind(cat_var,unique_lvl_cnt)

    cat_var_names<-list()
    cat_var_names<-names(cat_var)

    #display the list of categorical variables
    #cat_var_names
    categorical <- list(categorical=I(cat_var_names))

    df_cont <- data[, names(data) != "DV"]
    for(i in names(cat_var))
    {
      df_cont<-df_cont[names(df_cont) != i]
    }
    cont_var_names<-list()
    cont_var_names<-names(df_cont)

    #display the list of continuous variables
    #cont_var_names
    continuous <- list(continuous=I(cont_var_names))

    #store the variables as list of lists
    final_list <- list(discrete,categorical,continuous)

    #add string for variable list
    lapply(final_list, function(x) write.table( data.frame(x), 'LogFile.csv'  , append= T, sep=',' ))

    ######################################################################################################################################################
    #Categorical variables treatment
    ######################################################################################################################################################
    #Steps
    #Replace missing values as "Unknown" in categorical variables
    #Check for categorical variables with more than 52 levels and reduce them to the top 10 levels that occur frequently

    #convert categorical variables to factors and unique variables treatment
    for(j in cat_var_names)
    {
      print(j)
      data[,j]<-as.factor(data[,j])
      if(length(unique(data[[j]])) >= 0.9*nrow(data))
      {
        data<-data[, names(data) != j]
      }
    }
    #add string for cat var treatment
    cat_var_df <- data.frame(unclass(summary(data)), check.names = FALSE, stringsAsFactors = FALSE)
    write.table(cat_var_df, "LogFile.csv", sep = ",", col.names = T, append = T)

    #Identify replace the missing values as Unknown in categorical variables
    df_cat = data[sapply(data, is.factor) & colnames(data) != "DV"]
    #View(df_cat)
    #df_cat <- data[,sapply(data,is.factor)]
    #df_cat <- df_cat[, names(df_cat) != "DV"]
    if(ncol(df_cat)>0)
    {
      for (i in 1:ncol(df_cat))
      {
        levels <- levels(df_cat[,i])

        if('Unknown' %in% levels)
        {}else{
          levels[length(levels) + 1] <- 'Unknown'
          df_cat[,i] <- factor(df_cat[,i], levels = levels)
        }

        # refactor to include "Unknown" as a factor level
        # and replace NA, null, blanks and ? with "Unknown"
        df_cat[,i] <- factor(df_cat[,i], levels = levels)
        df_cat[,i][is.na(df_cat[,i])] <- "Unknown"
        df_cat[,i][is.null(df_cat[,i])] <- "Unknown"
        df_cat[,i] <- sub("[?]", "Unknown", df_cat[,i])
        df_cat[,i] <- sub("^$", "Unknown", df_cat[,i])
        df_cat[,i]<-as.factor(df_cat[,i])
      }

      #add string for missing value treatment
      missing_df <- data.frame(unclass(summary(df_cat)), check.names = FALSE, stringsAsFactors = FALSE)
      write.table(missing_df, "LogFile.csv", sep = ",", col.names = T, append = T)

      #Check for categorical variables with more than 52 levels and reduce them to the top 10 levels that
      #occur frequently
      for(i in names(df_cat))
      {
        column<-df_cat[,i]
        uniq_lvls_cnt <- length(unique(column))
        temp<-as.data.frame(column)
        if (uniq_lvls_cnt>52)
        { temp<-data.frame()
        cat_freq_cnt <- data.frame(table(column))
        cat_freq_cnt <- cat_freq_cnt[ which(cat_freq_cnt$column!='Unknown' ),]
        cat_sort <- cat_freq_cnt[order(-cat_freq_cnt$Freq),]
        top_1<-head(cat_sort,10)
        top<-as.character(top_1[,1])

        data_cnt<-length(column)


        levels = unique(column)
        levels=as.character(levels)
        temp <- factor(temp, levels = levels)

        if('Unknown' %in% levels)
        {}else{
          levels[length(levels) + 1] <- 'Unknown'
          temp <- factor(temp, levels = levels)
        }

        for(k in 1:data_cnt)
        {
          value<-column[k]
          if(value %in% top)
          {
            temp<-rbind(temp,as.character(value))
          }else
          {
            temp<-rbind(temp,'Unknown')
          }
        }}
        df_cat[,i]<-temp
      }

      #add string to show reduced categorical values > 52
      reduce_cat_df <- data.frame(unclass(summary(df_cat)), check.names = FALSE, stringsAsFactors = FALSE)
      write.table(reduce_cat_df, "LogFile.csv", sep = ",", col.names = T, append = T)

      #**********dv leakage code begin*************
      df_factor_check<-data.frame()
      dvleak_data<-df_cat
      dvleak_data$DV <- ifelse(data$DV==unique(data$DV)[1], 0, 1)
      for (fac in colnames(dvleak_data))
      {
        len<-1
        if (class(dvleak_data[,fac])=="factor")
        {
          df_factor<-dvleak_data[,c("DV",fac)]
          num_dv<-aggregate(DV~.,df_factor,sum)
          num_levels <- aggregate(DV~.,df_factor,length)
          df_factor_final<-merge(num_levels,num_dv,by=fac)

          while(len<=nrow(num_dv))
          {
            output_matrix <- as.data.frame(matrix(data=c(
              fac,
              as.character(df_factor_final[len,1]),
              df_factor_final[len,2],
              df_factor_final[len,3]
            ),nrow=1,ncol=4))
            df_factor_check <- rbind(df_factor_check, output_matrix)
            len = len + 1
          }
        }
      }

      sum_dv<-sum(df_factor$DV==1)
      df_factor_check<-cbind(df_factor_check,sum_dv)
      colnames(df_factor_check)[1]<-"Variable"
      colnames(df_factor_check)[2]<-"Factor_levels"
      colnames(df_factor_check)[3]<-"Records_each_level"
      colnames(df_factor_check)[4]<-"Num_Records_DV=1"
      colnames(df_factor_check)[5]<-"Sum_of_DV"

      df_factor_check$`Num_Records_DV=1`<-as.numeric(as.character(df_factor_check$`Num_Records_DV=1`))

      for ( i in 1:nrow(df_factor_check))
      {
        df_factor_check$dv_leak[i]<-(df_factor_check$Sum_of_DV[i])-(df_factor_check$`Num_Records_DV=1`[i])
      }

      # subset for dv_leak =0 and delete the variables in the original data set.
      #df_factor_check$`Num_Employees_DV=1`<-as.numeric(as.character(df_factor_check$`Num_Employees_DV=1`))

      rem_names=as.character(df_factor_check[df_factor_check$dv_leak==0,]$Variable) #Response same for 90% of the employees
      if(length(rem_names)!=0)
      {
        for ( i in 1:length(rem_names))
        {
          df_cat[,rem_names[i]]<-NULL
        }
      }
    }
    #******DV leakage code ends**************

    write.csv(df_cat, file = "final_out.csv")
    ######################################################################################################################################################
    #continuous variables treatment
    ######################################################################################################################################################

    #Steps
    #first get the correlation matrix
    #get the variable pairs that highly correlated
    #bin the variables using woe binning
    #get the significance of these binned variables using chi square test.
    #Remove variables from highly correlated variable list that are not significant
    #check if muliticollinearity still exists and keep removing variables until vif drops below 5 for all variables
    #get the binned version of the continuous variables

    df<-data

    #Removing categorical variables from data
    for(i in cat_var_names)
    {
      df<-df[names(df) != i]
    }
    #unique variables treatment if ID is not categorical
    for(i in names(df))
    {
      if(grepl("id", i) | grepl("ID", i))
      {
        if(length(unique(df[[i]])) >= 0.9*nrow(df))
        {
          df<-df[names(df) != i]
        }
      }
    }

    df1<-df%>%data.frame
    #creating correlation matrix for continuous variables
    if(length(df1)>1)
    {
      df1<-df1[complete.cases(df1),]
      ##New Change - Sai - DV should not be sent for correlation check
      corr<-round(cor(df1[,names(df1) != 'DV']),2)
      corr_val<-corr

      corr_val[lower.tri(corr_val,diag=TRUE)]=NA # make all the diagonal elements as NA
      corr_val<-as.data.frame(as.table(corr_val)) # as a dataframe
      corr_val<-na.omit(corr_val) # remove NA
      corr_val<-corr_val[with(corr_val, order(-Freq)), ] # order by correlation
      corr_test_var<-subset(corr_val,Freq>=0.85)

      #add string to show continuous var treatment
      reduce_cat_df <- data.frame(unclass(summary(df_cat)), check.names = FALSE, stringsAsFactors = FALSE)
      #write.table(reduce_cat_df, "LogFile.csv", sep = ",", col.names = T, append = T)

      #adding ".binned" to each variable
      ##New Change - Sai - Check if the df is not empty before operation
      if(nrow(corr_test_var) > 0)
      {
        corr_test_var$Var1<-paste(corr_test_var$Var1,"binned",sep = ".")
        corr_test_var$Var2<-paste(corr_test_var$Var2,"binned",sep = ".")

      }

      #woe binning
      var_del<-as.character(names(df))

      binning <- woeBinning::woe.binning(df, 'DV', df)
      tabulate.binning <- woeBinning::woe.binning.table(binning)

      data_cont_binned <- woeBinning::woe.binning.deploy(data, binning)
      #names(data_cont_binned)

      #add string to show binned variables
      bin_df <- data.frame(unclass(summary(data_cont_binned)), check.names = FALSE, stringsAsFactors = FALSE)
      #write.table(bin_df, "LogFile.csv", sep = ",", col.names = T, append = T)

      #removing original values of variables that have been binned
      for(i in var_del)
      {
        data_cont_binned<-data_cont_binned[, names(data_cont_binned) != i]
      }

      data_cont_binned[is.na(data_cont_binned)] <- "Missing"

      data_cont_binned$DV<-data$DV

      #getting the correlated variables as a unique list
      ##New Change - Sai - Check if there are any correlated variables before
      ##applying treatments
      if(nrow(corr_test_var) > 0)
      {
        corr_var<-list()
        corr_var<-corr_test_var$Var1
        corr_var<-c(corr_var,corr_test_var$Var2)
        corr_var_unique<-unique(corr_var)


        #getting the chi sq for each highly correlated variable
        corr_var_chsq <- data.frame()

        for(each in corr_var_unique)
        {

          p_val=chisq.test((data_cont_binned[[each]]),data_cont_binned$DV)$p.value
          c <- data.frame('Var_name' = each,'p_value' = p_val)
          corr_var_chsq <- rbind(corr_var_chsq,c)
        }

        #add string to show Chi sq test
        chisq_df <- data.frame(unclass(summary(corr_var_chsq)), check.names = FALSE, stringsAsFactors = FALSE)
        #write.table(chisq_df, "LogFile.csv", sep = ",", col.names = T, append = T)

        #remove the highly correlated variables that are not significant
        corr_var_insig<-as.character(corr_var_chsq[which(corr_var_chsq$p_value>0.05),1])

        #stripping off the "'binned" from variable names
        corr_var_insig_strip<-substr(corr_var_insig, 1, nchar(corr_var_insig)-7)

        #removing the insignificant variables
        for (f in corr_var_insig) {
          df1[[f]] <- NULL
        }
      }
      df1$DV <- ifelse(df1$DV==unique(df1$DV)[1], 0, 1)
      #checking if we still have multi collinearity and removing variables with very high vif until no such variable exists
      # Fit a model to the data
      fit=glm(DV ~ ., data=df1,family=binomial)
      ##New Change - Nithya - Check if there are linearly dependent variables in the model and remove it
      df_alias <- attributes(alias(fit)$Complete)$dimnames[[1]]
      if(!is.null(df_alias))
      {
        for(i in df_alias)
        {
          df1<-df1[, names(df1) != i]
        }
        # Fit a model to the data
        fit=glm(DV ~ ., data=df1,family=binomial)
      }
      # Calculating VIF for each independent variable
      car::vif(fit)

      # Set a VIF threshold. All the variables having higher VIF than threshold
      #are dropped from the model
      threshold=5

      # Sequentially drop the variable with the largest VIF until
      # all variables have VIF less than threshold
      drop=TRUE

      aftervif=data.frame()
      while(drop==TRUE) {
        vfit=car::vif(fit)
        aftervif=rbind.fill(aftervif,as.data.frame(t(vfit)))
        if(max(vfit)>threshold) { fit=
          update(fit,as.formula(paste(".","~",".","-",names(which.max(vfit))))) }
        else { drop=FALSE }}

      # How variables were removed sequentially
      t_aftervif= as.data.frame(t(aftervif))

      # Final (uncorrelated) variables and their VIFs
      vfit_d= as.data.frame(vfit)

      #add string to show VIF
      vif_df <- data.frame(unclass(summary(vfit_d)), check.names = FALSE, stringsAsFactors = FALSE)
      #write.table(vif_df, "LogFile.csv", sep = ",", col.names = T, append = T)

      rem_var<-as.character(rownames(vfit_d))

      #retaining only the uncorrelated variables in the final data
      df<-df[ , rem_var]

      #getting the concatenated version of variables that needs to be retained
      rem_var<-paste(rem_var,"binned",sep=".")

      #getting the binned continuous variables
      data_cont_binned_fin<-data_cont_binned[,rem_var]
      df_final <- df_cat
      df_cat <- cbind(df_cat, data_cont_binned_fin)

    }
    ######################################################################################################################################################
    #getting the final data frame with the required variables
    ######################################################################################################################################################
    df_final <- cbind(df_final, df)
    write.csv(df_final, file = "clean.csv")
    ##New Change - Sai - DV should be added since the df_cat
    ## is used in building final dataframe
    final_data_after_processing=data.frame()

    final_data_after_processing<-df_final
    final_data_after_processing<-cbind(final_data_after_processing,select(data,.data$DV))

    #add string to show summary of final pre-processed data
    #final_df <- data.frame(unclass(summary(final_data_after_processing)), check.names = FALSE, stringsAsFactors = FALSE)
    #write.table(final_df, "LogFile.csv", sep = ",", col.names = T, append = T)

    return(final_data_after_processing)
  }

  test <- preprocessing(test,target.var.name)
  names(data)[names(data)==target.var.name] <- "DV"
  names(test)[names(test)==target.var.name] <- "DV"


  #############################################

  #data_nDV<-data.frame(filename)



  library(dplyr)

  data_names<-names(data %>% select(contains("binned")))
  test_names<-sub(".binned","",data_names)
  names<-list()
  for (value in data_names){
    names[[value]]<-unique(data[value])
  }

  # for(i in 1:2){
  #   for(j in 1:length(unlist(names[[i]]))){
  #     print(unlist(names[[i]])[j])
  #   }
  # }
  #
  hold_data <- list()


  for( i in 1:length(names)){
    data_final <- c()
    tt <- names[[i]]
    colnames(tt) <- NULL
    for(z in 1:nrow(tt)){
      m <- tt[z,]
      l <- unlist( strsplit(as.character(m),","))
      lower <- NA
      upper <- NA
      for(z_z in 1:length(l)){
        o <- l[z_z]
        x <- gsub("\\(|\\[|\\)|\\]","",o)
        holdit <- gsub("^\\s+|\\s+$", "", x)
        hold_me <- holdit
        if( z_z == 1){
          lower <- hold_me
        }else{
          upper <- hold_me
        }
      }

      #temp <- data.frame(lower,upper)
      temp <- c(lower,upper)
      data_final <- rbind(data_final,temp)
    }

    data_final <- as.matrix(data_final)
    colnames(data_final) <- c("Lower","Upper")
    rownames(data_final) <- NULL
    hold_data[[i]] <- data_final
  }
  names(hold_data) <- names(names)

  for (i in data_names){
    test[,i]<-NA

  }

  for (row in 1:nrow(test))  {
    for (i in 1:length(data_names)){
      nam<-data_names[i]
      y=length(hold_data[[i]])/2
      for (x in 1:y){
        if(is.na(as.integer(hold_data[[i]][x,]["Lower"]))){
          if(test[test_names[i]][row,]<=as.numeric(hold_data[[i]][x,]["Upper"])){
            test[row,][nam]<-(paste("(",hold_data[[i]][x,]["Lower"],",",hold_data[[i]][x,]["Upper"],"]", sep=""))
          }
        }
        else if(is.na(as.integer(hold_data[[i]][x,]["Upper"]))){
          if(test[test_names[i]][row,]>as.numeric(hold_data[[i]][x,]["Lower"])){
            test[row,][nam]<-paste("(",hold_data[[i]][x,]["Lower"],", ",hold_data[[i]][x,]["Upper"],"]", sep="")
          }
        }
        else{
          if(test[test_names[i]][row,]>as.numeric(hold_data[[i]][x,]["Lower"]) && test[test_names[i]][row,]<=as.numeric(hold_data[[i]][x,]["Upper"])){
            test[row,][nam]<-paste("(",hold_data[[i]][x,]["Lower"],",",hold_data[[i]][x,]["Upper"],"]", sep="")
          }
        }
      }

    }}

  test$DV <- NULL
  test <- test[, !colnames(test) %in% test_names]

  write.csv(test,'test_binned.csv')

  testD<-test


  ###To Convert into categorical an variables
  variablesLoc <- paste0(loc,'/variable_list.csv')
  data_type<-read.csv(file=variablesLoc,stringsAsFactors = FALSE)

  cat_var<- as.vector(data_type$categorical)
  cat_var <- cat_var[!is.na(cat_var)]
  num_var<- as.vector(data_type$continuous)
  num_var <- num_var[!is.na(num_var)]

  for (value in cat_var){
    #print(typeof(testD[value]))
    testD[value]<- as.factor(testD[[value]])
  }

  for (value in num_var){
    testD[value]<- as.numeric(testD[[value]])
  }


  #
  model<-data_pts$ModelName
  if (model == "SVM")  {
    typeResp <- 'prob'
  }  else if(model == "NB"){
    typeResp <- 'raw'
  }  else  {
    typeResp <- 'response'
  }

  posit_class<-data_pts$PredictorClass
  if(is.null(posit_class))
  {
    if(is.numeric(testD$DV))
    {
      posit_class <- 1
    }
    else if(is.factor(testD$DV))
    {
      dvList <- tolower(unique(testD$DV))
      if("yes" %in% dvList)
      {
        posit_class <- "yes"
      }
      else
      {
        posit_class <- names(which.max(table(testD$DV)))
      }
    }
    positive_class <- posit_class
  }
  if(posit_class==1)  {
    negClass <- 0
  }  else  {
    uniqLvls <- as.character(unique(testD$DV))
    negClass <- uniqLvls[uniqLvls != posit_class]
  }

  #threshold<-k_stat_value(modelInput,trainD,testD,posit_class,model)
  threshold<-data_pts$Threshold

  modelName <- paste(tolower(modelSel),'_model.RData',sep="")
  finalmodelPath <- paste0(loc,paste0('/',modelName))
  #the_model<-load(paste(modelPath,modelName,sep=""))
  the_model <- load(finalmodelPath)

  sum_model <- get(the_model)
  if(! (model %in% c('SVM','NB')))  {
    pred <- predict(sum_model, newdata=testD, type=typeResp,se.fit=FALSE)

  } else {
    pred <- predict(sum_model, newdata=testD, type=typeResp,se.fit=FALSE)[,posit_class]
  }

  testD$Prob <- pred

  testD$Predicted[pred>max(threshold)] <- posit_class
  testD$Predicted[pred<=max(threshold)] <- negClass



  names(testD)[names(testD)=="Predicted"] <- target.var.name

  write.csv(testD,'scoredData.csv')
}
