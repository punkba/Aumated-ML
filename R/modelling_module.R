modelling_module<-function(model_selection,predictorClass,dv,prevSessionid)
{
  library(pROC)
  library(caret)
  library(caTools)
  library(ROCR)
  library(dplyr)

  print('inside modelling')

  clearWarnings <- function(){
    assign("last.warning", NULL, envir = baseenv())
  }

  returnUniqueWarnings <- function(){
    wars <- warnings()[!duplicated(warnings())]

    if (length(wars) >= 8)
    {
      wars <- wars[1:8]
    }

    return(as.list(wars))
  }

  processOutput <- function(model,vars,metrics,oemInd){
    library(dplyr)
    library(RJSONIO)
    library(data.table)

    if(oemInd)
    {
      selectedModel <- which.max(metrics$accuracy)

      variables <- vars[selectedModel]

      modResults <- metrics %>% select('tpr','fpr','tnr','fnr','accuracy')
      colnames(modResults) <- NULL
      metricOutput <- list()

      for(each in 1:nrow(modResults))
      {
        output <- list(as.numeric(metrics[each,'tpr']),
                       as.numeric(metrics[each,'fpr']),
                       as.numeric(metrics[each,'tnr']),
                       as.numeric(metrics[each,'fnr']),
                       as.numeric(metrics[each,'accuracy']))

        metricOutput[[each]] <- output
      }

      graph <- graph[selectedModel]
      save(graph,file="graph.RData")
    }
    else
    {
      modelName <- rownames(metrics)
      modelSaveLocation <- paste0(modelName,"_model.RData")


      save(model,file=modelSaveLocation)

      modelName <- list(modelName=I(modelName))
      modelSaveLocation <- list(modelSaveLocation=I(modelSaveLocation))

      metricOutput <- list(as.numeric(metrics['tpr']),
                           as.numeric(metrics['fpr']),
                           as.numeric(metrics['tnr']),
                           as.numeric(metrics['fnr']),
                           as.numeric(metrics['recall']),
                           as.numeric(metrics['precision']),
                           as.numeric(metrics['f1score']),
                           as.numeric(metrics['accuracy']))

      metricOutput <- list(metricOutput=I(metricOutput))
      sumMod <- summary(model)
      modelSummaryLocation <- paste0(modelName,"_summary.txt")
      capture.output(sumMod,file=modelSummaryLocation)
      summaryPath <- list(summaryPath=I(modelSummaryLocation))

      if(modelName == "nb")
     {
       var_imp_flag <- "N"
     } else {
       var_imp_flag <- "Y"
     }

    }
    outL <- list(modelName,
                 modelSaveLocation,
                 metricOutput,summaryPath,var_imp_flag)

    return (outL)
  }

  dataFunction <- function(sessionId){
    ##Splitting into test and train
    set.seed(666)
    #User to choose the ratio to be set for training and testing data sets
    splitratio <- as.numeric(0.7)

    loc <- getServerPath(sessionId,getwd())
    cleanedDataLoc <- paste0(loc,'/cleaned_data.csv')
    cleaned_data <- read.csv(file=cleanedDataLoc)
    write.csv(cleaned_data,'cleaned_data.csv')

    variablesLoc <- paste0(loc,'/variable_list.csv')
    data_type<-read.csv(file=variablesLoc,stringsAsFactors = FALSE)
    write.csv(data_type,'variable_list.csv')

    cat_var<- as.vector(data_type$categorical)
    cat_var <- cat_var[!is.na(cat_var)]

    for (value in cat_var){
      cleaned_data[value]<- as.factor(cleaned_data[[value]])
    }

    names(cleaned_data)[names(cleaned_data)==dv] <- "DV"
    split <- sample.split(cleaned_data$DV,SplitRatio = splitratio)

    train <- subset(cleaned_data,split == TRUE)
    test <- subset(cleaned_data,split == FALSE)

    drops <- c("X")
    train<-train[ , !(names(train) %in% drops)]
    test<-test[ , !(names(test) %in% drops)]
    return(list(train,test))
  }

  setUpFunction<- function(train,test,positive_class,model){
    if(is.numeric(train$DV))
    {
      if(model=="SVM")
      {
        train$DV <- as.factor(train$DV)
        test$DV <- as.factor(test$DV)

        levels(train$DV) <- c('No','Yes')
        levels(test$DV) <- c('No','Yes')
        positive_class <- "Yes"
      }
      else
      {
        if(!max(unique(train$DV)) == 1)
        {
          custlevels <- unique(train$DV)

          if(positive_class == 1)
          {
            train$DV[train$DV == positive_class] <- 1
            train$DV[train$DV != positive_class ] <- 0

            test$DV[test$DV == positive_class] <- 1
            test$DV[test$DV != positive_class ] <- 0
          }
          else
          {
            train$DV[train$DV == min(custlevels)] <- 0
            train$DV[train$DV == max(custlevels)] <- 1

            test$DV[test$DV == min(custlevels)] <- 0
            test$DV[test$DV == max(custlevels)] <- 1

            positive_class <- 1
          }
        }
      }
    }
    else
    {
      uniqLvls <- trimws(as.character(unique(test$DV)))
      negClass <- uniqLvls[uniqLvls != positive_class]

      train$DV <- trimws(as.character(train$DV))
      test$DV <- trimws(as.character(test$DV))

      if(model=='SVM')
      {
        positChangedClass <- make.names(positive_class)
        negChangedClass <- make.names(negClass)

        train$DV[train$DV == positive_class] <- positChangedClass
        train$DV[train$DV == negClass] <- negChangedClass
        train$DV <- as.factor(train$DV)


        test$DV[test$DV == positive_class] <- positChangedClass
        test$DV[test$DV == negClass] <- negChangedClass
        test$DV <- as.factor(test$DV)

        positive_class <- positChangedClass

      }
      else
      {
        train$DV[train$DV == positive_class] <- 1
        train$DV[train$DV == negClass] <- 0

        test$DV[test$DV == positive_class] <- 1
        test$DV[test$DV == negClass] <- 0

        train$DV <- as.numeric(train$DV)
        test$DV <- as.numeric(test$DV)

        positive_class <- 1
      }
    }

    return(list(train,test,positive_class))
  }

  evaluatemeasures <- function(testData,flag){

    pred_f <- testData$Prob
    DV <- testData$DV
    predicted_val <- testData$predicted

    library(EvaluationMeasures)
    library(pROC)
    library(dplyr)
    library(plotly)

    if(!is.numeric(DV))
    {
      predicted_val <- as.character(predicted_val)
      DV <- as.character(DV)

      flagPred <- predicted_val == positive_class
      dvPred <- DV == positive_class

      predicted_val <- as.numeric(flagPred)
      DV <- as.numeric(dvPred)
    }

    tpr<-(EvaluationMeasures.TPR(Real = DV,Predicted = predicted_val, Positive = 1))/2
    fpr<-(EvaluationMeasures.FPR(Real = DV,Predicted = predicted_val, Positive = 1))/2
    tnr<-(EvaluationMeasures.TNR(Real = DV,Predicted = predicted_val, Positive = 1))/2
    fnr<-(EvaluationMeasures.FNR(Real = DV,Predicted = predicted_val, Positive = 1))/2
    recall<-EvaluationMeasures.Recall(Real = DV,Predicted = predicted_val, Positive = 1)
    precision<-EvaluationMeasures.Precision(Real = DV,Predicted = predicted_val, Positive = 1)
    f1score<-EvaluationMeasures.F1Score(Real = DV,Predicted = predicted_val, Positive = 1)
    Accuracy<-EvaluationMeasures.Accuracy(Real = DV,Predicted = predicted_val, Positive = 1)
    res = roc(as.numeric(DV), pred_f)
    if(!flag){
      plot_res <- plot(res)
    }

    prediction_f <- prediction(pred_f, as.numeric(DV))

    perf <- performance(prediction_f,"tpr","rpp")
    if(!flag){
      plot_lc <- plot(perf, main="gain chart")
    }

    testCopy <- testData
    testCopy$DV <- DV
    testCopy$predicted <- predicted_val

    return(c(tpr,fpr,tnr,fnr,recall,precision,f1score,Accuracy))
  }

  k_stat_value<- function(fullmodel,train,pos,model){

    train_KStat <- train
    if(! (model %in% c('SVM','NB')))
    {

      train_KStat$pred <- predict(fullmodel,
                                  newdata = train,
                                  type = 'response')
    }
    else if(model == "NB")
    {
      train_KStat$pred <- predict(fullmodel,
                                  newdata = train,
                                  type = 'raw')[,pos]
    }
    else
    {
      train_KStat$pred <- predict(fullmodel,
                                  newdata = train,
                                  type = 'prob')[,pos]

      levels(train_KStat$DV) <- c(1,0)
    }

    library(SDMTools)
    optimum_threshold = optim.thresh(train_KStat$DV, train_KStat$pred)
    thresh = optimum_threshold$`max.sensitivity+specificity`

    return(thresh)
  }

  variable_importance <- function(var_imp_mod,flag_svm,flag){
    library(party)
    library(caret)

    if(flag_svm == "not_app"){
      return()
    }
    else {


      var_imp_res <-data.frame(var_names = character(),
                               Overall = double())

      mod_imp <- varImp(var_imp_mod,numTrees = 3000)

      if(flag_svm != "y")
      {
        names <- rownames(mod_imp)
        OverallScore <-mod_imp$Overall
      }
      else
      {
        names <- rownames(mod_imp$importance)
        OverallScore <- mod_imp$importance[,positive_class]
      }

      combinedList <- list(var_names=names,Overall=OverallScore)
      var_imp_res <- rbind(var_imp_res,combinedList)
      mod_imp <- arrange(var_imp_res, var_imp_res$Overall)
      mod_imp$var_names <- factor(mod_imp$var_names, levels = mod_imp$var_names)
      p <- ggplot(mod_imp, aes(var_names, Overall)) + geom_col() + coord_flip() + labs(x = "Variables", y = "Importance")
      if(!flag){
        print(p)
      }
      return(p)
    }
  }

  GBM_func <- function(train,test,flagInp,positive_class,flag){

    train_gbm<-train
    test_gbm<-test
    if(flag){
      test_gbm = train_gbm
    }

    print("running GBM")

    library(gbm)
    gbm_model = gbm(DV~.+0,
                    data=train_gbm,
                    shrinkage=0.01,
                    distribution = 'bernoulli',
                    cv.folds=5,
                    n.trees=3000,
                    verbose=F)

    predResult <- predFunction(gbm_model,train_gbm,test_gbm,positive_class,"GBM")

    test_gbm <- predResult

    best.iter = gbm.perf(gbm_model, method="cv")

    evalResults<- evaluatemeasures(test_gbm,flag)

    model_evaluations["gbm",] <- evalResults


    important_variables<- variable_importance(gbm_model,"n",flag)

    model_evaluations <- model_evaluations[rowSums(is.na(model_evaluations)) != ncol(model_evaluations),]

    if(flagInp)
    {
      return (list(as.character(important_variables$var_names),
                   model_evaluations,evalResults[[2]]))
    }
    else
    {
      return (processOutput(gbm_model,
                            important_variables,
                            model_evaluations,
                            flagInp))
    }
  }

  LR_func <- function(train,test,flagInp,positive_class,flag){

    print("running LR")

    train_lr<-train
    test_lr<-test
    if(flag){
      test_lr = train_lr
    }

    lr_model <- glm (DV ~ .,
                     data =train_lr,
                     family = binomial)

    predResult <- predFunction(lr_model,train_lr,test_lr,positive_class,"LR")

    test_lr <- predResult

    evalResults<- evaluatemeasures(test_lr,flag)

    model_evaluations["lr",] <- evalResults

    important_variables <- variable_importance(lr_model,"n",flag)

    model_evaluations <- model_evaluations[rowSums(is.na(model_evaluations)) != ncol(model_evaluations),]

    if(flagInp)
    {
      return (list(as.character(important_variables$var_names),
                   model_evaluations,evalResults[[2]]))
    }
    else
    {
      return (processOutput(lr_model,
                            important_variables,
                            model_evaluations,
                            flagInp))
    }

  }

  RF_func <- function(train,test,flagInp,positive_class,flag){
    print("running RF")
    train_rf <-train
    test_rf <- test
    if(flag){
      test_rf = train_rf
    }
    library(randomForest)
    library(ROSE)

    treeimp <- randomForest(DV ~ .,
                            data = train_rf,
                            ntrees=100,
                            importance=T)
    #Identifying threshold

    predResult <- predFunction(treeimp,train_rf,test_rf,positive_class,"RF")

    test_rf <- predResult

    roc.curve(test_rf$DV, test_rf$Prob, plotit = F)

    evalResults<- evaluatemeasures(test_rf,flag)

    model_evaluations["rf",] <- evalResults
    
    important_variables <- variable_importance(treeimp,"n",flag)

    model_evaluations <- model_evaluations[rowSums(is.na(model_evaluations)) != ncol(model_evaluations),]

    if(flagInp)
    {
      return (list(as.character(important_variables$var_names),
                   model_evaluations,evalResults[[2]]))
    }
    else
    {
      return (processOutput(treeimp,
                            important_variables,
                            model_evaluations,
                            flagInp))
    }
  }

  NB_func<- function(train,test,flagInp,positive_class,flag){

    print("running NB")
    train_nb<-train
    test_nb<-test
    if(flag){
      test_nb = train_nb
    }
    library(e1071)
    Naive_Bayes_Model <- naiveBayes(as.factor(train_nb$DV) ~.,
                                    data=train_nb)

    summary(Naive_Bayes_Model)

    predResult <- predFunction(Naive_Bayes_Model,train_nb,test_nb,positive_class,"NB")

    test_nb <- predResult

    evalResults<- evaluatemeasures(test_nb,flag)

    model_evaluations["nb",] <- evalResults

    important_variables  <- variable_importance(Naive_Bayes_Model,"not_app",flag)

    model_evaluations <- model_evaluations[rowSums(is.na(model_evaluations)) != ncol(model_evaluations),]

    if(flagInp)
    {
      return (list(as.character(important_variables$var_names),
                   model_evaluations,evalResults[[2]]))
    }
    else
    {
      return (processOutput(Naive_Bayes_Model,
                            important_variables,
                            model_evaluations,
                            flagInp))
    }
  }

  SVM_func <- function(test,train,flagInp,positive_class,flag){
    print("running SVM")
    train_svm<- train
    test_svm<- test
    if(flag){
      test_svm = train_svm
    }
    library(caret)

    trctrl <- trainControl(method = "cv",
                           number =5,
                           classProbs = TRUE,
                           savePredictions = 'final')

    set.seed(323)

    library(kernlab)
    ### finding optimal value of a tuning parameter
    sigDist <- sigest(DV ~ ., data = train_svm, frac = 1)

    svm_radial <- train(DV ~.,
                        data = train_svm,
                        method = "svmRadial",
                        trControl = trctrl)

    predResult <- predFunction(svm_radial,train_svm,test_svm,positive_class,"SVM")

    test_svm <- predResult

    evalResults<- evaluatemeasures(test_svm,flag)

    model_evaluations["svm",] <- evalResults

    important_variables  <- variable_importance(svm_radial,"y",flag)

    model_evaluations <- model_evaluations[rowSums(is.na(model_evaluations)) != ncol(model_evaluations),]

    if(flagInp)
    {
      return (list(as.character(important_variables$var_names),
                   model_evaluations,evalResults[[2]]))
    }
    else
    {
      return (processOutput(svm_radial,
                            important_variables,
                            model_evaluations,
                            flagInp))
    }
  }

  OEM_func<-function(train,test,flagInp,positive_class){
    train_oem <- train
    test_oem <- test
    oem_results <- data.frame()
    oem_vars <- list()
    oem_graph <- list()

    flag <- T

    lr_results <- LR_func(train_oem,test_oem,flag,positive_class)
    nb_results <- NB_func(train_oem,test_oem,flag,positive_class)
    rf_results <- RF_func(train_oem,test_oem,flag,positive_class)

    oem_results <- rbind(lr_results[2][[1]],
                         rf_results[2][[1]],
                         nb_results[2][[1]])

    oem_vars <- list(list(lr_results[[1]]),
                     list(rf_results[[1]]),
                     list(nb_results[[1]]))
    oem_graph <- list(lr_results[[3]],
                      rf_results[[3]],
                      nb_results[[3]])

    output<- processOutput(oem_vars,oem_results,oem_graph,flag)

    return (output)
  }

  predFunction <- function(modelInput,trainD,testD,posit_class,model){
    type <-""
    negClass <- ""
    if (model == "SVM")
    {
      typeResp <- 'prob'
    }
    else if(model == "NB"){
      typeResp <- 'raw'
    }
    else
    {
      typeResp <- 'response'
    }
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
    if(posit_class==1)
    {
      negClass <- 0
    }
    else
    {
      uniqLvls <- as.character(unique(testD$DV))
      negClass <- uniqLvls[uniqLvls != posit_class]
    }

    threshold<-k_stat_value(modelInput,trainD,posit_class,model)

    threshold_df <- data.frame("ModelName" = model_selection, "PredictorClass" = predictorClass, "DVName" = dv, "Threshold" = threshold)
    write.csv(threshold_df,"threshold.csv")

    if(! (model %in% c('SVM','NB')))
    {

      pred <- predict(modelInput,
                      newdata=testD,
                      type = typeResp)
    } else {
      pred <- predict(modelInput,
                      newdata=testD,
                      type = typeResp)[,posit_class]
    }

    testD$Prob <- pred

    testD$predicted[pred>max(threshold)] <- posit_class
    testD$predicted[pred<=max(threshold)] <- negClass

    return(testD)
  }

  data_model <- dataFunction(prevSessionid)

  train <- data_model[[1]]
  test <- data_model[[2]]

  model_evaluations<-setNames(data.frame(matrix(ncol = 8, nrow = 9)),
                              c("tpr","fpr","tnr","fnr","recall",
                                "precision","f1score","accuracy")
  )

  ##The class that needs to be predicted when the prob > threshold
  positive_class <- predictorClass
  model <- model_selection

  oemFlag <- F

  dataUpdated <- setUpFunction(train,test,positive_class,model)
  train <- dataUpdated[[1]]
  test <- dataUpdated[[2]]
  positive_class <- dataUpdated[[3]]

  rm(dataUpdated)

  fn <- get(paste(model,'func',sep='_'))
  vars_imp <- fn(train,test,oemFlag,positive_class,F)

  vars_imp[[3]][[1]]<-list(tpr = vars_imp[[3]][[1]][1], fpr = vars_imp[[3]][[1]][2],
                           tnr = vars_imp[[3]][[1]][3], fnr = vars_imp[[3]][[1]][4],
                           recall = vars_imp[[3]][[1]][5], precision = vars_imp[[3]][[1]][6],
                           f1score = vars_imp[[3]][[1]][7], accuracy = vars_imp[[3]][[1]][8])
  write.table(vars_imp[[3]][[1]], "ModelLogFile.csv", sep = ",", col.names = T, append = T, row.names = F)
  vars_imp_train <- fn(train,test,oemFlag,positive_class,T)

  vars_imp_train[[3]][[1]]<-list(tpr = vars_imp_train[[3]][[1]][1], fpr = vars_imp_train[[3]][[1]][2],
                           tnr = vars_imp_train[[3]][[1]][3], fnr = vars_imp_train[[3]][[1]][4],
                           recall = vars_imp_train[[3]][[1]][5], precision = vars_imp_train[[3]][[1]][6],
                           f1score = vars_imp_train[[3]][[1]][7], accuracy = vars_imp_train[[3]][[1]][8])
  write.table(vars_imp_train[[3]][[1]], "ModelLogFile.csv", sep = ",", col.names = T, append = T, row.names = F)
  benchmarking_modelling_module(model_selection,predictorClass,dv,prevSessionid)

  vars_imp_list<-list(vars_imp,vars_imp_train)
  return (vars_imp_list)
}
