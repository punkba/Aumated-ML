imp_var_list<- function(target.var.name,prevSessionid){

  library(rJava)

  location <- getServerPath(prevSessionid,getwd())
  cleanPath <- paste0(location,'/cleaned_data.csv')

  data<- read.csv(file=cleanPath)
  drops <- c("X")
  data<-data[ , !(names(data) %in% drops)]

  names(data)[names(data)==target.var.name] <- "DV"
  options(java.parameters = "-Xmx1g")
  options(java.home="C:\\Program Files\\Java\\jre1.8.0_201")
  Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_201")
  data$DV<- as.integer(data$DV)
  options(warn=-1)

  # Requires Java version of 8 or greater


  ###########################
  ###### Data Binning #######
  ###########################

  dataBinning <- function(data){


    #subset all integer variables in dataset
    allIntVarDF <- data[,sapply(data,is.integer)]

    if(class(allIntVarDF) == "data.frame")
    {
      #Int variables with levels less than 12
      intVarsLen <- apply(allIntVarDF,2,function(i) length(unique(i))<=12)
      intvar<-names(intVarsLen)

      #Int variables with more than 12 levels
      intbin_var <- allIntVarDF[,names(intVarsLen[intVarsLen==FALSE])]
      intbin_var2<- names(intbin_var)
    }

    if(class(allIntVarDF) != "data.frame")
    {
      intbin_var2 <- vector('character')
      intvar <- vector('numeric')
    }



    numvars <- names(data[,sapply(data,is.numeric)])
    numbin_var<-setdiff(numvars,intvar)


	if(length(numbin_var) > 0 && length(intbin_var2) > 0)
    {
      #Supervised Binning of variables based of woe
      binning <- woeBinning::woe.binning(data, 'DV', c(numbin_var,intbin_var2))

	  tabulate.binning <- woeBinning::woe.binning.table(binning)

	  #Adding binned variables to dataset
	  data_binned <- woeBinning::woe.binning.deploy(data, binning)

      return(data_binned)
    }

    return(data)
  }

  ######################################
  ###### Univariate filter Tests #######
  ######################################
  univFiltRes <- function (data_binned){



    allCatVarDF <- data_binned[,sapply(data_binned,is.factor),drop=FALSE]
    allIntVarDF <- data_binned[,sapply(data_binned,is.integer),drop=FALSE]
    allChrVarDF <- data_binned[,sapply(data_binned,is.character),drop=FALSE]

    # Remove dependent variable and get all integer variables which have less than 12 unique values,
    # as these can converted to factor directly
    allIntVarDF$DV <- NULL
    intVarsLen <- apply(allIntVarDF,2,function(i) length(unique(i))<=12)
    allIntVarDF <- allIntVarDF[,names(intVarsLen[intVarsLen==TRUE])]

    #chrVarsLen <- apply(allChrVarDF,2,function(i) length(unique(i))<=12)
    #allChrVarDF <- allChrVarDF[,names(chrVarsLen[chrVarsLen==TRUE])]


    allCatVarFinalDF <- data_binned[,c("DV",names(allIntVarDF),names(allCatVarDF),names(allChrVarDF))]
    allCatVarFinalDF[,names(allIntVarDF)] <- lapply(allCatVarFinalDF[,names(allIntVarDF)],factor)
    allCatVarFinalDF[,names(allChrVarDF)] <- lapply(allCatVarFinalDF[,names(allChrVarDF)],factor)
    allCatVarFinalDF$ID.binned <- NULL

    # Remove factor variables with more than 50 levels
    catVarsLen <- apply(allCatVarFinalDF,2,function(i) length(unique(i))>50)
    allCatVarFinalDF <- allCatVarFinalDF[,names(catVarsLen[catVarsLen==FALSE])]

    # Chi-sq Test
    chisqallpvalues <- apply(allCatVarFinalDF[-1] , 2 , function(i) stats::chisq.test(table(allCatVarFinalDF$DV , i ))$p.value)
    chisqallstatvals <- apply(allCatVarFinalDF[-1] , 2 , function(i) stats::chisq.test(table(allCatVarFinalDF$DV , i ))$statistic)
    chisq <- data.frame(VARS=names(chisqallpvalues),pval=chisqallpvalues,chistat=chisqallstatvals,stringsAsFactors = F)
    row.names(chisq) <- NULL

    # Remove variables which have p-value of 0 from the chi-square test results
    #removeVars <- chisq[which(chisq$pval == 0),"VARS"]
    #allCatVarFinalDF <- allCatVarFinalDF[,!(names(allCatVarFinalDF) %in% removeVars)]
    #chisq <- chisq[-which(chisq$VARS == removeVars),]

    chisq <- chisq[order(-chisq$chistat),]
    chisq$ChistatRank <- order(-chisq$chistat)

    # Entropy Related Tests
    infGainAllVarsTest <- FSelector::information.gain(DV~.,allCatVarFinalDF)
    gainRatioAllVarsTest <- FSelector::gain.ratio(DV~.,allCatVarFinalDF)
    symUncAllVarsTest <- FSelector::symmetrical.uncertainty(DV~.,allCatVarFinalDF)

    # Prepare the data frame and add rank according to the importance values from each test
    entropy <- data.frame(VARS = row.names(infGainAllVarsTest),InfGain = infGainAllVarsTest$attr_importance,GainRatio = gainRatioAllVarsTest$attr_importance,SymUnc = symUncAllVarsTest$attr_importance,stringsAsFactors = F)
    entropy <- entropy[order(-entropy$InfGain),]
    entropy$InfGainRank <- order(-entropy$InfGain)
    entropy <- entropy[order(-entropy$GainRatio),]
    entropy$GainRatioRank <- order(-entropy$GainRatio)
    entropy <- entropy[order(-entropy$SymUnc),]
    entropy$SymUncRank <- order(-entropy$SymUnc)

    # Information Value Test
    factor_vars <-  names(allCatVarFinalDF[-1])
    all_iv <- data.frame(VARS=factor_vars, IV=numeric(length(factor_vars)), STRENGTH=character(length(factor_vars)), stringsAsFactors = F)  # init output dataframe
    for (factor_var in factor_vars)
    {
      all_iv[all_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=allCatVarFinalDF[, factor_var], Y=allCatVarFinalDF$DV)
      all_iv[all_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=allCatVarFinalDF[, factor_var], Y=allCatVarFinalDF$DV), "howgood")
    }
    all_iv <- all_iv[order(-all_iv$IV),]
    all_iv$IVRank <- order(-all_iv$IV)

    # Putting all the results in a single dataframe
    allFiltRes <- merge(chisq,entropy,by="VARS")
    allFiltRes <- merge(allFiltRes,all_iv,by="VARS")

    # Create flags to check if the variable passed each of the five tests
    allFiltRes1 <- allFiltRes
    topN <- sum(allFiltRes1$IV >= 0.03)
    allFiltRes1$ChisqFLAG=ifelse(allFiltRes1$ChistatRank<=topN & allFiltRes1$pval<=0.05,1,0)
    allFiltRes1$IVFLAG=ifelse(allFiltRes1$ChistatRank<=topN,1,0)
    allFiltRes1$InfGainFLAG=ifelse(allFiltRes1$InfGainRank<=topN,1,0)
    allFiltRes1$GainRatioFLAG=ifelse(allFiltRes1$GainRatioRank<=topN,1,0)
    allFiltRes1$SymUncFLAG=ifelse(allFiltRes1$SymUncRank<=topN,1,0)

    #
    flags <- c("ChisqFLAG","IVFLAG","InfGainFLAG","GainRatioFLAG","SymUncFLAG")
    allFiltRes1$Cleared <- apply(allFiltRes1[,flags],1,sum)
    #write.csv(allFiltRes1,"allFilterResults.csv")
    return(allFiltRes1)
  }






  ##RESULTS

  data_binned <- dataBinning(data)

  allFiltRes <- univFiltRes(data_binned)



  Var_set_1 = list(allFiltRes$VARS[allFiltRes$Cleared>3])

  Var_set_2 = list(allFiltRes$VARS[allFiltRes$Cleared==5])

  ##Final set of variables (Selected features)
  Final_variable = Var_set_1

  ##Replace dummy with the Final_results


  Final_variable = as.data.frame(Final_variable)
  colnames(Final_variable) = c("VARS")

  z = merge(Final_variable,allFiltRes[,c("VARS","chistat")],by = "VARS")
  z = z[order(-z$chistat),]

  z$Rank = seq(1,nrow(z),1)




  z$VariableImportance<-z$chistat
  z$chistat = NULL

  imp.features=z
  options(warn=0)
  FS <- list(imp.features)
  a=FS[[1]]
  c=a[,c("VARS","Rank")]
  b=subset(a[,c("VARS","VariableImportance")],a$Rank<=10)

  return (as.character(c$VARS))

}
