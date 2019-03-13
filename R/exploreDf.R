exploreDf <- function(df_full, dv) {

data<-df_full

#add string for data summary
Summary_df <- data.frame(unclass(summary(data)), check.names = FALSE, stringsAsFactors = FALSE)
writeLines("Summary of Input Data", con = "LogFile.txt", sep = '\n')
write.table(Summary_df, "LogFile.txt", sep = ",", col.names = T, append = T)
#write("End of Summary",file="LogFile.txt",append=TRUE)

#consider target variable name given as input in HybridFS function as DV'
n <- dv

if(length(unique(data[[n]]))!=2)
{
  stop("Error: Dependent variables should be 2 for binary classification!")
}

df_temp<-data

write.csv(df_temp,"prepro_step1.csv")

#get the list of categorical variables
cat_var=data.frame()

df_temp<-df_temp[, names(df_temp) != n]

#get the list of character variables
char <- df_temp[sapply(df_temp,is.character)]
cat_var<-char

#get the list of logical variables
logcl <- df_temp[sapply(df_temp, is.logical)]
cat_var<-cbind(cat_var,logcl)

#get the list of factor variables
fac <- df_temp[sapply(df_temp, is.factor)]
cat_var<-cbind(cat_var,fac)

cat_var_names<-list()
cat_var_names<-names(cat_var)

#display the list of categorical variables
cat_var_names
categorical <- list(categorical=I(cat_var_names))

#removing the categorical variables in df_temp
df_temp<-df_temp[, !sapply(df_temp,is.logical)]
df_temp<-df_temp[, !sapply(df_temp,is.character)]
df_temp<-df_temp[, !sapply(df_temp,is.factor)]

#determining other categorical variables with less than 52 levels

unique_lvl_cnt<-df_temp[lengths(lapply(df_temp, unique)) <= 52]
disc_var_names<-list()
disc_var_names<-names(unique_lvl_cnt)


#display the list of discrete variables
disc_var_names
discrete <- list(discrete=I(disc_var_names))

bench_cat_var<-cbind(cat_var,unique_lvl_cnt)
bench_cat_var_names<-list()
bench_cat_var_names<-names(bench_cat_var)
#display the list of benchmarked categorical variables
bench_cat_var_names
bench_categorical <- list(bench_categorical=I(bench_cat_var_names))

df_cont <- data[, names(data) != n]
for(i in names(cat_var))
{
  df_cont<-df_cont[names(df_cont) != i]
}
for(i in names(unique_lvl_cnt))
{
  df_cont<-df_cont[names(df_cont) != i]
}
cont_var_names<-list()
cont_var_names<-names(df_cont)

#display the list of continuous variables
cont_var_names
continuous <- list(continuous=I(cont_var_names))

#store the variables as list of lists
final_list <- list(discrete,categorical,continuous)
bench_final_list <- list(bench_categorical, continuous)

#add string for variable list
write("List of variables (as selected by user)",file="LogFile.txt",append=TRUE)
lapply(final_list, function(x) write.table( data.frame(x), 'LogFile.txt'  , append= T, sep=',' ))
write("List of variables (Default)",file="LogFile.txt",append=TRUE)
lapply(bench_final_list, function(x) write.table( data.frame(x), 'LogFile_Bench.txt'  , append= T, sep=',' ))

#lapply(final_list, function(x) write.table( data.frame(x), 'LogFile.csv'  , append= T, sep=',' ))
#lapply(bench_final_list, function(x) write.table( data.frame(x), 'LogFile.csv'  , append= T, sep=',' ))

discrete = unlist(discrete, use.names=FALSE)
categorical = unlist(categorical, use.names=FALSE)
continuous = unlist(continuous, use.names=FALSE)

max.len = max(length(discrete), length(categorical), length(continuous))
discrete = c(discrete, rep(NA, max.len - length(discrete)))
categorical = c(categorical, rep(NA, max.len - length(categorical)))
continuous = c(continuous, rep(NA, max.len - length(continuous)))
final_df <- data.frame(discrete, categorical, continuous)
write.csv(final_df,"variable_list.csv")

bench_categorical = unlist(bench_categorical, use.names=FALSE)
max.len1 = max(length(bench_categorical), length(continuous))
bench_categorical = c(bench_categorical, rep(NA, max.len1 - length(bench_categorical)))
bench_continuous = c(continuous, rep(NA, max.len1 - length(continuous)))
bench_final_df <- data.frame(bench_categorical, bench_continuous)
write.csv(bench_final_df,"benchmarking_variable_list.csv")

return(final_list)
#close loop and return lists
}
