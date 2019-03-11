variable_profiling_function  <- function(dv, vars,prevSessionid) {

  library(ggplot2)

  location <- getServerPath(prevSessionid,getwd())
  cleanPath <- paste0(location,'/cleaned_data.csv')

  dat = read.csv(file=cleanPath)

  drops <- c("X")
  dat<-dat[ , !(names(dat) %in% drops)]

  var1 = dat[,vars]
  dv = dat[,dv]

  freq <- table(var1,dv)
  total <- freq[,"0"]+freq[,"1"]
  meaniv = freq[,"1"]/total

  df1=as.data.frame(total)
  df1$meaniv=meaniv
  df1$levels=rownames(df1)
  df1$freq=freq[,"1"]

  ggplot()+geom_col(data=df1, aes(y=total,x=levels))+labs(title = "Bivariate Analysis") + xlab(vars) +geom_hline(aes(yintercept = mean(meaniv)*max(total)))+geom_line(
    data=df1, aes(y=meaniv*max(total),x=levels, group=1),size=0.25)+
    geom_point(data=df1, aes(y=meaniv*max(total),x=levels, group=1))+theme(
      panel.background = element_rect(fill = "aliceblue",
                                      colour = "lightblue",
                                      size = 0.5),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"),
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "lightblue")
    )+scale_y_continuous(sec.axis = sec_axis(~./max(total)*100, name = "[%] Event Rate"))
}
