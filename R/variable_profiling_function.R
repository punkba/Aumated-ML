variable_profiling_function  <- function(dv, vars) {
  
  library(ggplot2)
  
  dat = read.csv("c:/opencpuapp_ip/cleaned_data.csv")
  
  drops <- c("X")
  dat<-dat[ , !(names(dat) %in% drops)]
  
  var1 = dat[,vars]
  dv = dat[,dv]
  
  freq <- table(var1,dv)
  total <- freq[,"0"]+freq[,"1"]
  meaniv = freq[,"1"]/total
  
  df1=as.data.frame(total)
  df1$meaniv=meaniv
  df1$levels=unique(dat[,vars])
  
  ggplot()+geom_col(data=df1, aes(y=total,x=levels))+labs(title = "Bivariate Analysis") + xlab(vars) +geom_line(data=df1, aes(y=meaniv*100,x=levels))+theme(
    panel.background = element_rect(fill = "aliceblue",
                                    colour = "lightblue",
                                    size = 0.5),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "lightblue")
  )+scale_y_continuous(sec.axis = sec_axis(~.*.75, name = "[%] Event Rate"))
}
