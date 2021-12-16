#Get Invidual Means
summary=NULL
for(i in unique(dat[,2])){
  sub<-which(dat[,2]==i)
  summary<-rbind(summary,cbind(
    dat[sub,1][3],
    dat[sub,2][4],
    mean(dat[sub,3]),
    sd(dat[sub,3])
  )
  )
}
colnames(summary)<-c("Group","Subject","Mean","SD")

TukeyHSD(aov(summary[,3]~as.factor(summary[,1])+ (1|summary[,2])))

#      Tukey multiple comparisons of means
#        95% family-wise confidence level
#    
#    Fit: aov(formula = summary[, 3] ~ as.factor(summary[, 1]) + (1 | summary[, 2]))
#    
#    $`as.factor(summary[, 1])`
#             diff       lwr       upr     p adj
#    2-1 -0.672619 -4.943205  3.597967 0.9124024
#    3-1  7.507937  1.813822 13.202051 0.0098935
#    3-2  8.180556  2.594226 13.766885 0.0046312

