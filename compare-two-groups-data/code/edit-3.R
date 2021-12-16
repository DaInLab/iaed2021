#Get Subject Means
means<-aggregate(Value~Group+Subject, data=dat, FUN=mean)

#Initialize "dat2" dataframe
dat2<-dat

#Initialize within-Subject sd
s<-.001
pvals=matrix(nrow=10000,ncol=2)

for(j in 1:10000){
  #Sample individual measurements for each subject
  temp=NULL
  for(i in 1:nrow(means)){
    temp<-c(temp,rnorm(6,means[i,3], s))
  }
  
  #Set new values
  dat2[,3]<-temp
  
  #Take means of sampled values and fit to model
  dd2 <- aggregate(Value~Group+Subject, data=dat2, FUN=mean)
  fit2 <- lm(Value~Group, data=dd2)
  
  #Save sd and pvalue
  pvals[j,]<-cbind(s,anova(fit2)[[5]][5])
  
  #Update sd
  s<-s+.001
}

plot(pvals[,1],pvals[,2], xlab="Within-Subject SD", ylab="P-value")
