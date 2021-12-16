n.simulations<-10000
pvals=matrix(nrow=n.simulations,ncol=1)
for(k in 1:n.simulations){
  subject=NULL
  for(i in 1:10){
    subject<-rbind(subject,as.matrix(rep(i,6)))
  }
  #set.seed(42)
  
  #Sample Subject Means
  subject.means<-rnorm(10,100,2)
  
  #Sample Individual Measurements
  values=NULL
  for(sm in subject.means){
    values<-rbind(values,as.matrix(rnorm(6,sm,20)))
  }
  
  out<-cbind(subject,values)
  
  #Split into GroupA and GroupB
  GroupA<-out[1:30,]
  GroupB<-out[31:60,]
  
  #Add effect size to GroupA
  GroupA[,2]<-GroupA[,2]+0
  
  colnames(GroupA)<-c("Subject", "Value")
  colnames(GroupB)<-c("Subject", "Value")
  
  #Calculate Individual Means and SDS
  GroupA.summary=matrix(nrow=length(unique(GroupA[,1])), ncol=2)
  for(i in 1:length(unique(GroupA[,1]))){
    GroupA.summary[i,1]<-mean(GroupA[which(GroupA[,1]==unique(GroupA[,1])[i]),2])
    GroupA.summary[i,2]<-sd(GroupA[which(GroupA[,1]==unique(GroupA[,1])[i]),2])
  }
  colnames(GroupA.summary)<-c("Mean","SD")
  
  
  GroupB.summary=matrix(nrow=length(unique(GroupB[,1])), ncol=2)
  for(i in 1:length(unique(GroupB[,1]))){
    GroupB.summary[i,1]<-mean(GroupB[which(GroupB[,1]==unique(GroupB[,1])[i]),2])
    GroupB.summary[i,2]<-sd(GroupB[which(GroupB[,1]==unique(GroupB[,1])[i]),2])
  }
  colnames(GroupB.summary)<-c("Mean","SD")
  
  Summary<-rbind(cbind(1,GroupA.summary),cbind(2,GroupB.summary))
  colnames(Summary)[1]<-"Group"
  
  pvals[k]<-t.test(GroupA.summary[,1],GroupB.summary[,1], var.equal=T)$p.value
}

#Plots
par(mfrow=c(2,2))
boxplot(GroupA[,2]~GroupA[,1], col="Red", main="Group A", 
        ylim=c(.9*min(out[,2]),1.1*max(out[,2])),
        xlab="Subject", ylab="Value")
stripchart(GroupA[,2]~GroupA[,1], vert=T, pch=16, add=T)
#abline(h=mean(GroupA[,2]), lty=2, lwd=3)

for(i in 1:length(unique(GroupA[,1]))){
  m<-mean(GroupA[which(GroupA[,1]==unique(GroupA[,1])[i]),2])
  ci<-t.test(GroupA[which(GroupA[,1]==unique(GroupA[,1])[i]),2])$conf.int[1:2]
  
  points(i-.2,m, pch=15,cex=1.5, col="Grey")
  segments(i-.2,
           ci[1],i-.2,
           ci[2], lwd=4, col="Grey"
  )
}
legend("topleft", legend=c("Individual Means +/- 95% CI"), bty="n", pch=15, lwd=3, col="Grey")


boxplot(GroupB[,2]~GroupB[,1], col="Light Blue", main="Group B", 
        ylim=c(.9*min(out[,2]),1.1*max(out[,2])),
        xlab="Subject", ylab="Value")
stripchart(GroupB[,2]~GroupB[,1], vert=T, pch=16, add=T)
#abline(h=mean(GroupB[,2]), lty=2, lwd=3)

for(i in 1:length(unique(GroupB[,1]))){
  m<-mean(GroupB[which(GroupB[,1]==unique(GroupB[,1])[i]),2])
  ci<-t.test(GroupB[which(GroupB[,1]==unique(GroupB[,1])[i]),2])$conf.int[1:2]
  
  points(i-.2,m, pch=15,cex=1.5, col="Grey")
  segments(i-.2,
           ci[1],i-.2,
           ci[2], lwd=4, col="Grey"
  )
}
legend("topleft", legend=c("Individual Means +/- 95% CI"), bty="n", pch=15, lwd=3, col="Grey")


boxplot(Summary[,2]~Summary[,1], col=c("Red","Light Blue"), xlab="Group", ylab="Average Value",
        ylim=c(.9*min(Summary[,2]),1.1*max(Summary[,2])),
        main="Individual Averages")
stripchart(Summary[,2]~Summary[,1], vert=T, pch=16, add=T)

points(.9, mean(GroupA.summary[,1]), pch=15,cex=1.5, col="Grey")
segments(.9,
         t.test(GroupA.summary[,1])$conf.int[1],.9,
         t.test(GroupA.summary[,1])$conf.int[2], lwd=4, col="Grey"
)

points(1.9, mean(GroupB.summary[,1]), pch=15,cex=1.5, col="Grey")
segments(1.9,
         t.test(GroupB.summary[,1])$conf.int[1],1.9,
         t.test(GroupB.summary[,1])$conf.int[2], lwd=4, col="Grey"
)
legend("topleft", legend=c("Group Means +/- 95% CI"), bty="n", pch=15, lwd=3, col="Grey")


hist(pvals, breaks=seq(0,1,by=.05), col="Grey",
     main=c(paste("# sims=", n.simulations),
            paste("% Sig p-values=",100*length(which(pvals<0.05))/length(pvals)))
)


