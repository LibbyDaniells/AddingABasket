#Simulation Study where the cut-off values are calibrated based on the interim sample size of 14, with this cut-off value applied to sample sizes 50-200% of 14 (i.e. 7-28). The sample size which gave the most inflation 
#over this n5=14 value was then selected and calibrated for, simulation study repeated with this cut-off value. 

library(rjags)
library(textmineR)
library(matrixStats)
library(parallel)
library(MASS)

Method3a <- function(nDelta,p,K1,K2,run,q0,pw){
  K <- K1+K2
  n <- nDelta[1:K]
  cut.off <- nDelta[(K+1):(2*K)]
  no.successes <- rep(0,K)
  Fun <- function(x){
    fun <- sum(x>q0)/length(x)
    return(fun)
  }
  point_estimates <- matrix(NA,nrow=run,ncol=K)
  hypo <- matrix(NA,nrow=run,ncol=K)
  response_store <- matrix(NA,nrow=run,ncol=K)
  for(j in 1:run){
    set.seed(j)
    for(i in 1:K){
      no.successes[i] <- rbinom(1,n[i],p[i])
    }
    response_store[j,] <- no.successes
    nexmu <- rep(log(pw/(1-pw)),K) #NEX mu parameter
    nexsigma <- rep((1/pw)+(1/(1-pw)),K) #NEX sigma parameter
    mu <- log(q0/(1-q0))
    prob <- c(0.5,0.5) #Fixed weights
    jags.data <- list('n'=n,'y'=no.successes,'N'=K,'nexmu'=nexmu,'nexsigma'=nexsigma,'prob'=prob,'MU'=mu)
    jags.fit <- jags.model(file='EXNEX.txt',data=jags.data,n.adapt=1000,n.chains=4,quiet = T)
    samplesEXNEX <- coda.samples(jags.fit,variable.names = c('p'),n.iter=10000,silent=TRUE)
    samplesEXNEX <- as.data.frame(samplesEXNEX[[1]])
    Model <- as.matrix(samplesEXNEX[,1:K])
    point_estimates[j,] <- colMeans(Model)
    post_prob <- apply(Model,2,Fun)
    hypo[j,] <- as.integer(post_prob>cut.off)
    print(j)
  }
  reject <- colMeans(hypo)
  perfect <- 0
  true <- as.numeric(p>q0)
  for(i in 1:run){
    if(all(hypo[i,]==true)){
      perfect <- perfect+1
    }
  }
  fwerID <- which(true==0)
  if(length(fwerID)==0){
    fwer <- 'NA'
  }else{
    fwer <- 0
    for(i in 1:run){
      if(sum(hypo[i,fwerID])!=0){
        fwer <- fwer+1
      }
    }
  }
  output <- list('Responses'=response_store,'Point Estimates'=point_estimates,'Hypothesis Rejection'=hypo,'Rejection Percentage'=reject,'FWER'=fwer,'All Correct'=perfect)
  return(output)
}





#Simulation
K1 <- 4
K2 <- 1
K <- K1+K2
q0 <- 0.2
p <- rep(q0,K)
n <- c(rep(24,K1),rep(14,K2))
pw <- 0.3
run <- 5000


load('WorstCaseDelta1_5.RData')
cut.off <- cut_off[9,] #Cut-off value for n5=9 as this gave the most inflation over n5=14

p1 <- rep(q0,K)
p2 <- c(0.4,0.2,0.2,0.2,0.2)
p3 <- c(0.4,0.4,0.2,0.2,0.2)
p4 <- c(0.4,0.4,0.4,0.2,0.2)
p5 <- c(0.4,0.4,0.4,0.4,0.2)
p6 <- rep(0.4,K)
p7 <- c(0.2,0.2,0.2,0.2,0.4)
p8 <- c(0.4,0.2,0.2,0.2,0.4)
p9 <- c(0.4,0.4,0.2,0.2,0.4)
p10 <- c(0.4,0.4,0.4,0.2,0.4)
pmat <- rbind(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)

nDelta_list <- list()
for(i in 7:28){
  nDelta_list[[i-6]] <- c(24,24,24,24,i,cut.off)
}

Fix14 <- list()
for(i in 1:10){
  Fix14[[i]] <- mclapply(nDelta_list,Method3a,pmat[i,],K1,K2,run,q0,p2,mc.cores=22)
}
save(Fix14,file='FixedAt14Applied7_28Conservative.RData')


#Finding worst inflation compared to n5=14--------------
load('FixedAt14Applied7_28.RData')

# 
# Sc1 <- matrix(NA,ncol=5,nrow=22)
# Sc2 <- matrix(NA,ncol=5,nrow=22)
# Sc3 <- matrix(NA,ncol=5,nrow=22)
# Sc4 <- matrix(NA,ncol=5,nrow=22)
# Sc5 <- matrix(NA,ncol=5,nrow=22)
# Sc6 <- matrix(NA,ncol=5,nrow=22)
# Sc7 <- matrix(NA,ncol=5,nrow=22)
# Sc8 <- matrix(NA,ncol=5,nrow=22)
# Sc9 <- matrix(NA,ncol=5,nrow=22)
# Sc10 <- matrix(NA,ncol=5,nrow=22)
# for(i in 1:22){
#   Sc1[i,] <- Fix14[[1]][[i]]$`Rejection Percentage`*100
#   Sc2[i,] <- Fix14[[2]][[i]]$`Rejection Percentage`*100
#   Sc3[i,] <- Fix14[[3]][[i]]$`Rejection Percentage`*100
#   Sc4[i,] <- Fix14[[4]][[i]]$`Rejection Percentage`*100
#   Sc5[i,] <- Fix14[[5]][[i]]$`Rejection Percentage`*100
#   Sc6[i,] <- Fix14[[6]][[i]]$`Rejection Percentage`*100
#   Sc7[i,] <- Fix14[[7]][[i]]$`Rejection Percentage`*100
#   Sc8[i,] <- Fix14[[8]][[i]]$`Rejection Percentage`*100
#   Sc9[i,] <- Fix14[[9]][[i]]$`Rejection Percentage`*100
#   Sc10[i,] <- Fix14[[10]][[i]]$`Rejection Percentage`*100
# }
# scenario <- list(Sc1,Sc2,Sc3,Sc4,Sc5,Sc6,Sc7,Sc8,Sc9,Sc10)
# 
# par(mfrow=c(2,2))
# col <- c(brewer.pal(6,'Set3'),brewer.pal(5,'Set1'))
# plot(scenario[[1]][,5],xaxt='n',type='b',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Change in Error Rate in New Basket as n5 Changes')
# for(i in 2:5){
#   points(scenario[[i]][,5],type='b',pch=16,col=col[i+1])
# }
# axis(1,at=1:22,7:28)
# legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 5'),col=c('black',col[3:6]),pch=16,cex=0.8)
# 
# 
# which.max(scenario[[1]][,5]-scenario[[1]][,5][8]) #n5=26 gives biggest inflation compared to the fixed value of 14 under the null scenario
# which.max(scenario[[2]][,5]-scenario[[2]][,5][8]) #n5=25 gives biggest inflation compared to the fixed value of 14 under scenario 2
# which.max(scenario[[3]][,5]-scenario[[3]][,5][8]) #n5=28 gives biggest inflation compared to the fixed value of 14 under scenario 3
# which.max(scenario[[4]][,5]-scenario[[4]][,5][8]) #n5=8 gives biggest inflation compared to the fixed value of 14 under scenario 4
# which.max(scenario[[5]][,5]-scenario[[5]][,5][8]) #n5=9 gives biggest inflation compared to the fixed value of 14 under scenario 5
# 
# 
# plot(scenario[[6]][,5],col=col[7],xaxt='n',type='b',pch=16,ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Change in Power in New Basket as n5 Changes')
# for(i in 7:10){
#   points(scenario[[i]][,5],type='b',pch=16,col=col[i+1])
# }
# axis(1,at=1:22,7:28)
# legend('bottomright',legend=c('Scenario 6','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c(col[7:11]),pch=16,cex=0.8)
# 
# plot(apply(scenario[[1]][,1:4],1,mean),type='b',xaxt='n',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Mean Change in Error Rate in Existing Baskets as n5 Changes')
# axis(1,at=1:22,7:28)
# points(apply(scenario[[2]][,2:4],1,mean),col=col[3],pch=16,type='b')
# points(apply(scenario[[3]][,3:4],1,mean),col=col[4],pch=16,type='b')
# points(scenario[[4]][,4],col=col[5],pch=16,type='b')
# points(apply(scenario[[7]][,1:4],1,mean),col=col[8],pch=16,type='b')
# points(apply(scenario[[8]][,2:4],1,mean),col=col[9],pch=16,type='b')
# points(apply(scenario[[9]][,3:4],1,mean),col=col[10],pch=16,type='b')
# points(scenario[[10]][,4],col=col[11],pch=16,type='b')
# legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c('black',col[3:5],col[8:11]),pch=16,cex=0.8)
# 
# plot(scenario[[2]][,1],typ='b',xaxt='n',pch=16,col=col[3],ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Mean Change in Power in Existing Baskets as n5 Changes')
# points(apply(scenario[[3]][,1:2],1,mean),col=col[4],pch=16,type='b')
# points(apply(scenario[[4]][,1:3],1,mean),col=col[5],pch=16,type='b')
# points(apply(scenario[[5]][,1:4],1,mean),col=col[6],pch=16,type='b')
# points(apply(scenario[[6]][,1:4],1,mean),col=col[7],pch=16,type='b')
# points(scenario[[8]][,1],col=col[9],pch=16,type='b')
# points(apply(scenario[[9]][,1:2],1,mean),col=col[10],pch=16,type='b')
# points(apply(scenario[[10]][,1:3],1,mean),col=col[11],pch=16,type='b')
# legend('bottomright',legend=c('Scenario 2','Scenario 3','Scenario 4','Scenario 5','Scenario 6','Scenario 8','Scenario 9','Scenario 10'),col=c(col[3:7],col[9:11]),pch=16,cex=0.8)
# axis(1,at=1:22,7:28)
# 
# 
#Apply most conservative cut-off--------------
load('FixedAt14Applied7_28Conservative.RData')


Sc1 <- matrix(NA,ncol=5,nrow=22)
Sc2 <- matrix(NA,ncol=5,nrow=22)
Sc3 <- matrix(NA,ncol=5,nrow=22)
Sc4 <- matrix(NA,ncol=5,nrow=22)
Sc5 <- matrix(NA,ncol=5,nrow=22)
Sc6 <- matrix(NA,ncol=5,nrow=22)
Sc7 <- matrix(NA,ncol=5,nrow=22)
Sc8 <- matrix(NA,ncol=5,nrow=22)
Sc9 <- matrix(NA,ncol=5,nrow=22)
Sc10 <- matrix(NA,ncol=5,nrow=22)
for(i in 1:22){
  Sc1[i,] <- Fix14[[1]][[i]]$`Rejection Percentage`*100
  Sc2[i,] <- Fix14[[2]][[i]]$`Rejection Percentage`*100
  Sc3[i,] <- Fix14[[3]][[i]]$`Rejection Percentage`*100
  Sc4[i,] <- Fix14[[4]][[i]]$`Rejection Percentage`*100
  Sc5[i,] <- Fix14[[5]][[i]]$`Rejection Percentage`*100
  Sc6[i,] <- Fix14[[6]][[i]]$`Rejection Percentage`*100
  Sc7[i,] <- Fix14[[7]][[i]]$`Rejection Percentage`*100
  Sc8[i,] <- Fix14[[8]][[i]]$`Rejection Percentage`*100
  Sc9[i,] <- Fix14[[9]][[i]]$`Rejection Percentage`*100
  Sc10[i,] <- Fix14[[10]][[i]]$`Rejection Percentage`*100
}
scenario <- list(Sc1,Sc2,Sc3,Sc4,Sc5,Sc6,Sc7,Sc8,Sc9,Sc10)

par(mfrow=c(2,2))
col <- c(brewer.pal(6,'Set3'),brewer.pal(5,'Set1'))
plot(scenario[[1]][,5],xaxt='n',type='b',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Change in Error Rate in New Basket as n5 Changes')
for(i in 2:5){
  points(scenario[[i]][,5],type='b',pch=16,col=col[i+1])
}
axis(1,at=1:22,7:28)
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 5'),col=c('black',col[3:6]),pch=16,cex=0.8)


which.max(scenario[[1]][,5]-scenario[[1]][,5][8]) #n5=26 gives biggest inflation compared to the fixed value of 14 under the null scenario
which.max(scenario[[2]][,5]-scenario[[2]][,5][8]) #n5=25 gives biggest inflation compared to the fixed value of 14 under scenario 2
which.max(scenario[[3]][,5]-scenario[[3]][,5][8]) #n5=28 gives biggest inflation compared to the fixed value of 14 under scenario 3
which.max(scenario[[4]][,5]-scenario[[4]][,5][8]) #n5=8 gives biggest inflation compared to the fixed value of 14 under scenario 4
which.max(scenario[[5]][,5]-scenario[[5]][,5][8]) #n5=9 gives biggest inflation compared to the fixed value of 14 under scenario 5


plot(scenario[[6]][,5],col=col[7],xaxt='n',type='b',pch=16,ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Change in Power in New Basket as n5 Changes')
for(i in 7:10){
  points(scenario[[i]][,5],type='b',pch=16,col=col[i+1])
}
axis(1,at=1:22,7:28)
legend('bottomright',legend=c('Scenario 6','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c(col[7:11]),pch=16,cex=0.8)

plot(apply(scenario[[1]][,1:4],1,mean),type='b',xaxt='n',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Mean Change in Error Rate in Existing Baskets as n5 Changes')
axis(1,at=1:22,7:28)
points(apply(scenario[[2]][,2:4],1,mean),col=col[3],pch=16,type='b')
points(apply(scenario[[3]][,3:4],1,mean),col=col[4],pch=16,type='b')
points(scenario[[4]][,4],col=col[5],pch=16,type='b')
points(apply(scenario[[7]][,1:4],1,mean),col=col[8],pch=16,type='b')
points(apply(scenario[[8]][,2:4],1,mean),col=col[9],pch=16,type='b')
points(apply(scenario[[9]][,3:4],1,mean),col=col[10],pch=16,type='b')
points(scenario[[10]][,4],col=col[11],pch=16,type='b')
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c('black',col[3:5],col[8:11]),pch=16,cex=0.8)

plot(scenario[[2]][,1],typ='b',xaxt='n',pch=16,col=col[3],ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Mean Change in Power in Existing Baskets as n5 Changes')
points(apply(scenario[[3]][,1:2],1,mean),col=col[4],pch=16,type='b')
points(apply(scenario[[4]][,1:3],1,mean),col=col[5],pch=16,type='b')
points(apply(scenario[[5]][,1:4],1,mean),col=col[6],pch=16,type='b')
points(apply(scenario[[6]][,1:4],1,mean),col=col[7],pch=16,type='b')
points(scenario[[8]][,1],col=col[9],pch=16,type='b')
points(apply(scenario[[9]][,1:2],1,mean),col=col[10],pch=16,type='b')
points(apply(scenario[[10]][,1:3],1,mean),col=col[11],pch=16,type='b')
legend('bottomright',legend=c('Scenario 2','Scenario 3','Scenario 4','Scenario 5','Scenario 6','Scenario 8','Scenario 9','Scenario 10'),col=c(col[3:7],col[9:11]),pch=16,cex=0.8)
axis(1,at=1:22,7:28)
# 
# cons[[1]]
# Fix14[[1]]
