#Simulation study where Delta values are calibrated under fixed sample sizes (n5=5,10,15,20,24) and applied to plus of minus 5 of the fixed value

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


load('WorstCaseDelta1_5.RData') #Cut-off values obtained from WorstCase3bCalibration.R

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


cut_offpm5 <- rbind(cut_off[5,],cut_off[10,],cut_off[15,],cut_off[20,],cut_off[24])

#Sample Size and Delta
n_Delta_list5 <- list()
for(i in 1:10){
  n_Delta_list5[[i]] <- c(24,24,24,24,i,cut_offpm5[1,])
}
n_Delta_list10 <- list()
for(i in 5:15){
  n_Delta_list10[[i-4]] <- c(24,24,24,24,i,cut_offpm5[2,])
}
n_Delta_list15 <- list()
for(i in 10:20){
  n_Delta_list15[[i-9]] <- c(24,24,24,24,i,cut_offpm5[3,])
}
n_Delta_list20 <- list()
for(i in 15:24){
  n_Delta_list20[[i-14]] <- c(24,24,24,24,i,cut_offpm5[4,])
}
n_Delta_list24 <- list()
for(i in 19:24){
  n_Delta_list24[[i-18]] <- c(24,24,24,24,i,cut_offpm5[5,])
}

#Simulation

# Fixed5 <- list()
# for(i in 1:10){
#   Fixed5[[i]] <- mclapply(n_Delta_list5,Method3a,pmat[i,],K1,K2,run,q0,p2,mc.cores=25)
# }
# save(Fixed5,file='WorstCasePM5Fixed5.RData')
# 
# Fixed10 <- list()
# for(i in 1:10){
#   Fixed10[[i]] <- mclapply(n_Delta_list10,Method3a,pmat[i,],K1,K2,run,q0,p2,mc.cores=25)
# }
# save(Fixed10,file='WorstCasePM5Fixed10.RData')
# 
# Fixed15 <- list()
# for(i in 1:10){
#   Fixed15[[i]] <- mclapply(n_Delta_list15,Method3a,pmat[i,],K1,K2,run,q0,p2,mc.cores=25)
# }
# save(Fixed15,file='WorstCasePM5Fixed15.RData')
# 
# Fixed20 <- list()
# for(i in 1:10){
#   Fixed20[[i]] <- mclapply(n_Delta_list20,Method3a,pmat[i,],K1,K2,run,q0,p2,mc.cores=25)
# }
# save(Fixed20,file='WorstCasePM5Fixed20.RData')
# 
# Fixed24 <- list()
# for(i in 1:10){
#   Fixed24[[i]] <- mclapply(n_Delta_list24,Method3a,pmat[i,],K1,K2,run,q0,p2,mc.cores=25)
# }
# save(Fixed24,file='WorstCasePM5Fixed24.RData')


#Plots
load('WorstCasePM5Fixed5.RData')
load('WorstCasePM5Fixed10.RData')
load('WorstCasePM5Fixed15.RData')
load('WorstCasePM5Fixed20.RData')
load('WorstCasePM5Fixed24.RData')

Fixed5_ScR <- list()
Fixed10_ScR <- list()
Fixed15_ScR <- list()
Fixed20_ScR <- list()
Fixed24_ScR <- list()
for(j in 1:10){ #Scenario
  Fixed5_Rmat <- matrix(NA,nrow=10,ncol=5)
  Fixed10_Rmat <- matrix(NA,nrow=11,ncol=5)
  Fixed15_Rmat <- matrix(NA,nrow=11,ncol=5)
  Fixed20_Rmat <- matrix(NA,nrow=10,ncol=5)
  Fixed24_Rmat <- matrix(NA,nrow=6,ncol=5)
  for(i in 1:10){ #n5
    Fixed5_Rmat[i,] <- Fixed5[[j]][[i]]$`Rejection Percentage`*100
    Fixed20_Rmat[i,] <- Fixed20[[j]][[i]]$`Rejection Percentage`*100
  }
  for(i in 1:6){ #n5
    Fixed24_Rmat[i,] <- Fixed24[[j]][[i]]$`Rejection Percentage`*100
  }
  for(i in 1:11){ #n5
  Fixed10_Rmat[i,] <- Fixed10[[j]][[i]]$`Rejection Percentage`*100
  Fixed15_Rmat[i,] <- Fixed15[[j]][[i]]$`Rejection Percentage`*100
}
  Fixed5_ScR[[j]] <- Fixed5_Rmat
  Fixed10_ScR[[j]] <- Fixed10_Rmat
  Fixed15_ScR[[j]] <- Fixed15_Rmat
  Fixed20_ScR[[j]] <- Fixed20_Rmat
  Fixed24_ScR[[j]] <- Fixed24_Rmat
}

#Error For New Basket
par(mfrow=c(2,3))
col <- c(brewer.pal(6,'Set3'),brewer.pal(5,'Set1'))
plot(Fixed5_ScR[[1]][,5],type='b',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=5, applied to sample size of 1-10',xaxt='n')
axis(1,1:10)
for(i in 2:5){
  points(Fixed5_ScR[[i]][,5],type='b',pch=16,col=col[i+1])
}
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 5'),col=c('black',col[3:6]),pch=16,cex=0.8)
plot(Fixed10_ScR[[1]][,5],type='b',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=10, applied to sample size of 5-15',xaxt='n')
axis(1,at=1:11,5:15)
for(i in 2:5){
  points(Fixed10_ScR[[i]][,5],type='b',pch=16,col=col[i+1])
}
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 5'),col=c('black',col[3:6]),pch=16,cex=0.8)
plot(Fixed15_ScR[[1]][,5],type='b',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=15, applied to sample size of 10-20',xaxt='n')
axis(1,at=1:11,10:20)
for(i in 2:5){
  points(Fixed15_ScR[[i]][,5],type='b',pch=16,col=col[i+1])
}
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 5'),col=c('black',col[3:6]),pch=16,cex=0.8)
plot(Fixed20_ScR[[1]][,5],type='b',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=20, applied to sample size of 15-24',xaxt='n')
axis(1,at=1:10,15:24)
for(i in 2:5){
  points(Fixed20_ScR[[i]][,5],type='b',pch=16,col=col[i+1])
}
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 5'),col=c('black',col[3:6]),pch=16,cex=0.8)
plot(Fixed24_ScR[[1]][,5],type='b',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=24, applied to sample size of 19-24',xaxt='n')
axis(1,at=1:6,19:24)
for(i in 2:5){
  points(Fixed24_ScR[[i]][,5],type='b',pch=16,col=col[i+1])
}
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 5'),col=c('black',col[3:6]),pch=16,cex=0.8)
mtext("Error Rate in New Basket", side = 3, line = -1.5, outer = TRUE)


#Power New Basket
par(mfrow=c(2,3))
plot(Fixed5_ScR[[6]][,5],type='b',col=col[7],pch=16,ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=5, applied to sample size of 1-10',xaxt='n')
axis(1,at=1:10,1:10)
for(i in 7:10){
  points(Fixed5_ScR[[i]][,5],type='b',pch=16,col=col[i+1])
}
legend('bottomright',legend=c('Scenario 6','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c(col[7:11]),pch=16,cex=0.8)
plot(Fixed10_ScR[[6]][,5],type='b',col=col[7],pch=16,ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=10, applied to sample size of 5-15',xaxt='n')
axis(1,at=1:11,5:15)
for(i in 7:10){
  points(Fixed10_ScR[[i]][,5],type='b',pch=16,col=col[i+1])
}
legend('bottomright',legend=c('Scenario 6','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c(col[7:11]),pch=16,cex=0.8)
plot(Fixed15_ScR[[6]][,5],type='b',col=col[7],pch=16,ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=15, applied to sample size of 10-20',xaxt='n')
axis(1,at=1:11,10:20)
for(i in 7:10){
  points(Fixed15_ScR[[i]][,5],type='b',pch=16,col=col[i+1])
}
legend('bottomright',legend=c('Scenario 6','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c(col[7:11]),pch=16,cex=0.8)
plot(Fixed20_ScR[[6]][,5],type='b',col=col[7],pch=16,ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=20, applied to sample size of 15-24',xaxt='n')
axis(1,at=1:10,15:24)
for(i in 7:10){
  points(Fixed20_ScR[[i]][,5],type='b',pch=16,col=col[i+1])
}
legend('bottomright',legend=c('Scenario 6','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c(col[7:11]),pch=16,cex=0.8)
plot(Fixed24_ScR[[6]][,5],type='b',col=col[7],pch=16,ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=24, applied to sample size of 19-24',xaxt='n')
axis(1,at=1:6,19:24)
for(i in 7:10){
  points(Fixed24_ScR[[i]][,5],type='b',pch=16,col=col[i+1])
}
legend('bottomright',legend=c('Scenario 6','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c(col[7:11]),pch=16,cex=0.8)
mtext("Power in New Basket", side = 3, line = -1.5, outer = TRUE)


#Error in Existing Baskets
par(mfrow=c(2,3))
col <- c(brewer.pal(6,'Set3'),brewer.pal(5,'Set1'))
plot(apply(Fixed5_ScR[[1]][,1:4],1,mean),type='b',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=5, applied to sample size of 1-10',xaxt='n')
points(apply(Fixed5_ScR[[2]][,2:4],1,mean),type='b',pch=16,col=col[3])
points(apply(Fixed5_ScR[[3]][,3:4],1,mean),type='b',pch=16,col=col[4])
points(Fixed5_ScR[[4]][,4],type='b',pch=16,col=col[5])
points(apply(Fixed5_ScR[[7]][,1:4],1,mean),type='b',pch=16,col=col[8])
points(apply(Fixed5_ScR[[8]][,2:4],1,mean),type='b',pch=16,col=col[9])
points(apply(Fixed5_ScR[[9]][,3:4],1,mean),type='b',pch=16,col=col[10])
points(Fixed5_ScR[[10]][,4],type='b',pch=16,col=col[11])
axis(1,1:10)
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c('black',col[3:5],col[8:11]),pch=16,cex=0.8)
plot(apply(Fixed10_ScR[[1]][,1:4],1,mean),type='b',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=10, applied to sample size of 5-15',xaxt='n')
points(apply(Fixed10_ScR[[2]][,2:4],1,mean),type='b',pch=16,col=col[3])
points(apply(Fixed10_ScR[[3]][,3:4],1,mean),type='b',pch=16,col=col[4])
points(Fixed10_ScR[[4]][,4],type='b',pch=16,col=col[5])
points(apply(Fixed10_ScR[[7]][,1:4],1,mean),type='b',pch=16,col=col[8])
points(apply(Fixed10_ScR[[8]][,2:4],1,mean),type='b',pch=16,col=col[9])
points(apply(Fixed10_ScR[[9]][,3:4],1,mean),type='b',pch=16,col=col[10])
points(Fixed10_ScR[[10]][,4],type='b',pch=16,col=col[11])
axis(1,at=1:11,5:15)
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c('black',col[3:5],col[8:11]),pch=16,cex=0.8)
plot(apply(Fixed15_ScR[[1]][,1:4],1,mean),type='b',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=15, applied to sample size of 10-20',xaxt='n')
points(apply(Fixed15_ScR[[2]][,2:4],1,mean),type='b',pch=16,col=col[3])
points(apply(Fixed15_ScR[[3]][,3:4],1,mean),type='b',pch=16,col=col[4])
points(Fixed15_ScR[[4]][,4],type='b',pch=16,col=col[5])
points(apply(Fixed15_ScR[[7]][,1:4],1,mean),type='b',pch=16,col=col[8])
points(apply(Fixed15_ScR[[8]][,2:4],1,mean),type='b',pch=16,col=col[9])
points(apply(Fixed15_ScR[[9]][,3:4],1,mean),type='b',pch=16,col=col[10])
points(Fixed15_ScR[[10]][,4],type='b',pch=16,col=col[11])
axis(1,at=1:11,10:20)
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c('black',col[3:5],col[8:11]),pch=16,cex=0.8)
plot(apply(Fixed20_ScR[[1]][,1:4],1,mean),type='b',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=20, applied to sample size of 15-24',xaxt='n')
points(apply(Fixed20_ScR[[2]][,2:4],1,mean),type='b',pch=16,col=col[3])
points(apply(Fixed20_ScR[[3]][,3:4],1,mean),type='b',pch=16,col=col[4])
points(Fixed20_ScR[[4]][,4],type='b',pch=16,col=col[5])
points(apply(Fixed20_ScR[[7]][,1:4],1,mean),type='b',pch=16,col=col[8])
points(apply(Fixed20_ScR[[8]][,2:4],1,mean),type='b',pch=16,col=col[9])
points(apply(Fixed20_ScR[[9]][,3:4],1,mean),type='b',pch=16,col=col[10])
points(Fixed20_ScR[[10]][,4],type='b',pch=16,col=col[11])
axis(1,at=1:10,15:24)
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c('black',col[3:5],col[8:11]),pch=16,cex=0.8)
plot(apply(Fixed24_ScR[[1]][,1:4],1,mean),type='b',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=24, applied to sample size of 19-24',xaxt='n')
points(apply(Fixed24_ScR[[2]][,2:4],1,mean),type='b',pch=16,col=col[3])
points(apply(Fixed24_ScR[[3]][,3:4],1,mean),type='b',pch=16,col=col[4])
points(Fixed24_ScR[[4]][,4],type='b',pch=16,col=col[5])
points(apply(Fixed24_ScR[[7]][,1:4],1,mean),type='b',pch=16,col=col[8])
points(apply(Fixed24_ScR[[8]][,2:4],1,mean),type='b',pch=16,col=col[9])
points(apply(Fixed24_ScR[[9]][,3:4],1,mean),type='b',pch=16,col=col[10])
points(Fixed24_ScR[[10]][,4],type='b',pch=16,col=col[11])
axis(1,at=1:6,19:24)
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c('black',col[3:5],col[8:11]),pch=16,cex=0.8)
mtext("Mean Error Rate in Existing Baskets", side = 3, line = -1.5, outer = TRUE)


#Power in Existing Baskets
#Error in Existing Baskets
par(mfrow=c(2,3))
col <- c(brewer.pal(6,'Set3'),brewer.pal(5,'Set1'))
plot(Fixed5_ScR[[2]][,1],col=col[3],type='b',pch=16,ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=5, applied to sample size of 1-10',xaxt='n')
points(apply(Fixed5_ScR[[3]][,1:2],1,mean),type='b',pch=16,col=col[4])
points(apply(Fixed5_ScR[[4]][,1:3],1,mean),type='b',pch=16,col=col[5])
points(apply(Fixed5_ScR[[5]][,1:4],1,mean),type='b',pch=16,col=col[6])
points(apply(Fixed5_ScR[[6]][,1:4],1,mean),type='b',pch=16,col=col[7])
points(Fixed5_ScR[[8]][,1],type='b',pch=16,col=col[9])
points(apply(Fixed5_ScR[[9]][,1:2],1,mean),type='b',pch=16,col=col[10])
points(apply(Fixed5_ScR[[10]][,1:3],1,mean),type='b',pch=16,col=col[11])
legend('bottomright',legend=c('Scenario 2','Scenario 3','Scenario 4','Scenario 5','Scenario 6','Scenario 8','Scenario 9','Scenario 10'),col=c(col[3:7],col[9:11]),pch=16,cex=0.8)
axis(1,1:10)
plot(Fixed10_ScR[[2]][,1],col=col[3],type='b',pch=16,ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=10, applied to sample size of 5-15',xaxt='n')
points(apply(Fixed10_ScR[[3]][,1:2],1,mean),type='b',pch=16,col=col[4])
points(apply(Fixed10_ScR[[4]][,1:3],1,mean),type='b',pch=16,col=col[5])
points(apply(Fixed10_ScR[[5]][,1:4],1,mean),type='b',pch=16,col=col[6])
points(apply(Fixed10_ScR[[6]][,1:4],1,mean),type='b',pch=16,col=col[7])
points(Fixed10_ScR[[8]][,1],type='b',pch=16,col=col[9])
points(apply(Fixed10_ScR[[9]][,1:2],1,mean),type='b',pch=16,col=col[10])
points(apply(Fixed10_ScR[[10]][,1:3],1,mean),type='b',pch=16,col=col[11])
legend('bottomright',legend=c('Scenario 2','Scenario 3','Scenario 4','Scenario 5','Scenario 6','Scenario 8','Scenario 9','Scenario 10'),col=c(col[3:7],col[9:11]),pch=16,cex=0.8)
axis(1,at=1:11,5:15)
plot(Fixed15_ScR[[2]][,1],col=col[3],type='b',pch=16,ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=15, applied to sample size of 10-20',xaxt='n')
points(apply(Fixed15_ScR[[3]][,1:2],1,mean),type='b',pch=16,col=col[4])
points(apply(Fixed15_ScR[[4]][,1:3],1,mean),type='b',pch=16,col=col[5])
points(apply(Fixed15_ScR[[5]][,1:4],1,mean),type='b',pch=16,col=col[6])
points(apply(Fixed15_ScR[[6]][,1:4],1,mean),type='b',pch=16,col=col[7])
points(Fixed15_ScR[[8]][,1],type='b',pch=16,col=col[9])
points(apply(Fixed15_ScR[[9]][,1:2],1,mean),type='b',pch=16,col=col[10])
points(apply(Fixed15_ScR[[10]][,1:3],1,mean),type='b',pch=16,col=col[11])
legend('bottomright',legend=c('Scenario 2','Scenario 3','Scenario 4','Scenario 5','Scenario 6','Scenario 8','Scenario 9','Scenario 10'),col=c(col[3:7],col[9:11]),pch=16,cex=0.8)
axis(1,at=1:11,10:20)
plot(Fixed20_ScR[[2]][,1],col=col[3],type='b',pch=16,ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=20, applied to sample size of 15-24',xaxt='n')
points(apply(Fixed20_ScR[[3]][,1:2],1,mean),type='b',pch=16,col=col[4])
points(apply(Fixed20_ScR[[4]][,1:3],1,mean),type='b',pch=16,col=col[5])
points(apply(Fixed20_ScR[[5]][,1:4],1,mean),type='b',pch=16,col=col[6])
points(apply(Fixed20_ScR[[6]][,1:4],1,mean),type='b',pch=16,col=col[7])
points(Fixed20_ScR[[8]][,1],type='b',pch=16,col=col[9])
points(apply(Fixed20_ScR[[9]][,1:2],1,mean),type='b',pch=16,col=col[10])
points(apply(Fixed20_ScR[[10]][,1:3],1,mean),type='b',pch=16,col=col[11])
legend('bottomright',legend=c('Scenario 2','Scenario 3','Scenario 4','Scenario 5','Scenario 6','Scenario 8','Scenario 9','Scenario 10'),col=c(col[3:7],col[9:11]),pch=16,cex=0.8)
axis(1,at=1:10,15:24)
plot(Fixed24_ScR[[2]][,1],col=col[3],type='b',pch=16,ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Calibrate based on n5=24, applied to sample size of 19-24',xaxt='n')
points(apply(Fixed24_ScR[[3]][,1:2],1,mean),type='b',pch=16,col=col[4])
points(apply(Fixed24_ScR[[4]][,1:3],1,mean),type='b',pch=16,col=col[5])
points(apply(Fixed24_ScR[[5]][,1:4],1,mean),type='b',pch=16,col=col[6])
points(apply(Fixed24_ScR[[6]][,1:4],1,mean),type='b',pch=16,col=col[7])
points(Fixed24_ScR[[8]][,1],type='b',pch=16,col=col[9])
points(apply(Fixed24_ScR[[9]][,1:2],1,mean),type='b',pch=16,col=col[10])
points(apply(Fixed24_ScR[[10]][,1:3],1,mean),type='b',pch=16,col=col[11])
legend('bottomright',legend=c('Scenario 2','Scenario 3','Scenario 4','Scenario 5','Scenario 6','Scenario 8','Scenario 9','Scenario 10'),col=c(col[3:7],col[9:11]),pch=16,cex=0.8)
axis(1,at=1:6,19:24)
mtext("Mean Power in Existing Baskets", side = 3, line = -1.5, outer = TRUE)
