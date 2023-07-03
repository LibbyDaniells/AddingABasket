#Calibration and simulation study under fixed data scenarios for Method 1 where cut-off values, Delta, are calibrated under a global null scenario to achieve 10% type I error rate. 

library(rjags)
library(textmineR)
library(matrixStats)

EXNEX <- function(pw,No.Baskets,q0,n,y){
  nexmu <- rep(log(pw/(1-pw)),No.Baskets) #NEX mu parameter
  nexsigma <- rep((1/pw)+(1/(1-pw)),No.Baskets) #NEX sigma parameter
  mu <- log(q0/(1-q0))
  prob <- c(0.5,0.5) #Fixed weights
  jags.data <- list('n'=n,'y'=y,'N'=No.Baskets,'nexmu'=nexmu,'nexsigma'=nexsigma,'prob'=prob,'MU'=mu)
  jags.fit <- jags.model(file='EXNEX.txt',data=jags.data,n.adapt=1000,n.chains=4,quiet = T)
  samplesEXNEX <- coda.samples(jags.fit,variable.names = c('p'),n.iter=100000,silent=TRUE)
  samplesEXNEX <- as.data.frame(samplesEXNEX[[1]])
  pmat <- as.matrix(samplesEXNEX[,1:No.Baskets])
  return(pmat)
}


Independent <- function(No.Baskets,q0,n,y){
  mu <- log(q0/(1-q0))
  jags.data <- list('n'=n,'y'=y,'N'=No.Baskets,'mu'=mu)
  jags.fit <- jags.model(file='Ind.txt',data=jags.data,n.adapt=1000,n.chains=1,quiet=T)
  samplesInd <- coda.samples(jags.fit,variable.names = c('p'),n.iter=100000,silent=TRUE) #Fit the model
  samplesInd <- as.data.frame(samplesInd[[1]])
  pmat <- as.matrix(samplesInd[,1:No.Baskets])
  return(pmat)
}

#Calibrate under the global null to achieve a 10% type I error rate (traditional approach)

Method1aCalibration <- function(K1,K2,p,n,q0,run,pw){
  K <- K1+K2
  no.successes <- rep(0,K)
  cut <- matrix(,nrow=run,ncol=K)
  true <- as.numeric(p>q0)
  Fun <- function(x){
    fun <- sum(x>q0)/length(x)
    return(fun)
  }
  for(j in 1:run){
    set.seed(j)
    for(i in 1:K){
      no.successes[i] <- rbinom(1,n[i],p[i])
    }
    Model.Old <- EXNEX(pw,K1,q0,n[1:K1],no.successes[1:K1])
    Model.New <- Independent(K2,q0,n[(K1+1):K],no.successes[(K1+1):K])
    post_prob.Old <- apply(Model.Old,2,Fun)
    post_prob.New <- apply(Model.New,2,Fun)
    cut[j,] <- c(post_prob.Old,post_prob.New)
    print(j)
  }
  cut_off <- colQuantiles(cut,probs=0.9) #Calibrating to ensure 10% type I error under the null
  return(cut_off)
}

Method1a <- function(K1,K2,run,n,p,q0,pw,cut.off.old,cut.off.new){
  K <- K1+K2
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
    nexmu <- rep(log(pw/(1-pw)),K1) #NEX mu parameter
    nexsigma <- rep((1/pw)+(1/(1-pw)),K1) #NEX sigma parameter
    mu <- log(q0/(1-q0))
    prob <- c(0.5,0.5) #Fixed weights
    jags.data <- list('n'=n[1:K1],'y'=no.successes[1:K1],'N'=K1,'nexmu'=nexmu,'nexsigma'=nexsigma,'prob'=prob,'MU'=mu)
    jags.fit <- jags.model(file='EXNEX.txt',data=jags.data,n.adapt=1000,n.chains=4,quiet = T)
    samplesEXNEX <- coda.samples(jags.fit,variable.names = c('p'),n.iter=10000,silent=TRUE)
    samplesEXNEX <- as.data.frame(samplesEXNEX[[1]])
    Model.Old <- as.matrix(samplesEXNEX[,1:K1])
    jags.dataNew <- list('n'=n[(K1+1):K],'y'=no.successes[(K1+1):K],'N'=K2,'mu'=mu)
    jags.fitNew <- jags.model(file='Ind.txt',data=jags.dataNew,n.adapt=1000,n.chains=1,quiet=T)
    samplesInd <- coda.samples(jags.fitNew,variable.names = c('p'),n.iter=10000,silent=TRUE) #Fit the model
    samplesInd <- as.data.frame(samplesInd[[1]])
    Model.New <- as.matrix(samplesInd[,1:K2])
    point_estimates[j,1:K1] <- round(colMeans(Model.Old),3)
    point_estimates[j,(K1+1):K] <- round(colMeans(Model.New),3)
    post_prob.Old <- apply(Model.Old,2,Fun)
    post_prob.New <- apply(Model.New,2,Fun)
    hypo[j,1:K1] <- as.integer(post_prob.Old>cut.off.old)
    hypo[j,(K1+1):K] <- as.integer(post_prob.New>cut.off.new)
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



#Calibration
K1 <- 4
K2 <- 1
K <- K1+K2
q0 <- 0.2
p <- rep(q0,K)
n <- c(rep(24,K1),rep(14,K2))
pw <- 0.3
run <- 10000
# M1aDelta <- Method1aCalibration(K1,K2,p,n,q0,run,pw)
# 
# print(M1aDelta)
# save(M1aDelta,file='Method1a_Paper_Delta.RData')

#Simulation
K1 <- 4
K2 <- 1
K <- K1+K2
q0 <- 0.2
p <- rep(q0,K)
n <- c(rep(24,K1),rep(14,K2))
pw <- 0.3
run <- 10000

#load('Method1a_Paper_Delta.RData')
#cut.off.old <- mean(M1aDelta[1:K1])
cut.off.old <- 0.8598923
#cut.off.new <- mean(M1aDelta[(K1+1):(K1+K2)])
cut.off.new <- 0.899811

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
p11 <- c(0.3,0.2,0.2,0.2,0.2)
p12 <- c(0.3,0.3,0.2,0.2,0.2)
p13 <- c(0.3,0.2,0.2,0.2,0.3)
p14 <- c(0.3,0.3,0.2,0.2,0.3)
p15 <- c(0.4,0.3,0.2,0.2,0.3)
p16 <- c(0.4,0.3,0.3,0.2,0.3)
pmat <- rbind(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16)

# Method1aSimulation <- c()
# for(i in 1:dim(pmat)[1]){
#   Simulation1a <- Method1a(K1,K2,run,n,pmat[i,],q0,pw,cut.off.old,cut.off.new)
#   Method1aSimulation <- cbind(Method1aSimulation,Simulation1a)
#   print(i)
# }
# Method1aSimulation
# save(Method1aSimulation,file='Method1_Paper_Simulation.RData')

Sc1 <- Method1a(K1,K2,run,n,pmat[1,],q0,pw,cut.off.old,cut.off.new)
save(Sc1,file='Method1aScenario1.RData')
Sc2 <- Method1a(K1,K2,run,n,pmat[2,],q0,pw,cut.off.old,cut.off.new)
save(Sc2,file='Method1aScenario2.RData')
Sc3 <- Method1a(K1,K2,run,n,pmat[3,],q0,pw,cut.off.old,cut.off.new)
save(Sc3,file='Method1aScenario3.RData')
Sc4 <- Method1a(K1,K2,run,n,pmat[4,],q0,pw,cut.off.old,cut.off.new)
save(Sc4,file='Method1aScenario4.RData')
Sc5 <- Method1a(K1,K2,run,n,pmat[5,],q0,pw,cut.off.old,cut.off.new)
save(Sc5,file='Method1aScenario5.RData')
Sc6 <- Method1a(K1,K2,run,n,pmat[6,],q0,pw,cut.off.old,cut.off.new)
save(Sc6,file='Method1aScenario6.RData')
Sc7 <- Method1a(K1,K2,run,n,pmat[7,],q0,pw,cut.off.old,cut.off.new)
save(Sc7,file='Method1aScenario7.RData')
Sc8 <- Method1a(K1,K2,run,n,pmat[8,],q0,pw,cut.off.old,cut.off.new)
save(Sc8,file='Method1aScenario8.RData')
Sc9 <- Method1a(K1,K2,run,n,pmat[9,],q0,pw,cut.off.old,cut.off.new)
save(Sc9,file='Method1aScenario9.RData')
Sc10 <- Method1a(K1,K2,run,n,pmat[10,],q0,pw,cut.off.old,cut.off.new)
save(Sc10,file='Method1aScenario10.RData')
Sc11 <- Method1a(K1,K2,run,n,pmat[11,],q0,pw,cut.off.old,cut.off.new)
save(Sc11,file='Method1aScenario11.RData')
Sc12 <- Method1a(K1,K2,run,n,pmat[12,],q0,pw,cut.off.old,cut.off.new)
save(Sc12,file='Method1aScenario12.RData')
Sc13 <- Method1a(K1,K2,run,n,pmat[13,],q0,pw,cut.off.old,cut.off.new)
save(Sc13,file='Method1aScenario13.RData')
Sc14 <- Method1a(K1,K2,run,n,pmat[14,],q0,pw,cut.off.old,cut.off.new)
save(Sc14,file='Method1aScenario14.RData')
Sc15 <- Method1a(K1,K2,run,n,pmat[15,],q0,pw,cut.off.old,cut.off.new)
save(Sc15,file='Method1aScenario15.RData')
Sc16 <- Method1a(K1,K2,run,n,pmat[16,],q0,pw,cut.off.old,cut.off.new)
save(Sc16,file='Method1aScenario16.RData')

Method1 <- cbind(Sc1,Sc2,Sc3,Sc4,Sc5,Sc6,Sc7,Sc8,Sc9,Sc10,Sc11,Sc12,Sc13,Sc14,Sc15,Sc16)
save(Method1,file='Method1PaperSimulation.RData')
