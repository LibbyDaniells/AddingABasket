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


Method3aCalibration <- function(K1,K2,p,n,q0,run,pw){
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
    Model <- EXNEX(pw,K,q0,n,no.successes)
    post_prob <- apply(Model,2,Fun)
    cut[j,] <- post_prob
    print(j)
  }
  cut_off <- colQuantiles(cut,probs=0.9) #Calibrating to ensure 10% type I error under the null
  return(cut_off)
}

Method3a <- function(K1,K2,run,n,p,q0,pw,cut.off){
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
    nexmu <- rep(log(pw/(1-pw)),K) #NEX mu parameter
    nexsigma <- rep((1/pw)+(1/(1-pw)),K) #NEX sigma parameter
    mu <- log(q0/(1-q0))
    prob <- c(0.5,0.5) #Fixed weights
    jags.data <- list('n'=n,'y'=no.successes,'N'=K,'nexmu'=nexmu,'nexsigma'=nexsigma,'prob'=prob,'MU'=mu)
    jags.fit <- jags.model(file='EXNEX.txt',data=jags.data,n.adapt=1000,n.chains=4,quiet = T)
    samplesEXNEX <- coda.samples(jags.fit,variable.names = c('p'),n.iter=10000,silent=TRUE)
    samplesEXNEX <- as.data.frame(samplesEXNEX[[1]])
    Model <- as.matrix(samplesEXNEX[,1:K])
    point_estimates[j,] <- round(colMeans(Model),3)
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



#Calibration
K1 <- 4
K2 <- 1
K <- K1+K2
q0 <- 0.2
p <- rep(q0,K)
n <- c(rep(24,K1),rep(14,K2))
pw <- 0.3
run <- 10000
#M3aDelta <- Method3aCalibration(K1,K2,p,n,q0,run,pw)
# 
# print(M3aDelta)
# save(M3aDelta,file='Method3a_Paper_Delta.RData')

#Simulation
K1 <- 4
K2 <- 1
K <- K1+K2
q0 <- 0.2
p <- rep(q0,K)
n <- c(rep(24,K1),rep(14,K2))
pw <- 0.3
run <- 10000

# load('Method3a_Paper_Delta.RData')
# cut.off <- M3aDelta
cut.off <- c(rep(0.8566457,K1),0.8409265)


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

# Method3aSimulation <- c()
# for(i in 1:dim(pmat)[1]){
#   Simulation3a <- Method3a(K1,K2,run,n,pmat[i,],q0,pw,cut.off)
#   Method3aSimulation <- cbind(Method3aSimulation,Simulation3a)
#   print(i)
# }
# Method3aSimulation
# save(Method3aSimulation,file='Method3a_Paper_Simulation.RData')


Sc1 <- Method3a(K1,K2,run,n,pmat[1,],q0,pw,cut.off)
save(Sc1,file='Method3aScenario1.RData')
Sc2 <- Method3a(K1,K2,run,n,pmat[2,],q0,pw,cut.off)
save(Sc2,file='Method3aScenario2.RData')
Sc3 <- Method3a(K1,K2,run,n,pmat[3,],q0,pw,cut.off)
save(Sc3,file='Method3aScenario3.RData')
Sc4 <- Method3a(K1,K2,run,n,pmat[4,],q0,pw,cut.off)
save(Sc4,file='Method3aScenario4.RData')
Sc5 <- Method3a(K1,K2,run,n,pmat[5,],q0,pw,cut.off)
save(Sc5,file='Method3aScenario5.RData')
Sc6 <- Method3a(K1,K2,run,n,pmat[6,],q0,pw,cut.off)
save(Sc6,file='Method3aScenario6.RData')
Sc7 <- Method3a(K1,K2,run,n,pmat[7,],q0,pw,cut.off)
save(Sc7,file='Method3aScenario7.RData')
Sc8 <- Method3a(K1,K2,run,n,pmat[8,],q0,pw,cut.off)
save(Sc8,file='Method3aScenario8.RData')
Sc9 <- Method3a(K1,K2,run,n,pmat[9,],q0,pw,cut.off)
save(Sc9,file='Method3aScenario9.RData')
Sc10 <- Method3a(K1,K2,run,n,pmat[10,],q0,pw,cut.off)
save(Sc10,file='Method3aScenario10.RData')
Sc11 <- Method3a(K1,K2,run,n,pmat[11,],q0,pw,cut.off)
save(Sc11,file='Method3aScenario11.RData')
Sc12 <- Method3a(K1,K2,run,n,pmat[12,],q0,pw,cut.off)
save(Sc12,file='Method3aScenario12.RData')
Sc13 <- Method3a(K1,K2,run,n,pmat[13,],q0,pw,cut.off)
save(Sc13,file='Method3aScenario13.RData')
Sc14 <- Method3a(K1,K2,run,n,pmat[14,],q0,pw,cut.off)
save(Sc14,file='Method3aScenario14.RData')
Sc15 <- Method3a(K1,K2,run,n,pmat[15,],q0,pw,cut.off)
save(Sc15,file='Method3aScenario15.RData')
Sc16 <- Method3a(K1,K2,run,n,pmat[16,],q0,pw,cut.off)
save(Sc16,file='Method3aScenario16.RData')

Method3a <- cbind(Sc1,Sc2,Sc3,Sc4,Sc5,Sc6,Sc7,Sc8,Sc9,Sc10,Sc11,Sc12,Sc13,Sc14,Sc15,Sc16)
save(Method3a,file='Method3aPaperSimulation')