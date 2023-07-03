#Simulation Study under Method 1(b) where we have E=2 existing and N=2 new baskets

library(rjags)
library(textmineR)
library(matrixStats)
library(parallel)
library(MASS)

Method1b <- function(p,K1,K2,run,n,q0,pw,cut.off.old,cut.off.new){
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
    nexmu <- rep(log(pw/(1-pw)),K2) #NEX mu parameter
    nexsigma <- rep((1/pw)+(1/(1-pw)),K2) #NEX sigma parameter
    jags.dataNew <- list('n'=n[(K1+1):K],'y'=no.successes[(K1+1):K],'N'=K2,'nexmu'=nexmu,'nexsigma'=nexsigma,'prob'=prob,'MU'=mu)
    jags.fitNew <- jags.model(file='EXNEX.txt',data=jags.dataNew,n.adapt=1000,n.chains=1,quiet=T)
    samplesInd <- coda.samples(jags.fitNew,variable.names = c('p'),n.iter=10000,silent=TRUE) #Fit the model
    samplesInd <- as.data.frame(samplesInd[[1]])
    Model.New <- as.matrix(samplesInd[,1:K2])
    point_estimates[j,1:K1] <- colMeans(Model.Old)
    point_estimates[j,(K1+1):K] <- colMeans(Model.New)
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




#Simulation
K1 <- 2
K2 <- 2
K <- K1+K2
q0 <- 0.2
p <- rep(q0,K)
n <- c(rep(24,K1),rep(14,K2))
pw <- 0.3
run <- 10000

cut.off.old <- 0.86725
cut.off.new <- 0.9002

p1 <- rep(q0,K)
p2 <- c(0.4,0.2,0.2,0.2)
p3 <- c(0.4,0.4,0.2,0.2)
p4 <- c(0.4,0.4,0.4,0.2)
p5 <- c(0.2,0.2,0.4,0.2)
p6 <- c(0.4,0.2,0.4,0.2)
p7 <- c(0.2,0.2,0.4,0.4)
p8 <- c(0.4,0.2,0.4,0.4)
p9 <- c(0.4,0.4,0.4,0.4)


list_scenarios <- list(p1,p2,p3,p4,p5,p6,p7,p8,p9)

Method1b <- mclapply(list_scenarios,Method1b,K1,K2,run,n,q0,pw,cut.off.old,cut.off.new,mc.cores=10)

save(Method1b,file='Method1bAcrossScenariosSimulation2add2.RData')
