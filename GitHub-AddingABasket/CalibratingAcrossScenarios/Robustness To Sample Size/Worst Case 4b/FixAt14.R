#Simulation Study where the cut-off values are calibrated based on the interim sample size of 14, with this cut-off value applied to sample sizes 50-200% of 14 (i.e. 7-28). The sample size which gave the most inflation 
#over this n5=14 value was then selected and calibrated for, simulation study repeated with this cut-off value. 

library(rjags)
library(textmineR)
library(matrixStats)
library(parallel)
library(MASS)

Method4b <- function(nDelta,p,K1,K2,run,q0,pw){
  n <- nDelta[1:K]
  cut.off <- nDelta[(K+1):(2*K)]
  cut.off.old <- cut.off[1]
  cut.off.new <- cut.off[K]
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
    nexmu <- rep(log(pw/(1-pw)),K) #NEX mu parameter
    nexsigma <- rep((1/pw)+(1/(1-pw)),K) #NEX sigma parameter
    mu <- log(q0/(1-q0))
    prob <- c(0.5,0.5) #Fixed weights
    jags.data <- list('n'=n,'y'=no.successes,'N'=K,'nexmu'=nexmu,'nexsigma'=nexsigma,'prob'=prob,'MU'=mu)
    jags.fit <- jags.model(file='EXNEX.txt',data=jags.data,n.adapt=1000,n.chains=4,quiet = T)
    samplesEXNEX <- coda.samples(jags.fit,variable.names = c('p'),n.iter=10000,silent=TRUE)
    samplesEXNEX <- as.data.frame(samplesEXNEX[[1]])
    Model.New <- as.matrix(samplesEXNEX[,1:K])
    point_estimates[j,1:K1] <- colMeans(Model.Old)
    point_estimates[j,(K1+1):K] <- colMeans(Model.New)[(K1+1):K]
    post_prob.Old <- apply(Model.Old,2,Fun)
    post_prob.New <- apply(Model.New,2,Fun)
    hypo[j,1:K1] <- as.integer(post_prob.Old>cut.off.old)
    hypo[j,(K1+1):K] <- as.integer(post_prob.New[(K1+1):K]>cut.off.new)
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


load('Method4Delta.RData')
cut.off <- cut_off[14,]

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
  Fix14[[i]] <- mclapply(nDelta_list,Method4b,pmat[i,],K1,K2,run,q0,pw,mc.cores=23)
}
save(Fix14,file='FixedAt14Applied7_28Method4.RData')

