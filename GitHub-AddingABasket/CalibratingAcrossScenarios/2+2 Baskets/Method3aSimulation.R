library(rjags)
library(textmineR)
library(matrixStats)
library(parallel)
library(MASS)

Method3a <- function(p,K1,K2,run,n,q0,pw,cut.off){
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
K1 <- 2
K2 <- 2
K <- K1+K2
q0 <- 0.2
p <- rep(q0,K)
n <- c(rep(24,K1),rep(14,K2))
pw <- 0.3
run <- 10000

cut.off <- rep(0.9001,K)


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

Method3 <- mclapply(list_scenarios,Method3a,K1,K2,run,n,q0,pw,cut.off,mc.cores=10)

save(Method3,file='Method3AcrossScenariosSimulation152add2.RData')