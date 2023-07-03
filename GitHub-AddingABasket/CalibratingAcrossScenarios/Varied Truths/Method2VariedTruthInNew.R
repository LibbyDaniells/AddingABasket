library(rjags)
library(textmineR)
library(matrixStats)
library(parallel)
library(MASS)


VaryingTruthOC <- function(t,n,K,run,p.old,pw,q0,cut_off){
  Fun <- function(x){
    fun <- sum(x>q0)/length(x)
    return(fun)
  }
  point_estimates <- matrix(NA,nrow=run,ncol=K)
  hypo <- matrix(NA,nrow=run,ncol=K)
  response_store <- matrix(NA,nrow=run,ncol=K)
  truth_store <- matrix(NA,nrow=run,ncol=K)
  for(j in 1:run){
    set.seed(j)
    truth <- c()
    y <- c()
    for(i in 1:K){
      if(i==K){
        truth[i] <- runif(1,min=t[1],max=t[2])
        y[i] <- rbinom(1,n[i],truth[i])
      }else{
        truth[i] <- p.old[i]
        y[i] <- rbinom(1,n[i],p.old[i])
      }
    }
    response_store[j,] <- y
    truth_store[j,] <- round(truth,3)
    nexmu <- rep(log(pw/(1-pw)),K) #NEX mu parameter
    nexsigma <- rep((1/pw)+(1/(1-pw)),K) #NEX sigma parameter
    mu <- log(q0/(1-q0))
    prob <- c(0.5,0.5) #Fixed weights
    jags.data <- list('n'=n,'y'=y,'N'=K,'nexmu'=nexmu,'nexsigma'=nexsigma,'prob'=prob,'MU'=mu)
    jags.fit <- jags.model(file='EXNEX.txt',data=jags.data,n.adapt=1000,n.chains=1,quiet = T)
    samplesEXNEX <- coda.samples(jags.fit,variable.names = c('p'),n.iter=100000,silent=TRUE)
    samplesEXNEX <- as.data.frame(samplesEXNEX[[1]])
    pmat <- as.matrix(samplesEXNEX[,1:K])
    point_estimates[j,] <- colMeans(pmat)
    post_prob <- apply(pmat,2,Fun)
    hypo[j,] <- as.integer(post_prob>cut_off)
  }
  output <- list('Truth Vector'=truth_store,'Responses'=response_store,'Point Estimates'=point_estimates,'Hypothesis Rejection'=hypo)
  return(output)
}


n <- c(24,24,24,24,14)
K <- 5
run <- 10000
t1 <- c(0.2,0.3)
t2 <- c(0.4,0.5)
t3 <- c(0.1,0.2)
p.old <- rep(0.2,4)
pw <- 0.3
q0 <- 0.2
cut_off <- rep(0.9056,K)

t_scenarios <- list(t1,t2,t3)
Method2TruthVaried <- mclapply(t_scenarios,VaryingTruthOC,n,K,run,p.old,pw,q0,cut_off,mc.cores=8)
save(Method2TruthVaried,file='Method2TruthVaried15Sim1.RData')

p.old <- rep(0.4,4)
Method2TruthVaried <- mclapply(t_scenarios,VaryingTruthOC,n,K,run,p.old,pw,q0,cut_off,mc.cores=8)
save(Method2TruthVaried,file='Method2TruthVaried15Sim2.RData')

p.old <- c(0.4,0.4,0.2,0.2)
Method2TruthVaried <- mclapply(t_scenarios,VaryingTruthOC,n,K,run,p.old,pw,q0,cut_off,mc.cores=8)
save(Method2TruthVaried,file='Method2TruthVaried15Sim3.RData')

p.old <- c(0.4,0.3,0.3,0.2)
Method2TruthVaried <- mclapply(t_scenarios,VaryingTruthOC,n,K,run,p.old,pw,q0,cut_off,mc.cores=8)
save(Method2TruthVaried,file='Method2TruthVaried15Sim4.RData')