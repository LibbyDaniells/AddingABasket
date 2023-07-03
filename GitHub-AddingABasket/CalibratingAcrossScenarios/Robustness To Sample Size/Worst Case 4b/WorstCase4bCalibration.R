library(rjags)
library(textmineR)
library(matrixStats)
library(parallel)
library(MASS)

Method4Calibration <- function(n,p,K1,q0,run,pw){
  N <- K1
  sc <- dim(p)[1]
  no.successes <- rep(0,K1)
  true <- rep(0,K1)
  for(l in 1:K1){
    if(p[l]<=q0){
      true[l] <- 0
    }else{
      true[l] <- 1
    }
  }
  cutsc <- list()
  for(m in 1:sc){
    id <- which(p[m,1:K1]==q0)
    cut <- matrix(NA,nrow=run,ncol=K1)
    for(j in 1:run){
      for(i in 1:K1){
        no.successes[i] <- rbinom(1,n[i],p[m,i])}
      M <- 10000
      Fun <- function(x){
        fun <- sum(x>q0)/M
        return(fun)
      }
      y <- no.successes
      nexmu <- rep(log(pw/(1-pw)),N) #NEX mu parameter
      nexsigma <- rep((1/pw)+(1/(1-pw)),N) #NEX sigma parameter
      mu <- log(q0/(1-q0))
      prob <- c(0.5,0.5) #Fixed weights
      jags.data <- list('n'=n,'y'=y,'N'=N,'nexmu'=nexmu,'nexsigma'=nexsigma,'prob'=prob,'MU'=mu)
      jags.fit <- jags.model(file='EXNEX.txt',data=jags.data,n.adapt=1000,n.chains=1,quiet = T)
      samplesEXNEX <- coda.samples(jags.fit,variable.names = c('p'),n.iter=M,silent=TRUE)
      samplesEXNEX <- as.data.frame(samplesEXNEX[[1]])
      pmat <- as.matrix(samplesEXNEX[,id])
      cut[j,id] <- apply(pmat,2,Fun)
      print(j)
    }
    cutsc[[m]] <- cut
  }
  cutmat <- c()
  for(t in 1:sc){
    cutmat <- rbind(cutmat,cutsc[[t]])
  }
  mylist <- list('Scenario Cut-offs'=cutsc,'Combined Cut-Offs'=cutmat)
  return(mylist)
}

K1 <- 4
n <- c(24,24,24,24)
q0 <- 0.2
q1 <- 0.4
run <- 5000
pw <- 0.3

n <- matrix(NA,nrow=24,ncol=5)
n_list <- list()
for(i in 1:24){
  n[i,] <- c(24,24,24,24,i)
  n_list[[i]] <- n[i,]
}



p1 <- c(q0,q0,q0,q0,q0)
p2 <- c(q1,q0,q0,q0,q0)
p3 <- c(q1,q1,q0,q0,q0)
p4 <- c(q1,q1,q1,q0,q0)
p5 <- c(q1,q1,q1,q1,q0)
p <- rbind(p1,p2,p3,p4,p5)



Method4Cal <- mclapply(n_list,Method4Calibration,p,K1,q0,run,pw,mc.cores=25)

save(Method4Cal,file='WorstCaseMethod4bCalibration.RData')


load('WorstCaseMethod4bCalibration.RData')
load('WorstCaseDelta1_5.RData')
Method3New <- cut_off

cut_off <- matrix(NA,nrow=24,ncol=5)
for(i in 1:24){
  cut_off[i,1:4] <- colQuantiles(Method4Cal[[i]]$`Combined Cut-Offs`,probs=0.9,na.rm=T)[4]
  cut_off[i,5] <- Method3New[i,5]
}
colQuantiles(Method4Cal[[1]]$`Combined Cut-Offs`,probs=0.9,na.rm=T)[4]
save(cut_off,file='Method4Delta.RData')
