library(rjags)
library(textmineR)
library(matrixStats)

Method1Calibration <- function(p,K1,K2,n,q0,run,pw){
  N <- K1+K2
  sc <- dim(p)[1]
  no.successes <- rep(0,N)
  true <- rep(0,N)
  for(l in 1:N){
    if(p[l]<=q0){
      true[l] <- 0
    }else{
      true[l] <- 1
    }
  }
  cutsc <- list()
  for(m in 1:sc){
    id1 <- which(p[m,1:K1]==q0)
    id2 <- which(p[m,(1+K1):(K1+K2)]==q0)
    id2 <- id2+K1
    cut <- matrix(NA,nrow=run,ncol=N)
    for(j in 1:run){
      for(i in 1:N){
        no.successes[i] <- rbinom(1,n[i],p[m,i])}
      M <- 10000
      Fun <- function(x){
        fun <- sum(x>q0)/M
        return(fun)
      }
      y <- no.successes
      nexmu <- rep(log(pw/(1-pw)),K1) #NEX mu parameter
      nexsigma <- rep((1/pw)+(1/(1-pw)),K1) #NEX sigma parameter
      mu <- log(q0/(1-q0))
      prob <- c(0.5,0.5) #Fixed weights
      jags.data <- list('n'=n[1:K1],'y'=y[1:K1],'N'=K1,'nexmu'=nexmu,'nexsigma'=nexsigma,'prob'=prob,'MU'=mu)
      jags.fit <- jags.model(file='EXNEX.txt',data=jags.data,n.adapt=1000,n.chains=1,quiet = T)
      samplesEXNEX <- coda.samples(jags.fit,variable.names = c('p'),n.iter=M,silent=TRUE)
      samplesEXNEX <- as.data.frame(samplesEXNEX[[1]])
      pmat <- as.matrix(samplesEXNEX[,id1])
      cut[j,id1] <- apply(pmat,2,Fun)
      nexmu <- rep(log(pw/(1-pw)),K2) #NEX mu parameter
      nexsigma <- rep((1/pw)+(1/(1-pw)),K2) #NEX sigma parameter
      mu <- log(q0/(1-q0))
      jags.data <- list('n'=n[(K1+1):N],'y'=y[(K1+1:N)],'N'=K2,'nexmu'=nexmu,'nexsigma'=nexsigma,'prob'=prob,'MU'=mu)
      jags.fit <- jags.model(file='EXNEX.txt',data=jags.data,n.adapt=1000,n.chains=1,quiet=T)
      samplesEXNEX <- coda.samples(jags.fit,variable.names = c('p'),n.iter=M,silent=TRUE) #Fit the model
      samplesEXNEX <- as.data.frame(samplesEXNEX[[1]])
      pmat <- as.matrix(samplesEXNEX[,id2-K1])
      cut[j,id2] <- apply(pmat,2,Fun)
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


K1 <- 2
K2 <- 2
n <- c(24,24,14,14)
q0 <- 0.2
q1 <- 0.4
run <- 5000
pw <- 0.3

p1 <- c(q0,q0,q0,q0)
p2 <- c(q1,q0,q0,q0)
p3 <- c(q1,q1,q0,q0)
p4 <- c(q1,q1,q1,q0)
p5 <- c(q0,q0,q1,q0)
p6 <- c(q1,q0,q1,q0)
p7 <- c(q0,q0,q1,q1)
p8 <- c(q1,q0,q1,q1)
p <- rbind(p1,p2,p3,p4,p5,p6,p7,p8)

Method1Delta <- Method1Calibration(p,K1,K2,n,q0,run,pw)
save(Method1Delta,file='Method1bDelta.RData')
