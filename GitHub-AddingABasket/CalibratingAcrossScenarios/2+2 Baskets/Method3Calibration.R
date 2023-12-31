#Calibration of cut-off values under Method 3(a) where Delta is calibrated across scenarios

library(rjags)
library(textmineR)
library(matrixStats)

Method3Calibration <- function(p,K,n,q0,run,pw){
  N <- K
  sc <- dim(p)[1]
  no.successes <- rep(0,K)
  true <- rep(0,K)
  for(l in 1:K){
    if(p[l]<=q0){
      true[l] <- 0
    }else{
      true[l] <- 1
    }
  }
  cutsc <- list()
  for(m in 1:sc){
    id <- which(p[m,]==q0)
    cut <- matrix(NA,nrow=run,ncol=K)
    for(j in 1:run){
      for(i in 1:K){
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


K <- 4
n <- c(24,24,14,14)
q0 <- 0.2
q1 <- 0.4
run <- 5000
pw <- 0.3

#Data scenarios considered to calibrate across
p1 <- c(q0,q0,q0,q0)
p2 <- c(q1,q0,q0,q0)
p3 <- c(q1,q1,q0,q0)
p4 <- c(q1,q1,q1,q0)
p5 <- c(q0,q0,q1,q0)
p6 <- c(q1,q0,q1,q0)
p7 <- c(q0,q0,q1,q1)
p8 <- c(q1,q0,q1,q1)
p <- rbind(p1,p2,p3,p4,p5,p6,p7,p8)

Method3Delta <- Method3Calibration(p,K,n,q0,run,pw)
save(Method3Delta,file='Method3Delta.RData')
