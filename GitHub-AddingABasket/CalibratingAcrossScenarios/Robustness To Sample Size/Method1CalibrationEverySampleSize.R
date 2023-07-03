library(rjags)
library(textmineR)
library(matrixStats)
library(parallel)
library(MASS)

Method1Calibration <- function(n,p,K1,K2,q0,run,pw){
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
    id2 <- (p[m,(1+K1)]==q0)
    if(id2==1){id2 <- (1+K1)}
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
      nexmu <- rep(log(pw/(1-pw)),N) #NEX mu parameter
      nexsigma <- rep((1/pw)+(1/(1-pw)),N) #NEX sigma parameter
      mu <- log(q0/(1-q0))
      prob <- c(0.5,0.5) #Fixed weights
      # jags.data <- list('n'=n[1:K1],'y'=y[1:K1],'N'=K1,'nexmu'=nexmu,'nexsigma'=nexsigma,'prob'=prob,'MU'=mu)
      # jags.fit <- jags.model(file='EXNEX.txt',data=jags.data,n.adapt=1000,n.chains=1,quiet = T)
      # samplesEXNEX <- coda.samples(jags.fit,variable.names = c('p'),n.iter=M,silent=TRUE)
      # samplesEXNEX <- as.data.frame(samplesEXNEX[[1]])
      # pmat <- as.matrix(samplesEXNEX[,id1])
      # cut[j,id1] <- apply(pmat,2,Fun)
      jags.data <- list('n'=n[(K1+1):N],'y'=y[(K1+1:N)],'N'=K2,'mu'=mu)
      jags.fit <- jags.model(file='Ind.txt',data=jags.data,n.adapt=1000,n.chains=1,quiet=T)
      samplesEXNEX <- coda.samples(jags.fit,variable.names = c('p'),n.iter=M,silent=TRUE) #Fit the model
      samplesEXNEX <- as.data.frame(samplesEXNEX[[1]])
      pmat <- samplesEXNEX
      cut[j,id2] <- Fun(pmat)
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
K2 <- 1
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



Method1Cal <- mclapply(n_list,Method1Calibration,p,K1,K2,q0,run,pw,mc.cores=25)

save(Method1Cal,file='WorstCaseMethod1Calibration.RData')


load('WorstCaseMethod1Calibration.RData')
cut_off <- matrix(NA,nrow=24,ncol=5)
for(i in 1:24){
  cut_off[i,1:4] <- 0.903
  cut_off[i,5] <- quantile(Method1Cal[[i]]$`Combined Cut-Offs`[,5],0.9)
}
save(cut_off,file='Method1Delta.RData')

