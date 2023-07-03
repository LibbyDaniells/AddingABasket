library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(lattice)
library(ggpubr)

load('Method1TruthVaried15Sim1.RData')
Method1TruthVariedSim1 <- Method1TruthVaried
load('Method2TruthVaried15Sim1.RData')
Method2TruthVariedSim1 <- Method2TruthVaried
load('Method3TruthVaried15Sim1.RData')
Method3TruthVariedSim1 <- Method3TruthVaried
load('Method4TruthVaried15Sim1.RData')
Method4TruthVariedSim1 <- Method4TruthVaried

load('Method1TruthVaried15Sim2.RData')
Method1TruthVariedSim2 <- Method1TruthVaried
load('Method2TruthVaried15Sim2.RData')
Method2TruthVariedSim2 <- Method2TruthVaried
load('Method3TruthVaried15Sim2.RData')
Method3TruthVariedSim2 <- Method3TruthVaried
load('Method4TruthVaried15Sim2.RData')
Method4TruthVariedSim2 <- Method4TruthVaried

load('Method1TruthVaried15Sim3.RData')
Method1TruthVariedSim3 <- Method1TruthVaried
load('Method2TruthVaried15Sim3.RData')
Method2TruthVariedSim3 <- Method2TruthVaried
load('Method3TruthVaried15Sim3.RData')
Method3TruthVariedSim3 <- Method3TruthVaried
load('Method4TruthVaried15Sim3.RData')
Method4TruthVariedSim3 <- Method4TruthVaried

load('Method1TruthVaried15Sim4.RData')
Method1TruthVariedSim4 <- Method1TruthVaried
load('Method2TruthVaried15Sim4.RData')
Method2TruthVariedSim4 <- Method2TruthVaried
load('Method3TruthVaried15Sim4.RData')
Method3TruthVariedSim4 <- Method3TruthVaried
load('Method4TruthVaried15Sim4.RData')
Method4TruthVariedSim4 <- Method4TruthVaried


#Analysis-----------------------
H1 <- Method1TruthVariedSim4[[2]]$`Hypothesis Rejection`
H2 <- Method2TruthVariedSim4[[2]]$`Hypothesis Rejection`
H3 <- Method3TruthVariedSim4[[2]]$`Hypothesis Rejection`
H4 <- Method4TruthVariedSim4[[2]]$`Hypothesis Rejection`

Hypo <- list(H1,H2,H3,H4)

Truths <- Method1TruthVariedSim4[[2]]$`Truth Vector`
Responses <- Method1TruthVariedSim4[[2]]$Responses


#Overall Error Rates & Power
Reject <- rbind(colMeans(H1),colMeans(H2),colMeans(H3),colMeans(H4))

FWER <- c()
for(j in 1:4){
  rejection <- Hypo[[j]]
  fwer <- 0
  for(i in 1:10000){
    H <- rejection[i,]
    t <- as.numeric(Truths[i,]>0.2)
    id <- which(t==0)
    if(sum(H[id])!=0){
      fwer <- fwer+1
    }
  }
  FWER[j] <- fwer/10000
}

Perfect <- c()
for(j in 1:4){
  rejection <- Hypo[[j]]
  perfect <- 0
  for(i in 1:10000){
    AltNull <- as.numeric(Truths[i,]>0.2)
    if(all(rejection[i,]==AltNull)==T){
      perfect <- perfect+1
    }
  }
  Perfect[j] <- perfect/10000
}
DataSummary <- cbind(Reject,FWER,Perfect)*100
DataSummary


#Number of Differing Conclusions
No.Differing <- function(hypo1,hypo2){
  ID <- list()
  No.Differences <- c()
  How.Many.Differences <- c()
  Diff <- c()
  for(i in 1:5){
    ID[[i]] <- which(hypo1[,i]!=hypo2[,i])
    No.Differences[i] <- length(ID[[i]])
  }
  for(j in 1:length(hypo1[,1])){
    diffs <- 0
    for(k in 1:5){
      if(hypo1[j,k]!=hypo2[j,k]){
        diffs <- diffs+1
      }
    }
    How.Many.Differences[j] <- diffs
  }
  Total.Differences <- length(hypo1[,1])-length(which(How.Many.Differences==0))
  for(m in 1:6){
    Diff[m] <- length(which(How.Many.Differences==m-1))
  }
  return(list('Differences'=ID,'Basket Differences'=No.Differences,'Total Differences'=Total.Differences,'How Many Differences'=Diff))
}
H12 <- No.Differing(H1,H2)
H13 <- No.Differing(H1,H3)
H14 <- No.Differing(H1,H4)
H23 <- No.Differing(H2,H3)
H24 <- No.Differing(H2,H4)
H34 <- No.Differing(H3,H4)

Total.Differences <- c(H12$`Total Differences`,H13$`Total Differences`,H14$`Total Differences`,H23$`Total Differences`,H24$`Total Differences`,H34$`Total Differences`)
Basket.Differences <- rbind(H12$`Basket Differences`,H13$`Basket Differences`,H14$`Basket Differences`,H23$`Basket Differences`,H24$`Basket Differences`,H34$`Basket Differences`)

Total.Differences
Basket.Differences
rowMeans(Basket.Differences[,1:4])



#Which Method Is Correct
Method.Comparison <- function(hypo1,hypo2,truths){
  Correct.decision <- matrix(NA,nrow=5,ncol=2)
  for(j in 1:5){
    hypo1correct <- 0
    hypo2correct <- 0
    ID.Diff <- No.Differing(hypo1,hypo2)$Differences[[j]]
    if(length(ID.Diff)==0){
      hypo1correct <- NA
      hypo2correct <- NA
    }else{
    for(i in 1:length(ID.Diff)){
      data1 <- hypo1[ID.Diff[i],j]
      data2 <- hypo2[ID.Diff[i],j]
      p <- truths[ID.Diff[i],j]
      if(p<=0.2){
        if(data1==0){
          hypo1correct <- hypo1correct+1
        }else if(data2==0){
          hypo2correct <- hypo2correct+1
        }
      }else{
        if(data1==1){
          hypo1correct <- hypo1correct+1
        }else if(data2==1){
          hypo2correct <- hypo2correct+1
        }
      }
    }
    }
    Correct.decision[j,] <- c(hypo1correct,hypo2correct)
  }
  Correct.decision <- rbind(Correct.decision,colSums(Correct.decision,na.rm=T))
  return(Correct.decision)
}

Correct12 <- Method.Comparison(H1,H2,Truths)
Correct13 <- Method.Comparison(H1,H3,Truths)
Correct14 <- Method.Comparison(H1,H4,Truths)
Correct23 <- Method.Comparison(H2,H3,Truths)
Correct24 <- Method.Comparison(H2,H4,Truths)
Correct34 <- Method.Comparison(H3,H4,Truths)

c((Correct12[,1]/(Correct12[,1]+Correct12[,2])*100)[6],mean((Correct12[,1]/(Correct12[,1]+Correct12[,2])*100)[1:4]),(Correct12[,1]/(Correct12[,1]+Correct12[,2])*100)[5])
c((Correct13[,1]/(Correct13[,1]+Correct13[,2])*100)[6],mean((Correct13[,1]/(Correct13[,1]+Correct13[,2])*100)[1:4]),(Correct13[,1]/(Correct13[,1]+Correct13[,2])*100)[5])
c((Correct14[,1]/(Correct14[,1]+Correct14[,2])*100)[6],mean((Correct14[,1]/(Correct14[,1]+Correct14[,2])*100)[1:4]),(Correct14[,1]/(Correct14[,1]+Correct14[,2])*100)[5])
c((Correct23[,1]/(Correct23[,1]+Correct23[,2])*100)[6],mean((Correct23[,1]/(Correct23[,1]+Correct23[,2])*100)[1:4]),(Correct23[,1]/(Correct23[,1]+Correct23[,2])*100)[5])
c((Correct24[,1]/(Correct24[,1]+Correct24[,2])*100)[6],mean((Correct24[,1]/(Correct24[,1]+Correct24[,2])*100)[1:4]),(Correct24[,1]/(Correct24[,1]+Correct24[,2])*100)[5])
c((Correct34[,1]/(Correct34[,1]+Correct34[,2])*100)[6],mean((Correct34[,1]/(Correct34[,1]+Correct34[,2])*100)[1:4]),(Correct34[,1]/(Correct34[,1]+Correct34[,2])*100)[5])




Sim4b <- list('Overall'=DataSummary,'No.Differing'=list(H12,H13,H14,H23,H24,H34),'Which Correct'=list(Correct12,Correct13,Correct14,Correct23,Correct24,Correct34))
save(Sim4b,file='Simulation4bAnalysis.RData')

