library(rjags)
library(textmineR)
library(matrixStats)
library(parallel)
library(MASS)

Method3a <- function(nDelta,p,K1,K2,run,q0,pw){
  K <- K1+K2
  n <- nDelta[1:K]
  cut.off <- nDelta[(K+1):(2*K)]
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
K1 <- 4
K2 <- 1
K <- K1+K2
q0 <- 0.2
p <- rep(q0,K)
n <- c(rep(24,K1),rep(14,K2))
pw <- 0.3
run <- 5000


load('WorstCaseDelta1_5.RData')

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
for(i in 1:24){
  nDelta_list[[i]] <- c(24,24,24,24,i,cut_off[i,])
}

# TryEveryn5 <- list()
# for(i in 1:10){
#   TryEveryn5[[i]] <- mclapply(nDelta_list,Method3a,pmat[i,],K1,K2,run,q0,p2,mc.cores=60)
# }
# save(TryEveryn5,file='TryEveryn5ValueSim.RData')
# 

load('TryEveryn5ValueSim.RData')

Sc1 <- matrix(NA,ncol=5,nrow=24)
Sc2 <- matrix(NA,ncol=5,nrow=24)
Sc3 <- matrix(NA,ncol=5,nrow=24)
Sc4 <- matrix(NA,ncol=5,nrow=24)
Sc5 <- matrix(NA,ncol=5,nrow=24)
Sc6 <- matrix(NA,ncol=5,nrow=24)
Sc7 <- matrix(NA,ncol=5,nrow=24)
Sc8 <- matrix(NA,ncol=5,nrow=24)
Sc9 <- matrix(NA,ncol=5,nrow=24)
Sc10 <- matrix(NA,ncol=5,nrow=24)
for(i in 1:24){
  Sc1[i,] <- TryEveryn5[[1]][[i]]$`Rejection Percentage`*100
  Sc2[i,] <- TryEveryn5[[2]][[i]]$`Rejection Percentage`*100
  Sc3[i,] <- TryEveryn5[[3]][[i]]$`Rejection Percentage`*100
  Sc4[i,] <- TryEveryn5[[4]][[i]]$`Rejection Percentage`*100
  Sc5[i,] <- TryEveryn5[[5]][[i]]$`Rejection Percentage`*100
  Sc6[i,] <- TryEveryn5[[6]][[i]]$`Rejection Percentage`*100
  Sc7[i,] <- TryEveryn5[[7]][[i]]$`Rejection Percentage`*100
  Sc8[i,] <- TryEveryn5[[8]][[i]]$`Rejection Percentage`*100
  Sc9[i,] <- TryEveryn5[[9]][[i]]$`Rejection Percentage`*100
  Sc10[i,] <- TryEveryn5[[10]][[i]]$`Rejection Percentage`*100
}
scenario <- list(Sc1,Sc2,Sc3,Sc4,Sc5,Sc6,Sc7,Sc8,Sc9,Sc10)

par(mfrow=c(2,2))
col <- c(brewer.pal(6,'Set3'),brewer.pal(5,'Set1'))
plot(scenario[[1]][,5],xaxt='n',type='b',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Change in Error Rate in New Basket as n\u2085 Changes')
for(i in 2:5){
  points(scenario[[i]][,5],type='b',pch=16,col=col[i+1])
}
axis(1,seq(1,24,2))
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 5'),col=c('black',col[3:6]),pch=16,cex=0.8)


plot(scenario[[6]][,5],col=col[7],xaxt='n',type='b',pch=16,ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Change in Power in New Basket as n\u2085 Changes')
for(i in 7:10){
  points(scenario[[i]][,5],type='b',pch=16,col=col[i+1])
}
axis(1,seq(1,24,2))
legend('bottomright',legend=c('Scenario 6','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c(col[7:11]),pch=16,cex=0.8)

plot(apply(scenario[[1]][,1:4],1,mean),type='b',xaxt='n',pch=16,ylim=c(0,50),ylab='% Reject',xlab=expression(n[5]),main='Mean Change in Error Rate in Existing Baskets as n\u2085 Changes')
axis(1,seq(1,24,2))
points(apply(scenario[[2]][,2:4],1,mean),col=col[3],pch=16,type='b')
points(apply(scenario[[3]][,3:4],1,mean),col=col[4],pch=16,type='b')
points(scenario[[4]][,4],col=col[5],pch=16,type='b')
points(apply(scenario[[7]][,1:4],1,mean),col=col[8],pch=16,type='b')
points(apply(scenario[[8]][,2:4],1,mean),col=col[9],pch=16,type='b')
points(apply(scenario[[9]][,3:4],1,mean),col=col[10],pch=16,type='b')
points(scenario[[10]][,4],col=col[11],pch=16,type='b')
legend('topright',legend=c('Scenario 1','Scenario 2','Scenario 3','Scenario 4','Scenario 7','Scenario 8','Scenario 9','Scenario 10'),col=c('black',col[3:5],col[8:11]),pch=16,cex=0.8)

plot(scenario[[2]][,1],typ='b',xaxt='n',pch=16,col=col[3],ylim=c(0,100),ylab='% Reject',xlab=expression(n[5]),main='Mean Change in Power in Existing Baskets as n\u2085 Changes')
points(apply(scenario[[3]][,1:2],1,mean),col=col[4],pch=16,type='b')
points(apply(scenario[[4]][,1:3],1,mean),col=col[5],pch=16,type='b')
points(apply(scenario[[5]][,1:4],1,mean),col=col[6],pch=16,type='b')
points(apply(scenario[[6]][,1:4],1,mean),col=col[7],pch=16,type='b')
points(scenario[[8]][,1],col=col[9],pch=16,type='b')
points(apply(scenario[[9]][,1:2],1,mean),col=col[10],pch=16,type='b')
points(apply(scenario[[10]][,1:3],1,mean),col=col[11],pch=16,type='b')
legend('bottomright',legend=c('Scenario 2','Scenario 3','Scenario 4','Scenario 5','Scenario 6','Scenario 8','Scenario 9','Scenario 10'),col=c(col[3:7],col[9:11]),pch=16,cex=0.8)
axis(1,seq(1,24,2))


#Making the plots in ggplot
newerror <- c(scenario[[1]][,5],scenario[[2]][,5],scenario[[3]][,5],scenario[[4]][,5],scenario[[5]][,5])
newpower <- c(scenario[[6]][,5],scenario[[7]][,5],scenario[[8]][,5],scenario[[9]][,5],scenario[[10]][,5])
existingerror <- c(apply(scenario[[1]][,1:4],1,mean),apply(scenario[[2]][,2:4],1,mean),apply(scenario[[3]][,3:4],1,mean),scenario[[4]][,4],apply(scenario[[7]][,1:4],1,mean),apply(scenario[[8]][,2:4],1,mean),apply(scenario[[9]][,3:4],1,mean),scenario[[10]][,4])
existingpower <- c(scenario[[2]][,1],apply(scenario[[3]][,1:2],1,mean),apply(scenario[[4]][,1:3],1,mean),apply(scenario[[5]][,1:4],1,mean),apply(scenario[[6]][,1:4],1,mean),scenario[[8]][,1],apply(scenario[[9]][,1:2],1,mean),apply(scenario[[10]][,1:3],1,mean))

colorpal <- brewer.pal(11,'Set3')
colorpal <- colorpal[-2]

#New Error
n5NE <- rep(seq(1,24,1),5)
NEData <- data.frame('Scenario'=c(rep('Sc1',24),rep('Sc2',24),rep('Sc3',24),rep('Sc4',24),rep('Sc5',24)),'Sample Size'=n5NE,'Error Rate'=newerror)
NEData$Scenario <- factor(NEData$Scenario,levels=c('Sc1','Sc2','Sc3','Sc4','Sc5'))

NewErrorPlot <- ggplot(NEData,aes(col=Scenario,x=n5NE,y=newerror))+geom_line()+geom_point()+
  scale_color_manual(values=colorpal[1:5])+
  theme_minimal()+geom_hline(yintercept=10,linetype='dashed')+
  scale_y_continuous(limits=c(0,30))+
  labs(y='Type I Error Rate',x='n\u2085')+
  ggtitle('Type I Error Rate in New Basket')+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "",axis.title.x = element_blank())

#New Power
n5NP <- rep(seq(1,24,1),5)
NPData <- data.frame('Scenario'=c(rep('Sc6',24),rep('Sc7',24),rep('Sc8',24),rep('Sc9',24),rep('Sc10',24)),'Sample Size'=n5NP,'Power'=newpower)
NPData$Scenario <- factor(NPData$Scenario,levels=c('Sc6','Sc7','Sc8','Sc9','Sc10'))

NewPowerPlot <- ggplot(NPData,aes(col=Scenario,x=n5NP,y=newpower))+geom_line()+geom_point()+
  scale_color_manual(values=colorpal[6:10])+
  theme_minimal()+geom_hline(yintercept=80,linetype='dashed')+
  scale_y_continuous(limits=c(0,100))+
  labs(y='Power',x='n\u2085')+
  ggtitle('Power in New Basket')+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "",axis.title.x = element_blank())

#Existing Error
n5EE <- rep(seq(1,24,1),8)
EEData <- data.frame('Scenario'=c(rep('Sc1',24),rep('Sc2',24),rep('Sc3',24),rep('Sc4',24),rep('Sc7',24),rep('Sc8',24),rep('Sc9',24),rep('Sc10',24)),'Sample Size'=n5EE,'Errpr'=existingerror)
EEData$Scenario <- factor(EEData$Scenario,levels=c('Sc1','Sc2','Sc3','Sc4','Sc7','Sc8','Sc9','Sc10'))

ExistingErrorPlot <- ggplot(EEData,aes(col=Scenario,x=n5EE,y=existingerror))+geom_line()+geom_point()+
  scale_color_manual(values=c(colorpal[1:4],colorpal[7:10]))+
  theme_minimal()+geom_hline(yintercept=10,linetype='dashed')+
  scale_y_continuous(limits=c(0,30))+
  labs(y='Mean Type I Error Rate',x='n\u2085')+
  ggtitle('Mean Type I Error Rate in Existing Baskets')+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "",axis.title.x = element_blank())

#Existing Power
n5EP <- rep(seq(1,24,1),8)
EPData <- data.frame('Scenario'=c(rep('Sc2',24),rep('Sc3',24),rep('Sc4',24),rep('Sc5',24),rep('Sc6',24),rep('Sc8',24),rep('Sc9',24),rep('Sc10',24)),'Sample Size'=n5EP,'Power'=existingpower)
EPData$Scenario <- factor(EPData$Scenario,levels=c('Sc2','Sc3','Sc4','Sc5','Sc6','Sc8','Sc9','Sc10'))

ExistingPowerPlot <- ggplot(EPData,aes(col=Scenario,x=n5EP,y=existingpower))+geom_line()+geom_point()+
  scale_color_manual(values=c(colorpal[2:6],colorpal[8:10]))+
  theme_minimal()+geom_hline(yintercept=80,linetype='dashed')+
  scale_y_continuous(limits=c(0,100))+
  labs(y='Mean Power',x='n\u2085')+
  ggtitle('Mean Power in Existing Baskets')+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "",axis.title.x = element_blank())



legendd <- c()
for(i in 1:10){
  legendd <- c(legendd,scenario[[1]][,5])
}
n5L <- rep(seq(1,24,1),10) 
LData <- data.frame('Scenario'=c(rep('Sc1',24),rep('Sc2',24),rep('Sc3',24),rep('Sc4',24),rep('Sc5',24),rep('Sc6',24),rep('Sc7',24),rep('Sc8',24),rep('Sc9',24),rep('Sc10',24)),'Sample Size'=n5L,'Power'=legendd)
LData$Scenario <- factor(LData$Scenario,levels=c('Sc1','Sc2','Sc3','Sc4','Sc5','Sc6','Sc7','Sc8','Sc9','Sc10'))

LegendPlot <- ggplot(LData,aes(col=Scenario,x=n5L,y=legendd))+geom_line()+geom_point()+
  scale_color_manual(values=colorpal)+
  theme_minimal()+geom_hline(yintercept=80,linetype='dashed')+
  scale_y_continuous(limits=c(0,100))+
  labs(y='Mean Power',x='n\u2085')+
  ggtitle('Mean Power in Existing Baskets')+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "right",axis.title.x = element_blank())
legend <- cowplot::get_legend(LegendPlot)

ggarrange(NewErrorPlot,NewPowerPlot,legend,ExistingErrorPlot,ExistingPowerPlot,NULL,nrow=2,ncol=3,widths = c(1, 1, 0.1))

