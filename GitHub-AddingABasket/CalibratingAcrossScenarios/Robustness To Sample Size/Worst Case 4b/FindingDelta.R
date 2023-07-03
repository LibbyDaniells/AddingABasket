#Cut-Offs across 1-10
cut_off <- matrix(NA,nrow=24,ncol=5)
for(i in 1:24){
  cut_off[i,1:4] <- colQuantiles(Method3Cal[[i]]$`Combined Cut-Offs`,probs=0.9,na.rm=T)[4]
  cut_off[i,5] <- colQuantiles(Method3Cal[[i]]$`Combined Cut-Offs`,probs=0.9,na.rm=T)[5]
}
cut_off
save(cut_off,file='WorstCaseDelta1_10.RData')


par(mfrow=c(1,2))
plot(cut_off[,1],xaxt='n',ylab=expression(Delta[1]),xlab=expression(n[5]),main='Existing Baskets',pch=16,ylim=c(0.86,0.92))
axis(1,seq(1,24,2))
plot(cut_off[,5],xaxt='n',ylab=expression(Delta[5]),xlab=expression(n[5]),main='New Basket',pch=16,ylim=c(0.86,0.92))
axis(1,seq(1,24,2))

#Cut-Offs across 1-5
cut_off <- matrix(NA,nrow=24,ncol=5)
for(i in 1:24){
  Comb <- rbind(Method3Cal[[i]]$`Scenario Cut-offs`[[1]],Method3Cal[[i]]$`Scenario Cut-offs`[[2]],Method3Cal[[i]]$`Scenario Cut-offs`[[3]],Method3Cal[[i]]$`Scenario Cut-offs`[[4]],Method3Cal[[i]]$`Scenario Cut-offs`[[5]])
  cut_off[i,1:4] <- colQuantiles(Comb,probs=0.9,na.rm=T)[4]
  cut_off[i,5] <- colQuantiles(Comb,probs=0.9,na.rm=T)[5]
}
save(cut_off,file='WorstCaseDelta1_5.RData')

par(mfrow=c(1,2))
plot(cut_off[,1],xaxt='n',ylab=expression(Delta[1]),xlab=expression(n[5]),main='Existing Baskets',pch=16,ylim=c(0.86,0.92))
axis(1,seq(1,24,2))
plot(cut_off[,5],xaxt='n',ylab=expression(Delta[5]),xlab=expression(n[5]),main='New Basket',pch=16,ylim=c(0.86,0.92))
axis(1,seq(1,24,2))