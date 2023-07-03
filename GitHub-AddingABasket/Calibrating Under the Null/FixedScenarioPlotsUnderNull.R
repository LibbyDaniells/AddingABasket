library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(lattice)
library(ggpubr)


load('Method1PaperSimulation.RData')
load('Method2PaperSimulation.RData')
load('Method3aPaperSimulation.RData')
load('Method4aPaperSimulation.RData')

col <- brewer.pal(9,name='Blues')
col2 <- brewer.pal(9,name='Greys')

K <- 5
Method <- c(rep("Method 1" , K),rep('Method 2',K),rep('Method 3(a)',K),rep('Method 4(a)',K))
Method <- factor(Method,levels=c('Method 1','Method 2','Method 3(a)','Method 4(a)'))
Basket <- rep(as.factor(1:K) , 4)

k <- 1
Reject1 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data1 <- data.frame(Method,Basket,'% Reject'=Reject1)
k <- 2
Reject2 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data2 <- data.frame(Method,Basket,'% Reject'=Reject2)
k <- 3
Reject3 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data3 <- data.frame(Method,Basket,'% Reject'=Reject3)
k <- 4
Reject4 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data4 <- data.frame(Method,Basket,'% Reject'=Reject4)
k <- 5
Reject5 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data5 <- data.frame(Method,Basket,'% Reject'=Reject5)
k <- 6
Reject6 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data6 <- data.frame(Method,Basket,'% Reject'=Reject6)
k <- 7
Reject7 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data7 <- data.frame(Method,Basket,'% Reject'=Reject7)
k <- 8
Reject8 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data8 <- data.frame(Method,Basket,'% Reject'=Reject8)
k <- 9
Reject9 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data9 <- data.frame(Method,Basket,'% Reject'=Reject9)
k <- 10
Reject10 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data10 <- data.frame(Method,Basket,'% Reject'=Reject10)
k <- 11
Reject11 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data11 <- data.frame(Method,Basket,'% Reject'=Reject11)
k <- 12
Reject12 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data12 <- data.frame(Method,Basket,'% Reject'=Reject12)
k <- 13
Reject13 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data13 <- data.frame(Method,Basket,'% Reject'=Reject13)
k <- 14
Reject14 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data14 <- data.frame(Method,Basket,'% Reject'=Reject14)
k <- 15
Reject15 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data15 <- data.frame(Method,Basket,'% Reject'=Reject15)
k <- 16
Reject16 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data16 <- data.frame(Method,Basket,'% Reject'=Reject16)

Rplot1 <- ggplot(data1, aes(fill=Basket, y=Reject1, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 1 - (0.2,0.2,0.2,0.2,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot2 <- ggplot(data2, aes(fill=Basket, y=Reject2, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 2 - (0.4,0.2,0.2,0.2,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot3 <- ggplot(data3, aes(fill=Basket, y=Reject3, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 3 - (0.4,0.4,0.2,0.2,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot4 <- ggplot(data4, aes(fill=Basket, y=Reject4, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 4 - (0.4,0.4,0.4,0.2,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot5 <- ggplot(data5, aes(fill=Basket, y=Reject5, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 5 - (0.4,0.4,0.4,0.4,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot6 <- ggplot(data6, aes(fill=Basket, y=Reject6, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 6 - (0.4,0.4,0.4,0.4,0.4)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot7 <- ggplot(data7, aes(fill=Basket, y=Reject7, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 7 - (0.2,0.2,0.2,0.2,0.4)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot8 <- ggplot(data8, aes(fill=Basket, y=Reject8, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 8 - (0.4,0.2,0.2,0.2,0.4)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot9 <- ggplot(data9, aes(fill=Basket, y=Reject9, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 9 - (0.4,0.4,0.2,0.2,0.4)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot10 <- ggplot(data10, aes(fill=Basket, y=Reject10, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 10 - (0.4,0.4,0.4,0.2,0.4)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot11 <- ggplot(data11, aes(fill=Basket, y=Reject11, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 11 - (0.3,0.2,0.2,0.2,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot12 <- ggplot(data12, aes(fill=Basket, y=Reject12, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 12 - (0.3,0.3,0.2,0.2,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot13 <- ggplot(data13, aes(fill=Basket, y=Reject13, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 13 - (0.3,0.2,0.2,0.2,0.3)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot14 <- ggplot(data14, aes(fill=Basket, y=Reject14, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 14 - (0.3,0.3,0.2,0.2,0.3)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot15 <- ggplot(data15, aes(fill=Basket, y=Reject15, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 15 - (0.4,0.3,0.2,0.2,0.3)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot16 <- ggplot(data16, aes(fill=Basket, y=Reject16, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c((col[8:5]),'palegreen4'))+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 16 - (0.4,0.3,0.3,0.2,0.3)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')

ggarrange(Rplot1,Rplot2,Rplot3,Rplot4,Rplot5,Rplot6,Rplot7,Rplot8,Rplot9,Rplot10,Rplot11,Rplot12,Rplot13,Rplot14,Rplot15,Rplot16,ncol=4,nrow=4, common.legend = T, legend="right")


#Playing Around With Colors----------------------------------
col <- brewer.pal(9,'Set3')
jBuPuFun <- colorRampPalette(c('black',col[1],'white'))
paletteSize <- 100
ColM1 <- jBuPuFun(paletteSize)
jBuPuFun <- colorRampPalette(c('black',col[3],'white'))
paletteSize <- 100
ColM2 <- jBuPuFun(paletteSize)
jBuPuFun <- colorRampPalette(c('black',col[4],'white'))
paletteSize <- 100
ColM3 <- jBuPuFun(paletteSize)
jBuPuFun <- colorRampPalette(c('black',col[7],'white'))
paletteSize <- 100
ColM4 <- jBuPuFun(paletteSize)

colMethods <- c(ColM1[c(30,40,50,60,70)],ColM2[c(30,40,50,60,70)],ColM3[c(30,40,50,60,70)],ColM4[c(30,40,50,60,70)])

Basket <- c('M1: Basket 1','M1: Basket 2','M1: Basket 3','M1: Basket 4','M1: Basket 5','M2: Basket 1','M2: Basket 2','M2: Basket 3','M2: Basket 4','M2: Basket 5','M3(a): Basket 1','M3(a): Basket 2','M3(a): Basket 3','M3(a): Basket 4','M3(a): Basket 5','M4(a): Basket 1','M4(a): Basket 2','M4(a): Basket 3','M4(a): Basket 4','M4(a): Basket 5')
Basket <- factor(Basket,levels=c('M1: Basket 1','M1: Basket 2','M1: Basket 3','M1: Basket 4','M1: Basket 5','M2: Basket 1','M2: Basket 2','M2: Basket 3','M2: Basket 4','M2: Basket 5','M3(a): Basket 1','M3(a): Basket 2','M3(a): Basket 3','M3(a): Basket 4','M3(a): Basket 5','M4(a): Basket 1','M4(a): Basket 2','M4(a): Basket 3','M4(a): Basket 4','M4(a): Basket 5'))
Method <- c(rep('Method 1',5),rep('Method 2',5),rep('Method 3(a)',5),rep('Method 4(a)',5))
Method <- factor(Method,levels=c('Method 1','Method 2','Method 3(a)','Method 4(a)'))

k <- 1
Reject1 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data1 <- data.frame(Method,Basket,'% Reject'=Reject1)
k <- 2
Reject2 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data2 <- data.frame(Method,Basket,'% Reject'=Reject2)
k <- 3
Reject3 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data3 <- data.frame(Method,Basket,'% Reject'=Reject3)
k <- 4
Reject4 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data4 <- data.frame(Method,Basket,'% Reject'=Reject4)
k <- 5
Reject5 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data5 <- data.frame(Method,Basket,'% Reject'=Reject5)
k <- 6
Reject6 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data6 <- data.frame(Method,Basket,'% Reject'=Reject6)
k <- 7
Reject7 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data7 <- data.frame(Method,Basket,'% Reject'=Reject7)
k <- 8
Reject8 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data8 <- data.frame(Method,Basket,'% Reject'=Reject8)
k <- 9
Reject9 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data9 <- data.frame(Method,Basket,'% Reject'=Reject9)
k <- 10
Reject10 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data10 <- data.frame(Method,Basket,'% Reject'=Reject10)
k <- 11
Reject11 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data11 <- data.frame(Method,Basket,'% Reject'=Reject11)
k <- 12
Reject12 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data12 <- data.frame(Method,Basket,'% Reject'=Reject12)
k <- 13
Reject13 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data13 <- data.frame(Method,Basket,'% Reject'=Reject13)
k <- 14
Reject14 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data14 <- data.frame(Method,Basket,'% Reject'=Reject14)
k <- 15
Reject15 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data15 <- data.frame(Method,Basket,'% Reject'=Reject15)
k <- 16
Reject16 <- 100*c(Method1[,k]$`Rejection Percentage`,Method2[,k]$`Rejection Percentage`,Method3a[,k]$`Rejection Percentage`,Method4a[,k]$`Rejection Percentage`)
data16 <- data.frame(Method,Basket,'% Reject'=Reject16)


Rplot1 <- ggplot(data1, aes(fill=Basket, y=Reject1, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 1 - (0.2,0.2,0.2,0.2,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot2 <- ggplot(data2, aes(fill=Basket, y=Reject2, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 2 - (0.4,0.2,0.2,0.2,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot3 <- ggplot(data3, aes(fill=Basket, y=Reject3, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 3 - (0.4,0.4,0.2,0.2,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot4 <- ggplot(data4, aes(fill=Basket, y=Reject4, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 4 - (0.4,0.4,0.4,0.2,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot5 <- ggplot(data5, aes(fill=Basket, y=Reject5, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 5 - (0.4,0.4,0.4,0.4,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot6 <- ggplot(data6, aes(fill=Basket, y=Reject6, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 6 - (0.4,0.4,0.4,0.4,0.4)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot7 <- ggplot(data7, aes(fill=Basket, y=Reject7, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 7 - (0.2,0.2,0.2,0.2,0.4)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot8 <- ggplot(data8, aes(fill=Basket, y=Reject8, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 8 - (0.4,0.2,0.2,0.2,0.4)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot9 <- ggplot(data9, aes(fill=Basket, y=Reject9, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 9 - (0.4,0.4,0.2,0.2,0.4)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot10 <- ggplot(data10, aes(fill=Basket, y=Reject10, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 10 - (0.4,0.4,0.4,0.2,0.4)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot11 <- ggplot(data11, aes(fill=Basket, y=Reject11, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 11 - (0.3,0.2,0.2,0.2,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot12 <- ggplot(data12, aes(fill=Basket, y=Reject12, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 12 - (0.3,0.3,0.2,0.2,0.2)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot13 <- ggplot(data13, aes(fill=Basket, y=Reject13, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 13 - (0.3,0.2,0.2,0.2,0.3)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot14 <- ggplot(data14, aes(fill=Basket, y=Reject14, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 14 - (0.3,0.3,0.2,0.2,0.3)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot15 <- ggplot(data15, aes(fill=Basket, y=Reject15, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 15 - (0.4,0.3,0.2,0.2,0.3)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')
Rplot16 <- ggplot(data16, aes(fill=Basket, y=Reject16, x=Method)) +
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = colMethods)+
  scale_y_continuous(limits=c(0,100))+
  scale_x_discrete(
    labels = c('Method 1','Method 2','Method 3(a)','Method 4(a)')
  )+
  labs(y='% Reject',x=NA)+
  ggtitle('Scenario 16 - (0.4,0.3,0.3,0.2,0.3)')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank())+
  geom_hline(yintercept=10,linetype='dashed')

ggarrange(Rplot1,Rplot2,Rplot3,Rplot4,Rplot5,Rplot6,Rplot7,Rplot8,Rplot9,Rplot10,Rplot11,Rplot12,Rplot13,Rplot14,Rplot15,Rplot16,nrow=4,ncol=4,common.legend = T,legend='right')
