library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(latex2exp)
library(grid)
library(gridExtra) 

#Results Plots----------------------------------------------------------------
load('Simulation1aAnalysis.RData')
load('Simulation1bAnalysis.RData')
load('Simulation1cAnalysis.RData')
load('Simulation2aAnalysis.RData')
load('Simulation2bAnalysis.RData')
load('Simulation2cAnalysis.RData')
load('Simulation3aAnalysis.RData')
load('Simulation3bAnalysis.RData')
load('Simulation3cAnalysis.RData')
load('Simulation4aAnalysis.RData')
load('Simulation4bAnalysis.RData')
load('Simulation4cAnalysis.RData')

labA <- "p\u2085\u2208[0.2,0.3]"
labB <- "p\u2085\u2208[0.4,0.5]"
labC <- "p\u2085\u2208[0.1,0.2]"

lab1 <- "p\u2081\u002C\u2082\u002C\u2083\u002C\u2084=0.2"
lab2 <- "p\u2081\u002C\u2082\u002C\u2083\u002C\u2084=0.4"
lab3 <- "p\u2081\u002C\u2082=0.4\u000A p\u2083\u002C\u2084=0.2"
lab4 <- "p\u2081=0.4 p\u2082\u002C\u2083=0.3\u000A p\u2084=0.2"

# lab1 <- "p=(0.2,0.2,0.2,0.2,p\u2085)"
# lab2 <- "p=(0.4,0.4,0.4,0.4,p\u2085)"
# lab3 <- "p=(0.4,0.4,0.2,0.2,p\u2085)"
# lab4 <- "p=(0.4,0.3,0.3,0.2,p\u2085)"

#Comparing Methods 1 and 2
TC1a12 <- Sim1a$`Which Correct`[[1]][6,]/sum(Sim1a$`Which Correct`[[1]][6,])
TC1b12 <- Sim1b$`Which Correct`[[1]][6,]/sum(Sim1b$`Which Correct`[[1]][6,])
TC1c12 <- Sim1c$`Which Correct`[[1]][6,]/sum(Sim1c$`Which Correct`[[1]][6,])
TC2a12 <- Sim2a$`Which Correct`[[1]][6,]/sum(Sim2a$`Which Correct`[[1]][6,])
TC2b12 <- Sim2b$`Which Correct`[[1]][6,]/sum(Sim2b$`Which Correct`[[1]][6,])
TC2c12 <- Sim2c$`Which Correct`[[1]][6,]/sum(Sim2c$`Which Correct`[[1]][6,])
TC3a12 <- Sim3a$`Which Correct`[[1]][6,]/sum(Sim3a$`Which Correct`[[1]][6,])
TC3b12 <- Sim3b$`Which Correct`[[1]][6,]/sum(Sim3b$`Which Correct`[[1]][6,])
TC3c12 <- Sim3c$`Which Correct`[[1]][6,]/sum(Sim3c$`Which Correct`[[1]][6,])
TC4a12 <- Sim4a$`Which Correct`[[1]][6,]/sum(Sim4a$`Which Correct`[[1]][6,])
TC4b12 <- Sim4b$`Which Correct`[[1]][6,]/sum(Sim4b$`Which Correct`[[1]][6,])
TC4c12 <- Sim4c$`Which Correct`[[1]][6,]/sum(Sim4c$`Which Correct`[[1]][6,])

x <- LETTERS[1:3]
y <- c(1,2,3,4)
y <- factor(y,levels=c(1,2,3,4))
data12 <- expand.grid(X=x, Y=y)
data2 <- rbind(TC1a12,TC1b12,TC1c12,TC2a12,TC2b12,TC2c12,TC3a12,TC3b12,TC3c12,TC4a12,TC4b12,TC4c12)
data12$Z <- data2[,1]-data2[,2]

col <- brewer.pal(9,name='Set3')

jBuPuFun <- colorRampPalette(c(col[1],'white',col[3]))
paletteSize <- 256
jBuPuPalette12 <- jBuPuFun(paletteSize)

# Heatmap 
Comp12 <- ggplot(data12, aes(X, Y, fill= Z)) + 
  geom_tile(color = "white",lwd = 1.5) +
  scale_fill_gradient2(high = jBuPuPalette12[1],
                       mid = jBuPuPalette12[paletteSize/2],
                       low = jBuPuPalette12[paletteSize],
                       midpoint = 0,name='Difference in \n Proportion',limits=c(-1,1))+
  theme_minimal()+labs(y='',x='')+theme(legend.title = element_text(size=10),legend.position = "none",axis.text.y = element_text(size=16),axis.text.x=element_text(size=16))+
  coord_flip()+
  scale_x_discrete(
    labels = c(labA,labB,labC)
  )+
  scale_y_discrete(
    labels = c(lab1,lab2,lab3,lab4)
  )+                                         # Add values to heatmap
  geom_text(aes(label = round(Z,2)),size=8)
Comp12



#Comparing Methods 1 and 3
TC1a13 <- Sim1a$`Which Correct`[[2]][6,]/sum(Sim1a$`Which Correct`[[2]][6,])
TC1b13 <- Sim1b$`Which Correct`[[2]][6,]/sum(Sim1b$`Which Correct`[[2]][6,])
TC1c13 <- Sim1c$`Which Correct`[[2]][6,]/sum(Sim1c$`Which Correct`[[2]][6,])
TC2a13 <- Sim2a$`Which Correct`[[2]][6,]/sum(Sim2a$`Which Correct`[[2]][6,])
TC2b13 <- Sim2b$`Which Correct`[[2]][6,]/sum(Sim2b$`Which Correct`[[2]][6,])
TC2c13 <- Sim2c$`Which Correct`[[2]][6,]/sum(Sim2c$`Which Correct`[[2]][6,])
TC3a13 <- Sim3a$`Which Correct`[[2]][6,]/sum(Sim3a$`Which Correct`[[2]][6,])
TC3b13 <- Sim3b$`Which Correct`[[2]][6,]/sum(Sim3b$`Which Correct`[[2]][6,])
TC3c13 <- Sim3c$`Which Correct`[[2]][6,]/sum(Sim3c$`Which Correct`[[2]][6,])
TC4a13 <- Sim4a$`Which Correct`[[2]][6,]/sum(Sim4a$`Which Correct`[[2]][6,])
TC4b13 <- Sim4b$`Which Correct`[[2]][6,]/sum(Sim4b$`Which Correct`[[2]][6,])
TC4c13 <- Sim4c$`Which Correct`[[2]][6,]/sum(Sim4c$`Which Correct`[[2]][6,])

x <- LETTERS[1:3]
y <- c(1,2,3,4)
y <- factor(y,levels=c(1,2,3,4))
data13 <- expand.grid(X=x, Y=y)
data2 <- rbind(TC1a13,TC1b13,TC1c13,TC2a13,TC2b13,TC2c13,TC3a13,TC3b13,TC3c13,TC4a13,TC4b13,TC4c13)
data13$Z <- data2[,1]-data2[,2]

jBuPuFun <- colorRampPalette(c(col[1],'white',col[4]))
paletteSize <- 256
jBuPuPalette13 <- jBuPuFun(paletteSize)

# Heatmap 
Comp13 <- ggplot(data13, aes(X, Y, fill= Z)) + 
  geom_tile(color = "white",lwd = 1.5) +
  scale_fill_gradient2(high = jBuPuPalette13[1],
                       mid = jBuPuPalette13[paletteSize/2],
                       low = jBuPuPalette13[paletteSize],
                       midpoint = 0,name='Difference in \n Proportion',limits=c(-1,1))+
  theme_minimal()+labs(y='',x='')+theme(legend.title = element_text(size=10),legend.position = "none",axis.text.y = element_text(size=16),axis.text.x=element_text(size=16))+
  coord_flip()+
  scale_x_discrete(
    labels = c(labA,labB,labC)
  )+
  scale_y_discrete(
    labels = c(lab1,lab2,lab3,lab4)
  )+                                         # Add values to heatmap
  geom_text(aes(label = round(Z,2)),size=8)
Comp13


#Comparing Methods 1 and 4
TC1a14 <- Sim1a$`Which Correct`[[3]][6,]/sum(Sim1a$`Which Correct`[[3]][6,])
TC1b14 <- Sim1b$`Which Correct`[[3]][6,]/sum(Sim1b$`Which Correct`[[3]][6,])
TC1c14 <- Sim1c$`Which Correct`[[3]][6,]/sum(Sim1c$`Which Correct`[[3]][6,])
TC2a14 <- Sim2a$`Which Correct`[[3]][6,]/sum(Sim2a$`Which Correct`[[3]][6,])
TC2b14 <- Sim2b$`Which Correct`[[3]][6,]/sum(Sim2b$`Which Correct`[[3]][6,])
TC2c14 <- Sim2c$`Which Correct`[[3]][6,]/sum(Sim2c$`Which Correct`[[3]][6,])
TC3a14 <- Sim3a$`Which Correct`[[3]][6,]/sum(Sim3a$`Which Correct`[[3]][6,])
TC3b14 <- Sim3b$`Which Correct`[[3]][6,]/sum(Sim3b$`Which Correct`[[3]][6,])
TC3c14 <- Sim3c$`Which Correct`[[3]][6,]/sum(Sim3c$`Which Correct`[[3]][6,])
TC4a14 <- Sim4a$`Which Correct`[[3]][6,]/sum(Sim4a$`Which Correct`[[3]][6,])
TC4b14 <- Sim4b$`Which Correct`[[3]][6,]/sum(Sim4b$`Which Correct`[[3]][6,])
TC4c14 <- Sim4c$`Which Correct`[[3]][6,]/sum(Sim4c$`Which Correct`[[3]][6,])

x <- LETTERS[1:3]
y <- c(1,2,3,4)
y <- factor(y,levels=c(1,2,3,4))
data14 <- expand.grid(X=x, Y=y)
data2 <- rbind(TC1a14,TC1b14,TC1c14,TC2a14,TC2b14,TC2c14,TC3a14,TC3b14,TC3c14,TC4a14,TC4b14,TC4c14)
data14$Z <- data2[,1]-data2[,2]

jBuPuFun <- colorRampPalette(c(col[1],'white',col[7]))
paletteSize <- 256
jBuPuPalette14 <- jBuPuFun(paletteSize)

Comp14 <- ggplot(data14, aes(X, Y, fill= Z)) + 
  geom_tile(color = "white",lwd = 1.5) +
  scale_fill_gradient2(high = jBuPuPalette14[1],
                       mid = jBuPuPalette14[paletteSize/2],
                       low = jBuPuPalette14[paletteSize],
                       midpoint = 0,name='Difference in \n Proportion',limits=c(-1,1))+
  theme_minimal()+labs(y='',x='')+theme(legend.title = element_text(size=10),legend.position = "none",axis.text.y = element_text(size=16),axis.text.x=element_text(size=16))+
  coord_flip()+
  scale_x_discrete(
    labels = c(labA,labB,labC)
  )+
  scale_y_discrete(
    labels = c(lab1,lab2,lab3,lab4)
  )+                                         # Add values to heatmap
  geom_text(aes(label = round(Z,2)),size=8)
Comp14


#Comparing Methods 2 and 3
TC1a23 <- Sim1a$`Which Correct`[[4]][6,]/sum(Sim1a$`Which Correct`[[4]][6,])
TC1b23 <- Sim1b$`Which Correct`[[4]][6,]/sum(Sim1b$`Which Correct`[[4]][6,])
TC1c23 <- Sim1c$`Which Correct`[[4]][6,]/sum(Sim1c$`Which Correct`[[4]][6,])
TC2a23 <- Sim2a$`Which Correct`[[4]][6,]/sum(Sim2a$`Which Correct`[[4]][6,])
TC2b23 <- Sim2b$`Which Correct`[[4]][6,]/sum(Sim2b$`Which Correct`[[4]][6,])
TC2c23 <- Sim2c$`Which Correct`[[4]][6,]/sum(Sim2c$`Which Correct`[[4]][6,])
TC3a23 <- Sim3a$`Which Correct`[[4]][6,]/sum(Sim3a$`Which Correct`[[4]][6,])
TC3b23 <- Sim3b$`Which Correct`[[4]][6,]/sum(Sim3b$`Which Correct`[[4]][6,])
TC3c23 <- Sim3c$`Which Correct`[[4]][6,]/sum(Sim3c$`Which Correct`[[4]][6,])
TC4a23 <- Sim4a$`Which Correct`[[4]][6,]/sum(Sim4a$`Which Correct`[[4]][6,])
TC4b23 <- Sim4b$`Which Correct`[[4]][6,]/sum(Sim4b$`Which Correct`[[4]][6,])
TC4c23 <- Sim4c$`Which Correct`[[4]][6,]/sum(Sim4c$`Which Correct`[[4]][6,])

x <- LETTERS[1:3]
y <- c(1,2,3,4)
y <- factor(y,levels=c(1,2,3,4))
data23 <- expand.grid(X=x, Y=y)
data2 <- rbind(TC1a23,TC1b23,TC1c23,TC2a23,TC2b23,TC2c23,TC3a23,TC3b23,TC3c23,TC4a23,TC4b23,TC4c23)
data23$Z <- data2[,1]-data2[,2]

jBuPuFun <- colorRampPalette(c(col[3],'white',col[4]))
paletteSize <- 256
jBuPuPalette23 <- jBuPuFun(paletteSize)

# Heatmap 
Comp23 <- ggplot(data23, aes(X, Y, fill= Z)) + 
  geom_tile(color = "white",lwd = 1.5) +
  scale_fill_gradient2(high = jBuPuPalette23[1],
                       mid = jBuPuPalette23[paletteSize/2],
                       low = jBuPuPalette23[paletteSize],
                       midpoint = 0,name='Difference in \n Proportion',limits=c(-1,1))+
  theme_minimal()+labs(y='',x='')+theme(legend.title = element_text(size=10),legend.position = "none",axis.text.y = element_text(size=16),axis.text.x=element_text(size=16))+
  coord_flip()+
  scale_x_discrete(
    labels = c(labA,labB,labC)
  )+
  scale_y_discrete(
    labels = c(lab1,lab2,lab3,lab4)
  )+                                         # Add values to heatmap
  geom_text(aes(label = round(Z,2)),size=8)
Comp23

#Comparing Methods 2 and 4
TC1a24 <- Sim1a$`Which Correct`[[5]][6,]/sum(Sim1a$`Which Correct`[[5]][6,])
TC1b24 <- Sim1b$`Which Correct`[[5]][6,]/sum(Sim1b$`Which Correct`[[5]][6,])
TC1c24 <- Sim1c$`Which Correct`[[5]][6,]/sum(Sim1c$`Which Correct`[[5]][6,])
TC2a24 <- Sim2a$`Which Correct`[[5]][6,]/sum(Sim2a$`Which Correct`[[5]][6,])
TC2b24 <- Sim2b$`Which Correct`[[5]][6,]/sum(Sim2b$`Which Correct`[[5]][6,])
TC2c24 <- Sim2c$`Which Correct`[[5]][6,]/sum(Sim2c$`Which Correct`[[5]][6,])
TC3a24 <- Sim3a$`Which Correct`[[5]][6,]/sum(Sim3a$`Which Correct`[[5]][6,])
TC3b24 <- Sim3b$`Which Correct`[[5]][6,]/sum(Sim3b$`Which Correct`[[5]][6,])
TC3c24 <- Sim3c$`Which Correct`[[5]][6,]/sum(Sim3c$`Which Correct`[[5]][6,])
TC4a24 <- Sim4a$`Which Correct`[[5]][6,]/sum(Sim4a$`Which Correct`[[5]][6,])
TC4b24 <- Sim4b$`Which Correct`[[5]][6,]/sum(Sim4b$`Which Correct`[[5]][6,])
TC4c24 <- Sim4c$`Which Correct`[[5]][6,]/sum(Sim4c$`Which Correct`[[5]][6,])

x <- LETTERS[1:3]
y <- c(1,2,3,4)
y <- factor(y,levels=c(1,2,3,4))
data24 <- expand.grid(X=x, Y=y)
data2 <- rbind(TC1a24,TC1b24,TC1c24,TC2a24,TC2b24,TC2c24,TC3a24,TC3b24,TC3c24,TC4a24,TC4b24,TC4c24)
data24$Z <- data2[,1]-data2[,2]

jBuPuFun <- colorRampPalette(c(col[3],'white',col[7]))
paletteSize <- 256
jBuPuPalette24 <- jBuPuFun(paletteSize)

Comp24 <- ggplot(data24, aes(X, Y, fill= Z)) + 
  geom_tile(color = "white",lwd = 1.5) +
  scale_fill_gradient2(high = jBuPuPalette24[1],
                       mid = jBuPuPalette24[paletteSize/2],
                       low = jBuPuPalette24[paletteSize],
                       midpoint = 0,name='Difference in \n Proportion',limits=c(-1,1))+
  theme_minimal()+labs(y='',x='')+theme(legend.title = element_text(size=10),legend.position = "none",axis.text.y = element_text(size=16),axis.text.x=element_text(size=16))+
  coord_flip()+
  scale_x_discrete(
    labels = c(labA,labB,labC)
  )+
  scale_y_discrete(
    labels = c(lab1,lab2,lab3,lab4)
  )+                                         # Add values to heatmap
  geom_text(aes(label = round(Z,2)),size=8)
Comp24


#Comparing Methods 3 and 4
TC1a34 <- Sim1a$`Which Correct`[[6]][6,]/sum(Sim1a$`Which Correct`[[6]][6,])
TC1b34 <- Sim1b$`Which Correct`[[6]][6,]/sum(Sim1b$`Which Correct`[[6]][6,])
TC1c34 <- Sim1c$`Which Correct`[[6]][6,]/sum(Sim1c$`Which Correct`[[6]][6,])
TC2a34 <- Sim2a$`Which Correct`[[6]][6,]/sum(Sim2a$`Which Correct`[[6]][6,])
TC2b34 <- Sim2b$`Which Correct`[[6]][6,]/sum(Sim2b$`Which Correct`[[6]][6,])
TC2c34 <- Sim2c$`Which Correct`[[6]][6,]/sum(Sim2c$`Which Correct`[[6]][6,])
TC3a34 <- Sim3a$`Which Correct`[[6]][6,]/sum(Sim3a$`Which Correct`[[6]][6,])
TC3b34 <- Sim3b$`Which Correct`[[6]][6,]/sum(Sim3b$`Which Correct`[[6]][6,])
TC3c34 <- Sim3c$`Which Correct`[[6]][6,]/sum(Sim3c$`Which Correct`[[6]][6,])
TC4a34 <- Sim4a$`Which Correct`[[6]][6,]/sum(Sim4a$`Which Correct`[[6]][6,])
TC4b34 <- Sim4b$`Which Correct`[[6]][6,]/sum(Sim4b$`Which Correct`[[6]][6,])
TC4c34 <- Sim4c$`Which Correct`[[6]][6,]/sum(Sim4c$`Which Correct`[[6]][6,])

x <- LETTERS[1:3]
y <- c(1,2,3,4)
y <- factor(y,levels=c(1,2,3,4))
data34 <- expand.grid(X=x, Y=y)
data2 <- rbind(TC1a34,TC1b34,TC1c34,TC2a34,TC2b34,TC2c34,TC3a34,TC3b34,TC3c34,TC4a34,TC4b34,TC4c34)
data34$Z <- data2[,1]-data2[,2]

jBuPuFun <- colorRampPalette(c(col[4],'white',col[7]))
paletteSize <- 256
jBuPuPalette34 <- jBuPuFun(paletteSize)

Comp34 <- ggplot(data34, aes(X, Y, fill= Z)) + 
  geom_tile(color = "white",lwd = 1.5) +
  scale_fill_gradient2(high = jBuPuPalette34[1],
                       mid = jBuPuPalette34[paletteSize/2],
                       low = jBuPuPalette34[paletteSize],
                       midpoint = 0,name='Difference in \n Proportion',limits=c(-1,1))+
  theme_minimal()+labs(y='',x='')+theme(legend.title = element_text(size=10),legend.position = "none",axis.text.y = element_text(size=16),axis.text.x=element_text(size=16))+
  coord_flip()+
  scale_x_discrete(
    labels = c(labA,labB,labC)
  )+
  scale_y_discrete(
    labels = c(lab1,lab2,lab3,lab4)
  )+                                         # Add values to heatmap
  geom_text(aes(label = round(Z,2)),size=8)
Comp34


ann1 <- ggplot() + 
  geom_text(aes(x=0, y=0, label = "M1"), 
            parse = TRUE, size = 10, hjust = -1) +
  theme_void()

ann2 <- ggplot() + 
  geom_text(aes(x=0, y=0, label = "M2"), 
            parse = TRUE, size = 10, hjust = -1) +
  theme_void()

ann3 <- ggplot() + 
  geom_text(aes(x=0, y=0, label = "M3"), 
            parse = TRUE, size = 10, hjust = -1) +
  theme_void()

ann4 <- ggplot() + 
  geom_text(aes(x=0, y=0, label = "M4"), 
            parse = TRUE, size = 10, hjust = -1) +
  theme_void()


#ggarrange(NULL,ann2,ann3,ann4,ann1,Comp12,Comp13,Comp14,ann2,NULL,Comp23,Comp24,ann3,NULL,NULL,Comp34,nrow=4,ncol=4,widths = c(0.05, 0.3, 0.3, 0.3),heights = c(0.05, 0.3, 0.3, 0.3))



#Creating A Legend
Method <- rep(c('Method 1 outperforms a competitor','Method 2 outperforms a competitor','Method 3(a) outperforms a competitor','Method 4(a) outperforms a competitor'),1)
Method <- factor(Method,levels=c('Method 1 outperforms a competitor','Method 2 outperforms a competitor','Method 3(a) outperforms a competitor','Method 4(a) outperforms a competitor'))

col <- brewer.pal(9,name='Set3')
#Scenario 1
k=1
Power1 <- rep(0,4)
data1 <- data.frame(Method,Power1)
LPlot <- ggplot(data1,aes(fill=Method,y=Power1,x=Method))+geom_bar(position='dodge',stat='identity')+
  scale_fill_manual(values=c(col[c(1,3,4,7)]))+
  scale_y_continuous(limits=c(-20,20))+
  scale_x_discrete(
    labels = c('Mean Error', 'Mean Power','New Basket Error')
  )+labs(y='% Reject (Across Scenarios - Null)',x=NA)+
  ggtitle('Scenario 1 - (0.2,0.2,0.2,0.2,0.2)')+
  theme_minimal()+
  theme(legend.key.size = unit(1.5, 'cm'), #change legend key size
        legend.key.height = unit(1.5, 'cm'), #change legend key height
        legend.key.width = unit(1.5, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=16)) +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank(),legend.title=element_blank())
legend <- cowplot::get_legend(LPlot)

grid.newpage()
grid.draw(legend)

ggarrange(NULL,ann2,ann3,ann4,ann1,Comp12,Comp13,Comp14,ann2,NULL,Comp23,Comp24,ann3,legend,NULL,Comp34,nrow=4,ncol=4,widths = c(0.1, 0.3, 0.3, 0.3),heights = c(0.05, 0.3, 0.3, 0.3))

