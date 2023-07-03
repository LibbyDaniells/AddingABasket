load('Method1aDelta.RData')
colQuantiles(Method1Delta$`Combined Cut-Offs`,probs=0.9,na.rm=T)
#Delta New = 0.8987
#Delta Existing = 0.8669

load('Method1bDelta.RData')
colQuantiles(Method1Delta$`Combined Cut-Offs`,probs=0.9,na.rm=T)
#Delta New = 0.9002
#Delta Existing = 0.8676


load('Method2Delta.RData')
quantile(Method2Delta$`Combined Cut-Offs`[,2],0.9)
#Delta = 0.86501

load('Method3Delta.RData')
colQuantiles(Method3Delta$`Combined Cut-Offs`,probs=0.9,na.rm=T)
#Delta New = 0.90010
#Delta Existing = 0.90010

#Method 4
#Delta New= 0.9001
#Delta Existing = 0.8987