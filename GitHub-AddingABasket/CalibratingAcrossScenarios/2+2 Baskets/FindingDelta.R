#Finding Delta values by calibrating across several data scenarios. 

load('Method1aDelta.RData') #Load Method 1(a) Data
colQuantiles(Method1Delta$`Combined Cut-Offs`,probs=0.9,na.rm=T) #Take 90% quantile of posterior probabilities across data scenarios.
#Delta New = 0.8987 
#Delta Existing = 0.8669 

load('Method1bDelta.RData') #Load Method 1(b) Data
colQuantiles(Method1Delta$`Combined Cut-Offs`,probs=0.9,na.rm=T)  #Take 90% quantile of posterior probabilities across data scenarios.
#Delta New = 0.9002
#Delta Existing = 0.8676


load('Method2Delta.RData') #Load Method 2 Data
quantile(Method2Delta$`Combined Cut-Offs`[,2],0.9) #Method 2 doesn't consider the new basket in calibration so just find the quantile for existing baskets.
#Delta = 0.86501

load('Method3Delta.RData') #Load Method 3(a) Data
colQuantiles(Method3Delta$`Combined Cut-Offs`,probs=0.9,na.rm=T)  #Take 90% quantile of posterior probabilities across data scenarios.
#Delta New = 0.90010
#Delta Existing = 0.90010

Method 4 #Load Method 4(a) Data 
Delta New= 0.9001
Delta Existing = 0.8987
