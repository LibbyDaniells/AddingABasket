#Based on calibration data from the calibration files, this code finds Delta for both new and existing baskets

load('Method1Delta.RData') #Load Method 1 Data
#Calibration across Scenarios 1-10
colQuantiles(Method1Delta$`Combined Cut-Offs`,probs=0.9,na.rm=T)
#Delta Existing = 0.9044
#Delta New = 0.8989

#Calibration across Scenarios 1-5
Comb15 <- rbind(Method1Delta$`Scenario Cut-offs`[[1]],Method1Delta$`Scenario Cut-offs`[[2]],Method1Delta$`Scenario Cut-offs`[[3]],Method1Delta$`Scenario Cut-offs`[[4]],Method1Delta$`Scenario Cut-offs`[[5]])
colQuantiles(Comb15,probs=0.9,na.rm=T)
#Delta Existing = 0.903
#Delta New = 0.8989

load('Method2Delta.RData') #Load Method 2 Data
#Calibration across Scenarios 1-5 (no calibration across 1-10 as new baskets aren't considered in the calibration)
colQuantiles(Method2Delta$`Combined Cut-Offs`,probs=0.9,na.rm=T)
#Delta Existing = 0.9056
#Delta New = 0.9056

load('Method3Delta.RData') #Load Method 3(a) Data
#Calibration across Scenarios 1-10
colQuantiles(Method3aDelta$`Combined Cut-Offs`,probs=0.9,na.rm=T)
#Delta Existing = 0.91011
#Delta New = 0.90211

#Calibration across Scenarios 1-5
Comb15 <- rbind(Method3aDelta$`Scenario Cut-offs`[[1]],Method3aDelta$`Scenario Cut-offs`[[2]],Method3aDelta$`Scenario Cut-offs`[[3]],Method3aDelta$`Scenario Cut-offs`[[4]],Method3aDelta$`Scenario Cut-offs`[[5]])
colQuantiles(Comb15,probs=0.9,na.rm=T)
#Delta Existing = 0.9034
#Delta New = 0.90211

#For Method 4(a)
#Delta Existing = 0.903 #Same as Method 1
#Delta New = 0.90211 #Same as Method 3(a)
