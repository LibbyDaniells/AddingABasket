load('Method2Delta.RData')

colQuantiles(Method2Delta$`Combined Cut-Offs`,probs=0.9,na.rm=T)
#Delta Existing = 0.9056
#Delta New = 0.9056


load('IndependentDelta.RData')
#Across Scenarios 1-10
colQuantiles(IndependentDelta$`Combined Cut-Offs`,probs=0.9,na.rm=T)
#Delta Existing = 0.846
#Delta New = 0.89931

#Across Scenarios 1-5
Comb15 <- rbind(IndependentDelta$`Scenario Cut-offs`[[1]],IndependentDelta$`Scenario Cut-offs`[[2]],IndependentDelta$`Scenario Cut-offs`[[3]],IndependentDelta$`Scenario Cut-offs`[[4]],IndependentDelta$`Scenario Cut-offs`[[5]])
colQuantiles(Comb15,probs=0.9,na.rm=T)
#Delta Existing = 0.8458
#Delta New = 0.89931

load('Method1Delta.RData')
#Across Scenarios 1-10
colQuantiles(Method1Delta$`Combined Cut-Offs`,probs=0.9,na.rm=T)
#Delta Existing = 0.9044
#Delta New = 0.8989

#Across Scenarios 1-5
Comb15 <- rbind(Method1Delta$`Scenario Cut-offs`[[1]],Method1Delta$`Scenario Cut-offs`[[2]],Method1Delta$`Scenario Cut-offs`[[3]],Method1Delta$`Scenario Cut-offs`[[4]],Method1Delta$`Scenario Cut-offs`[[5]])
colQuantiles(Comb15,probs=0.9,na.rm=T)
#Delta Existing = 0.903
#Delta New = 0.8989

load('Method3Delta.RData')
#Across Scenarios 1-10
colQuantiles(Method3aDelta$`Combined Cut-Offs`,probs=0.9,na.rm=T)
#Delta Existing = 0.91011
#Delta New = 0.90211

#Across Scenarios 1-5
Comb15 <- rbind(Method3aDelta$`Scenario Cut-offs`[[1]],Method3aDelta$`Scenario Cut-offs`[[2]],Method3aDelta$`Scenario Cut-offs`[[3]],Method3aDelta$`Scenario Cut-offs`[[4]],Method3aDelta$`Scenario Cut-offs`[[5]])
colQuantiles(Comb15,probs=0.9,na.rm=T)
#Delta Existing = 0.9034
#Delta New = 0.90211