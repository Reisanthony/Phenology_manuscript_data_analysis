library(rstan)
library(scales)
library(corrgram)
source("mycorrgram.R")

#CASTLE PEAK
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Castle Peak.rdat')
cssp= sp
#beta
betas<-extract(fit,"beta")
#spring max temp
#MO, TO, LO
cb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
cb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
cb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))

#winter precipitation
#MO, TO, LO
cb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
cb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
cb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))

#spring min temp
#MO, TO, LO
cb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
cb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
cb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))


#DONNER PASS
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Donner Pass.rdat')
dssp= sp

#beta
betas<-extract(fit,"beta")
#spring max temp
#MO, TO, LO
db1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
db7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
db8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))

#winter precipitation
#MO, TO, LO
db2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
db9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
db10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))

#spring min temp
#MO, TO, LO
db3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
db11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
db12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))

#LANG CROSSING
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Lang Crossing.rdat')
lssp= sp

#beta
betas<-extract(fit,"beta")
#spring max temp
#MO, TO, LO
lb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
lb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
lb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))

#winter precipitation
#MO, TO, LO
lb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
lb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
lb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))

#spring min temp
#MO, TO, LO
lb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
lb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
lb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))

#SIERRA VALLEY
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Sierra Valley.rdat')
sssp= sp

#beta
betas<-extract(fit,"beta")
#spring max temp
#MO, TO, LO
sb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
sb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
sb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))

#winter precipitation
#MO, TO, LO
sb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
sb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
sb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))

#spring min temp
#MO, TO, LO
sb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
sb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
sb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))

#WASHINGTON
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Washington.rdat')
wssp= sp

#beta
betas<-extract(fit,"beta")
#spring max temp
#MO, TO, LO
wb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
wb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
wb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))

#winter precipitation
#MO, TO, LO
wb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
wb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
wb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))

#spring min temp
#MO, TO, LO
wb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
wb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
wb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))



# All species
allsp <- c(cssp,dssp,lssp,sssp,wssp)
unsp <- unique(allsp)
nosp <- length(unsp)

#castle peak 
#spring max temp
#MO, TO, LO
cp1 <- rep(NA, length(unsp)) 
cp7 <- rep(NA, length(unsp)) 
cp8 <- rep(NA, length(unsp)) 
#winter precipitation
#MO, TO, LO
cp2 <- rep(NA, length(unsp)) 
cp9 <- rep(NA, length(unsp)) 
cp10 <- rep(NA, length(unsp))
#spring min temp
#MO, TO, LO
cp3 <- rep(NA, length(unsp)) 
cp11 <- rep(NA, length(unsp)) 
cp12 <- rep(NA, length(unsp)) 

# Indices of species present at Castle Peak within the full species list
matching_indices <- match(cssp, unsp)
# Assign beta values of species present at Castle Peak to their corresponding positions
cp1[matching_indices] <- cb1[1, ]
cp7[matching_indices] <- cb7[1,]
cp8[matching_indices] <- cb8[1,]

cp2[matching_indices] <- cb2[1,] 
cp9[matching_indices] <- cb9[1,] 
cp10[matching_indices] <- cb10[1,]  

cp3[matching_indices] <- cb3[1,]
cp11[matching_indices] <- cb11[1,]
cp12[matching_indices] <- cb12[1,]



#donner pass 
#spring max temp
#MO, TO, LO
dp1 <- rep(NA, length(unsp)) 
dp7 <- rep(NA, length(unsp)) 
dp8 <- rep(NA, length(unsp)) 
#winter precipitation
#MO, TO, LO
dp2 <- rep(NA, length(unsp)) 
dp9 <- rep(NA, length(unsp)) 
dp10 <- rep(NA, length(unsp))
#spring min temp
#MO, TO, LO
dp3 <- rep(NA, length(unsp)) 
dp11 <- rep(NA, length(unsp)) 
dp12 <- rep(NA, length(unsp)) 

# Indices of species present at Donner Pass within the full species list
matching_indices <- match(dssp, unsp)
# Assign beta values of species present at Donner Pass to their corresponding positions
dp1[matching_indices] <- db1[1,] 
dp7[matching_indices] <- db7[1,] 
dp8[matching_indices] <- db8[1,] 

dp2[matching_indices] <- db2[1,] 
dp9[matching_indices] <- db9[1,] 
dp10[matching_indices] <- db10[1,]

dp3[matching_indices] <- db3[1,]
dp11[matching_indices] <- db11[1,]
dp12[matching_indices] <- db12[1,] 


#langcrossing
#spring max temp
#MO, TO, LO
lc1 <- rep(NA, length(unsp)) 
lc7 <- rep(NA, length(unsp)) 
lc8 <- rep(NA, length(unsp)) 
#winter precipitation
#MO, TO, LO
lc2 <- rep(NA, length(unsp)) 
lc9 <- rep(NA, length(unsp)) 
lc10 <- rep(NA, length(unsp))
#spring min temp
#MO, TO, LO
lc3 <- rep(NA, length(unsp)) 
lc11 <- rep(NA, length(unsp)) 
lc12 <- rep(NA, length(unsp)) 

# Indices of species present at Lang Crossing within the full species list
matching_indices <- match(lssp, unsp)
# Assign beta values of species present at Lang Crossing to their corresponding positions
lc1[matching_indices] <- lb1[1,] 
lc7[matching_indices] <- lb7[1,] 
lc8[matching_indices] <- lb8[1,] 

lc2[matching_indices] <- lb2[1,] 
lc9[matching_indices] <- lb9[1,] 
lc10[matching_indices] <- lb10[1,]  

lc3[matching_indices] <- lb3[1,] 
lc11[matching_indices] <- lb11[1,] 
lc12[matching_indices] <- lb12[1,] 


#sierra valley 
#spring max temp
#MO, TO, LO
sv1 <- rep(NA, length(unsp)) 
sv7 <- rep(NA, length(unsp)) 
sv8 <- rep(NA, length(unsp)) 
#winter precipitation
#MO, TO, LO
sv2 <- rep(NA, length(unsp)) 
sv9 <- rep(NA, length(unsp)) 
sv10 <- rep(NA, length(unsp))
#spring min temp
#MO, TO, LO
sv3 <- rep(NA, length(unsp)) 
sv11 <- rep(NA, length(unsp)) 
sv12 <- rep(NA, length(unsp)) 

# Indices of species present at Sierra Valley within the full species list
matching_indices <- match(sssp, unsp)
# Assign beta values of species present at Sierra Valley to their corresponding positions
sv1[matching_indices] <- sb1[1,] 
sv7[matching_indices] <- sb7[1,] 
sv8[matching_indices] <- sb8[1,] 

sv2[matching_indices] <- sb2[1,] 
sv9[matching_indices] <- sb9[1,] 
sv10[matching_indices] <- sb10[1,] 

sv3[matching_indices] <- sb3[1,]
sv11[matching_indices] <- sb11[1,]
sv12[matching_indices] <- sb12[1,] 




#washington
#spring max temp
#MO, TO, LO
wa1 <- rep(NA, length(unsp)) 
wa7 <- rep(NA, length(unsp)) 
wa8 <- rep(NA, length(unsp)) 
#winter precipitation
#MO, TO, LO
wa2 <- rep(NA, length(unsp)) 
wa9 <- rep(NA, length(unsp)) 
wa10 <- rep(NA, length(unsp))
#spring min temp
#MO, TO, LO
wa3 <- rep(NA, length(unsp)) 
wa11 <- rep(NA, length(unsp)) 
wa12 <- rep(NA, length(unsp)) 

# Indices of species present at Washington within the full species list
matching_indices <- match(wssp, unsp)
# Assign beta values of species present at Washington to their corresponding positions
wa1[matching_indices] <- wb1[1,]
wa7[matching_indices] <- wb7[1,]
wa8[matching_indices] <- wb8[1,]

wa2[matching_indices] <- wb2[1,] 
wa9[matching_indices] <- wb9[1,]
wa10[matching_indices] <- wb10[1,] 

wa3[matching_indices] <- wb3[1,]
wa11[matching_indices] <- wb11[1,]
wa12[matching_indices] <- wb12[1,] 

# Combine vectors containing beta values for all species at each site 
# (NA for species not present at that site) into a data frame 
springmaxtemp_MO <- cbind(cp1,dp1,lc1, sv1, wa1)
colnames(springmaxtemp_MO) <- c('CP','DP','LC', 'SV', 'WA')
springmaxtemp_TO <- cbind(cp7,dp7,lc7, sv7, wa7)
colnames(springmaxtemp_TO) <- c('CP','DP','LC', 'SV', 'WA')
springmaxtemp_LO <- cbind(cp8,dp8,lc8, sv8, wa8)
colnames(springmaxtemp_LO) <- c('CP','DP','LC', 'SV', 'WA')

wintprcp_MO <- cbind(cp2,dp2,lc2, sv2, wa2)
colnames(wintprcp_MO) <- c('CP','DP','LC', 'SV', 'WA')
wintprcp_TO <- cbind(cp9,dp9,lc9, sv9, wa9)
colnames(wintprcp_TO) <- c('CP','DP','LC', 'SV', 'WA')
wintprcp_LO <- cbind(cp10,dp10,lc10, sv10, wa10)
colnames(wintprcp_LO) <- c('CP','DP','LC', 'SV', 'WA')


springmintemp_MO <- cbind(cp3,dp3,lc3, sv3, wa3)
colnames(springmintemp_MO) <- c('CP','DP','LC', 'SV', 'WA')
springmintemp_TO <- cbind(cp11,dp11,lc11, sv11, wa11)
colnames(springmintemp_TO) <- c('CP','DP','LC', 'SV', 'WA')
springmintemp_LO <- cbind(cp12,dp12,lc12, sv12, wa12)
colnames(springmintemp_LO) <- c('CP','DP','LC', 'SV', 'WA')




pdf("correlation_plot_showing_correlation_in_effect_of_climate_among_species_present_at_both_compared_sites.pdf")
layout(matrix(c(1:9),3,3,byrow=T))

#springmaxtemp_MO
mycorrgram(springmaxtemp_MO, lower.panel = panel.pts,
         upper.panel = panel.cor, main="(a) Spring max temperature (MO)",
         cex.main = 1.7, cex.labels = 4.0)

#wintprcp_MO
mycorrgram(wintprcp_MO, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(b) Winter precipitation (MO)",
           cex.main = 1.7, cex.labels = 4.0)

#springmintemp_MO
mycorrgram(springmintemp_MO, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(c) Spring min temperature (MO)",
           cex.main = 1.7)

#springmaxtemp_TO
mycorrgram(springmaxtemp_TO, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(d) Spring max temperature (TO)",
           cex.main = 1.7, cex.labels = 4.0)

#wintprcp_TO
mycorrgram(wintprcp_TO, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(e) Winter precipitation (TO)",
           cex.main = 1.7, cex.labels = 4.0)

#springmintemp_TO
mycorrgram(springmintemp_TO, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(f) Spring min temperature (TO)",
           cex.main = 1.7, cex.labels = 4.0)

#springmaxtemp_LO
mycorrgram(springmaxtemp_LO, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(g) Spring max temperature (LO)",
           cex.main = 1.7, cex.labels = 4.0)

#wintprcp_LO
mycorrgram(wintprcp_LO, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(h) Winter precipitation (LO)",
           cex.main = 1.7, cex.labels = 4.0)

#springmintemp_LO
mycorrgram(springmintemp_LO, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(i) Spring min temperature (LO)",
           cex.main = 1.7, cex.labels = 4.0)

dev.off()



