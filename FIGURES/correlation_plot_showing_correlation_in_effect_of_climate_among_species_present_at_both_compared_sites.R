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
#peak, timing, length
cb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
cb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
cb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))

#winter precipitation
#peak, timing, length
cb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
cb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
cb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))

#spring min temp
#peak, timing, length
cb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
cb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
cb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))


#DONNER PASS
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Donner Pass.rdat')
dssp= sp

#beta
betas<-extract(fit,"beta")
#spring max temp
#peak, timing, length
db1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
db7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
db8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))

#winter precipitation
#peak, timing, length
db2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
db9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
db10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))

#spring min temp
#peak, timing, length
db3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
db11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
db12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))

#LANG CROSSING
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Lang Crossing.rdat')
lssp= sp

#beta
betas<-extract(fit,"beta")
#spring max temp
#peak, timing, length
lb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
lb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
lb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))

#winter precipitation
#peak, timing, length
lb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
lb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
lb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))

#spring min temp
#peak, timing, length
lb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
lb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
lb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))

#SIERRA VALLEY
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Sierra Valley.rdat')
sssp= sp

#beta
betas<-extract(fit,"beta")
#spring max temp
#peak, timing, length
sb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
sb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
sb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))

#winter precipitation
#peak, timing, length
sb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
sb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
sb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))

#spring min temp
#peak, timing, length
sb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
sb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
sb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))

#WASHINGTON
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Washington.rdat')
wssp= sp

#beta
betas<-extract(fit,"beta")
#spring max temp
#peak, timing, length
wb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
wb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
wb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))

#winter precipitation
#peak, timing, length
wb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
wb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
wb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))

#spring min temp
#peak, timing, length
wb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
wb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
wb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))



# Sample data
allsp <- c(cssp,dssp,lssp,sssp,wssp)
unsp <- unique(allsp)
nosp <- length(unsp)



#castle peak 
#repeating NA by total number of unique species  for each climate interaction
#spring max temp
#peak, timing , length
cp1 <- rep(NA, length(unsp)) 
cp7 <- rep(NA, length(unsp)) 
cp8 <- rep(NA, length(unsp)) 
#winter precipitation
#peak, timing , length
cp2 <- rep(NA, length(unsp)) 
cp9 <- rep(NA, length(unsp)) 
cp10 <- rep(NA, length(unsp))
#spring min temp
#peak, timing , length
cp3 <- rep(NA, length(unsp)) 
cp11 <- rep(NA, length(unsp)) 
cp12 <- rep(NA, length(unsp)) 


# Find the indices of species present in castlepeak within all species
matching_indices <- match(cssp, unsp)   #which(unsp %in% cssp)

# Assign the betas of species to the corresponding positions of species present at castle  peak
cp1[matching_indices] <- cb1[1,] 
cp7[matching_indices] <- cb7[1,]
cp8[matching_indices] <- cb8[1,]

cp2[matching_indices] <- cb2[1,] 
cp9[matching_indices] <- cb9[1,] 
cp10[matching_indices] <- cb10[1,]  

cp3[matching_indices] <- cb3[1,]
cp11[matching_indices] <- cb11[1,]
cp12[matching_indices] <- cb12[1,]



#donner pass 
#repeating NA by total number of unique species  for each climate interaction
#spring max temp
#peak, timing , length
dp1 <- rep(NA, length(unsp)) 
dp7 <- rep(NA, length(unsp)) 
dp8 <- rep(NA, length(unsp)) 
#winter precipitation
#peak, timing , length
dp2 <- rep(NA, length(unsp)) 
dp9 <- rep(NA, length(unsp)) 
dp10 <- rep(NA, length(unsp))
#spring min temp
#peak, timing , length
dp3 <- rep(NA, length(unsp)) 
dp11 <- rep(NA, length(unsp)) 
dp12 <- rep(NA, length(unsp)) 

# Find the indices of species present in donnerpass within all species
matching_indices <- match(dssp, unsp)

# Assign the betas of species to the corresponding positions of species present at donnerpass
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
#repeating NA by total number of unique species  for each climate interaction
#spring max temp
#peak, timing , length
lc1 <- rep(NA, length(unsp)) 
lc7 <- rep(NA, length(unsp)) 
lc8 <- rep(NA, length(unsp)) 
#winter precipitation
#peak, timing , length
lc2 <- rep(NA, length(unsp)) 
lc9 <- rep(NA, length(unsp)) 
lc10 <- rep(NA, length(unsp))
#spring min temp
#peak, timing , length
lc3 <- rep(NA, length(unsp)) 
lc11 <- rep(NA, length(unsp)) 
lc12 <- rep(NA, length(unsp)) 

# Find the indices of species present in langcrossing within all species
matching_indices <- match(lssp, unsp)

# Assign the betas of species to the corresponding positions of species present at lang crossing
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
#repeating NA by total number of unique species  for each climate interaction
#spring max temp
#peak, timing , length
sv1 <- rep(NA, length(unsp)) 
sv7 <- rep(NA, length(unsp)) 
sv8 <- rep(NA, length(unsp)) 
#winter precipitation
#peak, timing , length
sv2 <- rep(NA, length(unsp)) 
sv9 <- rep(NA, length(unsp)) 
sv10 <- rep(NA, length(unsp))
#spring min temp
#peak, timing , length
sv3 <- rep(NA, length(unsp)) 
sv11 <- rep(NA, length(unsp)) 
sv12 <- rep(NA, length(unsp)) 


# Find the indices of species present in sierra valley within all species
matching_indices <- match(sssp, unsp)

# Assign the betas of species to the corresponding positions of species present at sierravalley
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
#repeating NA by total number of unique species  for each climate interaction
#spring max temp
#peak, timing , length
wa1 <- rep(NA, length(unsp)) 
wa7 <- rep(NA, length(unsp)) 
wa8 <- rep(NA, length(unsp)) 
#winter precipitation
#peak, timing , length
wa2 <- rep(NA, length(unsp)) 
wa9 <- rep(NA, length(unsp)) 
wa10 <- rep(NA, length(unsp))
#spring min temp
#peak, timing , length
wa3 <- rep(NA, length(unsp)) 
wa11 <- rep(NA, length(unsp)) 
wa12 <- rep(NA, length(unsp)) 


# Find the indices of species present in washington within all species
matching_indices <- match(wssp, unsp)

# Assign the betas of species to the corresponding positions of species present at washington 
wa1[matching_indices] <- wb1[1,]
wa7[matching_indices] <- wb7[1,]
wa8[matching_indices] <- wb8[1,]

wa2[matching_indices] <- wb2[1,] 
wa9[matching_indices] <- wb9[1,]
wa10[matching_indices] <- wb10[1,] 

wa3[matching_indices] <- wb3[1,]
wa11[matching_indices] <- wb11[1,]
wa12[matching_indices] <- wb12[1,] 

#combining vectors containg  betas for all species at a site (and Na for species not present at that site) into a data frame for each climate interaction.
springmaxtemp_peak <- cbind(cp1,dp1,lc1, sv1, wa1)
springmaxtemp_timing <- cbind(cp7,dp7,lc7, sv7, wa7)
springmaxtemp_length <- cbind(cp8,dp8,lc8, sv8, wa8)

wintprcp_peak <- cbind(cp2,dp2,lc2, sv2, wa2)
wintprcp_timing <- cbind(cp9,dp9,lc9, sv9, wa9)
wintprcp_length <- cbind(cp10,dp10,lc10, sv10, wa10)

springmintemp_peak <- cbind(cp3,dp3,lc3, sv3, wa3)
springmintemp_timing <- cbind(cp11,dp11,lc11, sv11, wa11)
springmintemp_length <- cbind(cp12,dp12,lc12, sv12, wa12)

#changing column names for plotting
colnames(springmaxtemp_peak) <- c('CP','DP','LC', 'SV', 'WA')
colnames(springmaxtemp_timing) <- c('CP','DP','LC', 'SV', 'WA')
colnames(springmaxtemp_length) <- c('CP','DP','LC', 'SV', 'WA')

colnames(wintprcp_peak) <- c('CP','DP','LC', 'SV', 'WA')
colnames(wintprcp_timing) <- c('CP','DP','LC', 'SV', 'WA')
colnames(wintprcp_length) <- c('CP','DP','LC', 'SV', 'WA')

colnames(springmintemp_peak) <- c('CP','DP','LC', 'SV', 'WA')
colnames(springmintemp_timing) <- c('CP','DP','LC', 'SV', 'WA')
colnames(springmintemp_length) <- c('CP','DP','LC', 'SV', 'WA')




#PLOT

pdf("correlation_plot_showing_correlation_in_effect_of_climate_among_species_present_at_both_compared_sites.pdf")

layout(matrix(c(1,2,3, 4),2,2,byrow=T))

#springmaxtemp_peak
mycorrgram(springmaxtemp_peak, lower.panel = panel.pts,
         upper.panel = panel.cor, main="(a) Effect of spring maximum temperature on peak",
         cex.main = 1.7, cex.labels = 4.0)

#springmaxtemp_timing
mycorrgram(springmaxtemp_timing, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(b) Effect of spring maximum temperature on timing",
           cex.main = 1.7, cex.labels = 4.0)
#springmaxtemp_length
mycorrgram(springmaxtemp_length, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(c) Effect of spring maximum temperature on length",
           cex.main = 1.7, cex.labels = 4.0)


#wintprcp_peak
mycorrgram(wintprcp_peak, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(d) Effect of winter precipitation on peak",
           cex.main = 1.7, cex.labels = 4.0)
#wintprcp_timing
mycorrgram(wintprcp_timing, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(e) Effect of winter precipitation on timing",
           cex.main = 1.7, cex.labels = 4.0)
#wintprcp_length
mycorrgram(wintprcp_length, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(f) Effect of winter precipitation on length",
           cex.main = 1.7, cex.labels = 4.0)


#springmintemp_peak
mycorrgram(springmintemp_peak, lower.panel = panel.pts,
         upper.panel = panel.cor, main="(g) Effect of spring minimum temperature on peak",
         cex.main = 1.7)

#springmintemp_timing
mycorrgram(springmintemp_timing, lower.panel = panel.pts,
         upper.panel = panel.cor, main="(h) Effect of spring minimum temperature on timing",
         cex.main = 1.7, cex.labels = 4.0)

#springmintemp_length
mycorrgram(springmintemp_length, lower.panel = panel.pts,
           upper.panel = panel.cor, main="(i) Effect of spring minimum temperature on length",
           cex.main = 1.7, cex.labels = 4.0)

dev.off()



