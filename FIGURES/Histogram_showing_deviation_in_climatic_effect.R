library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())




#CASTLE PEAK
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Castle Peak.rdat')
csp= length(sp)
cssp<-sp

#mu of posteriors
mus<-extract(fit,"mu")
dim(mus[[1]])

#mcmc output for each variable
#spring max temp
#peak, timing , length
cp_mumc_1 <- mus[[1]][ ,1]
cp_mumc_7 <- mus[[1]][ ,7]
cp_mumc_8 <- mus[[1]][ ,8]
#winter precipitation
#peak, timing , length
cp_mumc_2 <- mus[[1]][ ,2]
cp_mumc_9 <- mus[[1]][ ,9]
cp_mumc_10 <- mus[[1]][ ,10]
#spring min temp
#peak, timing , length
cp_mumc_3 <- mus[[1]][ ,3]
cp_mumc_11 <- mus[[1]][ ,11]
cp_mumc_12 <- mus[[1]][ ,12]



#sigma of posteriors
sigs<-extract(fit,"sigma")
dim(sigs[[1]])

#mcmc output for each variable
#spring max temp
#peak, timing , length
cp_sigsmc_1 <- sigs[[1]][ ,1]
cp_sigsmc_7 <- sigs[[1]][ ,7]
cp_sigsmc_8 <- sigs[[1]][ ,8]
#winter precipitation
#peak, timing , length
cp_sigsmc_2 <- sigs[[1]][ ,2]
cp_sigsmc_9 <- sigs[[1]][ ,9]
cp_sigsmc_10 <- sigs[[1]][ ,10]
#spring min temp
#peak, timing , length
cp_sigsmc_3 <- sigs[[1]][ ,3]
cp_sigsmc_11 <- sigs[[1]][ ,11]
cp_sigsmc_12 <- sigs[[1]][ ,12]



#beta of posteriors
betas<-extract(fit,"beta")
dim(betas[[1]])

#spring max temp
#peak, timing , length
cb1<-betas[[1]][,,1]
cb7<-betas[[1]][,,7]
cb8<-betas[[1]][,,8]

#winter precipitation
#peak, timing , length
cb2<-betas[[1]][,,2]
cb9<-betas[[1]][,,9]
cb10<-betas[[1]][,,10]

#spring min temp
#peak, timing , length
cb3<-betas[[1]][,,3]
cb11<-betas[[1]][,,11]
cb12<-betas[[1]][,,12]


#DONNER PASS
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Donner Pass.rdat')
dsp= length(sp)
dssp<-sp

#mu of posteriors
mus<-extract(fit,"mu")
dim(mus[[1]])

#mcmc output for each variable
#spring max temp
#peak, timing , length
dp_mumc_1 <- mus[[1]][ ,1]
dp_mumc_7 <- mus[[1]][ ,7]
dp_mumc_8 <- mus[[1]][ ,8]

#winter precipitation
#peak, timing , length
dp_mumc_2 <- mus[[1]][ ,2]
dp_mumc_9 <- mus[[1]][ ,9]
dp_mumc_10 <- mus[[1]][ ,10]

#spring min temp
#peak, timing , length
dp_mumc_3<- mus[[1]][ ,3]
dp_mumc_11<- mus[[1]][ ,11]
dp_mumc_12 <- mus[[1]][ ,12]


#sigma of posteriors
sigs<-extract(fit,"sigma")
dim(sigs[[1]])

#mcmc output for each variable
#spring max temp
#peak, timing , length
dp_sigsmc_1 <- sigs[[1]][ ,1]
dp_sigsmc_7 <- sigs[[1]][ ,7]
dp_sigsmc_8 <- sigs[[1]][ ,8]
#winter precipitation
#peak, timing , length
dp_sigsmc_2 <- sigs[[1]][ ,2]
dp_sigsmc_9 <- sigs[[1]][ ,9]
dp_sigsmc_10 <- sigs[[1]][ ,10]
#spring min temp
#peak, timing , length
dp_sigsmc_3<- sigs[[1]][ ,3]
dp_sigsmc_11<- sigs[[1]][ ,11]
dp_sigsmc_12 <- sigs[[1]][ ,12]

#beta of posteriors
betas<-extract(fit,"beta")
#spring max temp
#peak, timing , length
db1<-betas[[1]][,,1]
db7<-betas[[1]][,,7]
db8<-betas[[1]][,,8]

#winter precipitation
#peak, timing , length
db2<-betas[[1]][,,2]
db9<-betas[[1]][,,9]
db10<-betas[[1]][,,10]

#spring min temp
#peak, timing , length
db3<-betas[[1]][,,3]
db11<-betas[[1]][,,11]
db12<-betas[[1]][,,12]


#LANG CROSSING
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Lang Crossing.rdat')
lsp= length(sp)
lssp<-sp

#mu of posteriors
mus<-extract(fit,"mu")
dim(mus[[1]])

#mcmc output for each variable
#spring max temp
#peak, timing , length
lc_mumc_1 <- mus[[1]][ ,1]
lc_mumc_7 <- mus[[1]][ ,7]
lc_mumc_8 <- mus[[1]][ ,8]

#winter precipitation
#peak, timing , length
lc_mumc_2 <- mus[[1]][ ,2]
lc_mumc_9 <- mus[[1]][ ,9]
lc_mumc_10 <- mus[[1]][ ,10]

#spring min temp
#peak, timing , length
lc_mumc_3<- mus[[1]][ ,3]
lc_mumc_11<- mus[[1]][ ,11]
lc_mumc_12 <- mus[[1]][ ,12]

#sigma of posteriors
sigs<-extract(fit,"sigma")
dim(sigs[[1]])

#mcmc output for each variable
#spring max temp
#peak, timing , length
lc_sigsmc_1 <- sigs[[1]][ ,1]
lc_sigsmc_7 <- sigs[[1]][ ,7]
lc_sigsmc_8 <- sigs[[1]][ ,8]
#winter precipitation
#peak, timing , length
lc_sigsmc_2 <- sigs[[1]][ ,2]
lc_sigsmc_9 <- sigs[[1]][ ,9]
lc_sigsmc_10 <- sigs[[1]][ ,10]
#spring min temp
#peak, timing , length
lc_sigsmc_3<- sigs[[1]][ ,3]
lc_sigsmc_11<- sigs[[1]][ ,11]
lc_sigsmc_12 <- sigs[[1]][ ,12]


#beta of posteriors
betas<-extract(fit,"beta")
#spring max temp
#peak, timing , length
lb1<-betas[[1]][,,1]
lb7<-betas[[1]][,,7]
lb8<-betas[[1]][,,8]
#winter precipitation
#peak, timing , length
lb2<-betas[[1]][,,2]
lb9<-betas[[1]][,,9]
lb10<-betas[[1]][,,10]

#spring min temp
#peak, timing , length
lb3<-betas[[1]][,,3]
lb11<-betas[[1]][,,11]
lb12<-betas[[1]][,,12]


#SIERRA VALLEY
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Sierra Valley.rdat')
ssp= length(sp)
sssp<-sp

#mu of posteriors
mus<-extract(fit,"mu")
dim(mus[[1]])

#mcmc output for each variable
#spring max temp
#peak, timing , length
sv_mumc_1 <- mus[[1]][ ,1]
sv_mumc_7 <- mus[[1]][ ,7]
sv_mumc_8 <- mus[[1]][ ,8]
#winter precipitation
#peak, timing , length
sv_mumc_2 <- mus[[1]][ ,2]
sv_mumc_9 <- mus[[1]][ ,9]
sv_mumc_10 <- mus[[1]][ ,10]
#spring min temp
#peak, timing , length
sv_mumc_3<- mus[[1]][ ,3]
sv_mumc_11<- mus[[1]][ ,11]
sv_mumc_12 <- mus[[1]][ ,12]


#sigma of posteriors
sigs<-extract(fit,"sigma")
dim(sigs[[1]])

#mcmc output for each variable
#spring max temp
#peak, timing , length
sv_sigsmc_1 <- sigs[[1]][ ,1]
sv_sigsmc_7 <- sigs[[1]][ ,7]
sv_sigsmc_8 <- sigs[[1]][ ,8]
#winter precipitation
#peak, timing , length
sv_sigsmc_2 <- sigs[[1]][ ,2]
sv_sigsmc_9 <- sigs[[1]][ ,9]
sv_sigsmc_10 <- sigs[[1]][ ,10]
#spring min temp
#peak, timing , length
sv_sigsmc_3<- sigs[[1]][ ,3]
sv_sigsmc_11<- sigs[[1]][ ,11]
sv_sigsmc_12 <- sigs[[1]][ ,12]


#beta of posteriors
betas<-extract(fit,"beta")
#spring max temp
#peak, timing , length
sb1<-betas[[1]][,,1]
sb7<-betas[[1]][,,7]
sb8<-betas[[1]][,,8]

#winter precipitation
#peak, timing , length
sb2<-betas[[1]][,,2]
sb9<-betas[[1]][,,9]
sb10<-betas[[1]][,,10]

#spring min temp
#peak, timing , length
sb3<-betas[[1]][,,3]
sb11<-betas[[1]][,,11]
sb12<-betas[[1]][,,12]

#WASHINGTON
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Washington.rdat')
wsp= length(sp)
wssp<-sp

#mu of posteriors
mus<-extract(fit,"mu")
dim(mus[[1]])

#mcmc output for each variable
#spring max temp
#peak, timing , length
wa_mumc_1 <- mus[[1]][ ,1]
wa_mumc_7 <- mus[[1]][ ,7]
wa_mumc_8 <- mus[[1]][ ,8]
#winter precipitation
#peak, timing , length
wa_mumc_2 <- mus[[1]][ ,2]
wa_mumc_9 <- mus[[1]][ ,9]
wa_mumc_10 <- mus[[1]][ ,10]
#spring min temp
#peak, timing , length
wa_mumc_3 <- mus[[1]][ ,3]
wa_mumc_11 <- mus[[1]][ ,11]
wa_mumc_12 <- mus[[1]][ ,12]


#sigma of posteriors
sigs<-extract(fit,"sigma")
dim(sigs[[1]])

#mcmc output for each variable
#spring max temp
#peak, timing , length
wa_sigsmc_1 <- sigs[[1]][ ,1]
wa_sigsmc_7 <- sigs[[1]][ ,7]
wa_sigsmc_8 <- sigs[[1]][ ,8]
#winter precipitation
#peak, timing , length
wa_sigsmc_2 <- sigs[[1]][ ,2]
wa_sigsmc_9 <- sigs[[1]][ ,9]
wa_sigsmc_10 <- sigs[[1]][ ,10]
#spring min temp
#peak, timing , length
wa_sigsmc_3<- sigs[[1]][ ,3]
wa_sigsmc_11<- sigs[[1]][ ,11]
wa_sigsmc_12 <- sigs[[1]][ ,12]


#beta of posteriors
betas<-extract(fit,"beta")
#spring max temp
#peak, timing , length
wb1<-betas[[1]][,,1]
wb7<-betas[[1]][,,7]
wb8<-betas[[1]][,,8]
#winter precipitation
#peak, timing , length
wb2<-betas[[1]][,,2]
wb9<-betas[[1]][,,9]
wb10<-betas[[1]][,,10]
#spring min temp
#peak, timing , length
wb3<-betas[[1]][,,3]
wb11<-betas[[1]][,,11]
wb12<-betas[[1]][,,12]




#########################################################################
###################average species deviation across site########################
################################################################
# Total number of unique species across all sites
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
matching_indices_1 <- match(cssp, unsp)   #which(unsp %in% cssp)



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
matching_indices_2 <- match(dssp, unsp)



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
matching_indices_3 <- match(lssp, unsp)



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
matching_indices_4 <- match(sssp, unsp)




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
matching_indices_5 <- match(wssp, unsp)



springmaxtemp_peak_vec <- c()
springmaxtemp_timing_vec <- c()
springmaxtemp_length_vec <- c()

wintprcp_peak_vec<- c()
wintprcp_timing_vec <- c()
wintprcp_length_vec <- c()

springmintemp_peak_vec <- c()
springmintemp_timing_vec <- c()
springmintemp_length_vec <- c()


for (i in 1:8000) {
  
# Assign the betas of species to the corresponding positions of species present at castle  peak
cp1[matching_indices_1] <- cb1[i,] 
cp7[matching_indices_1] <- cb7[i,]
cp8[matching_indices_1] <- cb8[i,]

cp2[matching_indices_1] <- cb2[i,] 
cp9[matching_indices_1] <- cb9[i,] 
cp10[matching_indices_1] <- cb10[i,]  

cp3[matching_indices_1] <- cb3[i,]
cp11[matching_indices_1] <- cb11[i,]
cp12[matching_indices_1] <- cb12[i,]

# Assign the betas of species to the corresponding positions of species present at donnerpass
dp1[matching_indices_2] <- db1[i,] 
dp7[matching_indices_2] <- db7[i,] 
dp8[matching_indices_2] <- db8[i,] 

dp2[matching_indices_2] <- db2[i,] 
dp9[matching_indices_2] <- db9[i,] 
dp10[matching_indices_2] <- db10[i,]

dp3[matching_indices_2] <- db3[i,]
dp11[matching_indices_2] <- db11[i,]
dp12[matching_indices_2] <- db12[i,] 
  

# Assign the betas of species to the corresponding positions of species present at lang crossing
lc1[matching_indices_3] <- lb1[i,] 
lc7[matching_indices_3] <- lb7[i,] 
lc8[matching_indices_3] <- lb8[i,] 

lc2[matching_indices_3] <- lb2[i,] 
lc9[matching_indices_3] <- lb9[i,] 
lc10[matching_indices_3] <- lb10[i,]  

lc3[matching_indices_3] <- lb3[i,] 
lc11[matching_indices_3] <- lb11[i,] 
lc12[matching_indices_3] <- lb12[i,] 


# Assign the betas of species to the corresponding positions of species present at sierravalley
sv1[matching_indices_4] <- sb1[i,] 
sv7[matching_indices_4] <- sb7[i,] 
sv8[matching_indices_4] <- sb8[i,] 

sv2[matching_indices_4] <- sb2[i,] 
sv9[matching_indices_4] <- sb9[i,] 
sv10[matching_indices_4] <- sb10[i,] 

sv3[matching_indices_4] <- sb3[i,]
sv11[matching_indices_4] <- sb11[i,]
sv12[matching_indices_4] <- sb12[i,] 

# Assign the betas of species to the corresponding positions of species present at washington 
wa1[matching_indices_5] <- wb1[i,]
wa7[matching_indices_5] <- wb7[i,]
wa8[matching_indices_5] <- wb8[i,]

wa2[matching_indices_5] <- wb2[i,] 
wa9[matching_indices_5] <- wb9[i,]
wa10[matching_indices_5] <- wb10[i,] 

wa3[matching_indices_5] <- wb3[i,]
wa11[matching_indices_5] <- wb11[i,]
wa12[matching_indices_5] <- wb12[i,] 
  


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


#standard deviation each species response across all sites  
springmaxtemp_peak <- apply(springmaxtemp_peak, 1, sd)
springmaxtemp_timing <- apply(springmaxtemp_timing, 1, sd)
springmaxtemp_length <- apply(springmaxtemp_length, 1, sd)

wintprcp_peak <- apply(wintprcp_peak, 1, sd)
wintprcp_timing <- apply(wintprcp_timing, 1, sd)
wintprcp_length <- apply(wintprcp_length, 1, sd)

springmintemp_peak <- apply(springmintemp_peak, 1, sd)
springmintemp_timing <- apply(springmintemp_timing, 1, sd)
springmintemp_length <- apply(springmintemp_length, 1, sd)


springmaxtemp_peak_vec[i] <- mean(springmaxtemp_peak, na.rm = TRUE)
springmaxtemp_timing_vec[i]<- mean(springmaxtemp_timing, na.rm = TRUE)
springmaxtemp_length_vec[i] <- mean(springmaxtemp_length, na.rm = TRUE)

wintprcp_peak_vec[i] <- mean(wintprcp_peak, na.rm = TRUE)
wintprcp_timing_vec[i]<- mean(wintprcp_timing, na.rm = TRUE)
wintprcp_length_vec[i] <- mean(wintprcp_length, na.rm = TRUE)

springmintemp_peak_vec[i] <- mean(springmintemp_peak, na.rm = TRUE)
springmintemp_timing_vec[i]<- mean(springmintemp_timing, na.rm = TRUE)
springmintemp_length_vec[i] <- mean(springmintemp_length, na.rm = TRUE)


}


#median and confidence interval
sd_beta <- cbind(springmaxtemp_peak_vec, springmaxtemp_timing_vec, springmaxtemp_length_vec, 
                 wintprcp_peak_vec, wintprcp_timing_vec, wintprcp_length_vec, 
                 springmintemp_peak_vec, springmintemp_timing_vec, springmintemp_length_vec)
sd_beta_md <- apply(sd_beta, 2, quantile, probs = c(0.5, 0.05, 0.95), na.rm = TRUE)


############ across sites #######################
#matrix of mcmc for each variable at all sites
mumc_1 <-cbind(cp_mumc_1,dp_mumc_1,lc_mumc_1,sv_mumc_1,wa_mumc_1)
mumc_7 <-cbind(cp_mumc_7,dp_mumc_7,lc_mumc_7,sv_mumc_7,wa_mumc_7)
mumc_8 <-cbind(cp_mumc_8,dp_mumc_8,lc_mumc_8,sv_mumc_8,wa_mumc_8)

mumc_2 <-cbind(cp_mumc_2,dp_mumc_2,lc_mumc_2,sv_mumc_2,wa_mumc_2)
mumc_9 <-cbind(cp_mumc_9,dp_mumc_9,lc_mumc_9,sv_mumc_9,wa_mumc_9)
mumc_10 <-cbind(cp_mumc_10,dp_mumc_10,lc_mumc_10,sv_mumc_10,wa_mumc_10)

mumc_3 <-cbind(cp_mumc_3,dp_mumc_3,lc_mumc_3,sv_mumc_3,wa_mumc_3)
mumc_11 <-cbind(cp_mumc_11,dp_mumc_11,lc_mumc_11,sv_mumc_11,wa_mumc_11)
mumc_12 <-cbind(cp_mumc_12,dp_mumc_12,lc_mumc_12,sv_mumc_12,wa_mumc_12)


#standard deviation for each variable at all sites
sd_mumc_1 <- apply(mumc_1, 1, sd)
sd_mumc_7 <- apply(mumc_7, 1, sd)
sd_mumc_8 <- apply(mumc_8, 1, sd)

sd_mumc_2 <- apply(mumc_2, 1, sd)
sd_mumc_9 <- apply(mumc_9, 1, sd)
sd_mumc_10 <- apply(mumc_10, 1, sd)

sd_mumc_3 <- apply(mumc_3, 1, sd)
sd_mumc_11 <- apply(mumc_11, 1, sd)
sd_mumc_12 <- apply(mumc_12, 1, sd)


#median and confidence interval
sd_mumc <- cbind(sd_mumc_1, sd_mumc_7, sd_mumc_8, 
                 sd_mumc_2, sd_mumc_9, sd_mumc_10, 
                 sd_mumc_3, sd_mumc_11, sd_mumc_12)
sd_mumc_md <- apply(sd_mumc, 2, quantile, probs = c(0.5, 0.05, 0.95))




############ within sites #######################
#matrix of mcmc for each variable at all sites
sigsmc_1 <-cbind(cp_sigsmc_1,dp_sigsmc_1,lc_sigsmc_1,sv_sigsmc_1,wa_sigsmc_1)
sigsmc_7 <-cbind(cp_sigsmc_7,dp_sigsmc_7,lc_sigsmc_7,sv_sigsmc_7,wa_sigsmc_7)
sigsmc_8 <-cbind(cp_sigsmc_8,dp_sigsmc_8,lc_sigsmc_8,sv_sigsmc_8,wa_sigsmc_8)

sigsmc_2 <-cbind(cp_sigsmc_2,dp_sigsmc_2,lc_sigsmc_2,sv_sigsmc_2,wa_sigsmc_2)
sigsmc_9 <-cbind(cp_sigsmc_9,dp_sigsmc_9,lc_sigsmc_9,sv_sigsmc_9,wa_sigsmc_9)
sigsmc_10 <-cbind(cp_sigsmc_10,dp_sigsmc_10,lc_sigsmc_10,sv_sigsmc_10,wa_sigsmc_10)

sigsmc_3 <-cbind(cp_sigsmc_3,dp_sigsmc_3,lc_sigsmc_3,sv_sigsmc_3,wa_sigsmc_3)
sigsmc_11 <-cbind(cp_sigsmc_11,dp_sigsmc_11,lc_sigsmc_11,sv_sigsmc_11,wa_sigsmc_11)
sigsmc_12 <-cbind(cp_sigsmc_12,dp_sigsmc_12,lc_sigsmc_12,sv_sigsmc_12,wa_sigsmc_12)


#average deviation for each variable at all sites
sd_sigsmc_1 <- apply(sigsmc_1, 1, mean)
sd_sigsmc_7 <- apply(sigsmc_7, 1, mean)
sd_sigsmc_8 <- apply(sigsmc_8, 1, mean)

sd_sigsmc_2 <- apply(sigsmc_2, 1, mean)
sd_sigsmc_9 <- apply(sigsmc_9, 1, mean)
sd_sigsmc_10 <- apply(sigsmc_10, 1, mean)

sd_sigsmc_3 <- apply(sigsmc_3, 1, mean)
sd_sigsmc_11 <- apply(sigsmc_11, 1, mean)
sd_sigsmc_12 <- apply(sigsmc_12, 1, mean)


#median and confidence interval
sd_sigsmc <- cbind(sd_sigsmc_1, sd_sigsmc_7, sd_sigsmc_8,
                   sd_sigsmc_2, sd_sigsmc_9, sd_sigsmc_10,
                   sd_sigsmc_3, sd_sigsmc_11, sd_sigsmc_12)
sd_sigsmc_md <- apply(sd_sigsmc, 2, quantile, probs = c(0.5, 0.05, 0.95))





#average species deviation across site
spec_md <- c(sd_beta_md[1,1], sd_beta_md[1,2], sd_beta_md[1,3], 
             sd_beta_md[1,4], sd_beta_md[1,5], sd_beta_md[1,6], 
             sd_beta_md[1,7], sd_beta_md[1,8], sd_beta_md[1,9])
spec_lw <- c(sd_beta_md[2,1], sd_beta_md[2,2], sd_beta_md[2,3], 
             sd_beta_md[2,4], sd_beta_md[2,5], sd_beta_md[2,6],
             sd_beta_md[2,7], sd_beta_md[2,8], sd_beta_md[2,9])
spec_up <- c(sd_beta_md[3,1], sd_beta_md[3,2], sd_beta_md[3,3], 
             sd_beta_md[3,4], sd_beta_md[3,5], sd_beta_md[3,6], 
             sd_beta_md[3,7], sd_beta_md[3,8], sd_beta_md[3,9])

#across sites
across_md <- c(sd_mumc_md[1,1], sd_mumc_md[1,2], sd_mumc_md[1,3],
               sd_mumc_md[1,4], sd_mumc_md[1,5], sd_mumc_md[1,6],
               sd_mumc_md[1,7], sd_mumc_md[1,8], sd_mumc_md[1,9])
across_lw <- c(sd_mumc_md[2,1], sd_mumc_md[2,2], sd_mumc_md[2,3], 
               sd_mumc_md[2,4], sd_mumc_md[2,5], sd_mumc_md[2,6],
               sd_mumc_md[2,7], sd_mumc_md[2,8], sd_mumc_md[2,9])
across_up <- c(sd_mumc_md[3,1], sd_mumc_md[3,2], sd_mumc_md[3,3], 
               sd_mumc_md[3,4], sd_mumc_md[3,5], sd_mumc_md[3,6], 
               sd_mumc_md[3,7], sd_mumc_md[3,8], sd_mumc_md[3,9])

#within sites
within_md <- c(sd_sigsmc_md[1,1], sd_sigsmc_md[1,2], sd_sigsmc_md[1,3], 
               sd_sigsmc_md[1,4], sd_sigsmc_md[1,5], sd_sigsmc_md[1,6], 
               sd_sigsmc_md[1,7], sd_sigsmc_md[1,8], sd_sigsmc_md[1,9])
within_lw <- c(sd_sigsmc_md[2,1], sd_sigsmc_md[2,2], sd_sigsmc_md[2,3],
               sd_sigsmc_md[2,4], sd_sigsmc_md[2,5], sd_sigsmc_md[2,6], 
               sd_sigsmc_md[2,7], sd_sigsmc_md[2,8], sd_sigsmc_md[2,9])
within_up <- c(sd_sigsmc_md[3,1], sd_sigsmc_md[3,2], sd_sigsmc_md[3,3], 
               sd_sigsmc_md[3,4], sd_sigsmc_md[3,5], sd_sigsmc_md[3,6], 
               sd_sigsmc_md[3,7], sd_sigsmc_md[3,8], sd_sigsmc_md[3,9])





#plot comparing deviation  across sites and within sites
pdf(paste("Histogram_new_showing_deviation_in_effects_of_climate.pdf",sep=""),width=13,height=9)
layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=T))
par(oma= c(1,1,1,0), mar=c(3,5,3,2))

#Midseason occurrence
dev <- rbind(within_md[1] , across_md[1], spec_md[1])
bar <- barplot(dev, beside = TRUE, col = c("#c6d4e1", "#44749d", "#4169e1"),
               names.arg = "",
               main = "",
               xlab = "",
               ylab = expression(paste(sigma," of effects")), ylim = c(0, 1.6), cex.lab=1.8, cex.names = 1.5, cex.axis= 1.5, cex.main = 1.5)
mtext("(a) Spring max temperature (MO)", side = 3, line = 1, adj = 0, cex = 1.5) 
#Add error bars to each bar
arrows(bar[1], within_lw[1] , bar[1], within_up[1] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[2], across_lw[1] , bar[2], across_up[1] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[3], spec_lw[1] , bar[3], spec_up[1] , lwd = 1.5, angle = 90, code = 3, length = 0.05)

#Midseason occurrence
dev <- rbind(within_md[4] , across_md[4], spec_md[4])
bar <- barplot(dev, beside = TRUE, col = c("#c6d4e1", "#44749d", "#4169e1"),
               names.arg = "",
               main = "",
               xlab = "",
               ylab = expression(paste(sigma," of effects")), ylim = c(0, 1.6), cex.lab=1.8, cex.names = 1.5, cex.axis= 1.5, cex.main = 1.5)
mtext("(b) Winter precipitation (MO)", side = 3, line = 1, adj = 0, cex = 1.5) 
# error bars to each bar
arrows(bar[1], within_lw[4] , bar[1], within_up[4] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[2], across_lw[4] , bar[2], across_up[4] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[3], spec_lw[4] , bar[3], spec_up[4] , lwd = 1.5, angle = 90, code = 3, length = 0.05)


#Midseason occurrence
dev <- rbind(within_md[7] , across_md[7], spec_md[7])
bar <- barplot(dev, beside = TRUE, col = c("#c6d4e1", "#44749d", "#4169e1"),
               names.arg = "",
               main = "",
               xlab = "",
               ylab = expression(paste(sigma," of effects")), ylim = c(0, 1.6), cex.lab=1.8, cex.names = 1.5, cex.axis= 1.5, cex.main = 1.5)
mtext("(c) Spring min temperature (MO)", side = 3, line = 1, adj = 0, cex = 1.5) 
#error bars to each bar
arrows(bar[1], within_lw[7] , bar[1], within_up[7] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[2], across_lw[7] , bar[2], across_up[7] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[3], spec_lw[7] , bar[3], spec_up[7] , lwd = 1.5, angle = 90, code = 3, length = 0.05)

legend("topright", legend = c("Among species within sites", "Average effect across sites", "Species effect across sites"),
       fill = c("#c6d4e1", "#44749d", "#4169e1"), cex = 1.4, bty = "n")

# Timing of occurrence
dev <- rbind(within_md[2] , across_md[2], spec_md[2])
# barplot
bar <- barplot(dev, beside = TRUE, col = c("#c6d4e1", "#44749d", "#4169e1"),
               names.arg = "",
               main = "",
               xlab = "",
               ylab = expression(paste(sigma," of effects")), ylim = c(0, 1.6), cex.lab=1.8, cex.names = 1.5, cex.axis= 1.5, cex.main = 1.5)
mtext("(d) Spring max temperature (TO)", side = 3, line = 1, adj = 0, cex = 1.5) 
#error bars to each bar
arrows(bar[1], within_lw[2] , bar[1], within_up[2] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[2], across_lw[2] , bar[2], across_up[2] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[3], spec_lw[2] , bar[3], spec_up[2] , lwd = 1.5, angle = 90, code = 3, length = 0.05)


dev <- rbind(within_md[5], across_md[5], spec_md[5])
# barplot
bar <- barplot(dev, beside = TRUE, col = c("#c6d4e1", "#44749d", "#4169e1"),
               names.arg = "",
               main = "",
               xlab = "",
               ylab = expression(paste(sigma," of effects")), ylim = c(0, 1.6), cex.lab=1.8, cex.names = 1.5, cex.axis= 1.5, cex.main = 1.5)
mtext("(e) Winter precipitation (TO)", side = 3, line = 1, adj = 0, cex = 1.5) 
arrows(bar[1], within_lw[5] , bar[1], within_up[5] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[2], across_lw[5] , bar[2], across_up[5] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[3], spec_lw[5] , bar[3], spec_up[5] , lwd = 1.5, angle = 90, code = 3, length = 0.05)

dev <- rbind(within_md[8] , across_md[8], spec_md[8])
# barplot
bar <- barplot(dev, beside = TRUE, col = c("#c6d4e1", "#44749d", "#4169e1"),
               names.arg = "",
               main = "",
               xlab = "",
               ylab = expression(paste(sigma," of effects")), ylim = c(0, 1.6), cex.lab=1.8, cex.names = 1.5, cex.axis= 1.5, cex.main = 1.5)
mtext("(f) Spring min temperature (TO)", side = 3, line = 1, adj = 0, cex = 1.5) 
arrows(bar[1], within_lw[8] , bar[1], within_up[8] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[2], across_lw[8] , bar[2], across_up[8] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[3], spec_lw[8] , bar[3], spec_up[8] , lwd = 1.5, angle = 90, code = 3, length = 0.05)

# Length of occurrence
dev <- rbind(within_md[3], across_md[3], spec_md[3])
# barplot
bar <- barplot(dev, beside = TRUE, col = c("#c6d4e1", "#44749d", "#4169e1"),
               names.arg = "",
               main = "",
               xlab = "",
               ylab = expression(paste(sigma," of effects")), ylim = c(0, 1.6), cex.lab=1.8, cex.names = 1.5, cex.axis= 1.5, cex.main = 1.5)
mtext("(g) Spring max temperature (LO)", side = 3, line = 1, adj = 0, cex = 1.5) 
#error bars to each bar
arrows(bar[1], within_lw[3] , bar[1], within_up[3] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[2], across_lw[3] , bar[2], across_up[3] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[3], spec_lw[3] , bar[3], spec_up[3] , lwd = 1.5, angle = 90, code = 3, length = 0.05)


dev <- rbind(within_md[6], across_md[6], spec_md[6])
# barplot
bar <- barplot(dev, beside = TRUE, col = c("#c6d4e1", "#44749d", "#4169e1"),
               names.arg = "",
               main = "",
               xlab = "",
               ylab = expression(paste(sigma," of effects")), ylim = c(0, 1.6), cex.lab=1.8, cex.names = 1.5, cex.axis= 1.5, cex.main = 1.5)
mtext("(h) Winter precipitation (LO)", side = 3, line = 1, adj = 0, cex = 1.5) 
arrows(bar[1], within_lw[6] , bar[1], within_up[6] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[2], across_lw[6] , bar[2], across_up[6] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[3], spec_lw[6] , bar[3], spec_up[6] , lwd = 1.5, angle = 90, code = 3, length = 0.05)


dev <- rbind(within_md[9], across_md[9], spec_md[9])
# barplot
bar <- barplot(dev, beside = TRUE, col = c("#c6d4e1", "#44749d", "#4169e1"),
               names.arg = "",
               main = "",
               xlab = "",
               ylab = expression(paste(sigma," of effects")), ylim = c(0, 1.6), cex.lab=1.8, cex.names = 1.5, cex.axis= 1.5, cex.main = 1.5)
mtext("(i) Spring min temperature (LO)", side = 3, line = 1, adj = 0, cex = 1.5) 
arrows(bar[1], within_lw[9] , bar[1], within_up[9] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[2], across_lw[9] , bar[2], across_up[9] , lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(bar[3], spec_lw[9] , bar[3], spec_up[9] , lwd = 1.5, angle = 90, code = 3, length = 0.05)

dev.off()
