library(rstan)
library(scales)

#CASTLE PEAK
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Castle Peak.rdat')
csp= length(sp)
cssp = sp
#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
cb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)

#timing
cb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)

#length
cb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#peak
cb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)

#timing
cb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)

#length
cb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#peak
cb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)

#timing
cb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)

#length
cb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


cpspbe <- cbind(cb1, cb7, cb8, cb2, cb9, cb10, cb3, cb11, cb12)
colnames(cpspbe) <- c("sprmaxtemp-Peak", "sprmaxtemp-Timing", "sprmaxtemp-Length",
                      "wintprcp-Peak", "wintprcp-Timing", "wintprcp-Length",
                      "sprmintemp-Peak", "sprmintemp-Timing", "sprmintemp-Length")
rownames(cpspbe) <- sp
# PCA using base function - prcomp()
cppca <- prcomp(cpspbe, scale=TRUE)
# Summary
sumcp <- summary(cppca)

#all details
#unclass(cppca)

# Screeplot
layout(matrix(1:2, ncol=2))
screeplot(cppca)
screeplot(cppca, type="lines")



#DONNER PASS
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Donner Pass.rdat')
dsp= length(sp)
dssp = sp
#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
db1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)

#timing
db7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)

#length
db8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#peak
db2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)

#timing
db9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)

#length
db10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#peak
db3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)

#timing
db11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)

#length
db12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


dpspbe <- cbind(db1, db7, db8, db2, db9, db10, db3, db11, db12)
colnames(dpspbe) <- c("sprmaxtemp-Peak", "sprmaxtemp-Timing", "sprmaxtemp-Length",
                      "wintprcp-Peak", "wintprcp-Timing", "wintprcp-Length",
                      "sprmintemp-Peak", "sprmintemp-Timing", "sprmintemp-Length")
rownames(dpspbe) <- sp
# PCA using base function - prcomp()
dppca <- prcomp(dpspbe, scale=TRUE)
# Summary
sumdp <- summary(dppca)

#all details
#unclass(dppca)

# Screeplot
layout(matrix(1:2, ncol=2))
screeplot(dppca)
screeplot(dppca, type="lines")


#LANG CROSSING
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Lang Crossing.rdat')
lsp= length(sp)
lssp = sp
#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
lb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)

#timing
lb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)

#length
lb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#peak
lb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)

#timing
lb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)

#length
lb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#peak
lb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)

#timing
lb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)

#length
lb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


lcspbe <- cbind(lb1, lb7, lb8, lb2, lb9, lb10, lb3, lb11, lb12)

colnames(lcspbe) <- c("sprmaxtemp-Peak", "sprmaxtemp-Timing", "sprmaxtemp-Length",
                      "wintprcp-Peak", "wintprcp-Timing", "wintprcp-Length",
                      "sprmintemp-Peak", "sprmintemp-Timing", "sprmintemp-Length")

rownames(lcspbe) <- sp
# PCA using base function - prcomp()
lcpca <- prcomp(lcspbe, scale=TRUE)
# Summary
sumlc <- summary(lcpca)

#all details
#unclass(lcpca)

# Screeplot
layout(matrix(1:2, ncol=2))
screeplot(lcpca)
screeplot(lcpca, type="lines")



#SIERRA VALLEY
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Sierra Valley.rdat')
ssp= length(sp)
sssp = sp
#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
sb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)

#timing
sb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)

#length
sb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#peak
sb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)

#timing
sb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)

#length
sb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#peak
sb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)

#timing
sb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)

#length
sb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


svspbe <- cbind(sb1, sb7, sb8, sb2, sb9, sb10, sb3, sb11, sb12)

colnames(svspbe) <- c("sprmaxtemp-Peak", "sprmaxtemp-Timing", "sprmaxtemp-Length",
                      "wintprcp-Peak", "wintprcp-Timing", "wintprcp-Length",
                      "sprmintemp-Peak", "sprmintemp-Timing", "sprmintemp-Length")
rownames(svspbe) <- sp
# PCA using base function - prcomp()
svpca <- prcomp(svspbe, scale=TRUE)
# Summary
sumsv <- summary(svpca)

#all details
#unclass(svpca)

# Screeplot
layout(matrix(1:2, ncol=2))
screeplot(svpca)
screeplot(svpca, type="lines")


#WASHINGTON
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Washington.rdat')
wsp= length(sp)
wssp = sp
#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
wb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)

#timing
wb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)

#length
wb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#peak
wb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)

#timing
wb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)

#length
wb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#peak
wb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)

#timing
wb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)

#length
wb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


waspbe <- cbind(wb1, wb7, wb8, wb2, wb9, wb10, wb3, wb11, wb12)

colnames(waspbe) <- c("sprmaxtemp-Peak", "sprmaxtemp-Timing", "sprmaxtemp-Length",
                      "wintprcp-Peak", "wintprcp-Timing", "wintprcp-Length",
                      "sprmintemp-Peak", "sprmintemp-Timing", "sprmintemp-Length")

rownames(waspbe) <- sp
# PCA using base function - prcomp()
wapca <- prcomp(waspbe, scale=TRUE)
# Summary
sumwa <- summary(wapca)

#all details
#unclass(wapca)

# Screeplot
layout(matrix(1:2, ncol=2))
screeplot(wapca)
screeplot(wapca, type="lines")



#########################################################################
###################species present at all site########################
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



# Assign the betas of species to the corresponding positions of species present at castle  peak
cp1[matching_indices_1] <- cb1
cp7[matching_indices_1] <- cb7
cp8[matching_indices_1] <- cb8

cp2[matching_indices_1] <- cb2 
cp9[matching_indices_1] <- cb9 
cp10[matching_indices_1] <- cb10  

cp3[matching_indices_1] <- cb3
cp11[matching_indices_1] <- cb11
cp12[matching_indices_1] <- cb12

# Assign the betas of species to the corresponding positions of species present at donnerpass
dp1[matching_indices_2] <- db1 
dp7[matching_indices_2] <- db7 
dp8[matching_indices_2] <- db8 

dp2[matching_indices_2] <- db2 
dp9[matching_indices_2] <- db9 
dp10[matching_indices_2] <- db10

dp3[matching_indices_2] <- db3
dp11[matching_indices_2] <- db11
dp12[matching_indices_2] <- db12 


# Assign the betas of species to the corresponding positions of species present at lang crossing
lc1[matching_indices_3] <- lb1 
lc7[matching_indices_3] <- lb7 
lc8[matching_indices_3] <- lb8 

lc2[matching_indices_3] <- lb2 
lc9[matching_indices_3] <- lb9 
lc10[matching_indices_3] <- lb10  

lc3[matching_indices_3] <- lb3 
lc11[matching_indices_3] <- lb11 
lc12[matching_indices_3] <- lb12 


# Assign the betas of species to the corresponding positions of species present at sierravalley
sv1[matching_indices_4] <- sb1 
sv7[matching_indices_4] <- sb7 
sv8[matching_indices_4] <- sb8 

sv2[matching_indices_4] <- sb2 
sv9[matching_indices_4] <- sb9 
sv10[matching_indices_4] <- sb10 

sv3[matching_indices_4] <- sb3
sv11[matching_indices_4] <- sb11
sv12[matching_indices_4] <- sb12 

# Assign the betas of species to the corresponding positions of species present at washington 
wa1[matching_indices_5] <- wb1
wa7[matching_indices_5] <- wb7
wa8[matching_indices_5] <- wb8

wa2[matching_indices_5] <- wb2 
wa9[matching_indices_5] <- wb9
wa10[matching_indices_5] <- wb10 

wa3[matching_indices_5] <- wb3
wa11[matching_indices_5] <- wb11
wa12[matching_indices_5] <- wb12 


allbeta <- cbind(cp1,dp1,lc1,sv1,wa1,cp7,dp7,lc7,sv7,wa7,cp8,dp8,lc8,sv8,wa8,
                 cp2,dp2,lc2,sv2,wa2,cp9,dp9,lc9,sv9,wa9,cp10,dp10,lc10,sv10,wa10,
                 cp3,dp3,lc3,sv3,wa3,cp11,dp11,lc11,sv11,wa11,cp12,dp12,lc12,sv12,wa12)
allbeta <- as.data.frame(allbeta)
rownames(allbeta) <- unsp
#beta for species present at all sites
sp_allst_beta <- allbeta[complete.cases(allbeta), ]
row.names(sp_allst_beta)


# PCA using base function - prcomp()
sp_allst_pca <- prcomp(sp_allst_beta, scale=TRUE)
# Summary
sum_sp_allst <- summary(sp_allst_pca)

#all details
#unclass(sp_allst_pca)

# Screeplot
layout(matrix(1:2, ncol=2))
screeplot(sp_allst_pca)
screeplot(sp_allst_pca, type="lines")


###############################################################################
#######################Coloring by natural history#############################
###############################################################################



#natural history data
nath <- read.csv("natural_history_2024.csv")

nath <- subset(nath,
#when its a resident but we dont know its overwintering stage
  !(resident == "yes" & is.na(wintering)) &
#when we dont know its residency status but we know its overwintering stage
  !(is.na(resident) & !is.na(wintering)) &
#when its not a resident but we know its overwintering stage
  !(resident == "no" & !(wintering %in% c("no", NA)))
)

#converting to just univoltine and multivoltine
nath$broods[!is.na(nath$broods) & nath$broods != "one"] <- "multiple"

#converting hostgenera into monophagous and polyphagous
nath$hostgenera <- 
  ifelse(is.na(nath$hostgenera), NA,         # keep NA
  ifelse(nath$hostgenera == 1, "one",        # 1 → "one"
  ifelse(nath$hostgenera > 1, "multiple",    # >1 → "multiple"
                nath$hostgenera))                   # keep 0 or anything else as-is
)




nath_cp <- nath[nath$site_name == "Castle Peak", ]
nath_dp <- nath[nath$site_name == "Donner Pass", ]
nath_sv <- nath[nath$site_name == "Sierra Valley", ]
nath_lc <- nath[nath$site_name == "Lang Crossing", ]
nath_wa <- nath[nath$site_name == "Washington", ]



###################################################################
############################# Residency #############################
########################################################################
#Resident
resnath_cp <- nath_cp[which(nath_cp$resident == "yes"), ]
resspp_cp <- unique(resnath_cp$genus_species)
res_1 <- na.omit(match(resspp_cp, cssp))

resnath_dp <- nath_dp[which(nath_dp$resident=="yes"), ]
resspp_dp <- unique(resnath_dp$genus_species)
res_2 <- na.omit(match(resspp_dp, dssp))

resnath_lc <- nath_lc[which(nath_lc$resident=="yes"), ]
resspp_lc <- unique(resnath_lc$genus_species)
res_3 <- na.omit(match(resspp_lc, lssp))

resnath_sv <- nath_sv[which(nath_sv$resident=="yes"), ]
resspp_sv <- unique(resnath_sv$genus_species)
res_4 <- na.omit(match(resspp_sv, sssp))

resnath_wa <- nath_wa[which(nath_wa$resident=="yes"), ]
resspp_wa <- unique(resnath_wa$genus_species)
res_5<- na.omit(match(resspp_wa, wssp))


######################################################NonResident
nonresnath_cp <- nath_cp[which(nath_cp$resident=="no"), ]
nonresspp_cp <- unique(nonresnath_cp$genus_species)
nonres_1 <- na.omit(match(nonresspp_cp, cssp))

nonresnath_dp <- nath_dp[which(nath_dp$resident=="no"), ]
nonresspp_dp <- unique(nonresnath_dp$genus_species)
nonres_2 <- na.omit(match(nonresspp_dp, dssp))

nonresnath_lc <- nath_lc[which(nath_lc$resident=="no"), ]
nonresspp_lc <- unique(nonresnath_lc$genus_species)
nonres_3 <- na.omit(match(nonresspp_lc, lssp))

nonresnath_sv <- nath_sv[which(nath_sv$resident=="no"), ]
nonresspp_sv <- unique(nonresnath_sv$genus_species)
nonres_4 <- na.omit(match(nonresspp_sv, sssp))

nonresnath_wa <- nath_wa[which(nath_wa$resident=="no"), ]
nonresspp_wa <- unique(nonresnath_wa$genus_species)
nonres_5<- na.omit(match(nonresspp_wa, wssp))


###################################################################
############################# Weedy or not #############################
########################################################################
############################weedy
wednath_cp <- nath_cp[which(nath_cp$weedy==1), ]
wedspp_cp <- unique(wednath_cp$genus_species)
wed_1 <- na.omit(match(wedspp_cp, cssp))

wednath_dp <- nath_dp[which(nath_dp$weedy==1), ]
wedspp_dp <- unique(wednath_dp$genus_species)
wed_2 <- na.omit(match(wedspp_dp, dssp))

wednath_lc <- nath_lc[which(nath_lc$weedy==1), ]
wedspp_lc <- unique(wednath_lc$genus_species)
wed_3 <- na.omit(match(wedspp_lc, lssp))

wednath_sv <- nath_sv[which(nath_sv$weedy==1), ]
wedspp_sv <- unique(wednath_sv$genus_species)
wed_4 <- na.omit(match(wedspp_sv, sssp))

wednath_wa <- nath_wa[which(nath_wa$weedy==1), ]
wedspp_wa <- unique(wednath_wa$genus_species)
wed_5<- na.omit(match(wedspp_wa, wssp))

#nonweedy
nonwednath_cp <- nath_cp[which(nath_cp$weedy==0), ]
nonwedspp_cp <- unique(nonwednath_cp$genus_species)
nonwed_1 <- na.omit(match(nonwedspp_cp, cssp))

nonwednath_dp <- nath_dp[which(nath_dp$weedy==0), ]
nonwedspp_dp <- unique(nonwednath_dp$genus_species)
nonwed_2 <- na.omit(match(nonwedspp_dp, dssp))

nonwednath_lc <- nath_lc[which(nath_lc$weedy==0), ]
nonwedspp_lc <- unique(nonwednath_lc$genus_species)
nonwed_3 <- na.omit(match(nonwedspp_lc, lssp))

nonwednath_sv <- nath_sv[which(nath_sv$weedy==0), ]
nonwedspp_sv <- unique(nonwednath_sv$genus_species)
nonwed_4 <- na.omit(match(nonwedspp_sv, sssp))

nonwednath_wa <- nath_wa[which(nath_wa$weedy==0), ]
nonwedspp_wa <- unique(nonwednath_wa$genus_species)
nonwed_5<- na.omit(match(nonwedspp_wa, wssp))


###################################################################
############################# voltinism #############################
########################################################################
#####################################voltinism(one brood)
volonenath_cp <- nath_cp[which(nath_cp$broods=="one"), ]
volonespp_cp <- unique(volonenath_cp$genus_species)
volone_1 <- na.omit(match(volonespp_cp, cssp))

volonenath_dp <- nath_dp[which(nath_dp$broods=="one"), ]
volonespp_dp <- unique(volonenath_dp$genus_species)
volone_2 <- na.omit(match(volonespp_dp, dssp))

volonenath_lc <- nath_lc[which(nath_lc$broods=="one"), ]
volonespp_lc <- unique(volonenath_lc$genus_species)
volone_3 <- na.omit(match(volonespp_lc, lssp))

volonenath_sv <- nath_sv[which(nath_sv$broods=="one"), ]
volonespp_sv <- unique(volonenath_sv$genus_species)
volone_4 <- na.omit(match(volonespp_sv, sssp))

volonenath_wa <- nath_wa[which(nath_wa$broods=="one"), ]
volonespp_wa <- unique(volonenath_wa$genus_species)
volone_5<- na.omit(match(volonespp_wa, wssp))

####################voltinism(multi brood)

volmultinath_cp <- nath_cp[which(nath_cp$broods=="multiple"), ]
volmultispp_cp <- unique(volmultinath_cp$genus_species)
volmulti_1 <- na.omit(match(volmultispp_cp, cssp))

volmultinath_dp <- nath_dp[which(nath_dp$broods=="multiple"), ]
volmultispp_dp <- unique(volmultinath_dp$genus_species)
volmulti_2 <- na.omit(match(volmultispp_dp, dssp))

volmultinath_lc <- nath_lc[which(nath_lc$broods=="multiple"), ]
volmultispp_lc <- unique(volmultinath_lc$genus_species)
volmulti_3 <- na.omit(match(volmultispp_lc, lssp))

volmultinath_sv <- nath_sv[which(nath_sv$broods=="multiple"), ]
volmultispp_sv <- unique(volmultinath_sv$genus_species)
volmulti_4 <- na.omit(match(volmultispp_sv, sssp))

volmultinath_wa <- nath_wa[which(nath_wa$broods=="multiple"), ]
volmultispp_wa <- unique(volmultinath_wa$genus_species)
volmulti_5<- na.omit(match(volmultispp_wa, wssp))

###################################################################
############################# Overwintering stage #############################
########################################################################
########overwintering (eggs)

oveggnath_cp <- nath_cp[which(nath_cp$wintering== "egg"), ]
oveggspp_cp <- unique(oveggnath_cp$genus_species)
ovegg_1 <- na.omit(match(oveggspp_cp, cssp))

oveggnath_dp <- nath_dp[which(nath_dp$wintering== "egg"), ]
oveggspp_dp <- unique(oveggnath_dp$genus_species)
ovegg_2 <- na.omit(match(oveggspp_dp, dssp))

oveggnath_lc <- nath_lc[which(nath_lc$wintering== "egg"), ]
oveggspp_lc <- unique(oveggnath_lc$genus_species)
ovegg_3 <- na.omit(match(oveggspp_lc, lssp))

oveggnath_sv <- nath_sv[which(nath_sv$wintering== "egg"), ]
oveggspp_sv <- unique(oveggnath_sv$genus_species)
ovegg_4 <- na.omit(match(oveggspp_sv, sssp))

oveggnath_wa <- nath_wa[which(nath_wa$wintering== "egg"), ]
oveggspp_wa <- unique(oveggnath_wa$genus_species)
ovegg_5<- na.omit(match(oveggspp_wa, wssp))

#####################overwintering (larvae)
ovlarnath_cp <- nath_cp[which(nath_cp$wintering== "larva"), ]
ovlarspp_cp <- unique(ovlarnath_cp$genus_species)
ovlar_1 <- na.omit(match(ovlarspp_cp, cssp))

ovlarnath_dp <- nath_dp[which(nath_dp$wintering== "larva"), ]
ovlarspp_dp <- unique(ovlarnath_dp$genus_species)
ovlar_2 <- na.omit(match(ovlarspp_dp, dssp))

ovlarnath_lc <- nath_lc[which(nath_lc$wintering== "larva"), ]
ovlarspp_lc <- unique(ovlarnath_lc$genus_species)
ovlar_3 <- na.omit(match(ovlarspp_lc, lssp))

ovlarnath_sv <- nath_sv[which(nath_sv$wintering== "larva"), ]
ovlarspp_sv <- unique(ovlarnath_sv$genus_species)
ovlar_4 <- na.omit(match(ovlarspp_sv, sssp))

ovlarnath_wa <- nath_wa[which(nath_wa$wintering== "larva"), ]
ovlarspp_wa <- unique(ovlarnath_wa$genus_species)
ovlar_5<- na.omit(match(ovlarspp_wa, wssp))

###########overwintering (pupae)
ovpupnath_cp <- nath_cp[which(nath_cp$wintering== "pupa"), ]
ovpupspp_cp <- unique(ovpupnath_cp$genus_species)
ovpup_1 <- na.omit(match(ovpupspp_cp, cssp))

ovpupnath_dp <- nath_dp[which(nath_dp$wintering== "pupa"), ]
ovpupspp_dp <- unique(ovpupnath_dp$genus_species)
ovpup_2 <- na.omit(match(ovpupspp_dp, dssp))

ovpupnath_lc <- nath_lc[which(nath_lc$wintering== "pupa"), ]
ovpupspp_lc <- unique(ovpupnath_lc$genus_species)
ovpup_3 <- na.omit(match(ovpupspp_lc, lssp))

ovpupnath_sv <- nath_sv[which(nath_sv$wintering== "pupa"), ]
ovpupspp_sv <- unique(ovpupnath_sv$genus_species)
ovpup_4 <- na.omit(match(ovpupspp_sv, sssp))

ovpupnath_wa <- nath_wa[which(nath_wa$wintering== "pupa"), ]
ovpupspp_wa <- unique(ovpupnath_wa$genus_species)
ovpup_5<- na.omit(match(ovpupspp_wa, wssp))

##################################overwintering (Adult)
ovadunath_cp <- nath_cp[which(nath_cp$wintering== "adult"), ]
ovaduspp_cp <- unique(ovadunath_cp$genus_species)
ovadu_1 <- na.omit(match(ovaduspp_cp, cssp))

ovadunath_dp <- nath_dp[which(nath_dp$wintering== "adult"), ]
ovaduspp_dp <- unique(ovadunath_dp$genus_species)
ovadu_2 <- na.omit(match(ovaduspp_dp, dssp))

ovadunath_lc <- nath_lc[which(nath_lc$wintering== "adult"), ]
ovaduspp_lc <- unique(ovadunath_lc$genus_species)
ovadu_3 <- na.omit(match(ovaduspp_lc, lssp))

ovadunath_sv <- nath_sv[which(nath_sv$wintering== "adult"), ]
ovaduspp_sv <- unique(ovadunath_sv$genus_species)
ovadu_4 <- na.omit(match(ovaduspp_sv, sssp))

ovadunath_wa <- nath_wa[which(nath_wa$wintering== "adult"), ]
ovaduspp_wa <- unique(ovadunath_wa$genus_species)
ovadu_5<- na.omit(match(ovaduspp_wa, wssp))


###################################################################
############################# DIET BREADTH #############################
########################################################################
#####################################diet breadth(one )
dietonenath_cp <- nath_cp[which(nath_cp$hostgenera=="one"), ]
dietonespp_cp <- unique(dietonenath_cp$genus_species)
dietone_1 <- na.omit(match(dietonespp_cp, cssp))

dietonenath_dp <- nath_dp[which(nath_dp$hostgenera=="one"), ]
dietonespp_dp <- unique(dietonenath_dp$genus_species)
dietone_2 <- na.omit(match(dietonespp_dp, dssp))

dietonenath_lc <- nath_lc[which(nath_lc$hostgenera=="one"), ]
dietonespp_lc <- unique(dietonenath_lc$genus_species)
dietone_3 <- na.omit(match(dietonespp_lc, lssp))

dietonenath_sv <- nath_sv[which(nath_sv$hostgenera=="one"), ]
dietonespp_sv <- unique(dietonenath_sv$genus_species)
dietone_4 <- na.omit(match(dietonespp_sv, sssp))

dietonenath_wa <- nath_wa[which(nath_wa$hostgenera=="one"), ]
dietonespp_wa <- unique(dietonenath_wa$genus_species)
dietone_5<- na.omit(match(dietonespp_wa, wssp))


####################diet breadth(multi )
dietmultinath_cp <- nath_cp[which(nath_cp$hostgenera=="multiple"), ]
dietmultispp_cp <- unique(dietmultinath_cp$genus_species)
dietmulti_1 <- na.omit(match(dietmultispp_cp, cssp))

dietmultinath_dp <- nath_dp[which(nath_dp$hostgenera=="multiple"), ]
dietmultispp_dp <- unique(dietmultinath_dp$genus_species)
dietmulti_2 <- na.omit(match(dietmultispp_dp, dssp))

dietmultinath_lc <- nath_lc[which(nath_lc$hostgenera=="multiple"), ]
dietmultispp_lc <- unique(dietmultinath_lc$genus_species)
dietmulti_3 <- na.omit(match(dietmultispp_lc, lssp))

dietmultinath_sv <- nath_sv[which(nath_sv$hostgenera=="multiple"), ]
dietmultispp_sv <- unique(dietmultinath_sv$genus_species)
dietmulti_4 <- na.omit(match(dietmultispp_sv, sssp))

dietmultinath_wa <- nath_wa[which(nath_wa$hostgenera=="multiple"), ]
dietmultispp_wa <- unique(dietmultinath_wa$genus_species)
dietmulti_5<- na.omit(match(dietmultispp_wa, wssp))



#All of the below is for assigning colors to beta's based on residency
#resident species at EACH SITE
cpres <- cssp[res_1]
dpres <- dssp[res_2]
lcres <- lssp[res_3]
svres <- sssp[res_4]
wares <- wssp[res_5]
#non resident species at EACH SITE
cpnonres <- cssp[nonres_1]
dpnonres <- dssp[nonres_2]
lcnonres <- lssp[nonres_3]
svnonres <- sssp[nonres_4]
wanonres <- wssp[nonres_5]



#All of the below is for assigning colors to beta's based on residency
#CP
res_cc5_b1 <- rep(0, length(cssp))
res_cc5_b1[res_1] <- 1
res_cc5_b1[nonres_1] <- 2

#DP
res_cc4_b1 <- rep(0, length(dssp))
res_cc4_b1[res_2] <- 1
res_cc4_b1[nonres_2] <- 2

#LC
res_cc3_b1 <- rep(0, length(lssp))
res_cc3_b1[res_3] <- 1
res_cc3_b1[nonres_3] <- 2

#SV
res_cc2_b1 <- rep(0, length(sssp))
res_cc2_b1[res_4] <- 1
res_cc2_b1[nonres_4] <- 2

#WA
res_cc1_b1 <- rep(0, length(wssp))
res_cc1_b1[res_5] <- 1
res_cc1_b1[nonres_5] <- 2



#All of the below is for assigning colors to beta's based on weediness
#CP
wed_cc5_b1 <- rep(0, length(cssp))
wed_cc5_b1[wed_1] <- 1
wed_cc5_b1[nonwed_1] <- 2

#DP
wed_cc4_b1 <- rep(0, length(dssp))
wed_cc4_b1[wed_2] <- 1
wed_cc4_b1[nonwed_2] <- 2

#LC
wed_cc3_b1 <- rep(0, length(lssp))
wed_cc3_b1[wed_3] <- 1
wed_cc3_b1[nonwed_3] <- 2

#SV
wed_cc2_b1 <- rep(0, length(sssp))
wed_cc2_b1[wed_4] <- 1
wed_cc2_b1[nonwed_4] <- 2

#WA
wed_cc1_b1 <- rep(0, length(wssp))
wed_cc1_b1[wed_5] <- 1
wed_cc1_b1[nonwed_5] <- 2


#All of the below is for assigning colors to beta's based on voltinism
#CP
vol_cc5_b1 <- rep(0, length(cssp))
vol_cc5_b1[volone_1] <- 1
vol_cc5_b1[volmulti_1] <- 2

#DP
vol_cc4_b1 <- rep(0, length(dssp))
vol_cc4_b1[volone_2] <- 1
vol_cc4_b1[volmulti_2] <- 2

#LC
vol_cc3_b1 <- rep(0, length(lssp))
vol_cc3_b1[volone_3] <- 1
vol_cc3_b1[volmulti_3] <- 2

#SV
vol_cc2_b1 <- rep(0, length(sssp))
vol_cc2_b1[volone_4] <- 1
vol_cc2_b1[volmulti_4] <- 2

#WA
vol_cc1_b1 <- rep(0, length(wssp))
vol_cc1_b1[volone_5] <- 1
vol_cc1_b1[volmulti_5] <- 2



#All of the below is for assigning colors to beta's based on Overwintering stage
#CP
ove_cc5_b1 <- rep(0, length(cssp))
ove_cc5_b1[ovegg_1] <- 1
ove_cc5_b1[ovlar_1] <- 2
ove_cc5_b1[ovpup_1] <- 3
ove_cc5_b1[ovadu_1] <- 4

#DP
ove_cc4_b1 <- rep(0, length(dssp))
ove_cc4_b1[ovegg_2] <- 1
ove_cc4_b1[ovlar_2] <- 2
ove_cc4_b1[ovpup_2] <- 3
ove_cc4_b1[ovadu_2] <- 4

#LC
ove_cc3_b1 <- rep(0, length(lssp))
ove_cc3_b1[ovegg_3] <- 1
ove_cc3_b1[ovlar_3] <- 2
ove_cc3_b1[ovpup_3] <- 3
ove_cc3_b1[ovadu_3] <- 4

#SV
ove_cc2_b1 <- rep(0, length(sssp))
ove_cc2_b1[ovegg_4] <- 1
ove_cc2_b1[ovlar_4] <- 2
ove_cc2_b1[ovpup_4] <- 3
ove_cc2_b1[ovadu_4] <- 4

#WA
ove_cc1_b1 <- rep(0, length(wssp))
ove_cc1_b1[ovegg_5] <- 1
ove_cc1_b1[ovlar_5] <- 2
ove_cc1_b1[ovpup_5] <- 3
ove_cc1_b1[ovadu_5] <- 4

#All of the below is for assigning colors to beta's based on diet breadth
#CP
diet_cc5_b1 <- rep(0, length(cssp))
diet_cc5_b1[dietone_1] <- 1
diet_cc5_b1[dietmulti_1] <- 2

#DP
diet_cc4_b1 <- rep(0, length(dssp))
diet_cc4_b1[dietone_2] <- 1
diet_cc4_b1[dietmulti_2] <- 2

#LC
diet_cc3_b1 <- rep(0, length(lssp))
diet_cc3_b1[dietone_3] <- 1
diet_cc3_b1[dietmulti_3] <- 2

#SV
diet_cc2_b1 <- rep(0, length(sssp))
diet_cc2_b1[dietone_4] <- 1
diet_cc2_b1[dietmulti_4] <- 2

#WA
diet_cc1_b1 <- rep(0, length(wssp))
diet_cc1_b1[dietone_5] <- 1
diet_cc1_b1[dietmulti_5] <- 2




cols <- c("grey", "blue", "green", "orange", "pink")                



pdf(paste("RESIDENT_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_PLOT.pdf",sep=""),width=12,height=17)

layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))

par(oma=c(1,0.3,0,0.3), mar=c(6,5,3,2))

arrow_col <- c(rep("red", 3),  # red for spring max temp
               rep("#87CEEB", 3), # blue for winter precipitation
               rep("black", 3)) # green for spring min temp

arrow_lab <- rep(c("MO", "TO", "LO"), 3)

#biplot
#biplot(cppca)
plot(cppca$x[,1], cppca$x[,2], xlab=paste("PCA 1 (", round(sumcp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumcp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[res_cc5_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(cppca$x[,1], cppca$x[,2], labels=row.names(cppca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 5 to make arrows longer
cp_l_x <- cppca$rotation[,1]*4.5
cp_l_y <- cppca$rotation[,2]*4.5
arrows(x0=0, x1=cp_l_x, y0=0, y1=cp_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
cp_l_pos <- cp_l_y # Create a vector of y axis coordinates
cp_lo <- which(cp_l_y < 0) #  variables on the bottom half of the plot
cp_hi <- which(cp_l_y > 0) # variables on the top half
# Replace values in the vector
cp_l_pos <- replace(cp_l_pos, cp_lo, "1")
cp_l_pos <- replace(cp_l_pos, cp_hi, "3")

# Variable labels
#text(cp_l_x, cp_l_y, labels=row.names(cppca$rotation), col=arrow_col, pos=cp_l_pos, cex=1.5)
text(cp_l_x, cp_l_y, labels=arrow_lab, col=arrow_col, pos=cp_l_pos, cex=1.5)

#title(main = "(a) CP", cex.main = 2)
mtext("(a) CP", side = 3, line = 0.3, adj = 0, cex = 1.5)

# legend 
legend("topleft", legend = c("Resident", "Non-resident"), col = alpha(c("blue", "green")), pch = 19, cex = 1.5, bty= "n")



#biplot
#biplot(dppca)
plot(dppca$x[,1], dppca$x[,2], xlab=paste("PCA 1 (", round(sumdp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumdp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[res_cc4_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(dppca$x[,1], dppca$x[,2], labels=row.names(dppca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
dp_l_x <- dppca$rotation[,1]*5
dp_l_y <- dppca$rotation[,2]*5
arrows(x0=0, x1=dp_l_x, y0=0, y1=dp_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
dp_l_pos <- dp_l_y # Create a vector of y axis coordinates
dp_lo <- which(dp_l_y < 0) #  variables on the bottom half of the plot
dp_hi <- which(dp_l_y > 0) # variables on the top half
# Replace values in the vector
dp_l_pos <- replace(dp_l_pos, dp_lo, "1")
dp_l_pos <- replace(dp_l_pos, dp_hi, "3")

# Variable labels
text(dp_l_x, dp_l_y, labels=arrow_lab, col=arrow_col, pos=dp_l_pos, cex=1.5)
#title(main = "(b) DP", cex.main = 2)
mtext("(b) DP", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(lcpca)
plot(lcpca$x[,1], lcpca$x[,2], xlab=paste("PCA 1 (", round(sumlc$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumlc$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[res_cc3_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(lcpca$x[,1], lcpca$x[,2], labels=row.names(lcpca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
lc_l_x <- lcpca$rotation[,1]*5
lc_l_y <- lcpca$rotation[,2]*5
arrows(x0=0, x1=lc_l_x, y0=0, y1=lc_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
lc_l_pos <- lc_l_y # Create a vector of y axis coordinates
lc_lo <- which(lc_l_y < 0) #  variables on the bottom half of the plot
lc_hi <- which(lc_l_y > 0) # variables on the top half
# Replace values in the vector
lc_l_pos <- replace(lc_l_pos, lc_lo, "1")
lc_l_pos <- replace(lc_l_pos, lc_hi, "3")

# Variable labels
text(lc_l_x, lc_l_y, labels=arrow_lab, col=arrow_col, pos=lc_l_pos, cex=1.5)
#title(main = "(c) LC", cex.main = 2)
mtext("(c) LC", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(svpca)
plot(svpca$x[,1], svpca$x[,2], xlab=paste("PCA 1 (", round(sumsv$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumsv$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[res_cc2_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(svpca$x[,1], svpca$x[,2], labels=row.names(svpca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 4 to make arrows longer
sv_l_x <- svpca$rotation[,1]*4
sv_l_y <- svpca$rotation[,2]*4
arrows(x0=0, x1=sv_l_x, y0=0, y1=sv_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
sv_l_pos <- sv_l_y # Create a vector of y axis coordinates
sv_lo <- which(sv_l_y < 0) #  variables on the bottom half of the plot
sv_hi <- which(sv_l_y > 0) # variables on the top half
# Replace values in the vector
sv_l_pos <- replace(sv_l_pos, sv_lo, "1")
sv_l_pos <- replace(sv_l_pos, sv_hi, "3")

# Variable labels
text(sv_l_x, sv_l_y, labels=arrow_lab, col=arrow_col, pos=sv_l_pos, cex=1.5)
#title(main = "(d) SV", cex.main = 2)
mtext("(d) SV", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(wapca)
plot(wapca$x[,1], wapca$x[,2], xlab=paste("PCA 1 (", round(sumwa$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumwa$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[res_cc1_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(wapca$x[,1], wapca$x[,2], labels=row.names(wapca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
wa_l_x <- wapca$rotation[,1]*5
wa_l_y <- wapca$rotation[,2]*5
arrows(x0=0, x1=wa_l_x, y0=0, y1=wa_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
wa_l_pos <- wa_l_y # Create a vector of y axis coordinates
wa_lo <- which(wa_l_y < 0) #  variables on the bottom half of the plot
wa_hi <- which(wa_l_y > 0) # variables on the top half
# Replace values in the vector
wa_l_pos <- replace(wa_l_pos, wa_lo, "1")
wa_l_pos <- replace(wa_l_pos, wa_hi, "3")

# Variable labels
text(wa_l_x, wa_l_y, labels=arrow_lab, col=arrow_col, pos=wa_l_pos, cex=1.5)

#title(main = "(e) WA", cex.main = 2)
mtext("(e) WA", side = 3, line = 0.3, adj = 0, cex = 1.5)


#species present at all sites
#biplot
#biplot(sp_allst_pca)
plot(sp_allst_pca$x[,1], sp_allst_pca$x[,2], xlab=paste("PCA 1 (", round(sum_sp_allst$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sum_sp_allst$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2, xlim= c(-6,6))

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(sp_allst_pca$x[,1], sp_allst_pca$x[,2], labels=row.names(sp_allst_pca$x), pos=c(1,4,2,3), font=2)

sp_allst_arrow_col <- rep(arrow_col, each=5)
sp_allst_arrow_lab <- rep(arrow_lab, each=5)

indcp <- seq(1,45, 5)
inddp <- seq(2,45, 5)
indlc <- seq(3,45, 5)
indsv <- seq(4,45, 5)
indwa <- seq(5,45, 5)

sp_allst_arrow_lab[indcp] <- paste(sp_allst_arrow_lab[indcp], "(CP)", sep = "")
sp_allst_arrow_lab[inddp] <- paste(sp_allst_arrow_lab[inddp], "(DP)", sep = "")
sp_allst_arrow_lab[indlc] <- paste(sp_allst_arrow_lab[indlc], "(LC)", sep = "")
sp_allst_arrow_lab[indsv] <- paste(sp_allst_arrow_lab[indsv], "(SV)", sep = "")
sp_allst_arrow_lab[indwa] <- paste(sp_allst_arrow_lab[indwa], "(WA)", sep = "")

# Draw arrows, i multiply by 6 to make arrows longer
sp_allst_l_x <- sp_allst_pca$rotation[,1]*12
sp_allst_l_y <- sp_allst_pca$rotation[,2]*12
arrows(x0=0, x1=sp_allst_l_x, y0=0, y1=sp_allst_l_y, col=sp_allst_arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
sp_allst_l_pos <- sp_allst_l_y # Create a vector of y axis coordinates
sp_allst_lo <- which(sp_allst_l_y < 0) #  variables on the bottom half of the plot
sp_allst_hi <- which(sp_allst_l_y > 0) # variables on the top half
# Replace values in the vector
sp_allst_l_pos <- replace(sp_allst_l_pos, sp_allst_lo, "1")
sp_allst_l_pos <- replace(sp_allst_l_pos, sp_allst_hi, "3")

# Variable labels
text(sp_allst_l_x, sp_allst_l_y, labels=sp_allst_arrow_lab, col=sp_allst_arrow_col, pos=sp_allst_l_pos, cex=1.5)

#title(main = "(f) ALL SITES", cex.main = 2)
mtext("(f) All sites", side = 3, line = 0.3, adj = 0, cex = 1.5)

dev.off()




pdf(paste("WEEDY_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_PLOT.pdf",sep=""),width=12,height=17)

layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))

par(oma=c(1,0.3,0,0.3), mar=c(6,5,3,2))

arrow_col <- c(rep("red", 3),  # red for spring max temp
               rep("#87CEEB", 3), # blue for winter precipitation
               rep("black", 3)) # green for spring min temp

arrow_lab <- rep(c("MO", "TO", "LO"), 3)

#biplot
#biplot(cppca)
plot(cppca$x[,1], cppca$x[,2], xlab=paste("PCA 1 (", round(sumcp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumcp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[wed_cc5_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(cppca$x[,1], cppca$x[,2], labels=row.names(cppca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 5 to make arrows longer
cp_l_x <- cppca$rotation[,1]*4.5
cp_l_y <- cppca$rotation[,2]*4.5
arrows(x0=0, x1=cp_l_x, y0=0, y1=cp_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
cp_l_pos <- cp_l_y # Create a vector of y axis coordinates
cp_lo <- which(cp_l_y < 0) #  variables on the bottom half of the plot
cp_hi <- which(cp_l_y > 0) # variables on the top half
# Replace values in the vector
cp_l_pos <- replace(cp_l_pos, cp_lo, "1")
cp_l_pos <- replace(cp_l_pos, cp_hi, "3")

# Variable labels
#text(cp_l_x, cp_l_y, labels=row.names(cppca$rotation), col=arrow_col, pos=cp_l_pos, cex=1.5)
text(cp_l_x, cp_l_y, labels=arrow_lab, col=arrow_col, pos=cp_l_pos, cex=1.5)

#title(main = "(a) CP", cex.main = 2)
mtext("(a) CP", side = 3, line = 0.3, adj = 0, cex = 1.5)
# legend 
legend("topleft", legend = c("Ruderal", "Non-Ruderal"), col = alpha(c("blue", "green")), pch = 19, cex = 1.5, bty= "n")



#biplot
#biplot(dppca)
plot(dppca$x[,1], dppca$x[,2], xlab=paste("PCA 1 (", round(sumdp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumdp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[wed_cc4_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(dppca$x[,1], dppca$x[,2], labels=row.names(dppca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
dp_l_x <- dppca$rotation[,1]*5
dp_l_y <- dppca$rotation[,2]*5
arrows(x0=0, x1=dp_l_x, y0=0, y1=dp_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
dp_l_pos <- dp_l_y # Create a vector of y axis coordinates
dp_lo <- which(dp_l_y < 0) #  variables on the bottom half of the plot
dp_hi <- which(dp_l_y > 0) # variables on the top half
# Replace values in the vector
dp_l_pos <- replace(dp_l_pos, dp_lo, "1")
dp_l_pos <- replace(dp_l_pos, dp_hi, "3")

# Variable labels
text(dp_l_x, dp_l_y, labels=arrow_lab, col=arrow_col, pos=dp_l_pos, cex=1.5)
#title(main = "(b) DP", cex.main = 2)
mtext("(b) DP", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(lcpca)
plot(lcpca$x[,1], lcpca$x[,2], xlab=paste("PCA 1 (", round(sumlc$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumlc$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[wed_cc3_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(lcpca$x[,1], lcpca$x[,2], labels=row.names(lcpca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
lc_l_x <- lcpca$rotation[,1]*5
lc_l_y <- lcpca$rotation[,2]*5
arrows(x0=0, x1=lc_l_x, y0=0, y1=lc_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
lc_l_pos <- lc_l_y # Create a vector of y axis coordinates
lc_lo <- which(lc_l_y < 0) #  variables on the bottom half of the plot
lc_hi <- which(lc_l_y > 0) # variables on the top half
# Replace values in the vector
lc_l_pos <- replace(lc_l_pos, lc_lo, "1")
lc_l_pos <- replace(lc_l_pos, lc_hi, "3")

# Variable labels
text(lc_l_x, lc_l_y, labels=arrow_lab, col=arrow_col, pos=lc_l_pos, cex=1.5)
#title(main = "(c) LC", cex.main = 2)
mtext("(c) LC", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(svpca)
plot(svpca$x[,1], svpca$x[,2], xlab=paste("PCA 1 (", round(sumsv$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumsv$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[wed_cc2_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(svpca$x[,1], svpca$x[,2], labels=row.names(svpca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 4 to make arrows longer
sv_l_x <- svpca$rotation[,1]*4
sv_l_y <- svpca$rotation[,2]*4
arrows(x0=0, x1=sv_l_x, y0=0, y1=sv_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
sv_l_pos <- sv_l_y # Create a vector of y axis coordinates
sv_lo <- which(sv_l_y < 0) #  variables on the bottom half of the plot
sv_hi <- which(sv_l_y > 0) # variables on the top half
# Replace values in the vector
sv_l_pos <- replace(sv_l_pos, sv_lo, "1")
sv_l_pos <- replace(sv_l_pos, sv_hi, "3")

# Variable labels
text(sv_l_x, sv_l_y, labels=arrow_lab, col=arrow_col, pos=sv_l_pos, cex=1.5)
#title(main = "(d) SV", cex.main = 2)
mtext("(d) SV", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(wapca)
plot(wapca$x[,1], wapca$x[,2], xlab=paste("PCA 1 (", round(sumwa$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumwa$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[wed_cc1_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(wapca$x[,1], wapca$x[,2], labels=row.names(wapca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
wa_l_x <- wapca$rotation[,1]*5
wa_l_y <- wapca$rotation[,2]*5
arrows(x0=0, x1=wa_l_x, y0=0, y1=wa_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
wa_l_pos <- wa_l_y # Create a vector of y axis coordinates
wa_lo <- which(wa_l_y < 0) #  variables on the bottom half of the plot
wa_hi <- which(wa_l_y > 0) # variables on the top half
# Replace values in the vector
wa_l_pos <- replace(wa_l_pos, wa_lo, "1")
wa_l_pos <- replace(wa_l_pos, wa_hi, "3")

# Variable labels
text(wa_l_x, wa_l_y, labels=arrow_lab, col=arrow_col, pos=wa_l_pos, cex=1.5)

#title(main = "(e) WA", cex.main = 2)
mtext("(e) WA", side = 3, line = 0.3, adj = 0, cex = 1.5)


#species present at all sites
#biplot
#biplot(sp_allst_pca)
plot(sp_allst_pca$x[,1], sp_allst_pca$x[,2], xlab=paste("PCA 1 (", round(sum_sp_allst$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sum_sp_allst$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2, xlim= c(-6,6))

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(sp_allst_pca$x[,1], sp_allst_pca$x[,2], labels=row.names(sp_allst_pca$x), pos=c(1,4,2,3), font=2)

sp_allst_arrow_col <- rep(arrow_col, each=5)
sp_allst_arrow_lab <- rep(arrow_lab, each=5)

indcp <- seq(1,45, 5)
inddp <- seq(2,45, 5)
indlc <- seq(3,45, 5)
indsv <- seq(4,45, 5)
indwa <- seq(5,45, 5)

sp_allst_arrow_lab[indcp] <- paste(sp_allst_arrow_lab[indcp], "(CP)", sep = "")
sp_allst_arrow_lab[inddp] <- paste(sp_allst_arrow_lab[inddp], "(DP)", sep = "")
sp_allst_arrow_lab[indlc] <- paste(sp_allst_arrow_lab[indlc], "(LC)", sep = "")
sp_allst_arrow_lab[indsv] <- paste(sp_allst_arrow_lab[indsv], "(SV)", sep = "")
sp_allst_arrow_lab[indwa] <- paste(sp_allst_arrow_lab[indwa], "(WA)", sep = "")

# Draw arrows, i multiply by 6 to make arrows longer
sp_allst_l_x <- sp_allst_pca$rotation[,1]*12
sp_allst_l_y <- sp_allst_pca$rotation[,2]*12
arrows(x0=0, x1=sp_allst_l_x, y0=0, y1=sp_allst_l_y, col=sp_allst_arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
sp_allst_l_pos <- sp_allst_l_y # Create a vector of y axis coordinates
sp_allst_lo <- which(sp_allst_l_y < 0) #  variables on the bottom half of the plot
sp_allst_hi <- which(sp_allst_l_y > 0) # variables on the top half
# Replace values in the vector
sp_allst_l_pos <- replace(sp_allst_l_pos, sp_allst_lo, "1")
sp_allst_l_pos <- replace(sp_allst_l_pos, sp_allst_hi, "3")

# Variable labels
text(sp_allst_l_x, sp_allst_l_y, labels=sp_allst_arrow_lab, col=sp_allst_arrow_col, pos=sp_allst_l_pos, cex=1.5)

#title(main = "(f) ALL SITES", cex.main = 2)
mtext("(f) All sites", side = 3, line = 0.3, adj = 0, cex = 1.5)

dev.off()





pdf(paste("VOLTINISM_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_PLOT.pdf",sep=""),width=12,height=17)

layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))

par(oma=c(1,0.3,0,0.3), mar=c(6,5,3,2))

arrow_col <- c(rep("red", 3),  # red for spring max temp
               rep("#87CEEB", 3), # blue for winter precipitation
               rep("black", 3)) # green for spring min temp

arrow_lab <- rep(c("MO", "TO", "LO"), 3)

#biplot
#biplot(cppca)
plot(cppca$x[,1], cppca$x[,2], xlab=paste("PCA 1 (", round(sumcp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumcp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[vol_cc5_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(cppca$x[,1], cppca$x[,2], labels=row.names(cppca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 5 to make arrows longer
cp_l_x <- cppca$rotation[,1]*4.5
cp_l_y <- cppca$rotation[,2]*4.5
arrows(x0=0, x1=cp_l_x, y0=0, y1=cp_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
cp_l_pos <- cp_l_y # Create a vector of y axis coordinates
cp_lo <- which(cp_l_y < 0) #  variables on the bottom half of the plot
cp_hi <- which(cp_l_y > 0) # variables on the top half
# Replace values in the vector
cp_l_pos <- replace(cp_l_pos, cp_lo, "1")
cp_l_pos <- replace(cp_l_pos, cp_hi, "3")

# Variable labels
#text(cp_l_x, cp_l_y, labels=row.names(cppca$rotation), col=arrow_col, pos=cp_l_pos, cex=1.5)
text(cp_l_x, cp_l_y, labels=arrow_lab, col=arrow_col, pos=cp_l_pos, cex=1.5)

#title(main = "(a) CP", cex.main = 2)
mtext("(a) CP", side = 3, line = 0.3, adj = 0, cex = 1.5)
legend("topleft", legend = c("Univoltine", "Multivoltine"), col = alpha(c("blue", "green")), pch = 19, cex = 1.5, bty= "n")



#biplot
#biplot(dppca)
plot(dppca$x[,1], dppca$x[,2], xlab=paste("PCA 1 (", round(sumdp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumdp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[vol_cc4_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(dppca$x[,1], dppca$x[,2], labels=row.names(dppca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
dp_l_x <- dppca$rotation[,1]*5
dp_l_y <- dppca$rotation[,2]*5
arrows(x0=0, x1=dp_l_x, y0=0, y1=dp_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
dp_l_pos <- dp_l_y # Create a vector of y axis coordinates
dp_lo <- which(dp_l_y < 0) #  variables on the bottom half of the plot
dp_hi <- which(dp_l_y > 0) # variables on the top half
# Replace values in the vector
dp_l_pos <- replace(dp_l_pos, dp_lo, "1")
dp_l_pos <- replace(dp_l_pos, dp_hi, "3")

# Variable labels
text(dp_l_x, dp_l_y, labels=arrow_lab, col=arrow_col, pos=dp_l_pos, cex=1.5)
#title(main = "(b) DP", cex.main = 2)
mtext("(b) DP", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(lcpca)
plot(lcpca$x[,1], lcpca$x[,2], xlab=paste("PCA 1 (", round(sumlc$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumlc$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[vol_cc3_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(lcpca$x[,1], lcpca$x[,2], labels=row.names(lcpca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
lc_l_x <- lcpca$rotation[,1]*5
lc_l_y <- lcpca$rotation[,2]*5
arrows(x0=0, x1=lc_l_x, y0=0, y1=lc_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
lc_l_pos <- lc_l_y # Create a vector of y axis coordinates
lc_lo <- which(lc_l_y < 0) #  variables on the bottom half of the plot
lc_hi <- which(lc_l_y > 0) # variables on the top half
# Replace values in the vector
lc_l_pos <- replace(lc_l_pos, lc_lo, "1")
lc_l_pos <- replace(lc_l_pos, lc_hi, "3")

# Variable labels
text(lc_l_x, lc_l_y, labels=arrow_lab, col=arrow_col, pos=lc_l_pos, cex=1.5)
#title(main = "(c) LC", cex.main = 2)
mtext("(c) LC", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(svpca)
plot(svpca$x[,1], svpca$x[,2], xlab=paste("PCA 1 (", round(sumsv$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumsv$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[vol_cc2_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(svpca$x[,1], svpca$x[,2], labels=row.names(svpca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 4 to make arrows longer
sv_l_x <- svpca$rotation[,1]*4
sv_l_y <- svpca$rotation[,2]*4
arrows(x0=0, x1=sv_l_x, y0=0, y1=sv_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
sv_l_pos <- sv_l_y # Create a vector of y axis coordinates
sv_lo <- which(sv_l_y < 0) #  variables on the bottom half of the plot
sv_hi <- which(sv_l_y > 0) # variables on the top half
# Replace values in the vector
sv_l_pos <- replace(sv_l_pos, sv_lo, "1")
sv_l_pos <- replace(sv_l_pos, sv_hi, "3")

# Variable labels
text(sv_l_x, sv_l_y, labels=arrow_lab, col=arrow_col, pos=sv_l_pos, cex=1.5)
#title(main = "(d) SV", cex.main = 2)
mtext("(d) SV", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(wapca)
plot(wapca$x[,1], wapca$x[,2], xlab=paste("PCA 1 (", round(sumwa$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumwa$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[vol_cc1_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(wapca$x[,1], wapca$x[,2], labels=row.names(wapca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
wa_l_x <- wapca$rotation[,1]*5
wa_l_y <- wapca$rotation[,2]*5
arrows(x0=0, x1=wa_l_x, y0=0, y1=wa_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
wa_l_pos <- wa_l_y # Create a vector of y axis coordinates
wa_lo <- which(wa_l_y < 0) #  variables on the bottom half of the plot
wa_hi <- which(wa_l_y > 0) # variables on the top half
# Replace values in the vector
wa_l_pos <- replace(wa_l_pos, wa_lo, "1")
wa_l_pos <- replace(wa_l_pos, wa_hi, "3")

# Variable labels
text(wa_l_x, wa_l_y, labels=arrow_lab, col=arrow_col, pos=wa_l_pos, cex=1.5)

#title(main = "(e) WA", cex.main = 2)
mtext("(e) WA", side = 3, line = 0.3, adj = 0, cex = 1.5)


#species present at all sites
#biplot
#biplot(sp_allst_pca)
plot(sp_allst_pca$x[,1], sp_allst_pca$x[,2], xlab=paste("PCA 1 (", round(sum_sp_allst$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sum_sp_allst$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2, xlim= c(-6,6))

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(sp_allst_pca$x[,1], sp_allst_pca$x[,2], labels=row.names(sp_allst_pca$x), pos=c(1,4,2,3), font=2)

sp_allst_arrow_col <- rep(arrow_col, each=5)
sp_allst_arrow_lab <- rep(arrow_lab, each=5)

indcp <- seq(1,45, 5)
inddp <- seq(2,45, 5)
indlc <- seq(3,45, 5)
indsv <- seq(4,45, 5)
indwa <- seq(5,45, 5)

sp_allst_arrow_lab[indcp] <- paste(sp_allst_arrow_lab[indcp], "(CP)", sep = "")
sp_allst_arrow_lab[inddp] <- paste(sp_allst_arrow_lab[inddp], "(DP)", sep = "")
sp_allst_arrow_lab[indlc] <- paste(sp_allst_arrow_lab[indlc], "(LC)", sep = "")
sp_allst_arrow_lab[indsv] <- paste(sp_allst_arrow_lab[indsv], "(SV)", sep = "")
sp_allst_arrow_lab[indwa] <- paste(sp_allst_arrow_lab[indwa], "(WA)", sep = "")

# Draw arrows, i multiply by 6 to make arrows longer
sp_allst_l_x <- sp_allst_pca$rotation[,1]*12
sp_allst_l_y <- sp_allst_pca$rotation[,2]*12
arrows(x0=0, x1=sp_allst_l_x, y0=0, y1=sp_allst_l_y, col=sp_allst_arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
sp_allst_l_pos <- sp_allst_l_y # Create a vector of y axis coordinates
sp_allst_lo <- which(sp_allst_l_y < 0) #  variables on the bottom half of the plot
sp_allst_hi <- which(sp_allst_l_y > 0) # variables on the top half
# Replace values in the vector
sp_allst_l_pos <- replace(sp_allst_l_pos, sp_allst_lo, "1")
sp_allst_l_pos <- replace(sp_allst_l_pos, sp_allst_hi, "3")

# Variable labels
text(sp_allst_l_x, sp_allst_l_y, labels=sp_allst_arrow_lab, col=sp_allst_arrow_col, pos=sp_allst_l_pos, cex=1.5)

#title(main = "(f) ALL SITES", cex.main = 2)
mtext("(f) All sites", side = 3, line = 0.3, adj = 0, cex = 1.5)

dev.off()





pdf(paste("OVERWINTERING_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_PLOT.pdf",sep=""),width=12,height=17)

layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))

par(oma=c(1,0.3,0,0.3), mar=c(6,5,3,2))

arrow_col <- c(rep("red", 3),  # red for spring max temp
               rep("#87CEEB", 3), # blue for winter precipitation
               rep("black", 3)) # green for spring min temp

arrow_lab <- rep(c("MO", "TO", "LO"), 3)

#biplot
#biplot(cppca)
plot(cppca$x[,1], cppca$x[,2], xlab=paste("PCA 1 (", round(sumcp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumcp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[ove_cc5_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(cppca$x[,1], cppca$x[,2], labels=row.names(cppca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 5 to make arrows longer
cp_l_x <- cppca$rotation[,1]*4.5
cp_l_y <- cppca$rotation[,2]*4.5
arrows(x0=0, x1=cp_l_x, y0=0, y1=cp_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
cp_l_pos <- cp_l_y # Create a vector of y axis coordinates
cp_lo <- which(cp_l_y < 0) #  variables on the bottom half of the plot
cp_hi <- which(cp_l_y > 0) # variables on the top half
# Replace values in the vector
cp_l_pos <- replace(cp_l_pos, cp_lo, "1")
cp_l_pos <- replace(cp_l_pos, cp_hi, "3")

# Variable labels
#text(cp_l_x, cp_l_y, labels=row.names(cppca$rotation), col=arrow_col, pos=cp_l_pos, cex=1.5)
text(cp_l_x, cp_l_y, labels=arrow_lab, col=arrow_col, pos=cp_l_pos, cex=1.5)

#title(main = "(a) CP", cex.main = 2)
mtext("(a) CP", side = 3, line = 0.3, adj = 0, cex = 1.5)
# legend 
legend("topleft", legend = c("Egg", "Larvae", "Pupae", "Adult"), col = alpha(c("blue", "green", "orange", "pink")), pch = 19, cex = 1.5, bty= "n")


#biplot
#biplot(dppca)
plot(dppca$x[,1], dppca$x[,2], xlab=paste("PCA 1 (", round(sumdp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumdp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[ove_cc4_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(dppca$x[,1], dppca$x[,2], labels=row.names(dppca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
dp_l_x <- dppca$rotation[,1]*5
dp_l_y <- dppca$rotation[,2]*5
arrows(x0=0, x1=dp_l_x, y0=0, y1=dp_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
dp_l_pos <- dp_l_y # Create a vector of y axis coordinates
dp_lo <- which(dp_l_y < 0) #  variables on the bottom half of the plot
dp_hi <- which(dp_l_y > 0) # variables on the top half
# Replace values in the vector
dp_l_pos <- replace(dp_l_pos, dp_lo, "1")
dp_l_pos <- replace(dp_l_pos, dp_hi, "3")

# Variable labels
text(dp_l_x, dp_l_y, labels=arrow_lab, col=arrow_col, pos=dp_l_pos, cex=1.5)
#title(main = "(b) DP", cex.main = 2)
mtext("(b) DP", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(lcpca)
plot(lcpca$x[,1], lcpca$x[,2], xlab=paste("PCA 1 (", round(sumlc$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumlc$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[ove_cc3_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(lcpca$x[,1], lcpca$x[,2], labels=row.names(lcpca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
lc_l_x <- lcpca$rotation[,1]*5
lc_l_y <- lcpca$rotation[,2]*5
arrows(x0=0, x1=lc_l_x, y0=0, y1=lc_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
lc_l_pos <- lc_l_y # Create a vector of y axis coordinates
lc_lo <- which(lc_l_y < 0) #  variables on the bottom half of the plot
lc_hi <- which(lc_l_y > 0) # variables on the top half
# Replace values in the vector
lc_l_pos <- replace(lc_l_pos, lc_lo, "1")
lc_l_pos <- replace(lc_l_pos, lc_hi, "3")

# Variable labels
text(lc_l_x, lc_l_y, labels=arrow_lab, col=arrow_col, pos=lc_l_pos, cex=1.5)
#title(main = "(c) LC", cex.main = 2)
mtext("(c) LC", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(svpca)
plot(svpca$x[,1], svpca$x[,2], xlab=paste("PCA 1 (", round(sumsv$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumsv$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[ove_cc2_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(svpca$x[,1], svpca$x[,2], labels=row.names(svpca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 4 to make arrows longer
sv_l_x <- svpca$rotation[,1]*4
sv_l_y <- svpca$rotation[,2]*4
arrows(x0=0, x1=sv_l_x, y0=0, y1=sv_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
sv_l_pos <- sv_l_y # Create a vector of y axis coordinates
sv_lo <- which(sv_l_y < 0) #  variables on the bottom half of the plot
sv_hi <- which(sv_l_y > 0) # variables on the top half
# Replace values in the vector
sv_l_pos <- replace(sv_l_pos, sv_lo, "1")
sv_l_pos <- replace(sv_l_pos, sv_hi, "3")

# Variable labels
text(sv_l_x, sv_l_y, labels=arrow_lab, col=arrow_col, pos=sv_l_pos, cex=1.5)
#title(main = "(d) SV", cex.main = 2)
mtext("(d) SV", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(wapca)
plot(wapca$x[,1], wapca$x[,2], xlab=paste("PCA 1 (", round(sumwa$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumwa$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[ove_cc1_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(wapca$x[,1], wapca$x[,2], labels=row.names(wapca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
wa_l_x <- wapca$rotation[,1]*5
wa_l_y <- wapca$rotation[,2]*5
arrows(x0=0, x1=wa_l_x, y0=0, y1=wa_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
wa_l_pos <- wa_l_y # Create a vector of y axis coordinates
wa_lo <- which(wa_l_y < 0) #  variables on the bottom half of the plot
wa_hi <- which(wa_l_y > 0) # variables on the top half
# Replace values in the vector
wa_l_pos <- replace(wa_l_pos, wa_lo, "1")
wa_l_pos <- replace(wa_l_pos, wa_hi, "3")

# Variable labels
text(wa_l_x, wa_l_y, labels=arrow_lab, col=arrow_col, pos=wa_l_pos, cex=1.5)

#title(main = "(e) WA", cex.main = 2)
mtext("(e) WA", side = 3, line = 0.3, adj = 0, cex = 1.5)


#species present at all sites
#biplot
#biplot(sp_allst_pca)
plot(sp_allst_pca$x[,1], sp_allst_pca$x[,2], xlab=paste("PCA 1 (", round(sum_sp_allst$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sum_sp_allst$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2, xlim= c(-6,6))

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(sp_allst_pca$x[,1], sp_allst_pca$x[,2], labels=row.names(sp_allst_pca$x), pos=c(1,4,2,3), font=2)

sp_allst_arrow_col <- rep(arrow_col, each=5)
sp_allst_arrow_lab <- rep(arrow_lab, each=5)

indcp <- seq(1,45, 5)
inddp <- seq(2,45, 5)
indlc <- seq(3,45, 5)
indsv <- seq(4,45, 5)
indwa <- seq(5,45, 5)

sp_allst_arrow_lab[indcp] <- paste(sp_allst_arrow_lab[indcp], "(CP)", sep = "")
sp_allst_arrow_lab[inddp] <- paste(sp_allst_arrow_lab[inddp], "(DP)", sep = "")
sp_allst_arrow_lab[indlc] <- paste(sp_allst_arrow_lab[indlc], "(LC)", sep = "")
sp_allst_arrow_lab[indsv] <- paste(sp_allst_arrow_lab[indsv], "(SV)", sep = "")
sp_allst_arrow_lab[indwa] <- paste(sp_allst_arrow_lab[indwa], "(WA)", sep = "")

# Draw arrows, i multiply by 6 to make arrows longer
sp_allst_l_x <- sp_allst_pca$rotation[,1]*12
sp_allst_l_y <- sp_allst_pca$rotation[,2]*12
arrows(x0=0, x1=sp_allst_l_x, y0=0, y1=sp_allst_l_y, col=sp_allst_arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
sp_allst_l_pos <- sp_allst_l_y # Create a vector of y axis coordinates
sp_allst_lo <- which(sp_allst_l_y < 0) #  variables on the bottom half of the plot
sp_allst_hi <- which(sp_allst_l_y > 0) # variables on the top half
# Replace values in the vector
sp_allst_l_pos <- replace(sp_allst_l_pos, sp_allst_lo, "1")
sp_allst_l_pos <- replace(sp_allst_l_pos, sp_allst_hi, "3")

# Variable labels
text(sp_allst_l_x, sp_allst_l_y, labels=sp_allst_arrow_lab, col=sp_allst_arrow_col, pos=sp_allst_l_pos, cex=1.5)

#title(main = "(f) ALL SITES", cex.main = 2)
mtext("(f) All sites", side = 3, line = 0.3, adj = 0, cex = 1.5)

dev.off()




pdf(paste("DIETBREADTH_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_PLOT.pdf",sep=""),width=12,height=17)

layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))

par(oma=c(1,0.3,0,0.3), mar=c(6,5,3,2))

arrow_col <- c(rep("red", 3),  # red for spring max temp
               rep("#87CEEB", 3), # blue for winter precipitation
               rep("black", 3)) # green for spring min temp

arrow_lab <- rep(c("MO", "TO", "LO"), 3)

#biplot
#biplot(cppca)
plot(cppca$x[,1], cppca$x[,2], xlab=paste("PCA 1 (", round(sumcp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumcp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[diet_cc5_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(cppca$x[,1], cppca$x[,2], labels=row.names(cppca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 5 to make arrows longer
cp_l_x <- cppca$rotation[,1]*4.5
cp_l_y <- cppca$rotation[,2]*4.5
arrows(x0=0, x1=cp_l_x, y0=0, y1=cp_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
cp_l_pos <- cp_l_y # Create a vector of y axis coordinates
cp_lo <- which(cp_l_y < 0) #  variables on the bottom half of the plot
cp_hi <- which(cp_l_y > 0) # variables on the top half
# Replace values in the vector
cp_l_pos <- replace(cp_l_pos, cp_lo, "1")
cp_l_pos <- replace(cp_l_pos, cp_hi, "3")

# Variable labels
#text(cp_l_x, cp_l_y, labels=row.names(cppca$rotation), col=arrow_col, pos=cp_l_pos, cex=1.5)
text(cp_l_x, cp_l_y, labels=arrow_lab, col=arrow_col, pos=cp_l_pos, cex=1.5)

#title(main = "(a) CP", cex.main = 2)
mtext("(a) CP", side = 3, line = 0.3, adj = 0, cex = 1.5)
# legend 
legend("topleft", legend = c("Monophagous", "Polyphagous"), col = alpha(c("blue",  "green")), pch = 19, cex = 1.5, bty= "n")



#biplot
#biplot(dppca)
plot(dppca$x[,1], dppca$x[,2], xlab=paste("PCA 1 (", round(sumdp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumdp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[diet_cc4_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(dppca$x[,1], dppca$x[,2], labels=row.names(dppca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
dp_l_x <- dppca$rotation[,1]*5
dp_l_y <- dppca$rotation[,2]*5
arrows(x0=0, x1=dp_l_x, y0=0, y1=dp_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
dp_l_pos <- dp_l_y # Create a vector of y axis coordinates
dp_lo <- which(dp_l_y < 0) #  variables on the bottom half of the plot
dp_hi <- which(dp_l_y > 0) # variables on the top half
# Replace values in the vector
dp_l_pos <- replace(dp_l_pos, dp_lo, "1")
dp_l_pos <- replace(dp_l_pos, dp_hi, "3")

# Variable labels
text(dp_l_x, dp_l_y, labels=arrow_lab, col=arrow_col, pos=dp_l_pos, cex=1.5)
#title(main = "(b) DP", cex.main = 2)
mtext("(b) DP", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(lcpca)
plot(lcpca$x[,1], lcpca$x[,2], xlab=paste("PCA 1 (", round(sumlc$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumlc$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[diet_cc3_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(lcpca$x[,1], lcpca$x[,2], labels=row.names(lcpca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
lc_l_x <- lcpca$rotation[,1]*5
lc_l_y <- lcpca$rotation[,2]*5
arrows(x0=0, x1=lc_l_x, y0=0, y1=lc_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
lc_l_pos <- lc_l_y # Create a vector of y axis coordinates
lc_lo <- which(lc_l_y < 0) #  variables on the bottom half of the plot
lc_hi <- which(lc_l_y > 0) # variables on the top half
# Replace values in the vector
lc_l_pos <- replace(lc_l_pos, lc_lo, "1")
lc_l_pos <- replace(lc_l_pos, lc_hi, "3")

# Variable labels
text(lc_l_x, lc_l_y, labels=arrow_lab, col=arrow_col, pos=lc_l_pos, cex=1.5)
#title(main = "(c) LC", cex.main = 2)
mtext("(c) LC", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(svpca)
plot(svpca$x[,1], svpca$x[,2], xlab=paste("PCA 1 (", round(sumsv$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumsv$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[diet_cc2_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(svpca$x[,1], svpca$x[,2], labels=row.names(svpca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 4 to make arrows longer
sv_l_x <- svpca$rotation[,1]*4
sv_l_y <- svpca$rotation[,2]*4
arrows(x0=0, x1=sv_l_x, y0=0, y1=sv_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
sv_l_pos <- sv_l_y # Create a vector of y axis coordinates
sv_lo <- which(sv_l_y < 0) #  variables on the bottom half of the plot
sv_hi <- which(sv_l_y > 0) # variables on the top half
# Replace values in the vector
sv_l_pos <- replace(sv_l_pos, sv_lo, "1")
sv_l_pos <- replace(sv_l_pos, sv_hi, "3")

# Variable labels
text(sv_l_x, sv_l_y, labels=arrow_lab, col=arrow_col, pos=sv_l_pos, cex=1.5)
#title(main = "(d) SV", cex.main = 2)
mtext("(d) SV", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(wapca)
plot(wapca$x[,1], wapca$x[,2], xlab=paste("PCA 1 (", round(sumwa$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumwa$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[diet_cc1_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(wapca$x[,1], wapca$x[,2], labels=row.names(wapca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 6 to make arrows longer
wa_l_x <- wapca$rotation[,1]*5
wa_l_y <- wapca$rotation[,2]*5
arrows(x0=0, x1=wa_l_x, y0=0, y1=wa_l_y, col=arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
wa_l_pos <- wa_l_y # Create a vector of y axis coordinates
wa_lo <- which(wa_l_y < 0) #  variables on the bottom half of the plot
wa_hi <- which(wa_l_y > 0) # variables on the top half
# Replace values in the vector
wa_l_pos <- replace(wa_l_pos, wa_lo, "1")
wa_l_pos <- replace(wa_l_pos, wa_hi, "3")

# Variable labels
text(wa_l_x, wa_l_y, labels=arrow_lab, col=arrow_col, pos=wa_l_pos, cex=1.5)

#title(main = "(e) WA", cex.main = 2)
mtext("(e) WA", side = 3, line = 0.3, adj = 0, cex = 1.5)


#species present at all sites
#biplot
#biplot(sp_allst_pca)
plot(sp_allst_pca$x[,1], sp_allst_pca$x[,2], xlab=paste("PCA 1 (", round(sum_sp_allst$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sum_sp_allst$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2, xlim= c(-6,6))

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(sp_allst_pca$x[,1], sp_allst_pca$x[,2], labels=row.names(sp_allst_pca$x), pos=c(1,4,2,3), font=2)

sp_allst_arrow_col <- rep(arrow_col, each=5)
sp_allst_arrow_lab <- rep(arrow_lab, each=5)

indcp <- seq(1,45, 5)
inddp <- seq(2,45, 5)
indlc <- seq(3,45, 5)
indsv <- seq(4,45, 5)
indwa <- seq(5,45, 5)

sp_allst_arrow_lab[indcp] <- paste(sp_allst_arrow_lab[indcp], "(CP)", sep = "")
sp_allst_arrow_lab[inddp] <- paste(sp_allst_arrow_lab[inddp], "(DP)", sep = "")
sp_allst_arrow_lab[indlc] <- paste(sp_allst_arrow_lab[indlc], "(LC)", sep = "")
sp_allst_arrow_lab[indsv] <- paste(sp_allst_arrow_lab[indsv], "(SV)", sep = "")
sp_allst_arrow_lab[indwa] <- paste(sp_allst_arrow_lab[indwa], "(WA)", sep = "")

# Draw arrows, i multiply by 6 to make arrows longer
sp_allst_l_x <- sp_allst_pca$rotation[,1]*12
sp_allst_l_y <- sp_allst_pca$rotation[,2]*12
arrows(x0=0, x1=sp_allst_l_x, y0=0, y1=sp_allst_l_y, col=sp_allst_arrow_col, length=0.15, lwd=1.5)

#label drawn arrows
# Label position
sp_allst_l_pos <- sp_allst_l_y # Create a vector of y axis coordinates
sp_allst_lo <- which(sp_allst_l_y < 0) #  variables on the bottom half of the plot
sp_allst_hi <- which(sp_allst_l_y > 0) # variables on the top half
# Replace values in the vector
sp_allst_l_pos <- replace(sp_allst_l_pos, sp_allst_lo, "1")
sp_allst_l_pos <- replace(sp_allst_l_pos, sp_allst_hi, "3")

# Variable labels
text(sp_allst_l_x, sp_allst_l_y, labels=sp_allst_arrow_lab, col=sp_allst_arrow_col, pos=sp_allst_l_pos, cex=1.5)

#title(main = "(f) ALL SITES", cex.main = 2)
mtext("(f) All sites", side = 3, line = 0.3, adj = 0, cex = 1.5)

dev.off()


















############permutation test (for testing null hypothesis)################
####testing whether the observed eucledian distance is significantly 
####large compared to the null distribution.




pdf(paste("RESIDENT_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_EUCLIDEAN_DISTANCE_PLOT.pdf",sep=""),width=26,height=8)
layout(matrix(1:5, ncol=5))
par(oma=c(1,0.3,0,0.3), mar=c(8,7,5,2), mgp = c(5, 2, 0))

#CP
#pc1
pc_1 <- cppca$x[,1]
#pc2
pc_2 <- cppca$x[,2]
#residency
res <- res_cc5_b1
# Centroid for resident  
ctd_1 <- c(mean(pc_1[res == 1]), mean(pc_2[res == 1]))  
# Centroid for non resident 
ctd_2 <- c(mean(pc_1[res == 2]), mean(pc_2[res == 2]))  
# Compute Euclidean distance  
ecdi_cp <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_cp <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample without keeping the number of resident or non resident constant
  #random_res <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  
  # Random sample  keeping the number of resident or non resident constant
  random_res <- sample(res)
  #random_res <- sample(res, size = length(cssp), replace = FALSE)
  
  # Centroid for resident 
  ctd_1 <- c(mean(pc_1[random_res == 1]), mean(pc_2[random_res == 1]))  
  
  # Centroid for non resident  
  ctd_2 <- c(mean(pc_1[random_res == 2]), mean(pc_2[random_res == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_cp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 
#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_cp <- mean(sim_ecdi_cp >= ecdi_cp)

# Density plots
plot(density(sim_ecdi_cp, bw = 0.1),
     main = "(a) CP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_cp, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_cp > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_cp), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2)  


#DP
#pc1
pc_1 <- dppca$x[,1]
#pc2
pc_2 <- dppca$x[,2]
#residency
res <- res_cc4_b1
# Centroid for resident  
ctd_1 <- c(mean(pc_1[res == 1]), mean(pc_2[res == 1]))  
# Centroid for non resident  
ctd_2 <- c(mean(pc_1[res == 2]), mean(pc_2[res == 2]))  
# Compute Euclidean distance  
ecdi_dp <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_dp <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_res <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_res <- sample(res)
  
  # Centroid for resident  
  ctd_1 <- c(mean(pc_1[random_res == 1]), mean(pc_2[random_res == 1]))  
  
  # Centroid for non resident  
  ctd_2 <- c(mean(pc_1[random_res == 2]), mean(pc_2[random_res == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_dp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_dp <- mean(sim_ecdi_dp >= ecdi_dp)

# Density plots
plot(density(sim_ecdi_dp, bw = 0.1), 
     main = "(b) DP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_dp, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_dp > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_dp), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

#LC
#pc1
pc_1 <- lcpca$x[,1]
#pc2
pc_2 <- lcpca$x[,2]
#residency
res <- res_cc3_b1
# Centroid for resident  
ctd_1 <- c(mean(pc_1[res == 1]), mean(pc_2[res == 1]))  
# Centroid for non resident  
ctd_2 <- c(mean(pc_1[res == 2]), mean(pc_2[res == 2]))  
# Compute Euclidean distance  
ecdi_lc <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_lc <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_res <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_res <- sample(res)
  
  # Centroid for resident  
  ctd_1 <- c(mean(pc_1[random_res == 1]), mean(pc_2[random_res == 1]))  
  
  # Centroid for non resident  
  ctd_2 <- c(mean(pc_1[random_res == 2]), mean(pc_2[random_res == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_lc[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_lc <- mean(sim_ecdi_lc >= ecdi_lc)

# Density plots
plot(density(sim_ecdi_lc, bw = 0.1), 
     main = "(c) LC",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_lc, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_lc > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_lc), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

#SV
#pc1
pc_1 <- svpca$x[,1]
#pc2
pc_2 <- svpca$x[,2]
#residency
res <- res_cc2_b1
# Centroid for resident  
ctd_1 <- c(mean(pc_1[res == 1]), mean(pc_2[res == 1]))  
# Centroid for non resident  
ctd_2 <- c(mean(pc_1[res == 2]), mean(pc_2[res == 2]))  
# Compute Euclidean distance  
ecdi_sv <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_sv <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_res <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_res <- sample(res)
  
  # Centroid for resident  
  ctd_1 <- c(mean(pc_1[random_res == 1]), mean(pc_2[random_res == 1]))  
  
  # Centroid for non resident  
  ctd_2 <- c(mean(pc_1[random_res == 2]), mean(pc_2[random_res == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_sv[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_sv <- mean(sim_ecdi_sv >= ecdi_sv)

# Density plots
plot(density(sim_ecdi_sv, bw = 0.1),
     main = "(d) SV",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_sv, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_sv > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_sv), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

#WA
#pc1
pc_1 <- wapca$x[,1]
#pc2
pc_2 <- wapca$x[,2]
#residency
res <- res_cc1_b1
# Centroid for resident  
ctd_1 <- c(mean(pc_1[res == 1]), mean(pc_2[res == 1]))  
# Centroid for non resident  
ctd_2 <- c(mean(pc_1[res == 2]), mean(pc_2[res == 2]))  
# Compute Euclidean distance  
ecdi_wa <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_wa <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_res <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_res <- sample(res)
  
  # Centroid for resident  
  ctd_1 <- c(mean(pc_1[random_res == 1]), mean(pc_2[random_res == 1]))  
  
  # Centroid for non resident  
  ctd_2 <- c(mean(pc_1[random_res == 2]), mean(pc_2[random_res == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_wa[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_wa <- mean(sim_ecdi_wa >= ecdi_wa)

# Density plots
plot(density(sim_ecdi_wa, bw = 0.1), 
     main = "(e) WA",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_wa, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_wa > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_wa), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

dev.off()


ecdi_cp
ecdi_dp
ecdi_lc
ecdi_sv
ecdi_wa

pv_cp
pv_dp
pv_lc
pv_sv
pv_wa












pdf(paste("WEEDY_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_EUCLIDEAN_DISTANCE_PLOT.pdf",sep=""),width=26,height=8)
layout(matrix(1:5, ncol=5))
par(oma=c(1,0.3,0,0.3), mar=c(8,7,5,2), mgp = c(5, 2, 0))

#CP
#pc1
pc_1 <- cppca$x[,1]
#pc2
pc_2 <- cppca$x[,2]
#weediness
wed <- wed_cc5_b1
# Centroid for weedy 
ctd_1 <- c(mean(pc_1[wed == 1]), mean(pc_2[wed == 1]))  
# Centroid for non weedy  
ctd_2 <- c(mean(pc_1[wed == 2]), mean(pc_2[wed == 2]))  
# Compute Euclidean distance  
ecdi_cp <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_cp <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample without keeping the number of weedyor non weedy constant
  #random_wed <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  
  # Random sample  keeping the number of weedyor non weedy constant
  random_wed <- sample(wed)
  #random_wed <- sample(wed, size = length(cssp), replace = FALSE)
  
  # Centroid for weedy 
  ctd_1 <- c(mean(pc_1[random_wed == 1]), mean(pc_2[random_wed == 1]))  
  
  # Centroid for non weedy  
  ctd_2 <- c(mean(pc_1[random_wed == 2]), mean(pc_2[random_wed == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_cp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 
#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_cp <- mean(sim_ecdi_cp >= ecdi_cp)

# Density plots
plot(density(sim_ecdi_cp, bw = 0.1),
     main = "(a) CP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_cp, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_cp > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_cp), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2)  


#DP
#pc1
pc_1 <- dppca$x[,1]
#pc2
pc_2 <- dppca$x[,2]
#weediness
wed <- wed_cc4_b1
# Centroid for weedy 
ctd_1 <- c(mean(pc_1[wed == 1]), mean(pc_2[wed == 1]))  
# Centroid for non weedy  
ctd_2 <- c(mean(pc_1[wed == 2]), mean(pc_2[wed == 2]))  
# Compute Euclidean distance  
ecdi_dp <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_dp <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_wed <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_wed <- sample(wed)
  
  # Centroid for weedy 
  ctd_1 <- c(mean(pc_1[random_wed == 1]), mean(pc_2[random_wed == 1]))  
  
  # Centroid for non weedy  
  ctd_2 <- c(mean(pc_1[random_wed == 2]), mean(pc_2[random_wed == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_dp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_dp <- mean(sim_ecdi_dp >= ecdi_dp)

# Density plots
plot(density(sim_ecdi_dp, bw = 0.1), 
     main = "(b) DP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_dp, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_dp > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_dp), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

#LC
#pc1
pc_1 <- lcpca$x[,1]
#pc2
pc_2 <- lcpca$x[,2]
#weediness
wed <- wed_cc3_b1
# Centroid for weedy 
ctd_1 <- c(mean(pc_1[wed == 1]), mean(pc_2[wed == 1]))  
# Centroid for non weedy  
ctd_2 <- c(mean(pc_1[wed == 2]), mean(pc_2[wed == 2]))  
# Compute Euclidean distance  
ecdi_lc <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_lc <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_wed <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_wed <- sample(wed)
  
  # Centroid for weedy 
  ctd_1 <- c(mean(pc_1[random_wed == 1]), mean(pc_2[random_wed == 1]))  
  
  # Centroid for non weedy  
  ctd_2 <- c(mean(pc_1[random_wed == 2]), mean(pc_2[random_wed == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_lc[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_lc <- mean(sim_ecdi_lc >= ecdi_lc)

# Density plots
plot(density(sim_ecdi_lc, bw = 0.1), 
     main = "(c) LC",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_lc, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_lc > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_lc), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

#SV
#pc1
pc_1 <- svpca$x[,1]
#pc2
pc_2 <- svpca$x[,2]
#weediness
wed <- wed_cc2_b1
# Centroid for weedy 
ctd_1 <- c(mean(pc_1[wed == 1]), mean(pc_2[wed == 1]))  
# Centroid for non weedy  
ctd_2 <- c(mean(pc_1[wed == 2]), mean(pc_2[wed == 2]))  
# Compute Euclidean distance  
ecdi_sv <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_sv <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_wed <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_wed <- sample(wed)
  
  # Centroid for weedy 
  ctd_1 <- c(mean(pc_1[random_wed == 1]), mean(pc_2[random_wed == 1]))  
  
  # Centroid for non weedy  
  ctd_2 <- c(mean(pc_1[random_wed == 2]), mean(pc_2[random_wed == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_sv[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_sv <- mean(sim_ecdi_sv >= ecdi_sv)

# Density plots
plot(density(sim_ecdi_sv, bw = 0.1),
     main = "(d) SV",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_sv, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_sv > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_sv), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

#WA
#pc1
pc_1 <- wapca$x[,1]
#pc2
pc_2 <- wapca$x[,2]
#weediness
wed <- wed_cc1_b1
# Centroid for weedy 
ctd_1 <- c(mean(pc_1[wed == 1]), mean(pc_2[wed == 1]))  
# Centroid for non weedy  
ctd_2 <- c(mean(pc_1[wed == 2]), mean(pc_2[wed == 2]))  
# Compute Euclidean distance  
ecdi_wa <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_wa <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_wed <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_wed <- sample(wed)
  
  # Centroid for weedy 
  ctd_1 <- c(mean(pc_1[random_wed == 1]), mean(pc_2[random_wed == 1]))  
  
  # Centroid for non weedy  
  ctd_2 <- c(mean(pc_1[random_wed == 2]), mean(pc_2[random_wed == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_wa[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_wa <- mean(sim_ecdi_wa >= ecdi_wa)

# Density plots
plot(density(sim_ecdi_wa, bw = 0.1), 
     main = "(e) WA",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_wa, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_wa > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_wa), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

dev.off()


ecdi_cp
ecdi_dp
ecdi_lc
ecdi_sv
ecdi_wa

pv_cp
pv_dp
pv_lc
pv_sv
pv_wa










pdf(paste("VOLTINISM_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_EUCLIDEAN_DISTANCE_PLOT.pdf",sep=""),width=26,height=8)
layout(matrix(1:5, ncol=5))
par(oma=c(1,0.3,0,0.3), mar=c(8,7,5,2), mgp = c(5, 2, 0))

#CP
#pc1
pc_1 <- cppca$x[,1]
#pc2
pc_2 <- cppca$x[,2]
#voltinism
vol <- vol_cc5_b1
# Centroid for univoltine  
ctd_1 <- c(mean(pc_1[vol == 1]), mean(pc_2[vol == 1]))  
# Centroid for multivoltine  
ctd_2 <- c(mean(pc_1[vol == 2]), mean(pc_2[vol == 2]))  
# Compute Euclidean distance  
ecdi_cp <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_cp <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample without keeping the number of univoltine or multivoltine constant
  #random_vol <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  
  # Random sample  keeping the number of univoltine or multivoltine constant
  random_vol <- sample(vol)
  #random_vol <- sample(vol, size = length(cssp), replace = FALSE)
  
  # Centroid for univoltine  
  ctd_1 <- c(mean(pc_1[random_vol == 1]), mean(pc_2[random_vol == 1]))  
  
  # Centroid for multivoltine  
  ctd_2 <- c(mean(pc_1[random_vol == 2]), mean(pc_2[random_vol == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_cp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 
#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_cp <- mean(sim_ecdi_cp >= ecdi_cp)

# Density plots
plot(density(sim_ecdi_cp, bw = 0.1),
     main = "(a) CP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_cp, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_cp > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_cp), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2)  


#DP
#pc1
pc_1 <- dppca$x[,1]
#pc2
pc_2 <- dppca$x[,2]
#voltinism
vol <- vol_cc4_b1
# Centroid for univoltine  
ctd_1 <- c(mean(pc_1[vol == 1]), mean(pc_2[vol == 1]))  
# Centroid for multivoltine  
ctd_2 <- c(mean(pc_1[vol == 2]), mean(pc_2[vol == 2]))  
# Compute Euclidean distance  
ecdi_dp <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_dp <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_vol <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_vol <- sample(vol)
  
  # Centroid for univoltine  
  ctd_1 <- c(mean(pc_1[random_vol == 1]), mean(pc_2[random_vol == 1]))  
  
  # Centroid for multivoltine  
  ctd_2 <- c(mean(pc_1[random_vol == 2]), mean(pc_2[random_vol == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_dp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_dp <- mean(sim_ecdi_dp >= ecdi_dp)

# Density plots
plot(density(sim_ecdi_dp, bw = 0.1), 
     main = "(b) DP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_dp, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_dp > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_dp), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

#LC
#pc1
pc_1 <- lcpca$x[,1]
#pc2
pc_2 <- lcpca$x[,2]
#voltinism
vol <- vol_cc3_b1
# Centroid for univoltine  
ctd_1 <- c(mean(pc_1[vol == 1]), mean(pc_2[vol == 1]))  
# Centroid for multivoltine  
ctd_2 <- c(mean(pc_1[vol == 2]), mean(pc_2[vol == 2]))  
# Compute Euclidean distance  
ecdi_lc <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_lc <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_vol <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_vol <- sample(vol)
  
  # Centroid for univoltine  
  ctd_1 <- c(mean(pc_1[random_vol == 1]), mean(pc_2[random_vol == 1]))  
  
  # Centroid for multivoltine  
  ctd_2 <- c(mean(pc_1[random_vol == 2]), mean(pc_2[random_vol == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_lc[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_lc <- mean(sim_ecdi_lc >= ecdi_lc)

# Density plots
plot(density(sim_ecdi_lc, bw = 0.1), 
     main = "(c) LC",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_lc, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_lc > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_lc), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

#SV
#pc1
pc_1 <- svpca$x[,1]
#pc2
pc_2 <- svpca$x[,2]
#voltinism
vol <- vol_cc2_b1
# Centroid for univoltine  
ctd_1 <- c(mean(pc_1[vol == 1]), mean(pc_2[vol == 1]))  
# Centroid for multivoltine  
ctd_2 <- c(mean(pc_1[vol == 2]), mean(pc_2[vol == 2]))  
# Compute Euclidean distance  
ecdi_sv <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_sv <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_vol <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_vol <- sample(vol)
  
  # Centroid for univoltine  
  ctd_1 <- c(mean(pc_1[random_vol == 1]), mean(pc_2[random_vol == 1]))  
  
  # Centroid for multivoltine  
  ctd_2 <- c(mean(pc_1[random_vol == 2]), mean(pc_2[random_vol == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_sv[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_sv <- mean(sim_ecdi_sv >= ecdi_sv)

# Density plots
plot(density(sim_ecdi_sv, bw = 0.1),
     main = "(d) SV",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_sv, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_sv > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_sv), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

#WA
#pc1
pc_1 <- wapca$x[,1]
#pc2
pc_2 <- wapca$x[,2]
#voltinism
vol <- vol_cc1_b1
# Centroid for univoltine  
ctd_1 <- c(mean(pc_1[vol == 1]), mean(pc_2[vol == 1]))  
# Centroid for multivoltine  
ctd_2 <- c(mean(pc_1[vol == 2]), mean(pc_2[vol == 2]))  
# Compute Euclidean distance  
ecdi_wa <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_wa <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_vol <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_vol <- sample(vol)
  
  # Centroid for univoltine  
  ctd_1 <- c(mean(pc_1[random_vol == 1]), mean(pc_2[random_vol == 1]))  
  
  # Centroid for multivoltine  
  ctd_2 <- c(mean(pc_1[random_vol == 2]), mean(pc_2[random_vol == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_wa[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_wa <- mean(sim_ecdi_wa >= ecdi_wa)

# Density plots
plot(density(sim_ecdi_wa, bw = 0.1), 
     main = "(e) WA",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_wa, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_wa > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_wa), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

dev.off()


ecdi_cp
ecdi_dp
ecdi_lc
ecdi_sv
ecdi_wa

pv_cp
pv_dp
pv_lc
pv_sv
pv_wa














pdf(paste("DIETBREADTH_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_EUCLIDEAN_DISTANCE_PLOT.pdf",sep=""),width=26,height=8)
layout(matrix(1:5, ncol=5))
par(oma=c(1,0.3,0,0.3), mar=c(8,7,5,2), mgp = c(5, 2, 0))

#CP
#pc1
pc_1 <- cppca$x[,1]
#pc2
pc_2 <- cppca$x[,2]
#diet breadth
diet <- diet_cc5_b1
# Centroid for monophagous  
ctd_1 <- c(mean(pc_1[diet == 1]), mean(pc_2[diet == 1]))  
# Centroid for polyphagous  
ctd_2 <- c(mean(pc_1[diet == 2]), mean(pc_2[diet == 2]))  
# Compute Euclidean distance  
ecdi_cp <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_cp <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample without keeping the number of monophagous or polyphagous constant
  #random_diet <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  
  # Random sample  keeping the number of monophagous or polyphagous constant
  random_diet <- sample(diet)
  #random_diet <- sample(diet, size = length(cssp), replace = FALSE)
  
  # Centroid for monophagous  
  ctd_1 <- c(mean(pc_1[random_diet == 1]), mean(pc_2[random_diet == 1]))  
  
  # Centroid for polyphagous  
  ctd_2 <- c(mean(pc_1[random_diet == 2]), mean(pc_2[random_diet == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_cp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 
#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_cp <- mean(sim_ecdi_cp >= ecdi_cp)

# Density plots
plot(density(sim_ecdi_cp, bw = 0.1),
     main = "(a) CP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_cp, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_cp > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_cp), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2)  


#DP
#pc1
pc_1 <- dppca$x[,1]
#pc2
pc_2 <- dppca$x[,2]
#diet breadth
diet <- diet_cc4_b1
# Centroid for monophagous  
ctd_1 <- c(mean(pc_1[diet == 1]), mean(pc_2[diet == 1]))  
# Centroid for polyphagous  
ctd_2 <- c(mean(pc_1[diet == 2]), mean(pc_2[diet == 2]))  
# Compute Euclidean distance  
ecdi_dp <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_dp <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_diet <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_diet <- sample(diet)
  
  # Centroid for monophagous  
  ctd_1 <- c(mean(pc_1[random_diet == 1]), mean(pc_2[random_diet == 1]))  
  
  # Centroid for polyphagous  
  ctd_2 <- c(mean(pc_1[random_diet == 2]), mean(pc_2[random_diet == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_dp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_dp <- mean(sim_ecdi_dp >= ecdi_dp)

# Density plots
plot(density(sim_ecdi_dp, bw = 0.1), 
     main = "(b) DP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_dp, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_dp > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_dp), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

#LC
#pc1
pc_1 <- lcpca$x[,1]
#pc2
pc_2 <- lcpca$x[,2]
#diet breadth
diet <- diet_cc3_b1
# Centroid for monophagous  
ctd_1 <- c(mean(pc_1[diet == 1]), mean(pc_2[diet == 1]))  
# Centroid for polyphagous  
ctd_2 <- c(mean(pc_1[diet == 2]), mean(pc_2[diet == 2]))  
# Compute Euclidean distance  
ecdi_lc <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_lc <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_diet <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_diet <- sample(diet)
  
  # Centroid for monophagous  
  ctd_1 <- c(mean(pc_1[random_diet == 1]), mean(pc_2[random_diet == 1]))  
  
  # Centroid for polyphagous  
  ctd_2 <- c(mean(pc_1[random_diet == 2]), mean(pc_2[random_diet == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_lc[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_lc <- mean(sim_ecdi_lc >= ecdi_lc)

# Density plots
plot(density(sim_ecdi_lc, bw = 0.1), 
     main = "(c) LC",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_lc, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_lc > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_lc), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

#SV
#pc1
pc_1 <- svpca$x[,1]
#pc2
pc_2 <- svpca$x[,2]
#diet breadth
diet <- diet_cc2_b1
# Centroid for monophagous  
ctd_1 <- c(mean(pc_1[diet == 1]), mean(pc_2[diet == 1]))  
# Centroid for polyphagous  
ctd_2 <- c(mean(pc_1[diet == 2]), mean(pc_2[diet == 2]))  
# Compute Euclidean distance  
ecdi_sv <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_sv <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_diet <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_diet <- sample(diet)
  
  # Centroid for monophagous  
  ctd_1 <- c(mean(pc_1[random_diet == 1]), mean(pc_2[random_diet == 1]))  
  
  # Centroid for polyphagous  
  ctd_2 <- c(mean(pc_1[random_diet == 2]), mean(pc_2[random_diet == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_sv[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_sv <- mean(sim_ecdi_sv >= ecdi_sv)

# Density plots
plot(density(sim_ecdi_sv, bw = 0.1),
     main = "(d) SV",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_sv, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_sv > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_sv), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

#WA
#pc1
pc_1 <- wapca$x[,1]
#pc2
pc_2 <- wapca$x[,2]
#diet breadth
diet <- diet_cc1_b1
# Centroid for monophagous  
ctd_1 <- c(mean(pc_1[diet == 1]), mean(pc_2[diet == 1]))  
# Centroid for polyphagous  
ctd_2 <- c(mean(pc_1[diet == 2]), mean(pc_2[diet == 2]))  
# Compute Euclidean distance  
ecdi_wa <- sqrt(sum((ctd_1 - ctd_2)^2))  

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_wa <- numeric(n_iter)  

for (i in 1:n_iter) {
  # Random sample
  #random_diet <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  random_diet <- sample(diet)
  
  # Centroid for monophagous  
  ctd_1 <- c(mean(pc_1[random_diet == 1]), mean(pc_2[random_diet == 1]))  
  
  # Centroid for polyphagous  
  ctd_2 <- c(mean(pc_1[random_diet == 2]), mean(pc_2[random_diet == 2]))  
  
  # Compute Euclidean distance  
  sim_ecdi_wa[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

 

#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_wa <- mean(sim_ecdi_wa >= ecdi_wa)

# Density plots
plot(density(sim_ecdi_wa, bw = 0.1), 
     main = "(e) WA",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_wa, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_wa > 0) {
  text(x = -1, y = 0.8, labels = paste("p =", pv_wa), cex = 3)
} else {
  text(x = -1, y = 0.8, labels = "p < 0.0001", cex = 3)
}

box(lwd = 2)  

dev.off()


ecdi_cp
ecdi_dp
ecdi_lc
ecdi_sv
ecdi_wa

pv_cp
pv_dp
pv_lc
pv_sv
pv_wa


















pdf(paste("OVERWINTERING_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_EUCLIDEAN_DISTANCE_PLOT.pdf",sep=""),width=28,height=30)
layout(matrix(1:30, ncol=5))
par(oma=c(1,0.3,0,0.3), mar=c(8,7,5,2), mgp = c(5, 2, 0))

#CP
#pc1
pc_1 <- cppca$x[,1]
#pc2
pc_2 <- cppca$x[,2]
#overwintering stage
ove <- ove_cc5_b1

# Centroid for egg  
ctd_1 <- c(mean(pc_1[ove == 1]), mean(pc_2[ove == 1]))  
# Centroid for larva  
ctd_2 <- c(mean(pc_1[ove == 2]), mean(pc_2[ove == 2])) 
# Centroid for pupa
ctd_3 <- c(mean(pc_1[ove == 3]), mean(pc_2[ove == 3]))  
# Centroid for adult
ctd_4 <- c(mean(pc_1[ove == 4]), mean(pc_2[ove == 4]))  

# Euclidean distance function
euclidean_distance <- function(a, b) sqrt(sum((a - b)^2))

# Compute Euclidean distance  
ecdi_cp_1_2  <- euclidean_distance(ctd_1, ctd_2)
ecdi_cp_1_3  <- euclidean_distance(ctd_1, ctd_3)
ecdi_cp_1_4  <- euclidean_distance(ctd_1, ctd_4)
ecdi_cp_2_3  <- euclidean_distance(ctd_2, ctd_3)
ecdi_cp_2_4  <- euclidean_distance(ctd_2, ctd_4)
ecdi_cp_3_4  <- euclidean_distance(ctd_3, ctd_4)

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_cp_1_2  <- numeric(n_iter)
sim_ecdi_cp_1_3  <- numeric(n_iter)
sim_ecdi_cp_1_4  <- numeric(n_iter)
sim_ecdi_cp_2_3  <- numeric(n_iter)
sim_ecdi_cp_2_4  <- numeric(n_iter)
sim_ecdi_cp_3_4  <- numeric(n_iter)

for (i in 1:n_iter) {
  # Random sample without keeping the number of egg or larva constant
  #random_ove <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  
  # Random sample  keeping the number of egg or larva constant
  random_ove <- sample(ove)
  #random_ove <- sample(ove, size = length(cssp), replace = FALSE)
  
  # Centroid for egg  
  ctd_1 <- c(mean(pc_1[random_ove == 1]), mean(pc_2[random_ove == 1]))  
  # Centroid for larva  
  ctd_2 <- c(mean(pc_1[random_ove == 2]), mean(pc_2[random_ove == 2])) 
  # Centroid for pupa
  ctd_3 <- c(mean(pc_1[random_ove == 3]), mean(pc_2[random_ove == 3]))  
  # Centroid for adult
  ctd_4 <- c(mean(pc_1[random_ove == 4]), mean(pc_2[random_ove == 4]))  
  
  
  # Compute Euclidean distance  
  sim_ecdi_cp_1_2[i]  <- euclidean_distance(ctd_1, ctd_2)
  sim_ecdi_cp_1_3[i]  <- euclidean_distance(ctd_1, ctd_3)
  sim_ecdi_cp_1_4[i]  <- euclidean_distance(ctd_1, ctd_4)
  sim_ecdi_cp_2_3[i]  <- euclidean_distance(ctd_2, ctd_3)
  sim_ecdi_cp_2_4[i]  <- euclidean_distance(ctd_2, ctd_4)
  sim_ecdi_cp_3_4[i]  <- euclidean_distance(ctd_3, ctd_4)
  
}


#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_cp_1_2 <- mean(sim_ecdi_cp_1_2 >= ecdi_cp_1_2)
pv_cp_1_3 <- mean(sim_ecdi_cp_1_3 >= ecdi_cp_1_3)
pv_cp_1_4 <- mean(sim_ecdi_cp_1_4 >= ecdi_cp_1_4)
pv_cp_2_3 <- mean(sim_ecdi_cp_2_3 >= ecdi_cp_2_3)
pv_cp_2_4 <- mean(sim_ecdi_cp_2_4 >= ecdi_cp_2_4)
pv_cp_3_4 <- mean(sim_ecdi_cp_3_4 >= ecdi_cp_3_4)

# Density plots
plot(density(sim_ecdi_cp_1_2, bw = 0.1),
     main = "a(i) CP: egg-larva",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_cp_1_2, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_cp_1_2 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_cp_1_2), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2)  



# Density plots
plot(density(sim_ecdi_cp_1_3, bw = 0.1),
     main = "a(ii) CP: egg-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_cp_1_3, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_cp_1_3 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_cp_1_3), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 


# Density plots
plot(density(sim_ecdi_cp_1_4, bw = 0.1),
     main = "a(iii) CP: egg-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_cp_1_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_cp_1_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_cp_1_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 



# Density plots
plot(density(sim_ecdi_cp_2_3, bw = 0.1),
     main = "a(iv) CP: larva-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_cp_2_3, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_cp_2_3 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_cp_2_3), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 


# Density plots
plot(density(sim_ecdi_cp_2_4, bw = 0.1),
     main = "a(v) CP: larva-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_cp_2_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_cp_2_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_cp_2_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 



# Density plots
plot(density(sim_ecdi_cp_3_4, bw = 0.1),
     main = "a(vi) CP: pupa-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_cp_3_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_cp_3_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_cp_3_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 






#DP
#pc1
pc_1 <- dppca$x[,1]
#pc2
pc_2 <- dppca$x[,2]
#overwintering stage
ove <- ove_cc4_b1
# Centroid for egg  
ctd_1 <- c(mean(pc_1[ove == 1]), mean(pc_2[ove == 1]))  
# Centroid for larva  
ctd_2 <- c(mean(pc_1[ove == 2]), mean(pc_2[ove == 2])) 
# Centroid for pupa
ctd_3 <- c(mean(pc_1[ove == 3]), mean(pc_2[ove == 3]))  
# Centroid for adult
ctd_4 <- c(mean(pc_1[ove == 4]), mean(pc_2[ove == 4]))  

# Euclidean distance function
euclidean_distance <- function(a, b) sqrt(sum((a - b)^2))

# Compute Euclidean distance  
ecdi_dp_1_2  <- euclidean_distance(ctd_1, ctd_2)
ecdi_dp_1_3  <- euclidean_distance(ctd_1, ctd_3)
ecdi_dp_1_4  <- euclidean_distance(ctd_1, ctd_4)
ecdi_dp_2_3  <- euclidean_distance(ctd_2, ctd_3)
ecdi_dp_2_4  <- euclidean_distance(ctd_2, ctd_4)
ecdi_dp_3_4  <- euclidean_distance(ctd_3, ctd_4)

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_dp_1_2  <- numeric(n_iter)
sim_ecdi_dp_1_3  <- numeric(n_iter)
sim_ecdi_dp_1_4  <- numeric(n_iter)
sim_ecdi_dp_2_3  <- numeric(n_iter)
sim_ecdi_dp_2_4  <- numeric(n_iter)
sim_ecdi_dp_3_4  <- numeric(n_iter)

for (i in 1:n_iter) {
  # Random sample without keeping the number of egg or larva constant
  #random_ove <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  
  # Random sample  keeping the number of egg or larva constant
  random_ove <- sample(ove)
  #random_ove <- sample(ove, size = length(cssp), replace = FALSE)
  
  # Centroid for egg  
  ctd_1 <- c(mean(pc_1[random_ove == 1]), mean(pc_2[random_ove == 1]))  
  # Centroid for larva  
  ctd_2 <- c(mean(pc_1[random_ove == 2]), mean(pc_2[random_ove == 2])) 
  # Centroid for pupa
  ctd_3 <- c(mean(pc_1[random_ove == 3]), mean(pc_2[random_ove == 3]))  
  # Centroid for adult
  ctd_4 <- c(mean(pc_1[random_ove == 4]), mean(pc_2[random_ove == 4]))  
  
  # Compute Euclidean distance  
  sim_ecdi_dp_1_2[i]  <- euclidean_distance(ctd_1, ctd_2)
  sim_ecdi_dp_1_3[i]  <- euclidean_distance(ctd_1, ctd_3)
  sim_ecdi_dp_1_4[i]  <- euclidean_distance(ctd_1, ctd_4)
  sim_ecdi_dp_2_3[i]  <- euclidean_distance(ctd_2, ctd_3)
  sim_ecdi_dp_2_4[i]  <- euclidean_distance(ctd_2, ctd_4)
  sim_ecdi_dp_3_4[i]  <- euclidean_distance(ctd_3, ctd_4)
  
}


#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_dp_1_2 <- mean(sim_ecdi_dp_1_2 >= ecdi_dp_1_2)
pv_dp_1_3 <- mean(sim_ecdi_dp_1_3 >= ecdi_dp_1_3)
pv_dp_1_4 <- mean(sim_ecdi_dp_1_4 >= ecdi_dp_1_4)
pv_dp_2_3 <- mean(sim_ecdi_dp_2_3 >= ecdi_dp_2_3)
pv_dp_2_4 <- mean(sim_ecdi_dp_2_4 >= ecdi_dp_2_4)
pv_dp_3_4 <- mean(sim_ecdi_dp_3_4 >= ecdi_dp_3_4)

# Density plots
plot(density(sim_ecdi_dp_1_2, bw = 0.1),
     main = "b(i) DP: egg-larva",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_dp_1_2, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_dp_1_2 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_dp_1_2), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2)  



# Density plots
plot(density(sim_ecdi_dp_1_3, bw = 0.1),
     main = "b(ii) DP: egg-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_dp_1_3, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_dp_1_3 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_dp_1_3), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 


# Density plots
plot(density(sim_ecdi_dp_1_4, bw = 0.1),
     main = "b(iii) DP: egg-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_dp_1_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_dp_1_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_dp_1_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 



# Density plots
plot(density(sim_ecdi_dp_2_3, bw = 0.1),
     main = "b(iv) DP: larva-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_dp_2_3, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_dp_2_3 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_dp_2_3), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 


# Density plots
plot(density(sim_ecdi_dp_2_4, bw = 0.1),
     main = "b(v) DP: larva-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_dp_2_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_dp_2_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_dp_2_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 



# Density plots
plot(density(sim_ecdi_dp_3_4, bw = 0.1),
     main = "b(vi) DP: pupa-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_dp_3_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_dp_3_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_dp_3_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 




#LC
#pc1
pc_1 <- lcpca$x[,1]
#pc2
pc_2 <- lcpca$x[,2]
#overwintering stage
ove <- ove_cc3_b1
# Centroid for egg  
ctd_1 <- c(mean(pc_1[ove == 1]), mean(pc_2[ove == 1]))  
# Centroid for larva  
ctd_2 <- c(mean(pc_1[ove == 2]), mean(pc_2[ove == 2])) 
# Centroid for pupa
ctd_3 <- c(mean(pc_1[ove == 3]), mean(pc_2[ove == 3]))  
# Centroid for adult
ctd_4 <- c(mean(pc_1[ove == 4]), mean(pc_2[ove == 4]))  

# Euclidean distance function
euclidean_distance <- function(a, b) sqrt(sum((a - b)^2))

# Compute Euclidean distance  
ecdi_lc_1_2  <- euclidean_distance(ctd_1, ctd_2)
ecdi_lc_1_3  <- euclidean_distance(ctd_1, ctd_3)
ecdi_lc_1_4  <- euclidean_distance(ctd_1, ctd_4)
ecdi_lc_2_3  <- euclidean_distance(ctd_2, ctd_3)
ecdi_lc_2_4  <- euclidean_distance(ctd_2, ctd_4)
ecdi_lc_3_4  <- euclidean_distance(ctd_3, ctd_4)

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_lc_1_2  <- numeric(n_iter)
sim_ecdi_lc_1_3  <- numeric(n_iter)
sim_ecdi_lc_1_4  <- numeric(n_iter)
sim_ecdi_lc_2_3  <- numeric(n_iter)
sim_ecdi_lc_2_4  <- numeric(n_iter)
sim_ecdi_lc_3_4  <- numeric(n_iter)

for (i in 1:n_iter) {
  # Random sample without keeping the number of egg or larva constant
  #random_ove <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  
  # Random sample  keeping the number of egg or larva constant
  random_ove <- sample(ove)
  #random_ove <- sample(ove, size = length(cssp), replace = FALSE)
  
  # Centroid for egg  
  ctd_1 <- c(mean(pc_1[random_ove == 1]), mean(pc_2[random_ove == 1]))  
  # Centroid for larva  
  ctd_2 <- c(mean(pc_1[random_ove == 2]), mean(pc_2[random_ove == 2])) 
  # Centroid for pupa
  ctd_3 <- c(mean(pc_1[random_ove == 3]), mean(pc_2[random_ove == 3]))  
  # Centroid for adult
  ctd_4 <- c(mean(pc_1[random_ove == 4]), mean(pc_2[random_ove == 4]))  
  
  
  # Compute Euclidean distance  
  sim_ecdi_lc_1_2[i]  <- euclidean_distance(ctd_1, ctd_2)
  sim_ecdi_lc_1_3[i]  <- euclidean_distance(ctd_1, ctd_3)
  sim_ecdi_lc_1_4[i]  <- euclidean_distance(ctd_1, ctd_4)
  sim_ecdi_lc_2_3[i]  <- euclidean_distance(ctd_2, ctd_3)
  sim_ecdi_lc_2_4[i]  <- euclidean_distance(ctd_2, ctd_4)
  sim_ecdi_lc_3_4[i]  <- euclidean_distance(ctd_3, ctd_4)
  
}


#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_lc_1_2 <- mean(sim_ecdi_lc_1_2 >= ecdi_lc_1_2)
pv_lc_1_3 <- mean(sim_ecdi_lc_1_3 >= ecdi_lc_1_3)
pv_lc_1_4 <- mean(sim_ecdi_lc_1_4 >= ecdi_lc_1_4)
pv_lc_2_3 <- mean(sim_ecdi_lc_2_3 >= ecdi_lc_2_3)
pv_lc_2_4 <- mean(sim_ecdi_lc_2_4 >= ecdi_lc_2_4)
pv_lc_3_4 <- mean(sim_ecdi_lc_3_4 >= ecdi_lc_3_4)

# Density plots
plot(density(sim_ecdi_lc_1_2, bw = 0.1),
     main = "c(i) LC: egg-larva",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_lc_1_2, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_lc_1_2 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_lc_1_2), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2)  



# Density plots
plot(density(sim_ecdi_lc_1_3, bw = 0.1),
     main = "c(ii) LC: egg-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_lc_1_3, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_lc_1_3 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_lc_1_3), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 


# Density plots
plot(density(sim_ecdi_lc_1_4, bw = 0.1),
     main = "c(iii) LC: egg-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_lc_1_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_lc_1_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_lc_1_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 



# Density plots
plot(density(sim_ecdi_lc_2_3, bw = 0.1),
     main = "c(iv) LC: larva-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_lc_2_3, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_lc_2_3 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_lc_2_3), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 


# Density plots
plot(density(sim_ecdi_lc_2_4, bw = 0.1),
     main = "c(v) LC: larva-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_lc_2_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_lc_2_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_lc_2_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 



# Density plots
plot(density(sim_ecdi_lc_3_4, bw = 0.1),
     main = "c(vi) LC: pupa-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_lc_3_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_lc_3_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_lc_3_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 



#SV
#pc1
pc_1 <- svpca$x[,1]
#pc2
pc_2 <- svpca$x[,2]
#overwintering stage
ove <- ove_cc2_b1
# Centroid for egg  
ctd_1 <- c(mean(pc_1[ove == 1]), mean(pc_2[ove == 1]))  
# Centroid for larva  
ctd_2 <- c(mean(pc_1[ove == 2]), mean(pc_2[ove == 2])) 
# Centroid for pupa
ctd_3 <- c(mean(pc_1[ove == 3]), mean(pc_2[ove == 3]))  
# Centroid for adult
ctd_4 <- c(mean(pc_1[ove == 4]), mean(pc_2[ove == 4]))  

# Euclidean distance function
euclidean_distance <- function(a, b) sqrt(sum((a - b)^2))

# Compute Euclidean distance  
ecdi_sv_1_2  <- euclidean_distance(ctd_1, ctd_2)
ecdi_sv_1_3  <- euclidean_distance(ctd_1, ctd_3)
ecdi_sv_1_4  <- euclidean_distance(ctd_1, ctd_4)
ecdi_sv_2_3  <- euclidean_distance(ctd_2, ctd_3)
ecdi_sv_2_4  <- euclidean_distance(ctd_2, ctd_4)
ecdi_sv_3_4  <- euclidean_distance(ctd_3, ctd_4)

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_sv_1_2  <- numeric(n_iter)
sim_ecdi_sv_1_3  <- numeric(n_iter)
sim_ecdi_sv_1_4  <- numeric(n_iter)
sim_ecdi_sv_2_3  <- numeric(n_iter)
sim_ecdi_sv_2_4  <- numeric(n_iter)
sim_ecdi_sv_3_4  <- numeric(n_iter)

for (i in 1:n_iter) {
  # Random sample without keeping the number of egg or larva constant
  #random_ove <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  
  # Random sample  keeping the number of egg or larva constant
  random_ove <- sample(ove)
  #random_ove <- sample(ove, size = length(cssp), replace = FALSE)
  
  # Centroid for egg  
  ctd_1 <- c(mean(pc_1[random_ove == 1]), mean(pc_2[random_ove == 1]))  
  # Centroid for larva  
  ctd_2 <- c(mean(pc_1[random_ove == 2]), mean(pc_2[random_ove == 2])) 
  # Centroid for pupa
  ctd_3 <- c(mean(pc_1[random_ove == 3]), mean(pc_2[random_ove == 3]))  
  # Centroid for adult
  ctd_4 <- c(mean(pc_1[random_ove == 4]), mean(pc_2[random_ove == 4]))  
  
  
  # Compute Euclidean distance  
  sim_ecdi_sv_1_2[i]  <- euclidean_distance(ctd_1, ctd_2)
  sim_ecdi_sv_1_3[i]  <- euclidean_distance(ctd_1, ctd_3)
  sim_ecdi_sv_1_4[i]  <- euclidean_distance(ctd_1, ctd_4)
  sim_ecdi_sv_2_3[i]  <- euclidean_distance(ctd_2, ctd_3)
  sim_ecdi_sv_2_4[i]  <- euclidean_distance(ctd_2, ctd_4)
  sim_ecdi_sv_3_4[i]  <- euclidean_distance(ctd_3, ctd_4)
  
}


#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_sv_1_2 <- mean(sim_ecdi_sv_1_2 >= ecdi_sv_1_2)
pv_sv_1_3 <- mean(sim_ecdi_sv_1_3 >= ecdi_sv_1_3)
pv_sv_1_4 <- mean(sim_ecdi_sv_1_4 >= ecdi_sv_1_4)
pv_sv_2_3 <- mean(sim_ecdi_sv_2_3 >= ecdi_sv_2_3)
pv_sv_2_4 <- mean(sim_ecdi_sv_2_4 >= ecdi_sv_2_4)
pv_sv_3_4 <- mean(sim_ecdi_sv_3_4 >= ecdi_sv_3_4)

# Density plots
plot(density(sim_ecdi_sv_1_2, bw = 0.1),
     main = "d(i) SV: egg-larva",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_sv_1_2, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_sv_1_2 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_sv_1_2), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2)  



# Density plots
plot(density(sim_ecdi_sv_1_3, bw = 0.1),
     main = "d(ii) SV: egg-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_sv_1_3, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_sv_1_3 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_sv_1_3), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 


# Density plots
plot(density(sim_ecdi_cp_1_4, bw = 0.1),
     main = "d(iii) SV: egg-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_sv_1_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_sv_1_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_sv_1_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 



# Density plots
plot(density(sim_ecdi_sv_2_3, bw = 0.1),
     main = "d(iv) SV: larva-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_sv_2_3, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_sv_2_3 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_sv_2_3), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 


# Density plots
plot(density(sim_ecdi_sv_2_4, bw = 0.1),
     main = "d(v) SV: larva-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_sv_2_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_sv_2_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_sv_2_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 



# Density plots
plot(density(sim_ecdi_sv_3_4, bw = 0.1),
     main = "d(vi) SV: pupa-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_sv_3_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_sv_3_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_sv_3_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 




#WA
#pc1
pc_1 <- wapca$x[,1]
#pc2
pc_2 <- wapca$x[,2]
#overwintering stage
ove <- ove_cc1_b1
# Centroid for egg  
ctd_1 <- c(mean(pc_1[ove == 1]), mean(pc_2[ove == 1]))  
# Centroid for larva  
ctd_2 <- c(mean(pc_1[ove == 2]), mean(pc_2[ove == 2])) 
# Centroid for pupa
ctd_3 <- c(mean(pc_1[ove == 3]), mean(pc_2[ove == 3]))  
# Centroid for adult
ctd_4 <- c(mean(pc_1[ove == 4]), mean(pc_2[ove == 4]))  

# Euclidean distance function
euclidean_distance <- function(a, b) sqrt(sum((a - b)^2))

# Compute Euclidean distance  
ecdi_wa_1_2  <- euclidean_distance(ctd_1, ctd_2)
ecdi_wa_1_3  <- euclidean_distance(ctd_1, ctd_3)
ecdi_wa_1_4  <- euclidean_distance(ctd_1, ctd_4)
ecdi_wa_2_3  <- euclidean_distance(ctd_2, ctd_3)
ecdi_wa_2_4  <- euclidean_distance(ctd_2, ctd_4)
ecdi_wa_3_4  <- euclidean_distance(ctd_3, ctd_4)

#simulated random euclidean distances
set.seed(100) 
n_iter <- 10000  # Number of iterations
# Preallocating vector for the euclidean distance of each random sample
sim_ecdi_wa_1_2  <- numeric(n_iter)
sim_ecdi_wa_1_3  <- numeric(n_iter)
sim_ecdi_wa_1_4  <- numeric(n_iter)
sim_ecdi_wa_2_3  <- numeric(n_iter)
sim_ecdi_wa_2_4  <- numeric(n_iter)
sim_ecdi_wa_3_4  <- numeric(n_iter)

for (i in 1:n_iter) {
  # Random sample without keeping the number of egg or larva constant
  #random_ove <- sample(c(1, 2), size = length(cssp), replace = TRUE)
  
  # Random sample  keeping the number of egg or larva constant
  random_ove <- sample(ove)
  #random_ove <- sample(ove, size = length(cssp), replace = FALSE)
  
  # Centroid for egg  
  ctd_1 <- c(mean(pc_1[random_ove == 1]), mean(pc_2[random_ove == 1]))  
  # Centroid for larva  
  ctd_2 <- c(mean(pc_1[random_ove == 2]), mean(pc_2[random_ove == 2])) 
  # Centroid for pupa
  ctd_3 <- c(mean(pc_1[random_ove == 3]), mean(pc_2[random_ove == 3]))  
  # Centroid for adult
  ctd_4 <- c(mean(pc_1[random_ove == 4]), mean(pc_2[random_ove == 4]))  
  
  # Compute Euclidean distance  
  sim_ecdi_wa_1_2[i]  <- euclidean_distance(ctd_1, ctd_2)
  sim_ecdi_wa_1_3[i]  <- euclidean_distance(ctd_1, ctd_3)
  sim_ecdi_wa_1_4[i]  <- euclidean_distance(ctd_1, ctd_4)
  sim_ecdi_wa_2_3[i]  <- euclidean_distance(ctd_2, ctd_3)
  sim_ecdi_wa_2_4[i]  <- euclidean_distance(ctd_2, ctd_4)
  sim_ecdi_wa_3_4[i]  <- euclidean_distance(ctd_3, ctd_4)
  
}

 
#pvalue
#proportion of simulated euclidean distance greater or equal to 
#observed euclidean distance
pv_wa_1_2 <- mean(sim_ecdi_wa_1_2 >= ecdi_wa_1_2)
pv_wa_1_3 <- mean(sim_ecdi_wa_1_3 >= ecdi_wa_1_3)
pv_wa_1_4 <- mean(sim_ecdi_wa_1_4 >= ecdi_wa_1_4)
pv_wa_2_3 <- mean(sim_ecdi_wa_2_3 >= ecdi_wa_2_3)
pv_wa_2_4 <- mean(sim_ecdi_wa_2_4 >= ecdi_wa_2_4)
pv_wa_3_4 <- mean(sim_ecdi_wa_3_4 >= ecdi_wa_3_4)

# Density plots
plot(density(sim_ecdi_wa_1_2, bw = 0.1),
     main = "e(i) WA: egg-larva",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_wa_1_2, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_wa_1_2 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_wa_1_2), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2)  



# Density plots
plot(density(sim_ecdi_wa_1_3, bw = 0.1),
     main = "e(ii) WA: egg-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_wa_1_3, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_wa_1_3 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_wa_1_3), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 


# Density plots
plot(density(sim_ecdi_wa_1_4, bw = 0.1),
     main = "e(iii) WA: egg-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_wa_1_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_wa_1_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_wa_1_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 



# Density plots
plot(density(sim_ecdi_wa_2_3, bw = 0.1),
     main = "e(iv) WA: larva-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_wa_2_3, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_wa_2_3 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_wa_2_3), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 


# Density plots
plot(density(sim_ecdi_wa_2_4, bw = 0.1),
     main = "e(v) WA: larva-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_wa_2_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_wa_2_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_wa_2_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 



# Density plots
plot(density(sim_ecdi_wa_3_4, bw = 0.1),
     main = "e(vi) WA: pupa-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# observed euclidean distance
abline(v=ecdi_wa_3_4, col = "red", lwd = 2, lty = 2)  # Red dashed line
#annotate p-value
if (pv_wa_3_4 > 0) {
  text(x = -1, y = 0.4, labels = paste("p =", pv_wa_3_4), cex = 3)
} else {
  text(x = -1, y = 0.4, labels = "p < 0.0001", cex = 3)
}



box(lwd = 2) 


dev.off()




ecdi_cp_1_2 
ecdi_cp_1_3 
ecdi_cp_1_4 
ecdi_cp_2_3
ecdi_cp_2_4 
ecdi_cp_3_4 

pv_cp_1_2 
pv_cp_1_3 
pv_cp_1_4 
pv_cp_2_3 
pv_cp_2_4 
pv_cp_3_4 






ecdi_dp_1_2 
ecdi_dp_1_3 
ecdi_dp_1_4 
ecdi_dp_2_3
ecdi_dp_2_4 
ecdi_dp_3_4 

pv_dp_1_2 
pv_dp_1_3 
pv_dp_1_4 
pv_dp_2_3 
pv_dp_2_4 
pv_dp_3_4 




ecdi_lc_1_2 
ecdi_lc_1_3 
ecdi_lc_1_4 
ecdi_lc_2_3
ecdi_lc_2_4 
ecdi_lc_3_4 

pv_lc_1_2 
pv_lc_1_3 
pv_lc_1_4 
pv_lc_2_3 
pv_lc_2_4 
pv_lc_3_4 



ecdi_sv_1_2 
ecdi_sv_1_3 
ecdi_sv_1_4 
ecdi_sv_2_3
ecdi_sv_2_4 
ecdi_sv_3_4 

pv_sv_1_2 
pv_sv_1_3 
pv_sv_1_4 
pv_sv_2_3 
pv_sv_2_4 
pv_sv_3_4 



ecdi_wa_1_2 
ecdi_wa_1_3 
ecdi_wa_1_4 
ecdi_wa_2_3
ecdi_wa_2_4 
ecdi_wa_3_4 

pv_wa_1_2 
pv_wa_1_3 
pv_wa_1_4 
pv_wa_2_3 
pv_wa_2_4 
pv_wa_3_4 
