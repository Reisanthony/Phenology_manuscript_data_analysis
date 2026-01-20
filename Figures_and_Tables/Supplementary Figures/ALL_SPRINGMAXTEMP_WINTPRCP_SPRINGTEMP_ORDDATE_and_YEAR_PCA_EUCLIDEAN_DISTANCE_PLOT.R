library(rstan)
library(scales)

#CASTLE PEAK
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Castle Peak.rdat')
csp= length(sp)
cssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
cb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
cb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
cb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
cb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
cb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
cb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
cb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
cb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
cb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


cpspbe <- cbind(cb1, cb7, cb8, cb2, cb9, cb10, cb3, cb11, cb12)
colnames(cpspbe) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                      "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                      "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")
rownames(cpspbe) <- sp

# PCA 
cppca <- prcomp(cpspbe, scale=TRUE)
# Summary
sumcp <- summary(cppca)


#DONNER PASS
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Donner Pass.rdat')
dsp= length(sp)
dssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
db1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
db7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
db8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
db2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
db9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
db10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
db3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
db11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
db12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)

dpspbe <- cbind(db1, db7, db8, db2, db9, db10, db3, db11, db12)
colnames(dpspbe) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                      "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                      "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")
rownames(dpspbe) <- sp

# PCA 
dppca <- prcomp(dpspbe, scale=TRUE)
# Summary
sumdp <- summary(dppca)


#LANG CROSSING
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Lang Crossing.rdat')
lsp= length(sp)
lssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
lb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
lb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
lb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
lb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
lb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
lb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
lb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
lb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
lb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)

lcspbe <- cbind(lb1, lb7, lb8, lb2, lb9, lb10, lb3, lb11, lb12)
colnames(lcspbe) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                      "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                      "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")
rownames(lcspbe) <- sp
# PCA 
lcpca <- prcomp(lcspbe, scale=TRUE)
# Summary
sumlc <- summary(lcpca)


#SIERRA VALLEY
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Sierra Valley.rdat')
ssp= length(sp)
sssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
sb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
sb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
sb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
sb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
sb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
sb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
sb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
sb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
sb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


svspbe <- cbind(sb1, sb7, sb8, sb2, sb9, sb10, sb3, sb11, sb12)
colnames(svspbe) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                      "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                      "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")
rownames(svspbe) <- sp
# PCA 
svpca <- prcomp(svspbe, scale=TRUE)
# Summary
sumsv <- summary(svpca)


#WashingtonSHINGTON
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Washington.rdat')
wsp= length(sp)
wssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
wb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
wb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
wb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
wb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
wb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
wb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
wb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
wb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
wb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


waspbe <- cbind(wb1, wb7, wb8, wb2, wb9, wb10, wb3, wb11, wb12)

colnames(waspbe) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                      "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                      "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")

rownames(waspbe) <- sp
# PCA 
wapca <- prcomp(waspbe, scale=TRUE)
# Summary
sumwa <- summary(wapca)



#natural history data
nath <- read.csv("natural_history_2024.csv")
nath <- subset(nath,
               #when its a resident but we dont know its overwintering stage
               !(resident == "yes" & is.na(wintering)) &
                 #when we dont know its residency status but we know its overwintering stage
                 !(is.na(resident) & !is.na(wintering)) &
                 #when its not a resident but we know its overwintering stage
                 !(resident == "no" & !(wintering %in% c("no", NA))))

#converting to just univoltine and multivoltine
nath$broods[!is.na(nath$broods) & nath$broods != "one"] <- "multiple"

#converting hostgenera into monophagous and polyphagous
nath$hostgenera <- 
  ifelse(is.na(nath$hostgenera), NA,         
         ifelse(nath$hostgenera == 1, "one",        
                ifelse(nath$hostgenera > 1, "multiple",    
                       nath$hostgenera)))

# Split dataset by site
nath_cp <- nath[nath$site_name == "Castle Peak", ]
nath_dp <- nath[nath$site_name == "Donner Pass", ]
nath_sv <- nath[nath$site_name == "Sierra Valley", ]
nath_lc <- nath[nath$site_name == "Lang Crossing", ]
nath_wa <- nath[nath$site_name == "Washington", ]


# Assign Residency-based color codes for each site
# (1 = Resident, 2 = Nonresident)
# Castle Peak
resnath_cp <- nath_cp[which(nath_cp$resident == "yes"), ]
resspp_cp <- unique(resnath_cp$genus_species)
res_1 <- na.omit(match(resspp_cp, cssp))

nonresnath_cp <- nath_cp[which(nath_cp$resident=="no"), ]
nonresspp_cp <- unique(nonresnath_cp$genus_species)
nonres_1 <- na.omit(match(nonresspp_cp, cssp))

res_cc5_b1 <- rep(0, length(cssp))
res_cc5_b1[res_1] <- 1
res_cc5_b1[nonres_1] <- 2

# Donner Pass
resnath_dp <- nath_dp[which(nath_dp$resident=="yes"), ]
resspp_dp <- unique(resnath_dp$genus_species)
res_2 <- na.omit(match(resspp_dp, dssp))

nonresnath_dp <- nath_dp[which(nath_dp$resident=="no"), ]
nonresspp_dp <- unique(nonresnath_dp$genus_species)
nonres_2 <- na.omit(match(nonresspp_dp, dssp))

res_cc4_b1 <- rep(0, length(dssp))
res_cc4_b1[res_2] <- 1
res_cc4_b1[nonres_2] <- 2

# Lang Crossing
resnath_lc <- nath_lc[which(nath_lc$resident=="yes"), ]
resspp_lc <- unique(resnath_lc$genus_species)
res_3 <- na.omit(match(resspp_lc, lssp))

nonresnath_lc <- nath_lc[which(nath_lc$resident=="no"), ]
nonresspp_lc <- unique(nonresnath_lc$genus_species)
nonres_3 <- na.omit(match(nonresspp_lc, lssp))

res_cc3_b1 <- rep(0, length(lssp))
res_cc3_b1[res_3] <- 1
res_cc3_b1[nonres_3] <- 2

# Sierra Valley
resnath_sv <- nath_sv[which(nath_sv$resident=="yes"), ]
resspp_sv <- unique(resnath_sv$genus_species)
res_4 <- na.omit(match(resspp_sv, sssp))

nonresnath_sv <- nath_sv[which(nath_sv$resident=="no"), ]
nonresspp_sv <- unique(nonresnath_sv$genus_species)
nonres_4 <- na.omit(match(nonresspp_sv, sssp))

res_cc2_b1 <- rep(0, length(sssp))
res_cc2_b1[res_4] <- 1
res_cc2_b1[nonres_4] <- 2

# Washington
resnath_wa <- nath_wa[which(nath_wa$resident=="yes"), ]
resspp_wa <- unique(resnath_wa$genus_species)
res_5<- na.omit(match(resspp_wa, wssp))

nonresnath_wa <- nath_wa[which(nath_wa$resident=="no"), ]
nonresspp_wa <- unique(nonresnath_wa$genus_species)
nonres_5<- na.omit(match(nonresspp_wa, wssp))

res_cc1_b1 <- rep(0, length(wssp))
res_cc1_b1[res_5] <- 1
res_cc1_b1[nonres_5] <- 2


# Assign Ruderal status-based color codes for each site
# (1 = Weedy, 2 = Nonweedy)
# Castle Peak
wednath_cp <- nath_cp[which(nath_cp$weedy==1), ]
wedspp_cp <- unique(wednath_cp$genus_species)
wed_1 <- na.omit(match(wedspp_cp, cssp))

nonwednath_cp <- nath_cp[which(nath_cp$weedy==0), ]
nonwedspp_cp <- unique(nonwednath_cp$genus_species)
nonwed_1 <- na.omit(match(nonwedspp_cp, cssp))

wed_cc5_b1 <- rep(0, length(cssp))
wed_cc5_b1[wed_1] <- 1
wed_cc5_b1[nonwed_1] <- 2

#Donner Pass
wednath_dp <- nath_dp[which(nath_dp$weedy==1), ]
wedspp_dp <- unique(wednath_dp$genus_species)
wed_2 <- na.omit(match(wedspp_dp, dssp))

nonwednath_dp <- nath_dp[which(nath_dp$weedy==0), ]
nonwedspp_dp <- unique(nonwednath_dp$genus_species)
nonwed_2 <- na.omit(match(nonwedspp_dp, dssp))

wed_cc4_b1 <- rep(0, length(dssp))
wed_cc4_b1[wed_2] <- 1
wed_cc4_b1[nonwed_2] <- 2

#Lang Crossing
wednath_lc <- nath_lc[which(nath_lc$weedy==1), ]
wedspp_lc <- unique(wednath_lc$genus_species)
wed_3 <- na.omit(match(wedspp_lc, lssp))

nonwednath_lc <- nath_lc[which(nath_lc$weedy==0), ]
nonwedspp_lc <- unique(nonwednath_lc$genus_species)
nonwed_3 <- na.omit(match(nonwedspp_lc, lssp))

wed_cc3_b1 <- rep(0, length(lssp))
wed_cc3_b1[wed_3] <- 1
wed_cc3_b1[nonwed_3] <- 2

#Sierra Valley
wednath_sv <- nath_sv[which(nath_sv$weedy==1), ]
wedspp_sv <- unique(wednath_sv$genus_species)
wed_4 <- na.omit(match(wedspp_sv, sssp))

nonwednath_sv <- nath_sv[which(nath_sv$weedy==0), ]
nonwedspp_sv <- unique(nonwednath_sv$genus_species)
nonwed_4 <- na.omit(match(nonwedspp_sv, sssp))

wed_cc2_b1 <- rep(0, length(sssp))
wed_cc2_b1[wed_4] <- 1
wed_cc2_b1[nonwed_4] <- 2

#Washington
wednath_wa <- nath_wa[which(nath_wa$weedy==1), ]
wedspp_wa <- unique(wednath_wa$genus_species)
wed_5<- na.omit(match(wedspp_wa, wssp))

nonwednath_wa <- nath_wa[which(nath_wa$weedy==0), ]
nonwedspp_wa <- unique(nonwednath_wa$genus_species)
nonwed_5<- na.omit(match(nonwedspp_wa, wssp))

wed_cc1_b1 <- rep(0, length(wssp))
wed_cc1_b1[wed_5] <- 1
wed_cc1_b1[nonwed_5] <- 2




# Assign voltinism-based color codes for each site
# (1 = univoltine, 2 = multivoltine)
# Castle Peak
volonenath_cp <- nath_cp[nath_cp$broods=="one", ]
volonespp_cp <- unique(volonenath_cp$genus_species)
volone_1 <- na.omit(match(volonespp_cp, cssp))

volmultinath_cp <- nath_cp[nath_cp$broods=="multiple", ]
volmultispp_cp <- unique(volmultinath_cp$genus_species)
volmulti_1 <- na.omit(match(volmultispp_cp, cssp))

vol_cc5_b1 <- rep(0, length(cssp))
vol_cc5_b1[volone_1] <- 1
vol_cc5_b1[volmulti_1] <- 2

# Donner Pass
volonenath_dp <- nath_dp[nath_dp$broods=="one", ]
volonespp_dp <- unique(volonenath_dp$genus_species)
volone_2 <- na.omit(match(volonespp_dp, dssp))

volmultinath_dp <- nath_dp[nath_dp$broods=="multiple", ]
volmultispp_dp <- unique(volmultinath_dp$genus_species)
volmulti_2 <- na.omit(match(volmultispp_dp, dssp))

vol_cc4_b1 <- rep(0, length(dssp))
vol_cc4_b1[volone_2] <- 1
vol_cc4_b1[volmulti_2] <- 2

# Lang Crossing
volonenath_lc <- nath_lc[nath_lc$broods=="one", ]
volonespp_lc <- unique(volonenath_lc$genus_species)
volone_3 <- na.omit(match(volonespp_lc, lssp))

volmultinath_lc <- nath_lc[nath_lc$broods=="multiple", ]
volmultispp_lc <- unique(volmultinath_lc$genus_species)
volmulti_3 <- na.omit(match(volmultispp_lc, lssp))

vol_cc3_b1 <- rep(0, length(lssp))
vol_cc3_b1[volone_3] <- 1
vol_cc3_b1[volmulti_3] <- 2

# Sierra Valley
volonenath_sv <- nath_sv[nath_sv$broods=="one", ]
volonespp_sv <- unique(volonenath_sv$genus_species)
volone_4 <- na.omit(match(volonespp_sv, sssp))

volmultinath_sv <- nath_sv[nath_sv$broods=="multiple", ]
volmultispp_sv <- unique(volmultinath_sv$genus_species)
volmulti_4 <- na.omit(match(volmultispp_sv, sssp))

vol_cc2_b1 <- rep(0, length(sssp))
vol_cc2_b1[volone_4] <- 1
vol_cc2_b1[volmulti_4] <- 2

# Washington
volonenath_wa <- nath_wa[nath_wa$broods=="one", ]
volonespp_wa <- unique(volonenath_wa$genus_species)
volone_5<- na.omit(match(volonespp_wa, wssp))

volmultinath_wa <- nath_wa[nath_wa$broods=="multiple", ]
volmultispp_wa <- unique(volmultinath_wa$genus_species)
volmulti_5<- na.omit(match(volmultispp_wa, wssp))

vol_cc1_b1 <- rep(0, length(wssp))
vol_cc1_b1[volone_5] <- 1
vol_cc1_b1[volmulti_5] <- 2



# Assign overwintering stage-based color codes for each site
# (1 = eggs, 2 = larvae, 3 = pupae, 4 = adult)
# Castle Peak
oveggnath_cp <- nath_cp[which(nath_cp$wintering== "egg"), ]
oveggspp_cp <- unique(oveggnath_cp$genus_species)
ovegg_1 <- na.omit(match(oveggspp_cp, cssp))

ovlarnath_cp <- nath_cp[which(nath_cp$wintering== "larva"), ]
ovlarspp_cp <- unique(ovlarnath_cp$genus_species)
ovlar_1 <- na.omit(match(ovlarspp_cp, cssp))

ovpupnath_cp <- nath_cp[which(nath_cp$wintering== "pupa"), ]
ovpupspp_cp <- unique(ovpupnath_cp$genus_species)
ovpup_1 <- na.omit(match(ovpupspp_cp, cssp))

ovadunath_cp <- nath_cp[which(nath_cp$wintering== "adult"), ]
ovaduspp_cp <- unique(ovadunath_cp$genus_species)
ovadu_1 <- na.omit(match(ovaduspp_cp, cssp))

ove_cc5_b1 <- rep(0, length(cssp))
ove_cc5_b1[ovegg_1] <- 1
ove_cc5_b1[ovlar_1] <- 2
ove_cc5_b1[ovpup_1] <- 3
ove_cc5_b1[ovadu_1] <- 4

# Donner Pass
oveggnath_dp <- nath_dp[which(nath_dp$wintering== "egg"), ]
oveggspp_dp <- unique(oveggnath_dp$genus_species)
ovegg_2 <- na.omit(match(oveggspp_dp, dssp))

ovlarnath_dp <- nath_dp[which(nath_dp$wintering== "larva"), ]
ovlarspp_dp <- unique(ovlarnath_dp$genus_species)
ovlar_2 <- na.omit(match(ovlarspp_dp, dssp))

ovpupnath_dp <- nath_dp[which(nath_dp$wintering== "pupa"), ]
ovpupspp_dp <- unique(ovpupnath_dp$genus_species)
ovpup_2 <- na.omit(match(ovpupspp_dp, dssp))

ovadunath_dp <- nath_dp[which(nath_dp$wintering== "adult"), ]
ovaduspp_dp <- unique(ovadunath_dp$genus_species)
ovadu_2 <- na.omit(match(ovaduspp_dp, dssp))

ove_cc4_b1 <- rep(0, length(dssp))
ove_cc4_b1[ovegg_2] <- 1
ove_cc4_b1[ovlar_2] <- 2
ove_cc4_b1[ovpup_2] <- 3
ove_cc4_b1[ovadu_2] <- 4

# Lang Crossing
oveggnath_lc <- nath_lc[which(nath_lc$wintering== "egg"), ]
oveggspp_lc <- unique(oveggnath_lc$genus_species)
ovegg_3 <- na.omit(match(oveggspp_lc, lssp))

ovlarnath_lc <- nath_lc[which(nath_lc$wintering== "larva"), ]
ovlarspp_lc <- unique(ovlarnath_lc$genus_species)
ovlar_3 <- na.omit(match(ovlarspp_lc, lssp))

ovpupnath_lc <- nath_lc[which(nath_lc$wintering== "pupa"), ]
ovpupspp_lc <- unique(ovpupnath_lc$genus_species)
ovpup_3 <- na.omit(match(ovpupspp_lc, lssp))

ovadunath_lc <- nath_lc[which(nath_lc$wintering== "adult"), ]
ovaduspp_lc <- unique(ovadunath_lc$genus_species)
ovadu_3 <- na.omit(match(ovaduspp_lc, lssp))

ove_cc3_b1 <- rep(0, length(lssp))
ove_cc3_b1[ovegg_3] <- 1
ove_cc3_b1[ovlar_3] <- 2
ove_cc3_b1[ovpup_3] <- 3
ove_cc3_b1[ovadu_3] <- 4

# Sierra Valley
oveggnath_sv <- nath_sv[which(nath_sv$wintering== "egg"), ]
oveggspp_sv <- unique(oveggnath_sv$genus_species)
ovegg_4 <- na.omit(match(oveggspp_sv, sssp))

ovlarnath_sv <- nath_sv[which(nath_sv$wintering== "larva"), ]
ovlarspp_sv <- unique(ovlarnath_sv$genus_species)
ovlar_4 <- na.omit(match(ovlarspp_sv, sssp))

ovpupnath_sv <- nath_sv[which(nath_sv$wintering== "pupa"), ]
ovpupspp_sv <- unique(ovpupnath_sv$genus_species)
ovpup_4 <- na.omit(match(ovpupspp_sv, sssp))

ovadunath_sv <- nath_sv[which(nath_sv$wintering== "adult"), ]
ovaduspp_sv <- unique(ovadunath_sv$genus_species)
ovadu_4 <- na.omit(match(ovaduspp_sv, sssp))

ove_cc2_b1 <- rep(0, length(sssp))
ove_cc2_b1[ovegg_4] <- 1
ove_cc2_b1[ovlar_4] <- 2
ove_cc2_b1[ovpup_4] <- 3
ove_cc2_b1[ovadu_4] <- 4

# Washington
oveggnath_wa <- nath_wa[which(nath_wa$wintering== "egg"), ]
oveggspp_wa <- unique(oveggnath_wa$genus_species)
ovegg_5<- na.omit(match(oveggspp_wa, wssp))

ovlarnath_wa <- nath_wa[which(nath_wa$wintering== "larva"), ]
ovlarspp_wa <- unique(ovlarnath_wa$genus_species)
ovlar_5<- na.omit(match(ovlarspp_wa, wssp))

ovpupnath_wa <- nath_wa[which(nath_wa$wintering== "pupa"), ]
ovpupspp_wa <- unique(ovpupnath_wa$genus_species)
ovpup_5<- na.omit(match(ovpupspp_wa, wssp))

ovadunath_wa <- nath_wa[which(nath_wa$wintering== "adult"), ]
ovaduspp_wa <- unique(ovadunath_wa$genus_species)
ovadu_5<- na.omit(match(ovaduspp_wa, wssp))

ove_cc1_b1 <- rep(0, length(wssp))
ove_cc1_b1[ovegg_5] <- 1
ove_cc1_b1[ovlar_5] <- 2
ove_cc1_b1[ovpup_5] <- 3
ove_cc1_b1[ovadu_5] <- 4


# Assign diet breadth-based color codes for each site
# (1 = monophagous, 2 = polyphagous)
# Castle Peak
dietonenath_cp <- nath_cp[which(nath_cp$hostgenera=="one"), ]
dietonespp_cp <- unique(dietonenath_cp$genus_species)
dietone_1 <- na.omit(match(dietonespp_cp, cssp))

dietmultinath_cp <- nath_cp[which(nath_cp$hostgenera=="multiple"), ]
dietmultispp_cp <- unique(dietmultinath_cp$genus_species)
dietmulti_1 <- na.omit(match(dietmultispp_cp, cssp))

diet_cc5_b1 <- rep(0, length(cssp))
diet_cc5_b1[dietone_1] <- 1
diet_cc5_b1[dietmulti_1] <- 2

# Donner Pass
dietonenath_dp <- nath_dp[which(nath_dp$hostgenera=="one"), ]
dietonespp_dp <- unique(dietonenath_dp$genus_species)
dietone_2 <- na.omit(match(dietonespp_dp, dssp))

dietmultinath_dp <- nath_dp[which(nath_dp$hostgenera=="multiple"), ]
dietmultispp_dp <- unique(dietmultinath_dp$genus_species)
dietmulti_2 <- na.omit(match(dietmultispp_dp, dssp))

diet_cc4_b1 <- rep(0, length(dssp))
diet_cc4_b1[dietone_2] <- 1
diet_cc4_b1[dietmulti_2] <- 2

# Lang Crossing
dietonenath_lc <- nath_lc[which(nath_lc$hostgenera=="one"), ]
dietonespp_lc <- unique(dietonenath_lc$genus_species)
dietone_3 <- na.omit(match(dietonespp_lc, lssp))

dietmultinath_lc <- nath_lc[which(nath_lc$hostgenera=="multiple"), ]
dietmultispp_lc <- unique(dietmultinath_lc$genus_species)
dietmulti_3 <- na.omit(match(dietmultispp_lc, lssp))

diet_cc3_b1 <- rep(0, length(lssp))
diet_cc3_b1[dietone_3] <- 1
diet_cc3_b1[dietmulti_3] <- 2

# Sierra Valley
dietonenath_sv <- nath_sv[which(nath_sv$hostgenera=="one"), ]
dietonespp_sv <- unique(dietonenath_sv$genus_species)
dietone_4 <- na.omit(match(dietonespp_sv, sssp))

dietmultinath_sv <- nath_sv[which(nath_sv$hostgenera=="multiple"), ]
dietmultispp_sv <- unique(dietmultinath_sv$genus_species)
dietmulti_4 <- na.omit(match(dietmultispp_sv, sssp))

diet_cc2_b1 <- rep(0, length(sssp))
diet_cc2_b1[dietone_4] <- 1
diet_cc2_b1[dietmulti_4] <- 2

# Washington
dietonenath_wa <- nath_wa[which(nath_wa$hostgenera=="one"), ]
dietonespp_wa <- unique(dietonenath_wa$genus_species)
dietone_5<- na.omit(match(dietonespp_wa, wssp))

dietmultinath_wa <- nath_wa[which(nath_wa$hostgenera=="multiple"), ]
dietmultispp_wa <- unique(dietmultinath_wa$genus_species)
dietmulti_5<- na.omit(match(dietmultispp_wa, wssp))

diet_cc1_b1 <- rep(0, length(wssp))
diet_cc1_b1[dietone_5] <- 1
diet_cc1_b1[dietmulti_5] <- 2




# Permutation test: Evaluating significance of Euclidean distance
# This analysis tests whether the observed Euclidean distance between the
# centroids of grouped species in PCA space (PC1–PC2)
# at each site is significantly larger than expected under a null
# model generated by random permutation.

#Castle Peak
#PC1
pc_1 <- cppca$x[,1]
#PC2
pc_2 <- cppca$x[,2]
#residency
res <- res_cc5_b1
# Centroid for resident  
ctd_1 <- c(mean(pc_1[res == 1]), mean(pc_2[res == 1]))  
# Centroid for non resident 
ctd_2 <- c(mean(pc_1[res == 2]), mean(pc_2[res == 2]))  
# Euclidean distance  
ecdi_cp <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
# Number of random permutations
n_iter <- 10000  
sim_ecdi_cp <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_res <- sample(res)
  # Centroid for resident 
  ctd_1 <- c(mean(pc_1[random_res == 1]), mean(pc_2[random_res == 1]))  
  # Centroid for non resident  
  ctd_2 <- c(mean(pc_1[random_res == 2]), mean(pc_2[random_res == 2]))  
  # Euclidean distance  
  sim_ecdi_cp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}

# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_cp <- mean(sim_ecdi_cp >= ecdi_cp)



#Donner Pass
#PC1
pc_1 <- dppca$x[,1]
#PC2
pc_2 <- dppca$x[,2]
#residency
res <- res_cc4_b1
# Centroid for resident  
ctd_1 <- c(mean(pc_1[res == 1]), mean(pc_2[res == 1]))  
# Centroid for non resident  
ctd_2 <- c(mean(pc_1[res == 2]), mean(pc_2[res == 2]))  
# Euclidean distance  
ecdi_dp <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_dp <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_res <- sample(res)
  # Centroid for resident  
  ctd_1 <- c(mean(pc_1[random_res == 1]), mean(pc_2[random_res == 1]))  
  # Centroid for non resident  
  ctd_2 <- c(mean(pc_1[random_res == 2]), mean(pc_2[random_res == 2]))  
  # Euclidean distance  
  sim_ecdi_dp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_dp <- mean(sim_ecdi_dp >= ecdi_dp)

#Lang Crossing
#PC1
pc_1 <- lcpca$x[,1]
#PC2
pc_2 <- lcpca$x[,2]
#residency
res <- res_cc3_b1
# Centroid for resident  
ctd_1 <- c(mean(pc_1[res == 1]), mean(pc_2[res == 1]))  
# Centroid for non resident  
ctd_2 <- c(mean(pc_1[res == 2]), mean(pc_2[res == 2]))  
# Euclidean distance  
ecdi_lc <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_lc <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_res <- sample(res)
  # Centroid for resident  
  ctd_1 <- c(mean(pc_1[random_res == 1]), mean(pc_2[random_res == 1]))  
  # Centroid for non resident  
  ctd_2 <- c(mean(pc_1[random_res == 2]), mean(pc_2[random_res == 2]))  
  # Euclidean distance  
  sim_ecdi_lc[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_lc <- mean(sim_ecdi_lc >= ecdi_lc)


#Sierra Valley
#PC1
pc_1 <- svpca$x[,1]
#PC2
pc_2 <- svpca$x[,2]
#residency
res <- res_cc2_b1
# Centroid for resident  
ctd_1 <- c(mean(pc_1[res == 1]), mean(pc_2[res == 1]))  
# Centroid for non resident  
ctd_2 <- c(mean(pc_1[res == 2]), mean(pc_2[res == 2]))  
# Euclidean distance  
ecdi_sv <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_sv <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_res <- sample(res)
  # Centroid for resident  
  ctd_1 <- c(mean(pc_1[random_res == 1]), mean(pc_2[random_res == 1]))  
  # Centroid for non resident  
  ctd_2 <- c(mean(pc_1[random_res == 2]), mean(pc_2[random_res == 2]))  
  # Euclidean distance  
  sim_ecdi_sv[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_sv <- mean(sim_ecdi_sv >= ecdi_sv)

#Washington
#PC1
pc_1 <- wapca$x[,1]
#PC2
pc_2 <- wapca$x[,2]
#residency
res <- res_cc1_b1
# Centroid for resident  
ctd_1 <- c(mean(pc_1[res == 1]), mean(pc_2[res == 1]))  
# Centroid for non resident  
ctd_2 <- c(mean(pc_1[res == 2]), mean(pc_2[res == 2]))  
# Euclidean distance  
ecdi_wa <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_wa <- numeric(n_iter)
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_res <- sample(res)
  # Centroid for resident  
  ctd_1 <- c(mean(pc_1[random_res == 1]), mean(pc_2[random_res == 1]))  
  # Centroid for non resident  
  ctd_2 <- c(mean(pc_1[random_res == 2]), mean(pc_2[random_res == 2]))  
  # Euclidean distance  
  sim_ecdi_wa[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_wa <- mean(sim_ecdi_wa >= ecdi_wa)




pdf(paste("RESIDENT_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_EUCLIDEAN_DISTANCE_PLOT.pdf",sep=""),width=20,height=4)
layout(matrix(1:5, ncol=5))
par(oma=c(1,1,0,0.3), mar=c(8,7.5,5,1.5), mgp = c(5, 2, 0))

# Density plots
plot(density(sim_ecdi_cp, bw = 0.1),
     main = "(a) CP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_cp, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_cp > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_cp), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  


# Density plots
plot(density(sim_ecdi_dp, bw = 0.1), 
     main = "(b) DP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_dp, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_dp > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_dp), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}

box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_lc, bw = 0.1), 
     main = "(c) LC",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_lc, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_lc > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_lc), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}

box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_sv, bw = 0.1),
     main = "(d) SV",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_sv, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_sv > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_sv), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}

box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_wa, bw = 0.1), 
     main = "(e) WA",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_wa, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_wa > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_wa), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
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





#Ruderal status
#Castle Peak
#PC1
pc_1 <- cppca$x[,1]
#PC2
pc_2 <- cppca$x[,2]
#weediness
wed <- wed_cc5_b1
# Centroid for weedy 
ctd_1 <- c(mean(pc_1[wed == 1]), mean(pc_2[wed == 1]))  
# Centroid for non weedy  
ctd_2 <- c(mean(pc_1[wed == 2]), mean(pc_2[wed == 2]))  
# Euclidean distance  
ecdi_cp <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_cp <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes  
  random_wed <- sample(wed)
  # Centroid for weedy 
  ctd_1 <- c(mean(pc_1[random_wed == 1]), mean(pc_2[random_wed == 1]))  
  # Centroid for non weedy  
  ctd_2 <- c(mean(pc_1[random_wed == 2]), mean(pc_2[random_wed == 2]))  
  # Euclidean distance  
  sim_ecdi_cp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_cp <- mean(sim_ecdi_cp >= ecdi_cp)


#Donner Pass
#PC1
pc_1 <- dppca$x[,1]
#PC2
pc_2 <- dppca$x[,2]
#weediness
wed <- wed_cc4_b1
# Centroid for weedy 
ctd_1 <- c(mean(pc_1[wed == 1]), mean(pc_2[wed == 1]))  
# Centroid for non weedy  
ctd_2 <- c(mean(pc_1[wed == 2]), mean(pc_2[wed == 2]))  
# Euclidean distance  
ecdi_dp <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_dp <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_wed <- sample(wed)
  # Centroid for weedy 
  ctd_1 <- c(mean(pc_1[random_wed == 1]), mean(pc_2[random_wed == 1]))  
  # Centroid for non weedy  
  ctd_2 <- c(mean(pc_1[random_wed == 2]), mean(pc_2[random_wed == 2]))  
  # Euclidean distance  
  sim_ecdi_dp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_dp <- mean(sim_ecdi_dp >= ecdi_dp)


#Lang Crossing
#PC1
pc_1 <- lcpca$x[,1]
#PC2
pc_2 <- lcpca$x[,2]
#weediness
wed <- wed_cc3_b1
# Centroid for weedy 
ctd_1 <- c(mean(pc_1[wed == 1]), mean(pc_2[wed == 1]))  
# Centroid for non weedy  
ctd_2 <- c(mean(pc_1[wed == 2]), mean(pc_2[wed == 2]))  
# Euclidean distance  
ecdi_lc <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_lc <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_wed <- sample(wed)
  # Centroid for weedy 
  ctd_1 <- c(mean(pc_1[random_wed == 1]), mean(pc_2[random_wed == 1]))  
  # Centroid for non weedy  
  ctd_2 <- c(mean(pc_1[random_wed == 2]), mean(pc_2[random_wed == 2]))  
  # Euclidean distance  
  sim_ecdi_lc[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_lc <- mean(sim_ecdi_lc >= ecdi_lc)


#Sierra Valley
#PC1
pc_1 <- svpca$x[,1]
#PC2
pc_2 <- svpca$x[,2]
#weediness
wed <- wed_cc2_b1
# Centroid for weedy 
ctd_1 <- c(mean(pc_1[wed == 1]), mean(pc_2[wed == 1]))  
# Centroid for non weedy  
ctd_2 <- c(mean(pc_1[wed == 2]), mean(pc_2[wed == 2]))  
# Euclidean distance  
ecdi_sv <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_sv <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_wed <- sample(wed)
  # Centroid for weedy 
  ctd_1 <- c(mean(pc_1[random_wed == 1]), mean(pc_2[random_wed == 1]))  
  # Centroid for non weedy  
  ctd_2 <- c(mean(pc_1[random_wed == 2]), mean(pc_2[random_wed == 2]))  
  # Euclidean distance  
  sim_ecdi_sv[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_sv <- mean(sim_ecdi_sv >= ecdi_sv)

#Washington
#PC1
pc_1 <- wapca$x[,1]
#PC2
pc_2 <- wapca$x[,2]
#weediness
wed <- wed_cc1_b1
# Centroid for weedy 
ctd_1 <- c(mean(pc_1[wed == 1]), mean(pc_2[wed == 1]))  
# Centroid for non weedy  
ctd_2 <- c(mean(pc_1[wed == 2]), mean(pc_2[wed == 2]))  
# Euclidean distance  
ecdi_wa <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_wa <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_wed <- sample(wed)
  # Centroid for weedy 
  ctd_1 <- c(mean(pc_1[random_wed == 1]), mean(pc_2[random_wed == 1]))  
  # Centroid for non weedy  
  ctd_2 <- c(mean(pc_1[random_wed == 2]), mean(pc_2[random_wed == 2]))  
  # Euclidean distance  
  sim_ecdi_wa[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_wa <- mean(sim_ecdi_wa >= ecdi_wa)


pdf(paste("WEEDY_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_EUCLIDEAN_DISTANCE_PLOT.pdf",sep=""),width=20,height=4)
layout(matrix(1:5, ncol=5))
par(oma=c(1,1,0,0.3), mar=c(8,7.5,5,1.5), mgp = c(5, 2, 0))

# Density plots
plot(density(sim_ecdi_cp, bw = 0.1),
     main = "(a) CP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_cp, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_cp > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_cp), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_dp, bw = 0.1), 
     main = "(b) DP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_dp, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_dp > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_dp), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}

box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_lc, bw = 0.1), 
     main = "(c) LC",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_lc, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_lc > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_lc), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_sv, bw = 0.1),
     main = "(d) SV",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_sv, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_sv > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_sv), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_wa, bw = 0.1), 
     main = "(e) WA",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_wa, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_wa > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_wa), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
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


#Voltinism
#Castle Peak
#PC1
pc_1 <- cppca$x[,1]
#PC2
pc_2 <- cppca$x[,2]
#voltinism
vol <- vol_cc5_b1
# Centroid for univoltine  
ctd_1 <- c(mean(pc_1[vol == 1]), mean(pc_2[vol == 1]))  
# Centroid for multivoltine  
ctd_2 <- c(mean(pc_1[vol == 2]), mean(pc_2[vol == 2]))  
# Euclidean distance  
ecdi_cp <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_cp <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes  
  random_vol <- sample(vol)
  # Centroid for univoltine  
  ctd_1 <- c(mean(pc_1[random_vol == 1]), mean(pc_2[random_vol == 1]))  
  # Centroid for multivoltine  
  ctd_2 <- c(mean(pc_1[random_vol == 2]), mean(pc_2[random_vol == 2]))  
  # Euclidean distance  
  sim_ecdi_cp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_cp <- mean(sim_ecdi_cp >= ecdi_cp)


#Donner Pass
#PC1
pc_1 <- dppca$x[,1]
#PC2
pc_2 <- dppca$x[,2]
#voltinism
vol <- vol_cc4_b1
# Centroid for univoltine  
ctd_1 <- c(mean(pc_1[vol == 1]), mean(pc_2[vol == 1]))  
# Centroid for multivoltine  
ctd_2 <- c(mean(pc_1[vol == 2]), mean(pc_2[vol == 2]))  
# Euclidean distance  
ecdi_dp <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_dp <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_vol <- sample(vol)
  # Centroid for univoltine  
  ctd_1 <- c(mean(pc_1[random_vol == 1]), mean(pc_2[random_vol == 1]))  
  # Centroid for multivoltine  
  ctd_2 <- c(mean(pc_1[random_vol == 2]), mean(pc_2[random_vol == 2]))  
  # Euclidean distance  
  sim_ecdi_dp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_dp <- mean(sim_ecdi_dp >= ecdi_dp)


#Lang Crossing
#PC1
pc_1 <- lcpca$x[,1]
#PC2
pc_2 <- lcpca$x[,2]
#voltinism
vol <- vol_cc3_b1
# Centroid for univoltine  
ctd_1 <- c(mean(pc_1[vol == 1]), mean(pc_2[vol == 1]))  
# Centroid for multivoltine  
ctd_2 <- c(mean(pc_1[vol == 2]), mean(pc_2[vol == 2]))  
# Euclidean distance  
ecdi_lc <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_lc <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_vol <- sample(vol)
  # Centroid for univoltine  
  ctd_1 <- c(mean(pc_1[random_vol == 1]), mean(pc_2[random_vol == 1]))  
  # Centroid for multivoltine  
  ctd_2 <- c(mean(pc_1[random_vol == 2]), mean(pc_2[random_vol == 2]))  
  # Euclidean distance  
  sim_ecdi_lc[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_lc <- mean(sim_ecdi_lc >= ecdi_lc)


#Sierra Valley
#PC1
pc_1 <- svpca$x[,1]
#PC2
pc_2 <- svpca$x[,2]
#voltinism
vol <- vol_cc2_b1
# Centroid for univoltine  
ctd_1 <- c(mean(pc_1[vol == 1]), mean(pc_2[vol == 1]))  
# Centroid for multivoltine  
ctd_2 <- c(mean(pc_1[vol == 2]), mean(pc_2[vol == 2]))  
# Euclidean distance  
ecdi_sv <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_sv <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_vol <- sample(vol)
  # Centroid for univoltine  
  ctd_1 <- c(mean(pc_1[random_vol == 1]), mean(pc_2[random_vol == 1]))  
  # Centroid for multivoltine  
  ctd_2 <- c(mean(pc_1[random_vol == 2]), mean(pc_2[random_vol == 2]))  
  # Euclidean distance  
  sim_ecdi_sv[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_sv <- mean(sim_ecdi_sv >= ecdi_sv)


#Washington
#PC1
pc_1 <- wapca$x[,1]
#PC2
pc_2 <- wapca$x[,2]
#voltinism
vol <- vol_cc1_b1
# Centroid for univoltine  
ctd_1 <- c(mean(pc_1[vol == 1]), mean(pc_2[vol == 1]))  
# Centroid for multivoltine  
ctd_2 <- c(mean(pc_1[vol == 2]), mean(pc_2[vol == 2]))  
# Euclidean distance  
ecdi_wa <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_wa <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_vol <- sample(vol)
  # Centroid for univoltine  
  ctd_1 <- c(mean(pc_1[random_vol == 1]), mean(pc_2[random_vol == 1]))  
  # Centroid for multivoltine  
  ctd_2 <- c(mean(pc_1[random_vol == 2]), mean(pc_2[random_vol == 2]))  
  # Euclidean distance  
  sim_ecdi_wa[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_wa <- mean(sim_ecdi_wa >= ecdi_wa)

pdf(paste("VOLTINISM_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_EUCLIDEAN_DISTANCE_PLOT.pdf",sep=""),width=20,height=4)
layout(matrix(1:5, ncol=5))
par(oma=c(1,1,0,0.3), mar=c(8,7.5,5,1.5), mgp = c(5, 2, 0))

# Density plots
plot(density(sim_ecdi_cp, bw = 0.1),
     main = "(a) CP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_cp, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_cp > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_cp), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_dp, bw = 0.1), 
     main = "(b) DP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_dp, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_dp > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_dp), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_lc, bw = 0.1), 
     main = "(c) LC",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_lc, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_lc > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_lc), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_sv, bw = 0.1),
     main = "(d) SV",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_sv, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_sv > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_sv), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_wa, bw = 0.1), 
     main = "(e) WA",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_wa, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_wa > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_wa), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
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



#Diet breadth
#Castle Peak
#PC1
pc_1 <- cppca$x[,1]
#PC2
pc_2 <- cppca$x[,2]
#diet breadth
diet <- diet_cc5_b1
# Centroid for monophagous  
ctd_1 <- c(mean(pc_1[diet == 1]), mean(pc_2[diet == 1]))  
# Centroid for polyphagous  
ctd_2 <- c(mean(pc_1[diet == 2]), mean(pc_2[diet == 2]))  
# Euclidean distance  
ecdi_cp <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_cp <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes  
  random_diet <- sample(diet)
  # Centroid for monophagous  
  ctd_1 <- c(mean(pc_1[random_diet == 1]), mean(pc_2[random_diet == 1]))  
  # Centroid for polyphagous  
  ctd_2 <- c(mean(pc_1[random_diet == 2]), mean(pc_2[random_diet == 2]))  
  # Euclidean distance  
  sim_ecdi_cp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_cp <- mean(sim_ecdi_cp >= ecdi_cp)



#Donner Pass
#PC1
pc_1 <- dppca$x[,1]
#PC2
pc_2 <- dppca$x[,2]
#diet breadth
diet <- diet_cc4_b1
# Centroid for monophagous  
ctd_1 <- c(mean(pc_1[diet == 1]), mean(pc_2[diet == 1]))  
# Centroid for polyphagous  
ctd_2 <- c(mean(pc_1[diet == 2]), mean(pc_2[diet == 2]))  
# Euclidean distance  
ecdi_dp <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_dp <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_diet <- sample(diet)
  # Centroid for monophagous  
  ctd_1 <- c(mean(pc_1[random_diet == 1]), mean(pc_2[random_diet == 1]))  
  # Centroid for polyphagous  
  ctd_2 <- c(mean(pc_1[random_diet == 2]), mean(pc_2[random_diet == 2]))  
  # Euclidean distance  
  sim_ecdi_dp[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_dp <- mean(sim_ecdi_dp >= ecdi_dp)


#Lang Crossing
#PC1
pc_1 <- lcpca$x[,1]
#PC2
pc_2 <- lcpca$x[,2]
#diet breadth
diet <- diet_cc3_b1
# Centroid for monophagous  
ctd_1 <- c(mean(pc_1[diet == 1]), mean(pc_2[diet == 1]))  
# Centroid for polyphagous  
ctd_2 <- c(mean(pc_1[diet == 2]), mean(pc_2[diet == 2]))  
# Euclidean distance  
ecdi_lc <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_lc <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_diet <- sample(diet)
  # Centroid for monophagous  
  ctd_1 <- c(mean(pc_1[random_diet == 1]), mean(pc_2[random_diet == 1]))  
  # Centroid for polyphagous  
  ctd_2 <- c(mean(pc_1[random_diet == 2]), mean(pc_2[random_diet == 2]))  
  # Euclidean distance  
  sim_ecdi_lc[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_lc <- mean(sim_ecdi_lc >= ecdi_lc)



#Sierra Valley
#PC1
pc_1 <- svpca$x[,1]
#PC2
pc_2 <- svpca$x[,2]
#diet breadth
diet <- diet_cc2_b1
# Centroid for monophagous  
ctd_1 <- c(mean(pc_1[diet == 1]), mean(pc_2[diet == 1]))  
# Centroid for polyphagous  
ctd_2 <- c(mean(pc_1[diet == 2]), mean(pc_2[diet == 2]))  
# Euclidean distance  
ecdi_sv <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_sv <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_diet <- sample(diet)
  # Centroid for monophagous  
  ctd_1 <- c(mean(pc_1[random_diet == 1]), mean(pc_2[random_diet == 1]))  
  # Centroid for polyphagous  
  ctd_2 <- c(mean(pc_1[random_diet == 2]), mean(pc_2[random_diet == 2]))  
  # Euclidean distance  
  sim_ecdi_sv[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_sv <- mean(sim_ecdi_sv >= ecdi_sv)



#Washington
#PC1
pc_1 <- wapca$x[,1]
#PC2
pc_2 <- wapca$x[,2]
#diet breadth
diet <- diet_cc1_b1
# Centroid for monophagous  
ctd_1 <- c(mean(pc_1[diet == 1]), mean(pc_2[diet == 1]))  
# Centroid for polyphagous  
ctd_2 <- c(mean(pc_1[diet == 2]), mean(pc_2[diet == 2]))  
# Euclidean distance  
ecdi_wa <- sqrt(sum((ctd_1 - ctd_2)^2))  

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_wa <- numeric(n_iter)  
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_diet <- sample(diet)
  # Centroid for monophagous  
  ctd_1 <- c(mean(pc_1[random_diet == 1]), mean(pc_2[random_diet == 1]))  
  # Centroid for polyphagous  
  ctd_2 <- c(mean(pc_1[random_diet == 2]), mean(pc_2[random_diet == 2]))  
  # Euclidean distance  
  sim_ecdi_wa[i] <- sqrt(sum((ctd_1 - ctd_2)^2))  
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_wa <- mean(sim_ecdi_wa >= ecdi_wa)

pdf(paste("DIETBREADTH_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_EUCLIDEAN_DISTANCE_PLOT.pdf",sep=""),width=20,height=4)
layout(matrix(1:5, ncol=5))
par(oma=c(1,1,0,0.3), mar=c(8,7.5,5,1.5), mgp = c(5, 2, 0))

# Density plots
plot(density(sim_ecdi_cp, bw = 0.1),
     main = "(a) CP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_cp, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_cp > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_cp), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_dp, bw = 0.1), 
     main = "(b) DP",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_dp, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_dp > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_dp), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_lc, bw = 0.1), 
     main = "(c) LC",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_lc, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_lc > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_lc), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_sv, bw = 0.1),
     main = "(d) SV",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_sv, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_sv > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_sv), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_wa, bw = 0.1), 
     main = "(e) WA",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-2,2), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_wa, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_wa > 0) {
  text(x = -1, y = 0.9, labels = paste("p =", pv_wa), cex = 2.75)
} else {
  text(x = -1, y = 0.9, labels = "p < 0.0001", cex = 2.75)
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





#Overwintering stage
#Castle Peak
#PC1
pc_1 <- cppca$x[,1]
#PC2
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
# Euclidean distance  
ecdi_cp_1_2  <- euclidean_distance(ctd_1, ctd_2)
ecdi_cp_1_3  <- euclidean_distance(ctd_1, ctd_3)
ecdi_cp_1_4  <- euclidean_distance(ctd_1, ctd_4)
ecdi_cp_2_3  <- euclidean_distance(ctd_2, ctd_3)
ecdi_cp_2_4  <- euclidean_distance(ctd_2, ctd_4)
ecdi_cp_3_4  <- euclidean_distance(ctd_3, ctd_4)

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_cp_1_2  <- numeric(n_iter)
sim_ecdi_cp_1_3  <- numeric(n_iter)
sim_ecdi_cp_1_4  <- numeric(n_iter)
sim_ecdi_cp_2_3  <- numeric(n_iter)
sim_ecdi_cp_2_4  <- numeric(n_iter)
sim_ecdi_cp_3_4  <- numeric(n_iter)

for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_ove <- sample(ove)
  # Centroid for egg  
  ctd_1 <- c(mean(pc_1[random_ove == 1]), mean(pc_2[random_ove == 1]))  
  # Centroid for larva  
  ctd_2 <- c(mean(pc_1[random_ove == 2]), mean(pc_2[random_ove == 2])) 
  # Centroid for pupa
  ctd_3 <- c(mean(pc_1[random_ove == 3]), mean(pc_2[random_ove == 3]))  
  # Centroid for adult
  ctd_4 <- c(mean(pc_1[random_ove == 4]), mean(pc_2[random_ove == 4]))  
  # Euclidean distance  
  sim_ecdi_cp_1_2[i]  <- euclidean_distance(ctd_1, ctd_2)
  sim_ecdi_cp_1_3[i]  <- euclidean_distance(ctd_1, ctd_3)
  sim_ecdi_cp_1_4[i]  <- euclidean_distance(ctd_1, ctd_4)
  sim_ecdi_cp_2_3[i]  <- euclidean_distance(ctd_2, ctd_3)
  sim_ecdi_cp_2_4[i]  <- euclidean_distance(ctd_2, ctd_4)
  sim_ecdi_cp_3_4[i]  <- euclidean_distance(ctd_3, ctd_4)
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_cp_1_2 <- mean(sim_ecdi_cp_1_2 >= ecdi_cp_1_2)
pv_cp_1_3 <- mean(sim_ecdi_cp_1_3 >= ecdi_cp_1_3)
pv_cp_1_4 <- mean(sim_ecdi_cp_1_4 >= ecdi_cp_1_4)
pv_cp_2_3 <- mean(sim_ecdi_cp_2_3 >= ecdi_cp_2_3)
pv_cp_2_4 <- mean(sim_ecdi_cp_2_4 >= ecdi_cp_2_4)
pv_cp_3_4 <- mean(sim_ecdi_cp_3_4 >= ecdi_cp_3_4)



#Donner Pass
#PC1
pc_1 <- dppca$x[,1]
#PC2
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
# Euclidean distance  
ecdi_dp_1_2  <- euclidean_distance(ctd_1, ctd_2)
ecdi_dp_1_3  <- euclidean_distance(ctd_1, ctd_3)
ecdi_dp_1_4  <- euclidean_distance(ctd_1, ctd_4)
ecdi_dp_2_3  <- euclidean_distance(ctd_2, ctd_3)
ecdi_dp_2_4  <- euclidean_distance(ctd_2, ctd_4)
ecdi_dp_3_4  <- euclidean_distance(ctd_3, ctd_4)

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_dp_1_2  <- numeric(n_iter)
sim_ecdi_dp_1_3  <- numeric(n_iter)
sim_ecdi_dp_1_4  <- numeric(n_iter)
sim_ecdi_dp_2_3  <- numeric(n_iter)
sim_ecdi_dp_2_4  <- numeric(n_iter)
sim_ecdi_dp_3_4  <- numeric(n_iter)
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_ove <- sample(ove)
  # Centroid for egg  
  ctd_1 <- c(mean(pc_1[random_ove == 1]), mean(pc_2[random_ove == 1]))  
  # Centroid for larva  
  ctd_2 <- c(mean(pc_1[random_ove == 2]), mean(pc_2[random_ove == 2])) 
  # Centroid for pupa
  ctd_3 <- c(mean(pc_1[random_ove == 3]), mean(pc_2[random_ove == 3]))  
  # Centroid for adult
  ctd_4 <- c(mean(pc_1[random_ove == 4]), mean(pc_2[random_ove == 4]))  
  # Euclidean distance  
  sim_ecdi_dp_1_2[i]  <- euclidean_distance(ctd_1, ctd_2)
  sim_ecdi_dp_1_3[i]  <- euclidean_distance(ctd_1, ctd_3)
  sim_ecdi_dp_1_4[i]  <- euclidean_distance(ctd_1, ctd_4)
  sim_ecdi_dp_2_3[i]  <- euclidean_distance(ctd_2, ctd_3)
  sim_ecdi_dp_2_4[i]  <- euclidean_distance(ctd_2, ctd_4)
  sim_ecdi_dp_3_4[i]  <- euclidean_distance(ctd_3, ctd_4)
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_dp_1_2 <- mean(sim_ecdi_dp_1_2 >= ecdi_dp_1_2)
pv_dp_1_3 <- mean(sim_ecdi_dp_1_3 >= ecdi_dp_1_3)
pv_dp_1_4 <- mean(sim_ecdi_dp_1_4 >= ecdi_dp_1_4)
pv_dp_2_3 <- mean(sim_ecdi_dp_2_3 >= ecdi_dp_2_3)
pv_dp_2_4 <- mean(sim_ecdi_dp_2_4 >= ecdi_dp_2_4)
pv_dp_3_4 <- mean(sim_ecdi_dp_3_4 >= ecdi_dp_3_4)


#Lang Crossing
#PC1
pc_1 <- lcpca$x[,1]
#PC2
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
# Euclidean distance  
ecdi_lc_1_2  <- euclidean_distance(ctd_1, ctd_2)
ecdi_lc_1_3  <- euclidean_distance(ctd_1, ctd_3)
ecdi_lc_1_4  <- euclidean_distance(ctd_1, ctd_4)
ecdi_lc_2_3  <- euclidean_distance(ctd_2, ctd_3)
ecdi_lc_2_4  <- euclidean_distance(ctd_2, ctd_4)
ecdi_lc_3_4  <- euclidean_distance(ctd_3, ctd_4)

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_lc_1_2  <- numeric(n_iter)
sim_ecdi_lc_1_3  <- numeric(n_iter)
sim_ecdi_lc_1_4  <- numeric(n_iter)
sim_ecdi_lc_2_3  <- numeric(n_iter)
sim_ecdi_lc_2_4  <- numeric(n_iter)
sim_ecdi_lc_3_4  <- numeric(n_iter)
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_ove <- sample(ove)
  # Centroid for egg  
  ctd_1 <- c(mean(pc_1[random_ove == 1]), mean(pc_2[random_ove == 1]))  
  # Centroid for larva  
  ctd_2 <- c(mean(pc_1[random_ove == 2]), mean(pc_2[random_ove == 2])) 
  # Centroid for pupa
  ctd_3 <- c(mean(pc_1[random_ove == 3]), mean(pc_2[random_ove == 3]))  
  # Centroid for adult
  ctd_4 <- c(mean(pc_1[random_ove == 4]), mean(pc_2[random_ove == 4]))  
  # Euclidean distance  
  sim_ecdi_lc_1_2[i]  <- euclidean_distance(ctd_1, ctd_2)
  sim_ecdi_lc_1_3[i]  <- euclidean_distance(ctd_1, ctd_3)
  sim_ecdi_lc_1_4[i]  <- euclidean_distance(ctd_1, ctd_4)
  sim_ecdi_lc_2_3[i]  <- euclidean_distance(ctd_2, ctd_3)
  sim_ecdi_lc_2_4[i]  <- euclidean_distance(ctd_2, ctd_4)
  sim_ecdi_lc_3_4[i]  <- euclidean_distance(ctd_3, ctd_4)
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_lc_1_2 <- mean(sim_ecdi_lc_1_2 >= ecdi_lc_1_2)
pv_lc_1_3 <- mean(sim_ecdi_lc_1_3 >= ecdi_lc_1_3)
pv_lc_1_4 <- mean(sim_ecdi_lc_1_4 >= ecdi_lc_1_4)
pv_lc_2_3 <- mean(sim_ecdi_lc_2_3 >= ecdi_lc_2_3)
pv_lc_2_4 <- mean(sim_ecdi_lc_2_4 >= ecdi_lc_2_4)
pv_lc_3_4 <- mean(sim_ecdi_lc_3_4 >= ecdi_lc_3_4)


#Sierra Valley
#PC1
pc_1 <- svpca$x[,1]
#PC2
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
# Euclidean distance  
ecdi_sv_1_2  <- euclidean_distance(ctd_1, ctd_2)
ecdi_sv_1_3  <- euclidean_distance(ctd_1, ctd_3)
ecdi_sv_1_4  <- euclidean_distance(ctd_1, ctd_4)
ecdi_sv_2_3  <- euclidean_distance(ctd_2, ctd_3)
ecdi_sv_2_4  <- euclidean_distance(ctd_2, ctd_4)
ecdi_sv_3_4  <- euclidean_distance(ctd_3, ctd_4)

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_sv_1_2  <- numeric(n_iter)
sim_ecdi_sv_1_3  <- numeric(n_iter)
sim_ecdi_sv_1_4  <- numeric(n_iter)
sim_ecdi_sv_2_3  <- numeric(n_iter)
sim_ecdi_sv_2_4  <- numeric(n_iter)
sim_ecdi_sv_3_4  <- numeric(n_iter)
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_ove <- sample(ove)
  # Centroid for egg  
  ctd_1 <- c(mean(pc_1[random_ove == 1]), mean(pc_2[random_ove == 1]))  
  # Centroid for larva  
  ctd_2 <- c(mean(pc_1[random_ove == 2]), mean(pc_2[random_ove == 2])) 
  # Centroid for pupa
  ctd_3 <- c(mean(pc_1[random_ove == 3]), mean(pc_2[random_ove == 3]))  
  # Centroid for adult
  ctd_4 <- c(mean(pc_1[random_ove == 4]), mean(pc_2[random_ove == 4]))  
  # Euclidean distance  
  sim_ecdi_sv_1_2[i]  <- euclidean_distance(ctd_1, ctd_2)
  sim_ecdi_sv_1_3[i]  <- euclidean_distance(ctd_1, ctd_3)
  sim_ecdi_sv_1_4[i]  <- euclidean_distance(ctd_1, ctd_4)
  sim_ecdi_sv_2_3[i]  <- euclidean_distance(ctd_2, ctd_3)
  sim_ecdi_sv_2_4[i]  <- euclidean_distance(ctd_2, ctd_4)
  sim_ecdi_sv_3_4[i]  <- euclidean_distance(ctd_3, ctd_4)
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_sv_1_2 <- mean(sim_ecdi_sv_1_2 >= ecdi_sv_1_2)
pv_sv_1_3 <- mean(sim_ecdi_sv_1_3 >= ecdi_sv_1_3)
pv_sv_1_4 <- mean(sim_ecdi_sv_1_4 >= ecdi_sv_1_4)
pv_sv_2_3 <- mean(sim_ecdi_sv_2_3 >= ecdi_sv_2_3)
pv_sv_2_4 <- mean(sim_ecdi_sv_2_4 >= ecdi_sv_2_4)
pv_sv_3_4 <- mean(sim_ecdi_sv_3_4 >= ecdi_sv_3_4)


#Washington
#PC1
pc_1 <- wapca$x[,1]
#PC2
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
# Euclidean distance  
ecdi_wa_1_2  <- euclidean_distance(ctd_1, ctd_2)
ecdi_wa_1_3  <- euclidean_distance(ctd_1, ctd_3)
ecdi_wa_1_4  <- euclidean_distance(ctd_1, ctd_4)
ecdi_wa_2_3  <- euclidean_distance(ctd_2, ctd_3)
ecdi_wa_2_4  <- euclidean_distance(ctd_2, ctd_4)
ecdi_wa_3_4  <- euclidean_distance(ctd_3, ctd_4)

# Generate null distribution of Euclidean distances by permutation
set.seed(100) 
n_iter <- 10000  # Number of random permutations
sim_ecdi_wa_1_2  <- numeric(n_iter)
sim_ecdi_wa_1_3  <- numeric(n_iter)
sim_ecdi_wa_1_4  <- numeric(n_iter)
sim_ecdi_wa_2_3  <- numeric(n_iter)
sim_ecdi_wa_2_4  <- numeric(n_iter)
sim_ecdi_wa_3_4  <- numeric(n_iter)
for (i in 1:n_iter) {
  # Randomly permute residency labels while preserving group sizes
  random_ove <- sample(ove)
  # Centroid for egg  
  ctd_1 <- c(mean(pc_1[random_ove == 1]), mean(pc_2[random_ove == 1]))  
  # Centroid for larva  
  ctd_2 <- c(mean(pc_1[random_ove == 2]), mean(pc_2[random_ove == 2])) 
  # Centroid for pupa
  ctd_3 <- c(mean(pc_1[random_ove == 3]), mean(pc_2[random_ove == 3]))  
  # Centroid for adult
  ctd_4 <- c(mean(pc_1[random_ove == 4]), mean(pc_2[random_ove == 4]))  
  # Euclidean distance  
  sim_ecdi_wa_1_2[i]  <- euclidean_distance(ctd_1, ctd_2)
  sim_ecdi_wa_1_3[i]  <- euclidean_distance(ctd_1, ctd_3)
  sim_ecdi_wa_1_4[i]  <- euclidean_distance(ctd_1, ctd_4)
  sim_ecdi_wa_2_3[i]  <- euclidean_distance(ctd_2, ctd_3)
  sim_ecdi_wa_2_4[i]  <- euclidean_distance(ctd_2, ctd_4)
  sim_ecdi_wa_3_4[i]  <- euclidean_distance(ctd_3, ctd_4)
}
# permutation p-value
# The p-value is the proportion of simulated distances greater than or equal
# to the observed Euclidean distance.
pv_wa_1_2 <- mean(sim_ecdi_wa_1_2 >= ecdi_wa_1_2)
pv_wa_1_3 <- mean(sim_ecdi_wa_1_3 >= ecdi_wa_1_3)
pv_wa_1_4 <- mean(sim_ecdi_wa_1_4 >= ecdi_wa_1_4)
pv_wa_2_3 <- mean(sim_ecdi_wa_2_3 >= ecdi_wa_2_3)
pv_wa_2_4 <- mean(sim_ecdi_wa_2_4 >= ecdi_wa_2_4)
pv_wa_3_4 <- mean(sim_ecdi_wa_3_4 >= ecdi_wa_3_4)


pdf(paste("OVERWINTERING_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_EUCLIDEAN_DISTANCE_PLOT.pdf",sep=""),width=20,height=24)
layout(matrix(1:30, ncol=5))
par(oma=c(1,1,0,0.2), mar=c(8,7.1,5,1.7), mgp = c(5, 2, 0))

# Density plots
plot(density(sim_ecdi_cp_1_2, bw = 0.1),
     main = "a(i) CP: egg-larva",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_cp_1_2, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_cp_1_2 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_cp_1_2), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_cp_1_3, bw = 0.1),
     main = "a(ii) CP: egg-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_cp_1_3, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_cp_1_3 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_cp_1_3), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_cp_1_4, bw = 0.1),
     main = "a(iii) CP: egg-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_cp_1_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_cp_1_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_cp_1_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_cp_2_3, bw = 0.1),
     main = "a(iv) CP: larva-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_cp_2_3, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_cp_2_3 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_cp_2_3), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_cp_2_4, bw = 0.1),
     main = "a(v) CP: larva-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_cp_2_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_cp_2_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_cp_2_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_cp_3_4, bw = 0.1),
     main = "a(vi) CP: pupa-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_cp_3_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_cp_3_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_cp_3_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 





# Density plots
plot(density(sim_ecdi_dp_1_2, bw = 0.1),
     main = "b(i) DP: egg-larva",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_dp_1_2, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_dp_1_2 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_dp_1_2), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_dp_1_3, bw = 0.1),
     main = "b(ii) DP: egg-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_dp_1_3, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_dp_1_3 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_dp_1_3), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_dp_1_4, bw = 0.1),
     main = "b(iii) DP: egg-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_dp_1_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_dp_1_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_dp_1_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_dp_2_3, bw = 0.1),
     main = "b(iv) DP: larva-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_dp_2_3, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_dp_2_3 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_dp_2_3), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_dp_2_4, bw = 0.1),
     main = "b(v) DP: larva-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_dp_2_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_dp_2_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_dp_2_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_dp_3_4, bw = 0.1),
     main = "b(vi) DP: pupa-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_dp_3_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_dp_3_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_dp_3_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 




# Density plots
plot(density(sim_ecdi_lc_1_2, bw = 0.1),
     main = "c(i) LC: egg-larva",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_lc_1_2, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_lc_1_2 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_lc_1_2), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_lc_1_3, bw = 0.1),
     main = "c(ii) LC: egg-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_lc_1_3, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_lc_1_3 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_lc_1_3), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_lc_1_4, bw = 0.1),
     main = "c(iii) LC: egg-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_lc_1_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_lc_1_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_lc_1_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_lc_2_3, bw = 0.1),
     main = "c(iv) LC: larva-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_lc_2_3, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_lc_2_3 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_lc_2_3), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_lc_2_4, bw = 0.1),
     main = "c(v) LC: larva-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_lc_2_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_lc_2_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_lc_2_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_lc_3_4, bw = 0.1),
     main = "c(vi) LC: pupa-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_lc_3_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_lc_3_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_lc_3_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 




# Density plots
plot(density(sim_ecdi_sv_1_2, bw = 0.1),
     main = "d(i) SV: egg-larva",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_sv_1_2, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_sv_1_2 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_sv_1_2), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_sv_1_3, bw = 0.1),
     main = "d(ii) SV: egg-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_sv_1_3, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_sv_1_3 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_sv_1_3), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_cp_1_4, bw = 0.1),
     main = "d(iii) SV: egg-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_sv_1_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_sv_1_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_sv_1_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_sv_2_3, bw = 0.1),
     main = "d(iv) SV: larva-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_sv_2_3, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_sv_2_3 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_sv_2_3), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_sv_2_4, bw = 0.1),
     main = "d(v) SV: larva-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_sv_2_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_sv_2_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_sv_2_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_sv_3_4, bw = 0.1),
     main = "d(vi) SV: pupa-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_sv_3_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_sv_3_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_sv_3_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 




# Density plots
plot(density(sim_ecdi_wa_1_2, bw = 0.1),
     main = "e(i) WA: egg-larva",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_wa_1_2, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_wa_1_2 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_wa_1_2), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2)  

# Density plots
plot(density(sim_ecdi_wa_1_3, bw = 0.1),
     main = "e(ii) WA: egg-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_wa_1_3, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_wa_1_3 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_wa_1_3), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_wa_1_4, bw = 0.1),
     main = "e(iii) WA: egg-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_wa_1_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_wa_1_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_wa_1_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_wa_2_3, bw = 0.1),
     main = "e(iv) WA: larva-pupa",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_wa_2_3, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_wa_2_3 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_wa_2_3), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_wa_2_4, bw = 0.1),
     main = "e(v) WA: larva-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_wa_2_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_wa_2_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_wa_2_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
}
box(lwd = 2) 

# Density plots
plot(density(sim_ecdi_wa_3_4, bw = 0.1),
     main = "e(vi) WA: pupa-adult",
     col = "black",  
     xlab = "Euclidean Distance",
     ylab = "Frequency", lwd=3, xlim=c(-3.5,3.5), cex.lab= 3, cex.axis= 3, cex.main=3)

# Observed Euclidean distance as a red dashed line
abline(v=ecdi_wa_3_4, col = "red", lwd = 2, lty = 2)  
# Annotate p-value on plot
if (pv_wa_3_4 > 0) {
  text(x = -1.7, y = 0.45, labels = paste("p =", pv_wa_3_4), cex = 2.75)
} else {
  text(x = -1.7, y = 0.45, labels = "p < 0.0001", cex = 2.75)
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

