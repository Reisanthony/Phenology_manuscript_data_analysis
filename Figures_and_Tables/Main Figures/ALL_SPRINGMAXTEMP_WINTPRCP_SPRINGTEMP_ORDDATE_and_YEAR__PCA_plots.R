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


#WASHINGTON
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



#Species found at all sites
# full list of all species across sites
allsp <- c(cssp,dssp,lssp,sssp,wssp)
unsp <- unique(allsp)
nosp <- length(unsp)


#castle peak 
# Indices of species found in Castle Peak within the full list of species
matching_indices_1 <- match(cssp, unsp)

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


#donner pass 
# Indices of species found in Donner Pass within the full list of species
matching_indices_2 <- match(dssp, unsp)
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


#langcrossing
# Indices of species found in Lang Crossing within the full list of species
matching_indices_3 <- match(lssp, unsp)
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


#sierra valley 
# Indices of species found in Sierra Valley within the full list of species
matching_indices_4 <- match(sssp, unsp)
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


#washington
# Indices of species found in Washington within the full list of species
matching_indices_5 <- match(wssp, unsp)
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





#Assign species-specific effect of climate to the
#corresponding indices of species present at castle  peak
cp1[matching_indices_1] <- cb1 
cp7[matching_indices_1] <- cb7
cp8[matching_indices_1] <- cb8

cp2[matching_indices_1] <- cb2 
cp9[matching_indices_1] <- cb9 
cp10[matching_indices_1] <- cb10  

cp3[matching_indices_1] <- cb3
cp11[matching_indices_1] <- cb11
cp12[matching_indices_1] <- cb12

#Assign species-specific effect of climate to the
#corresponding indices of species present at donner pass
dp1[matching_indices_2] <- db1 
dp7[matching_indices_2] <- db7 
dp8[matching_indices_2] <- db8 

dp2[matching_indices_2] <- db2 
dp9[matching_indices_2] <- db9 
dp10[matching_indices_2] <- db10

dp3[matching_indices_2] <- db3
dp11[matching_indices_2] <- db11
dp12[matching_indices_2] <- db12 


#Assign species-specific effect of climate to the
#corresponding indices of species present at lang crossing
lc1[matching_indices_3] <- lb1 
lc7[matching_indices_3] <- lb7 
lc8[matching_indices_3] <- lb8 

lc2[matching_indices_3] <- lb2 
lc9[matching_indices_3] <- lb9 
lc10[matching_indices_3] <- lb10  

lc3[matching_indices_3] <- lb3 
lc11[matching_indices_3] <- lb11 
lc12[matching_indices_3] <- lb12 


#Assign species-specific effect of climate to the
#corresponding indices of species present at sierra valley
sv1[matching_indices_4] <- sb1 
sv7[matching_indices_4] <- sb7 
sv8[matching_indices_4] <- sb8 

sv2[matching_indices_4] <- sb2 
sv9[matching_indices_4] <- sb9 
sv10[matching_indices_4] <- sb10 

sv3[matching_indices_4] <- sb3
sv11[matching_indices_4] <- sb11
sv12[matching_indices_4] <- sb12 

#Assign species-specific effect of climate to the
#corresponding indices of species present at washington 
wa1[matching_indices_5] <- wb1
wa7[matching_indices_5] <- wb7
wa8[matching_indices_5] <- wb8

wa2[matching_indices_5] <- wb2 
wa9[matching_indices_5] <- wb9
wa10[matching_indices_5] <- wb10 

wa3[matching_indices_5] <- wb3
wa11[matching_indices_5] <- wb11
wa12[matching_indices_5] <- wb12 


# Each site vector above has a length equal to the total number of species across all sites. 
# Each site vector contain species-specific effects only for species found at that site, 
# with NA values for species not found at that site.

# Combine the effects of climate on each aspect of the flight period across all sites
allbeta <- cbind(cp1, dp1, lc1, sv1, wa1,
                 cp7, dp7, lc7, sv7, wa7,
                 cp8, dp8, lc8, sv8, wa8,
                 cp2, dp2, lc2, sv2, wa2,
                 cp9, dp9, lc9, sv9, wa9,
                 cp10, dp10, lc10, sv10, wa10,
                 cp3, dp3, lc3, sv3, wa3,
                 cp11, dp11, lc11, sv11, wa11,
                 cp12, dp12, lc12, sv12, wa12)

allbeta <- as.data.frame(allbeta)
rownames(allbeta) <- unsp
# species present at all sites (species-specific effect)
sp_allst_beta <- allbeta[complete.cases(allbeta), ]
row.names(sp_allst_beta)

# PCA 
sp_allst_pca <- prcomp(sp_allst_beta, scale=TRUE)
# Summary
sum_sp_allst <- summary(sp_allst_pca)


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



pdf(paste("VOLTINISM_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_PLOT.pdf",sep=""),width=10,height=15)

layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))

par(oma=c(1,0.3,0,0.3), mar=c(6,5,3,2))

cols <- c("grey", "blue", "green")                

arrow_col <- c(rep("red", 3),  # red for spring max temp
               rep("#87CEEB", 3), # blue for winter precipitation
               rep("black", 3)) # black for spring min temp

arrow_lab <- rep(c("MO", "TO", "LO"), 3)

#biplot
plot(cppca$x[,1], cppca$x[,2], xlab=paste("PCA 1 (", round(sumcp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumcp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[vol_cc5_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")

# Arrows (multiply by 4.5 to make arrows longer))
cp_l_x <- cppca$rotation[,1]*4.5
cp_l_y <- cppca$rotation[,2]*4.5
arrows(x0=0, x1=cp_l_x, y0=0, y1=cp_l_y, col=arrow_col, length=0.15, lwd=1.5)

#Label  arrows
# Position for each label
# y axis coordinates
cp_l_pos <- cp_l_y 
# variables on the bottom half of the plot
cp_lo <- which(cp_l_y < 0) 
# variables on the top half
cp_hi <- which(cp_l_y > 0) 
# Replace values 
cp_l_pos <- replace(cp_l_pos, cp_lo, "1")
cp_l_pos <- replace(cp_l_pos, cp_hi, "3")
text(cp_l_x, cp_l_y, labels=arrow_lab, col=arrow_col, pos=cp_l_pos, cex=1.5)

mtext("(a) CP", side = 3, line = 0.3, adj = 0, cex = 1.5)
legend("topleft", legend = c("Univoltine",  "Multivoltine"), col = alpha(c("blue", "green")), pch = 19, cex = 2.0, bty= "n")



#biplot
plot(dppca$x[,1], dppca$x[,2], xlab=paste("PCA 1 (", round(sumdp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumdp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[vol_cc4_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")

# Arrows (multiply by 5 to make arrows longer)
dp_l_x <- dppca$rotation[,1]*5
dp_l_y <- dppca$rotation[,2]*5
arrows(x0=0, x1=dp_l_x, y0=0, y1=dp_l_y, col=arrow_col, length=0.15, lwd=1.5)

#Label  arrows
# Position for each label
 # y axis coordinates
dp_l_pos <- dp_l_y
# variables on the bottom half of the plot
dp_lo <- which(dp_l_y < 0) 
# variables on the top half
dp_hi <- which(dp_l_y > 0)
# Replace values 
dp_l_pos <- replace(dp_l_pos, dp_lo, "1")
dp_l_pos <- replace(dp_l_pos, dp_hi, "3")
text(dp_l_x, dp_l_y, labels=arrow_lab, col=arrow_col, pos=dp_l_pos, cex=1.5)

mtext("(b) DP", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
plot(lcpca$x[,1], lcpca$x[,2], xlab=paste("PCA 1 (", round(sumlc$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumlc$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[vol_cc3_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")

# Arrows (multiply by 5 to make arrows longer)
lc_l_x <- lcpca$rotation[,1]*5
lc_l_y <- lcpca$rotation[,2]*5
arrows(x0=0, x1=lc_l_x, y0=0, y1=lc_l_y, col=arrow_col, length=0.15, lwd=1.5)

#Label  arrows
# Position for each label
# y axis coordinates
lc_l_pos <- lc_l_y 
#  variables on the bottom half of the plot
lc_lo <- which(lc_l_y < 0) 
# variables on the top half
lc_hi <- which(lc_l_y > 0) 
# Replace values 
lc_l_pos <- replace(lc_l_pos, lc_lo, "1")
lc_l_pos <- replace(lc_l_pos, lc_hi, "3")
text(lc_l_x, lc_l_y, labels=arrow_lab, col=arrow_col, pos=lc_l_pos, cex=1.5)

mtext("(c) LC", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
plot(svpca$x[,1], svpca$x[,2], xlab=paste("PCA 1 (", round(sumsv$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumsv$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[vol_cc2_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")

# Arrows (multiply by 4 to make arrows longer)
sv_l_x <- svpca$rotation[,1]*4
sv_l_y <- svpca$rotation[,2]*4
arrows(x0=0, x1=sv_l_x, y0=0, y1=sv_l_y, col=arrow_col, length=0.15, lwd=1.5)

#Label  arrows
# Position for each label
# y axis coordinates
sv_l_pos <- sv_l_y 
#  variables on the bottom half of the plot
sv_lo <- which(sv_l_y < 0) 
# variables on the top half
sv_hi <- which(sv_l_y > 0) 
# Replace values 
sv_l_pos <- replace(sv_l_pos, sv_lo, "1")
sv_l_pos <- replace(sv_l_pos, sv_hi, "3")
text(sv_l_x, sv_l_y, labels=arrow_lab, col=arrow_col, pos=sv_l_pos, cex=1.5)

mtext("(d) SV", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
plot(wapca$x[,1], wapca$x[,2], xlab=paste("PCA 1 (", round(sumwa$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumwa$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col=cols[vol_cc1_b1 + 1], cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")

# Arrows (multiply by 5 to make arrows longer)
wa_l_x <- wapca$rotation[,1]*5
wa_l_y <- wapca$rotation[,2]*5
arrows(x0=0, x1=wa_l_x, y0=0, y1=wa_l_y, col=arrow_col, length=0.15, lwd=1.5)

#Label  arrows
# Position for each label
# y axis coordinates
wa_l_pos <- wa_l_y
#  variables on the bottom half of the plot
wa_lo <- which(wa_l_y < 0) 
# variables on the top half
wa_hi <- which(wa_l_y > 0) 
# Replace values 
wa_l_pos <- replace(wa_l_pos, wa_lo, "1")
wa_l_pos <- replace(wa_l_pos, wa_hi, "3")
text(wa_l_x, wa_l_y, labels=arrow_lab, col=arrow_col, pos=wa_l_pos, cex=1.5)

mtext("(e) WA", side = 3, line = 0.3, adj = 0, cex = 1.5)


#species present at all sites
#biplot
plot(sp_allst_pca$x[,1], sp_allst_pca$x[,2], xlab=paste("PCA 1 (", round(sum_sp_allst$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sum_sp_allst$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2, xlim= c(-6,6))

# Grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")

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

# Arrows (multiply by 12 to make arrows longer)
sp_allst_l_x <- sp_allst_pca$rotation[,1]*12
sp_allst_l_y <- sp_allst_pca$rotation[,2]*12
arrows(x0=0, x1=sp_allst_l_x, y0=0, y1=sp_allst_l_y, col=sp_allst_arrow_col, length=0.15, lwd=1.5)

#Label  arrows
# Position for each label
# y axis coordinates
sp_allst_l_pos <- sp_allst_l_y 
#  variables on the bottom half of the plot
sp_allst_lo <- which(sp_allst_l_y < 0) 
# variables on the top half
sp_allst_hi <- which(sp_allst_l_y > 0)
# Replace values 
sp_allst_l_pos <- replace(sp_allst_l_pos, sp_allst_lo, "1")
sp_allst_l_pos <- replace(sp_allst_l_pos, sp_allst_hi, "3")
text(sp_allst_l_x, sp_allst_l_y, labels=sp_allst_arrow_lab, col=sp_allst_arrow_col, pos=sp_allst_l_pos, cex=1.5)

mtext("(f) All sites", side = 3, line = 0.3, adj = 0, cex = 1.5)

dev.off()



