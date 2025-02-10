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


pdf(paste("SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_PLOT.pdf",sep=""),width=12,height=17)

layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))

par(oma=c(1,0.3,0,0.3), mar=c(6,5,3,2))

arrow_col <- c(rep("red", 3),  # red for spring max temp
               rep("#87CEEB", 3), # blue for winter precipitation
               rep("black", 3)) # black for spring min temp

arrow_lab <- rep(c("MO", "TO", "LO"), 3)

#biplot
#biplot(cppca)
plot(cppca$x[,1], cppca$x[,2], xlab=paste("PCA 1 (", round(sumcp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumcp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(cppca$x[,1], cppca$x[,2], labels=row.names(cppca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 4.5 to make arrows longer
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
text(cp_l_x, cp_l_y, labels=arrow_lab, col=arrow_col, pos=cp_l_pos, cex=1.5)

mtext("(a) CP", side = 3, line = 0.3, adj = 0, cex = 1.5)



#biplot
#biplot(dppca)
plot(dppca$x[,1], dppca$x[,2], xlab=paste("PCA 1 (", round(sumdp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumdp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(dppca$x[,1], dppca$x[,2], labels=row.names(dppca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 5 to make arrows longer
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
mtext("(b) DP", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(lcpca)
plot(lcpca$x[,1], lcpca$x[,2], xlab=paste("PCA 1 (", round(sumlc$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumlc$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(lcpca$x[,1], lcpca$x[,2], labels=row.names(lcpca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 5 to make arrows longer
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
mtext("(c) LC", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(svpca)
plot(svpca$x[,1], svpca$x[,2], xlab=paste("PCA 1 (", round(sumsv$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumsv$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

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
mtext("(d) SV", side = 3, line = 0.3, adj = 0, cex = 1.5)

#biplot
#biplot(wapca)
plot(wapca$x[,1], wapca$x[,2], xlab=paste("PCA 1 (", round(sumwa$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumwa$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
#text(wapca$x[,1], wapca$x[,2], labels=row.names(wapca$x), pos=c(1,4,2,3), font=2)

# Draw arrows, i multiply by 5 to make arrows longer
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

# Draw arrows, i multiply by 12 to make arrows longer
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

mtext("(f) All sites", side = 3, line = 0.3, adj = 0, cex = 1.5)

dev.off()























###############################################################################
#######################Coloring by natural history#############################
###############################################################################



#natural history data
#natural history data
nath<-read.csv("shapiro_transect_natural_history_data.csv")
#turn 2 to multiple
nath$broods <- gsub("two", "multiple", nath$broods)

nath_cp <- nath[nath$site_name == "Castle Peak", ]
nath_dp <- nath[nath$site_name == "Donner Pass", ]
nath_sv <- nath[nath$site_name == "Sierra Valley", ]
nath_lc <- nath[nath$site_name == "Lang Crossing", ]
nath_wa <- nath[nath$site_name == "Washington", ]
###################################################################
############################# voltinism #############################
########################################################################
#####################################voltinism(one brood)
volonenath_cp <- nath_cp[nath_cp$broods=="one", ]
volonespp_cp <- unique(volonenath_cp$genus_species)
volone_1 <- na.omit(match(volonespp_cp, cssp))

volonenath_dp <- nath_dp[nath_dp$broods=="one", ]
volonespp_dp <- unique(volonenath_dp$genus_species)
volone_2 <- na.omit(match(volonespp_dp, dssp))

volonenath_lc <- nath_lc[nath_lc$broods=="one", ]
volonespp_lc <- unique(volonenath_lc$genus_species)
volone_3 <- na.omit(match(volonespp_lc, lssp))

volonenath_sv <- nath_sv[nath_sv$broods=="one", ]
volonespp_sv <- unique(volonenath_sv$genus_species)
volone_4 <- na.omit(match(volonespp_sv, sssp))

volonenath_wa <- nath_wa[nath_wa$broods=="one", ]
volonespp_wa <- unique(volonenath_wa$genus_species)
volone_5<- na.omit(match(volonespp_wa, wssp))


####################voltinism(multi brood)

volmultinath_cp <- nath_cp[nath_cp$broods=="multiple", ]
volmultispp_cp <- unique(volmultinath_cp$genus_species)
volmulti_1 <- na.omit(match(volmultispp_cp, cssp))

volmultinath_dp <- nath_dp[nath_dp$broods=="multiple", ]
volmultispp_dp <- unique(volmultinath_dp$genus_species)
volmulti_2 <- na.omit(match(volmultispp_dp, dssp))

volmultinath_lc <- nath_lc[nath_lc$broods=="multiple", ]
volmultispp_lc <- unique(volmultinath_lc$genus_species)
volmulti_3 <- na.omit(match(volmultispp_lc, lssp))

volmultinath_sv <- nath_sv[nath_sv$broods=="multiple", ]
volmultispp_sv <- unique(volmultinath_sv$genus_species)
volmulti_4 <- na.omit(match(volmultispp_sv, sssp))

volmultinath_wa <- nath_wa[nath_wa$broods=="multiple", ]
volmultispp_wa <- unique(volmultinath_wa$genus_species)
volmulti_5<- na.omit(match(volmultispp_wa, wssp))





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





cols <- c("grey", "blue", "green", "orange", "pink")                


pdf(paste("VOLTINISM_SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_PLOT.pdf",sep=""),width=12,height=17)

layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))

par(oma=c(1,0.3,0,0.3), mar=c(6,5,3,2))

arrow_col <- c(rep("red", 3),  # red for spring max temp
               rep("#87CEEB", 3), # blue for winter precipitation
               rep("black", 3)) # black for spring min temp

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

# Draw arrows, i multiply by 4.5 to make arrows longer
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

mtext("(a) CP", side = 3, line = 0.3, adj = 0, cex = 1.5)
legend("topleft", legend = c("Univoltine",  "Multivoltine"), col = alpha(c("blue", "green")), pch = 19, cex = 1.5, bty= "n")



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

# Draw arrows, i multiply by 5 to make arrows longer
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

# Draw arrows, i multiply by 5 to make arrows longer
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

# Draw arrows, i multiply by 5 to make arrows longer
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

# Draw arrows, i multiply by 12 to make arrows longer
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

mtext("(f) All sites", side = 3, line = 0.3, adj = 0, cex = 1.5)

dev.off()





















############permutation test (for testing null hypothesis)################
####testing whether the observed eucledian distance is significantly 
####large compared to the null distribution.

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
n_iter <- 5000  # Number of iterations
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

sim_ecdi_cp
ecdi_cp
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
n_iter <- 5000  # Number of iterations
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

sim_ecdi_dp
ecdi_dp

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
n_iter <- 5000  # Number of iterations
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

sim_ecdi_lc
ecdi_lc

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
n_iter <- 5000  # Number of iterations
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

sim_ecdi_sv
ecdi_sv

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
n_iter <- 5000  # Number of iterations
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

sim_ecdi_wa
ecdi_wa

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