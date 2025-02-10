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
               rep("black", 3)) # green for spring min temp

arrow_lab <- rep(c("PO", "TO", "LO"), 3)

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
mtext("(a)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)



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
mtext("(b)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

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
mtext("(c)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

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
#title(main = "(d) SV", cex.main = 2)
mtext("(d)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

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
mtext("(e)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)


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
mtext("(f)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

dev.off()





pdf(paste("SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_PLOT_new.pdf",sep=""),width=12,height=17)

layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))

par(oma=c(1,0.3,0,0.3), mar=c(6,5,3,2))

arrow_col <- c(rep("red", 3),  # red for spring max temp
               rep("#87CEEB", 3), # blue for winter precipitation
               rep("black", 3)) # green for spring min temp

arrow_lab <- rep(c("PO", "TO", "LO"), 3)

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

# Draw arrows, i multiply by 5 to make arrows longer
cp_l_x <- cppca$rotation[,1]*4.5
cp_l_y <- cppca$rotation[,2]*4.5
#arrows(x0=0, x1=cp_l_x, y0=0, y1=cp_l_y, col=arrow_col, length=0.15, lwd=1.5)

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
mtext("(a)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)



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

# Draw arrows, i multiply by 6 to make arrows longer
dp_l_x <- dppca$rotation[,1]*5
dp_l_y <- dppca$rotation[,2]*5
#arrows(x0=0, x1=dp_l_x, y0=0, y1=dp_l_y, col=arrow_col, length=0.15, lwd=1.5)

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
mtext("(b)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

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

# Draw arrows, i multiply by 6 to make arrows longer
lc_l_x <- lcpca$rotation[,1]*5
lc_l_y <- lcpca$rotation[,2]*5
#arrows(x0=0, x1=lc_l_x, y0=0, y1=lc_l_y, col=arrow_col, length=0.15, lwd=1.5)

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
mtext("(c)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

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
#arrows(x0=0, x1=sv_l_x, y0=0, y1=sv_l_y, col=arrow_col, length=0.15, lwd=1.5)

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
mtext("(d)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

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

# Draw arrows, i multiply by 6 to make arrows longer
wa_l_x <- wapca$rotation[,1]*5
wa_l_y <- wapca$rotation[,2]*5
#arrows(x0=0, x1=wa_l_x, y0=0, y1=wa_l_y, col=arrow_col, length=0.15, lwd=1.5)

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
mtext("(e)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)


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
#arrows(x0=0, x1=sp_allst_l_x, y0=0, y1=sp_allst_l_y, col=sp_allst_arrow_col, length=0.15, lwd=1.5)

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
mtext("(f)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

dev.off()







pdf(paste("SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_PLOT_for_you.pdf",sep=""),width=12,height=17)

layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))

par(oma=c(1,0.3,0,0.3), mar=c(6,5,3,2))

arrow_col <- c(rep("red", 3),  # red for spring max temp
               rep("#87CEEB", 3), # blue for winter precipitation
               rep("black", 3)) # green for spring min temp

arrow_lab <- rep(c("PO", "TO", "LO"), 3)

#biplot
#biplot(cppca)
plot(cppca$x[,1], cppca$x[,2], xlab=paste("PCA 1 (", round(sumcp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumcp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
text(cppca$x[,1], cppca$x[,2], labels=row.names(cppca$x), pos=c(1,4,2,3), font=2)

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
mtext("(a)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)



#biplot
#biplot(dppca)
plot(dppca$x[,1], dppca$x[,2], xlab=paste("PCA 1 (", round(sumdp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumdp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
text(dppca$x[,1], dppca$x[,2], labels=row.names(dppca$x), pos=c(1,4,2,3), font=2)

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
mtext("(b)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

#biplot
#biplot(lcpca)
plot(lcpca$x[,1], lcpca$x[,2], xlab=paste("PCA 1 (", round(sumlc$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumlc$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
text(lcpca$x[,1], lcpca$x[,2], labels=row.names(lcpca$x), pos=c(1,4,2,3), font=2)

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
mtext("(c)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

#biplot
#biplot(svpca)
plot(svpca$x[,1], svpca$x[,2], xlab=paste("PCA 1 (", round(sumsv$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumsv$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
text(svpca$x[,1], svpca$x[,2], labels=row.names(svpca$x), pos=c(1,4,2,3), font=2)

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
mtext("(d)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

#biplot
#biplot(wapca)
plot(wapca$x[,1], wapca$x[,2], xlab=paste("PCA 1 (", round(sumwa$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumwa$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
text(wapca$x[,1], wapca$x[,2], labels=row.names(wapca$x), pos=c(1,4,2,3), font=2)

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
mtext("(e)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)


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
text(sp_allst_pca$x[,1], sp_allst_pca$x[,2], labels=row.names(sp_allst_pca$x), pos=c(1,4,2,3), font=2)

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
mtext("(f)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

dev.off()


pdf(paste("SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_PCA_PLOT_for_me.pdf",sep=""),width=12,height=17)

layout(matrix(c(1,2,3,4,5,6),3,2,byrow=T))

par(oma=c(1,0.3,0,0.3), mar=c(6,5,3,2))

arrow_col <- c(rep("red", 3),  # red for spring max temp
               rep("#87CEEB", 3), # blue for winter precipitation
               rep("black", 3)) # black for spring min temp

arrow_lab <- rep(c("PO", "TO", "LO"), 3)

#biplot
#biplot(cppca)
plot(cppca$x[,1], cppca$x[,2], xlab=paste("PCA 1 (", round(sumcp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumcp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
text(cppca$x[,1], cppca$x[,2], labels=seq_len(nrow(cppca$x)), pos=c(1,4,2,3), font=2)

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
mtext("(a)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)



#biplot
#biplot(dppca)
plot(dppca$x[,1], dppca$x[,2], xlab=paste("PCA 1 (", round(sumdp$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumdp$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
text(dppca$x[,1], dppca$x[,2], labels=seq_len(nrow(dppca$x)), pos=c(1,4,2,3), font=2)

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
mtext("(b)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

#biplot
#biplot(lcpca)
plot(lcpca$x[,1], lcpca$x[,2], xlab=paste("PCA 1 (", round(sumlc$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumlc$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
text(lcpca$x[,1], lcpca$x[,2], labels=seq_len(nrow(lcpca$x)), pos=c(1,4,2,3), font=2)

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
mtext("(c)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

#biplot
#biplot(svpca)
plot(svpca$x[,1], svpca$x[,2], xlab=paste("PCA 1 (", round(sumsv$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumsv$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
text(svpca$x[,1], svpca$x[,2], labels=seq_len(nrow(svpca$x)), pos=c(1,4,2,3), font=2)

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
mtext("(d)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

#biplot
#biplot(wapca)
plot(wapca$x[,1], wapca$x[,2], xlab=paste("PCA 1 (", round(sumwa$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(sumwa$importance[5]*100, 1), "%)", sep = ""),
     pch=19, col="grey", cex=1.5, las=1, asp=1, cex.lab=2, cex.axis=2)

# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
text(wapca$x[,1], wapca$x[,2], labels=seq_len(nrow(wapca$x)), pos=c(1,4,2,3), font=2)

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
mtext("(e)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)


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
text(sp_allst_pca$x[,1], sp_allst_pca$x[,2], labels=seq_len(nrow(sp_allst_pca$x)), pos=c(1,4,2,3), font=2)

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
mtext("(f)", side = 3, line = 0.3, adj = -0.1, cex = 1.5)

dev.off()








> row.names(cppca$x)
[1] "Agriades podarce"                 "Anthocharis stella"              
[3] "Boloria epithore"                 "Callophrys sheridanii lemberti"  
[5] "Celastrina ladon echo"            "Cercyonis oetus"                 
[7] "Chlosyne hoffmanni"               "Colias eurytheme"                
[9] "Danaus plexippus"                 "Erynnis propertius"              
[11] "Euchloe ausonides"                "Euchloe hyantis hyantis"         
[13] "Euphilotes battoides"             "Euphilotes enoptes"              
[15] "Everes amyntula"                  "Glaucopsyche lygdamus"           
[17] "Hesperia colorado harpalus"       "Hesperia nevada"                 
[19] "Incisalia augustinus iroides"     "Incisalia eryphon"               
[21] "Junonia coenia"                   "Limenitis lorquini"              
[23] "Lycaeides idas anna"              "Lycaena arota arota"             
[25] "Lycaena cupreus"                  "Lycaena editha"                  
[27] "Lycaena heteronea"                "Lycaena nivalis"                 
[29] "Nymphalis milberti"               "Ochlodes sylvanoides"            
[31] "Oeneis chryxus ivallda"           "Papilio eurymedon"               
[33] "Papilio indra"                    "Papilio rutulus"                 
[35] "Papilio zelicaon"                 "Parnassius clodius"              
[37] "Phyciodes campestris montana"     "Pieris rapae"                    
[39] "Plebejus acmon"                   "Plebejus icarioides"             
[41] "Plebejus lupini"                  "Plebejus saepiolus"              
[43] "Plebejus shasta"                  "Polites sabuleti tecumseh"       
[45] "Polites sonora"                   "Polygonia zephyrus"              
[47] "Pontia occidentalis"              "Pontia protodice"                
[49] "Pontia sisymbrii"                 "Pyrgus communis"                 
[51] "Pyrgus ruralis"                   "Satyrium behrii"                 
[53] "Satyrium fuliginosum fuliginosum" "Satyrium saepium"                
[55] "Speyeria atlantis irene"          "Speyeria coronis"                
[57] "Speyeria egleis"                  "Speyeria mormonia"               
[59] "Speyeria zerene"                  "Strymon melinus"                 
[61] "Thorybes mexicana nevada"         "Vanessa annabella"               
[63] "Vanessa cardui"                   "Vanessa virginiensis"

> row.names(dppca$x)
[1] "Adelpha bredowii californica"     "Agriades podarce"                
[3] "Amblyscirtes vialis"              "Anthocharis sara sara"           
[5] "Anthocharis stella"               "Apodemia mormo"                  
[7] "Boloria epithore"                 "Brephidium exile"                
[9] "Callophrys sheridanii lemberti"   "Celastrina ladon echo"           
[11] "Chlosyne hoffmanni"               "Chlosyne palla"                  
[13] "Coenonympha tullia ampelos"       "Colias eurytheme"                
[15] "Danaus plexippus"                 "Erynnis persius"                 
[17] "Erynnis propertius"               "Euchloe hyantis hyantis"         
[19] "Euphilotes battoides"             "Euphilotes enoptes"              
[21] "Everes amyntula"                  "Glaucopsyche lygdamus"           
[23] "Hesperia colorado harpalus"       "Hesperia juba"                   
[25] "Hylephila phyleus"                "Incisalia augustinus iroides"    
[27] "Incisalia eryphon"                "Incisalia mossii"                
[29] "Junonia coenia"                   "Limenitis lorquini"              
[31] "Lycaeides idas anna"              "Lycaeides melissa melissa"       
[33] "Lycaena arota arota"              "Lycaena cupreus"                 
[35] "Lycaena editha"                   "Lycaena helloides"               
[37] "Lycaena heteronea"                "Lycaena mariposa"                
[39] "Lycaena nivalis"                  "Lycaena rubidus"                 
[41] "Mitoura gryneus nelsoni"          "Neophasia menapia"               
[43] "Nymphalis antiopa"                "Nymphalis californica"           
[45] "Nymphalis milberti"               "Ochlodes sylvanoides"            
[47] "Papilio eurymedon"                "Papilio indra"                   
[49] "Papilio rutulus"                  "Papilio zelicaon"                
[51] "Parnassius clodius"               "Phyciodes campestris montana"    
[53] "Phyciodes mylitta"                "Phyciodes orseis herlani"        
[55] "Pieris rapae"                     "Plebejus acmon"                  
[57] "Plebejus icarioides"              "Plebejus lupini"                 
[59] "Plebejus saepiolus"               "Plebejus shasta"                 
[61] "Polites sabuleti tecumseh"        "Polites sonora"                  
[63] "Polygonia faunus"                 "Polygonia zephyrus"              
[65] "Pontia beckerii"                  "Pontia occidentalis"             
[67] "Pontia protodice"                 "Pontia sisymbrii"                
[69] "Pyrgus communis"                  "Pyrgus ruralis"                  
[71] "Satyrium behrii"                  "Satyrium californica"            
[73] "Satyrium fuliginosum fuliginosum" "Satyrium saepium"                
[75] "Satyrium sylvinus"                "Speyeria atlantis irene"         
[77] "Speyeria coronis"                 "Speyeria cybele leto"            
[79] "Speyeria egleis"                  "Speyeria hydaspe"                
[81] "Speyeria mormonia"                "Speyeria zerene"                 
[83] "Strymon melinus"                  "Thorybes mexicana nevada"        
[85] "Vanessa annabella"                "Vanessa atalanta"                
[87] "Vanessa cardui"                   "Vanessa virginiensis"            
> row.names(lcpca$x)
[1] "Adelpha bredowii californica"   "Amblyscirtes vialis"           
[3] "Anthocharis lanceolata"         "Anthocharis sara sara"         
[5] "Anthocharis stella"             "Apodemia mormo"                
[7] "Atlides halesus"                "Boloria epithore"              
[9] "Callophrys dumetorum"           "Callophrys sheridanii lemberti"
[11] "Celastrina ladon echo"          "Cercyonis sthenele silvestris" 
[13] "Chlosyne palla"                 "Coenonympha tullia california" 
[15] "Colias eurytheme"               "Danaus plexippus"              
[17] "Epargyreus clarus"              "Erynnis icelus"                
[19] "Erynnis persius"                "Erynnis propertius"            
[21] "Euchloe hyantis foothill"       "Euchloe hyantis hyantis"       
[23] "Euphilotes battoides"           "Euphilotes enoptes"            
[25] "Euphydryas chalcedona"          "Everes amyntula"               
[27] "Glaucopsyche lygdamus"          "Habrodais grunus"              
[29] "Hesperia colorado harpalus"     "Hesperia juba"                 
[31] "Incisalia augustinus iroides"   "Incisalia eryphon"             
[33] "Incisalia mossii"               "Junonia coenia"                
[35] "Limenitis lorquini"             "Lycaeides idas anna"           
[37] "Lycaena arota arota"            "Lycaena cupreus"               
[39] "Lycaena editha"                 "Lycaena helloides"             
[41] "Lycaena nivalis"                "Mitoura gryneus nelsoni"       
[43] "Neophasia menapia"              "Nymphalis antiopa"             
[45] "Ochlodes agricola"              "Ochlodes sylvanoides"          
[47] "Papilio eurymedon"              "Papilio rutulus"               
[49] "Papilio zelicaon"               "Parnassius clodius"            
[51] "Philotes sonorensis"            "Phyciodes campestris montana"  
[53] "Phyciodes mylitta"              "Pieris napi"                   
[55] "Pieris rapae"                   "Plebejus acmon"                
[57] "Plebejus icarioides"            "Plebejus lupini"               
[59] "Plebejus saepiolus"             "Polites sabuleti tecumseh"     
[61] "Polites sonora"                 "Polygonia zephyrus"            
[63] "Pontia occidentalis"            "Pontia protodice"              
[65] "Pontia sisymbrii"               "Pyrgus communis"               
[67] "Pyrgus ruralis"                 "Satyrium auretorum"            
[69] "Satyrium californica"           "Satyrium saepium"              
[71] "Satyrium sylvinus"              "Speyeria atlantis irene"       
[73] "Speyeria callippe juba"         "Speyeria coronis"              
[75] "Speyeria cybele leto"           "Speyeria hydaspe"              
[77] "Speyeria zerene"                "Strymon melinus"               
[79] "Thorybes pylades"               "Vanessa annabella"             
[81] "Vanessa atalanta"               "Vanessa cardui"                
[83] "Vanessa virginiensis"          
> row.names(svpca$x)
[1] "Adelpha bredowii californica"           
[2] "Anthocharis sara thoosa"                
[3] "Anthocharis stella"                     
[4] "Atalopedes campestris"                  
[5] "Atlides halesus"                        
[6] "Brephidium exile"                       
[7] "Celastrina ladon echo"                  
[8] "Cercyonis oetus"                        
[9] "Cercyonis pegala boopis"                
[10] "Cercyonis sthenele silvestris"          
[11] "Chlosyne palla"                         
[12] "Coenonympha tullia ampelos"             
[13] "Colias alexandra"                       
[14] "Colias eurytheme"                       
[15] "Colias philodice (eriphyle)"            
[16] "Danaus plexippus"                       
[17] "Erynnis propertius"                     
[18] "Euchloe ausonides"                      
[19] "Euchloe hyantis lotta"                  
[20] "Euphilotes battoides"                   
[21] "Euphydryas chalcedona"                  
[22] "Euphydryas editha"                      
[23] "Glaucopsyche lygdamus"                  
[24] "Hesperia colorado idaho"                
[25] "Hesperia juba"                          
[26] "Incisalia augustinus iroides"           
[27] "Junonia coenia"                         
[28] "Limenitis lorquini"                     
[29] "Lycaeides melissa melissa"              
[30] "Lycaena arota arota"                    
[31] "Lycaena arota virginiensis"             
[32] "Lycaena editha"                         
[33] "Lycaena helloides"                      
[34] "Mitoura gryneus chalcosiva"             
[35] "Neophasia menapia"                      
[36] "Nymphalis antiopa"                      
[37] "Ochlodes sylvanoides"                   
[38] "Papilio eurymedon"                      
[39] "Papilio multicaudatus"                  
[40] "Papilio rutulus"                        
[41] "Papilio zelicaon"                       
[42] "Phyciodes campestris campestris/montana"
[43] "Phyciodes campestris montana"           
[44] "Phyciodes mylitta"                      
[45] "Pieris rapae"                           
[46] "Plebejus acmon"                         
[47] "Plebejus icarioides"                    
[48] "Plebejus saepiolus"                     
[49] "Polites sabuleti sabuleti"              
[50] "Polites sabuleti ssp."                  
[51] "Polites sabuleti tecumseh"              
[52] "Polites sonora"                         
[53] "Pontia beckerii"                        
[54] "Pontia occidentalis"                    
[55] "Pontia protodice"                       
[56] "Pontia sisymbrii"                       
[57] "Pyrgus communis"                        
[58] "Satyrium behrii"                        
[59] "Satyrium californica"                   
[60] "Satyrium saepium"                       
[61] "Satyrium sylvinus"                      
[62] "Satyrium tetra"                         
[63] "Speyeria callippe nevadensis"           
[64] "Speyeria coronis"                       
[65] "Speyeria zerene"                        
[66] "Strymon melinus"                        
[67] "Vanessa annabella"                      
[68] "Vanessa atalanta"                       
[69] "Vanessa cardui"                         
[70] "Vanessa virginiensis"                   
> row.names(wapca$x)
[1] "Adelpha bredowii californica"  "Amblyscirtes vialis"          
[3] "Anthocharis lanceolata"        "Anthocharis sara sara"        
[5] "Apodemia mormo"                "Atalopedes campestris"        
[7] "Atlides halesus"               "Battus philenor"              
[9] "Callophrys dumetorum"          "Celastrina ladon echo"        
[11] "Cercyonis sthenele silvestris" "Chlosyne palla"               
[13] "Coenonympha tullia california" "Colias eurytheme"             
[15] "Danaus plexippus"              "Epargyreus clarus"            
[17] "Erynnis icelus"                "Erynnis persius"              
[19] "Erynnis propertius"            "Erynnis tristis"              
[21] "Euchloe hyantis foothill"      "Euphilotes enoptes"           
[23] "Euphydryas chalcedona"         "Euphydryas editha"            
[25] "Everes amyntula"               "Everes comyntas"              
[27] "Glaucopsyche lygdamus"         "Habrodais grunus"             
[29] "Hesperia colorado harpalus"    "Hesperia colorado ssp."       
[31] "Hesperia juba"                 "Hesperia lindseyi"            
[33] "Hylephila phyleus"             "Incisalia augustinus iroides" 
[35] "Incisalia eryphon"             "Incisalia mossii"             
[37] "Junonia coenia"                "Limenitis lorquini"           
[39] "Lycaena arota arota"           "Lycaena gorgon"               
[41] "Lycaena xanthoides"            "Mitoura gryneus nelsoni"      
[43] "Mitoura spinetorum"            "Neophasia menapia"            
[45] "Nymphalis antiopa"             "Nymphalis californica"        
[47] "Ochlodes agricola"             "Ochlodes sylvanoides"         
[49] "Papilio eurymedon"             "Papilio multicaudatus"        
[51] "Papilio rutulus"               "Papilio zelicaon"             
[53] "Parnassius clodius"            "Philotes sonorensis"          
[55] "Phyciodes campestris montana"  "Phyciodes mylitta"            
[57] "Pieris napi"                   "Pieris rapae"                 
[59] "Plebejus acmon"                "Plebejus icarioides"          
[61] "Poanes melane"                 "Polygonia zephyrus"           
[63] "Pontia protodice"              "Pontia sisymbrii"             
[65] "Pyrgus communis"               "Pyrgus ruralis"               
[67] "Satyrium californica"          "Satyrium saepium"             
[69] "Satyrium sylvinus"             "Speyeria callippe juba"       
[71] "Speyeria hydaspe"              "Speyeria zerene"              
[73] "Strymon melinus"               "Thessalia leanira"            
[75] "Thorybes pylades"              "Vanessa annabella"            
[77] "Vanessa atalanta"              "Vanessa cardui"               
[79] "Vanessa virginiensis"          "Zerene eurydice"              
> row.names(sp_allst_pca$x)
[1] "Celastrina ladon echo"        "Colias eurytheme"            
[3] "Danaus plexippus"             "Erynnis propertius"          
[5] "Glaucopsyche lygdamus"        "Incisalia augustinus iroides"
[7] "Junonia coenia"               "Limenitis lorquini"          
[9] "Lycaena arota arota"          "Ochlodes sylvanoides"        
[11] "Papilio eurymedon"            "Papilio rutulus"             
[13] "Papilio zelicaon"             "Phyciodes campestris montana"
[15] "Pieris rapae"                 "Plebejus acmon"              
[17] "Plebejus icarioides"          "Pontia protodice"            
[19] "Pontia sisymbrii"             "Pyrgus communis"             
[21] "Satyrium saepium"             "Speyeria zerene"             
[23] "Strymon melinus"              "Vanessa annabella"           
[25] "Vanessa cardui"               "Vanessa virginiensis"        
> 
  
  
