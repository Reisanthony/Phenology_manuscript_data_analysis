library(rstan)
library(scales)


#CASTLE PEAK
load('YEAR_PHENOLOGY_INTERACTION_Castle Peak.rdat')
csp= length(sp)
#mu of posteriors
mus<-extract(fit,"mu")
dim(mus[[1]])

#year
#MO
cp_mu <- apply(mus[[1]], 2, quantile, probs = c(0.5, 0.05, 0.95))
cp_mu_3 <- cp_mu[1, 3]
cp_lw_3 <- cp_mu[2, 3]
cp_up_3 <- cp_mu[3, 3]

#TO
cp_mu_4 <- cp_mu[1, 4]
cp_lw_4 <- cp_mu[2, 4]
cp_up_4 <- cp_mu[3, 4]

#LO
cp_mu_5 <- cp_mu[1, 5]
cp_lw_5 <- cp_mu[2, 5]
cp_up_5 <- cp_mu[3, 5]


#beta
betas<-extract(fit,"beta")
#MO
cb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
cc5_b3<-as.numeric(cb3[2,] > 0 | cb3[3,] < 0)+1

#TO
cb4<-apply(betas[[1]][,,4],2,quantile,probs=c(.5,0.05,0.95))
cc5_b4<-as.numeric(cb4[2,] > 0 | cb4[3,] < 0)+1

#LO
cb5<-apply(betas[[1]][,,5],2,quantile,probs=c(.5,0.05,0.95))
cc5_b5<-as.numeric(cb5[2,] > 0 | cb5[3,] < 0)+1


#DONNER PASS
load('YEAR_PHENOLOGY_INTERACTION_Donner Pass.rdat')
dsp= length(sp)
mus<-extract(fit,"mu")
dim(mus[[1]])

dp_mu<-apply(mus[[1]], 2, quantile, probs = c(0.5, 0.05, 0.95))

#MO
dp_mu_3 <- dp_mu[1, 3]
dp_lw_3 <- dp_mu[2, 3]
dp_up_3 <- dp_mu[3, 3]

#TO
dp_mu_4 <- dp_mu[1, 4]
dp_lw_4 <- dp_mu[2, 4]
dp_up_4 <- dp_mu[3, 4]

#LO
dp_mu_5 <- dp_mu[1, 5]
dp_lw_5 <- dp_mu[2, 5]
dp_up_5 <- dp_mu[3, 5]



#beta
betas<-extract(fit,"beta")
#MO
db3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
cc4_b3<-as.numeric(db3[2,] > 0 | db3[3,] < 0)+1

#TO
db4<-apply(betas[[1]][,,4],2,quantile,probs=c(.5,0.05,0.95))
cc4_b4<-as.numeric(db4[2,] > 0 | db4[3,] < 0)+1

#LO
db5<-apply(betas[[1]][,,5],2,quantile,probs=c(.5,0.05,0.95))
cc4_b5<-as.numeric(db5[2,] > 0 | db5[3,] < 0)+1


#LANG CROSSING
load('YEAR_PHENOLOGY_INTERACTION_Lang Crossing.rdat')
lsp= length(sp)
mus<-extract(fit,"mu")
dim(mus[[1]])

lc_mu<-apply(mus[[1]], 2, quantile, probs = c(0.5, 0.05, 0.95))
#MO
lc_mu_3 <- lc_mu[1, 3]
lc_lw_3 <- lc_mu[2, 3]
lc_up_3 <- lc_mu[3, 3]

#TO
lc_mu_4 <- lc_mu[1, 4]
lc_lw_4 <- lc_mu[2, 4]
lc_up_4 <- lc_mu[3, 4]

#LO
lc_mu_5 <- lc_mu[1, 5]
lc_lw_5 <- lc_mu[2, 5]
lc_up_5 <- lc_mu[3, 5]


#beta
betas<-extract(fit,"beta")

#MO
lb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
cc3_b3<-as.numeric(lb3[2,] > 0 | lb3[3,] < 0)+1

#TO
lb4<-apply(betas[[1]][,,4],2,quantile,probs=c(.5,0.05,0.95))
cc3_b4<-as.numeric(lb4[2,] > 0 | lb4[3,] < 0)+1

#LO
lb5<-apply(betas[[1]][,,5],2,quantile,probs=c(.5,0.05,0.95))
cc3_b5<-as.numeric(lb5[2,] > 0 | lb5[3,] < 0)+1



#SIERRA VALLEY
load('YEAR_PHENOLOGY_INTERACTION_Sierra Valley.rdat')
ssp= length(sp)
mus<-extract(fit,"mu")
dim(mus[[1]])

sv_mu<-apply(mus[[1]], 2, quantile, probs = c(0.5, 0.05, 0.95))
#MO
sv_mu_3 <- sv_mu[1, 3]
sv_lw_3 <- sv_mu[2, 3]
sv_up_3 <- sv_mu[3, 3]

#TO
sv_mu_4 <- sv_mu[1, 4]
sv_lw_4 <- sv_mu[2, 4]
sv_up_4 <- sv_mu[3, 4]

#LO
sv_mu_5 <- sv_mu[1, 5]
sv_lw_5 <- sv_mu[2, 5]
sv_up_5 <- sv_mu[3, 5]



#beta
betas<-extract(fit,"beta")
#MO
sb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
cc2_b3<-as.numeric(sb3[2,] > 0 | sb3[3,] < 0)+1

#TO
sb4<-apply(betas[[1]][,,4],2,quantile,probs=c(.5,0.05,0.95))
cc2_b4<-as.numeric(sb4[2,] > 0 | sb4[3,] < 0)+1

#LO
sb5<-apply(betas[[1]][,,5],2,quantile,probs=c(.5,0.05,0.95))
cc2_b5<-as.numeric(sb5[2,] > 0 | sb5[3,] < 0)+1


#WASHINGTON
load('YEAR_PHENOLOGY_INTERACTION_Washington.rdat')
wsp= length(sp)
mus<-extract(fit,"mu")
dim(mus[[1]])

wa_mu<-apply(mus[[1]], 2, quantile, probs = c(0.5, 0.05, 0.95))
#MO
wa_mu_3 <- wa_mu[1, 3]
wa_lw_3 <- wa_mu[2, 3]
wa_up_3 <- wa_mu[3, 3]

#TO
wa_mu_4 <- wa_mu[1, 4]
wa_lw_4 <- wa_mu[2, 4]
wa_up_4 <- wa_mu[3, 4]

#LO
wa_mu_5 <- wa_mu[1, 5]
wa_lw_5 <- wa_mu[2, 5]
wa_up_5 <- wa_mu[3, 5]

#beta
betas<-extract(fit,"beta")
#MO
wb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
cc1_b3<-as.numeric(wb3[2,] > 0 | wb3[3,] < 0)+1

#TO
wb4<-apply(betas[[1]][,,4],2,quantile,probs=c(.5,0.05,0.95))
cc1_b4<-as.numeric(wb4[2,] > 0 | wb4[3,] < 0)+1

#LO
wb5<-apply(betas[[1]][,,5],2,quantile,probs=c(.5,0.05,0.95))
cc1_b5<-as.numeric(wb5[2,] > 0 | wb5[3,] < 0)+1





pdf(paste("YEAR_PHENOLOGY_INTERACTION_DOT_PLOT.pdf",sep=""),width=27,height =9)
bps <- 4.8
rbps <- 12
sps <- 6
axi <- 5
xlabs <- 3.5
tis <- 3.5

layout(matrix(c(1,2,3),1,3,byrow=T))
par(mgp= c(0,-0.1,0), oma=c(1,1,0,0.3), mar=c(2, 1, 1, 1))

#MO
mus<-as.vector(c(wa_mu_3, sv_mu_3, lc_mu_3, dp_mu_3, cp_mu_3))
lower<-as.vector(c(wa_lw_3, sv_lw_3, lc_lw_3, dp_lw_3, cp_lw_3))
upper<-as.vector(c(wa_up_3, sv_up_3, lc_up_3, dp_up_3, cp_up_3))

#x-axis limit
gd2<-c(cb3[1,], db3[1,], lb3[1,], sb3[1,], wb3[1,])

# Dot chart
dotchart(mus, pch = 19, xlim = c(min(gd2), max(gd2)), col = "white", cex = bps, xaxt='n')
points(mus, 1:5, col = "black", pch=19, cex= rbps)

#x-axis
axis(1, tick = T, line = 0, las = 0, cex.axis=axi, tcl = -0.2)

#  x-axis label
mtext("Regression coefficient", side = 1, line = 0.7, cex = xlabs)

#error bars
for (i in 1:length(mus)) {
  arrows(
    y0 = i,
    x0 = lower[i],
    y1 = i,
    x1 = upper[i],
    angle = 180,
    code = 3,
    length = 0.05,
    col = "#FF6F00",
    lwd = 20
  )
}

# y-axis labels
axis(2, at = 1:length(mus), labels = c("WA  ", "SV  ", "LC  ", "DP  ", "CP  "), las = 1, cex.axis = axi, tcl = -0.2)
# Vertical line at point 0 on the x-axis
abline(v = 0, col = "grey", lty = 1)

points(wb3[1,],jitter(rep(1,wsp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc1_b3],pch=19, cex= sps)
points(sb3[1,],jitter(rep(2,ssp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc2_b3],pch=19, cex= sps)
points(lb3[1,],jitter(rep(3,lsp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc3_b3],pch=19, cex= sps)
points(db3[1,],jitter(rep(4,dsp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc4_b3],pch=19, cex= sps)
points(cb3[1,],jitter(rep(5,csp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc5_b3],pch=19, cex= sps)

mtext("(a) MO", side = 3, line = 0.3, adj = 0, cex = tis)


#TO
mus<-as.vector(c(wa_mu_4, sv_mu_4, lc_mu_4, dp_mu_4, cp_mu_4))
lower<-as.vector(c(wa_lw_4, sv_lw_4, lc_lw_4, dp_lw_4, cp_lw_4))
upper<-as.vector(c(wa_up_4, sv_up_4, lc_up_4, dp_up_4, cp_up_4))

#x-axis limit
gd2<-c(cb4[1,], db4[1,], lb4[1,], sb4[1,], wb4[1,])

# Dot chart
dotchart(mus, pch = 19, xlim = c(min(gd2), max(gd2)), col = "white", cex = bps, xaxt='n')
points(mus, 1:5, col = "black", pch=19, cex= rbps)

#x-axis
axis(1, tick = T, line = 0, las = 0, cex.axis=axi, tcl = -0.2)

#  x-axis label
mtext("Regression coefficient", side = 1, line = 0.7, cex = xlabs)

#error bars
for (i in 1:length(mus)) {
  arrows(
    y0 = i,
    x0 = lower[i],
    y1 = i,
    x1 = upper[i],
    angle = 180,
    code = 3,
    length = 0.05,
    col = "#FF6F00",
    lwd = 20
  )
}

# y-axis labels
axis(2, at = 1:length(mus), labels = c("WA  ", "SV  ", "LC  ", "DP  ", "CP  "), las = 1, cex.axis = axi, tcl = -0.2)
# Vertical line at point 0 on the x-axis
abline(v = 0, col = "grey", lty = 1)

points(wb4[1,],jitter(rep(1,wsp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc1_b4],pch=19, cex= sps)
points(sb4[1,],jitter(rep(2,ssp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc2_b4],pch=19, cex= sps)
points(lb4[1,],jitter(rep(3,lsp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc3_b4],pch=19, cex= sps)
points(db4[1,],jitter(rep(4,dsp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc4_b4],pch=19, cex= sps)
points(cb4[1,],jitter(rep(5,csp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc5_b4],pch=19, cex= sps)

mtext("(b) TO", side = 3, line = 0.3, adj = 0, cex = tis)

#LO
mus<-as.vector(c(wa_mu_5, sv_mu_5, lc_mu_5, dp_mu_5, cp_mu_5))
lower<-as.vector(c(wa_lw_5, sv_lw_5, lc_lw_5, dp_lw_5, cp_lw_5))
upper<-as.vector(c(wa_up_5, sv_up_5, lc_up_5, dp_up_5, cp_up_5))

#x-axis limit
gd<-c(cb5[1,], db5[1,], lb5[1,], sb5[1,], wb5[1,])


# Dot chart
dotchart(mus, pch = 19, xlim = c(min(gd), max(gd)), col = "white", cex = bps, xaxt='n')
points(mus, 1:5, col = "black", pch=19, cex= rbps)

#x-axis
axis(1, tick = T, line = 0, las = 0, cex.axis=axi, tcl = -0.2)

#  x-axis label
mtext("Regression coefficient", side = 1, line = 0.7, cex = xlabs)

#error bars
for (i in 1:length(mus)) {
  arrows(
    y0 = i,
    x0 = lower[i],
    y1 = i,
    x1 = upper[i],
    angle = 180,
    code = 3,
    length = 0.05,
    col = "#FF6F00",
    lwd = 20
  )
}

# y-axis labels
axis(2, at = 1:length(mus), labels = c("WA  ", "SV  ", "LC  ", "DP  ", "CP  "), las = 1, cex.axis = axi, tcl = -0.2)
# Vertical line at point 0 on the x-axis
abline(v = 0, col = "grey", lty = 1)


points(wb5[1,],jitter(rep(1,wsp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc1_b5],pch=19, cex= sps)
points(sb5[1,],jitter(rep(2,ssp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc2_b5],pch=19, cex= sps)
points(lb5[1,],jitter(rep(3,lsp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc3_b5],pch=19, cex= sps)
points(db5[1,],jitter(rep(4,dsp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc4_b5],pch=19, cex= sps)
points(cb5[1,],jitter(rep(5,csp), factor = 4),col=alpha(c("gray60","dodgerblue"),.2)[cc5_b5],pch=19, cex= sps)

mtext("(c) LO", side = 3, line = 0.3, adj = 0, cex = tis)

dev.off()



