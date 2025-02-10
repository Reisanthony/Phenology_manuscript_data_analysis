library(rstan)
library(scales)

#CASTLE PEAK
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Castle Peak.rdat')
csp= length(sp)
#mu of posteriors
mus<-extract(fit,"mu")
dim(mus[[1]])
#median, lower and confidence interval
#spring max temperature
#peak
cp_mu <- apply(mus[[1]], 2, quantile, probs = c(0.5, 0.05, 0.95))
cp_mu_1 <- cp_mu[1, 1]
cp_lw_1 <- cp_mu[2, 1]
cp_up_1 <- cp_mu[3, 1]

#timing
cp_mu_7 <- cp_mu[1, 7]
cp_lw_7 <- cp_mu[2, 7]
cp_up_7 <- cp_mu[3, 7]

#length
cp_mu_8 <- cp_mu[1, 8]
cp_lw_8 <- cp_mu[2, 8]
cp_up_8 <- cp_mu[3, 8]


#winter precipitation
#peak
cp_mu_2 <- cp_mu[1, 2]
cp_lw_2 <- cp_mu[2, 2]
cp_up_2 <- cp_mu[3, 2]

#timing
cp_mu_9 <- cp_mu[1, 9]
cp_lw_9 <- cp_mu[2, 9]
cp_up_9 <- cp_mu[3, 9]

#length
cp_mu_10 <- cp_mu[1, 10]
cp_lw_10 <- cp_mu[2, 10]
cp_up_10 <- cp_mu[3, 10]

#spring min temperature
#peak
cp_mu_3 <- cp_mu[1, 3]
cp_lw_3 <- cp_mu[2, 3]
cp_up_3 <- cp_mu[3, 3]

#timing
cp_mu_11 <- cp_mu[1, 11]
cp_lw_11 <- cp_mu[2, 11]
cp_up_11 <- cp_mu[3, 11]

#length
cp_mu_12 <- cp_mu[1, 12]
cp_lw_12 <- cp_mu[2, 12]
cp_up_12 <- cp_mu[3, 12]


#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
cb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
cc5_b1<-as.numeric(cb1[2,] > 0 | cb1[3,] < 0)+1

#timing
cb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
cc5_b7<-as.numeric(cb7[2,] > 0 | cb7[3,] < 0)+1

#length
cb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))
cc5_b8<-as.numeric(cb8[2,] > 0 | cb8[3,] < 0)+1

#winter precipitation
#peak
cb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
cc5_b2<-as.numeric(cb2[2,] > 0 | cb2[3,] < 0)+1

#timing
cb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
cc5_b9<-as.numeric(cb9[2,] > 0 | cb9[3,] < 0)+1

#length
cb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))
cc5_b10<-as.numeric(cb10[2,] > 0 | cb10[3,] < 0)+1

#spring min temperature
#peak
cb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
cc5_b3<-as.numeric(cb3[2,] > 0 | cb3[3,] < 0)+1

#timing
cb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
cc5_b11<-as.numeric(cb11[2,] > 0 | cb11[3,] < 0)+1

#length
cb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))
cc5_b12<-as.numeric(cb12[2,] > 0 | cb12[3,] < 0)+1


#DONNER PASS
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Donner Pass.rdat')
dsp= length(sp)
mus<-extract(fit,"mu")
dim(mus[[1]])
#median, lower and confidence interval

dp_mu<-apply(mus[[1]], 2, quantile, probs = c(0.5, 0.05, 0.95))

#spring max temperature
#peak
dp_mu_1 <- dp_mu[1, 1]
dp_lw_1 <- dp_mu[2, 1]
dp_up_1 <- dp_mu[3, 1]

#timing
dp_mu_7 <- dp_mu[1, 7]
dp_lw_7 <- dp_mu[2, 7]
dp_up_7 <- dp_mu[3, 7]

#length
dp_mu_8 <- dp_mu[1, 8]
dp_lw_8 <- dp_mu[2, 8]
dp_up_8 <- dp_mu[3, 8]

#winter precipitation
#peak
dp_mu_2 <- dp_mu[1, 2]
dp_lw_2 <- dp_mu[2, 2]
dp_up_2 <- dp_mu[3, 2]

#timing
dp_mu_9 <- dp_mu[1, 9]
dp_lw_9 <- dp_mu[2, 9]
dp_up_9 <- dp_mu[3, 9]

#length
dp_mu_10 <- dp_mu[1, 10]
dp_lw_10 <- dp_mu[2, 10]
dp_up_10 <- dp_mu[3, 10]

#spring min temperature
#peak
dp_mu_3 <- dp_mu[1, 3]
dp_lw_3 <- dp_mu[2, 3]
dp_up_3 <- dp_mu[3, 3]

#timing
dp_mu_11 <- dp_mu[1, 11]
dp_lw_11 <- dp_mu[2, 11]
dp_up_11 <- dp_mu[3, 11]

#length
dp_mu_12 <- dp_mu[1, 12]
dp_lw_12 <- dp_mu[2, 12]
dp_up_12 <- dp_mu[3, 12]



#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
db1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
cc4_b1<-as.numeric(db1[2,] > 0 | db1[3,] < 0)+1

#timing
db7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
cc4_b7<-as.numeric(db7[2,] > 0 | db7[3,] < 0)+1

#length
db8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))
cc4_b8<-as.numeric(db8[2,] > 0 | db8[3,] < 0)+1

#winter precipitation
#peak
db2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
cc4_b2<-as.numeric(db2[2,] > 0 | db2[3,] < 0)+1

#timing
db9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
cc4_b9<-as.numeric(db9[2,] > 0 | db9[3,] < 0)+1

#length
db10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))
cc4_b10<-as.numeric(db10[2,] > 0 | db10[3,] < 0)+1

#spring min temperature
#peak
db3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
cc4_b3<-as.numeric(db3[2,] > 0 | db3[3,] < 0)+1

#timing
db11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
cc4_b11<-as.numeric(db11[2,] > 0 | db11[3,] < 0)+1

#length
db12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))
cc4_b12<-as.numeric(db12[2,] > 0 | db12[3,] < 0)+1


#LANG CROSSING
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Lang Crossing.rdat')
lsp= length(sp)
mus<-extract(fit,"mu")
dim(mus[[1]])
#median, lower and confidence interval

lc_mu<-apply(mus[[1]], 2, quantile, probs = c(0.5, 0.05, 0.95))
#spring max temperature
#peak
lc_mu_1 <- lc_mu[1, 1]
lc_lw_1 <- lc_mu[2, 1]
lc_up_1 <- lc_mu[3, 1]

#timing
lc_mu_7 <- lc_mu[1, 7]
lc_lw_7 <- lc_mu[2, 7]
lc_up_7 <- lc_mu[3, 7]

#length
lc_mu_8 <- lc_mu[1, 8]
lc_lw_8 <- lc_mu[2, 8]
lc_up_8 <- lc_mu[3, 8]

#winter precipitation
#peak
lc_mu_2 <- lc_mu[1, 2]
lc_lw_2 <- lc_mu[2, 2]
lc_up_2 <- lc_mu[3, 2]

#timing
lc_mu_9 <- lc_mu[1, 9]
lc_lw_9 <- lc_mu[2, 9]
lc_up_9 <- lc_mu[3, 9]

#length
lc_mu_10 <- lc_mu[1, 10]
lc_lw_10 <- lc_mu[2, 10]
lc_up_10 <- lc_mu[3, 10]

#spring min temperature
#peak
lc_mu_3 <- lc_mu[1, 3]
lc_lw_3 <- lc_mu[2, 3]
lc_up_3 <- lc_mu[3, 3]

#timing
lc_mu_11 <- lc_mu[1, 11]
lc_lw_11 <- lc_mu[2, 11]
lc_up_11 <- lc_mu[3, 11]

#length
lc_mu_12 <- lc_mu[1, 12]
lc_lw_12 <- lc_mu[2, 12]
lc_up_12 <- lc_mu[3, 12]


#beta
betas<-extract(fit,"beta")

#spring max temperature
#peak
lb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
cc3_b1<-as.numeric(lb1[2,] > 0 | lb1[3,] < 0)+1

#timing
lb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
cc3_b7<-as.numeric(lb7[2,] > 0 | lb7[3,] < 0)+1

#length
lb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))
cc3_b8<-as.numeric(lb8[2,] > 0 | lb8[3,] < 0)+1

#winter precipitation
#peak
lb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
cc3_b2<-as.numeric(lb2[2,] > 0 | lb2[3,] < 0)+1

#timing
lb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
cc3_b9<-as.numeric(lb9[2,] > 0 | lb9[3,] < 0)+1

#length
lb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))
cc3_b10<-as.numeric(lb10[2,] > 0 | lb10[3,] < 0)+1

#spring min temperature
#peak
lb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
cc3_b3<-as.numeric(lb3[2,] > 0 | lb3[3,] < 0)+1

#timing
lb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
cc3_b11<-as.numeric(lb11[2,] > 0 | lb11[3,] < 0)+1

#length
lb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))
cc3_b12<-as.numeric(lb12[2,] > 0 | lb12[3,] < 0)+1



#SIERRA VALLEY
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Sierra Valley.rdat')
ssp= length(sp)
mus<-extract(fit,"mu")
dim(mus[[1]])

#median, lower and confidence interval
sv_mu<-apply(mus[[1]], 2, quantile, probs = c(0.5, 0.05, 0.95))
#spring max temperature
#peak
sv_mu_1 <- sv_mu[1, 1]
sv_lw_1 <- sv_mu[2, 1]
sv_up_1 <- sv_mu[3, 1]

#timing
sv_mu_7 <- sv_mu[1, 7]
sv_lw_7 <- sv_mu[2, 7]
sv_up_7 <- sv_mu[3, 7]

#length
sv_mu_8 <- sv_mu[1, 8]
sv_lw_8 <- sv_mu[2, 8]
sv_up_8 <- sv_mu[3, 8]

#winter precipitation
#peak
sv_mu_2 <- sv_mu[1, 2]
sv_lw_2 <- sv_mu[2, 2]
sv_up_2 <- sv_mu[3, 2]

#timing
sv_mu_9 <- sv_mu[1, 9]
sv_lw_9 <- sv_mu[2, 9]
sv_up_9 <- sv_mu[3, 9]

#length
sv_mu_10 <- sv_mu[1, 10]
sv_lw_10 <- sv_mu[2, 10]
sv_up_10 <- sv_mu[3, 10]

#spring min temperature
#peak
sv_mu_3 <- sv_mu[1, 3]
sv_lw_3 <- sv_mu[2, 3]
sv_up_3 <- sv_mu[3, 3]

#timing
sv_mu_11 <- sv_mu[1, 11]
sv_lw_11 <- sv_mu[2, 11]
sv_up_11 <- sv_mu[3, 11]

#length
sv_mu_12 <- sv_mu[1, 12]
sv_lw_12 <- sv_mu[2, 12]
sv_up_12 <- sv_mu[3, 12]



#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
sb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
cc2_b1<-as.numeric(sb1[2,] > 0 | sb1[3,] < 0)+1

#timing
sb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
cc2_b7<-as.numeric(sb7[2,] > 0 | sb7[3,] < 0)+1

#length
sb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))
cc2_b8<-as.numeric(sb8[2,] > 0 | sb8[3,] < 0)+1

#winter precipitation
#peak
sb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
cc2_b2<-as.numeric(sb2[2,] > 0 | sb2[3,] < 0)+1

#timing
sb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
cc2_b9<-as.numeric(sb9[2,] > 0 | sb9[3,] < 0)+1

#length
sb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))
cc2_b10<-as.numeric(sb10[2,] > 0 | sb10[3,] < 0)+1

#spring min temperature
#peak
sb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
cc2_b3<-as.numeric(sb3[2,] > 0 | sb3[3,] < 0)+1

#timing
sb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
cc2_b11<-as.numeric(sb11[2,] > 0 | sb11[3,] < 0)+1

#length
sb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))
cc2_b12<-as.numeric(sb12[2,] > 0 | sb12[3,] < 0)+1


#WASHINGTON
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Washington.rdat')
wsp= length(sp)
mus<-extract(fit,"mu")
dim(mus[[1]])
wa_mu<-apply(mus[[1]], 2, quantile, probs = c(0.5, 0.05, 0.95))
#spring max temperature
#peak
wa_mu_1 <- wa_mu[1, 1]
wa_lw_1 <- wa_mu[2, 1]
wa_up_1 <- wa_mu[3, 1]

#timing
wa_mu_7 <- wa_mu[1, 7]
wa_lw_7 <- wa_mu[2, 7]
wa_up_7 <- wa_mu[3, 7]

#length
wa_mu_8 <- wa_mu[1, 8]
wa_lw_8 <- wa_mu[2, 8]
wa_up_8 <- wa_mu[3, 8]

#winter precipitation
#peak
wa_mu_2 <- wa_mu[1, 2]
wa_lw_2 <- wa_mu[2, 2]
wa_up_2 <- wa_mu[3, 2]

#timing
wa_mu_9 <- wa_mu[1, 9]
wa_lw_9 <- wa_mu[2, 9]
wa_up_9 <- wa_mu[3, 9]

#length
wa_mu_10 <- wa_mu[1, 10]
wa_lw_10 <- wa_mu[2, 10]
wa_up_10 <- wa_mu[3, 10]

#spring min temperature
#peak
wa_mu_3 <- wa_mu[1, 3]
wa_lw_3 <- wa_mu[2, 3]
wa_up_3 <- wa_mu[3, 3]

#timing
wa_mu_11 <- wa_mu[1, 11]
wa_lw_11 <- wa_mu[2, 11]
wa_up_11 <- wa_mu[3, 11]

#length
wa_mu_12 <- wa_mu[1, 12]
wa_lw_12 <- wa_mu[2, 12]
wa_up_12 <- wa_mu[3, 12]




#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
wb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5,0.05,0.95))
cc1_b1<-as.numeric(wb1[2,] > 0 | wb1[3,] < 0)+1

#timing
wb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5,0.05,0.95))
cc1_b7<-as.numeric(wb7[2,] > 0 | wb7[3,] < 0)+1

#length
wb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5,0.05,0.95))
cc1_b8<-as.numeric(wb8[2,] > 0 | wb8[3,] < 0)+1

#winter precipitation
#peak
wb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5,0.05,0.95))
cc1_b2<-as.numeric(wb2[2,] > 0 | wb2[3,] < 0)+1

#timing
wb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5,0.05,0.95))
cc1_b9<-as.numeric(wb9[2,] > 0 | wb9[3,] < 0)+1

#length
wb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5,0.05,0.95))
cc1_b10<-as.numeric(wb10[2,] > 0 | wb10[3,] < 0)+1

#spring min temperature
#peak
wb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5,0.05,0.95))
cc1_b3<-as.numeric(wb3[2,] > 0 | wb3[3,] < 0)+1

#timing
wb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5,0.05,0.95))
cc1_b11<-as.numeric(wb11[2,] > 0 | wb11[3,] < 0)+1

#length
wb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5,0.05,0.95))
cc1_b12<-as.numeric(wb12[2,] > 0 | wb12[3,] < 0)+1


#to know the values (effect(confidence interval)) for writing
#paste(round(cp_mu_12, 2),"(",round(cp_lw_12, 2),"-",round(cp_up_12, 2),")",",",
 #     round(dp_mu_12, 2),"(",round(dp_lw_12, 2),"-",round(dp_up_12, 2),")",",",
  #    round(lc_mu_12, 2),"(",round(lc_lw_12, 2),"-",round(lc_up_12, 2),")",",",
   #   round(sv_mu_12, 2),"(",round(sv_lw_12, 2),"-",round(sv_up_12, 2),")","and",
    #  round(wa_mu_12, 2),"(",round(wa_lw_12, 2),"-",round(wa_up_12, 2),")")



pdf(paste("SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_DOT_PLOT.pdf",sep=""),width=28,height=18)
bps <- 4.8
sps <- 3.5
axi <- 2.8
tis <- 2.5
xlabs <- 2.2
layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=T))

par(mgp= c(0,-0.1,0), oma=c(1,0.3,0,0.3), mar=c(2, 1, 1, 1))
# Prepare data for dot chart
#concatenate all and convert them from their matrix forms to vectors

#spring max temperature
#peak
mus<-as.vector(c(wa_mu_1, sv_mu_1, lc_mu_1, dp_mu_1, cp_mu_1))
lower<-as.vector(c(wa_lw_1, sv_lw_1, lc_lw_1, dp_lw_1, cp_lw_1))
upper<-as.vector(c(wa_up_1, sv_up_1, lc_up_1, dp_up_1, cp_up_1))

#create x-axis limit
gd2<-c(cb1[1,], db1[1,], lb1[1,], sb1[1,], wb1[1,])

# Create the dot chart
dotchart(mus, pch = 19, xlim = c(min(gd2), max(gd2)), col = "black", cex = bps, xaxt='n')

#add x-axis
axis(1, tick = T, line = 0, las = 0, cex.axis = axi, tcl = -0.2)

#  x-axis label
mtext("Regression coefficient", side = 1, line =0.5, cex = xlabs)

# Add error bars
for (i in 1:length(mus)) {
  arrows(
    y0 = i,
    x0 = lower[i],
    y1 = i,
    x1 = upper[i],
    angle = 180,
    code = 3,
    length = 0.05,
    col = "darkorange"
  )
}

# Add y-axis labels
axis(2, at = 1:length(mus), labels = c("WA      ", "SV      ", "LC      ", "DP      ", "CP      "), las = 1, cex.axis = axi, tcl = -0.2)
# Vertical line at point 0 on the x-axis
abline(v = 0, col = "grey", lty = 1)

points(wb1[1,],jitter(rep(1,wsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc1_b1],pch=19, cex= sps)
points(sb1[1,],jitter(rep(2,ssp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc2_b1],pch=19, cex= sps)
points(lb1[1,],jitter(rep(3,lsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc3_b1],pch=19, cex= sps)
points(db1[1,],jitter(rep(4,dsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc4_b1],pch=19, cex= sps)
points(cb1[1,],jitter(rep(5,csp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc5_b1],pch=19, cex= sps)

#title(main = "(c) Effect of spring max temperature on peak", cex.main = tis)
mtext("(a) Spring max temperature (MO)", side = 3, line = 0.3, adj = 0, cex = tis)



#winter precipitation
#peak
mus<-as.vector(c(wa_mu_2, sv_mu_2, lc_mu_2, dp_mu_2, cp_mu_2))
lower<-as.vector(c(wa_lw_2, sv_lw_2, lc_lw_2, dp_lw_2, cp_lw_2))
upper<-as.vector(c(wa_up_2, sv_up_2, lc_up_2, dp_up_2, cp_up_2))

#create x-axis limit
gd2<-c(cb2[1,], db2[1,], lb2[1,], sb2[1,], wb2[1,])

# Create the dot chart
dotchart(mus, pch = 19, xlim = c(min(gd2), max(gd2)), col = "black", cex = bps, xaxt='n')

#add x-axis
axis(1, tick = T, line = 0, las = 0, cex.axis = axi, tcl = -0.2)

#  x-axis label
mtext("Regression coefficient", side = 1, line =0.5, cex = xlabs)

# Add error bars
for (i in 1:length(mus)) {
  arrows(
    y0 = i,
    x0 = lower[i],
    y1 = i,
    x1 = upper[i],
    angle = 180,
    code = 3,
    length = 0.05,
    col = "darkorange"
  )
}

# Add y-axis labels
axis(2, at = 1:length(mus), labels = c("WA      ", "SV      ", "LC      ", "DP      ", "CP      "), las = 1, cex.axis = axi, tcl = -0.2)
# Vertical line at point 0 on the x-axis
abline(v = 0, col = "grey", lty = 1)

points(wb2[1,],jitter(rep(1,wsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc1_b2],pch=19, cex= sps)
points(sb2[1,],jitter(rep(2,ssp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc2_b2],pch=19, cex= sps)
points(lb2[1,],jitter(rep(3,lsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc3_b2],pch=19, cex= sps)
points(db2[1,],jitter(rep(4,dsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc4_b2],pch=19, cex= sps)
points(cb2[1,],jitter(rep(5,csp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc5_b2],pch=19, cex= sps)

#title(main = "(f) Effect of winter precipitation on peak", cex.main = tis)
mtext("(b) Winter precipitation (MO)", side = 3, line = 0.3, adj = 0, cex = tis)



#spring min temperature
#peak
mus<-as.vector(c(wa_mu_3, sv_mu_3, lc_mu_3, dp_mu_3, cp_mu_3))
lower<-as.vector(c(wa_lw_3, sv_lw_3, lc_lw_3, dp_lw_3, cp_lw_3))
upper<-as.vector(c(wa_up_3, sv_up_3, lc_up_3, dp_up_3, cp_up_3))

#create x-axis limit
gd2<-c(cb3[1,], db3[1,], lb3[1,], sb3[1,], wb3[1,])

# Create the dot chart
dotchart(mus, pch = 19, xlim = c(min(gd2), max(gd2)), col = "black", cex = bps, xaxt='n')

#add x-axis
axis(1, tick = T, line = 0, las = 0, cex.axis = axi, tcl = -0.2)

#  x-axis label
mtext("Regression coefficient", side = 1, line =0.5, cex = xlabs)

# Add error bars
for (i in 1:length(mus)) {
  arrows(
    y0 = i,
    x0 = lower[i],
    y1 = i,
    x1 = upper[i],
    angle = 180,
    code = 3,
    length = 0.05,
    col = "darkorange"
  )
}

# Add y-axis labels
axis(2, at = 1:length(mus), labels = c("WA      ", "SV      ", "LC      ", "DP      ", "CP      "), las = 1, cex.axis = axi, tcl = -0.2)
# Vertical line at point 0 on the x-axis
abline(v = 0, col = "grey", lty = 1)

points(wb3[1,],jitter(rep(1,wsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc1_b3],pch=19, cex= sps)
points(sb3[1,],jitter(rep(2,ssp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc2_b3],pch=19, cex= sps)
points(lb3[1,],jitter(rep(3,lsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc3_b3],pch=19, cex= sps)
points(db3[1,],jitter(rep(4,dsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc4_b3],pch=19, cex= sps)
points(cb3[1,],jitter(rep(5,csp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc5_b3],pch=19, cex= sps)

#title(main = "(i) Effect of spring min temperature on peak", cex.main = tis)
mtext("(c) Spring min temperature (MO)", side = 3, line = 0.3, adj = 0, cex = tis)


#spring max temperature
#timing
mus<-as.vector(c(wa_mu_7, sv_mu_7, lc_mu_7, dp_mu_7, cp_mu_7))
lower<-as.vector(c(wa_lw_7, sv_lw_7, lc_lw_7, dp_lw_7, cp_lw_7))
upper<-as.vector(c(wa_up_7, sv_up_7, lc_up_7, dp_up_7, cp_up_7))

#create x-axis limit
gd2<-c(cb7[1,], db7[1,], lb7[1,], sb7[1,], wb7[1,])

# Create the dot chart
dotchart(mus, pch = 19, xlim = c(min(gd2), max(gd2)), col = "black", cex = bps, xaxt='n')

#add x-axis
axis(1, tick = T, line = 0, las = 0, cex.axis = axi, tcl = -0.2)

#  x-axis label
mtext("Regression coefficient", side = 1, line =0.5, cex = xlabs)

# Add error bars
for (i in 1:length(mus)) {
  arrows(
    y0 = i,
    x0 = lower[i],
    y1 = i,
    x1 = upper[i],
    angle = 180,
    code = 3,
    length = 0.05,
    col = "darkorange"
  )
}

# Add y-axis labels
axis(2, at = 1:length(mus), labels = c("WA      ", "SV      ", "LC      ", "DP      ", "CP      "), las = 1, cex.axis = axi, tcl = -0.2)
# Vertical line at point 0 on the x-axis
abline(v = 0, col = "grey", lty = 1)

points(wb7[1,],jitter(rep(1,wsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc1_b7],pch=19, cex= sps)
points(sb7[1,],jitter(rep(2,ssp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc2_b7],pch=19, cex= sps)
points(lb7[1,],jitter(rep(3,lsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc3_b7],pch=19, cex= sps)
points(db7[1,],jitter(rep(4,dsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc4_b7],pch=19, cex= sps)
points(cb7[1,],jitter(rep(5,csp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc5_b7],pch=19, cex= sps)

#title(main = "(a) Effect of spring max temperature on timing", cex.main = tis)
mtext("(d) Spring max temperature (TO)", side = 3, line = 0.3, adj = 0, cex = tis)


#winter precipitation
#timing
mus<-as.vector(c(wa_mu_9, sv_mu_9, lc_mu_9, dp_mu_9, cp_mu_9))
lower<-as.vector(c(wa_lw_9, sv_lw_9, lc_lw_9, dp_lw_9, cp_lw_9))
upper<-as.vector(c(wa_up_9, sv_up_9, lc_up_9, dp_up_9, cp_up_9))

#create x-axis limit
pd<- c(cb9[1,], db9[1,], lb9[1,], sb9[1,], wb9[1,])

# Create the dot chart
dotchart(mus, pch = 19, xlim = c(min(pd), max(pd)), col = "black", cex = bps, xaxt='n')

#add x-axis
axis(1, tick = T, line = 0, las = 0, cex.axis = axi, tcl = -0.2)

# x-axis label
mtext("Regression coefficient", side = 1, line =0.5, cex = xlabs)

# Add error bars
for (i in 1:length(mus)) {
  arrows(
    y0 = i,
    x0 = lower[i],
    y1 = i,
    x1 = upper[i],
    angle = 180,
    code = 3,
    length = 0.05,
    col = "darkorange"
  )
}

# Add y-axis labels
axis(2, at = 1:length(mus), labels = c("WA      ", "SV      ", "LC      ", "DP      ", "CP      "), las = 1, cex.axis = axi, tcl = -0.2)
# Draw a vertical line at point 0 on the x-axis
abline(v = 0, col = "grey", lty = 1)

points(wb9[1,],jitter(rep(1,wsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc1_b9],pch=19, cex= sps)
points(sb9[1,],jitter(rep(2,ssp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc2_b9],pch=19, cex= sps)
points(lb9[1,],jitter(rep(3,lsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc3_b9],pch=19, cex= sps)
points(db9[1,],jitter(rep(4,dsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc4_b9],pch=19, cex= sps)
points(cb9[1,],jitter(rep(5,csp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc5_b9],pch=19, cex= sps)

#title(main = "(d) Effect of winter precipitation on timing", cex.main = tis)
mtext("(e) Winter precipitation (TO)", side = 3, line = 0.3, adj = 0, cex = tis)

#spring min temperature
#timing
mus<-as.vector(c(wa_mu_11, sv_mu_11, lc_mu_11, dp_mu_11, cp_mu_11))
lower<-as.vector(c(wa_lw_11, sv_lw_11, lc_lw_11, dp_lw_11, cp_lw_11))
upper<-as.vector(c(wa_up_11, sv_up_11, lc_up_11, dp_up_11, cp_up_11))

#create x-axis limit
gd2<-c(cb11[1,], db11[1,], lb11[1,], sb11[1,], wb11[1,])

# Create the dot chart
dotchart(mus, pch = 19, xlim = c(min(gd2), max(gd2)), col = "black", cex = bps, xaxt='n')

#add x-axis
axis(1, tick = T, line = 0, las = 0, cex.axis = axi, tcl = -0.2)

#  x-axis label
mtext("Regression coefficient", side = 1, line =0.5, cex = xlabs)

# Add error bars
for (i in 1:length(mus)) {
  arrows(
    y0 = i,
    x0 = lower[i],
    y1 = i,
    x1 = upper[i],
    angle = 180,
    code = 3,
    length = 0.05,
    col = "darkorange"
  )
}

# Add y-axis labels
axis(2, at = 1:length(mus), labels = c("WA      ", "SV      ", "LC      ", "DP      ", "CP      "), las = 1, cex.axis = axi, tcl = -0.2)
# Vertical line at point 0 on the x-axis
abline(v = 0, col = "grey", lty = 1)

points(wb11[1,],jitter(rep(1,wsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc1_b11],pch=19, cex= sps)
points(sb11[1,],jitter(rep(2,ssp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc2_b11],pch=19, cex= sps)
points(lb11[1,],jitter(rep(3,lsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc3_b11],pch=19, cex= sps)
points(db11[1,],jitter(rep(4,dsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc4_b11],pch=19, cex= sps)
points(cb11[1,],jitter(rep(5,csp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc5_b11],pch=19, cex= sps)

#title(main = "(g) Effect of spring min temperature on timing", cex.main = tis)
mtext("(f) Spring min temperature (TO)", side = 3, line = 0.3, adj = 0, cex = tis)



#length
mus<-as.vector(c(wa_mu_8, sv_mu_8, lc_mu_8, dp_mu_8, cp_mu_8))
lower<-as.vector(c(wa_lw_8, sv_lw_8, lc_lw_8, dp_lw_8, cp_lw_8))
upper<-as.vector(c(wa_up_8, sv_up_8, lc_up_8, dp_up_8, cp_up_8))

#create x-axis limit
gd2<-c(cb8[1,], db8[1,], lb8[1,], sb8[1,], wb8[1,])

# Create the dot chart
dotchart(mus, pch = 19, xlim = c(min(gd2), max(gd2)), col = "black", cex = bps, xaxt='n')

#add x-axis
axis(1, tick = T, line = 0, las = 0, cex.axis = axi, tcl = -0.2)

#  x-axis label
mtext("Regression coefficient", side = 1, line =0.5, cex = xlabs)

# Add error bars
for (i in 1:length(mus)) {
  arrows(
    y0 = i,
    x0 = lower[i],
    y1 = i,
    x1 = upper[i],
    angle = 180,
    code = 3,
    length = 0.05,
    col = "darkorange"
  )
}

# Add y-axis labels
axis(2, at = 1:length(mus), labels = c("WA      ", "SV      ", "LC      ", "DP      ", "CP      "), las = 1, cex.axis = axi, tcl = -0.2)
# Vertical line at point 0 on the x-axis
abline(v = 0, col = "grey", lty = 1)

points(wb8[1,],jitter(rep(1,wsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc1_b8],pch=19, cex= sps)
points(sb8[1,],jitter(rep(2,ssp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc2_b8],pch=19, cex= sps)
points(lb8[1,],jitter(rep(3,lsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc3_b8],pch=19, cex= sps)
points(db8[1,],jitter(rep(4,dsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc4_b8],pch=19, cex= sps)
points(cb8[1,],jitter(rep(5,csp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc5_b8],pch=19, cex= sps)

#title(main = "(b) Effect of spring max temperature on length", cex.main = tis)
mtext("(g) Spring max temperature (LO)", side = 3, line = 0.3, adj = 0, cex = tis)

#winter precipitation
#length
mus<-as.vector(c(wa_mu_10, sv_mu_10, lc_mu_10, dp_mu_10, cp_mu_10))
lower<-as.vector(c(wa_lw_10, sv_lw_10, lc_lw_10, dp_lw_10, cp_lw_10))
upper<-as.vector(c(wa_up_10, sv_up_10, lc_up_10, dp_up_10, cp_up_10))

#create x-axis limit
pd2<-c(cb10[1,], db10[1,], lb10[1,], sb10[1,], wb10[1,])

# Create the dot chart
dotchart(mus, pch = 19, xlim = c(min(pd2), max(pd2)), col = "black", cex = bps, xaxt='n')

#add x-axis
axis(1, tick = T, line = 0, las = 0, cex.axis = axi, tcl = -0.2)

# x-axis label
mtext("Regression coefficient", side = 1, line =0.5, cex = xlabs)

# Add error bars
for (i in 1:length(mus)) {
  arrows(
    y0 = i,
    x0 = lower[i],
    y1 = i,
    x1 = upper[i],
    angle = 180,
    code = 3,
    length = 0.05,
    col = "darkorange"
  )
}

# Add y-axis labels
axis(2, at = 1:length(mus), labels = c("WA      ", "SV      ", "LC      ", "DP      ", "CP      "), las = 1, cex.axis = axi, tcl = -0.2)
# Draw a vertical line at point 0 on the x-axis
abline(v = 0, col = "grey", lty = 1)


points(wb10[1,],jitter(rep(1,wsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc1_b10],pch=19, cex= sps)
points(sb10[1,],jitter(rep(2,ssp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc2_b10],pch=19, cex= sps)
points(lb10[1,],jitter(rep(3,lsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc3_b10],pch=19, cex= sps)
points(db10[1,],jitter(rep(4,dsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc4_b10],pch=19, cex= sps)
points(cb10[1,],jitter(rep(5,csp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc5_b10],pch=19, cex= sps)

#title(main = "(e) Effect of winter precipitation on length", cex.main = tis)
mtext("(h)  Winter precipitation (LO)", side = 3, line = 0.3, adj = 0, cex = tis)


#spring min temperature
#length
mus<-as.vector(c(wa_mu_12, sv_mu_12, lc_mu_12, dp_mu_12, cp_mu_12))
lower<-as.vector(c(wa_lw_12, sv_lw_12, lc_lw_12, dp_lw_12, cp_lw_12))
upper<-as.vector(c(wa_up_12, sv_up_12, lc_up_12, dp_up_12, cp_up_12))

#create x-axis limit
gd<-c(cb12[1,], db12[1,], lb12[1,], sb12[1,], wb12[1,])


# Create the dot chart
dotchart(mus, pch = 19, xlim = c(min(gd), max(gd)), col = "black", cex = bps, xaxt='n')


#add x-axis
axis(1, tick = T, line = 0, las = 0, cex.axis = axi, tcl = -0.2)

#  x-axis label
mtext("Regression coefficient", side = 1, line =0.5, cex = xlabs)

# Add error bars
for (i in 1:length(mus)) {
  arrows(
    y0 = i,
    x0 = lower[i],
    y1 = i,
    x1 = upper[i],
    angle = 180,
    code = 3,
    length = 0.05,
    col = "darkorange"
  )
}

# Add y-axis labels
axis(2, at = 1:length(mus), labels = c("WA      ", "SV      ", "LC      ", "DP      ", "CP      "), las = 1, cex.axis = axi, tcl = -0.2)
# Draw a vertical line at point 0 on the x-axis
abline(v = 0, col = "grey", lty = 1)


points(wb12[1,],jitter(rep(1,wsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc1_b12],pch=19, cex= sps)
points(sb12[1,],jitter(rep(2,ssp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc2_b12],pch=19, cex= sps)
points(lb12[1,],jitter(rep(3,lsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc3_b12],pch=19, cex= sps)
points(db12[1,],jitter(rep(4,dsp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc4_b12],pch=19, cex= sps)
points(cb12[1,],jitter(rep(5,csp), factor = 3),col=alpha(c("gray60","dodgerblue"),.3)[cc5_b12],pch=19, cex= sps)

#title(main = "(h) Effect of spring min temperature on length", cex.main = tis)
mtext("(i) Spring min temperature (LO)", side = 3, line = 0.3, adj = 0, cex = tis)

dev.off()

