library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#CASTLE PEAK
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Castle Peak.rdat')
csp= length(sp)
cssp<-sp

#beta 
betas<-extract(fit,"beta")
dim(betas[[1]])

#spring max temp
#MO, TO, LO
cb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5))
cb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5))
cb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5))

#winter precipitation
#MO, TO, LO
cb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5))
cb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5))
cb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5))

#spring min temp
#MO, TO, LO
cb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5))
cb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5))
cb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5))


#DONNER PASS
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Donner Pass.rdat')
dsp= length(sp)
dssp<-sp

#beta  
betas<-extract(fit,"beta")

#spring max temp
#MO, TO, LO
db1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5))
db7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5))
db8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5))

#winter precipitation
#MO, TO, LO
db2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5))
db9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5))
db10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5))

#spring min temp
#MO, TO, LO
db3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5))
db11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5))
db12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5))


#LANG CROSSING
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Lang Crossing.rdat')
lsp= length(sp)
lssp<-sp

#beta 
betas<-extract(fit,"beta")

#spring max temp
#MO, TO, LO
lb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5))
lb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5))
lb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5))
#winter precipitation
#MO, TO, LO
lb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5))
lb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5))
lb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5))

#spring min temp
#MO, TO, LO
lb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5))
lb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5))
lb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5))


#SIERRA VALLEY
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Sierra Valley.rdat')
ssp= length(sp)
sssp<-sp

#beta 
betas<-extract(fit,"beta")

#spring max temp
#MO, TO, LO
sb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5))
sb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5))
sb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5))

#winter precipitation
#MO, TO, LO
sb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5))
sb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5))
sb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5))

#spring min temp
#MO, TO, LO
sb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5))
sb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5))
sb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5))

#WASHINGTON
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Washington.rdat')
wsp= length(sp)
wssp<-sp

#beta 
betas<-extract(fit,"beta")

#spring max temp
#MO, TO, LO
wb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5))
wb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5))
wb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5))

#winter precipitation
#MO, TO, LO
wb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5))
wb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5))
wb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5))

#spring min temp
#MO, TO, LO
wb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5))
wb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5))
wb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5))




# Correlation between the effect of spring maximum temperature on MO and TO
# Sites ordered according to inset map for plotting: "WA", "LC", "DP", "CP", "SV"
correl_37 <- cor.test(wb1, wb7) 
correl_19 <- cor.test(lb1, lb7) 
correl_10 <- cor.test(db1, db7) 
correl_1 <- cor.test(cb1, cb7) 
correl_28 <- cor.test(sb1, sb7)
spma_mo_to <- c(correl_37$estimate,correl_19$estimate,correl_10$estimate,
                correl_1$estimate,correl_28$estimate) 
#lower confidence interval
spma_mo_to_lw <- c(correl_37$conf.int[1],correl_19$conf.int[1],correl_10$conf.int[1],
                   correl_1$conf.int[1],correl_28$conf.int[1]) 
#upper confidence interval
spma_mo_to_up <- c(correl_37$conf.int[2],correl_19$conf.int[2],correl_10$conf.int[2],
                   correl_1$conf.int[2],correl_28$conf.int[2]) 

# Correlation between the effect of spring maximum temperature on MO and LO
# Sites ordered according to inset map for plotting: "WA", "LC", "DP", "CP", "SV"
correl_38 <- cor.test(wb1, wb8) 
correl_20 <- cor.test(lb1, lb8)
correl_11 <- cor.test(db1, db8)
correl_2 <- cor.test(cb1, cb8)
correl_29 <- cor.test(sb1, sb8) 
spma_mo_lo <- c(correl_38$estimate,correl_20$estimate,correl_11$estimate,
                correl_2$estimate,correl_29$estimate) 
#lower confidence interval
spma_mo_lo_lw <- c(correl_38$conf.int[1],correl_20$conf.int[1],correl_11$conf.int[1],
                   correl_2$conf.int[1],correl_29$conf.int[1]) 
#upper confidence interval
spma_mo_lo_up <- c(correl_38$conf.int[2],correl_20$conf.int[2],correl_11$conf.int[2],
                   correl_2$conf.int[2],correl_29$conf.int[2])

# Correlation between the effect of spring maximum temperature on TO and LO
# Sites ordered according to inset map for plotting: "WA", "LC", "DP", "CP", "SV"
correl_39 <- cor.test(wb7, wb8)
correl_21 <- cor.test(lb7, lb8)
correl_12 <- cor.test(db7, db8) 
correl_3 <- cor.test(cb7, cb8) 
correl_30 <- cor.test(sb7, sb8)
spma_to_lo <- c(correl_39$estimate,correl_21$estimate,correl_12$estimate,
                correl_3$estimate,correl_30$estimate)
#lower confidence interval
spma_to_lo_lw <- c(correl_39$conf.int[1],correl_21$conf.int[1],correl_12$conf.int[1],
                   correl_3$conf.int[1],correl_30$conf.int[1])
#upper confidence interval
spma_to_lo_up <- c(correl_39$conf.int[2],correl_21$conf.int[2],correl_12$conf.int[2],
                   correl_3$conf.int[2],correl_30$conf.int[2])

# Correlation between the effect of winter precipitation on MO and TO
# Sites ordered according to inset map for plotting: "WA", "LC", "DP", "CP", "SV"
correl_40 <- cor.test(wb2, wb9) 
correl_22 <- cor.test(lb2, lb9) 
correl_13 <- cor.test(db2, db9) 
correl_4 <- cor.test(cb2, cb9) 
correl_31 <- cor.test(sb2, sb9) 
wipr_mo_to <- c(correl_40$estimate,correl_22$estimate,correl_13$estimate,
                correl_4$estimate,correl_31$estimate) 
#lower confidence interval
wipr_mo_to_lw <- c(correl_40$conf.int[1],correl_22$conf.int[1],correl_13$conf.int[1],
                   correl_4$conf.int[1],correl_31$conf.int[1]) 
#upper confidence interval
wipr_mo_to_up <- c(correl_40$conf.int[2],correl_22$conf.int[2],correl_13$conf.int[2],
                   correl_4$conf.int[2],correl_31$conf.int[2]) 

# Correlation between the effect of winter precipitation on MO and LO
# Sites ordered according to inset map for plotting: "WA", "LC", "DP", "CP", "SV"
correl_41 <- cor.test(wb2, wb10) 
correl_23 <- cor.test(lb2, lb10) 
correl_14 <- cor.test(db2, db10) 
correl_5 <- cor.test(cb2, cb10) 
correl_32 <- cor.test(sb2, sb10) 
wipr_mo_lo <- c(correl_41$estimate,correl_23$estimate,correl_14$estimate,
                correl_5$estimate,correl_32$estimate) 
#lower confidence interval
wipr_mo_lo_lw <- c(correl_41$conf.int[1],correl_23$conf.int[1],correl_14$conf.int[1],
                   correl_5$conf.int[1],correl_32$conf.int[1]) 
#upper confidence interval
wipr_mo_lo_up <- c(correl_41$conf.int[2],correl_23$conf.int[2],correl_14$conf.int[2],
                   correl_5$conf.int[2],correl_32$conf.int[2]) 

# Correlation between the effect of winter precipitation on TO and LO
# Sites ordered according to inset map for plotting: "WA", "LC", "DP", "CP", "SV"
correl_42 <- cor.test(wb9, wb10) 
correl_24 <- cor.test(lb9, lb10)
correl_15 <- cor.test(db9, db10)
correl_6 <- cor.test(cb9, cb10)
correl_33 <- cor.test(sb9, sb10) 
wipr_to_lo <- c(correl_42$estimate,correl_24$estimate,correl_15$estimate,
                correl_6$estimate,correl_33$estimate)
#lower confidence interval
wipr_to_lo_lw <- c(correl_42$conf.int[1],correl_24$conf.int[1],correl_15$conf.int[1],
                   correl_6$conf.int[1],correl_33$conf.int[1])
#upper confidence interval
wipr_to_lo_up <- c(correl_42$conf.int[2],correl_24$conf.int[2],correl_15$conf.int[2],
                   correl_6$conf.int[2],correl_33$conf.int[2])

# Correlation between the effect of spring minimum temperature on MO and TO
# Sites ordered according to inset map for plotting: "WA", "LC", "DP", "CP", "SV"
correl_43 <- cor.test(wb3, wb11) 
correl_25 <- cor.test(lb3, lb11) 
correl_16 <- cor.test(db3, db11) 
correl_7 <- cor.test(cb3, cb11) 
correl_34 <- cor.test(sb3, sb11) 
spmi_mo_to <- c(correl_43$estimate,correl_25$estimate,correl_16$estimate,
                correl_7$estimate,correl_34$estimate) 
#lower confidence interval
spmi_mo_to_lw <- c(correl_43$conf.int[1],correl_25$conf.int[1],correl_16$conf.int[1],
                   correl_7$conf.int[1],correl_34$conf.int[1]) 
#upper confidence interval
spmi_mo_to_up <- c(correl_43$conf.int[2],correl_25$conf.int[2],correl_16$conf.int[2],
                   correl_7$conf.int[2],correl_34$conf.int[2])

# Correlation between the effect of spring minimum temperature on MO and LO
# Sites ordered according to inset map for plotting: "WA", "LC", "DP", "CP", "SV"
correl_44 <- cor.test(wb3, wb12) 
correl_26 <- cor.test(lb3, lb12) 
correl_17 <- cor.test(db3, db12) 
correl_8 <- cor.test(cb3, cb12) 
correl_35 <- cor.test(sb3, sb12) 
spmi_mo_lo <- c(correl_44$estimate,correl_26$estimate,correl_17$estimate,
                correl_8$estimate,correl_35$estimate) 
#lower confidence interval
spmi_mo_lo_lw <- c(correl_44$conf.int[1],correl_26$conf.int[1],correl_17$conf.int[1],
                   correl_8$conf.int[1],correl_35$conf.int[1]) 
#upper confidence interval
spmi_mo_lo_up <- c(correl_44$conf.int[2],correl_26$conf.int[2],correl_17$conf.int[2],
                   correl_8$conf.int[2],correl_35$conf.int[2])

# Correlation between the effect of spring minimum temperature on TO and LO
# Sites ordered according to inset map for plotting: "WA", "LC", "DP", "CP", "SV"
correl_45 <- cor.test(wb11, wb12) 
correl_27 <- cor.test(lb11, lb12) 
correl_18 <- cor.test(db11, db12) 
correl_9 <- cor.test(cb11, cb12) 
correl_36 <- cor.test(sb11, sb12) 
spmi_to_lo <- c(correl_45$estimate,correl_27$estimate,correl_18$estimate,
                correl_9$estimate,correl_36$estimate)
#lower confidence interval
spmi_to_lo_lw <- c(correl_45$conf.int[1],correl_27$conf.int[1],correl_18$conf.int[1],
                   correl_9$conf.int[1],correl_36$conf.int[1])
#upper confidence interval
spmi_to_lo_up <- c(correl_45$conf.int[2],correl_27$conf.int[2],correl_18$conf.int[2],
                   correl_9$conf.int[2],correl_36$conf.int[2])




pdf(paste("correlation_in_effect_of_climate_on_different_properties_of_phenology_curve_summary.pdf",sep=""),width=13,height=13)
layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=T))
par(oma = c(1, 1, 1, 1), mar = c(5, 5, 5, 6), mgp = c(3.5, 1.5, 0))
sites <- c("WA", "LC",  "DP", "CP", "SV")
cols <- c("#57575f", "#849fad", "#276478", "#ca3542", "#FAAC77")

plot(spma_mo_to, type = "n", xlab = "", ylab = "",  xaxt = "n", 
     ylim = c(-max(c(spma_mo_to_up,spma_mo_to_lw)), max(c(spma_mo_to_up,spma_mo_to_lw))), cex.axis = 2.5)
points(spma_mo_to, cex=3.5, col=cols, pch=19)
# error bars
arrows(x0 = 1:length(spma_mo_to), y0 = spma_mo_to_lw, y1 = spma_mo_to_up, length = 0.05, angle = 90, code = 3, lwd = 2)
# x-axis with site names as labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 2.0)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
mtext("(a) Spring max temperature: MO and TO", side = 3, line = 1.2, adj = 0.5, cex = 1.4) 
title(xlab = "Sites", cex.lab=2.0) 
title(ylab = "r", cex.lab=3.0)

plot(wipr_mo_to, type = "n", xlab = "", ylab = "",  xaxt = "n", 
     ylim = c(-max(c(wipr_mo_to_up,wipr_mo_to_lw)),
              max(c(wipr_mo_to_up,wipr_mo_to_lw))), cex.axis = 2.5)
points(wipr_mo_to, cex=3.5, col=cols, pch=19)
# error bars
arrows(x0 = 1:length(wipr_mo_to), y0 = wipr_mo_to_lw, y1 = wipr_mo_to_up, length = 0.05, angle = 90, code = 3, lwd = 2)
# x-axis with site names as labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 2.0)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
mtext("(b) Winter precipitation: MO and TO", side = 3, line = 1.2, adj = 0.5, cex = 1.4)
title(xlab = "Sites", cex.lab=2.0) 
title(ylab = "r", cex.lab=3.0)


plot(spmi_mo_to, type = "n", xlab = "", ylab = "",  xaxt = "n", 
     ylim = c(min(c(spmi_mo_to_up,spmi_mo_to_lw)), -min(c(spmi_mo_to_up,spmi_mo_to_lw))), cex.axis = 2.5)
points(spmi_mo_to, cex=3.5, col=cols, pch=19)
# error bars
arrows(x0 = 1:length(spmi_mo_to), y0 = spmi_mo_to_lw, y1 = spmi_mo_to_up, length = 0.05, angle = 90, code = 3, lwd = 2)
# x-axis with site names as labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 2.0)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
mtext("(c) Spring min temperature: MO and TO", side = 3, line = 1.2, adj = 0.5, cex = 1.4)
title(xlab = "Sites", cex.lab=2.0) 
title(ylab = "r", cex.lab=3.0)


plot(spma_mo_lo, type = "n", xlab = "", ylab = "",  xaxt = "n", 
     ylim = c(min(c(spma_mo_lo_up,spma_mo_lo_lw)), -min(c(spma_mo_lo_up,spma_mo_lo_lw))), cex.axis = 2.5)
points(spma_mo_lo, cex=3.5, col=cols, pch=19)
# error bars
arrows(x0 = 1:length(spma_mo_lo), y0 = spma_mo_lo_lw, y1 = spma_mo_lo_up, length = 0.05, angle = 90, code = 3, lwd = 2)
# x-axis with site names as labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 2.0)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
mtext("(d) Spring max temperature: MO and LO", side = 3, line = 1.2, adj = 0.5, cex = 1.4)
title(xlab = "Sites", cex.lab=2.0) 
title(ylab = "r", cex.lab=3.0)


plot(wipr_mo_lo, type = "n", xlab = "", ylab = "",  xaxt = "n", 
     ylim = c(min(c(wipr_mo_lo_up,wipr_mo_lo_lw)), -min(c(wipr_mo_lo_up,wipr_mo_lo_lw))), cex.axis = 2.5)
points(wipr_mo_lo, cex=3.5, col=cols, pch=19)
# error bars
arrows(x0 = 1:length(wipr_mo_lo), y0 = wipr_mo_lo_lw, y1 = wipr_mo_lo_up, length = 0.05, angle = 90, code = 3, lwd = 2)
# x-axis with site names as labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 2.0)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
mtext("(e) Winter precipitation: MO and LO", side = 3, line = 1.2, adj = 0.5, cex = 1.4)
title(xlab = "Sites", cex.lab=2.0) 
title(ylab = "r", cex.lab=3.0)


plot(spmi_mo_lo, type = "n", xlab = "", ylab = "",  xaxt = "n", 
     ylim = c(min(c(spmi_mo_lo_up,spmi_mo_lo_lw)), -min(c(spmi_mo_lo_up,spmi_mo_lo_lw))), cex.axis = 2.5)
points(spmi_mo_lo, cex=3.5, col=cols, pch=19)
# error bars
arrows(x0 = 1:length(spmi_mo_lo), y0 = spmi_mo_lo_lw, y1 = spmi_mo_lo_up, length = 0.05, angle = 90, code = 3, lwd = 2)
# x-axis with site names as labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 2.0)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
mtext("(f) Spring min temperature: MO and LO", side = 3, line = 1.2, adj = 0.5, cex = 1.4)
title(xlab = "Sites", cex.lab=2.0) 
title(ylab = "r", cex.lab=3.0)

plot(spma_to_lo, type = "n", xlab = "", ylab = "",  xaxt = "n", 
     ylim = c(-max(c(spma_to_lo_up,spma_to_lo_lw)), max(c(spma_to_lo_up,spma_to_lo_lw))), cex.axis = 2.5)
points(spma_to_lo, cex=3.5, col=cols, pch=19)
# error bars
arrows(x0 = 1:length(spma_to_lo), y0 = spma_to_lo_lw, y1 = spma_to_lo_up, length = 0.05, angle = 90, code = 3, lwd = 2)
# x-axis with site names as labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 2.0)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
mtext("(g) Spring max temperature: TO and LO", side = 3, line = 1.2, adj = 0.5, cex = 1.4)
title(xlab = "Sites", cex.lab=2.0) 
title(ylab = "r", cex.lab=3.0)


plot(wipr_to_lo, type = "n", xlab = "", ylab = "",  xaxt = "n", 
     ylim = c(-max(c(wipr_to_lo_up,wipr_to_lo_lw)), max(c(wipr_to_lo_up,wipr_to_lo_lw))), cex.axis = 2.5)
points(wipr_to_lo, cex=3.5, col=cols, pch=19)
# error bars
arrows(x0 = 1:length(wipr_to_lo), y0 = wipr_to_lo_lw, y1 = wipr_to_lo_up, length = 0.05, angle = 90, code = 3, lwd = 2)
# x-axis with site names as labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 2.0)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
mtext("(h) Winter precipitation: TO and LO", side = 3, line = 1.2, adj = 0.5, cex = 1.4)
title(xlab = "Sites", cex.lab=2.0) 
title(ylab = "r", cex.lab=3.0)


plot(spmi_to_lo, type = "n", xlab = "", ylab = "",  xaxt = "n", 
     ylim = c(min(c(spmi_to_lo_up,spmi_to_lo_lw)), -min(c(spmi_to_lo_up,spmi_to_lo_lw))), cex.axis = 2.5)
points(spmi_to_lo, cex=3.5, col=cols, pch=19)
# error bars
arrows(x0 = 1:length(spmi_to_lo), y0 = spmi_to_lo_lw, y1 = spmi_to_lo_up, length = 0.05, angle = 90, code = 3, lwd = 2)
# x-axis with site names as labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 2.0)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
mtext("(i) Spring min temperature: TO and LO", side = 3, line = 1.2, adj = 0.5, cex = 1.4)
title(xlab = "Sites", cex.lab=2.0)
title(ylab = "r", cex.lab=3.0)

dev.off()

