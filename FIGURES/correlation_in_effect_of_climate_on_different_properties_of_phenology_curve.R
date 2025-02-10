library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())




#CASTLE PEAK
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Castle Peak.rdat')
csp= length(sp)
cssp<-sp

#beta of posteriors
betas<-extract(fit,"beta")
dim(betas[[1]])

#spring max temp
#peak, timing , length
cb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5))
cb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5))
cb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5))

#winter precipitation
#peak, timing , length
cb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5))
cb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5))
cb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5))

#spring min temp
#peak, timing , length
cb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5))
cb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5))
cb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5))


#DONNER PASS
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Donner Pass.rdat')
dsp= length(sp)
dssp<-sp


#beta of posteriors
betas<-extract(fit,"beta")
#spring max temp
#peak, timing , length
db1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5))
db7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5))
db8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5))

#winter precipitation
#peak, timing , length
db2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5))
db9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5))
db10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5))

#spring min temp
#peak, timing , length
db3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5))
db11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5))
db12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5))


#LANG CROSSING
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Lang Crossing.rdat')
lsp= length(sp)
lssp<-sp



#beta of posteriors
betas<-extract(fit,"beta")
#spring max temp
#peak, timing , length
lb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5))
lb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5))
lb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5))
#winter precipitation
#peak, timing , length
lb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5))
lb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5))
lb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5))

#spring min temp
#peak, timing , length
lb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5))
lb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5))
lb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5))


#SIERRA VALLEY
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Sierra Valley.rdat')
ssp= length(sp)
sssp<-sp


#beta of posteriors
betas<-extract(fit,"beta")
#spring max temp
#peak, timing , length
sb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5))
sb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5))
sb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5))

#winter precipitation
#peak, timing , length
sb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5))
sb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5))
sb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5))

#spring min temp
#peak, timing , length
sb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5))
sb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5))
sb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5))

#WASHINGTON
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Washington.rdat')
wsp= length(sp)
wssp<-sp


#beta of posteriors
betas<-extract(fit,"beta")
#spring max temp
#peak, timing , length
wb1<-apply(betas[[1]][,,1],2,quantile,probs=c(.5))
wb7<-apply(betas[[1]][,,7],2,quantile,probs=c(.5))
wb8<-apply(betas[[1]][,,8],2,quantile,probs=c(.5))
#winter precipitation
#peak, timing , length
wb2<-apply(betas[[1]][,,2],2,quantile,probs=c(.5))
wb9<-apply(betas[[1]][,,9],2,quantile,probs=c(.5))
wb10<-apply(betas[[1]][,,10],2,quantile,probs=c(.5))
#spring min temp
#peak, timing , length
wb3<-apply(betas[[1]][,,3],2,quantile,probs=c(.5))
wb11<-apply(betas[[1]][,,11],2,quantile,probs=c(.5))
wb12<-apply(betas[[1]][,,12],2,quantile,probs=c(.5))




pdf(paste("correlation_in_effect_of_climate_on_different_properties_of_phenology_curve_castle_peak.pdf",sep=""),width=12,height=10)

layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=T))

#par(mar=c(3, 2, 1, 2))

#Castle peak
#Spring max temp-Peak-Timing of season
plot(cb1, cb7, pch=19, main= "Spring max temp-Peak-Timing of season",  
     type = "p", xlab="Spring max temp-Peak of season",
     ylab="Spring max temp-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_1 <- cor.test(cb1, cb7) 
sub_title <- paste("Correlation: ", round(correl_1$estimate, 2),
                   "(",round(correl_1$conf.int[1], 2),"-",round(correl_1$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring max temp-Peak-Length of season
plot(cb1, cb8, pch=19, main= "Spring max temp-Peak-Length of season",
     type = "p", xlab="Spring max temp-peak of season", 
     ylab="Spring max temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_2 <- cor.test(cb1, cb8) 
sub_title <- paste("correlation: ", round(correl_2$estimate, 2),
                   "(",round(correl_2$conf.int[1], 2),"-",round(correl_2$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring max temp-Timing-Length of season
plot(cb7, cb8, pch=19, main= "Spring max temp-Timing-Length of season",
     type = "p", xlab="Spring max temp-timing of season", 
     ylab="Spring max temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_3 <- cor.test(cb7, cb8) 
sub_title <- paste("correlation: ", round(correl_3$estimate, 2),
                   "(",round(correl_3$conf.int[1], 2),"-",round(correl_3$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")



#winter precipitation-Peak-Timing of season
plot(cb2, cb9, pch=19, main= "Winter precipitation-Peak-Timing of season",  
     type = "p", xlab="Winter precipitation-peak of season",
     ylab="Winter precipitation-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_4 <- cor.test(cb2, cb9) 
sub_title <- paste("Correlation: ", round(correl_4$estimate, 2),
                   "(",round(correl_4$conf.int[1], 2),"-",round(correl_4$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Winter precipitation-Peak-Length of season
plot(cb2, cb10, pch=19, main= "Winter precipitation-Peak-Length of season",
     type = "p", xlab="Winter precipitation-peak of season", 
     ylab="Winter precipitation-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_5 <- cor.test(cb2, cb10) 
sub_title <- paste("correlation: ", round(correl_5$estimate, 2),
                   "(",round(correl_5$conf.int[1], 2),"-",round(correl_5$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Winter precipitation-Timing-Length of season
plot(cb9, cb10, pch=19, main= "Winter precipitation-Timing-Length of season",
     type = "p", xlab="Winter precipitation-timing of season", 
     ylab="Winter precipitation-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_6 <- cor.test(cb9, cb10) 
sub_title <- paste("correlation: ", round(correl_6$estimate, 2),
                   "(",round(correl_6$conf.int[1], 2),"-",round(correl_6$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")


#Spring min temp-Peak-Timing of season
plot(cb3, cb11, pch=19, main= "Spring min temp-Peak-Timing of season",  
     type = "p", xlab="Spring min temp-peak of season",
     ylab="Spring min temp-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_7 <- cor.test(cb3, cb11) 
sub_title <- paste("Correlation: ", round(correl_7$estimate, 2),
                   "(",round(correl_7$conf.int[1], 2),"-",round(correl_7$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring min temp-Peak-Length of season
plot(cb3, cb12, pch=19, main= "Spring min temp-Peak-Length of season",
     type = "p", xlab="Spring min temp-peak of season", 
     ylab="Spring min temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_8 <- cor.test(cb3, cb12) 
sub_title <- paste("correlation: ", round(correl_8$estimate, 2),
                   "(",round(correl_8$conf.int[1], 2),"-",round(correl_8$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring min temp-Timing-Length of season
plot(cb11, cb12, pch=19, main= "Spring min temp-Timing-Length of season",
     type = "p", xlab="Spring min temp-timing of season", 
     ylab="Spring min temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_9 <- cor.test(cb11, cb12) 
sub_title <- paste("correlation: ", round(correl_9$estimate, 2),
                   "(",round(correl_9$conf.int[1], 2),"-",round(correl_9$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

dev.off()






pdf(paste("correlation_in_effect_of_climate_on_different_properties_of_phenology_curve_donner_pass.pdf",sep=""),width=12,height=10)

layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=T))

#par(mar=c(3, 2, 1, 2))

#Spring max temp-Peak-Timing of season
plot(db1, db7, pch=19, main= "Spring max temp-Peak-Timing of season",  
     type = "p", xlab="Spring max temp-peak of season",
     ylab="Spring max temp-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_10 <- cor.test(db1, db7) 
sub_title <- paste("Correlation: ", round(correl_10$estimate, 2),
                   "(",round(correl_10$conf.int[1], 2),"-",round(correl_10$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring max temp-Peak-Length of season
plot(db1, db8, pch=19, main= "Spring max temp-Peak-Length of season",
     type = "p", xlab="Spring max temp-peak of season", 
     ylab="Spring max temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_11 <- cor.test(db1, db8) 
sub_title <- paste("correlation: ", round(correl_11$estimate, 2),
                   "(",round(correl_11$conf.int[1], 2),"-",round(correl_11$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring max temp-Timing-Length of season
plot(db7, db8, pch=19, main= "Spring max temp-Timing-Length of season",
     type = "p", xlab="Spring max temp-timing of season", 
     ylab="Spring max temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_12 <- cor.test(db7, db8) 
sub_title <- paste("correlation: ", round(correl_12$estimate, 2),
                   "(",round(correl_12$conf.int[1], 2),"-",round(correl_12$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")



#winter precipitation-Peak-Timing of season
plot(db2, db9, pch=19, main= "Winter precipitation-Peak-Timing of season",  
     type = "p", xlab="Winter precipitation-peak of season",
     ylab="Winter precipitation-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_13 <- cor.test(db2, db9) 
sub_title <- paste("Correlation: ", round(correl_13$estimate, 2),
                   "(",round(correl_13$conf.int[1], 2),"-",round(correl_13$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Winter precipitation-Peak-Length of season
plot(db2, db10, pch=19, main= "Winter precipitation-Peak-Length of season",
     type = "p", xlab="Winter precipitation-peak of season", 
     ylab="Winter precipitation-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_14 <- cor.test(db2, db10) 
sub_title <- paste("correlation: ", round(correl_14$estimate, 2),
                   "(",round(correl_14$conf.int[1], 2),"-",round(correl_14$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Winter precipitation-Timing-Length of season
plot(db9, db10, pch=19, main= "Winter precipitation-Timing-Length of season",
     type = "p", xlab="Winter precipitation-timing of season", 
     ylab="Winter precipitation-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_15 <- cor.test(db9, db10) 
sub_title <- paste("correlation: ", round(correl_15$estimate, 2),
                   "(",round(correl_15$conf.int[1], 2),"-",round(correl_15$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")




#Spring min temp-Peak-Timing of season
plot(db3, db11, pch=19, main= "Spring min temp-Peak-Timing of season",  
     type = "p", xlab="Spring min temp-peak of season",
     ylab="Spring min temp-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_16 <- cor.test(db3, db11) 
sub_title <- paste("Correlation: ", round(correl_16$estimate, 2),
                   "(",round(correl_16$conf.int[1], 2),"-",round(correl_16$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring min temp-Peak-Length of season
plot(db3, db12, pch=19, main= "Spring min temp-Peak-Length of season",
     type = "p", xlab="Spring min temp-peak of season", 
     ylab="Spring min temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_17 <- cor.test(db3, db12) 
sub_title <- paste("correlation: ", round(correl_17$estimate, 2),
                   "(",round(correl_17$conf.int[1], 2),"-",round(correl_17$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring min temp-Timing-Length of season
plot(db11, db12, pch=19, main= "Spring min temp-Timing-Length of season",
     type = "p", xlab="Spring min temp-timing of season", 
     ylab="Spring min temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_18 <- cor.test(db11, db12) 
sub_title <- paste("correlation: ", round(correl_18$estimate, 2),
                   "(",round(correl_18$conf.int[1], 2),"-",round(correl_18$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

dev.off()



pdf(paste("correlation_in_effect_of_climate_on_different_properties_of_phenology_curve_lang_crossing.pdf",sep=""),width=12,height=10)


layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=T))

#par(mar=c(3, 2, 1, 2))

#Spring max temp-Peak-Timing of season
plot(lb1, lb7, pch=19, main= "Spring max temp-Peak-Timing of season",  
     type = "p", xlab="Spring max temp-peak of season",
     ylab="Spring max temp-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_19 <- cor.test(lb1, lb7) 
sub_title <- paste("Correlation: ", round(correl_19$estimate, 2),
                   "(",round(correl_19$conf.int[1], 2),"-",round(correl_19$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring max temp-Peak-Length of season
plot(lb1, lb8, pch=19, main= "Spring max temp-Peak-Length of season",
     type = "p", xlab="Spring max temp-peak of season", 
     ylab="Spring max temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_20 <- cor.test(lb1, lb8) 
sub_title <- paste("correlation: ", round(correl_20$estimate, 2),
                   "(",round(correl_20$conf.int[1], 2),"-",round(correl_20$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring max temp-Timing-Length of season
plot(lb7, lb8, pch=19, main= "Spring max temp-Timing-Length of season",
     type = "p", xlab="Spring max temp-timing of season", 
     ylab="Spring max temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_21 <- cor.test(lb7, lb8) 
sub_title <- paste("correlation: ", round(correl_21$estimate, 2),
                   "(",round(correl_21$conf.int[1], 2),"-",round(correl_21$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")



#winter precipitation-Peak-Timing of season
plot(lb2, lb9, pch=19, main= "Winter precipitation-Peak-Timing of season",  
     type = "p", xlab="Winter precipitation-peak of season",
     ylab="Winter precipitation-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_22 <- cor.test(lb2, lb9) 
sub_title <- paste("Correlation: ", round(correl_22$estimate, 2),
                   "(",round(correl_22$conf.int[1], 2),"-",round(correl_22$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Winter precipitation-Peak-Length of season
plot(lb2, lb10, pch=19, main= "Winter precipitation-Peak-Length of season",
     type = "p", xlab="Winter precipitation-peak of season", 
     ylab="Winter precipitation-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_23 <- cor.test(lb2, lb10) 
sub_title <- paste("correlation: ", round(correl_23$estimate, 2),
                   "(",round(correl_23$conf.int[1], 2),"-",round(correl_23$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Winter precipitation-Timing-Length of season
plot(lb9, lb10, pch=19, main= "Winter precipitation-Timing-Length of season",
     type = "p", xlab="Winter precipitation-timing of season", 
     ylab="Winter precipitation-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_24 <- cor.test(lb9, lb10) 
sub_title <- paste("correlation: ", round(correl_24$estimate, 2),
                   "(",round(correl_24$conf.int[1], 2),"-",round(correl_24$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")




#Spring min temp-Peak-Timing of season
plot(lb3, lb11, pch=19, main= "Spring min temp-Peak-Timing of season",  
     type = "p", xlab="Spring min temp-peak of season",
     ylab="Spring min temp-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_25 <- cor.test(lb3, lb11) 
sub_title <- paste("Correlation: ", round(correl_25$estimate, 2),
                   "(",round(correl_25$conf.int[1], 2),"-",round(correl_25$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring min temp-Peak-Length of season
plot(lb3, lb12, pch=19, main= "Spring min temp-Peak-Length of season",
     type = "p", xlab="Spring min temp-peak of season", 
     ylab="Spring min temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_26 <- cor.test(lb3, lb12) 
sub_title <- paste("correlation: ", round(correl_26$estimate, 2),
                   "(",round(correl_26$conf.int[1], 2),"-",round(correl_26$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring min temp-Timing-Length of season
plot(lb11, lb12, pch=19, main= "Spring min temp-Timing-Length of season",
     type = "p", xlab="Spring min temp-timing of season", 
     ylab="Spring min temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_27 <- cor.test(lb11, lb12) 
sub_title <- paste("correlation: ", round(correl_27$estimate, 2),
                   "(",round(correl_27$conf.int[1], 2),"-",round(correl_27$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

dev.off()



pdf(paste("correlation_in_effect_of_climate_on_different_properties_of_phenology_curve_sierra_valley.pdf",sep=""),width=12,height=10)


layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=T))

#par(mar=c(3, 2, 1, 2))

#Spring max temp-Peak-Timing of season
plot(sb1, sb7, pch=19, main= "Spring max temp-Peak-Timing of season",  
     type = "p", xlab="Spring max temp-peak of season",
     ylab="Spring max temp-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_28 <- cor.test(sb1, sb7) 
sub_title <- paste("Correlation: ", round(correl_28$estimate, 2),
                   "(",round(correl_28$conf.int[1], 2),"-",round(correl_28$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring max temp-Peak-Length of season
plot(sb1, sb8, pch=19, main= "Spring max temp-Peak-Length of season",
     type = "p", xlab="Spring max temp-peak of season", 
     ylab="Spring max temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_29 <- cor.test(sb1, sb8) 
sub_title <- paste("correlation: ", round(correl_29$estimate, 2),
                   "(",round(correl_29$conf.int[1], 2),"-",round(correl_29$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring max temp-Timing-Length of season
plot(sb7, sb8, pch=19, main= "Spring max temp-Timing-Length of season",
     type = "p", xlab="Spring max temp-timing of season", 
     ylab="Spring max temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_30 <- cor.test(sb7, sb8) 
sub_title <- paste("correlation: ", round(correl_30$estimate, 2),
                   "(",round(correl_30$conf.int[1], 2),"-",round(correl_30$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")



#winter precipitation-Peak-Timing of season
plot(sb2, sb9, pch=19, main= "Winter precipitation-Peak-Timing of season",  
     type = "p", xlab="Winter precipitation-peak of season",
     ylab="Winter precipitation-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_31 <- cor.test(sb2, sb9) 
sub_title <- paste("Correlation: ", round(correl_31$estimate, 2),
                   "(",round(correl_31$conf.int[1], 2),"-",round(correl_31$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Winter precipitation-Peak-Length of season
plot(sb2, sb10, pch=19, main= "Winter precipitation-Peak-Length of season",
     type = "p", xlab="Winter precipitation-peak of season", 
     ylab="Winter precipitation-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_32 <- cor.test(sb2, sb10) 
sub_title <- paste("correlation: ", round(correl_32$estimate, 2),
                   "(",round(correl_32$conf.int[1], 2),"-",round(correl_32$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Winter precipitation-Timing-Length of season
plot(sb9, sb10, pch=19, main= "Winter precipitation-Timing-Length of season",
     type = "p", xlab="Winter precipitation-timing of season", 
     ylab="Winter precipitation-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_33 <- cor.test(sb9, sb10) 
sub_title <- paste("correlation: ", round(correl_33$estimate, 2),
                   "(",round(correl_33$conf.int[1], 2),"-",round(correl_33$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")




#Spring min temp-Peak-Timing of season
plot(sb3, sb11, pch=19, main= "Spring min temp-Peak-Timing of season",  
     type = "p", xlab="Spring min temp-peak of season",
     ylab="Spring min temp-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_34 <- cor.test(sb3, sb11) 
sub_title <- paste("Correlation: ", round(correl_34$estimate, 2),
                   "(",round(correl_34$conf.int[1], 2),"-",round(correl_34$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring min temp-Peak-Length of season
plot(sb3, sb12, pch=19, main= "Spring min temp-Peak-Length of season",
     type = "p", xlab="Spring min temp-peak of season", 
     ylab="Spring min temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_35 <- cor.test(sb3, sb12) 
sub_title <- paste("correlation: ", round(correl_35$estimate, 2),
                   "(",round(correl_35$conf.int[1], 2),"-",round(correl_35$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring min temp-Timing-Length of season
plot(sb11, sb12, pch=19, main= "Spring min temp-Timing-Length of season",
     type = "p", xlab="Spring min temp-timing of season", 
     ylab="Spring min temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_36 <- cor.test(sb11, sb12) 
sub_title <- paste("correlation: ", round(correl_36$estimate, 2),
                   "(",round(correl_36$conf.int[1], 2),"-",round(correl_36$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

dev.off()



pdf(paste("correlation_in_effect_of_climate_on_different_properties_of_phenology_curve_washington.pdf",sep=""),width=12,height=10)


layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=T))
#par(mar=c(3, 2, 1, 2))

#Spring max temp-Peak-Timing of season
plot(wb1, wb7, pch=19, main= "Spring max temp-Peak-Timing of season",  
     type = "p", xlab="Spring max temp-peak of season",
     ylab="Spring max temp-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_37 <- cor.test(wb1, wb7) 
sub_title <- paste("Correlation: ", round(correl_37$estimate, 2),
                   "(",round(correl_37$conf.int[1], 2),"-",round(correl_37$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring max temp-Peak-Length of season
plot(wb1, wb8, pch=19, main= "Spring max temp-Peak-Length of season",
     type = "p", xlab="Spring max temp-peak of season", 
     ylab="Spring max temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_38 <- cor.test(wb1, wb8) 
sub_title <- paste("correlation: ", round(correl_38$estimate, 2),
                   "(",round(correl_38$conf.int[1], 2),"-",round(correl_38$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring max temp-Timing-Length of season
plot(wb7, wb8, pch=19, main= "Spring max temp-Timing-Length of season",
     type = "p", xlab="Spring max temp-timing of season", 
     ylab="Spring max temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_39 <- cor.test(wb7, wb8) 
sub_title <- paste("correlation: ", round(correl_39$estimate, 2),
                   "(",round(correl_39$conf.int[1], 2),"-",round(correl_39$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")



#winter precipitation-Peak-Timing of season
plot(wb2, wb9, pch=19, main= "Winter precipitation-Peak-Timing of season",  
     type = "p", xlab="Winter precipitation-peak of season",
     ylab="Winter precipitation-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_40 <- cor.test(wb2, wb9) 
sub_title <- paste("Correlation: ", round(correl_40$estimate, 2),
                   "(",round(correl_40$conf.int[1], 2),"-",round(correl_40$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Winter precipitation-Peak-Length of season
plot(wb2, wb10, pch=19, main= "Winter precipitation-Peak-Length of season",
     type = "p", xlab="Winter precipitation-peak of season", 
     ylab="Winter precipitation-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_41 <- cor.test(wb2, wb10) 
sub_title <- paste("correlation: ", round(correl_41$estimate, 2),
                   "(",round(correl_41$conf.int[1], 2),"-",round(correl_41$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Winter precipitation-Timing-Length of season
plot(wb9, wb10, pch=19, main= "Winter precipitation-Timing-Length of season",
     type = "p", xlab="Winter precipitation-timing of season", 
     ylab="Winter precipitation-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_42 <- cor.test(wb9, wb10) 
sub_title <- paste("correlation: ", round(correl_42$estimate, 2),
                   "(",round(correl_42$conf.int[1], 2),"-",round(correl_42$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")




#Spring min temp-Peak-Timing of season
plot(wb3, wb11, pch=19, main= "Spring min temp-Peak-Timing of season",  
     type = "p", xlab="Spring min temp-peak of season",
     ylab="Spring min temp-timing of season", 
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_43 <- cor.test(wb3, wb11) 
sub_title <- paste("Correlation: ", round(correl_43$estimate, 2),
                   "(",round(correl_43$conf.int[1], 2),"-",round(correl_43$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring min temp-Peak-Length of season
plot(wb3, wb12, pch=19, main= "Spring min temp-Peak-Length of season",
     type = "p", xlab="Spring min temp-peak of season", 
     ylab="Spring min temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_44 <- cor.test(wb3, wb12) 
sub_title <- paste("correlation: ", round(correl_44$estimate, 2),
                   "(",round(correl_44$conf.int[1], 2),"-",round(correl_44$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

#Spring min temp-Timing-Length of season
plot(wb11, wb12, pch=19, main= "Spring min temp-Timing-Length of season",
     type = "p", xlab="Spring min temp-timing of season", 
     ylab="Spring min temp-length of season",
     cex.main= 1.5,cex.lab=1.5, cex.axis=1.5, bty = "l")
correl_45 <- cor.test(wb11, wb12) 
sub_title <- paste("correlation: ", round(correl_45$estimate, 2),
                   "(",round(correl_45$conf.int[1], 2),"-",round(correl_45$conf.int[2], 2),")")
mtext(sub_title, side = 3, line = 0, cex = 1, font = 2, col = "black")

dev.off()















#summary plot comparing effects of phenology and abundance
pdf(paste("correlation_in_effect_of_climate_on_different_properties_of_phenology_curve_summary.pdf",sep=""),width=14.5,height=10)
layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=T))
par(oma = c(0, 1, 0, 1))
sites <- c("WA", "LC",  "DP", "CP", "SV")
cols <- c("#57575f", "#849fad", "#276478", "#ca3542", "#FAAC77")

#correlation between the effect of spring maximum temperature on MO and TO 
spma_mo_to <- c(correl_37$estimate,correl_19$estimate,correl_10$estimate,
             correl_1$estimate,correl_28$estimate) 
#lower confidence interval
spma_mo_to_lw <- c(correl_37$conf.int[1],correl_19$conf.int[1],correl_10$conf.int[1],
                  correl_1$conf.int[1],correl_28$conf.int[1]) 
#upper confidence interval
spma_mo_to_up <- c(correl_37$conf.int[2],correl_19$conf.int[2],correl_10$conf.int[2],
                   correl_1$conf.int[2],correl_28$conf.int[2]) 

#correlation between the effect of spring maximum temperature on MO and LO 
spma_mo_lo <- c(correl_38$estimate,correl_20$estimate,correl_11$estimate,
                correl_2$estimate,correl_29$estimate) 
#lower confidence interval
spma_mo_lo_lw <- c(correl_38$conf.int[1],correl_20$conf.int[1],correl_11$conf.int[1],
                   correl_2$conf.int[1],correl_29$conf.int[1]) 
#upper confidence interval
spma_mo_lo_up <- c(correl_38$conf.int[2],correl_20$conf.int[2],correl_11$conf.int[2],
                   correl_2$conf.int[2],correl_29$conf.int[2])

#correlation between the effect of spring maximum temperature on TO and LO
spma_to_lo <- c(correl_39$estimate,correl_21$estimate,correl_12$estimate,
                correl_3$estimate,correl_30$estimate)
#lower confidence interval
spma_to_lo_lw <- c(correl_39$conf.int[1],correl_21$conf.int[1],correl_12$conf.int[1],
                   correl_3$conf.int[1],correl_30$conf.int[1])
#upper confidence interval
spma_to_lo_up <- c(correl_39$conf.int[2],correl_21$conf.int[2],correl_12$conf.int[2],
                   correl_3$conf.int[2],correl_30$conf.int[2])



#correlation between the effect of winter precipitation on MO and TO 
wipr_mo_to <- c(correl_40$estimate,correl_22$estimate,correl_13$estimate,
                correl_4$estimate,correl_31$estimate) 
#lower confidence interval
wipr_mo_to_lw <- c(correl_40$conf.int[1],correl_22$conf.int[1],correl_13$conf.int[1],
                   correl_4$conf.int[1],correl_31$conf.int[1]) 
#upper confidence interval
wipr_mo_to_up <- c(correl_40$conf.int[2],correl_22$conf.int[2],correl_13$conf.int[2],
                   correl_4$conf.int[2],correl_31$conf.int[2]) 

#correlation between the effect of winter precipitation on MO and LO 
wipr_mo_lo <- c(correl_41$estimate,correl_23$estimate,correl_14$estimate,
                correl_5$estimate,correl_32$estimate) 
#lower confidence interval
wipr_mo_lo_lw <- c(correl_41$conf.int[1],correl_23$conf.int[1],correl_14$conf.int[1],
                   correl_5$conf.int[1],correl_32$conf.int[1]) 
#upper confidence interval
wipr_mo_lo_up <- c(correl_41$conf.int[2],correl_23$conf.int[2],correl_14$conf.int[2],
                   correl_5$conf.int[2],correl_32$conf.int[2]) 

#correlation between the effect of winter precipitation on TO and LO
wipr_to_lo <- c(correl_42$estimate,correl_24$estimate,correl_15$estimate,
                correl_6$estimate,correl_33$estimate)
#lower confidence interval
wipr_to_lo_lw <- c(correl_42$conf.int[1],correl_24$conf.int[1],correl_15$conf.int[1],
                   correl_6$conf.int[1],correl_33$conf.int[1])
#upper confidence interval
wipr_to_lo_up <- c(correl_42$conf.int[2],correl_24$conf.int[2],correl_15$conf.int[2],
                   correl_6$conf.int[2],correl_33$conf.int[2])


#correlation between the effect of spring minimum temperature on MO and TO 
spmi_mo_to <- c(correl_43$estimate,correl_25$estimate,correl_16$estimate,
                correl_7$estimate,correl_34$estimate) 
#lower confidence interval
spmi_mo_to_lw <- c(correl_43$conf.int[1],correl_25$conf.int[1],correl_16$conf.int[1],
                   correl_7$conf.int[1],correl_34$conf.int[1]) 
#upper confidence interval
spmi_mo_to_up <- c(correl_43$conf.int[2],correl_25$conf.int[2],correl_16$conf.int[2],
                   correl_7$conf.int[2],correl_34$conf.int[2])

#correlation between the effect of spring minimum temperature on MO and LO 
spmi_mo_lo <- c(correl_44$estimate,correl_26$estimate,correl_17$estimate,
                correl_8$estimate,correl_35$estimate) 
#lower confidence interval
spmi_mo_lo_lw <- c(correl_44$conf.int[1],correl_26$conf.int[1],correl_17$conf.int[1],
                   correl_8$conf.int[1],correl_35$conf.int[1]) 
#upper confidence interval
spmi_mo_lo_up <- c(correl_44$conf.int[2],correl_26$conf.int[2],correl_17$conf.int[2],
                   correl_8$conf.int[2],correl_35$conf.int[2])

#correlation between the effect of spring minimum temperature on TO and LO
spmi_to_lo <- c(correl_45$estimate,correl_27$estimate,correl_18$estimate,
                correl_9$estimate,correl_36$estimate)
#lower confidence interval
spmi_to_lo_lw <- c(correl_45$conf.int[1],correl_27$conf.int[1],correl_18$conf.int[1],
                   correl_9$conf.int[1],correl_36$conf.int[1])
#upper confidence interval
spmi_to_lo_up <- c(correl_45$conf.int[2],correl_27$conf.int[2],correl_18$conf.int[2],
                   correl_9$conf.int[2],correl_36$conf.int[2])







plot(spma_mo_to, type = "p", xlab = "", ylab = "",  xaxt = "n", lwd=2, col=cols, pch=19, 
     ylim = c(-max(c(spma_mo_to_up,spma_mo_to_lw)), max(c(spma_mo_to_up,spma_mo_to_lw))), cex=2.0, cex.axis = 1.5)
# error bars
arrows(x0 = 1:length(spma_mo_to), y0 = spma_mo_to_lw, y1 = spma_mo_to_up, length = 0.05, angle = 90, code = 3)
# x-axis with custom labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 1.5)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
title(main = "(a) Spring max temperature: MO and TO", cex.main=1.5)
title(xlab = "Sites", cex.lab=1.5) 
title(ylab = "r", cex.lab=2.2)

plot(wipr_mo_to, type = "p", xlab = "", ylab = "",  xaxt = "n", lwd=2, col=cols, pch=19, 
     ylim = c(-max(c(wipr_mo_to_up,wipr_mo_to_lw)), max(c(wipr_mo_to_up,wipr_mo_to_lw))), cex=2.0, cex.axis = 1.5)
# error bars
arrows(x0 = 1:length(wipr_mo_to), y0 = wipr_mo_to_lw, y1 = wipr_mo_to_up, length = 0.05, angle = 90, code = 3)
# x-axis with custom labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 1.5)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
title(main = "(b) Winter precipitation: MO and TO", cex.main=1.5)
title(xlab = "Sites", cex.lab=1.5) 
title(ylab = "r", cex.lab=2.2)


plot(spmi_mo_to, type = "p", xlab = "", ylab = "",  xaxt = "n", lwd=2, col=cols, pch=19, 
     ylim = c(min(c(spmi_mo_to_up,spmi_mo_to_lw)), -min(c(spmi_mo_to_up,spmi_mo_to_lw))), cex=2.0, cex.axis = 1.5)
# error bars
arrows(x0 = 1:length(spmi_mo_to), y0 = spmi_mo_to_lw, y1 = spmi_mo_to_up, length = 0.05, angle = 90, code = 3)
# x-axis with custom labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 1.5)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
title(main = "(c) Spring min temperature: MO and TO", cex.main=1.5)
title(xlab = "Sites", cex.lab=1.5) 
title(ylab = "r", cex.lab=2.2)


plot(spma_mo_lo, type = "p", xlab = "", ylab = "",  xaxt = "n", lwd=2, col=cols, pch=19, 
     ylim = c(min(c(spma_mo_lo_up,spma_mo_lo_lw)), -min(c(spma_mo_lo_up,spma_mo_lo_lw))), cex=2.0, cex.axis = 1.5)
# error bars
arrows(x0 = 1:length(spma_mo_lo), y0 = spma_mo_lo_lw, y1 = spma_mo_lo_up, length = 0.05, angle = 90, code = 3)
# x-axis with custom labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 1.5)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
title(main = "(d) Spring max temperature: MO and LO", cex.main=1.5)
title(xlab = "Sites", cex.lab=1.5) 
title(ylab = "r", cex.lab=2.2)


plot(wipr_mo_lo, type = "p", xlab = "", ylab = "",  xaxt = "n", lwd=2, col=cols, pch=19, 
     ylim = c(min(c(wipr_mo_lo_up,wipr_mo_lo_lw)), -min(c(wipr_mo_lo_up,wipr_mo_lo_lw))), cex=2.0, cex.axis = 1.5)
# error bars
arrows(x0 = 1:length(wipr_mo_lo), y0 = wipr_mo_lo_lw, y1 = wipr_mo_lo_up, length = 0.05, angle = 90, code = 3)
# x-axis with custom labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 1.5)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
title(main = "(e) Winter precipitation: MO and LO", cex.main=1.5)
title(xlab = "Sites", cex.lab=1.5) 
title(ylab = "r", cex.lab=2.2)



plot(spmi_mo_lo, type = "p", xlab = "", ylab = "",  xaxt = "n", lwd=2, col=cols, pch=19, 
     ylim = c(min(c(spmi_mo_lo_up,spmi_mo_lo_lw)), -min(c(spmi_mo_lo_up,spmi_mo_lo_lw))), cex=2.0, cex.axis = 1.5)
# error bars
arrows(x0 = 1:length(spmi_mo_lo), y0 = spmi_mo_lo_lw, y1 = spmi_mo_lo_up, length = 0.05, angle = 90, code = 3)
# x-axis with custom labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 1.5)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
title(main = "(f) Spring min temperature: MO and LO", cex.main=1.5)
title(xlab = "Sites", cex.lab=1.5) 
title(ylab = "r", cex.lab=2.2)

plot(spma_to_lo, type = "p", xlab = "", ylab = "",  xaxt = "n", lwd=2, col=cols, pch=19, 
     ylim = c(-max(c(spma_to_lo_up,spma_to_lo_lw)), max(c(spma_to_lo_up,spma_to_lo_lw))), cex=2.0, cex.axis = 1.5)
# error bars
arrows(x0 = 1:length(spma_to_lo), y0 = spma_to_lo_lw, y1 = spma_to_lo_up, length = 0.05, angle = 90, code = 3)
# x-axis with custom labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 1.5)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
title(main = "(g) Spring max temperature: TO and LO  ", cex.main=1.5)
title(xlab = "Sites", cex.lab=1.5) 
title(ylab = "r", cex.lab=2.2)


plot(wipr_to_lo, type = "p", xlab = "", ylab = "",  xaxt = "n", lwd=2, col=cols, pch=19, 
     ylim = c(-max(c(wipr_to_lo_up,wipr_to_lo_lw)), max(c(wipr_to_lo_up,wipr_to_lo_lw))), cex=2.0, cex.axis = 1.5)
# error bars
arrows(x0 = 1:length(wipr_to_lo), y0 = wipr_to_lo_lw, y1 = wipr_to_lo_up, length = 0.05, angle = 90, code = 3)
# x-axis with custom labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 1.5)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
title(main = "(h) Winter precipitation: TO and LO  ", cex.main=1.5)
title(xlab = "Sites", cex.lab=1.5) 
title(ylab = "r", cex.lab=2.2)


plot(spmi_to_lo, type = "p", xlab = "", ylab = "",  xaxt = "n", lwd=2, col=cols, pch=19, 
     ylim = c(min(c(spmi_to_lo_up,spmi_to_lo_lw)), -min(c(spmi_to_lo_up,spmi_to_lo_lw))), cex=2.0, cex.axis = 1.5)
# error bars
arrows(x0 = 1:length(spmi_to_lo), y0 = spmi_to_lo_lw, y1 = spmi_to_lo_up, length = 0.05, angle = 90, code = 3)
# x-axis with custom labels
axis(1, at = 1:length(sites), labels = sites, cex.axis = 1.5)
# horizontal line at y = 0 for reference
abline(h = 0, col = "gray")
# title and axis labels
title(main = "(i) Spring min temperature: TO and LO  ", cex.main=1.5)
title(xlab = "Sites", cex.lab=1.5)
title(ylab = "r", cex.lab=2.2)

dev.off()

