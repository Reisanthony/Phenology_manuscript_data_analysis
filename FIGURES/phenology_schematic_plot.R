library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(scales)

pdf(paste("phenology_schematic_plot.pdf",sep=""),width=14,height=9) #width=12, height=4
layout(matrix(c(1,2,3,4,5,6),2,3,byrow=T))
par( mar= c(4, 5, 1, 1), oma= c(2,2,2,2))
################################################################################################
#################################schematic plot for phenology####################################
#################################################################################################

cols<- c("#f5793a","#0f2080", "#85c0f9")
x <- seq(1,55, 1)


######################### Probability of occurrence

#ambient
a<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0, 0, 0, 0.30,0.30,  0.4, 0.4,0.45, 0.5,0.5,0.5,0.5, 0.5,0.45, 0.4, 0.4,0.30,0.30,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
length(a)
length(which(a ==0))
plot(x,a,type="n",xlim =c(-4, 60),  ylim = c(-0.2, 0.8),xlab="",ylab="",axes=F)
spl <- smooth.spline(x,a,df=9)
lines(spl,lwd=2,col=cols[2])


#low
b <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0, 0, 0, 0.15,0.20,  0.27, 0.30,0.33, 0.35,0.38,0.41,0.38, 0.35,0.33, 0.30, 0.27,0.20,0.15,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#b <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0, 0, 0.1, 0.30,0.30,  0.4, 0.4,0.4, 0.41,0.41,0.41,0.41, 0.41,0.4, 0.4, 0.4,0.30,0.30,0.1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
length(b)
length(which(b==0))
spl <- smooth.spline(x,b,df=9)
lines(spl,lwd=2, col= cols[1])

#high
c<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0, 0, 0.2, 0.3, 0.4,  0.4, 0.45,0.45,0.5, 0.52, 0.55,0.63,0.65,0.63, 0.55,0.52, 0.5, 0.45,0.45,0.4,0.4, 0.3,0.2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#c<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0, 0.1, 0.1, 0.30,0.30,  0.4, 0.4,0.45, 0.55,0.63,0.65,0.63, 0.55,0.45, 0.4, 0.4,0.30,0.30,0.1, 0.1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
length(c)
length(which(c ==0))
spl <- smooth.spline(x,c,df=9)
lines(spl,lwd=2,col=cols[3])
#y-axis
lines(c(0,0),c(-0.01,0.75),lty=1,col="black",lwd=2)
#x-axis
lines(c(0,58),c(-0.01,-0.01),lty=1,col="black", lwd=2)
#y-axis
text(-3, 0.4, "Probability of occurrence", col = "black", srt = 90, cex = 1.5)
#x-axis
text(30, -0.09, "Ordinal day", col = "black", cex = 1.5)

# Add a legend
legend("topright", legend = c("Decreased midseason occurrence (-)", "Increased midseason occurrence (+)"),
       col = c(cols[1], cols[3]), lwd = 2, cex = 1.4, bty="n")

#title(main= "(c) Probability of occurrence",cex.main=1.5)
mtext("(a) MO", side = 3, line = 0.5, adj = 0.1, cex = 1.5)


############################start of season

#earlier
a<- c(0,0,0,0,0.1, 0.1, 0.2, 0.20, 0.30,0.30,  0.4, 0.4,0.45, 0.5,0.5,0.5,0.5, 0.5,0.45, 0.4, 0.4,0.30,0.30,0.2, 0.20,0.1,0.1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
length(a)
length(which(a ==0))

plot(x,a,type="n",xlim =c(-4, 60),  ylim = c(-0.2, 0.7),xlab="",ylab="",axes=F)
spl <- smooth.spline(x,a,df=16)
lines(spl,lwd=2,col=cols[1])


#ambient
b <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.1, 0.1, 0.2, 0.20, 0.30,0.30,  0.4, 0.4,0.45, 0.5,0.5,0.5, 0.5,0.5,0.45, 0.4, 0.4,0.30,0.30,0.2, 0.20,0.1,0.1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
length(b)
length(which(b==0))
spl <- smooth.spline(x,b,df=16)
lines(spl,lwd=2, col= cols[2])

#late
c<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.1, 0.1, 0.2, 0.20, 0.30,0.30,  0.4, 0.4,0.45, 0.5,0.5,0.5, 0.5, 0.5,0.45, 0.4, 0.4,0.30,0.30,0.2, 0.20,0.1,0.1,0,0,0,0)
length(c)
length(which(c ==0))
spl <- smooth.spline(x,c,df=16)
lines(spl,lwd=2,col=cols[3])

#y-axis
lines(c(0,0),c(-0.01,0.7),lty=1,col="black",lwd=2)
#x-axis
lines(c(0,58),c(-0.01,-0.01),lty=1,col="black", lwd=2)
#y-axis
text(-3, 0.4, "Probability of occurrence", col = "black", srt = 90, cex = 1.5)
#x-axis
text(30, -0.09, "Ordinal day", col = "black", cex = 1.5)

# Add a legend
legend("topright", legend = c("Earlier timing (-)",  "Later timing (+)"),
       col = c(cols[1],cols[3]), lwd = 2, cex = 1.4, bty="n")

#title(main= "(a) Timing of occurrence",cex.main=1.5)
mtext("(b) TO", side = 3, line = 0.5, adj = 0.1, cex = 1.5)



#########################length of season

#short
a<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0, 0, 0, 0.30,0.30,  0.4, 0.4,0.45, 0.5,0.5,0.5,0.5, 0.5,0.45, 0.4, 0.4,0.30,0.30,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
length(a)
length(which(a ==0))
plot(x,a,type="n",xlim =c(-4, 60),  ylim = c(-0.2, 0.7),xlab="",ylab="",axes=F)
spl <- smooth.spline(x,a,df=16)
lines(spl,lwd=2,col=cols[1])


#ambient
b <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.1, 0.1, 0.2, 0.20, 0.30,0.30,  0.4, 0.4,0.45, 0.5,0.5,0.5, 0.5,0.5,0.45, 0.4, 0.4,0.30,0.30,0.2, 0.20,0.1,0.1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
length(b)
length(which(b==0))
spl <- smooth.spline(x,b,df=16)
lines(spl,lwd=2, col= cols[2])

#long
c<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.1,0.1,0.1,0.2, 0.2, 0.2, 0.30, 0.30,0.30,  0.4, 0.45,0.45, 0.5,0.5,0.5, 0.5,0.5,0.45, 0.45, 0.4,0.30,0.30,0.3, 0.20,0.2,0.2,0.1,0.1,0.1,0,0,0,0,0,0,0,0,0,0,0,0,0)
length(c)
length(which(c ==0))
spl <- smooth.spline(x,c,df=16)
lines(spl,lwd=2,col=cols[3])
#y-axis
lines(c(0,0),c(-0.01,0.7),lty=1,col="black",lwd=2)
#x-axis
lines(c(0,58),c(-0.01,-0.01),lty=1,col="black", lwd=2)
#y-axis
text(-3, 0.4, "Probability of occurrence", col = "black", srt = 90, cex = 1.5)
#x-axis
text(30, -0.09, "Ordinal day", col = "black", cex = 1.5)

# Add a legend
legend("top", legend = c("Shorter flight period (-)", "Longer flight period (+)"),
       col = c(cols[1], cols[3]), lwd = 2, cex = 1.4, bty="n")

#title(main= "(b) Length of occurrence",cex.main=1.5)
mtext("(c) LO", side = 3, line = 0.5, adj = 0.1, cex = 1.5)












#DONNER PASS
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Donner Pass.rdat')

pa<-extract(fit,"pp")
dim(pa[[1]])
#median
pa <- apply(pa[[1]], 2, quantile, probs = 0.5)

betas <- extract(fit,"beta")
#median 
beta <- apply(betas[[1]], c(2, 3), quantile, probs = 0.5)

spid<-as.numeric(as.factor(sub_dat$genus_species))
brks<-quantile(sub_dat$spring_tmax, probs=seq(0,1,1/18))
cs<-rev(heat.colors(n=18))
#cs = hsv(0.5, seq(0,1,length.out = 18), .85)
#colorrange <- colorRampPalette(c("#ADD8E6", "#00008B"))    
#cs <- colorrange(18)

yer <- as.numeric(as.factor(sub_dat$Year))
year <- unique(sub_dat$Year)
cyr<-rep(cs[1],length(yer))


for(i in 2:18){
  cyr[sub_dat$spring_tmax > brks[i]]<-cs[i]
}

#plots
#62Polites sonora
i=   62
a<-which(spid==i)
plot(sub_dat[a, 18], pa[a], pch=19,type = "n", xlab="Ordinal day",ylab="Probability of occurrence",cex.lab=1.5, ylim=c(0,1), cex.axis=1.5, bty = "l")
#title(species_names[i], cex.main=1.5)

mtext(expression(italic("(d) Polites sonora")), side = 3, line = 0.5, adj = 0, cex = 1.5)

for(j in 1:length(year)){
  b <- which(yer==j & spid==i) 
  c<- sub_dat[b, 18]
  d <- pa[b]
  #sort ordinal day
  x<-sort(c)
  y <-d[order(c)]
  
  lines(x,y, type = "l", xlab="Ordinal day",ylab="Probability of occurrence",col=alpha(cyr[b],0.9),cex.lab=1.5, lwd=1.5, cex.axis=1.5)
  
}
be <- round(beta[i,c(1,7,8)], 3)
text(min(sub_dat[a, 18])+45, 0.95, paste("MO =", be[1]), cex = 1.3, col="red")
text(max(sub_dat[a, 18])-45, 0.95, paste("TO =", be[2], "\nLO =", be[3]), cex = 1.3, col="red")


#69Pyrgus communis
i=   69
a<-which(spid==i)
plot(sub_dat[a, 18], pa[a], pch=19,type = "n", xlab="Ordinal day",ylab="Probability of occurrence",cex.lab=1.5, ylim=c(0,1), cex.axis=1.5, bty = "l")
#title(species_names[i], cex.main=1.5)

mtext(expression(italic("(e) Pyrgus communis")), side = 3, line = 0.5, adj = 0, cex = 1.5)

for(j in 1:length(year)){
  b <- which(yer==j & spid==i) 
  c<- sub_dat[b, 18]
  d <- pa[b]
  #sort ordinal day
  x<-sort(c)
  y <-d[order(c)]
  
  lines(x,y, type = "l", xlab="Ordinal day",ylab="Probability of occurrence",col=alpha(cyr[b],0.9),cex.lab=1.5, lwd=1.5, cex.axis=1.5)
  
}
be <- round(beta[i,c(1,7,8)], 3)
text(min(sub_dat[a, 18])+45, 0.95, paste("MO =", be[1]), cex = 1.3, col="red")
text(max(sub_dat[a, 18])-20, 0.95, paste("TO =", be[2], "\nLO =", be[3]), cex = 1.3, col="red")



#79Speyeria egleis
i=   79
a<-which(spid==i)
plot(sub_dat[a, 18], pa[a], pch=19,type = "n", xlab="Ordinal day",ylab="Probability of occurrence",cex.lab=1.5, ylim=c(0,1), cex.axis=1.5, bty = "l")
#title(species_names[i], cex.main=1.5)

mtext(expression(italic("(f) Speyeria egleis")), side = 3, line = 0.5, adj = 0, cex = 1.5)

for(j in 1:length(year)){
  b <- which(yer==j & spid==i) 
  c<- sub_dat[b, 18]
  d <- pa[b]
  #sort ordinal day
  x<-sort(c)
  y <-d[order(c)]
  
  lines(x,y, type = "l", xlab="Ordinal day",ylab="Probability of occurrence",col=alpha(cyr[b],0.9),cex.lab=1.5, lwd=1.5, cex.axis=1.5)
  
}
be <- round(beta[i,c(1,7,8)], 3)
text(min(sub_dat[a, 18])+45, 0.95, paste("MO =", be[1]), cex = 1.3, col="red")
text(max(sub_dat[a, 18])-45, 0.95, paste("TO =", be[2], "\nLO =", be[3]), cex = 1.3, col="red")


dev.off()
