library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(scales)

pdf(paste("phenology_schematic_plot_new.pdf",sep=""),width=17, height=11) 
layout(matrix(c(1,2,3,4,5,6),2,3,byrow=T))
par( mar= c(7, 7, 4,3), oma= c(1,1,1,1))

#Figure 1 (schematic plot for phenology)
cols<- c("#f5793a","#0f2080", "#85c0f9")
x <- seq(1,55, 1)

#Figure 1a
#Mid-season occurrence

#Ambient
a <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0, 0, 0,
       0.30,0.30, 0.4, 0.4,0.45, 0.5,0.5,0.5,0.5, 0.5,0.45, 0.4, 0.4,0.30,0.30,
       0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

length(a)
length(which(a ==0))

plot(x,a,type="n",xlim =c(-4, 60),  ylim = c(-0.2, 0.8),xlab="",ylab="",axes=F)
spl <- smooth.spline(x,a,df=9)
lines(spl,lwd=2,col=cols[2])


#low
b <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0, 0, 0,
       0.15,0.20,  0.27, 0.30,0.33, 0.35,0.38,0.41,0.38, 0.35,0.33, 0.30, 0.27,0.20,0.15,
       0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

length(b)
length(which(b==0))
spl <- smooth.spline(x,b,df=9)
lines(spl,lwd=2, col= cols[1])

#high
c<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0, 0,
      0.2, 0.3, 0.4,  0.4, 0.45,0.45,0.5, 0.52, 0.55,0.63,0.65,0.63,0.55,0.52, 0.5, 0.45,0.45,0.4,0.4, 0.3,0.2,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

length(c)
length(which(c ==0))
spl <- smooth.spline(x,c,df=9)
lines(spl,lwd=2,col=cols[3])

#y-axis
lines(c(0,0),c(-0.01,1),lty=1,col="black",lwd=2)
text(-3, 0.4, "Probability of occurrence", col = "black", srt = 90, cex = 2.5)
#x-axis
lines(c(0,70),c(-0.01,-0.01),lty=1,col="black", lwd=2)
text(30, -0.09, "Ordinal day", col = "black", cex = 2.8)

#legend
legend("top", legend = c("Decreased MO (-)", "Increased MO (+)"),
       col = c(cols[1], cols[3]), lwd = 2, cex = 2.2, bty="n")

mtext("(a) MO", side = 3, line = 0.5, adj = 0.1, cex = 2.2)


#Figure 1b
#Timing of occurrence

#Earlier
a<- c(0,0,0,0,
      0.1, 0.1, 0.2, 0.20, 0.30,0.30,  0.4, 0.4,0.45,
      0.5,0.5,0.5,0.5, 0.5,0.45, 0.4, 0.4,0.30,0.30,0.2,0.20,0.1,0.1,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

length(a)
length(which(a ==0))

plot(x,a,type="n",xlim =c(-4, 60),  ylim = c(-0.2, 0.7),xlab="",ylab="",axes=F)
spl <- smooth.spline(x,a,df=16)
lines(spl,lwd=2,col=cols[1])

#Ambient
b <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0.1, 0.1, 0.2, 0.20, 0.30,0.30,  0.4, 0.4,0.45, 0.5,
       0.5,0.5, 0.5,0.5,0.45, 0.4, 0.4,0.30,0.30,0.2, 0.20,0.1,0.1,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

length(b)
length(which(b==0))
spl <- smooth.spline(x,b,df=16)
lines(spl,lwd=2, col= cols[2])

#Late
c<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0.1, 0.1, 0.2, 0.20, 0.30,0.30,  0.4, 0.4,0.45, 0.5,
      0.5,0.5, 0.5, 0.5,0.45, 0.4, 0.4,0.30,0.30,0.2, 0.20,0.1,0.1,
      0,0,0,0)

length(c)
length(which(c ==0))
spl <- smooth.spline(x,c,df=16)
lines(spl,lwd=2,col=cols[3])

#y-axis
lines(c(0,0),c(-0.01,1),lty=1,col="black",lwd=2)
text(-3, 0.35, "Probability of occurrence", col = "black", srt = 90, cex = 2.5)
#x-axis
lines(c(0,70),c(-0.01,-0.01),lty=1,col="black", lwd=2)
text(30, -0.09, "Ordinal day", col = "black", cex = 2.8)

# Add a legend
legend("top", legend = c("Earlier TO (-)",  "Later TO (+)"),
       col = c(cols[1],cols[3]), lwd = 2, cex = 2.2, bty="n")

mtext("(b) TO", side = 3, line = 0.5, adj = 0.1, cex = 2.2)


#Figure 1c
#Length of occurrence

#Short
a <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0, 0, 0,
       0.30,0.30,  0.4, 0.4,0.45, 0.5,0.5,0.5,0.5, 0.5,0.45, 0.4, 0.4,0.30,0.30,
       0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

length(a)
length(which(a ==0))
plot(x,a,type="n",xlim =c(-4, 60),  ylim = c(-0.2, 0.7),xlab="",ylab="",axes=F)
spl <- smooth.spline(x,a,df=16)
lines(spl,lwd=2,col=cols[1])

#Ambient
b <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0.1, 0.1, 0.2, 0.20, 0.30,0.30,  0.4, 0.4,0.45, 0.5,0.5,0.5, 0.5,0.5,
       0.45, 0.4, 0.4,0.30,0.30,0.2, 0.20,0.1,0.1,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

length(b)
length(which(b==0))
spl <- smooth.spline(x,b,df=16)
lines(spl,lwd=2, col= cols[2])

#Long
c <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,
       0.1,0.1,0.1,0.2, 0.2, 0.2, 0.30, 0.30,0.30,  0.4, 0.45,0.45,
       0.5,0.5,0.5, 0.5,0.5,0.45, 0.45, 0.4,0.30,0.30,0.3, 0.20,0.2,0.2,0.1,0.1,0.1,
       0,0,0,0,0,0,0,0,0,0,0,0,0)

length(c)
length(which(c ==0))
spl <- smooth.spline(x,c,df=16)
lines(spl,lwd=2,col=cols[3])

#y-axis
lines(c(0,0),c(-0.01,1),lty=1,col="black",lwd=2)
text(-3, 0.35, "Probability of occurrence", col = "black", srt = 90, cex = 2.5)
#x-axis
lines(c(0,70),c(-0.01,-0.01),lty=1,col="black", lwd=2)
text(30, -0.09, "Ordinal day", col = "black", cex = 2.8)

# Add a legend
legend("top", legend = c("Decreased LO (-)", "Increased LO (+)"),
       col = c(cols[1], cols[3]), lwd = 2, cex = 2.2, bty="n")

mtext("(c) LO", side = 3, line = 0.5, adj = 0.1, cex = 2.2)











#Figure 1d, e, f (Annual phenological curve of selected species at Donner Pass) 

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

yer <- as.numeric(as.factor(sub_dat$Year))
year <- unique(sub_dat$Year)
cyr<-rep(cs[1],length(yer))
for(i in 2:18){
  cyr[sub_dat$spring_tmax > brks[i]]<-cs[i]
}


#Polites sonora (62)
i = 62
a <- which(spid==i)
plot(sub_dat[a, "ordDate"], pa[a], pch=19,type = "n", xlab="Ordinal day",ylab="Probability of occurrence",cex.lab = 2.8, ylim=c(0,1), cex.axis = 2.8, bty = "l", mgp=c(5, 2, 0))

mtext(expression(plain("(d) ") * italic("Polites sonora")), side = 3, line = 0.5, adj = 0, cex = 2.2)

for(j in 1:length(year)){
  b <- which(yer==j & spid==i) 
  c<- sub_dat[b, "ordDate"]
  d <- pa[b]
  #sort ordinal day
  x<-sort(c)
  y <-d[order(c)]
  lines(x,y, xlab="Ordinal day",ylab="Probability of occurrence",col=alpha(cyr[b],0.9), lwd=1.5)
}

#Effect of spring maximum temperature on MO, TO ,LO
be <- round(beta[i,c(1,7,8)], 2)

text(min(sub_dat[a, "ordDate"])+45, 0.95, paste("MO =", be[1]), cex = 2.8)
text(max(sub_dat[a, "ordDate"])-50, 0.95, paste("TO =", be[2], "\nLO =", be[3]), cex = 2.8)


#Pyrgus communis (69)
i=   69
a<-which(spid==i)
plot(sub_dat[a, "ordDate"], pa[a], pch=19,type = "n", xlab="Ordinal day",ylab="Probability of occurrence",cex.lab = 2.8, ylim=c(0,1), cex.axis = 2.8, bty = "l", mgp=c(5, 2, 0))

mtext(expression(plain("(e) ") * italic("Pyrgus communis")), side = 3, line = 0.5, adj = 0, cex = 2.2)

for(j in 1:length(year)){
  b <- which(yer==j & spid==i) 
  c<- sub_dat[b, "ordDate"]
  d <- pa[b]
  #sort ordinal day
  x<-sort(c)
  y <-d[order(c)]
  lines(x,y, xlab="Ordinal day",ylab="Probability of occurrence",col=alpha(cyr[b],0.9), lwd=1.5)  
}

#Effect of spring maximum temperature on MO, TO ,LO
be <- round(beta[i,c(1,7,8)], 2)

text(min(sub_dat[a, "ordDate"])+45, 0.95, paste("MO =", be[1]), cex = 2.8)
text(max(sub_dat[a, "ordDate"])-40, 0.95, paste("TO =", be[2], "\nLO =", be[3]), cex = 2.8)



#Speyeria egleis (79)
i=   79
a<-which(spid==i)
plot(sub_dat[a, "ordDate"], pa[a], pch=19,type = "n", xlab="Ordinal day",ylab="Probability of occurrence",cex.lab = 2.8, ylim=c(0,1), cex.axis = 2.8, bty = "l", mgp=c(5, 2, 0))

mtext(expression(plain("(f) ") * italic("Speyeria egleis")), side = 3, line = 0.5, adj = 0, cex = 2.2)

for(j in 1:length(year)){
  b <- which(yer==j & spid==i) 
  c<- sub_dat[b, "ordDate"]
  d <- pa[b]
  #sort ordinal day
  x<-sort(c)
  y <-d[order(c)]
  lines(x,y, xlab="Ordinal day",ylab="Probability of occurrence",col=alpha(cyr[b],0.9), lwd=1.5)
}

#Effect of spring maximum temperature on MO, TO ,LO
be <- round(beta[i,c(1,7,8)], 2)

text(min(sub_dat[a, "ordDate"])+45, 0.95, paste("MO =", be[1]), cex = 2.8)
text(max(sub_dat[a, "ordDate"])-50, 0.95, paste("TO =", be[2], "\nLO =", be[3]), cex = 2.8)


dev.off()
