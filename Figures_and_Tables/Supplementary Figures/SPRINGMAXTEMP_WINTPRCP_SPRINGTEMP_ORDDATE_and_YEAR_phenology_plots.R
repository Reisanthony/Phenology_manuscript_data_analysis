library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(scales)


#PLOTS SHOWING THE PROBABILITY OF A SPECIES BEING PRESENT AT AN ORDINAL DAY
#DARKER COLORS = HIGHER SPRING MAX TEMP)

#castle peak
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Castle Peak.rdat')
pa<-extract(fit,"pp")
betas <- extract(fit,"beta")
alphas <- extract(fit,"alpha")
#median 
pa <- apply(pa[[1]], 2, quantile, probs = 0.5)
alpha<- apply(alphas[[1]], 2, quantile, probs = 0.5)
beta <- apply(betas[[1]], c(2, 3), quantile, probs = 0.5)


spid<-as.numeric(as.factor(sub_dat$genus_species))
brks<-quantile(sub_dat$spring_tmax, probs=seq(0,1,1/18))
cs<-rev(heat.colors(n=18))
css<-rep(cs[1],length(spid))

yer <- as.numeric(as.factor(sub_dat$Year))
year <- unique(sub_dat$Year)
cyr<-rep(cs[1],length(yer))

for(i in 2:18){
  css[sub_dat$spring_tmax > brks[i]]<-cs[i]
}
for(i in 2:18){
  cyr[sub_dat$spring_tmax > brks[i]]<-cs[i]
}

pdf(paste("SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_and_YEAR_Castle_Peak.pdf",sep=""),width=9,height=9)
par(mfrow=c(3,3))
par(mar=c(4.5,5.5,2.5,1.5))
for(i in 1:length(sp)){
  a<-which(spid==i)
  plot(sub_dat[a, "ordDate"], pa[a], pch=19,type = "n", xlab="Ordinal day",ylab="Presence probability",cex.lab=1.4, ylim=c(0,1))
  title(main=bquote(italic(.(sp[i]))), cex.main=1.4)
  
  for(j in 1:length(year)){
    b <- which(yer==j & spid==i) 
    c<- sub_dat[b, "ordDate"]
    d <- pa[b]
    #sort ordinal day
    x<-sort(c)
    y <-d[order(c)]
    lines(x,y, xlab="Ordinal day",ylab="Presence probability",col=alpha(cyr[b],0.9), lwd=1.5)
  }
  #Effect of spring maximum temperature on MO, TO and LO
  be <- round(beta[i,c(1,7,8)], 3)
  text(min(sub_dat[a, "ordDate"])+35, 0.95, paste("MO =", be[1]), cex = 1.3)
  text(max(sub_dat[a, "ordDate"])-35, 0.95, paste("TO=", be[2], "\nLO =", be[3]), cex = 1.3)
  }
dev.off()

#donner pass
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Donner Pass.rdat')
pa<-extract(fit,"pp")
betas <- extract(fit,"beta")
alphas <- extract(fit,"alpha")

#median
pa <- apply(pa[[1]], 2, quantile, probs = 0.5)
alpha<- apply(alphas[[1]], 2, quantile, probs = 0.5)
beta <- apply(betas[[1]], c(2, 3), quantile, probs = 0.5)


spid<-as.numeric(as.factor(sub_dat$genus_species))
brks<-quantile(sub_dat$spring_tmax, probs=seq(0,1,1/18))
cs<-rev(heat.colors(n=18))
css<-rep(cs[1],length(spid))

yer <- as.numeric(as.factor(sub_dat$Year))
year <- unique(sub_dat$Year)
cyr<-rep(cs[1],length(yer))


for(i in 2:18){
  css[sub_dat$spring_tmax > brks[i]]<-cs[i]
}
for(i in 2:18){
  cyr[sub_dat$spring_tmax > brks[i]]<-cs[i]
}

pdf(paste("SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_and_YEAR_Donner_Pass.pdf",sep=""),width=9,height=9)
par(mfrow=c(3,3))
par(mar=c(4.5,5.5,2.5,1.5))
for(i in 1:length(sp)){
  a<-which(spid==i)
  plot(sub_dat[a, "ordDate"], pa[a], pch=19,type = "n", xlab="Ordinal day",ylab="Presence probability",cex.lab=1.4, ylim=c(0,1))
  title(main=bquote(italic(.(sp[i]))), cex.main=1.4)
  
  for(j in 1:length(year)){
    b <- which(yer==j & spid==i) 
    c<- sub_dat[b, "ordDate"]
    d <- pa[b]
    #sort ordinal day
    x<-sort(c)
    y <-d[order(c)]
    
    lines(x,y, xlab="Ordinal day",ylab="Presence probability",col=alpha(cyr[b],0.9), lwd=1.5)
    
  }
  #Effect of spring maximum temperature on MO, TO and LO
  be <- round(beta[i,c(1,7,8)], 3)
  text(min(sub_dat[a, "ordDate"])+55, 0.95, paste("MO =", be[1]), cex = 1.3)
  text(max(sub_dat[a, "ordDate"])-55, 0.95, paste("TO=", be[2], "\nLO =", be[3]), cex = 1.3)
}
dev.off()


#lang crossing
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Lang Crossing.rdat')
pa<-extract(fit,"pp")
betas <- extract(fit,"beta")
alphas <- extract(fit,"alpha")

#median 
pa <- apply(pa[[1]], 2, quantile, probs = 0.5)
alpha<- apply(alphas[[1]], 2, quantile, probs = 0.5)
beta <- apply(betas[[1]], c(2, 3), quantile, probs = 0.5)


spid<-as.numeric(as.factor(sub_dat$genus_species))
brks<-quantile(sub_dat$spring_tmax, probs=seq(0,1,1/18))
cs<-rev(heat.colors(n=18))
css<-rep(cs[1],length(spid))

yer <- as.numeric(as.factor(sub_dat$Year))
year <- unique(sub_dat$Year)
cyr<-rep(cs[1],length(yer))


for(i in 2:18){
  css[sub_dat$spring_tmax > brks[i]]<-cs[i]
}
for(i in 2:18){
  cyr[sub_dat$spring_tmax > brks[i]]<-cs[i]
}

pdf(paste("SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_and_YEAR_Lang_Crossing.pdf",sep=""),width=9,height=9)
par(mfrow=c(3,3))
par(mar=c(4.5,5.5,2.5,1.5))
for(i in 1:length(sp)){
  a<-which(spid==i)
  plot(sub_dat[a, "ordDate"], pa[a], pch=19,type = "n", xlab="Ordinal day",ylab="Presence probability",cex.lab=1.4, ylim=c(0,1))
  title(main=bquote(italic(.(sp[i]))), cex.main=1.4)
  
  for(j in 1:length(year)){
    b <- which(yer==j & spid==i) 
    c<- sub_dat[b, "ordDate"]
    d <- pa[b]
    #sort ordinal day
    x<-sort(c)
    y <-d[order(c)]
    lines(x,y, xlab="Ordinal day",ylab="Presence probability",col=alpha(cyr[b],0.9), lwd=1.5)
  }
  #Effect of spring maximum temperature on MO, TO and LO
  be <- round(beta[i,c(1,7,8)], 3)
  text(min(sub_dat[a, "ordDate"])+55, 0.95, paste("MO =", be[1]), cex = 1.3)
  text(max(sub_dat[a, "ordDate"])-55, 0.95, paste("TO=", be[2], "\nLO =", be[3]), cex = 1.3)
}
dev.off()


#sierra valley
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Sierra Valley.rdat')
pa<-extract(fit,"pp")
betas <- extract(fit,"beta")
alphas <- extract(fit,"alpha")

#median 
pa <- apply(pa[[1]], 2, quantile, probs = 0.5)
alpha<- apply(alphas[[1]], 2, quantile, probs = 0.5)
beta <- apply(betas[[1]], c(2, 3), quantile, probs = 0.5)


spid<-as.numeric(as.factor(sub_dat$genus_species))
brks<-quantile(sub_dat$spring_tmax, probs=seq(0,1,1/18))
cs<-rev(heat.colors(n=18))
css<-rep(cs[1],length(spid))

yer <- as.numeric(as.factor(sub_dat$Year))
year <- unique(sub_dat$Year)
cyr<-rep(cs[1],length(yer))


for(i in 2:18){
  css[sub_dat$spring_tmax > brks[i]]<-cs[i]
}
for(i in 2:18){
  cyr[sub_dat$spring_tmax > brks[i]]<-cs[i]
}

pdf(paste("SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_and_YEAR_Sierra_Valley.pdf",sep=""),width=9,height=9)
par(mfrow=c(3,3))
par(mar=c(4.5,5.5,2.5,1.5))
for(i in 1:length(sp)){
  a<-which(spid==i)
  plot(sub_dat[a, "ordDate"], pa[a], pch=19,type = "n", xlab="Ordinal day",ylab="Presence probability",cex.lab=1.4, ylim=c(0,1))
  title(main=bquote(italic(.(sp[i]))), cex.main=1.4)
  
  for(j in 1:length(year)){
    b <- which(yer==j & spid==i) 
    c<- sub_dat[b, "ordDate"]
    d <- pa[b]
    #sort ordinal day
    x<-sort(c)
    y <-d[order(c)]
    lines(x,y, xlab="Ordinal day",ylab="Presence probability",col=alpha(cyr[b],0.9), lwd=1.5)
  }
  #Effect of spring maximum temperature on MO, TO and LO
  be <- round(beta[i,c(1,7,8)], 3)
  text(min(sub_dat[a, "ordDate"])+55, 0.95, paste("MO =", be[1]), cex = 1.3)
  text(max(sub_dat[a, "ordDate"])-55, 0.95, paste("TO=", be[2], "\nLO =", be[3]), cex = 1.3)
}
dev.off()


#washington
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Washington.rdat')
pa<-extract(fit,"pp")
betas <- extract(fit,"beta")
alphas <- extract(fit,"alpha")

#median 
pa <- apply(pa[[1]], 2, quantile, probs = 0.5)
alpha<- apply(alphas[[1]], 2, quantile, probs = 0.5)
beta <- apply(betas[[1]], c(2, 3), quantile, probs = 0.5)

spid<-as.numeric(as.factor(sub_dat$genus_species))
brks<-quantile(sub_dat$spring_tmax, probs=seq(0,1,1/18))
cs<-rev(heat.colors(n=18))
css<-rep(cs[1],length(spid))

yer <- as.numeric(as.factor(sub_dat$Year))
year <- unique(sub_dat$Year)
cyr<-rep(cs[1],length(yer))


for(i in 2:18){
  css[sub_dat$spring_tmax > brks[i]]<-cs[i]
}
for(i in 2:18){
  cyr[sub_dat$spring_tmax > brks[i]]<-cs[i]
}

pdf(paste("SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_and_YEAR_Washington.pdf",sep=""),width=9,height=9)
par(mfrow=c(3,3))
par(mar=c(4.5,5.5,2.5,1.5))
for(i in 1:length(sp)){
  a<-which(spid==i)
  plot(sub_dat[a, "ordDate"], pa[a], pch=19,type = "n", xlab="Ordinal day",ylab="Presence probability",cex.lab=1.4, ylim=c(0,1))
  title(main=bquote(italic(.(sp[i]))), cex.main=1.4)
  
  for(j in 1:length(year)){
    b <- which(yer==j & spid==i) 
    c<- sub_dat[b, "ordDate"]
    d <- pa[b]
    #sort ordinal day
    x<-sort(c)
    y <-d[order(c)]
    lines(x,y, xlab="Ordinal day",ylab="Presence probability",col=alpha(cyr[b],0.9), lwd=1.5)
  }
  #Effect of spring maximum temperature on MO, TO and LO
  be <- round(beta[i,c(1,7,8)], 3)
  text(min(sub_dat[a, "ordDate"])+65, 0.95, paste("MO =", be[1]), cex = 1.3)
  text(max(sub_dat[a, "ordDate"])-65, 0.95, paste("TO=", be[2], "\nLO =", be[3]), cex = 1.3)
}
dev.off()

