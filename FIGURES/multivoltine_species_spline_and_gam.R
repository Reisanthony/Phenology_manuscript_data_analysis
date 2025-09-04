library(rstan)
library(scales)
library(mgcv)


###############################################################################
#######################natural history#############################
###############################################################################

#natural history data
nath <- read.csv("natural_history_2024.csv")

nath <- subset(nath,
#when its a resident but we dont know its overwintering stage
  !(resident == "yes" & is.na(wintering)) &
#when we dont know its residency status but we know its overwintering stage
  !(is.na(resident) & !is.na(wintering)) &
#when its not a resident but we know its overwintering stage
  !(resident == "no" & !(wintering %in% c("no", NA)))
)

#converting to just univoltine and multivoltine
nath$broods[!is.na(nath$broods) & nath$broods != "one"] <- "multiple"

#converting hostgenera into monophagous and polyphagous
nath$hostgenera <-
  ifelse(is.na(nath$hostgenera), NA,         # keep NA
  ifelse(nath$hostgenera == 1, "one",        # 1 → "one"
  ifelse(nath$hostgenera > 1, "multiple",    # >1 → "multiple"
                nath$hostgenera))                   # keep 0 or anything else as-is
)


#just voltinism
nath <- nath[, c("genus_species", "site_name", "broods")]

nath_cp <- nath[nath$site_name == "Castle Peak", ]
nath_dp <- nath[nath$site_name == "Donner Pass", ]
nath_sv <- nath[nath$site_name == "Sierra Valley", ]
nath_lc <- nath[nath$site_name == "Lang Crossing", ]
nath_wa <- nath[nath$site_name == "Washington", ]
   
    
  
cs <- c(
  "#E41A1C",  # red
  "#377EB8",  # blue
  "#4DAF4A",  # green
  "#984EA3",  # purple
  "#FF7F00",  # orange
  "#FFFF33",  # yellow
  "#A65628",  # brown
  "#F781BF",  # pink
  "#999999",  # gray
  "#66C2A5"   # teal
)

cs <- rep(cs, each=4)

#LINE TYPE
ls <- c(1,2,3,4)
ls <- rep(ls, 10)
    
#CASTLE PEAK
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Castle Peak.rdat')
sub_dat <- sub_dat[ , c("genus_species", "Year", "pa", "ordDate")]


subdat_with_broods <- merge(sub_dat, nath_cp, by = "genus_species")

subdat_multivoltine <- subdat_with_broods[subdat_with_broods$broods == "multiple", ]



spmulti <- unique(subdat_multivoltine$genus_species)
spid<-as.numeric(as.factor(subdat_multivoltine$genus_species))

yer <- as.numeric(as.factor(subdat_multivoltine$Year))
year <- unique(subdat_multivoltine$Year)


pdf(paste("multivoltine_species_spline_Castle_Peak.pdf",sep=""),width=9,height=12)
par(mfrow=c(4,3))
par(mar=c(4.5,5.5,2.5,1.5))

for(i in 1:length(spmulti)){
  a<-which(spid==i)

plot(subdat_multivoltine[a, "ordDate"], subdat_multivoltine[a, "pa"], pch=19,type = "n", xlab="Ordinal day",ylab="Probability of occurrence",cex.lab=1.5, ylim=c(0,1), cex.axis=1.5, bty = "l")
title(main=bquote(italic(.(spmulti[i]))), cex.main=1.5)

for(j in 1:length(year)){
  b <- which(yer==j & spid==i)
  c <- subdat_multivoltine[b, "ordDate"]
  d <- subdat_multivoltine[b, "pa"]
  #sort ordinal day
  x<-sort(c)
  y <-d[order(c)]


  spline_fit <- smooth.spline(x, y, spar = 0.5)

  #lines(spline_fit,  type = "l", xlab="Ordinal day",ylab="Probability of occurrence",col=alpha(cyr[b],0.9),cex.lab=1.5, lwd=1.5, cex.axis=1.5)
  lines(spline_fit,  type = "l", xlab="Ordinal day",ylab="Probability of occurrence",col=cs[j],cex.lab=1.5, lwd=1.5, cex.axis=1.5, lty=ls[j]) 
}
}

dev.off()

pdf(paste("multivoltine_species_gam_Castle_Peak.pdf",sep=""),width=9,height=12)
par(mfrow = c(4, 3))
par(mar = c(4.5, 5.5, 2.5, 1.5))

for (i in 1:length(spmulti)) {
  a <- which(spid == i)

  plot(subdat_multivoltine[a, "ordDate"],
       subdat_multivoltine[a, "pa"],
       pch = 19,
       type = "n",
       xlab = "Ordinal day",
       ylab = "Probability of occurrence",
       cex.lab = 1.5,
       ylim = c(0, 1),
       cex.axis = 1.5,
       bty = "l")
  title(main = bquote(italic(.(spmulti[i]))), cex.main = 1.5)

  for (j in 1:length(year)) {
    b <- which(yer == j & spid == i)

    if (length(b) > 6) {  # only fit GAM if enough data
      c <- subdat_multivoltine[b, "ordDate"]
      d <- subdat_multivoltine[b, "pa"]

      datj <- data.frame(ordDate = c, pa = d)

      fit <- gam(pa ~ s(ordDate, k = 5), data = datj, family = binomial)  # use binomial for probabilities

      ordseq <- seq(min(c), max(c), length.out = 100)
      preds <- predict(fit, newdata = data.frame(ordDate = ordseq), type = "response")

      #lines(ordseq, preds, col = alpha(cyr[b], 0.9), lwd = 1.5)
      lines(ordseq, preds, col = cs[j], lwd = 1.5, lty=ls[j])
            
    }
  }
}

dev.off()

#DONNER PASS
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Donner Pass.rdat')

sub_dat <- sub_dat[ , c("genus_species", "Year", "pa", "ordDate")]


subdat_with_broods <- merge(sub_dat, nath_dp, by = "genus_species")

subdat_multivoltine <- subdat_with_broods[subdat_with_broods$broods == "multiple", ]



spmulti <- unique(subdat_multivoltine$genus_species)
spid<-as.numeric(as.factor(subdat_multivoltine$genus_species))

yer <- as.numeric(as.factor(subdat_multivoltine$Year))
year <- unique(subdat_multivoltine$Year)

pdf(paste("multivoltine_species_spline_Donner_Pass.pdf",sep=""),width=9,height=12)
par(mfrow=c(4,3))
par(mar=c(4.5,5.5,2.5,1.5))

for(i in 1:length(spmulti)){
  a<-which(spid==i)

plot(subdat_multivoltine[a, "ordDate"], subdat_multivoltine[a, "pa"], pch=19,type = "n", xlab="Ordinal day",ylab="Probability of occurrence",cex.lab=1.5, ylim=c(0,1), cex.axis=1.5, bty = "l")
title(main=bquote(italic(.(spmulti[i]))), cex.main=1.5)

#mtext(expression(italic("(f) ", spmulti[i])), side = 3, line = 0.5, adj = 0, cex = 1.5)

for(j in 1:length(year)){
  b <- which(yer==j & spid==i)
  c <- subdat_multivoltine[b, "ordDate"]
  d <- subdat_multivoltine[b, "pa"]
  #sort ordinal day
  x<-sort(c)
  y <-d[order(c)]


  spline_fit <- smooth.spline(x, y, spar = 0.5)

  #lines(spline_fit,  type = "l", xlab="Ordinal day",ylab="Probability of occurrence",col=alpha(cyr[b],0.9),cex.lab=1.5, lwd=1.5, cex.axis=1.5)
  lines(spline_fit,  type = "l", xlab="Ordinal day",ylab="Probability of occurrence",col=cs[j],cex.lab=1.5, lwd=1.5, cex.axis=1.5, lty=ls[j]) 
}
}

dev.off()

pdf(paste("multivoltine_species_gam_Donner_Pass.pdf",sep=""),width=9,height=12)
par(mfrow = c(4, 3))
par(mar = c(4.5, 5.5, 2.5, 1.5))

for (i in 1:length(spmulti)) {
  a <- which(spid == i)

  plot(subdat_multivoltine[a, "ordDate"],
       subdat_multivoltine[a, "pa"],
       pch = 19,
       type = "n",
       xlab = "Ordinal day",
       ylab = "Probability of occurrence",
       cex.lab = 1.5,
       ylim = c(0, 1),
       cex.axis = 1.5,
       bty = "l")
  title(main = bquote(italic(.(spmulti[i]))), cex.main = 1.5)

  for (j in 1:length(year)) {
    b <- which(yer == j & spid == i)

    if (length(b) > 6) {  # only fit GAM if enough data
      c <- subdat_multivoltine[b, "ordDate"]
      d <- subdat_multivoltine[b, "pa"]

      datj <- data.frame(ordDate = c, pa = d)

      fit <- gam(pa ~ s(ordDate, k = 5), data = datj, family = binomial)  # use binomial for probabilities

      ordseq <- seq(min(c), max(c), length.out = 100)
      preds <- predict(fit, newdata = data.frame(ordDate = ordseq), type = "response")

      #lines(ordseq, preds, col = alpha(cyr[b], 0.9), lwd = 1.5)
      lines(ordseq, preds, col = cs[j], lwd = 1.5, lty=ls[j])
    }
  }
}

dev.off()


#LANG CROSSING
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Lang Crossing.rdat')
sub_dat <- sub_dat[ , c("genus_species", "Year", "pa", "ordDate")]


subdat_with_broods <- merge(sub_dat, nath_lc, by = "genus_species")

subdat_multivoltine <- subdat_with_broods[subdat_with_broods$broods == "multiple", ]



spmulti <- unique(subdat_multivoltine$genus_species)
spid<-as.numeric(as.factor(subdat_multivoltine$genus_species))

yer <- as.numeric(as.factor(subdat_multivoltine$Year))
year <- unique(subdat_multivoltine$Year)

pdf(paste("multivoltine_species_spline_Lang_Crossing.pdf",sep=""),width=9,height=12)
par(mfrow=c(4,3))
par(mar=c(4.5,5.5,2.5,1.5))

for(i in 1:length(spmulti)){
  a<-which(spid==i)

plot(subdat_multivoltine[a, "ordDate"], subdat_multivoltine[a, "pa"], pch=19,type = "n", xlab="Ordinal day",ylab="Probability of occurrence",cex.lab=1.5, ylim=c(0,1), cex.axis=1.5, bty = "l")
title(main=bquote(italic(.(spmulti[i]))), cex.main=1.5)

#mtext(expression(italic("(f) ", spmulti[i])), side = 3, line = 0.5, adj = 0, cex = 1.5)

for(j in 1:length(year)){
  b <- which(yer==j & spid==i)
  c <- subdat_multivoltine[b, "ordDate"]
  d <- subdat_multivoltine[b, "pa"]
  #sort ordinal day
  x<-sort(c)
  y <-d[order(c)]


  spline_fit <- smooth.spline(x, y, spar = 0.5)

  #lines(spline_fit,  type = "l", xlab="Ordinal day",ylab="Probability of occurrence",col=alpha(cyr[b],0.9),cex.lab=1.5, lwd=1.5, cex.axis=1.5)
  lines(spline_fit,  type = "l", xlab="Ordinal day",ylab="Probability of occurrence",col=cs[j],cex.lab=1.5, lwd=1.5, cex.axis=1.5, lty=ls[j]) 
}
}

dev.off()



pdf(paste("multivoltine_species_gam_Lang_Crossing.pdf",sep=""),width=9,height=12)

par(mfrow = c(4, 3))
par(mar = c(4.5, 5.5, 2.5, 1.5))

for (i in 1:length(spmulti)) {
  a <- which(spid == i)

  plot(subdat_multivoltine[a, "ordDate"],
       subdat_multivoltine[a, "pa"],
       pch = 19,
       type = "n",
       xlab = "Ordinal day",
       ylab = "Probability of occurrence",
       cex.lab = 1.5,
       ylim = c(0, 1),
       cex.axis = 1.5,
       bty = "l")
  title(main = bquote(italic(.(spmulti[i]))), cex.main = 1.5)

  for (j in 1:length(year)) {
    b <- which(yer == j & spid == i)

    if (length(b) > 6) {  # only fit GAM if enough data
      c <- subdat_multivoltine[b, "ordDate"]
      d <- subdat_multivoltine[b, "pa"]

      datj <- data.frame(ordDate = c, pa = d)

      fit <- gam(pa ~ s(ordDate, k = 5), data = datj, family = binomial)  # use binomial for probabilities

      ordseq <- seq(min(c), max(c), length.out = 100)
      preds <- predict(fit, newdata = data.frame(ordDate = ordseq), type = "response")

      #lines(ordseq, preds, col = alpha(cyr[b], 0.9), lwd = 1.5)
      lines(ordseq, preds, col = cs[j], lwd = 1.5, lty=ls[j])
    }
  }
}

dev.off()




#SIERRA VALLEY
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Sierra Valley.rdat')


sub_dat <- sub_dat[ , c("genus_species", "Year", "pa", "ordDate")]


subdat_with_broods <- merge(sub_dat, nath_sv, by = "genus_species")

subdat_multivoltine <- subdat_with_broods[subdat_with_broods$broods == "multiple", ]



spmulti <- unique(subdat_multivoltine$genus_species)
spid<-as.numeric(as.factor(subdat_multivoltine$genus_species))

yer <- as.numeric(as.factor(subdat_multivoltine$Year))
year <- unique(subdat_multivoltine$Year)

pdf(paste("multivoltine_species_spline_Sierra_Valley.pdf",sep=""),width=9,height=12)

par(mfrow=c(4,3))
par(mar=c(4.5,5.5,2.5,1.5))

for(i in 1:length(spmulti)){
  a<-which(spid==i)

plot(subdat_multivoltine[a, "ordDate"], subdat_multivoltine[a, "pa"], pch=19,type = "n", xlab="Ordinal day",ylab="Probability of occurrence",cex.lab=1.5, ylim=c(0,1), cex.axis=1.5, bty = "l")
title(main=bquote(italic(.(spmulti[i]))), cex.main=1.5)

#mtext(expression(italic("(f) ", spmulti[i])), side = 3, line = 0.5, adj = 0, cex = 1.5)

for(j in 1:length(year)){
  b <- which(yer==j & spid==i)
  c <- subdat_multivoltine[b, "ordDate"]
  d <- subdat_multivoltine[b, "pa"]
  #sort ordinal day
  x<-sort(c)
  y <-d[order(c)]


  spline_fit <- smooth.spline(x, y, spar = 0.5)

  #lines(spline_fit,  type = "l", xlab="Ordinal day",ylab="Probability of occurrence",col=alpha(cyr[b],0.9),cex.lab=1.5, lwd=1.5, cex.axis=1.5)
  lines(spline_fit,  type = "l", xlab="Ordinal day",ylab="Probability of occurrence",col=cs[j],cex.lab=1.5, lwd=1.5, cex.axis=1.5, lty=ls[j]) 
}
}

dev.off()



pdf(paste("multivoltine_species_gam_Sierra_Valley.pdf",sep=""),width=9,height=12)

par(mfrow = c(4, 3))
par(mar = c(4.5, 5.5, 2.5, 1.5))

for (i in 1:length(spmulti)) {
  a <- which(spid == i)

  plot(subdat_multivoltine[a, "ordDate"],
       subdat_multivoltine[a, "pa"],
       pch = 19,
       type = "n",
       xlab = "Ordinal day",
       ylab = "Probability of occurrence",
       cex.lab = 1.5,
       ylim = c(0, 1),
       cex.axis = 1.5,
       bty = "l")
  title(main = bquote(italic(.(spmulti[i]))), cex.main = 1.5)

  for (j in 1:length(year)) {
    b <- which(yer == j & spid == i)

    if (length(b) > 6) {  # only fit GAM if enough data
      c <- subdat_multivoltine[b, "ordDate"]
      d <- subdat_multivoltine[b, "pa"]

      datj <- data.frame(ordDate = c, pa = d)

      fit <- gam(pa ~ s(ordDate, k = 5), data = datj, family = binomial)  # use binomial for probabilities

      ordseq <- seq(min(c), max(c), length.out = 100)
      preds <- predict(fit, newdata = data.frame(ordDate = ordseq), type = "response")

      #lines(ordseq, preds, col = alpha(cyr[b], 0.9), lwd = 1.5)
      lines(ordseq, preds, col = cs[j], lwd = 1.5, lty=ls[j])
    }
  }
}
dev.off()


#WASHINGTON
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Washington.rdat')


sub_dat <- sub_dat[ , c("genus_species", "Year", "pa", "ordDate")]


subdat_with_broods <- merge(sub_dat, nath_wa, by = "genus_species")

subdat_multivoltine <- subdat_with_broods[subdat_with_broods$broods == "multiple", ]



spmulti <- unique(subdat_multivoltine$genus_species)
spid<-as.numeric(as.factor(subdat_multivoltine$genus_species))

yer <- as.numeric(as.factor(subdat_multivoltine$Year))
year <- unique(subdat_multivoltine$Year)

pdf(paste("multivoltine_species_spline_Washington.pdf",sep=""),width=9,height=12)

par(mfrow=c(4,3))
par(mar=c(4.5,5.5,2.5,1.5))

for(i in 1:length(spmulti)){
  a<-which(spid==i)

plot(subdat_multivoltine[a, "ordDate"], subdat_multivoltine[a, "pa"], pch=19,type = "n", xlab="Ordinal day",ylab="Probability of occurrence",cex.lab=1.5, ylim=c(0,1), cex.axis=1.5, bty = "l")
title(main=bquote(italic(.(spmulti[i]))), cex.main=1.5)

#mtext(expression(italic("(f) ", spmulti[i])), side = 3, line = 0.5, adj = 0, cex = 1.5)

for(j in 1:length(year)){
  b <- which(yer==j & spid==i)
  c <- subdat_multivoltine[b, "ordDate"]
  d <- subdat_multivoltine[b, "pa"]
  #sort ordinal day
  x<-sort(c)
  y <-d[order(c)]


  spline_fit <- smooth.spline(x, y, spar = 0.5)

  #lines(spline_fit,  type = "l", xlab="Ordinal day",ylab="Probability of occurrence",col=alpha(cyr[b],0.9),cex.lab=1.5, lwd=1.5, cex.axis=1.5)
  lines(spline_fit,  type = "l", xlab="Ordinal day",ylab="Probability of occurrence",col=cs[j],cex.lab=1.5, lwd=1.5, cex.axis=1.5, lty=ls[j]) 
}
}

dev.off()

pdf(paste("multivoltine_species_gam_Washington.pdf",sep=""),width=9,height=12)

par(mfrow = c(4, 3))
par(mar = c(4.5, 5.5, 2.5, 1.5))

for (i in 1:length(spmulti)) {
  a <- which(spid == i)

  plot(subdat_multivoltine[a, "ordDate"],
       subdat_multivoltine[a, "pa"],
       pch = 19,
       type = "n",
       xlab = "Ordinal day",
       ylab = "Probability of occurrence",
       cex.lab = 1.5,
       ylim = c(0, 1),
       cex.axis = 1.5,
       bty = "l")
  title(main = bquote(italic(.(spmulti[i]))), cex.main = 1.5)

  for (j in 1:length(year)) {
    b <- which(yer == j & spid == i)

    if (length(b) > 6) {  # only fit GAM if enough data
      c <- subdat_multivoltine[b, "ordDate"]
      d <- subdat_multivoltine[b, "pa"]

      datj <- data.frame(ordDate = c, pa = d)

      fit <- gam(pa ~ s(ordDate, k = 5), data = datj, family = binomial)  # use binomial for probabilities

      ordseq <- seq(min(c), max(c), length.out = 100)
      preds <- predict(fit, newdata = data.frame(ordDate = ordseq), type = "response")

      #lines(ordseq, preds, col = alpha(cyr[b], 0.9), lwd = 1.5)
      lines(ordseq, preds, col = cs[j], lwd = 1.5, lty=ls[j])
    }
  }
}

dev.off()

