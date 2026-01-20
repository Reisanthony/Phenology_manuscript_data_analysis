library(rstan)
library(scales)

#CASTLE PEAK
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Castle Peak.rdat')
csp= length(sp)
cssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
cb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
cb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
cb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
cb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
cb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
cb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
cb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
cb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
cb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


cpspbe <- cbind(cb1, cb7, cb8, cb2, cb9, cb10, cb3, cb11, cb12)
colnames(cpspbe) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                      "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                      "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")
rownames(cpspbe) <- sp


#DONNER PASS
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Donner Pass.rdat')
dsp= length(sp)
dssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
db1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
db7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
db8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
db2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
db9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
db10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
db3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
db11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
db12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)

dpspbe <- cbind(db1, db7, db8, db2, db9, db10, db3, db11, db12)
colnames(dpspbe) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                      "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                      "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")
rownames(dpspbe) <- sp


#LANG CROSSING
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Lang Crossing.rdat')
lsp= length(sp)
lssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
lb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
lb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
lb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
lb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
lb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
lb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
lb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
lb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
lb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)

lcspbe <- cbind(lb1, lb7, lb8, lb2, lb9, lb10, lb3, lb11, lb12)
colnames(lcspbe) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                      "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                      "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")
rownames(lcspbe) <- sp



#SIERRA VALLEY
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Sierra Valley.rdat')
ssp= length(sp)
sssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
sb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
sb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
sb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
sb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
sb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
sb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
sb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
sb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
sb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


svspbe <- cbind(sb1, sb7, sb8, sb2, sb9, sb10, sb3, sb11, sb12)
colnames(svspbe) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                      "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                      "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")
rownames(svspbe) <- sp



#WASHINGTON
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Washington.rdat')
wsp= length(sp)
wssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
wb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
wb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
wb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
wb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
wb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
wb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
wb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
wb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
wb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


waspbe <- cbind(wb1, wb7, wb8, wb2, wb9, wb10, wb3, wb11, wb12)

colnames(waspbe) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                      "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                      "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")

rownames(waspbe) <- sp























#CASTLE PEAK
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_2017_Castle Peak.rdat')
csp= length(sp)
cssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
cb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
cb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
cb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
cb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
cb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
cb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
cb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
cb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
cb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)

cpspbe_2017 <- cbind(cb1, cb7, cb8, cb2, cb9, cb10, cb3, cb11, cb12)
colnames(cpspbe_2017) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                           "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                           "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")
rownames(cpspbe_2017) <- sp


#DONNER PASS
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_2017_Donner Pass.rdat')
dsp= length(sp)
dssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
db1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
db7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
db8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
db2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
db9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
db10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
db3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
db11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
db12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)

dpspbe_2017 <- cbind(db1, db7, db8, db2, db9, db10, db3, db11, db12)
colnames(dpspbe_2017) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                           "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                           "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")
rownames(dpspbe_2017) <- sp


#LANG CROSSING
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_2017_Lang Crossing.rdat')
lsp= length(sp)
lssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
lb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
lb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
lb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
lb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
lb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
lb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
lb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
lb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
lb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)

lcspbe_2017 <- cbind(lb1, lb7, lb8, lb2, lb9, lb10, lb3, lb11, lb12)

colnames(lcspbe_2017) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                           "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                           "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")

rownames(lcspbe_2017) <- sp



#SIERRA VALLEY
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_2017_Sierra Valley.rdat')
ssp= length(sp)
sssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
sb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
sb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
sb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
sb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
sb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
sb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
sb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
sb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
sb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)

svspbe_2017 <- cbind(sb1, sb7, sb8, sb2, sb9, sb10, sb3, sb11, sb12)

colnames(svspbe_2017) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                           "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                           "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")
rownames(svspbe_2017) <- sp



#WASHINGTON
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_2017_Washington.rdat')
wsp= length(sp)
wssp = sp
#beta
betas<-extract(fit,"beta")

#spring max temperature
#MO, TO, LO
wb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)
wb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)
wb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#MO, TO, LO
wb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)
wb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)
wb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#MO, TO, LO
wb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)
wb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)
wb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)

waspbe_2017 <- cbind(wb1, wb7, wb8, wb2, wb9, wb10, wb3, wb11, wb12)

colnames(waspbe_2017) <- c("sprmaxtemp-MO", "sprmaxtemp-TO", "sprmaxtemp-LO",
                           "wintprcp-MO", "wintprcp-TO", "wintprcp-LO",
                           "sprmintemp-MO", "sprmintemp-TO", "sprmintemp-LO")

rownames(waspbe_2017) <- sp





pdf(paste("correlation_between_full_effects_and_2017_effects_by_variable.pdf",sep=""),width=25,height=13)

figname <- c("SMAXT-MO", "SMAXT-TO", "SMAXT-LO", "WPREC-MO", "WPREC-TO", "WPREC-LO", "SMINT-MO", "SMINT-TO", "SMINT-LO")

# Set up layout for 45 plots
par(mfrow = c(5, 9), mar = c(2, 1, 2, 1), oma = c(3, 3, 4, 1))

# Site list
site_names <- c("CP", "DP", "LC", "SV", "WA")

for (site in site_names) {
  mat_full <- get(tolower(paste0(site, "spbe")))
  mat_2017 <- get(tolower(paste0(site, "spbe_2017")))
  
  # Align rows between full and 2017 matrices
  matched_idx <- match(rownames(mat_2017), rownames(mat_full))
  matched_idx <- na.omit(matched_idx)
  mat_full <- mat_full[matched_idx, ]
  mat_2017 <- mat_2017[rownames(mat_full), ]
  
  for (i in seq_len(ncol(mat_full))) {
    x <- mat_full[, i]
    y <- mat_2017[, i]
    
    plot(x, y,
         xlab = "", ylab = "", main = "", pch = 16, cex = 1.5,
         col = rgb(0, 0, 0, 0.5), xaxt = "n", yaxt = "n")
    
    r <- round(cor(x, y, use = "complete.obs"), 2)
    text(min(x, na.rm = TRUE), max(y, na.rm = TRUE)-0.2, labels = paste0("r=", r),
         pos = 4, cex = 3, col = "blue")
    
    
    mtext(paste0(site, ": ", figname[i]), side = 3, line = 0.3, cex = 2)

  }
}
dev.off()
