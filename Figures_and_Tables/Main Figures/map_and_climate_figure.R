
pdf(paste("map_and_climate_figure.pdf",sep=""), width=16, height=10)
par(oma=c(0, 1, 0, 0), mar=c(8, 8, 3, 4))
layout(matrix(c(1, 1, 2, 2, 2, 2,  
                3, 3, 4, 4, 5, 5), 2, 6, byrow=T))

library(scales)
library(maps)

## site and color vectors
sites <- c("Castle Peak","Donner Pass","Lang Crossing", "Sierra Valley","Washington")
cols <- c("#ca3542", "#276478",  "#849fad", "#FAAC77", "#57575f")


#Figure 2a (Inset)
x1 <- -123.3; x2 <- -117.4; y1 <- 37; y2 <- 40.3
plot(seq(x1, x2, length = 10), seq(y1, y2, length = 10), type = "n", axes = F, xlab = "", ylab = "", main = "", cex.main = 1.5)  
mtext("(a) Map", side = 3, line = 0.5, adj = 0.6, cex = 2)

#the map
gray1 <- "gray98"
map("state", "California", xlim = c(x1, x2), ylim = c(y1, y2), fill = T, col = gray1, add = TRUE)
map("state", "Nevada", xlim = c(x1, x2), ylim = c(y1, y2), fill = T, col = gray1, add = TRUE)

# border
box(col = "#8B0000", lwd=2)
#bottom axis
axis(1, labels = F)
axis(1, tick = F, labels = c(-123, -122, -121, -120, -119, -118), at = c(-123, -122, -121, -120, -119, -118), cex.axis = 2.5, line = 0.6)
#right axis
axis(4, labels = F)
axis(4, tick = F, at = c(37, 38, 39, 40), cex.axis = 2.5, line = -0.2, las = 2)


text(-121.8,38.7,"California",cex=2.5)
text(-118.2,38.7,"Nevada",cex=2.5)
arrows(-121,37.25,-119.5,37.25,length=0.03,angle=90,code=3,lwd=1.5,lty=1)
text(-120.28,37.48,"150km",cex=2.5)

#labels
mtext("Degrees latitude", side = 4, line = 5, cex= 2)
mtext("Degrees longitude", side = 1, line = 4, cex= 2)

sitePoints <- read.csv("siteData.csv",header=T)
head(sitePoints)
sizes <-c(3.9,3.5,3.2,3.0)
for(i in 1:length(sizes)){siz <- sizes[i]
points(sitePoints$Longitude[10]+0.06,sitePoints$Latitude[10],pch=21,bg=cols[1],col="black",lwd=0.75,cex=siz) #cp
points(sitePoints$Longitude[9]-0.08,sitePoints$Latitude[9],pch=21,bg=cols[2],col="black",lwd=0.75,cex=siz) #dp
points(sitePoints$Longitude[8],sitePoints$Latitude[8],pch=21,bg=cols[3],col="black",lwd=0.75,cex=siz) #lc
points(sitePoints$Longitude[7],sitePoints$Latitude[7],pch=21,bg=cols[4],col="black",lwd=0.75,cex=siz) #sv
points(sitePoints$Longitude[6]-0.08,sitePoints$Latitude[6]-0.07,pch=21,bg=cols[5],col="black",lwd=0.75,cex=siz) #wa
}


# SAVE PANEL (a) FIG REGION 
fig_a <- par("fig")



#Figure 2b (Elevation profile)
dat <- read.table("profile2.txt",header=T)
head(dat)
dat <- dat[318:987,]
summary(dat)
sizesa <-c(3.9,3.5,3.2,3.0)

plot(dat$x,dat$y,col="white",ylim=c(0.5,10),xlim=c(4,10),type="n",xlab="",ylab="",axes=F, main = "")
mtext("(b) Elevation", side = 3, line = 0.5, adj = 0.5, cex = 2)
polygon(dat$x,dat$y,col="gray98",border = NA)
spl <- smooth.spline(dat$x,dat$y,df=25)
lines(spl,lwd=2)

lines(x=c(min(dat$x),max(dat$x)), y=c(min(dat$y),min(dat$y)), lwd=2 )
lines(x=c(max(dat$x),max(dat$x)), y=c(min(dat$y), 4.5 ), lwd=2 )

text(7,3.95,"2000m",col="black", cex=2.5)
lines(c(7,7),c(1.45,3.5),lty=,col="black",lwd=1.5)
lines(c(7,7),c(4.3,6.8),lty=,col="black", lwd=1.5)
lines(c(6.9,7.1),c(6.8,6.8),lty=,col="black", lwd=1.5)
lines(c(6.9,7.1),c(1.45,1.45),lty=,col="black", lwd=1.5)

#castle peak
x<-7.45;y<-8.95
for(i in 1:length(sizesa)){
  siz <- sizesa[i]
  points(x,y,pch=21,bg=cols[1],col="black",lwd=0.75,cex=siz) 
}

#donner pass
x<-6.45;y<-7.25
for(i in 1:length(sizesa)){
  siz <- sizesa[i]
  points(x,y,pch=21,bg=cols[2],col="black",lwd=0.75,cex=siz) 
}

#langcrossing
x<-5.8;y<-5.8
for(i in 1:length(sizesa)){
  siz <- sizesa[i]
  points(x,y,pch=21,bg=cols[3],col="black",lwd=0.75,cex=siz)
}

#sierra valley
x<-8.6;y<-5.8
for(i in 1:length(sizesa)){
  siz <- sizesa[i]
  points(x,y,pch=21,bg=cols[4],col="black",lwd=0.75,cex=siz)
}

#washington
x<-5.35;y<-4.3
for(i in 1:length(sizesa)){
  siz <- sizesa[i]
  points(x,y,pch=21,bg=cols[5],col="black",lwd=0.75,cex=siz) 
}


par(xpd = NA)

usr <- par("usr")

legend(
  x = usr[1] - 0.3,  # just left of plot
  y = mean(usr[3:4]) + 1.5,                  # vertically centered
  legend = c("CP", "DP", "LC", "SV", "WA"),
  col = cols,
  pch = 19,
  lwd = 2,
  pt.cex = 3,
  cex = 2,
  bty = "O",
  xjust = 0,   # left-justify legend box
  yjust = 0.5
)

par(xpd = FALSE)







## Figure 2 c, d, e (Interannual variation in climate)
dat<-read.csv("Montane_sites_2023_with_Daymet_data.csv")
table(dat$site_name)
sites<-unique(dat$site_name)
myargs<-commandArgs(trailingOnly=TRUE)

# Castle Peak   
j <- 1
castle <- dat[dat$site_name==sites[j],]
spKeep <- names(which(tapply(castle$pa,INDEX=castle$genus_species,sum) > 10))
castle <- castle[(castle$genus_species %in% spKeep), ]
castleyears <- unique(castle$Year)


# Donner Pass 
j <- 2
donner <- dat[dat$site_name==sites[j],]
spKeep <- names(which(tapply(donner$pa,INDEX=donner$genus_species,sum) > 10))
donner <- donner[(donner$genus_species %in% spKeep), ]
donneryears <- unique(donner$Year)

#Lang Crossing" 
j <- 3
lang <- dat[dat$site_name==sites[j],]
spKeep <- names(which(tapply(lang$pa,INDEX=lang$genus_species,sum) > 10))
lang <- lang[(lang$genus_species %in% spKeep), ]
langyears <- unique(lang$Year)
year3 <- lang$Year

#Sierra Valley
j <- 4
sierra <- dat[dat$site_name==sites[j],]
spKeep <- names(which(tapply(sierra$pa,INDEX=sierra$genus_species,sum) > 10))
sierra <- sierra[(sierra$genus_species %in% spKeep), ]
sierrayears <- unique(sierra$Year)
year4 <- sierra$Year


#Washington
j <- 5
washington <- dat[dat$site_name==sites[j],]
spKeep <- names(which(tapply(washington$pa,INDEX=washington$genus_species,sum) > 10))
washington <- washington[(washington$genus_species %in% spKeep), ]
washingtonyears <- unique(washington$Year)
year5 <- washington$Year

unique(castle$site_name)
unique(donner$site_name)
unique(lang$site_name)
unique(sierra$site_name)
unique(washington$site_name)




#  spring max temp
castlespring_tmaxers <- c()
castlespring_tmax <- c()
for (year in 1:length(castleyears)) {
  castlespring_tmaxers[year] <- castle[castle$Year == castleyears[year], 3]
  castlespring_tmax[year] <- unique(castlespring_tmaxers[year])
}
donnerspring_tmaxers <- c()
donnerspring_tmax <- c()
for (year in 1:length(donneryears)) {
  donnerspring_tmaxers[year] <- donner[donner$Year == donneryears[year], 3]
  donnerspring_tmax[year] <- unique(donnerspring_tmaxers[year])
}
langspring_tmaxers <- c()
langspring_tmax <- c()
for (year in 1:length(langyears)) {
  langspring_tmaxers[year] <- lang[lang$Year == langyears[year], 3]
  langspring_tmax[year] <- unique(langspring_tmaxers[year])
}
sierraspring_tmaxers <- c()
sierraspring_tmax <- c()
for (year in 1:length(sierrayears)) {
  sierraspring_tmaxers[year] <- sierra[sierra$Year == sierrayears[year], 3]
  sierraspring_tmax[year] <- unique(sierraspring_tmaxers[year])
}
washingtonspring_tmaxers <- c()
washingtonspring_tmax <- c()
for (year in 1:length(washingtonyears)) {
  washingtonspring_tmaxers[year] <- washington[washington$Year == washingtonyears[year], 3]
  washingtonspring_tmax[year] <- unique(washingtonspring_tmaxers[year])
}


#  winter precipitation
castlewinter_prcpers <- c()
castlewinter_prcp <- c()
for (year in 1:length(castleyears)) {
  castlewinter_prcpers[year] <- castle[castle$Year == castleyears[year], 8]
  castlewinter_prcp[year] <- unique(castlewinter_prcpers[year])
}

donnerwinter_prcpers <- c()
donnerwinter_prcp <- c()
for (year in 1:length(donneryears)) {
  donnerwinter_prcpers[year] <- donner[donner$Year == donneryears[year], 8]
  donnerwinter_prcp[year] <- unique(donnerwinter_prcpers[year])
}

langwinter_prcpers <- c()
langwinter_prcp <- c()
for (year in 1:length(langyears)) {
  langwinter_prcpers[year] <- lang[lang$Year == langyears[year], 8]
  langwinter_prcp[year] <- unique(langwinter_prcpers[year])
}

sierrawinter_prcpers <- c()
sierrawinter_prcp <- c()
for (year in 1:length(sierrayears)) {
  sierrawinter_prcpers[year] <- sierra[sierra$Year == sierrayears[year], 8]
  sierrawinter_prcp[year] <- unique(sierrawinter_prcpers[year])
}

washingtonwinter_prcpers <- c()
washingtonwinter_prcp <- c()
for (year in 1:length(washingtonyears)) {
  washingtonwinter_prcpers[year] <- washington[washington$Year == washingtonyears[year], 8]
  washingtonwinter_prcp[year] <- unique(washingtonwinter_prcpers[year])
}

#  spring min temp
castlespring_tminers <- c()
castlespring_tmin <- c()
for (year in 1:length(castleyears)) {
  castlespring_tminers[year] <- castle[castle$Year == castleyears[year], 4]
  castlespring_tmin[year] <- unique(castlespring_tminers[year])
}

donnerspring_tminers <- c()
donnerspring_tmin <- c()
for (year in 1:length(donneryears)) {
  donnerspring_tminers[year] <- donner[donner$Year == donneryears[year], 4]
  donnerspring_tmin[year] <- unique(donnerspring_tminers[year])
}

langspring_tminers <- c()
langspring_tmin <- c()
for (year in 1:length(langyears)) {
  langspring_tminers[year] <- lang[lang$Year == langyears[year], 4]
  langspring_tmin[year] <- unique(langspring_tminers[year])
}

sierraspring_tminers <- c()
sierraspring_tmin <- c()
for (year in 1:length(sierrayears)) {
  sierraspring_tminers[year] <- sierra[sierra$Year == sierrayears[year], 4]
  sierraspring_tmin[year] <- unique(sierraspring_tminers[year])
}

washingtonspring_tminers <- c()
washingtonspring_tmin <- c()
for (year in 1:length(washingtonyears)) {
  washingtonspring_tminers[year] <- washington[washington$Year == washingtonyears[year], 4]
  washingtonspring_tmin[year] <- unique(washingtonspring_tminers[year])
}


#plots
#Spring maximum temperature
plot(dat$Year, dat$spring_tmax, type = "n", xlab = "Year",
     ylab = expression(paste("Spring max temperature (", degree, "C)")),
     main = "", ylim=c(min(dat$spring_tmax),max(dat$spring_tmax)+1),
     cex.axis=2.8, cex.lab=2.8, mgp=c(5, 2, 0))

mtext("(c) Spring max temperature", side = 3, line = 0.5, adj = 0, cex = 2)
colors <- c("#ca3542", "#276478",  "#849fad", "#FAAC77", "#57575f")
colors <- unique(colors)
lines(castleyears, castlespring_tmax, type = "l", col = colors[1], lwd = 2)
lines(donneryears, donnerspring_tmax, type = "l", col = colors[2], lwd = 2)
lines(langyears, langspring_tmax, type = "l", col = colors[3], lwd = 2)
lines(sierrayears, sierraspring_tmax, type = "l", col = colors[4], lwd = 2)
lines(washingtonyears, washingtonspring_tmax, type = "l", col = colors[5], lwd = 2)

#winter precipitation
plot(dat$Year, dat$winter_prcp, type = "n", xlab = "Year",
     ylab = "Winter precipitation(mm/day)",
     main = "", ylim=c(min(dat$winter_prcp),max(dat$winter_prcp)+1),
     cex.axis=2.8, cex.lab=2.8, mgp=c(5, 2, 0))

mtext("(d) Winter precipitation", side = 3, line = 0.5, adj = 0, cex = 2)
colors <- c("#ca3542", "#276478",  "#849fad", "#FAAC77", "#57575f")
colors <- unique(colors)
lines(castleyears, castlewinter_prcp, type = "l", col = colors[1], lwd = 2)
lines(donneryears, donnerwinter_prcp, type = "l", col = colors[2], lwd = 2)
lines(langyears, langwinter_prcp, type = "l", col = colors[3], lwd = 2)
lines(sierrayears, sierrawinter_prcp, type = "l", col = colors[4], lwd = 2)
lines(washingtonyears, washingtonwinter_prcp, type = "l", col = colors[5], lwd = 2)

#Spring minimum temperature
plot(dat$Year, dat$spring_tmin, type = "n", xlab = "Year",
     ylab = expression(paste("Spring min temperature (", degree, "C)")),
     main = "", ylim=c(min(dat$spring_tmin),max(dat$spring_tmin)+1),
     cex.axis=2.8, cex.lab=2.8, mgp=c(5, 2, 0))

mtext("(e) Spring min temperature", side = 3, line = 0.5, adj = 0, cex = 2)
colors <- c("#ca3542", "#276478",  "#849fad", "#FAAC77", "#57575f")
colors <- unique(colors)
lines(castleyears, castlespring_tmin, type = "l", col = colors[1], lwd = 2)
lines(donneryears, donnerspring_tmin, type = "l", col = colors[2], lwd = 2)
lines(langyears, langspring_tmin, type = "l", col = colors[3], lwd = 2)
lines(sierrayears, sierraspring_tmin, type = "l", col = colors[4], lwd = 2)
lines(washingtonyears, washingtonspring_tmin, type = "l", col = colors[5], lwd = 2)



#USA map (top-right corner of the figure 2a)
par(
  fig = c(fig_a[2] - 0.23, fig_a[2] - 0.03,
          fig_a[4] - 0.18, fig_a[4] - 0.07),
  new = TRUE,
  mar = c(0, 0, 0, 0)
)

y1 <- 24.396308; y2 <- 49.384358
x1 <- -179.148909; x2 <- -66.93457
plot(seq(x1, x2, length=10), seq(y1, y2, length=10), type="n", axes=F, xlab="", ylab="", main = "", cex.main=1.5) 
map("state", "", xlim=c(x1, x2), ylim=c(y1, y2), fill=T, col=gray1, border = "black", lwd = 1.5, add=TRUE)
x1 <- -123.3; x2 <- -117.4; y1 <- 37; y2 <- 40.3
rect(x1, y1, x2, y2, border = "#8B0000", lwd = 2)


par(new = FALSE)


dev.off()

