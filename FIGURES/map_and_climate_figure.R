library(scales)
library(maps)



## site and color vectors
sites <- c("Castle Peak","Donner Pass","Lang Crossing", "Sierra Valley","Washington")
cols <- c("#ca3542", "#276478",  "#849fad", "#FAAC77", "#57575f")

pdf(paste("map_and_climate_figure.pdf",sep=""),width=24,height=10)

par(oma=c(0,1,0,0), mar=c(5,5,3,3.5))

#layout(matrix(c(1,1,0,3,3,
 #        1,1,2,3,3,
  #       4,4,2,5,5,
   #      4,4,0,5,5),4,5,byrow=T))

#layout(matrix(c(1,2,3,4,5,6),2,3,byrow=T))



layout(matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 4,
                1, 1, 1, 1, 0, 0, 3, 3, 3, 3, 3, 0,
                5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7,
                5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7),4,12,byrow=T))



x1 <- -123.3; x2 <- -117.4; y1 <- 37; y2 <- 40.3
plot(seq(x1, x2, length = 10), seq(y1, y2, length = 10), type = "n", axes = F, xlab = "", ylab = "", main = "", cex.main = 1.5)  # (a) Map
mtext("(a) Map", side = 3, line = 0.5, adj = 0.9, cex = 1.5)
#the map
gray1 <- "gray98"
map("state", "California", xlim = c(x1, x2), ylim = c(y1, y2), fill = T, col = gray1, add = TRUE)
map("state", "Nevada", xlim = c(x1, x2), ylim = c(y1, y2), fill = T, col = gray1, add = TRUE)
# #00008B border
box(col = "#00008B", lwd=2)
#bottom axis
axis(1, labels = F, col = "#00008B", col.ticks = "#00008B")
axis(1, tick = F, labels = c(-123, -122, -121, -120, -119, -118), at = c(-123, -122, -121, -120, -119, -118), 
     cex.axis = 1.5, line = -0.2, col.axis = "#00008B", col = "#00008B", col.ticks = "#00008B")
#right axis
axis(4, labels = F, col = "#00008B", col.ticks = "#00008B")
axis(4, tick = F, at = c(37, 38, 39, 40), cex.axis = 1.5, line = -0.2, las = 2, col.axis = "#00008B", col = "#00008B", col.ticks = "#00008B")


text(-122.2,39.65,"California",cex=1.8)
text(-118.7,39.65,"Nevada",cex=1.8)
arrows(-121,37.25,-119.5,37.25,length=0.03,angle=90,code=3,lwd=1.5,lty=1)
text(-120.28,37.48,"150km",cex=1.5)

#labels
mtext("Degrees latitude", side = 4, line = 3,cex=1.5, col = "#00008B")
mtext("Degrees longitude", side = 1, line = 3,cex=1.5, col = "#00008B")

sitePoints <- read.csv("siteData.csv",header=T)
head(sitePoints)
#sizes <-c(3.0,2.7,2.3,2.2)
sizes <-c(3.9,3.5,3.2,3.0)
for(i in 1:length(sizes)){siz <- sizes[i]
points(sitePoints$Longitude[10]+0.06,sitePoints$Latitude[10],pch=21,bg=cols[1],col="black",lwd=0.75,cex=siz) #cp
points(sitePoints$Longitude[9]-0.08,sitePoints$Latitude[9],pch=21,bg=cols[2],col="black",lwd=0.75,cex=siz) #dp
points(sitePoints$Longitude[8],sitePoints$Latitude[8],pch=21,bg=cols[3],col="black",lwd=0.75,cex=siz) #lc
points(sitePoints$Longitude[7],sitePoints$Latitude[7],pch=21,bg=cols[4],col="black",lwd=0.75,cex=siz) #sv
points(sitePoints$Longitude[6]-0.08,sitePoints$Latitude[6]-0.07,pch=21,bg=cols[5],col="black",lwd=0.75,cex=siz) #wa
}


#  - Whole map (top-right corner of the first plot)
y1 <- 24.396308; y2 <- 49.384358
x1 <- -179.148909; x2 <- -66.93457
plot(seq(x1, x2, length=10), seq(y1, y2, length=10), type="n", axes=F, xlab="", ylab="", main = "", cex.main=1.5)  # Whole Map
map("state", "", xlim=c(x1, x2), ylim=c(y1, y2), fill=T, col=gray1, add=TRUE)
x1 <- -123.3; x2 <- -117.4; y1 <- 37; y2 <- 40.3
rect(x1, y1, x2, y2, border = "#00008B")




###################################################
########################################
##############second plot



dat <- read.table("profile2.txt",header=T)
head(dat)
dat <- dat[318:987,]
summary(dat)
sizesa <-c(3.9,3.5,3.2,3.0)
plot(dat$x,dat$y,col="white",ylim=c(0.5,10),xlim=c(4,10),type="n",xlab="",ylab="",axes=F, main = "", cex.main=1.5)#(b) Elevation plot
mtext("(b) Elevation", side = 3, line = 0.5, adj = 0.5, cex = 1.5)
polygon(dat$x,dat$y,col="gray98",border = NA)
spl <- smooth.spline(dat$x,dat$y,df=25)
lines(spl,lwd=2)

lines(x=c(min(dat$x),max(dat$x)), y=c(min(dat$y),min(dat$y)), lwd=2 )
lines(x=c(max(dat$x),max(dat$x)), y=c(min(dat$y), 4.5 ), lwd=2 )


text(7,3.95,"2000m",col="black", cex=1.5)
lines(c(7,7),c(1.45,3.5),lty=,col="black",lwd=1.5)
lines(c(7,7),c(4.3,6.8),lty=,col="black", lwd=1.5)
lines(c(6.9,7.1),c(6.8,6.8),lty=,col="black", lwd=1.5)
lines(c(6.9,7.1),c(1.45,1.45),lty=,col="black", lwd=1.5)



#text(6.45, 0.7,"100km",col="black", cex=1.5)
#lines(c(5.35,5.83),c(0.7,0.7),lty=,col="black", lwd=1.5)
#lines(c(7.14,7.55),c(0.7,0.7),lty=,col="black",lwd=1.5)
#lines(c(5.35,5.35),c(0.5,0.9),lty=,col="black",lwd=1.5)
#lines(c(7.55,7.55),c(0.5,0.9),lty=,col="black",lwd=1.5)



#castle peak
x<-7.45;y<-8.95
for(i in 1:length(sizesa)){
  siz <- sizesa[i]
  points(x,y,pch=21,bg=cols[1],col="black",lwd=0.75,cex=siz) #cp
}
#text(7.4,9.8,"CP",cex=1.5) 



#donner pass
x<-6.45;y<-7.25
for(i in 1:length(sizes)){
  siz <- sizesa[i]
  points(x,y,pch=21,bg=cols[2],col="black",lwd=0.75,cex=siz) #dp
}
#text(6.4,8.25,"DP",cex=1.5) 

#langcrossing
x<-5.8;y<-5.8
for(i in 1:length(sizes)){
  siz <- sizesa[i]
  points(x,y,pch=21,bg=cols[3],col="black",lwd=0.75,cex=siz)
  #lc
}
#text(5.8,6.8,"LC",cex=1.5) 

#sierra valley
x<-8.6;y<-5.8
for(i in 1:length(sizes)){
  siz <- sizesa[i]
  points(x,y,pch=21,bg=cols[4],col="black",lwd=0.75,cex=siz)
  #sv
}
#text(8.6,6.6,"SV",cex=1.5) 

#washington
x<-5.35;y<-4.3
for(i in 1:length(sizes)){
  siz <- sizesa[i]
  points(x,y,pch=21,bg=cols[5],col="black",lwd=0.75,cex=siz) 
  #wa
}
#text(5.35,5.1,"WA",cex=1.5) 




############Third plot
#Legend
x=y <- 1:2
#(c) Legend
plot(x, y, type = "n", axes = FALSE, xlab = "", ylab = "", main = "", cex.main = 1.5)

legend("center", legend = c("CP", "DP", "LC", "SV", "WA"), 
       col = cols, lwd = 2, cex = 2, bty = "O", pch = 19, 
       horiz = F, x.intersp = 2, inset = c(0, 0), xpd =T, ncol = 1, pt.cex = 3)






#########################################################################################################################
##############################Plots showing the interannual variation in climatic variables###########################
############################################################################################################################

## read in the data
dat<-read.csv("Montane_sites_2023_with_Daymet_data.csv")
table(dat$site_name)
#  Castle Peak   Donner Pass Lang Crossing Sierra Valley    Washington 
#        41890        103477        102832        101514        108523
sites<-unique(dat$site_name)
myargs<-commandArgs(trailingOnly=TRUE)

# Castle Peak   
j <- as.numeric(myargs[1])
j <- 1
cat("working on site",sites[j],"\n")
castle <- dat[dat$site_name==sites[j],]
spKeep <- names(which(tapply(castle$pa,INDEX=castle$genus_species,sum) > 10))
castle <- castle[(castle$genus_species %in% spKeep), ]
castleyears <- unique(castle$Year)


# Donner Pass 
j <- as.numeric(myargs[1])
j <- 2
cat("working on site",sites[j],"\n")
donner <- dat[dat$site_name==sites[j],]
spKeep <- names(which(tapply(donner$pa,INDEX=donner$genus_species,sum) > 10))
donner <- donner[(donner$genus_species %in% spKeep), ]
donneryears <- unique(donner$Year)

#Lang Crossing" 
j <- as.numeric(myargs[1])
j <- 3
cat("working on site",sites[j],"\n")
lang <- dat[dat$site_name==sites[j],]
spKeep <- names(which(tapply(lang$pa,INDEX=lang$genus_species,sum) > 10))
lang <- lang[(lang$genus_species %in% spKeep), ]
langyears <- unique(lang$Year)
year3 <- lang$Year

#Sierra Valley
j <- as.numeric(myargs[1])
j <- 4
cat("working on site",sites[j],"\n")
sierra <- dat[dat$site_name==sites[j],]
spKeep <- names(which(tapply(sierra$pa,INDEX=sierra$genus_species,sum) > 10))
sierra <- sierra[(sierra$genus_species %in% spKeep), ]
sierrayears <- unique(sierra$Year)
year4 <- sierra$Year


#Washington
j <- as.numeric(myargs[1])
j <- 5
cat("working on site",sites[j],"\n")
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



#Figures
#Spring maximum temperature
#(c) Spring maximum temperature
plot(dat$Year, dat$spring_tmax, type = "n", xlab = "Year", ylab = expression(paste("Spring maximum temperature (", degree, "C)")),
     main = "", ylim=c(min(dat$spring_tmax),max(dat$spring_tmax)+1),cex.axis=2, cex.lab=2, cex.main=1.5)

mtext("(c) Spring max temperature", side = 3, line = 0.5, adj = 0, cex = 1.5)
colors <- c("#ca3542", "#276478",  "#849fad", "#FAAC77", "#57575f")
colors <- unique(colors)
lines(castleyears, castlespring_tmax, type = "l", col = colors[1], lwd = 2)
lines(donneryears, donnerspring_tmax, type = "l", col = colors[2], lwd = 2)
lines(langyears, langspring_tmax, type = "l", col = colors[3], lwd = 2)
lines(sierrayears, sierraspring_tmax, type = "l", col = colors[4], lwd = 2)
lines(washingtonyears, washingtonspring_tmax, type = "l", col = colors[5], lwd = 2)

# Add a legend
#legend("topleft", legend = c("CP", "DP", "LC", "SV", "WA"), 
 #      col = colors, lwd = 2, cex = 1.5, ncol=5, bty="n")

#winter precipitation
#(d) Winter precipitation"
plot(dat$Year, dat$winter_prcp, type = "n", xlab = "Year", ylab = "Winter precipitation(mm/day)",
     main = "", ylim=c(min(dat$winter_prcp),max(dat$winter_prcp)+1),cex.axis=2, cex.lab=2, cex.main=1.5)

mtext("(d) Winter precipitation", side = 3, line = 0.5, adj = 0, cex = 1.5)
colors <- c("#ca3542", "#276478",  "#849fad", "#FAAC77", "#57575f")
colors <- unique(colors)
lines(castleyears, castlewinter_prcp, type = "l", col = colors[1], lwd = 2)
lines(donneryears, donnerwinter_prcp, type = "l", col = colors[2], lwd = 2)
lines(langyears, langwinter_prcp, type = "l", col = colors[3], lwd = 2)
lines(sierrayears, sierrawinter_prcp, type = "l", col = colors[4], lwd = 2)
lines(washingtonyears, washingtonwinter_prcp, type = "l", col = colors[5], lwd = 2)

# Add a legend
#legend("topleft", legend = c("CP", "DP", "LC", "SV", "WA"), 
#       col = colors, lwd = 2, cex=1.5, ncol=5, bty="n")


#Spring minimum temperature
#(e) Spring minimum temperature
plot(dat$Year, dat$spring_tmin, type = "n", xlab = "Year", ylab = expression(paste("Spring minimum temperature (", degree, "C)")),
     main = "", ylim=c(min(dat$spring_tmin),max(dat$spring_tmin)+1),cex.axis=2, cex.lab=2, cex.main=1.5)

mtext("(e) Spring min temperature", side = 3, line = 0.5, adj = 0, cex = 1.5)
colors <- c("#ca3542", "#276478",  "#849fad", "#FAAC77", "#57575f")
colors <- unique(colors)
lines(castleyears, castlespring_tmin, type = "l", col = colors[1], lwd = 2)
lines(donneryears, donnerspring_tmin, type = "l", col = colors[2], lwd = 2)
lines(langyears, langspring_tmin, type = "l", col = colors[3], lwd = 2)
lines(sierrayears, sierraspring_tmin, type = "l", col = colors[4], lwd = 2)
lines(washingtonyears, washingtonspring_tmin, type = "l", col = colors[5], lwd = 2)

# Add a legend
#legend("topleft", legend = c("CP", "DP", "LC", "SV", "WA"), 
 #      col = colors, lwd = 2, cex=1.5, ncol=5, bty="n")

dev.off()

