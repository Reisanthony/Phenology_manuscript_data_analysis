library(corrplot)

dat <- read.csv("Montane_sites_2023_with_Daymet_data.csv")

#castle peak
CP <- unique(dat[dat$site_name == "Castle Peak", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])
colnames(CP) <- c("YEAR", "SMAXT", "SMINT", "WPREC")
CP_COR <- cor(CP)

#donner pass
DP <- unique(dat[dat$site_name == "Donner Pass", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])
colnames(DP) <- c("YEAR", "SMAXT", "SMINT", "WPREC")
DP_COR <- cor(DP)

#lang crossing
LC <- unique(dat[dat$site_name == "Lang Crossing", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])
colnames(LC) <- c("YEAR", "SMAXT", "SMINT", "WPREC")
LC_COR <- cor(LC)

#sierra valley         
SV <- unique(dat[dat$site_name == "Sierra Valley", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])
colnames(SV) <- c("YEAR", "SMAXT", "SMINT", "WPREC")
SV_COR <- cor(SV)

#washington         
WA <- unique(dat[dat$site_name == "Washington", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])
colnames(WA) <- c("YEAR", "SMAXT", "SMINT", "WPREC")
WA_COR <- cor(WA)

pdf("correlation_between_predictor_variables.pdf", width = 17, height = 4)
layout(matrix(c(1,2,3,4,5), 1, 5))
par(mar = c(3, 4.5, 4.5, 4.5))

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

#castle peak
corrplot(CP_COR, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, cl.pos = "n", tl.cex = 2.3, number.cex = 2.3)
mtext("(a) CP", side = 3, line = 0.3, adj = 0, cex = 2)

#donner pass
corrplot(DP_COR, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, cl.pos = "n", tl.cex = 2.3, number.cex = 2.3)
mtext("(b) DP", side = 3, line = 0.3, adj = 0, cex = 2)


#lang crossing
corrplot(LC_COR, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, cl.pos = "n" , tl.cex = 2.3, number.cex = 2.3)
mtext("(c) LC", side = 3, line = 0.3, adj = 0, cex = 2)

         
         
#sierra valley         
corrplot(SV_COR, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, cl.pos = "n", tl.cex = 2.3, number.cex = 2.3)
mtext("(d) SV", side = 3, line = 0.3, adj = 0, cex = 2)

         
#washington         
corrplot(WA_COR, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, cl.pos = "n", tl.cex = 2.3, number.cex = 2.3)
mtext("(e) WA", side = 3, line = 0.3, adj = 0, cex = 2)

dev.off()



