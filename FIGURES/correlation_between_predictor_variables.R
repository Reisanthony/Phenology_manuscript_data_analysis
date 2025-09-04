
library(corrplot)

# matrix of the p-value of the correlation
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}



dat <- read.csv("Montane_sites_2023_with_Daymet_data.csv")
table(dat$site_name)

# Extract unique site names
sites <- unique(dat$site_name)

# Site-level subsetting
sub_cp <- unique(dat[dat$site_name == "Castle Peak", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])
sub_dp <- unique(dat[dat$site_name == "Donner Pass", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])
sub_lc <- unique(dat[dat$site_name == "Lang Crossing", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])
sub_sv <- unique(dat[dat$site_name == "Sierra Valley", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])
sub_wa <- unique(dat[dat$site_name == "Washington", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])



pdf("correlation_between_predictor_variables.pdf", width = 17, height = 4)

# Set up plot layout
layout(matrix(c(1,2,3,4,5), 1, 5))
#par(mfrow = c(1, 5), mar = c(4.5, 4.5, 3.5, 3))
par(mar = c(3, 4.5, 4.5, 4.5))

CP <- cbind(sub_cp$Year, sub_cp$spring_tmax, sub_cp$spring_tmin, sub_cp$winter_prcp)
colnames(CP) <- c("YEAR", "SMAXT", "SMINT", "WPREC")
CP_COR <- cor(CP)
# matrix of the p-value of the correlation
CP_p.mat <- cor.mtest(CP)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(CP_COR, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, cl.pos = "n", tl.cex = 2, number.cex = 2 
         )
mtext("(a) CP", side = 3, line = 0.3, adj = 0, cex = 2)


DP <- cbind(sub_dp$Year, sub_dp$spring_tmax, sub_dp$spring_tmin, sub_dp$winter_prcp)
colnames(DP) <- c("YEAR", "SMAXT", "SMINT", "WPREC")
DP_COR <- cor(DP)
# matrix of the p-value of the correlation
DP_p.mat <- cor.mtest(DP)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(DP_COR, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, cl.pos = "n", tl.cex = 2, number.cex = 2  
         )
mtext("(b) DP", side = 3, line = 0.3, adj = 0, cex = 2)



LC <- cbind(sub_lc$Year, sub_lc$spring_tmax, sub_lc$spring_tmin, sub_lc$winter_prcp)
colnames(LC) <- c("YEAR", "SMAXT", "SMINT", "WPREC")
LC_COR <- cor(LC)
# matrix of the p-value of the correlation
LC_p.mat <- cor.mtest(LC)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(LC_COR, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, cl.pos = "n" , tl.cex = 2, number.cex = 2 
         )
mtext("(c) LC", side = 3, line = 0.3, adj = 0, cex = 2)

         
         
         
SV <- cbind(sub_sv$Year, sub_sv$spring_tmax, sub_sv$spring_tmin, sub_sv$winter_prcp)
colnames(SV) <- c("YEAR", "SMAXT", "SMINT", "WPREC")
SV_COR <- cor(SV)
# matrix of the p-value of the correlation
SV_p.mat <- cor.mtest(SV)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(SV_COR, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, cl.pos = "n", tl.cex = 2, number.cex = 2  
         )
mtext("(d) SV", side = 3, line = 0.3, adj = 0, cex = 2)

         
         
WA <- cbind(sub_wa$Year, sub_wa$spring_tmax, sub_wa$spring_tmin, sub_wa$winter_prcp)
colnames(WA) <- c("YEAR", "SMAXT", "SMINT", "WPREC")
WA_COR <- cor(WA)
# matrix of the p-value of the correlation
WA_p.mat <- cor.mtest(WA)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(WA_COR, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, cl.pos = "n", tl.cex = 2, number.cex = 2 
         )
mtext("(e) WA", side = 3, line = 0.3, adj = 0, cex = 2)

         
 dev.off()



