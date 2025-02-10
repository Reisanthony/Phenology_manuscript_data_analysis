library(rstan)
library(scales)
library(VennDiagram)


#CASTLE PEAK
load('YEAR_PHENOLOGY_INTERACTION_Castle Peak.rdat')
csp= length(sp)
cssp <- sp

#DONNER PASS
load('YEAR_PHENOLOGY_INTERACTION_Donner Pass.rdat')
dsp= length(sp)
dssp <- sp

#LANG CROSSING
load('YEAR_PHENOLOGY_INTERACTION_Lang Crossing.rdat')
lsp= length(sp)
lssp <- sp

#SIERRA VALLEY
load('YEAR_PHENOLOGY_INTERACTION_Sierra Valley.rdat')
ssp= length(sp)
sssp <- sp

#WASHINGTON
load('YEAR_PHENOLOGY_INTERACTION_Washington.rdat')
wsp= length(sp)
wssp <- sp

cols <- c("#ca3542", "#276478",  "#849fad", "#FAAC77", "#57575f")

# Create the Venn diagram
venn.plot <- venn.diagram(
  x = list(Set1 = cssp, Set2 = dssp , Set3 = lssp, Set4 = sssp, Set5 = wssp),
  category.names = c("CP", "DP", "LC", "SV", "WA"), fill = cols,  col = cols, 
  filename = NULL, cat.col = cols
)


pdf(paste("species_summary_venn_diagram.pdf",sep=""), height = 10, width = 10)
par(oma=c(2,2,2,2))
grid.draw(venn.plot)
dev.off()




#species present at all site
all_site_sp_list <- list(cssp, dssp , lssp, sssp, wssp)
# species present in all sites
common_sp <- Reduce(intersect, all_site_sp_list)
common_sp


# species present at two or more sites
all_site_sp_vec <- c(cssp, dssp , lssp, sssp, wssp)
# frequency table
freq_table <- table(all_site_sp_vec)
# species present in 2 or more sites
at_least_two <- names(freq_table[freq_table >= 2])
# Print elements
at_least_two


CP <- rep(NA,135)
DP <- rep(NA,135)
LC <- rep(NA,135)
SV <- rep(NA,135)
WA <- rep(NA,135)
#Species_at_all_sites <- rep(NA,135)
#Species_at_two_or_more_sites <- rep(NA,135)
All_species <- rep(NA,135)

every <- cbind(CP, DP, LC, SV, WA, Species_at_all_sites, Species_at_two_or_more_sites, All_species)
every[1:64,1] <- cssp
every[1:88,2] <- dssp
every[1:83,3] <- lssp 
every[1:70,4] <- sssp 
every[1:80,5] <- wssp 
every[1:26,6] <- common_sp 
every[1:104,7] <- at_least_two
every[,8] <- unique(all_site_sp_vec)


write.csv(every, "species_summary.csv", row.names = FALSE)


CP <- rep(NA,135)
DP <- rep(NA,135)
LC <- rep(NA,135)
SV <- rep(NA,135)
WA <- rep(NA,135)

allsp <- unique(c(cssp, dssp , lssp, sssp, wssp))
allsp<- sort(allsp)
match_1 <- which(allsp %in% cssp)
match_2 <- which(allsp %in% dssp)
match_3 <- which(allsp %in% lssp)
match_4 <- which(allsp %in% sssp)
match_5 <- which(allsp %in% wssp)

cpx <- rep("X", length(cssp))
dpx <- rep("X", length(dssp))
lcx <- rep("X", length(lssp))
svx <- rep("X", length(sssp))
wax <- rep("X", length(wssp))

CP[match_1] <- cpx
DP[match_2] <- dpx
LC[match_3] <- lcx
SV[match_4] <- svx
WA[match_5] <- wax

All_species <- allsp

every <- cbind(All_species, CP, DP, LC, SV, WA)
write.csv(every, "species_summary_with_X.csv", row.names = FALSE)
