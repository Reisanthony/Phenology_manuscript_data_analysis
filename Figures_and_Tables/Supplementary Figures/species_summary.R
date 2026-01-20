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



pdf(paste("species_summary_venn_diagram.pdf",sep=""), height = 8, width = 8.5)
par(oma=c(3,3,3,3))
cols <- c("#ca3542", "#276478",  "#849fad", "#FAAC77", "#57575f")

venn.plot <- venn.diagram(
  x = list(Set1 = cssp, Set2 = dssp , Set3 = lssp, Set4 = sssp, Set5 = wssp),
  category.names = c("CP", "DP", "LC", "SV", "WA"),
  fill = cols,  col = cols, filename = NULL, cat.col = cols
)
grid.draw(venn.plot)
dev.off()

