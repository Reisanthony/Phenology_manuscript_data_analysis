library(rstan)
library(scales)
library(vegan)
library(parallel)

#CASTLE PEAK
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Castle Peak.rdat')
csp= length(sp)
cssp = sp
#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
cb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)

#timing
cb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)

#length
cb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#peak
cb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)

#timing
cb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)

#length
cb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#peak
cb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)

#timing
cb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)

#length
cb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


cpspbe <- cbind(cb1, cb7, cb8, cb2, cb9, cb10, cb3, cb11, cb12)
colnames(cpspbe) <- c("sprmaxtemp-Peak", "sprmaxtemp-Timing", "sprmaxtemp-Length",
                      "wintprcp-Peak", "wintprcp-Timing", "wintprcp-Length",
                      "sprmintemp-Peak", "sprmintemp-Timing", "sprmintemp-Length")
rownames(cpspbe) <- sp


#DONNER PASS
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Donner Pass.rdat')
dsp= length(sp)
dssp = sp
#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
db1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)

#timing
db7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)

#length
db8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#peak
db2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)

#timing
db9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)

#length
db10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#peak
db3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)

#timing
db11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)

#length
db12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


dpspbe <- cbind(db1, db7, db8, db2, db9, db10, db3, db11, db12)
colnames(dpspbe) <- c("sprmaxtemp-Peak", "sprmaxtemp-Timing", "sprmaxtemp-Length",
                      "wintprcp-Peak", "wintprcp-Timing", "wintprcp-Length",
                      "sprmintemp-Peak", "sprmintemp-Timing", "sprmintemp-Length")
rownames(dpspbe) <- sp


#LANG CROSSING
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Lang Crossing.rdat')
lsp= length(sp)
lssp = sp
#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
lb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)

#timing
lb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)

#length
lb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#peak
lb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)

#timing
lb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)

#length
lb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#peak
lb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)

#timing
lb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)

#length
lb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


lcspbe <- cbind(lb1, lb7, lb8, lb2, lb9, lb10, lb3, lb11, lb12)

colnames(lcspbe) <- c("sprmaxtemp-Peak", "sprmaxtemp-Timing", "sprmaxtemp-Length",
                      "wintprcp-Peak", "wintprcp-Timing", "wintprcp-Length",
                      "sprmintemp-Peak", "sprmintemp-Timing", "sprmintemp-Length")

rownames(lcspbe) <- sp



#SIERRA VALLEY
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Sierra Valley.rdat')
ssp= length(sp)
sssp = sp
#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
sb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)

#timing
sb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)

#length
sb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#peak
sb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)

#timing
sb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)

#length
sb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#peak
sb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)

#timing
sb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)

#length
sb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


svspbe <- cbind(sb1, sb7, sb8, sb2, sb9, sb10, sb3, sb11, sb12)

colnames(svspbe) <- c("sprmaxtemp-Peak", "sprmaxtemp-Timing", "sprmaxtemp-Length",
                      "wintprcp-Peak", "wintprcp-Timing", "wintprcp-Length",
                      "sprmintemp-Peak", "sprmintemp-Timing", "sprmintemp-Length")
rownames(svspbe) <- sp



#WASHINGTON
load('SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_Washington.rdat')
wsp= length(sp)
wssp = sp
#beta
betas<-extract(fit,"beta")
#spring max temperature
#peak
wb1<-apply(betas[[1]][,,1],2,quantile,probs=0.5)

#timing
wb7<-apply(betas[[1]][,,7],2,quantile,probs=0.5)

#length
wb8<-apply(betas[[1]][,,8],2,quantile,probs=0.5)

#winter precipitation
#peak
wb2<-apply(betas[[1]][,,2],2,quantile,probs=0.5)

#timing
wb9<-apply(betas[[1]][,,9],2,quantile,probs=0.5)

#length
wb10<-apply(betas[[1]][,,10],2,quantile,probs=0.5)

#spring min temperature
#peak
wb3<-apply(betas[[1]][,,3],2,quantile,probs=0.5)

#timing
wb11<-apply(betas[[1]][,,11],2,quantile,probs=0.5)

#length
wb12<-apply(betas[[1]][,,12],2,quantile,probs=0.5)


waspbe <- cbind(wb1, wb7, wb8, wb2, wb9, wb10, wb3, wb11, wb12)

colnames(waspbe) <- c("sprmaxtemp-Peak", "sprmaxtemp-Timing", "sprmaxtemp-Length",
                      "wintprcp-Peak", "wintprcp-Timing", "wintprcp-Length",
                      "sprmintemp-Peak", "sprmintemp-Timing", "sprmintemp-Length")

rownames(waspbe) <- sp








#climate variables for each year at each site
sub_cp <- unique(dat[dat$site_name == "Castle Peak", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])
sub_dp <- unique(dat[dat$site_name == "Donner Pass", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])
sub_lc <- unique(dat[dat$site_name == "Lang Crossing", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])
sub_sv <- unique(dat[dat$site_name == "Sierra Valley", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])
sub_wa <- unique(dat[dat$site_name == "Washington", c("Year", "spring_tmax", "spring_tmin", "winter_prcp")])




# mean and standard deviation at each site
climcp <- apply(sub_cp[, -1], 2, function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
climdp <- apply(sub_dp[, -1], 2, function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
climlc <- apply(sub_lc[, -1], 2, function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
climsv <- apply(sub_sv[, -1], 2, function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
climwa <- apply(sub_wa[, -1], 2, function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))

#making data frame with each of those in coloums
climcp <- c(climcp[1,], climcp[2,])
climdp <- c(climdp[1,], climdp[2,])
climlc <- c(climlc[1,], climlc[2,])
climsv <- c(climsv[1,], climsv[2,])
climwa <- c(climwa[1,], climwa[2,])


clims <- rbind(climcp, climdp, climlc, climsv, climwa)
clims <- as.data.frame(clims)
colnames(clims) <- c("mean_spring_tmax", "mean_spring_tmin", "mean_winter_prcp", "sd_spring_tmax", "sd_spring_tmin", "sd_winter_prcp")

clims$mean_spring_tmax <- as.numeric(clims$mean_spring_tmax)
clims$mean_spring_tmin <- as.numeric(clims$mean_spring_tmin)
clims$mean_winter_prcp <- as.numeric(clims$mean_winter_prcp)
clims$sd_spring_tmax <- as.numeric(clims$sd_spring_tmax)
clims$sd_spring_tmin <- as.numeric(clims$sd_spring_tmin)
clims$sd_winter_prcp <- as.numeric(clims$sd_winter_prcp)

# Standardize climate variables
 clims <- scale(clims)
 
 #combine sites
clims <- cbind(clims, sites)

#rename for merging later
colnames(clims)[7] <- "site_name"



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


nath <- nath[, c( "genus_species", "site_name", "resident", "weedy", "broods", "wintering", "hostgenera")]


nath_cp <- nath[nath$site_name == "Castle Peak", ]
nath_dp <- nath[nath$site_name == "Donner Pass", ]
nath_sv <- nath[nath$site_name == "Sierra Valley", ]
nath_lc <- nath[nath$site_name == "Lang Crossing", ]
nath_wa <- nath[nath$site_name == "Washington", ]


mt_1 <- na.omit(match(cssp, nath_cp$genus_species))
nath_cp <- nath_cp[mt_1, ]

mt_2 <- na.omit(match(dssp, nath_dp$genus_species))
nath_dp <- nath_dp[mt_2, ]

mt_3 <- na.omit(match(lssp, nath_lc$genus_species))
nath_lc <- nath_lc[mt_3, ]

mt_4 <- na.omit(match(sssp, nath_sv$genus_species))
nath_sv <- nath_sv[mt_4, ]

mt_5 <- na.omit(match(wssp, nath_wa$genus_species))
nath_wa <- nath_wa[mt_5, ]

nathcomb <- rbind(nath_cp, nath_dp, nath_lc, nath_sv, nath_wa)


#merge clima nd nath
clim_nath <- merge(nathcomb, clims, by = "site_name", all.x = TRUE)

#factor natural history traits
clim_nath$resident    <- as.factor(clim_nath$resident)
clim_nath$weedy       <- as.factor(clim_nath$weedy)
clim_nath$broods      <- as.factor(clim_nath$broods)
clim_nath$wintering   <- as.factor(clim_nath$wintering)
clim_nath$hostgenera  <- as.factor(clim_nath$hostgenera)
clim_nath$site_name   <- as.factor(clim_nath$site_name)



#betas
cpspbes <- cpspbe[match(nath_cp$genus_species, cssp),]
dpspbes <- dpspbe[match(nath_dp$genus_species, dssp),]
lcspbes <- lcspbe[match(nath_lc$genus_species, lssp),]
svspbes <- svspbe[match(nath_sv$genus_species, sssp),]
waspbes <- waspbe[match(nath_wa$genus_species, wssp),]

betacomb <- rbind(cpspbes, dpspbes, lcspbes, svspbes, waspbes)



# remove NAs
rows <- complete.cases(betacomb, clim_nath)
betacomb <- betacomb[rows, ]
clim_nath    <- clim_nath[rows, ]

# climate mean and sd
climate <- clim_nath[, c("mean_spring_tmax", "mean_spring_tmin", "mean_winter_prcp",
                              "sd_spring_tmax", "sd_spring_tmin", "sd_winter_prcp")]
#natural history traits
trait <- clim_nath[, c("resident", "weedy", "broods", "wintering", "hostgenera")]

#sites
ltreb <- clim_nath[, "site_name"]



#full model
rda_full <- rda(betacomb ~ mean_spring_tmax + mean_spring_tmin + mean_winter_prcp +
                                   sd_spring_tmax + sd_spring_tmin + sd_winter_prcp +
                                   resident + weedy + broods + wintering + hostgenera,
                                   data = clim_nath)
#summary(rda_full)
RsquareAdj(rda_full)
anova(rda_full, parallel = detectCores() - 2)


#climate conditioned on trait model
rda_clim_cond_trait <- rda(betacomb ~ mean_spring_tmax + mean_spring_tmin + mean_winter_prcp +
                 sd_spring_tmax + sd_spring_tmin + sd_winter_prcp +
                 Condition(resident + weedy + broods + wintering + hostgenera), data = clim_nath)
#summary(rda_clim_cond_trait)
RsquareAdj(rda_clim_cond_trait)
anova(rda_clim_cond_trait, parallel = detectCores() - 2)


#trait conditioned on climate model
rda_trait_cond_clim <- rda(betacomb ~ resident + weedy + broods + wintering + hostgenera +
                 Condition(mean_spring_tmax + mean_spring_tmin + mean_winter_prcp +
                 sd_spring_tmax + sd_spring_tmin + sd_winter_prcp), data = clim_nath)
#summary(rda_trait_cond_clim)
RsquareAdj(rda_trait_cond_clim)
anova(rda_trait_cond_clim, parallel = detectCores() - 2)











#sites instead of climate
#full model
rda_full_site <- rda(betacomb ~ site_name + resident + weedy + broods + wintering + hostgenera, data = clim_nath)
#summary(rda_full_site)
RsquareAdj(rda_full_site)
anova(rda_full_site, parallel = detectCores() - 2)


#site conditioned on trait model
rda_site_cond_trait <- rda(betacomb ~ site_name + Condition(resident + weedy + broods + wintering + hostgenera), data = clim_nath)
#summary(rda_site_cond_trait)
RsquareAdj(rda_site_cond_trait)
anova(rda_site_cond_trait, parallel = detectCores() - 2)


#trait conditioned on climate model
rda_trait_cond_site <- rda(betacomb ~ resident + weedy + broods + wintering + hostgenera + Condition(site_name), data = clim_nath)
#summary(rda_trait_cond_site)
RsquareAdj(rda_trait_cond_site)
anova(rda_trait_cond_site, parallel = detectCores() - 2)












#just traits
#trait full model
rda_trait <- rda(betacomb ~ resident + weedy + broods + wintering + hostgenera, data = trait)
#summary(rda_trait)
RsquareAdj(rda_trait)
anova(rda_trait, parallel = detectCores() - 2)


#individual trait model
# List to store results
rda_indiv_traits <- list()
anova_results <- list()
rsq_adj_results <- list()
trait_names <- colnames(trait)


# Loop through each trait
for (tr in trait_names) {
  # Current trait as predictor
  predictors <- trait[, tr, drop = FALSE]
  
  # Other traits as conditioning variables
  conditions <- trait[, setdiff(trait_names, tr), drop = FALSE]
  
  # Build the RDA model
  formula_str <- as.formula(paste("betacomb ~", tr, "+ Condition(", 
                                  paste(setdiff(trait_names, tr), collapse = " + "), ")"))
  
  model <- rda(formula_str, data = trait)
  
  # Store results
  rda_indiv_traits[[tr]] <- model
  anova_results[[tr]] <- anova(model, parallel = detectCores() - 2)
  rsq_adj_results[[tr]] <- RsquareAdj(model)
}


#summary(rda_indiv_traits[["resident"]])
anova_results[["resident"]]
rsq_adj_results[["resident"]]

#summary(rda_indiv_traits[["weedy"]])
anova_results[["weedy"]]
rsq_adj_results[["weedy"]]

#summary(rda_indiv_traits[["broods"]])
anova_results[["broods"]]
rsq_adj_results[["broods"]]

#summary(rda_indiv_traits[["wintering"]])
anova_results[["wintering"]]
rsq_adj_results[["wintering"]]

#summary(rda_indiv_traits[["hostgenera"]])
anova_results[["hostgenera"]]
rsq_adj_results[["hostgenera"]]


# Filter to only traits with valid adj R² and p-value
valid_traits <- names(rsq_adj_results)[
  sapply(rsq_adj_results, function(x) length(x$adj.r.squared) > 0) &
  sapply(anova_results, function(x) !is.null(x$`Pr(>F)`) && length(x$`Pr(>F)`) > 0)
]

# Now build the summary table
trait_model <- data.frame(
  trait = valid_traits,
  adj_R2 = sapply(valid_traits, function(tr) rsq_adj_results[[tr]]$adj.r.squared),
  p_value = sapply(valid_traits, function(tr) anova_results[[tr]]$`Pr(>F)`[1])
)


trait_model
