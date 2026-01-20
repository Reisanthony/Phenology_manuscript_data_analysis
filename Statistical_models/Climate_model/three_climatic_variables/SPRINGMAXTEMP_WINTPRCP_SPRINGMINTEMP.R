library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(scales)
library(fields)  

# Function to standardize a numeric variable (z-score)
stand<-function(x=NA){
  x<-(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
  return(x)
}

## Butterfly and climate data
dat<-read.csv("Montane_sites_2023_with_Daymet_data.csv")
table(dat$site_name)
sites<-unique(dat$site_name) 

for (j in 1:length(sites)) {
  
  # Site Selection 
  # 1.Castle Peak 2.Donner Pass 3.Lang Crossing 4.Sierra Valley 5.Washington" 
  cat("working on site",sites[j],"\n")
  
  # Subset data for selected site
  sub_dat<-dat[dat$site_name==sites[j],]
  
  # Filtering species
  # Keep species with >10 presence records
  spKeep<-names(which(tapply(sub_dat$pa,INDEX=sub_dat$genus_species,sum) > 10))
  sub_dat<-sub_dat[ (sub_dat$genus_species %in% spKeep),]
  sp<-unique(sub_dat$genus_species)
  
  # Covariate matrix
bb<-as.matrix(cbind(stand(sub_dat$spring_tmax), stand(sub_dat$winter_prcp), stand(sub_dat$spring_tmin), stand(sub_dat$ordDate),
                    stand(sub_dat$ordDate)^2, stand(sub_dat$Year)))
bbi<-as.matrix(cbind(bb,  bb[,1]*bb[,4], bb[,1]*bb[,5], bb[,2]*bb[,4], bb[,2]*bb[,5], bb[,3]*bb[,4], bb[,3]*bb[,5]))
# Stan data and model fitting
D<-list(X=bbi,N=dim(bbi)[1],K=12,L=length(sp),y=sub_dat$pa,ll=as.numeric(as.factor(sub_dat$genus_species)))
fit<-stan("hmodel_without_forecast.stan",data=D,iter=4000,warmup=2000) ## default is warmup = 1/2 iter

save(list=ls(),file=paste("SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_ORDDATE_and_YEAR_",sites[j],".rdat",sep=""))


}