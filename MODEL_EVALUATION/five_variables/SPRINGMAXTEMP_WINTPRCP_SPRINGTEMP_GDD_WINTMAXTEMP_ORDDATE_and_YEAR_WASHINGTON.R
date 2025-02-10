library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(scales)
library(fields)  


## helper functions
stand<-function(x=NA){
  x<-(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
  return(x)
}

standNew<-function(x=NA,newx){
  newx<-(newx-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
  return(newx)
}



## read in the data
dat<-read.csv("Montane_sites_2023_with_Daymet_data.csv")
table(dat$site_name)

#  Castle Peak   Donner Pass Lang Crossing Sierra Valley    Washington 
#        41890        103477        102832        101514        108523

sites<-unique(dat$site_name) 
myargs<-commandArgs(trailingOnly=TRUE)
j<-as.numeric(myargs[1])
# "Castle Peak"   "Donner Pass"   "Lang Crossing" "Sierra Valley" "Washington" 
j<-5
cat("working on site",sites[j],"\n")
sub_dat<-dat[dat$site_name==sites[j],]
spKeep<-names(which(tapply(sub_dat$pa,INDEX=sub_dat$genus_species,sum) > 10))
sub_dat<-sub_dat[ (sub_dat$genus_species %in% spKeep),]
sp<-unique(sub_dat$genus_species)

bb<-as.matrix(cbind(stand(sub_dat$spring_tmax), stand(sub_dat$winter_prcp), stand(sub_dat$spring_tmin), stand(sub_dat$estGdd), 
                    stand(sub_dat$estGdd)^2, stand(sub_dat$winter_tmax), stand(sub_dat$ordDate), stand(sub_dat$ordDate)^2, 
                    stand(sub_dat$Year)))
bbi<-as.matrix(cbind(bb,  bb[,1]*bb[,7], bb[,1]*bb[,8], bb[,2]*bb[,7], bb[,2]*bb[,8], bb[,3]*bb[,7], bb[,3]*bb[,8],
                     bb[,4]*bb[,7], bb[,4]*bb[,8], bb[,6]*bb[,7], bb[,6]*bb[,8]))

D<-list(X=bbi,N=dim(bbi)[1],K=19,L=length(sp),y=sub_dat$pa,ll=as.numeric(as.factor(sub_dat$genus_species)))
fit<-stan("hmodel_without_forecast.stan",data=D,iter=4000,warmup=2000) ## default is warmup = 1/2 iter

save(list=ls(),file=paste("SPRINGMAXTEMP_WINTPRCP_SPRINGTEMP_GDD_WINTMAXTEMP_ORDDATE_and_YEAR_",sites[j],".rdat",sep=""))



