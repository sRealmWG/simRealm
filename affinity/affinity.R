###Script for the analysis of affinity
##Helmut Hillebrand
#2022-03-17

##########################
##Initiate WS, libraries##
##########################

rm(list=ls())
graphics.off()
setwd("~/R/test")

library(tidyverse)
library(vegan)
library(grid)
library(gridExtra)
library(calibrate)
library(psych)
library(reshape2)
#library(agricolae)
#library(ggExtra)
#library(clusterSim)
library(cowplot)
#library(ggridges)
#library(multcomp)
#library(RColorBrewer)
library(kableExtra)
library(effects)
library(lmerTest)
library(nlme)
library(broom)

########################################
## Section 1 testing affinity beta100 ##
########################################

data100 <- read_csv("neutral_v2_beta_dist_100.csv")
summary(data100)
data100$X1<-NULL
unique(data100$timeSeriesID)
data100$USI<-paste(data100$parameter_id,data100$timeSeriesID,sep="_")

affinity100<-data100 %>%
  dplyr::group_by(USI,parameter_id, timeSeriesID) %>%
  dplyr::summarize(ts_max = max(temp_dist,na.rm=T))

fit.jaccard100 <- nlsList(Jbeta ~ SSmicmen(temp_dist, a, b) | USI, data100 ,control = list(maxiter = 500))
out.jaccard100<-as.data.frame(coef(fit.jaccard100))
out.jaccard100$USI<-row.names((out.jaccard100))
out.jaccard100$hd_jaccard<-out.jaccard100$b
out.jaccard100$affinity_jaccard<-out.jaccard100$a/out.jaccard100$b

affinity100<-merge(affinity100,out.jaccard100[,c("USI","hd_jaccard","affinity_jaccard")], by = "USI")

fit.MH_dist100 <- nlsList(MH_dist ~ SSmicmen(temp_dist, a, b) | USI, data100 ,control = list(maxiter = 500))
out.MH_dist100<-as.data.frame(coef(fit.MH_dist100))
out.MH_dist100$USI<-row.names((out.MH_dist100))
out.MH_dist100$hd_MH_dist<-out.MH_dist100$b
out.MH_dist100$affinity_MH_dist<-out.MH_dist100$a/out.MH_dist100$b

affinity100<-merge(affinity100,out.MH_dist100[,c("USI","hd_MH_dist","affinity_MH_dist")], by = "USI")

metadata <- read_csv("~/R/simRealm/data/simulations/neutral_metadata_v2.csv")

affinity100<-merge(metadata,affinity100, by="parameter_id")

summary(affinity100)

affinity100$scenario<-"beta100"


pairs.panels(affinity100[,c("ts_max","THETA", "M", "N",  "hd_jaccard"  ,     "affinity_jaccard"  , "hd_MH_dist" ,"affinity_MH_dist")])



########################################
## Section 2 testing affinity beta75 ##
########################################

data75 <- read_csv("neutral_v2_beta_dist_75.csv")
summary(data75)
data75$X1<-NULL
unique(data75$timeSeriesID)
data75$USI<-paste(data75$parameter_id,data75$timeSeriesID,sep="_")

affinity75<-data75 %>%
  dplyr::group_by(USI,parameter_id, timeSeriesID) %>%
  dplyr::summarize(ts_max = max(temp_dist,na.rm=T))

fit.jaccard75 <- nlsList(Jbeta ~ SSmicmen(temp_dist, a, b) | USI, data75 ,control = list(maxiter = 500))
out.jaccard75<-as.data.frame(coef(fit.jaccard75))
out.jaccard75$USI<-row.names((out.jaccard75))
out.jaccard75$hd_jaccard<-out.jaccard75$b
out.jaccard75$affinity_jaccard<-out.jaccard75$a/out.jaccard75$b

affinity75<-merge(affinity75,out.jaccard75[,c("USI","hd_jaccard","affinity_jaccard")], by = "USI")

fit.MH_dist75 <- nlsList(MH_dist ~ SSmicmen(temp_dist, a, b) | USI, data75 ,control = list(maxiter = 500))
out.MH_dist75<-as.data.frame(coef(fit.MH_dist75))
out.MH_dist75$USI<-row.names((out.MH_dist75))
out.MH_dist75$hd_MH_dist<-out.MH_dist75$b
out.MH_dist75$affinity_MH_dist<-out.MH_dist75$a/out.MH_dist75$b

affinity75<-merge(affinity75,out.MH_dist75[,c("USI","hd_MH_dist","affinity_MH_dist")], by = "USI")

affinity75<-merge(metadata,affinity75, by="parameter_id")

summary(affinity75)

pairs.panels(affinity75[,c("ts_max","THETA", "M", "N",  "hd_jaccard"  ,     "affinity_jaccard"  , "hd_MH_dist" ,"affinity_MH_dist")])


affinity75$scenario<-"beta75"


########################################
## Section 3 testing affinity beta50 ##
########################################

data50 <- read_csv("neutral_v2_beta_dist_50.csv")
summary(data50)
data50$X1<-NULL
unique(data50$timeSeriesID)
data50$USI<-paste(data50$parameter_id,data50$timeSeriesID,sep="_")

affinity50<-data50 %>%
  dplyr::group_by(USI,parameter_id, timeSeriesID) %>%
  dplyr::summarize(ts_max = max(temp_dist,na.rm=T))

fit.jaccard50 <- nlsList(Jbeta ~ SSmicmen(temp_dist, a, b) | USI, data50 ,control = list(maxiter = 500))
out.jaccard50<-as.data.frame(coef(fit.jaccard50))
out.jaccard50$USI<-row.names((out.jaccard50))
out.jaccard50$hd_jaccard<-out.jaccard50$b
out.jaccard50$affinity_jaccard<-out.jaccard50$a/out.jaccard50$b

affinity50<-merge(affinity50,out.jaccard50[,c("USI","hd_jaccard","affinity_jaccard")], by = "USI")

fit.MH_dist50 <- nlsList(MH_dist ~ SSmicmen(temp_dist, a, b) | USI, data50 ,control = list(maxiter = 500))
out.MH_dist50<-as.data.frame(coef(fit.MH_dist50))
out.MH_dist50$USI<-row.names((out.MH_dist50))
out.MH_dist50$hd_MH_dist<-out.MH_dist50$b
out.MH_dist50$affinity_MH_dist<-out.MH_dist50$a/out.MH_dist50$b

affinity50<-merge(affinity50,out.MH_dist50[,c("USI","hd_MH_dist","affinity_MH_dist")], by = "USI")

metadata <- read_csv("~/R/simRealm/data/simulations/neutral_metadata_v2.csv")

affinity50<-merge(metadata,affinity50, by="parameter_id")

summary(affinity50)

affinity50$scenario<-"beta50"


pairs.panels(affinity50[,c("ts_max","THETA", "M", "N",  "hd_jaccard"  ,     "affinity_jaccard"  , "hd_MH_dist" ,"affinity_MH_dist")])



########################################
## Section 4 testing affinity beta25 ##
########################################

data25 <- read_csv("neutral_v2_beta_dist_25.csv")
summary(data25)
data25$X1<-NULL
unique(data25$timeSeriesID)
data25$USI<-paste(data25$parameter_id,data25$timeSeriesID,sep="_")

affinity25<-data25 %>%
  dplyr::group_by(USI,parameter_id, timeSeriesID) %>%
  dplyr::summarize(ts_max = max(temp_dist,na.rm=T))

fit.jaccard25 <- nlsList(Jbeta ~ SSmicmen(temp_dist, a, b) | USI, data25 ,control = list(maxiter = 500))
out.jaccard25<-as.data.frame(coef(fit.jaccard25))
out.jaccard25$USI<-row.names((out.jaccard25))
out.jaccard25$hd_jaccard<-out.jaccard25$b
out.jaccard25$affinity_jaccard<-out.jaccard25$a/out.jaccard25$b

affinity25<-merge(affinity25,out.jaccard25[,c("USI","hd_jaccard","affinity_jaccard")], by = "USI")

fit.MH_dist25 <- nlsList(MH_dist ~ SSmicmen(temp_dist, a, b) | USI, data25 ,control = list(maxiter = 500))
out.MH_dist25<-as.data.frame(coef(fit.MH_dist25))
out.MH_dist25$USI<-row.names((out.MH_dist25))
out.MH_dist25$hd_MH_dist<-out.MH_dist25$b
out.MH_dist25$affinity_MH_dist<-out.MH_dist25$a/out.MH_dist25$b

affinity25<-merge(affinity25,out.MH_dist25[,c("USI","hd_MH_dist","affinity_MH_dist")], by = "USI")

metadata <- read_csv("~/R/simRealm/data/simulations/neutral_metadata_v2.csv")

affinity25<-merge(metadata,affinity25, by="parameter_id")

summary(affinity25)

affinity25$scenario<-"beta25"


pairs.panels(affinity25[,c("ts_max","THETA", "M", "N",  "hd_jaccard"  ,     "affinity_jaccard"  , "hd_MH_dist" ,"affinity_MH_dist")])



aff.all<-rbind(affinity100,affinity75,affinity50,affinity25)

#different Theta exist for N=1000 und m=0.2
#different N exist for M 0.2 und Theta 40
#different M exist for N=1000 und Theta 40
summary(aff.all)
summary(affinity75)
summary(affinity50)
summary(affinity25)
### Plot 1: Jaccard affinity

jac_theta<-ggplot(aff.all[aff.all$N==1000&aff.all$M==0.2,], aes(THETA,log(affinity_jaccard+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("Theta at N=1000, m = 0.2")+
  ylab("Affinity [LN-transformed]")+
  theme_bw()+
  xlab("Species Pool Size [theta]")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
jac_theta


jac_tslength<-ggplot(aff.all[aff.all$N==1000&aff.all$THETA==40&aff.all$M==0.2,], aes(ts_max,log(affinity_jaccard+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("TS length at N=1000, m = 0.2, Theta = 40")+
  ylab("Affinity [LN-transformed]")+
  theme_bw()+
  xlab("Time Series Length")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
jac_tslength


jac_N<-ggplot(aff.all[aff.all$THETA==40&aff.all$M==0.2,], aes(N,log(affinity_jaccard+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("N at m = 0.2, Theta = 40")+
  ylab("Affinity [LN-transformed]")+
  theme_bw()+
  xlab("Number of Individuals")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
jac_N


jac_m<-ggplot(aff.all[aff.all$N==1000&aff.all$THETA==40,], aes(M,log(affinity_jaccard+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("m at N=1000, Theta = 40")+
  ylab("Affinity [LN-transformed]")+
  theme_bw()+
  xlab("Arrival rate (m)")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
jac_m

tiff(file = "affin_jac.tiff", width = 6000, height = 4800, units = "px", res = 400)
cowplot::plot_grid(jac_tslength, jac_theta,
                   jac_N,jac_m,
                   ncol=4,align="h")
dev.off()


### Plot 2: Jaccard halving distance



jacHD_theta<-ggplot(aff.all[aff.all$N==1000&aff.all$M==0.2,], aes(THETA,log(hd_jaccard+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("Theta at N=1000, m = 0.2")+
  ylab("Halving distance [LN-transformed]")+
  theme_bw()+
  xlab("Species Pool Size [theta]")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
jacHD_theta


jacHD_tslength<-ggplot(aff.all[aff.all$N==1000&aff.all$THETA==40&aff.all$M==0.2,], aes(ts_max,log(hd_jaccard+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("TS length at N=1000, m = 0.2, Theta = 40")+
  ylab("Halving distance [LN-transformed]")+
  theme_bw()+
  xlab("Time Series Length")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
jacHD_tslength


jacHD_N<-ggplot(aff.all[aff.all$THETA==40&aff.all$M==0.2,], aes(N,log(hd_jaccard+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("N at m = 0.2, Theta = 40")+
  ylab("Halving distance [LN-transformed]")+
  theme_bw()+
  xlab("Number of Individuals")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
jacHD_N


jacHD_m<-ggplot(aff.all[aff.all$N==1000&aff.all$THETA==40,], aes(M,log(hd_jaccard+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("m at N=1000, Theta = 40")+
  ylab("Halving distance [LN-transformed]")+
  theme_bw()+
  xlab("Arrival rate (m)")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
jacHD_m

tiff(file = "hd_jac.tiff", width = 6000, height = 4800, units = "px", res = 400)
cowplot::plot_grid(jacHD_tslength, jacHD_theta,
                   jacHD_N,jacHD_m,
                   ncol=4,align="h")
dev.off()



### Plot 3: Morisita affinity

MH_theta<-ggplot(aff.all[aff.all$N==1000&aff.all$M==0.2,], aes(THETA,log(affinity_MH_dist+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("Theta at N=1000, m = 0.2")+
  ylab("MH-Affinity [LN-transformed]")+
  theme_bw()+
  xlab("Species Pool Size [theta]")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
MH_theta


MH_tslength<-ggplot(aff.all[aff.all$N==1000&aff.all$THETA==40&aff.all$M==0.2,], aes(ts_max,log(affinity_MH_dist+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("TS length at N=1000, m = 0.2, Theta = 40")+
  ylab("MH-Affinity [LN-transformed]")+
  theme_bw()+
  xlab("Time Series Length")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
MH_tslength


MH_N<-ggplot(aff.all[aff.all$THETA==40&aff.all$M==0.2,], aes(N,log(affinity_MH_dist+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("N at m = 0.2, Theta = 40")+
  ylab("MH-Affinity [LN-transformed]")+
  theme_bw()+
  xlab("Number of Individuals")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
MH_N


MH_m<-ggplot(aff.all[aff.all$N==1000&aff.all$THETA==40,], aes(M,log(affinity_MH_dist+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("m at N=1000, Theta = 40")+
  ylab("MH-Affinity [LN-transformed]")+
  theme_bw()+
  xlab("Arrival rate (m)")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
MH_m

tiff(file = "affin_MH.tiff", width = 6000, height = 4800, units = "px", res = 400)
cowplot::plot_grid(MH_tslength, MH_theta,
                   MH_N,MH_m,
                   ncol=4,align="h")
dev.off()


### Plot 4: MH halving distance



MHHD_theta<-ggplot(aff.all[aff.all$N==1000&aff.all$M==0.2,], aes(THETA,log(hd_MH_dist+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("Theta at N=1000, m = 0.2")+
  ylab("MH-Halving distance [LN-transformed]")+
  theme_bw()+
  xlab("Species Pool Size [theta]")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
MHHD_theta


MHHD_tslength<-ggplot(aff.all[aff.all$N==1000&aff.all$THETA==40&aff.all$M==0.2,], aes(ts_max,log(hd_MH_dist+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("TS length at N=1000, m = 0.2, Theta = 40")+
  ylab("MH-Halving distance [LN-transformed]")+
  theme_bw()+
  xlab("Time Series Length")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
MHHD_tslength


MHHD_N<-ggplot(aff.all[aff.all$THETA==40&aff.all$M==0.2,], aes(N,log(hd_MH_dist+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("N at m = 0.2, Theta = 40")+
  ylab("MH-Halving distance [LN-transformed]")+
  theme_bw()+
  xlab("Number of Individuals")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
MHHD_N


MHHD_m<-ggplot(aff.all[aff.all$N==1000&aff.all$THETA==40,], aes(M,log(hd_MH_dist+1), col=scenario))+
  geom_point(alpha=.5,shape=21)+
  geom_smooth(method="lm",se=FALSE)+
  ggtitle("m at N=1000, Theta = 40")+
  ylab("MH-Halving distance [LN-transformed]")+
  theme_bw()+
  xlab("Arrival rate (m)")+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+facet_wrap(~scenario, scales = "free",ncol=1)
MHHD_m

tiff(file = "hd_MH.tiff", width = 6000, height = 4800, units = "px", res = 400)
cowplot::plot_grid(MHHD_tslength, MHHD_theta,
                   MHHD_N,MHHD_m,
                   ncol=4,align="h")
dev.off()


box1<-ggplot(aff.all[aff.all$N==1000&aff.all$THETA==40&aff.all$M==0.2,], aes(scenario,(affinity_jaccard), col=scenario))+
  geom_boxplot()+
  ggtitle("For m = 0.2, N = 1000, Theta = 40")+
  ylab("Affinity")+ylim(c(-20,100))+
  theme_bw()+
  xlab("Scenario")+geom_hline(yintercept = 0)+
  theme(axis.title.y=element_text(size=18, colour="black",vjust=0.7),axis.text.y=element_text(size=12,face="bold",colour="black",angle=0,hjust=0.9))+
  theme(axis.title.x=element_text(size=18,colour="black",vjust=0.2),axis.text.x=element_text(size=12,colour="black",face="bold"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,.2,0.2),"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.5))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(legend.position = "none")+coord_flip()
box1

gof<-aff.all %>%
  dplyr::group_by(scenario) %>%
  dplyr::summarize(
    pos_jac = length(affinity_jaccard[affinity_jaccard>0&!is.na(affinity_jaccard)])/61.88,
    neg_jac = length(affinity_jaccard[affinity_jaccard<0&!is.na(affinity_jaccard)])/61.88,
    na_jac = length(affinity_jaccard[is.na(affinity_jaccard)])/61.88,
    pos_mh = length(affinity_MH_dist[affinity_MH_dist>0&!is.na(affinity_MH_dist)])/61.88,
    neg_mh = length(affinity_MH_dist[affinity_MH_dist<0&!is.na(affinity_MH_dist)])/61.88,
    na_mh = length(affinity_MH_dist[is.na(affinity_MH_dist)])/61.88)

library(kableExtra)

gof %>%
  kbl(digits=2) %>%
  kable_styling()%>%
  add_header_above(c("","Jaccard"=3,"Morisita"=3))

