#code to plot the StO2 per group with errorbars
library(readr)
library(multcomp)
library(here)
#Data_for_R_analysis_Dec11 <- read_csv("//deckard.uark.edu/BMEG/Muldoon-Lab/RESEARCH/AM_ Ortothopic mice study_ colonoscopy DRS data for analysis/Data for first extraction Folder made on May 17th 2019/Complete dataset with new boundaries Dec 9.csv")
data1 <- read.csv(here("Extraction_2020_Cohort.csv"))
attach(data1)
library("ggplot2")
library(scales)
library(nlme)
library(ggplot2)
pd<-position_dodge(0.2)

data1$STO2min<-(data1$STO2-data1$STO2_SD)
data1$STO2max<-(data1$STO2+data1$STO2_SD)
data1$HBmin<-(data1$HB-data1$HB_SD)
data1$HBmax<-(data1$HB+data1$HB_SD)
data1$Amin<-(data1$A-data1$A_SD)
data1$Amax<-(data1$A+data1$A_SD)
data1$Bmin<-(data1$B-data1$B_SD)
data1$Bmax<-(data1$B+data1$B_SD)

data1$HBO2<-(data1$HB*data1$STO2)
data1$HBO2_SD<-data1$HB_SD*data1$STO2
data1$HBO2max<-data1$HBO2+data1$HBO2_SD
data1$HBO2min<-data1$HBO2-data1$HBO2_SD

data1$HBO<-data1$HB*(1-data1$STO2)
data1$HBO_SD<-data1$HB_SD*(1-data1$STO2)
data1$HBOmax<-data1$HBO+data1$HBO_SD
data1$HBOmin<-data1$HBO-data1$HBO_SD

#Calculating means 
means1<-aggregate(data1$STO2, by=list(time=data1$DAY,group=data1$GROUP),na.rm=TRUE,FUN=mean) #from StO2
sdevs1<-aggregate(data1$STO2, by=list(time=data1$DAY,group=data1$GROUP),na.rm=TRUE,FUN=sd) #from StO2
means1$STO2<-means1$x
means1$SDSTO2<-sdevs1$x
means2<-aggregate(data1$HB, by=list(time=data1$DAY,group=data1$GROUP),na.rm=TRUE,FUN=mean) #from Hb
sdevs2<-aggregate(data1$HB, by=list(time=data1$DAY,group=data1$GROUP),na.rm=TRUE,FUN=sd) #from Hb
means2$HB<-means2$x
means2$HBSD<-sdevs2$x
means3<-aggregate(data1$A, by=list(time=data1$DAY,group=data1$GROUP),na.rm=TRUE,FUN=mean) #from Hb
sdevs3<-aggregate(data1$A, by=list(time=data1$DAY,group=data1$GROUP),na.rm=TRUE,FUN=sd) #from Hb
means3$A<-means3$x
means3$ASD<-sdevs3$x

OxyHbmeans<-aggregate(data1$HBO2, by=list(time=data1$DAY,group=data1$GROUP),na.rm=TRUE,FUN=mean) #from HbO2
OxyHbstd<-aggregate(data1$HBO2, by=list(time=data1$DAY,group=data1$GROUP),na.rm=TRUE,FUN=sd) #from HbO2
OxyHbmeans$OxyHemoglobin<-OxyHbmeans$x
OxyHbmeans$OxyHbSD<-OxyHbstd$x

DeoxyHbmeans<-aggregate(data1$HBO, by=list(time=data1$DAY,group=data1$GROUP),na.rm=TRUE,FUN=mean) #from Hb
DeoxyHbstd<-aggregate(data1$HBO, by=list(time=data1$DAY,group=data1$GROUP),na.rm=TRUE,FUN=sd) #from Hb
DeoxyHbmeans$DeoxyHemoglobin<-DeoxyHbmeans$x
DeoxyHbmeans$DeoxyHbSD<-DeoxyHbstd$x


###plotting mean values####
ggplot(means1,aes(x=time,y=STO2))+geom_line()+geom_errorbar(aes(ymin=STO2-SDSTO2,ymax=STO2+SDSTO2,color=group),width=0.4, size=0.9,position=pd)+facet_wrap(~group) #plot mean StO2 values
ggplot(means2,aes(x=time,y=HB))+geom_line()+facet_wrap(~group) #plot mean Hb values
ggplot(OxyHbmeans,aes(x=time,y=OxyHemoglobin))+geom_line()+geom_errorbar(aes(ymin=OxyHemoglobin-OxyHbSD,ymax=OxyHemoglobin+OxyHbSD,color=group),width=0.4, size=0.9,position=pd)+facet_wrap(~group) #plot mean scattering coeff values
ggplot(means3,aes(x=time,y=A))+geom_line()+facet_wrap(~group) #plot mean scattering coeff values
#StO2 plot per week per group with means ****best one****
# pd=position_dodge(1.0)
# ggplot(data1,aes(x=DAY, y=STO2, linetype=TUMOR))+theme_bw()+
#   theme(text=element_text(size=20))+
#   geom_point(aes(color=factor(TUMOR)), position=pd, size=2)+
#   geom_line(aes(color=factor(TUMOR)), position=pd,size=1)+
#   scale_x_continuous(breaks=seq(1,6,1))+
#   geom_errorbar(aes(ymin=STO2min, ymax=STO2max,color=factor(TUMOR),linetype=NULL), width=2,position=pd,size=0.6,show.legend = FALSE)+
#   geom_text(data = data1, aes(x = 4.35, y = STO2, label = TUMOR),
#             size = 4, hjust = 1, fontface = "bold")+
#   stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
#   labs(title='',y="Oxygen Saturation(%)")+
#   facet_wrap(~ID,ncol=5)




#StO2 plot per week per subject per group MG **best**
library(ggrepel)
library(grid)
library(tidyverse)


#geom_label_repel(data=subset(data1,GROUP=='MG'),aes(label = TUMOR , fill=factor(TUMOR)),
#box.padding   = 0.35, 
#point.padding = 0.5,
#color ='black',
#show.legend = FALSE)+

#StO2 plots 
#################################################################

#MG group
pd=position_dodge(1.0)
ggplot(data=subset(data1,GROUP=='MG'),aes(x=DAY, y=STO2, linetype=TUMOR))+theme_bw()+
  geom_point(aes(color=TUMOR), position=pd, size=2, show.legend = FALSE)+
  geom_line(aes(color=factor(TUMOR)), position=pd,size=2, linetype="solid")+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=STO2min, ymax=STO2max,
                    color=factor(TUMOR),
                    linetype=NULL), width=0.5,position=pd,size=1,show.legend = FALSE)+
  scale_color_viridis_d(end = 0.75)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  facet_wrap(~ID,nrow=2)+
  labs(title='METRONOMIC',y="Oxygen Saturation(%)")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)



#tHb plots
#################################################################
#MG group
pd=position_dodge(1.0)
ggplot(data=subset(data1,GROUP=='MG'),aes(x=DAY, y=HB, linetype=TUMOR))+theme_bw()+
  geom_point(aes(color=TUMOR), position=pd, size=2, show.legend = FALSE)+
  geom_line(aes(color=factor(TUMOR)), position=pd,size=2, linetype="solid")+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=HBmin, ymax=HBmax,
                    color=factor(TUMOR),
                    linetype=NULL), width=0.5,position=pd,size=1,show.legend = FALSE)+
  scale_color_viridis_d(end = 0.75)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  facet_wrap(~ID,nrow=2)+
  labs(title='METRONOMIC',y="Total hemoglobin (mg/mL)")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)


#HbO2 plots
################################
#MG group
library(ggrepel)
library(tidyverse)
pd=position_dodge(1.0)
ggplot(data=subset(data1,GROUP=='MG'),aes(x=DAY, y=HBO2, linetype=TUMOR))+theme_bw()+
  geom_point(aes(color=TUMOR), position=pd, size=2, show.legend = FALSE)+
  geom_line(aes(color=factor(TUMOR)), position=pd,size=2, linetype="solid")+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=HBO2min, ymax=HBO2max,
                    color=factor(TUMOR),
                    linetype=NULL), width=0.5,position=pd,size=1,show.legend = FALSE)+
  scale_color_viridis_d(end = 0.75)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  facet_wrap(~ID,nrow =2)+
  labs(title='METRONOMIC',y="HbO2(mg/mL)")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)


#HbO plots
##############################
#MG group
pd=position_dodge(1.0)
ggplot(data=subset(data1,GROUP=='MG'),aes(x=DAY, y=HBO, linetype=TUMOR))+theme_bw()+
  geom_point(aes(color=TUMOR), position=pd, size=2, show.legend = FALSE)+
  geom_line(aes(color=factor(TUMOR)), position=pd,size=2, linetype="solid")+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=HBOmin, ymax=HBOmax,
                    color=factor(TUMOR),
                    linetype=NULL), width=0.5,position=pd,size=1,show.legend = FALSE)+
  scale_color_viridis_d(end = 0.75)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  facet_wrap(~ID,nrow=2)+
  labs(title='METRONOMIC',y=" Deoxyhemoglobin (mg/mL)")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)



####Mean values


