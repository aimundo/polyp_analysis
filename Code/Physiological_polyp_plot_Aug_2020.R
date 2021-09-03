#####AIM##Aug 30 2020. code for polyp wise analysis

#code to plot the StO2 per group with errorbars
library(readr)
library(multcomp)
library(here)
#Data_for_R_analysis_Dec11 <- read_csv("//deckard.uark.edu/BMEG/Muldoon-Lab/RESEARCH/AM_ Ortothopic mice study_ colonoscopy DRS data for analysis/Data for first extraction Folder made on May 17th 2019/Complete dataset with new boundaries Dec 9.csv")
data1 <- read.csv(here("Extraction_Aug_2020_Updated.csv"))
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

#MTD group
library(ggrepel)
library(tidyverse)
pd=position_dodge(1.0)
ggplot(data=subset(data1,GROUP=='MTD'),aes(x=DAY, y=STO2, linetype=TUMOR))+theme_bw()+
  geom_point(aes(color=TUMOR), position=pd, size=2, show.legend = FALSE)+
  geom_line(aes(color=factor(TUMOR)), position=pd,size=2, linetype="solid")+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=STO2min, ymax=STO2max,
                    color=factor(TUMOR),
                    linetype=NULL), width=0.5,position=pd,size=1,show.legend = FALSE)+
  scale_color_viridis_d(end = 0.75)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
   facet_wrap(~ID,nrow =2)+
  labs(title='MAXIMUM TOLERATED DOSE',y="Oxygen Saturation(%)")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)


#StO2 plot per week per subject per group CG **best**
library(ggrepel)
library(tidyverse)
pd=position_dodge(1.0)
ggplot(data=subset(data1,GROUP=='CG'),aes(x=DAY, y=STO2, linetype=TUMOR))+theme_bw()+
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
  labs(title='CONTROL',y="Oxygen Saturation(%)")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)

#####
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

#MTD group
pd=position_dodge(1.0)
ggplot(data=subset(data1,GROUP=='MTD'),aes(x=DAY, y=HB, linetype=TUMOR))+theme_bw()+
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
  labs(title='MAXIMUM TOLERATED DOSE',y="Total hemoglobin (mg/mL)")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)

#CG group
pd=position_dodge(1.0)
ggplot(data=subset(data1,GROUP=='CG'),aes(x=DAY, y=HB, linetype=TUMOR))+theme_bw()+
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
  labs(title='CONTROL',y="Total hemoglobin (mg/mL)")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)

##################
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

#MTD group

ggplot(data=subset(data1,GROUP=='MTD'),aes(x=DAY, y=HBO2, linetype=TUMOR))+theme_bw()+
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
  labs(title='MAXIMUM TOLERATED DOSE',y="HbO2(mg/mL)")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)


#CG group
ggplot(data=subset(data1,GROUP=='CG'),aes(x=DAY, y=HBO2, linetype=TUMOR))+theme_bw()+
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
  labs(title='CONTROL',y="HbO2(mg/mL)")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)



###############
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


#MTD group
pd=position_dodge(1.0)
ggplot(data=subset(data1,GROUP=='MTD'),aes(x=DAY, y=HBO, linetype=TUMOR))+theme_bw()+
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
  labs(title='MAXIMUM TOLERATED DOSE',y=" Deoxyhemoglobin (mg/mL)")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)

#CG group
pd=position_dodge(1.0)
ggplot(data=subset(data1,GROUP=='CG'),aes(x=DAY, y=HBO, linetype=TUMOR))+theme_bw()+
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
  labs(title='CONTROL',y=" Deoxyhemoglobin (mg/mL)")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)


#######
#Scattering coefficient (A) plots
####################################
#MG group
pd=position_dodge(1.0)
ggplot(data=subset(data1,GROUP=='MG'),aes(x=DAY, y=A, linetype=TUMOR))+theme_bw()+
  geom_point(aes(color=TUMOR), position=pd, size=2, show.legend = FALSE)+
  geom_line(aes(color=factor(TUMOR)), position=pd,size=2, linetype="solid")+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=Amin, ymax=Amax,
                    color=factor(TUMOR),
                    linetype=NULL), width=0.5,position=pd,size=1,show.legend = FALSE)+
  scale_color_viridis_d(end = 0.75)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  facet_wrap(~ID,nrow =2)+
  labs(title='METRONOMIC',y="A")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)

##MTD group
ggplot(data=subset(data1,GROUP=='MTD'),aes(x=DAY, y=A, linetype=TUMOR))+theme_bw()+
  geom_point(aes(color=TUMOR), position=pd, size=2, show.legend = FALSE)+
  geom_line(aes(color=factor(TUMOR)), position=pd,size=2, linetype="solid")+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=Amin, ymax=Amax,
                    color=factor(TUMOR),
                    linetype=NULL), width=0.5,position=pd,size=1,show.legend = FALSE)+
  scale_color_viridis_d(end = 0.75)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  facet_wrap(~ID,nrow =2)+
  labs(title='MAXIMUM TOLERATED DOSE',y="A")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)

##CG group
ggplot(data=subset(data1,GROUP=='CG'),aes(x=DAY, y=A, linetype=TUMOR))+theme_bw()+
  geom_point(aes(color=TUMOR), position=pd, size=2, show.legend = FALSE)+
  geom_line(aes(color=factor(TUMOR)), position=pd,size=2, linetype="solid")+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=Amin, ymax=Amax,
                    color=factor(TUMOR),
                    linetype=NULL), width=0.5,position=pd,size=1,show.legend = FALSE)+
  scale_color_viridis_d(end = 0.75)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  facet_wrap(~ID,nrow =2)+
  labs(title='CONTROL',y="A")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)

###########
#Scattering exponent (B)
#######################################
##MG group
ggplot(data=subset(data1,GROUP=='MG'),aes(x=DAY, y=B, linetype=TUMOR))+theme_bw()+
  geom_point(aes(color=TUMOR), position=pd, size=2, show.legend = FALSE)+
  geom_line(aes(color=factor(TUMOR)), position=pd,size=2, linetype="solid")+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=Bmin, ymax=Bmax,
                    color=factor(TUMOR),
                    linetype=NULL), width=0.5,position=pd,size=1,show.legend = FALSE)+
  scale_color_viridis_d(end = 0.75)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  facet_wrap(~ID,nrow =2)+
  labs(title='METRONOMIC',y="B")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)

##MTD group
ggplot(data=subset(data1,GROUP=='MTD'),aes(x=DAY, y=B, linetype=TUMOR))+theme_bw()+
  geom_point(aes(color=TUMOR), position=pd, size=2, show.legend = FALSE)+
  geom_line(aes(color=factor(TUMOR)), position=pd,size=2, linetype="solid")+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=Bmin, ymax=Bmax,
                    color=factor(TUMOR),
                    linetype=NULL), width=0.5,position=pd,size=1,show.legend = FALSE)+
  scale_color_viridis_d(end = 0.75)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  facet_wrap(~ID,nrow =2)+
  labs(title='MAXIMUM TOLERATED DOSE',y="B")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)

##CG group
ggplot(data=subset(data1,GROUP=='CG'),aes(x=DAY, y=B, linetype=TUMOR))+theme_bw()+
  geom_point(aes(color=TUMOR), position=pd, size=2, show.legend = FALSE)+
  geom_line(aes(color=factor(TUMOR)), position=pd,size=2, linetype="solid")+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=Bmin, ymax=Bmax,
                    color=factor(TUMOR),
                    linetype=NULL), width=0.5,position=pd,size=1,show.legend = FALSE)+
  scale_color_viridis_d(end = 0.75)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  facet_wrap(~ID,nrow =2)+
  labs(title='CONTROL',y="B")+
  theme(text=element_text(size=20),plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)


#
#
#
#
########
############################################################################
pd=position_dodge(1.0)
ggplot(data1,aes(x=DAY, y=STO2, linetype=ID))+theme_bw()+
  theme(text=element_text(size=20))+
  geom_point(aes(color=factor(ID)), position=pd, size=2,show.legend=FALSE)+
  geom_line(aes(color=factor(ID)), linetype=1, position = pd,size=1.5,show.legend=TRUE)+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=STO2min, ymax=STO2max,color=factor(GROUP),linetype=NULL), width=2,position=pd,size=0.9,show.legend = FALSE)+
  stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  labs(title='',y="Oxygen Saturation(%)")+
  facet_wrap(~GROUP)
#########################################################################

############################################################################
##########################################################################



##Calculating means 
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
ggplot(data=subset(means1,group=='MG'),aes(x=time,y=STO2))+geom_line(size=2)+
  geom_errorbar(aes(ymin=STO2-SDSTO2,ymax=STO2+SDSTO2),width=0.4, size=0.9,position=pd)+
  labs(y="Oxygen Saturation (%)")+
  ggtitle("Oxygen saturation") #plot mean StO2 values


ggplot(data=subset(means2,group=='MG'),aes(x=time,y=HB))+geom_line(size=2)+geom_errorbar(aes(ymin=HB-HBSD,ymax=HB+HBSD),width=0.4, size=0.9,position=pd)+facet_wrap(~group)+ggtitle("total Hemoglobin")+labs(y="total Hemoglobin (mg/mL)") #plot mean Hb values
ggplot(data=subset(OxyHbmeans,group=='MG'),aes(x=time,y=OxyHemoglobin))+geom_line(size=2)+geom_errorbar(aes(ymin=OxyHemoglobin-OxyHbSD,ymax=OxyHemoglobin+OxyHbSD),width=0.4, size=0.9,position=pd)+facet_wrap(~group)+ggtitle("Oxy Hemoglobin")+labs(y="Oxyhemoglobin(mg/mL)") #plot HbO2
ggplot(data=subset(DeoxyHbmeans,group=='MG'),aes(x=time,y=DeoxyHemoglobin))+geom_line(size=2)+geom_errorbar(aes(ymin=DeoxyHemoglobin-DeoxyHbSD,ymax=DeoxyHemoglobin+DeoxyHbSD),width=0.4, size=0.9,position=pd)+facet_wrap(~group)+ggtitle("Deoxyhemoglobin")+labs(y="Deoxyhemoglobin (mg/mL)") #plot mean scattering coeff values
ggplot(data=subset(means3,group=='MG'),aes(x=time,y=A))+geom_line(size=2)+geom_errorbar(aes(ymin=A-ASD,ymax=A+ASD),width=0.4, size=0.9,position=pd)+facet_wrap(~group)+ggtitle("Scattering Coefficient (A)")+labs(y="Scattering coefficient (A)") #plot mean scattering coeff values
##create a stacked dataframe of mean StO2 and A per group
library(reshape2)
merged1=merge(means1,means3, by=c('time','group'))
merged1<-subset(merged1,select=-c(x.x,x.y)) #delete repeated columns and StO2 basevals
names(merged1)[names(merged1)=='group']<-'Treatment' #rename groups as Treatments


##plotting both StO2 and A 
pd1=position_dodge(2)
ggplot()+theme_bw()+
  #geom_line(data=merged1,aes(x=time,y=StO2),colour="#FF9999",position=pd)+
  #geom_errorbar(data=merged1,aes(x=time,ymin=StO2-SDStO2,ymax=StO2+SDStO2),position=pd,colour="#FF9999",width=0.4, size=0.9)+
  geom_line(data=merged1,aes(x=time,y=A*10),position=pd1,colour="#FF0000")+
  geom_errorbar(data=merged1,aes(x=time,ymin=(A-ASD)*10,ymax=(A+ASD)*10),width=0.4, size=0.9,position=pd1,colour="#FF0000")+
  #scale_y_continuous(sec.axis = sec_axis(~./10, name = "Mean scattering coeff"))+
  facet_wrap(~Treatment)

#geom_errorbar(aes(ymin=StO2foldchange-FoldErr,ymax=StO2foldchange+FoldErr,color=group),width=0.4, size=0.9,position=pd)



#Extract initial values per group
StO2basevals<-means1[means1$time==1, c("group", "STO2")] #extract initial StO2 values per group
means1<-merge(means1,StO2basevals, by.x="group", by.y='group',suffixes = c("","basevalue"))#copy StO2 base values per group

Hbbasevals<-means2[means2$time==1,c("group",'HB')] #extract initial Hb values per group
means2<-merge(means2,Hbbasevals, by.x="group", by.y='group',suffixes = c("","basevalue"))#copy StO2 base values per group


OxyHbbasevals<-OxyHbmeans[OxyHbmeans$time==1, c("group", "OxyHemoglobin")] #extract initial HbO2 values per group
OxyHbmeans<-merge(OxyHbmeans,OxyHbbasevals, by.x="group", by.y='group',suffixes = c("","basevalue"))#copy HbO2 base values per group


DeoxyHbbasevals<-DeoxyHbmeans[DeoxyHbmeans$time==1, c("group", "DeoxyHemoglobin")] #extract initial Hb values per group
DeoxyHbmeans<-merge(DeoxyHbmeans,DeoxyHbbasevals, by.x="group", by.y='group',suffixes = c("","basevalue"))#copy Hb base values per group

Abasevals<-means3[means3$time==1, c("group", "A")] #extract initial StO2 values per group
means3<-merge(means3,Abasevals, by.x="group", by.y='group',suffixes = c("","basevalue"))#copy StO2 base values per group

##getting a subset of StO2 values without NAs
library(dplyr)
Trimmed<-data1%>%filter(!is.na(STO2)) #extracting the non NA values 
Numbers<-aggregate(Trimmed$STO2, by=list(time=Trimmed$DAY,group=Trimmed$GROUP),FUN=length) #counting the number of occurrences per group per week
names(Numbers)<-c('Week','Group','Observations')
#Calculate fold change and pasting number of observations per group per week
##StO2
means1$StO2foldchange<-means1$STO2/means1$STO2basevalue #Mean StO2 fold change
means1$Observations<-Numbers$Observations
means1$StandErr<-means1$SDSTO2/(means1$Observations)^0.5
means1$FoldErr<-means1$StandErr/means1$STO2basevalue


##total hemoglobin
means2$Hbfoldchange<-means2$HB/means2$HBbasevalue #Mean Hb fold change
means2$Observations<-Numbers$Observations
means2$StandErr<-means2$HBSD/(means2$Observations)^0.5
means2$FoldErr<-means2$StandErr/means2$HBbasevalue

##A (reduced scattering coefficient)
means3$Afoldchange<-means3$A/means3$Abasevalue #Mean A fold change
means3$Observations<-Numbers$Observations
means3$StandErr<-means3$ASD/(means3$Observations)^0.5
means3$FoldErr<-means3$StandErr/means3$Abasevalue
###HbO2####
OxyHbmeans$OxyHbfoldchange<-OxyHbmeans$OxyHemoglobin/OxyHbmeans$OxyHemoglobinbasevalue
OxyHbmeans$Observations<-Numbers$Observations
OxyHbmeans$StandErr<-OxyHbmeans$OxyHbSD/(OxyHbmeans$Observations)
OxyHbmeans$FoldErr<-OxyHbmeans$StandErr/OxyHbmeans$OxyHemoglobinbasevalue

##Hb###
DeoxyHbmeans$DeoxyHbfoldchange<-DeoxyHbmeans$DeoxyHemoglobin/DeoxyHbmeans$DeoxyHemoglobinbasevalue
DeoxyHbmeans$Observations<-Numbers$Observations
DeoxyHbmeans$StandErr<-DeoxyHbmeans$DeoxyHbSD/(DeoxyHbmeans$Observations)
DeoxyHbmeans$FoldErr<-DeoxyHbmeans$StandErr/DeoxyHbmeans$DeoxyHemoglobinbasevalue

#plot fold changes
pd=position_dodge(0.4)
ggplot(data=subset(means1,group=='MG'),aes(x=time, y=StO2foldchange))+theme_bw()+geom_point(aes(color=group,group=group),size=2, position=pd)+geom_line(aes(color=group,group=group),size=1.5,position=pd)+geom_errorbar(aes(ymin=StO2foldchange-FoldErr,ymax=StO2foldchange+FoldErr,color=group),width=0.4, size=0.9,position=pd)+theme(text=element_text(size=30),aspect.ratio = 1)+scale_x_continuous(breaks=seq(1,6,1))+labs(title=expression(StO[2]), y='', x='Weeks')+theme(plot.title = element_text(hjust = 0.5)) #plot StO2 fold change
ggplot(data=subset(means2,group=='MG'),aes(x=time, y=Hbfoldchange))+theme_bw()+geom_point(aes(color=group,group=group),size=2, position=pd)+geom_line(aes(color=group,group=group),size=1.5, position=pd)+geom_errorbar(aes(ymin=Hbfoldchange-FoldErr,ymax=Hbfoldchange+FoldErr,color=group),width=0.4, size=0.9, position=pd)+theme(text=element_text(size=30),aspect.ratio = 1)+scale_x_continuous(breaks=seq(1,6,1))+labs(title=expression(tHb), y='', x='Weeks')+theme(plot.title = element_text(hjust = 0.5)) #plot Hb fold change
ggplot(data=subset(means3,group=='MG'),aes(x=time, y=Afoldchange))+theme_bw()+geom_point(aes(color=group,group=group),size=2, position=pd)+geom_line(aes(color=group,group=group),size=1.5, position=pd)+geom_errorbar(aes(ymin=Afoldchange-FoldErr,ymax=Afoldchange+FoldErr,color=group),width=0.4, size=0.9, position=pd)+theme(text=element_text(size=30),aspect.ratio = 1)+scale_x_continuous(breaks=seq(1,6,1))+labs(title=expression(mu[s]^{"'"}), y='', x='Weeks')+theme(plot.title = element_text(hjust = 0.5)) #plot A fold change
ggplot(data=subset(OxyHbmeans,group=='MG'),aes(x=time, y=OxyHbfoldchange))+theme_bw()+geom_point(aes(color=group,group=group),size=2, position=pd)+geom_line(aes(color=group,group=group),size=1.5,position=pd)+geom_errorbar(aes(ymin=OxyHbfoldchange-FoldErr,ymax=OxyHbfoldchange+FoldErr,color=group),width=0.4, size=0.9,position=pd)+theme(text=element_text(size=30),aspect.ratio = 1)+scale_x_continuous(breaks=seq(1,6,1))+labs(title=expression(HbO[2]), y='', x='Weeks')+theme(plot.title = element_text(hjust = 0.5)) #plot HbO2 fold change
ggplot(data=subset(DeoxyHbmeans,group=='MG'),aes(x=time, y=DeoxyHbfoldchange))+theme_bw()+geom_point(aes(color=group,group=group),size=2, position=pd)+geom_line(aes(color=group,group=group),size=1.5,position=pd)+geom_errorbar(aes(ymin=DeoxyHbfoldchange-FoldErr,ymax=DeoxyHbfoldchange+FoldErr,color=group),width=0.4, size=0.9,position=pd)+theme(text=element_text(size=30),aspect.ratio = 1)+scale_x_continuous(breaks=seq(1,6,1))+labs(title=expression(HbO), y='', x='Weeks')+theme(plot.title = element_text(hjust = 0.5)) #plot Hb fold change

