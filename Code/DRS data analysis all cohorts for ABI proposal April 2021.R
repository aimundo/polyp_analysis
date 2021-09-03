#code to plot the StO2 per group with errorbars
library(readr)
library(multcomp)
library(here)

library("ggplot2")
library(scales)
library(nlme)
library(patchwork)
library(ggplot2)
library(tidyverse)
pd<-position_dodge(0.2)
data1 <- read.csv(here("data","Extraction_merged_2018_2020_cohorts_v1.csv"))
attach(data1)
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

##scatter plot per group

#StO2 plot per week per group with means ****best one****
pd=position_dodge(1.0)
ggplot(data1,aes(x=DAY, y=STO2))+theme_bw()+
  theme(text=element_text(size=20))+
  geom_point(aes(color=factor(GROUP)), position=pd, size=2)+
  stat_summary(aes(group=GROUP,color=factor(GROUP)), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  scale_color_viridis_d(end = 0.8)+
  labs(title='',y="Oxygen Saturation(%)")+
  facet_wrap(~GROUP,ncol=5)


####StO2 integrated plot
pd=position_dodge(1.0)
ggplot(data1,aes(x=DAY, y=STO2))+theme_bw()+
  theme(text=element_text(size=20))+
  geom_point(aes(color=factor(GROUP)), position=pd, size=2)+
  stat_summary(aes(group=GROUP,color=factor(GROUP)), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  scale_color_viridis_d(end = 0.8)+
  labs(title='',y="Oxygen Saturation(%)")


#HbO2 plot per week per group with means ****best one****
pd=position_dodge(1.0)
ggplot(data1,aes(x=DAY, y=HBO2))+theme_bw()+
  theme(text=element_text(size=20))+
  geom_point(aes(color=factor(GROUP)), position=pd, size=2)+
  stat_summary(aes(group=GROUP,color=factor(GROUP)), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  scale_color_viridis_d(end = 0.8)+
  labs(title='',y="Oxyhemoglobin (mg/mL)")+
  facet_wrap(~GROUP,ncol=5)

#HbO plot per week per group with means ****best one****
pd=position_dodge(1.0)
ggplot(data1,aes(x=DAY, y=HBO))+theme_bw()+
  theme(text=element_text(size=20))+
  geom_point(aes(color=factor(GROUP)), position=pd, size=2)+
  stat_summary(aes(group=GROUP,color=factor(GROUP)), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  scale_color_viridis_d(end = 0.8)+
  labs(title='',y="Deoxy Hemoglobin (mg/mL)")+
  facet_wrap(~GROUP,ncol=5)



#tHb plot per week per group with means ****best one****
pd=position_dodge(1.0)
ggplot(data1,aes(x=DAY, y=HB))+theme_bw()+
  theme(text=element_text(size=20))+
  geom_point(aes(color=factor(GROUP)), position=pd, size=2)+
  stat_summary(aes(group=GROUP,color=factor(GROUP)), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  scale_color_viridis_d(end = 0.8)+
  labs(title='',y="total Hemoglobin (mg/mL)")+
  facet_wrap(~GROUP,ncol=5)






###plotting mean values####
txt1<-14
g1<-ggplot(means1,aes(x=time,y=STO2))+geom_line(aes(color=group),size=2)+theme_classic()+
  geom_errorbar(aes(ymin=STO2-SDSTO2,ymax=STO2+SDSTO2,color=group),width=0.4, size=0.9,position=pd)+
  scale_color_manual(values=c('gray31','blue3','red3'))+
  scale_x_continuous(breaks=seq(1,6,1))+
  labs(title=expression(StO[2]),subtitle='Oxygen Saturation', y='Oxygen Saturation (%)', x='Weeks')+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text (size=14),
        text=element_text(size=txt1),
        aspect.ratio = 1)


g2<-ggplot(means2,aes(x=time,y=HB))+geom_line(aes(color=group),size=2)+theme_classic()+
  geom_errorbar(aes(ymin=HB-HBSD,ymax=HB+HBSD,color=group),width=0.4, size=0.9,position=pd)+
  scale_color_manual(values=c('gray31','blue3','red3'))+
  scale_x_continuous(breaks=seq(1,6,1))+
  labs(title=expression(tHb),subtitle='Total hemoglobin', y='Total hemoglobin (mg/ml)', x='Weeks')+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text (size=14),
        text=element_text(size=txt1),
        aspect.ratio = 1)

g3<-ggplot(OxyHbmeans,aes(x=time,y=OxyHemoglobin))+geom_line(aes(color=group),size=2)+theme_classic()+
  geom_errorbar(aes(ymin=OxyHemoglobin-OxyHbSD,ymax=OxyHemoglobin+OxyHbSD,color=group),width=0.4, size=0.9,position=pd)+
  scale_color_manual(values=c('gray31','blue3','red3'))+
  scale_x_continuous(breaks=seq(1,6,1))+
  labs(title=expression(HbO[2]),subtitle='Oxyhemoglobin ', y='Oxyhemoglobin (mg/ml)', x='Weeks')+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text (size=14),
        text=element_text(size=txt1),
        aspect.ratio = 1)

g4<-ggplot(DeoxyHbmeans,aes(x=time,y=DeoxyHemoglobin))+geom_line(aes(color=group),size=2)+theme_classic()+
  geom_errorbar(aes(ymin=DeoxyHemoglobin-DeoxyHbSD,ymax=DeoxyHemoglobin+DeoxyHbSD,color=group),width=0.4, size=0.9,position=pd)+
  scale_color_manual(values=c('gray31','blue3','red3'))+
  scale_x_continuous(breaks=seq(1,6,1))+
  labs(title=expression(HbO),subtitle='Deoxyhemoglobin ', y='Deoxyhemoglobin (mg/ml)', x='Weeks')+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text (size=14),
        text=element_text(size=txt1),
        aspect.ratio = 1)


##plotting
g1+g2+g3+g4+plot_layout(nrow=2)+
  plot_annotation(tag_levels='A')&
  theme(plot.tag=element_text(colour = 'navyblue'))


ggplot(means3,aes(x=time,y=A))+geom_line(aes(color=group),size=2)+
  geom_errorbar(aes(ymin=A-ASD,ymax=A+ASD,color=group),width=0.4, size=0.9,position=pd)+facet_wrap(~group)+
  scale_color_viridis_d(end = 0.8)+
  ggtitle("Scattering Coefficient (A)")+labs(y="Scattering coefficient (A)") #plot mean scattering coeff values




###April 2021: For this graphs the baseline value will be the Control Day 1 values for all variables
# The fold changes will be calculated 
#relative to the CG group then, not using each group as its own calibrator
#In each case the baseline value is pasted in the dataframe and the fold change in calculated.

#StO2
means1<-means1%>%
  mutate(Baseline=rep(data1 %>%
              filter(GROUP=='CG',DAY==1) %>%
              summarize(val=as.numeric(mean(STO2,na.rm=TRUE)
                           ))))
means1<-means1%>%
  mutate(Fold_change=STO2/as.numeric(Baseline))



#Hb
means2<-means2%>%
  mutate(Baseline=rep(data1 %>%
                        filter(GROUP=='CG',DAY==1) %>%
                        summarize(val=as.numeric(mean(HB,na.rm=TRUE)
                        ))))

means2<-means2%>%
  mutate(Fold_change=HB/as.numeric(Baseline))

#A
means3<-means3%>%
  mutate(Baseline=rep(data1 %>%
                        filter(GROUP=='CG',DAY==1) %>%
                        summarize(val=as.numeric(mean(A,na.rm=TRUE)
                        ))))

means3<-means3%>%
  mutate(Fold_change=A/as.numeric(Baseline))

#OxyHb
OxyHbmeans<-OxyHbmeans%>%
  mutate(Baseline=rep(data1 %>%
                        filter(GROUP=='CG',DAY==1) %>%
                        summarize(val=as.numeric(mean(HBO2,na.rm=TRUE)
                        ))))


OxyHbmeans<-OxyHbmeans%>%
  mutate(Fold_change=OxyHemoglobin/as.numeric(Baseline))

#DeoxyHb
DeoxyHbmeans<-DeoxyHbmeans%>%
  mutate(Baseline=rep(data1 %>%
                        filter(GROUP=='CG',DAY==1) %>%
                        summarize(val=as.numeric(mean(HBO,na.rm=TRUE)
                        ))))


DeoxyHbmeans<-DeoxyHbmeans%>%
  mutate(Fold_change=DeoxyHemoglobin/as.numeric(Baseline))


##getting a subset of StO2 values without NAs
Trimmed<-data1%>%filter(!is.na(STO2)) #extracting the non NA values 
Numbers<-aggregate(Trimmed$STO2, by=list(time=Trimmed$DAY,group=Trimmed$GROUP),FUN=length) #counting the number of occurrences per group per week
names(Numbers)<-c('Week','Group','Observations')
#Calculate fold change and pasting number of observations per group per week
##StO2

means1$Baseline<-as.numeric(means1$Baseline)
means1$Observations<-Numbers$Observations
means1$StandErr<-means1$SDSTO2/(means1$Observations)^0.5
means1$FoldErr<-means1$StandErr/means1$Baseline


##total hemoglobin
means2$Baseline<-as.numeric(means2$Baseline)
means2$Hbfoldchange<-means2$HB/means2$Baseline #Mean Hb fold change
means2$Observations<-Numbers$Observations
means2$StandErr<-means2$HBSD/(means2$Observations)^0.5
means2$FoldErr<-means2$StandErr/means2$Baseline

##A (reduced scattering coefficient)
means3$Baseline<-as.numeric(means3$Baseline)
means3$Afoldchange<-means3$A/means3$Baseline #Mean A fold change
means3$Observations<-Numbers$Observations
means3$StandErr<-means3$ASD/(means3$Observations)^0.5
means3$FoldErr<-means3$StandErr/means3$Baseline

###HbO2####

OxyHbmeans$Baseline<-as.numeric(OxyHbmeans$Baseline)
OxyHbmeans$OxyHbfoldchange<-OxyHbmeans$OxyHemoglobin/OxyHbmeans$Baseline
OxyHbmeans$Observations<-Numbers$Observations
OxyHbmeans$StandErr<-OxyHbmeans$OxyHbSD/(OxyHbmeans$Observations)
OxyHbmeans$FoldErr<-OxyHbmeans$StandErr/OxyHbmeans$Baseline

##Hb###

DeoxyHbmeans$Baseline<-as.numeric(DeoxyHbmeans$Baseline)
DeoxyHbmeans$DeoxyHbfoldchange<-DeoxyHbmeans$DeoxyHemoglobin/DeoxyHbmeans$Baseline
DeoxyHbmeans$Observations<-Numbers$Observations
DeoxyHbmeans$StandErr<-DeoxyHbmeans$DeoxyHbSD/(DeoxyHbmeans$Observations)
DeoxyHbmeans$FoldErr<-DeoxyHbmeans$StandErr/DeoxyHbmeans$Baseline

#plot fold changes
library(patchwork)
library(ggsci)
pd=position_dodge(0.4)
thm1<-scale_color_aaas()
txt<-20
clr<-c('gray31','blue3','red3')
p1<-ggplot(subset(means1,group %in% c("CG","MG")),aes(x=time, y=Fold_change))+theme_classic()+
  geom_point(aes(color=group,group=group),size=2, position=pd,show.legend = FALSE)+
  geom_line(aes(color=group,group=group),size=1.5,position=pd,show.legend = FALSE)+
  geom_errorbar(aes(ymin=Fold_change-FoldErr,ymax=Fold_change+FoldErr,color=group),width=0.4, size=0.9,position=pd,show.legend = FALSE)+
  scale_x_continuous(breaks=seq(1,6,1))+
  labs(title=expression(StO[2]),y='Fold Change', x='Weeks')+ #plot StO2 fold change
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text (size=14),
        text=element_text(size=txt),
        aspect.ratio = 1)+
  thm1



p2<-ggplot(subset(means2,group %in% c("CG","MG")),aes(x=time, y=Fold_change))+theme_classic()+
  geom_point(aes(color=group,group=group),size=2, position=pd,show.legend = FALSE)+
  geom_line(aes(color=group,group=group),size=1.5, position=pd,show.legend = FALSE)+
  geom_errorbar(aes(ymin=Fold_change-FoldErr,ymax=Fold_change+FoldErr,color=group),width=0.4, size=0.9, position=pd,show.legend = FALSE)+
  theme(text=element_text(size=txt),aspect.ratio = 1)+
  scale_x_continuous(breaks=seq(1,6,1))+
  labs(title=expression(tHb), subtitle='Total Hemoglobin',y='', x='Weeks')+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text (size=14),
        text=element_text(size=txt),
        aspect.ratio = 1)+thm1


p3<-ggplot(subset(means3,group %in% c("CG","MG")),aes(x=time, y=Fold_change))+theme_classic()+
  geom_point(aes(color=group,group=group),size=2, position=pd,show.legend = FALSE)+
  geom_line(aes(color=group,group=group),size=1.5, position=pd,show.legend = FALSE)+
  geom_errorbar(aes(ymin=Fold_change-FoldErr,ymax=Fold_change+FoldErr,color=group),width=0.4, size=0.9, position=pd,show.legend = FALSE)+
  theme(text=element_text(size=txt),aspect.ratio = 1)+
  scale_x_continuous(breaks=seq(1,6,1))+labs(title=expression(mu[s]^{"'"}), y='', x='Weeks')+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text (size=14),
        text=element_text(size=txt),
        aspect.ratio = 1)+thm1


p4<-ggplot(subset(OxyHbmeans,group %in% c("CG","MG")),aes(x=time, y=Fold_change))+theme_classic()+
  geom_point(aes(color=group,group=group),size=2, position=pd,show.legend = FALSE)+
  geom_line(aes(color=group,group=group),size=1.5,position=pd,show.legend = FALSE)+
  geom_errorbar(aes(ymin=Fold_change-FoldErr,ymax=Fold_change+FoldErr,color=group),width=0.4, size=0.9,position=pd,show.legend = FALSE)+
  theme(text=element_text(size=txt),aspect.ratio = 1)+scale_x_continuous(breaks=seq(1,6,1))+
  labs(title=expression(HbO[2]), y='', x='Weeks')+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text (size=14),
        text=element_text(size=txt),
        aspect.ratio = 1)+thm1


p5<-ggplot(subset(DeoxyHbmeans,group %in% c("CG","MG")),aes(x=time, y=Fold_change))+theme_classic()+
  geom_point(aes(color=group,group=group),size=2, position=pd)+
  geom_line(aes(color=group,group=group),size=1.5,position=pd)+
  geom_errorbar(aes(ymin=Fold_change-FoldErr,ymax=Fold_change+FoldErr,color=group),width=0.4, size=0.9,position=pd)+
  theme(text=element_text(size=txt),aspect.ratio = 1)+
  scale_x_continuous(breaks=seq(1,6,1))+
  labs(title=expression(HbO),subtitle='Deoxyhemoglobin', y='', x='Weeks')+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text (size=14),
        text=element_text(size=txt),
        aspect.ratio = 1)+
  thm1



p1+p2+p4+p5+
  plot_layout(nrow=2)+
  plot_annotation(tag_levels='A')&
  theme(plot.tag=element_text(colour = 'navyblue'))


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


