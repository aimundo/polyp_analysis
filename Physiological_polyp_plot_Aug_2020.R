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
#StO2 plots
##extract Hb and HbO2 values per animal per group
#data1$STO2<-(data1$STO2*100/100)
#data1$StO2sd<-(data1$STO2_SD*100/100)
data1$STO2min<-(data1$STO2-data1$STO2_SD)
data1$STO2max<-(data1$STO2+data1$STO2_SD)
data1$HBmin<-(data1$HB-data1$HB_SD)
data1$HBmax<-(data1$HB+data1$HB_SD)
data1$Amin<-(data1$A-data1$A_SD)
data1$Amax<-(data1$A+data1$A_SD)
data1$Bmin<-(data1$B-data1$B_SD)
data1$Bmax<-(data1$B+data1$B_SD)

data1$HBO2<-(data1$HB*data1$STO2)/100
data1$HB0<-(data1$HB*(100-data1$STO2))/100


  #StO2 plot per week per group with means ****best one****
pd=position_dodge(1.0)
ggplot(data1,aes(x=DAY, y=STO2, linetype=TUMOR))+theme_bw()+
  theme(text=element_text(size=20))+
  geom_point(aes(color=factor(TUMOR)), position=pd, size=2)+
  geom_line(aes(color=factor(TUMOR)), position=pd,size=1)+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=STO2min, ymax=STO2max,color=factor(TUMOR),linetype=NULL), width=2,position=pd,size=0.6,show.legend = FALSE)+
  geom_text(data = data1, aes(x = 4.35, y = STO2, label = TUMOR), 
            size = 4, hjust = 1, fontface = "bold")+
  stat_summary(aes(group=GROUP), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  labs(title='',y="Oxygen Saturation(%)")+
  facet_wrap(~ID,ncol=5)


#StO2 plot per week per subject per group all groups
library(ggrepel)
library(tidyverse)
pd=position_dodge(1.0)
ggplot(data1,aes(x=DAY, y=STO2, linetype=TUMOR))+theme_bw()+
  theme(text=element_text(size=20))+
  geom_point(aes(color=factor(TUMOR)), position=pd, size=2, show.legend = FALSE)+
  geom_line(aes(color=factor(TUMOR)), position=pd,size=1, linetype="solid",show.legend = FALSE)+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=STO2min, ymax=STO2max,
                    color=factor(TUMOR),
                    linetype=NULL), width=2,position=pd,size=1,show.legend = FALSE)+
  facet_wrap(ID~GROUP,ncol=5)+
  geom_label_repel(data=data1,aes(label = TUMOR , fill=factor(TUMOR)),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   color ='black',
show.legend = FALSE)+ 
  labs(title='',y="Oxygen Saturation(%)")
  

#StO2 plot per week per subject per group MG **best**
library(ggrepel)
library(grid)
library(tidyverse)
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
  theme(text=element_text(size=20),legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)+
  facet_wrap(~ID,nrow=2)+
  labs(title='METRONOMIC',y="Oxygen Saturation(%)")

#geom_label_repel(data=subset(data1,GROUP=='MG'),aes(label = TUMOR , fill=factor(TUMOR)),
                 #box.padding   = 0.35, 
                 #point.padding = 0.5,
                 #color ='black',
                 #show.legend = FALSE)+

#StO2 plot per week per subject per group MTD **best**
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
  theme(text=element_text(size=20),legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)+
  facet_wrap(~ID,nrow =2)+
  labs(title='MAXIMUM TOLERATED DOSE',y="Oxygen Saturation(%)")


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
  theme(text=element_text(size=20),legend.text=element_text(size=20),legend.position = "bottom",aspect.ratio = 0.75)+
  facet_wrap(~ID,nrow=2)+
  labs(title='CONTROL',y="Oxygen Saturation(%)")


#StO2 plot per week per group plus means with color per subject
pd=position_dodge(1.0)
ggplot(data1,aes(x=Week, y=StO2, linetype=Treatment))+theme_bw()+
  theme(text=element_text(size=20),aspect.ratio = 1)+
  geom_point(aes(color=factor(ID)), position=pd, size=2,show.legend=FALSE)+
  geom_line(aes(color=factor(ID)), linetype=1, position = pd,size=1.5,show.legend=FALSE)+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=StO2min, ymax=StO2max,color=factor(ID),linetype=NULL), width=2,position=pd,size=0.9,show.legend = FALSE)+
  stat_summary(aes(group=Treatment), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  labs(title='',y="Oxygen Saturation(%)")+
  facet_grid(~Treatment)

##StO2 plot per week per subject
#StO2 plot per week per group plus means with color per subject
############################################################################
############################################################################
pd=position_dodge(1.0)
ggplot(data1,aes(x=Week, y=StO2, linetype=Treatment))+theme_bw()+
  theme(text=element_text(size=20))+
  geom_point(aes(color=factor(Treatment)), position=pd, size=2,show.legend=FALSE)+
  geom_line(aes(color=factor(Treatment)), linetype=1, position = pd,size=1.5,show.legend=TRUE)+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=StO2min, ymax=StO2max,color=factor(Treatment),linetype=NULL), width=2,position=pd,size=0.9,show.legend = FALSE)+
  stat_summary(aes(group=Treatment), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  labs(title='',y="Oxygen Saturation(%)")+
  facet_wrap(~Treatment)
#########################################################################
###################Sto2 per subject per group best one##############################
pd=position_dodge(1.0)
ggplot(data1,aes(x=Week, y=StO2, linetype=ID))+theme_bw()+
  theme(text=element_text(size=20), aspect.ratio = 1)+
  geom_point(aes(color=factor(ID)), position=pd, size=2,show.legend=FALSE)+
  geom_line(aes(color=factor(ID)), linetype=1, position = pd,size=1.5,show.legend=FALSE)+
  scale_x_continuous(breaks=seq(1,6,1))+
  geom_errorbar(aes(ymin=StO2min, ymax=StO2max,color=factor(ID),linetype=NULL), width=2,position=pd,size=0.9,show.legend = FALSE)+
  #stat_summary(aes(group=ID), fun.y=mean, geom="line",linetype=1, size=1.5,show.legend=FALSE)+
  labs(title='',y="Oxygen Saturation(%)")+
  facet_wrap(~Treatment)



############################################################################
##########################################################################

#Hb plot per week per group plus means with color per subject
pd=position_dodge(1.0)
ggplot(data1,aes(x=Week, y=Hb, linetype=Treatment))+theme_bw()+
  theme(text=element_text(size=20),aspect.ratio = 1)+
  geom_point(aes(color=factor(Treatment)), position=pd, size=2,show.legend=FALSE)+
  geom_line(aes(color=factor(Treatment)), linetype=1,position = pd,size=1.5,show.legend=TRUE)+
  geom_errorbar(aes(ymin=Hbmin, ymax=Hbmax,color=factor(Treatment),linetype=NULL), width=2,position=pd,size=0.9,show.legend = FALSE)+
  scale_x_continuous(breaks=seq(1,6,1))+
  ylim(2,6)+
  stat_summary(aes(group=Treatment), fun.y=mean, geom="line",linetype=1,size=1.5, show.legend=FALSE)+
  labs(title='',y="Hb(mg/mL)")+
  facet_wrap(~ID)

###########################Best one################################


pd=position_dodge(1.0)
ggplot(data1,aes(x=Week, y=Hb, linetype=ID))+theme_bw()+
  theme(text=element_text(size=20),aspect.ratio=1)+
  geom_point(aes(color=factor(ID)), position=pd, size=2,show.legend=FALSE)+
  geom_line(aes(color=factor(ID)), linetype=1,position = pd,size=1.5,show.legend=FALSE)+
  geom_errorbar(aes(ymin=Hbmin, ymax=Hbmax,color=factor(ID),linetype=NULL), width=2,position=pd,size=0.9,show.legend = FALSE)+
  scale_x_continuous(breaks=seq(1,6,1))+
  ylim(2,6)+
  stat_summary(aes(group=Treatment), fun.y=mean, geom="line",linetype=1,size=1.5, show.legend=FALSE)+
  labs(title='',y="Hb(mg/mL)")+
  facet_wrap(~Treatment)
############################################################################
#####################################################################



#Hb plot per week per group boxplots ****best one****
ggplot(data1,aes(x=Week,y=Hb, colour=Treatment))+
  theme_bw()+
  theme(text=element_text(size=20),aspect.ratio = 1)+
  geom_boxplot(aes(color=Treatment, group=Week),outlier.size=3)+
  labs(title='',y="Hb (mg/mL)")+
  stat_summary(aes(group=Treatment), fun.y=mean, geom="line",size=1.5)+
  scale_x_continuous(breaks=seq(1,6,1))+
  facet_wrap(~Treatment)


##Calculating means 
means1<-aggregate(data1$StO2, by=list(time=data1$Week,group=data1$Treatment),na.rm=TRUE,FUN=mean) #from StO2
sdevs1<-aggregate(data1$StO2, by=list(time=data1$Week,group=data1$Treatment),na.rm=TRUE,FUN=sd) #from StO2
means1$StO2<-means1$x
means1$SDStO2<-sdevs1$x
means2<-aggregate(data1$Hb, by=list(time=data1$Week,group=data1$Treatment),na.rm=TRUE,FUN=mean) #from Hb
sdevs2<-aggregate(data1$Hb, by=list(time=data1$Week,group=data1$Treatment),na.rm=TRUE,FUN=sd) #from Hb
means2$Hb<-means2$x
means2$HbSD<-sdevs2$x
means3<-aggregate(data1$A, by=list(time=data1$Week,group=data1$Treatment),na.rm=TRUE,FUN=mean) #from Hb
sdevs3<-aggregate(data1$A, by=list(time=data1$Week,group=data1$Treatment),na.rm=TRUE,FUN=sd) #from Hb
means3$A<-means3$x
means3$ASD<-sdevs3$x
####Oxy and deoxyhemoglobin data
OxyHbmeans<-aggregate(data1$OxyHb, by=list(time=data1$Week,group=data1$Treatment),na.rm=TRUE,FUN=mean) #from HbO2
OxyHbstd<-aggregate(data1$OxyHb, by=list(time=data1$Week,group=data1$Treatment),na.rm=TRUE,FUN=sd) #from HbO2
OxyHbmeans$OxyHemoglobin<-OxyHbmeans$x
OxyHbmeans$OxyHbSD<-OxyHbstd$x

DeoxyHbmeans<-aggregate(data1$DeoxyHb, by=list(time=data1$Week,group=data1$Treatment),na.rm=TRUE,FUN=mean) #from Hb
DeoxyHbstd<-aggregate(data1$DeoxyHb, by=list(time=data1$Week,group=data1$Treatment),na.rm=TRUE,FUN=sd) #from Hb
DeoxyHbmeans$DeoxyHemoglobin<-DeoxyHbmeans$x
DeoxyHbmeans$DeoxyHbSD<-DeoxyHbstd$x


###plotting mean values####
ggplot(means1,aes(x=time,y=StO2))+geom_line()+facet_wrap(~group) #plot mean StO2 values
ggplot(means2,aes(x=time,y=Hb))+geom_line()+facet_wrap(~group) #plot mean Hb values
ggplot(means3,aes(x=time,y=A))+geom_line()+facet_wrap(~group) #plot mean scattering coeff values
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
StO2basevals<-means1[means1$time==1, c("group", "StO2")] #extract initial StO2 values per group
means1<-merge(means1,StO2basevals, by.x="group", by.y='group',suffixes = c("","basevalue"))#copy StO2 base values per group

Hbbasevals<-means2[means2$time==1,c("group",'Hb')] #extract initial Hb values per group
means2<-merge(means2,Hbbasevals, by.x="group", by.y='group',suffixes = c("","basevalue"))#copy StO2 base values per group


OxyHbbasevals<-OxyHbmeans[OxyHbmeans$time==1, c("group", "OxyHemoglobin")] #extract initial HbO2 values per group
OxyHbmeans<-merge(OxyHbmeans,OxyHbbasevals, by.x="group", by.y='group',suffixes = c("","basevalue"))#copy HbO2 base values per group


DeoxyHbbasevals<-DeoxyHbmeans[DeoxyHbmeans$time==1, c("group", "DeoxyHemoglobin")] #extract initial Hb values per group
DeoxyHbmeans<-merge(DeoxyHbmeans,DeoxyHbbasevals, by.x="group", by.y='group',suffixes = c("","basevalue"))#copy Hb base values per group

Abasevals<-means3[means3$time==1, c("group", "A")] #extract initial StO2 values per group
means3<-merge(means3,Abasevals, by.x="group", by.y='group',suffixes = c("","basevalue"))#copy StO2 base values per group

##getting a subset of StO2 values without NAs
library(dplyr)
Trimmed<-data1%>%filter(!is.na(StO2)) #extracting the non NA values 
Numbers<-aggregate(Trimmed$StO2, by=list(time=Trimmed$Week,group=Trimmed$Treatment),FUN=length) #counting the number of occurrences per group per week
names(Numbers)<-c('Week','Group','Observations')
#Calculate fold change and pasting number of observations per group per week
##StO2
means1$StO2foldchange<-means1$StO2/means1$StO2basevalue #Mean StO2 fold change
means1$Observations<-Numbers$Observations
means1$StandErr<-means1$SD/(means1$Observations)^0.5
means1$FoldErr<-means1$StandErr/means1$StO2basevalue


##total hemoglobin
means2$Hbfoldchange<-means2$Hb/means2$Hbbasevalue #Mean Hb fold change
means2$Observations<-Numbers$Observations
means2$StandErr<-means2$HbSD/(means2$Observations)^0.5
means2$FoldErr<-means2$StandErr/means2$Hbbasevalue

##A (reduced scattering coefficient)
means3$Afoldchange<-means3$A/means3$Abasevalue #Mean A fold change
means3$Observations<-Numbers$Observations
means3$StandErr<-means3$ASD/(means3$Observations)^0.5
means3$FoldErr<-means3$StandErr/means3$Abasevalue
###HbO2####
OxyHbmeans$OxyHbfoldchange<-OxyHbmeans$OxyHemoglobin/OxyHbmeans$OxyHemoglobinbasevalue
OxyHbmeans$Observations<-Numbers$Observations
OxyHbmeans$StandErr<-OxyHbmeans$SD/(OxyHbmeans$Observations)
OxyHbmeans$FoldErr<-OxyHbmeans$StandErr/OxyHbmeans$OxyHemoglobinbasevalue

##Hb###
DeoxyHbmeans$DeoxyHbfoldchange<-DeoxyHbmeans$DeoxyHemoglobin/DeoxyHbmeans$DeoxyHemoglobinbasevalue
DeoxyHbmeans$Observations<-Numbers$Observations
DeoxyHbmeans$StandErr<-DeoxyHbmeans$SD/(DeoxyHbmeans$Observations)
DeoxyHbmeans$FoldErr<-DeoxyHbmeans$StandErr/DeoxyHbmeans$DeoxyHemoglobinbasevalue

#plot fold changes
pd=position_dodge(0.4)
ggplot(means1,aes(x=time, y=StO2foldchange))+theme_bw()+geom_point(aes(color=group,group=group),size=2, position=pd)+geom_line(aes(color=group,group=group),size=1.5,position=pd)+geom_errorbar(aes(ymin=StO2foldchange-FoldErr,ymax=StO2foldchange+FoldErr,color=group),width=0.4, size=0.9,position=pd)+theme(text=element_text(size=30),aspect.ratio = 1)+scale_x_continuous(breaks=seq(1,6,1))+labs(title=expression(StO[2]), y='', x='Weeks')+theme(plot.title = element_text(hjust = 0.5)) #plot StO2 fold change
ggplot(means2,aes(x=time, y=Hbfoldchange))+theme_bw()+geom_point(aes(color=group,group=group),size=2, position=pd)+geom_line(aes(color=group,group=group),size=1.5, position=pd)+geom_errorbar(aes(ymin=Hbfoldchange-FoldErr,ymax=Hbfoldchange+FoldErr,color=group),width=0.4, size=0.9, position=pd)+theme(text=element_text(size=30),aspect.ratio = 1)+scale_x_continuous(breaks=seq(1,6,1))+labs(title=expression(tHb), y='', x='Weeks')+theme(plot.title = element_text(hjust = 0.5)) #plot Hb fold change
ggplot(means3,aes(x=time, y=Afoldchange))+theme_bw()+geom_point(aes(color=group,group=group),size=2, position=pd)+geom_line(aes(color=group,group=group),size=1.5, position=pd)+geom_errorbar(aes(ymin=Afoldchange-FoldErr,ymax=Afoldchange+FoldErr,color=group),width=0.4, size=0.9, position=pd)+theme(text=element_text(size=30),aspect.ratio = 1)+scale_x_continuous(breaks=seq(1,6,1))+labs(title=expression(mu[s]^{"'"}), y='', x='Weeks')+theme(plot.title = element_text(hjust = 0.5)) #plot A fold change
ggplot(OxyHbmeans,aes(x=time, y=OxyHbfoldchange))+theme_bw()+geom_point(aes(color=group,group=group),size=2, position=pd)+geom_line(aes(color=group,group=group),size=1.5,position=pd)+geom_errorbar(aes(ymin=OxyHbfoldchange-FoldErr,ymax=OxyHbfoldchange+FoldErr,color=group),width=0.4, size=0.9,position=pd)+theme(text=element_text(size=30),aspect.ratio = 1)+scale_x_continuous(breaks=seq(1,6,1))+labs(title=expression(HbO[2]), y='', x='Weeks')+theme(plot.title = element_text(hjust = 0.5)) #plot HbO2 fold change
ggplot(DeoxyHbmeans,aes(x=time, y=DeoxyHbfoldchange))+theme_bw()+geom_point(aes(color=group,group=group),size=2, position=pd)+geom_line(aes(color=group,group=group),size=1.5,position=pd)+geom_errorbar(aes(ymin=DeoxyHbfoldchange-FoldErr,ymax=DeoxyHbfoldchange+FoldErr,color=group),width=0.4, size=0.9,position=pd)+theme(text=element_text(size=30),aspect.ratio = 1)+scale_x_continuous(breaks=seq(1,6,1))+labs(title=expression(Hb), y='', x='Weeks')+theme(plot.title = element_text(hjust = 0.5)) #plot Hb fold change


##implementing LMEM on the data
#StO2 model
# LMEM with mixed effects for treatment, time and interaction between time and treatment, random effect for each animal (intercepts for each animal)
# this model DOES NOT WORK, CHECKING THE INTERVALS THE VARIATION IN STANDARD DEVIATIONS IS COMPLETELY NONSENSE. Same results when the marginal
#model was tested

model1.fit<-lme(StO2~Treatment+Week+Week:Treatment,random =~1|ID, method='REML',data=data1, na.action=na.omit)
summary(model1.fit) #summary of the model
anova(model1.fit) #run ANOVA test for the fixed effects
#Change the model omitting random effects to see if they need to be kept up
model1.fitTest<-gls(StO2~Treatment+Week+Week:Treatment,data=data1, na.action = na.omit)
##test between models by comparing the -2 REM log-likehood values
anova(model1.fit,model1.fitTest)
#p value of the anova is 0.999, therefore the random effects can be removed


###Therefore fit the marginal model with an ununstructured covariance matrix### Page 302 of the book of Brady et. al.
#Suggest because of small number of observations on each subject, and at the same time -same case of our data-


## this marginal model has a diagonal structure in the Ri matrix (Default) perhaps best approach according to Pinherio and Bates?? (p. 297)
marg.model.fit1<-gls(StO2~Treatment+Week+Treatment:Week,data1, na.action = na.omit)
summary(marg.model.fit1)
anova(marg.model.fit1)
summary(glht(marg.model.fit1),linfct=mcp(Treatment='Tukey'))
#this model sets up a heterogeneous residual variance structure, with observations at each levels of treatment having different residual variance parameters
glsControl(maxIter=500, msMaxIter = 2000)
marg.model.fit2<-gls(StO2~Treatment+Week+Treatment:Week,correlation=corSymm(form=~Week|ID),weights = varIdent(form=~1|Week),data1, control=TRUE, na.action = na.omit)


#Test the signficance of the fixed terms
anova(model1.fitTest)

summary(model1.fitTest) #summary of the model



summary(glht(model1.fit,linfct=mcp(Treatment='Tukey',interaction_average = TRUE, covariate_average = TRUE))) #Tukey's Post-Hoc comparisons, incluiding 
summary(glht(model1.fit,linfct=mcp(Time='Tukey',interaction_average = TRUE, covariate_average = TRUE))) #Tukey's Post-Hoc comparisons, incluiding 
#interaction between the covariates
#carry out likehood ratio tests omitting random effects to see if they are worth to be included in the final model
#model1a.fit<-

model2.fit<-lme(Hb~Treatment+Week+Treatment:Week,random = ~1|ID, method='REML',data=data1, na.action=na.omit)
# test the hypothesis that the random effects 

summary(model2.fit)
anova(model1.fit)
anova(model2.fit)
#Diagnostics for the model for StO2
##Variance residuals##
plot(model1.fit,resid(., type = "p") ~ fitted(.) | factor(Treatment),layout=c(3,1), aspect=2, abline=0) #assessing constant variance of residuals
plot(model1.fit, resid(., type = "p") ~ StO2| Treatment , abline = 0) #variance residuals of StO2 across time
qqnorm(model1.fit,~resid(.) | factor(Treatment),layout = c(3,1), aspect = 1, id = 0.05) #normality of residuals and label outliers

plot(model1.fit, resid(., type = "p") ~ fitted(.) | ID, abline = 0)
plot(model1.fit, StO2 ~ fitted(.) | ID, abline = c(0,1))
##Random effect diagnostics##
plot(model1.fit,~ranef(.))
qqnorm(model1.fit, ~ resid(., type = "p") | Treatment, abline = c(0, 1))

##Observed and predicted values 
plot(model1.fit, StO2 ~ fitted(.) | factor(Treatment), id = 0.05, layout = c(3,1) , aspect = 2,abline=c(0,1))



#Diagnostics for the model for Hb
plot(model2.fit, resid(., type = "p") ~ fitted(.) | ID, abline = 0)
plot(model2.fit, Hb ~ fitted(.) | ID, abline = c(0,1))
ggplot(model2.fit, Hb ~ fitted(.) | ID, abline = c(0,1))
qqnorm(model2.fit, ~ resid(., type = "p") | Treatment, abline = c(0, 1))

##Non parametric test using the nparLD package **model is correct***
library(nparLD)
model1.fit2<-f1.ld.f1(data1$StO2,data1$Week,data1$Treatment,data1$ID,time.name='', group.order = c('CG','MG','MTD'), group.name = 'Treatment')
print(model1.fit2)

##
model2.fit2<-f1.ld.f1(data1$Hb,data1$Week,data1$Treatment,data1$ID, time.name='', group.order = c('CG','MG','MTD'), group.name = 'Treatment')
print(model2.fit2)



#########################################################
#Use only the data from weeks 1-4, remove mice that had only one measurement. This reduces the missing data rate to 14%
Data_for_R_analysis_July_25th_modified_up_to_week_4 <- read_csv("//deckard.uark.edu/BMEG/Muldoon-Lab/RESEARCH/AM_ Ortothopic mice study_ colonoscopy DRS data for analysis/Data for first extraction Folder made on May 17th 2019/Data for R analysis July 25th modified up to week 4.csv")
data3<-Data_for_R_analysis_July_25th_modified_up_to_week_4
data3$Treatment<-factor(data3$Treatment)
model3.fit<-lme(StO2~Treatment*Week,random = ~1|ID, method='REML',data=data3, na.action=na.omit)
summary(model3.fit)
plot(model3.fit)
intervals(model3.fit)
summary(glht(model3.fit,linfct=mcp(Treatment='Tukey',interaction_average = TRUE, covariate_average = TRUE))) #Tukey's Post-Hoc comparisons, incluiding 
model3.fit2<-lmer(StO2~Treatment+(Treatment|ID),data=data3)

#############################################################
#Use  the data from weeks 1-6, remove mice that had only one measurement. This reduces the missing data rate to 23%
Data_for_R_analysis_July_25th_modified_up_to_week_6 <- read_csv("//deckard.uark.edu/BMEG/Muldoon-Lab/RESEARCH/AM_ Ortothopic mice study_ colonoscopy DRS data for analysis/Data for first extraction Folder made on May 17th 2019/Data for R analysis July 25th modified for 6 weeks.csv")
data4<-Data_for_R_analysis_July_25th_modified_up_to_week_6
#data4$Treatment<-factor(data4$Treatment)
model4.fit<-lme(StO2~Treatment+Week+Treatment:Week,random = ~1|ID, method='REML',data=data4, na.action=na.omit)
summary(model4.fit)
plot(model4.fit)
intervals(model4.fit)
summary(glht(model4.fit,linfct=mcp(Treatment='Tukey',interaction_average = TRUE, covariate_average = TRUE))) #Tukey's Post-Hoc comparisons, incluiding 
#model3.fit2<-lmer(StO2~Treatment+(Treatment|ID),data=data3)
######
library(nparLD)
model5.test1<-f1.ld.f1(data3$StO2,data3$Week,data3$Treatment,data3$ID,time.name='', group.order = c('CG','MG','MTD'), group.name = 'Treatment')
print(model5.test1)

model6.test1<-f1.ld.f1(data4$StO2,data4$Week,data4$Treatment,data4$ID,time.name='', group.order = c('CG','MG','MTD'), group.name = 'Treatment')
print(model6.test1)

####Calculate power of study
library(lme4)
testing<-lmer(StO2~Treatment*Week+(1|ID),data=data4, REML=T, na.action = na.omit) #creat a linear mixed effect model 
testing1<-lmer(StO2~Treatment+(1|ID),data=data4, REML=T, na.action = na.omit) #creat a linear mixed effect model)
summary(testing)
library(simr)
sim1<-powerSim(testing,test=fcompare(StO2~Treatment), nsim=100) #power calculation for model with no time interaction
sim2<-powerSim(testing,test=fcompare(StO2~Week), nsim=100) #power calculation for model with no treatment interaction
print(sim1)
print(sim2)
curve1<-powerCurve(testing, test=fcompare(StO2~Treatment), along ="ID", breaks=c(5,10,15,20), nsim=100)
plot(curve1)
