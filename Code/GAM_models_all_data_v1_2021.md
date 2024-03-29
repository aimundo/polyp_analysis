---
title: "**DRS GAMs FOR COMPLETE DATA SET V1**"
author: Ariel Mundo
format:
  html:
    css: style.css
    toc: true
    number-sections: true
execute:
  warning: false
  keep-md: true
editor_options: 
  chunk_output_type: console
---

# Setup

::: {.cell}

:::

Load data. Variable `GROUP` needs to be a factor. Also, create the Oxyhemoglobin and Deoxyhemoglobin variables and add them to the dataframe.

::: {.cell}

```{.r .cell-code}
data1 <- read.csv(here("data","Extraction_merged_2018_2020_2021_cohorts_v1.csv"))

data1$GROUP<-as.factor(data1$GROUP)
data1$HBO2<-(data1$HB*data1$STO2)
data1$HBO2_SD<-data1$HB_SD*data1$STO2
data1$HBO<-data1$HB*(1-data1$STO2)
data1$HBO_SD<-data1$HB_SD*(1-data1$STO2)
```
:::

We will plot all the raw data adding a `geom_smooth` line to have a sense of the trend of the data. **Ignore the confidence interval, the smooth is just to have a general idea.**

# Plots
::: {.cell fig.fullwidth="true"}

```{.r .cell-code}
#StO2 plot per week per group with means ****best one****
pd=position_dodge(0.2)

p1<-ggplot(data1,aes(x=DAY, y=STO2))+
  theme_bw()+
  geom_point(aes(color=factor(GROUP)), 
             position=pd, 
             size=4, 
             alpha=0.6,
             show.legend=FALSE)+
 geom_smooth(show.legend=FALSE)+
  # stat_summary(aes(group=GROUP,
  #                  color=factor(GROUP)
  #                  ), 
  #              fun=mean, 
  #              geom="line",
  #              linetype=1, 
  #              size=1.5,
  #              show.legend=FALSE
  #              )+
  scale_color_viridis_d(end = 0.8)+
  labs(title='',
       x="Week",
       y="Oxygen \nSaturation(%)")+
  facet_wrap(~GROUP,ncol=5)

#Hb plot
p2<-ggplot(data1,aes(x=DAY, y=HB))+
  theme_bw()+
  geom_point(aes(color=factor(GROUP)), 
             position=pd, 
             size=4, 
             alpha=0.6,
             show.legend=FALSE)+
    geom_smooth(show.legend=FALSE)+
  # stat_summary(aes(group=GROUP,
  #                  color=factor(GROUP)
  #                  ), 
  #              fun=mean, 
  #              geom="line",
  #              linetype=1, 
  #              size=1.5,
  #              show.legend=FALSE)+
  scale_color_viridis_d(end = 0.8)+
  labs(title='',
       x="Week",
       y="total Hb \n(mg/mL)")+
  facet_wrap(~GROUP,ncol=5)

#HbO2 plot
p3<-ggplot(data1,aes(x=DAY, y=HBO2))+
  theme_bw()+
  geom_point(aes(color=factor(GROUP)), 
             position=pd, 
             size=4, 
             alpha=0.6,
             show.legend=FALSE)+
  geom_smooth(show.legend=FALSE)+
  # stat_summary(aes(group=GROUP,
  #                  color=factor(GROUP)
  #                  ), 
  #              fun=mean, 
  #              geom="line",
  #              linetype=1, 
  #              size=1.5,
  #              show.legend=FALSE)+
  scale_color_viridis_d(end = 0.8)+
  labs(title='',
       x="Week",
       y="Oxyhemoglobin \n(mg/mL)")+
  facet_wrap(~GROUP,ncol=5)

#HbO plot

p4<-ggplot(data1,aes(x=DAY, y=HBO))+
  theme_bw()+
  geom_point(aes(color=factor(GROUP)), 
             position=pd,  
             alpha=0.6,
             size=4)+
  geom_smooth(show.legend=FALSE)+
  # stat_summary(aes(group=GROUP,
  #                  color=factor(GROUP)
  #                  ), 
  #              fun=mean, 
  #              geom="line",
  #              linetype=1, 
  #              size=1.5,
  #              show.legend=FALSE)+
  scale_color_viridis_d(end = 0.8)+
  labs(title='',
       x="Week",
       y="Deoxyhemoglobin \n(mg/mL)")+
  facet_wrap(~GROUP,ncol=5)

p1+p2+p3+p4 &
  theme(text=element_text(family="Atkinson",size=20))
```

::: {.cell-output-display}
![](GAM_models_all_data_v1_2021_files/figure-html/plots-1.png){width=1344}
:::
:::

# GAMs

GAM with interaction of group and time:
All models will follow the same basic construction:

`model<-gam(Variable~Group+s(Day,by=Group=k=6,` \n
`method= 'REML',`\n
`data=data1)`

::: {.cell}

```{.r .cell-code}
pnt<-4
al<-0.2
StO2_gam <- gam(STO2 ~ GROUP+s(DAY, by = GROUP, k = 6),
            method='REML',
            data  = data1)

appraise(StO2_gam)
```

::: {.cell-output-display}
![](GAM_models_all_data_v1_2021_files/figure-html/StO2 GAM-1.png){width=672}
:::

```{.r .cell-code}
#creates a dataframe using the length of the covariates for the GAM
gam_predict_StO2 <- expand_grid(GROUP = factor(c("CG", "MG","MTD")),
                         DAY = seq(1, 6, by = 0.1),
                         ID=factor(data1$ID))


gam_predict_StO2<-gam_predict_StO2%>%
    mutate(fit = predict(StO2_gam,gam_predict_StO2,se.fit = TRUE,type='response')$fit,
           se.fit = predict(StO2_gam, gam_predict_StO2,se.fit = TRUE,type='response')$se.fit)


#plot model

#plot smooths and confidence interval for GAM
GAM_StO2_plot<-ggplot(data=data1, aes(x=DAY, y=STO2, group=GROUP)) +
    geom_point(aes(color=GROUP),size=pnt,alpha=al,show.legend = FALSE)+
  geom_ribbon(aes( x=DAY,ymin=(fit - 2*se.fit), 
                   ymax=(fit + 2*se.fit),
                   fill=GROUP
                   ),
              alpha=0.3,
              data=gam_predict_StO2,
            show.legend=FALSE,
                inherit.aes=FALSE) +
  geom_line(aes(y=fit,
                color=GROUP),
              size=2,
            data=gam_predict_StO2,
              show.legend = FALSE)+
  #facet_wrap(~Group)+
  labs(y=expression(atop(StO[2],'complete')),x="Week")+
    scale_x_continuous(breaks=c(0,2,4,6))+
      theme_classic()+
  scale_color_viridis_d(option="magma",end=.7)

model1<-GAM_StO2_plot+facet_wrap(~GROUP)
```
:::

OxyHemglobin GAM

::: {.cell}

```{.r .cell-code}
HbO2_gam <- gam(HBO2 ~ GROUP+s(DAY, by = GROUP, k = 6),
            method='REML',
            data  = data1)

#creates a dataframe using the length of the covariates for the GAM
gam_predict_HbO2 <- expand_grid(GROUP = factor(c("CG", "MG","MTD")),
                         DAY = seq(1, 6, by = 0.1),
                         ID=factor(data1$ID))


gam_predict_HbO2<-gam_predict_HbO2%>%
    mutate(fit = predict(HbO2_gam,gam_predict_HbO2,se.fit = TRUE,type='response')$fit,
           se.fit = predict(HbO2_gam, gam_predict_HbO2,se.fit = TRUE,type='response')$se.fit)


#plot model

#plot smooths and confidence interval for GAM
GAM_HbO2_plot<-ggplot(data=data1, aes(x=DAY, y=HBO2, group=GROUP)) +
    geom_point(aes(color=GROUP),size=pnt,alpha=al,show.legend = FALSE)+
  geom_ribbon(aes( x=DAY,ymin=(fit - 2*se.fit), 
                   ymax=(fit + 2*se.fit),
                   fill=GROUP
                   ),
              alpha=0.3,
              data=gam_predict_HbO2,
            show.legend=FALSE,
                inherit.aes=FALSE) +
  geom_line(aes(y=fit,
                color=GROUP),
              size=2,
            data=gam_predict_HbO2,
              show.legend = FALSE)+
  #facet_wrap(~Group)+
  labs(y=expression(atop(HbO[2],'complete')),x="Week")+
    scale_x_continuous(breaks=c(0,2,4,6))+
      theme_classic()+
 scale_color_viridis_d(option="magma",end=.7)

model2<-GAM_HbO2_plot+facet_wrap(~GROUP)
```
:::

Total Hemoglobin GAM

::: {.cell}

```{.r .cell-code}
tHb_gam <- gam(HB ~ GROUP+s(DAY, by = GROUP, k = 6),
            method='REML',
            data  = data1)

#creates a dataframe using the length of the covariates for the GAM
gam_predict_tHb <- expand_grid(GROUP = factor(c("CG", "MG","MTD")),
                         DAY = seq(1, 6, by = 0.1),
                         ID=factor(data1$ID))


gam_predict_tHb<-gam_predict_tHb%>%
    mutate(fit = predict(tHb_gam,gam_predict_tHb,se.fit = TRUE,type='response')$fit,
           se.fit = predict(tHb_gam, gam_predict_tHb,se.fit = TRUE,type='response')$se.fit)


#plot model

#plot smooths and confidence interval for GAM
GAM_tHb_plot<-ggplot(data=data1, aes(x=DAY, y=HB, group=GROUP)) +
    geom_point(aes(color=GROUP),size=pnt,alpha=al,show.legend = FALSE)+
  geom_ribbon(aes( x=DAY,ymin=(fit - 2*se.fit), 
                   ymax=(fit + 2*se.fit),
                   fill=GROUP
                   ),
              alpha=0.3,
              data=gam_predict_tHb,
            show.legend=FALSE,
                inherit.aes=FALSE) +
  geom_line(aes(y=fit,
                color=GROUP),
              size=2,
            data=gam_predict_tHb,
              show.legend = FALSE)+
  #facet_wrap(~Group)+
  labs(y="tHb",x="Week")+
    scale_x_continuous(breaks=c(0,2,4,6))+
      theme_classic()+
 scale_color_viridis_d(option="magma",end=.7)

model3<-GAM_tHb_plot+facet_wrap(~GROUP)
```
:::


Deoxyhemoglobin GAM

::: {.cell}

```{.r .cell-code}
HbO_gam <- gam(HBO ~ GROUP+s(DAY, by = GROUP, k = 6),
            method='REML',
            data  = data1)

#creates a dataframe using the length of the covariates for the GAM
gam_predict_HbO <- expand_grid(GROUP = factor(c("CG", "MG","MTD")),
                         DAY = seq(1, 6, by = 0.1),
                         ID=factor(data1$ID))


gam_predict_HbO<-gam_predict_HbO2%>%
    mutate(fit = predict(HbO_gam,gam_predict_HbO,se.fit = TRUE,type='response')$fit,
           se.fit = predict(HbO_gam, gam_predict_HbO,se.fit = TRUE,type='response')$se.fit)


#plot model

#plot smooths and confidence interval for GAM
GAM_HbO_plot<-ggplot(data=data1, aes(x=DAY, y=HBO, group=GROUP)) +
    geom_point(aes(color=GROUP),size=pnt,alpha=al,show.legend = FALSE)+
  geom_ribbon(aes( x=DAY,ymin=(fit - 2*se.fit), 
                   ymax=(fit + 2*se.fit),
                   fill=GROUP
                   ),
              alpha=0.3,
              data=gam_predict_HbO,
            show.legend=FALSE,
                inherit.aes=FALSE) +
  geom_line(aes(y=fit,
                color=GROUP),
              size=2,
            data=gam_predict_HbO,
              show.legend = FALSE)+
  #facet_wrap(~Group)+
  labs(y=expression(atop(HbO,'complete')),x="Week")+
    scale_x_continuous(breaks=c(0,2,4,6))+
      theme_classic()+
   scale_color_viridis_d(option="magma",end=.7)

model4<-GAM_HbO_plot+facet_wrap(~GROUP)
```
:::


Now, plot all the models at once to compare:

::: {.cell}

```{.r .cell-code}
(model1+model2)/(model3+model4)+
  plot_annotation(tag_levels="A")& 
  theme(text=element_text(size=20,family="Atkinson"))
```

::: {.cell-output-display}
![](GAM_models_all_data_v1_2021_files/figure-html/plot all models-1.png){width=1344}
:::
:::


# Pairwise comparisons for the models
::: {.cell}

```{.r .cell-code}
br<-c(0,2,4,6)
rib_col<-'#EDD03AFF' #color for ribbon
```
:::

## StO2

## CG-MG comparison
::: {.cell}

```{.r .cell-code}
##Pairwise comparisons

pdat <- expand.grid(DAY = seq(1, 6, length = 400),
                    GROUP = c('CG', 'MG'))

#this function takes the model, grid and groups to be compared using the lpmatrix

smooth_diff <- function(model, newdata, g1, g2, alpha = 0.05,
                        unconditional = FALSE) {
    xp <- predict(model, newdata = newdata, type = 'lpmatrix')
    #Find columns in xp where the name contains "Control" and "Treatment"
    col1 <- grepl(g1, colnames(xp))
    col2 <- grepl(g2, colnames(xp))
    #Find rows in xp that correspond to each treatment
    row1 <- with(newdata, GROUP == g1)
    row2 <- with(newdata, GROUP == g2)
    ## difference rows of xp for data from comparison
    X <- xp[row1, ] - xp[row2, ]
    ## zero out cols of X related to splines for other lochs
    X[, ! (col1 | col2)] <- 0
    ## zero out the parametric cols
    #X[, !grepl('^s\\(', colnames(xp))] <- 0
    dif <- X %*% coef(model)
    se <- sqrt(rowSums((X %*% vcov(model, unconditional = unconditional)) * X))
    crit <- qt(alpha/2, df.residual(model), lower.tail = FALSE)
    upr <- dif + (crit * se)
    lwr <- dif - (crit * se)
    data.frame(pair = paste(g1, g2, sep = '-'),
               diff = dif,
               se = se,
               upper = upr,
               lower = lwr)
}

comp1<-smooth_diff(StO2_gam,pdat,'CG','MG')

comp_StO2_full <- cbind(Day = seq(1, 6, length = 400),
                   rbind(comp1)) %>%
  mutate(interval=case_when(
    upper>0 & lower<0~"no-diff",
    upper<0~"less",
    lower>0~"greater"
  ))


#object for plotting
StO2_CG_MG<-ggplot(comp_StO2_full, aes(x = Day, y = diff, group = pair)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.5,
                fill=rib_col) +
    geom_line(data=comp_StO2_full,aes(y=0),size=0.5)+
    geom_line(color='black',size=1) +
    facet_wrap(~ pair) +
    theme_classic()+
    labs(x = 'Days', y = expression(paste('Difference in StO'[2] )))+
    scale_x_continuous(breaks=br)+
    theme(
        text=element_text(size=18),
        legend.title=element_blank()
    )
```
:::


### CG-MTD comparisons

::: {.cell}

```{.r .cell-code}
pdat <- expand.grid(DAY = seq(1, 6, length = 400),
                    GROUP = c('CG', 'MTD'))

comp2<-smooth_diff(StO2_gam,pdat,'CG','MTD')

comp_StO2_full <- cbind(Day = seq(1, 6, length = 400),
                   rbind(comp2)) %>%
  mutate(interval=case_when(
    upper>0 & lower<0~"no-diff",
    upper<0~"less",
    lower>0~"greater"
  ))




#object for plotting
StO2_CG_MTD<-ggplot(comp_StO2_full, aes(x = Day, y = diff, group = pair)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.5,
                fill=rib_col) +
    geom_line(data=comp_StO2_full,aes(y=0),size=0.5)+
    geom_line(color='black',size=1) +
    facet_wrap(~ pair) +
    theme_classic()+
    labs(x = 'Days', y = expression(paste('Difference in StO'[2] )))+
    scale_x_continuous(breaks=br)+
    theme(
        text=element_text(size=18),
        legend.title=element_blank()
    )
```
:::

### MG-MTD comparison

::: {.cell}

```{.r .cell-code}
pdat <- expand.grid(DAY = seq(1, 6, length = 400),
                    GROUP = c('MG', 'MTD'))

comp3<-smooth_diff(StO2_gam,pdat,'MG','MTD')

comp_StO2_full <- cbind(Day = seq(1, 6, length = 400),
                   rbind(comp3)) %>%
  mutate(interval=case_when(
    upper>0 & lower<0~"no-diff",
    upper<0~"less",
    lower>0~"greater"
  ))




#object for plotting
StO2_MG_MTD<-ggplot(comp_StO2_full, aes(x = Day, y = diff, group = pair)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.5,
                fill=rib_col) +
    geom_line(data=comp_StO2_full,aes(y=0),size=0.5)+
    geom_line(color='black',size=1) +
    facet_wrap(~ pair) +
    theme_classic()+
    labs(x = 'Days', y = expression(paste('Difference in StO'[2] )))+
    scale_x_continuous(breaks=br)+
    theme(
        text=element_text(size=18),
        legend.title=element_blank()
    )
```
:::


## HbO2 comparisons

## CG-MG comparison
::: {.cell}

```{.r .cell-code}
##Pairwise comparisons

pdat <- expand.grid(DAY = seq(1, 6, length = 400),
                    GROUP = c('CG', 'MG'))

#this function takes the model, grid and groups to be compared using the lpmatrix


comp4<-smooth_diff(HbO2_gam,pdat,'CG','MG')

comp_StO2_full <- cbind(Day = seq(1, 6, length = 400),
                   rbind(comp4)) %>%
  mutate(interval=case_when(
    upper>0 & lower<0~"no-diff",
    upper<0~"less",
    lower>0~"greater"
  ))




#object for plotting
HbO2_CG_MG<-ggplot(comp_StO2_full, aes(x = Day, y = diff, group = pair)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.5,
                fill=rib_col) +
    geom_line(data=comp_StO2_full,aes(y=0),size=0.5)+
    geom_line(color='black',size=1) +
    facet_wrap(~ pair) +
    theme_classic()+
    labs(x = 'Days', y = expression(paste('Difference in HbO'[2] )))+
    scale_x_continuous(breaks=br)+
    theme(
        text=element_text(size=18),
        legend.title=element_blank()
    )
```
:::


### CG-MTD comparisons

::: {.cell}

```{.r .cell-code}
pdat <- expand.grid(DAY = seq(1, 6, length = 400),
                    GROUP = c('CG', 'MTD'))

comp5<-smooth_diff(HbO2_gam,pdat,'CG','MTD')

comp_StO2_full <- cbind(Day = seq(1, 6, length = 400),
                   rbind(comp5)) %>%
  mutate(interval=case_when(
    upper>0 & lower<0~"no-diff",
    upper<0~"less",
    lower>0~"greater"
  ))


#object for plotting
HbO2_CG_MTD<-ggplot(comp_StO2_full, aes(x = Day, y = diff, group = pair)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.5,
                fill=rib_col) +
    geom_line(data=comp_StO2_full,aes(y=0),size=0.5)+
    geom_line(color='black',size=1) +
    facet_wrap(~ pair) +
    theme_classic()+
    labs(x = 'Days', y = expression(paste('Difference in StO'[2] )))+
    scale_x_continuous(breaks=br)+
    theme(
        text=element_text(size=18),
        legend.title=element_blank()
    )
```
:::

### MG-MTD comparison

::: {.cell}

```{.r .cell-code}
pdat <- expand.grid(DAY = seq(1, 6, length = 400),
                    GROUP = c('MG', 'MTD'))

comp6<-smooth_diff(HbO2_gam,pdat,'MG','MTD')

comp_StO2_full <- cbind(Day = seq(1, 6, length = 400),
                   rbind(comp6)) %>%
  mutate(interval=case_when(
    upper>0 & lower<0~"no-diff",
    upper<0~"less",
    lower>0~"greater"
  ))



#object for plotting
HbO2_MG_MTD<-ggplot(comp_StO2_full, aes(x = Day, y = diff, group = pair)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.5,
                fill=rib_col) +
    geom_line(data=comp_StO2_full,aes(y=0),size=0.5)+
    geom_line(color='black',size=1) +
    facet_wrap(~ pair) +
    theme_classic()+
    labs(x = 'Days', y = expression(paste('Difference in StO'[2] )))+
    scale_x_continuous(breaks=br)+
    theme(
        text=element_text(size=18),
        legend.title=element_blank()
    )
```
:::

## tHb comparisons

## CG-MG comparison

::: {.cell}

```{.r .cell-code}
##Pairwise comparisons

pdat <- expand.grid(DAY = seq(1, 6, length = 400),
                    GROUP = c('CG', 'MG'))

#this function takes the model, grid and groups to be compared using the lpmatrix


comp7<-smooth_diff(tHb_gam,pdat,'CG','MG')

comp_StO2_full <- cbind(Day = seq(1, 6, length = 400),
                   rbind(comp7)) %>%
  mutate(interval=case_when(
    upper>0 & lower<0~"no-diff",
    upper<0~"less",
    lower>0~"greater"
  ))


#object for plotting
tHb_CG_MG<-ggplot(comp_StO2_full, aes(x = Day, y = diff, group = pair)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.5,
                fill=rib_col) +
    geom_line(data=comp_StO2_full,aes(y=0),size=0.5)+
    geom_line(color='black',size=1) +
    facet_wrap(~ pair) +
    theme_classic()+
    labs(x = 'Days', y = expression(paste('Difference in tHb')))+
    scale_x_continuous(breaks=br)+
    theme(
        text=element_text(size=18),
        legend.title=element_blank()
    )
```
:::


### CG-MTD comparisons

::: {.cell}

```{.r .cell-code}
pdat <- expand.grid(DAY = seq(1, 6, length = 400),
                    GROUP = c('CG', 'MTD'))

comp8<-smooth_diff(tHb_gam,pdat,'CG','MTD')

comp_StO2_full <- cbind(Day = seq(1, 6, length = 400),
                   rbind(comp8)) %>%
  mutate(interval=case_when(
    upper>0 & lower<0~"no-diff",
    upper<0~"less",
    lower>0~"greater"
  ))



#object for plotting
tHb_CG_MTD<-ggplot(comp_StO2_full, aes(x = Day, y = diff, group = pair)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.5,
                fill=rib_col) +
    geom_line(data=comp_StO2_full,aes(y=0),size=0.5)+
    geom_line(color='black',size=1) +
    facet_wrap(~ pair) +
    theme_classic()+
    labs(x = 'Days', y = expression(paste('Difference in tHb' )))+
    scale_x_continuous(breaks=br)+
    theme(
        text=element_text(size=18),
        legend.title=element_blank()
    )
```
:::

### MG-MTD comparison

::: {.cell}

```{.r .cell-code}
pdat <- expand.grid(DAY = seq(1, 6, length = 400),
                    GROUP = c('MG', 'MTD'))

comp9<-smooth_diff(tHb_gam,pdat,'MG','MTD')

comp_StO2_full <- cbind(Day = seq(1, 6, length = 400),
                   rbind(comp9)) %>%
  mutate(interval=case_when(
    upper>0 & lower<0~"no-diff",
    upper<0~"less",
    lower>0~"greater"
  ))



#object for plotting
tHb_MG_MTD<-ggplot(comp_StO2_full, aes(x = Day, y = diff, group = pair)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.5,
                fill=rib_col) +
    geom_line(data=comp_StO2_full,aes(y=0),size=0.5)+
    geom_line(color='black',size=1) +
    facet_wrap(~ pair) +
    theme_classic()+
    labs(x = 'Days', y = expression(paste('Difference in tHb' )))+
    scale_x_continuous(breaks=c(0,2,5,7,10))+
    theme(
        text=element_text(size=18),
        legend.title=element_blank()
    )
```
:::

## HbO comparisons

## CG-MG comparison

::: {.cell}

```{.r .cell-code}
##Pairwise comparisons

pdat <- expand.grid(DAY = seq(1, 6, length = 400),
                    GROUP = c('CG', 'MG'))

#this function takes the model, grid and groups to be compared using the lpmatrix


comp10<-smooth_diff(HbO_gam,pdat,'CG','MG')

comp_StO2_full <- cbind(Day = seq(1, 6, length = 400),
                   rbind(comp10)) %>%
  mutate(interval=case_when(
    upper>0 & lower<0~"no-diff",
    upper<0~"less",
    lower>0~"greater"
  ))


#object for plotting
HbO_CG_MG<-ggplot(comp_StO2_full, aes(x = Day, y = diff, group = pair)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.5,
                fill=rib_col) +
    geom_line(data=comp_StO2_full,aes(y=0),size=0.5)+
    geom_line(color='black',size=1) +
    facet_wrap(~ pair) +
    theme_classic()+
    labs(x = 'Days', y = expression(paste('Difference in HbO')))+
    scale_x_continuous(breaks=br)+
    theme(
        text=element_text(size=18),
        legend.title=element_blank()
    )
```
:::


### CG-MTD comparisons

::: {.cell}

```{.r .cell-code}
pdat <- expand.grid(DAY = seq(1, 6, length = 400),
                    GROUP = c('CG', 'MTD'))

comp11<-smooth_diff(HbO_gam,pdat,'CG','MTD')

comp_StO2_full <- cbind(Day = seq(1, 6, length = 400),
                   rbind(comp11)) %>%
  mutate(interval=case_when(
    upper>0 & lower<0~"no-diff",
    upper<0~"less",
    lower>0~"greater"
  ))



#object for plotting
HbO_CG_MTD<-ggplot(comp_StO2_full, aes(x = Day, y = diff, group = pair)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.5,
                fill=rib_col) +
    geom_line(data=comp_StO2_full,aes(y=0),size=0.5)+
    geom_line(color='black',size=1) +
    facet_wrap(~ pair) +
    theme_classic()+
    labs(x = 'Days', y = expression(paste('Difference in HbO' )))+
    scale_x_continuous(breaks=br)+
    theme(
        text=element_text(size=18),
        legend.title=element_blank()
    )
```
:::

### MG-MTD comparison

::: {.cell}

```{.r .cell-code}
pdat <- expand.grid(DAY = seq(1, 6, length = 400),
                    GROUP = c('MG', 'MTD'))

comp12<-smooth_diff(HbO_gam,pdat,'MG','MTD')

comp_StO2_full <- cbind(Day = seq(1, 6, length = 400),
                   rbind(comp12)) %>%
  mutate(interval=case_when(
    upper>0 & lower<0~"no-diff",
    upper<0~"less",
    lower>0~"greater"
  ))



#object for plotting
HbO_MG_MTD<-ggplot(comp_StO2_full, aes(x = Day, y = diff, group = pair)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.5,
                fill=rib_col) +
    geom_line(data=comp_StO2_full,aes(y=0),size=0.5)+
    geom_line(color='black',size=1) +
    facet_wrap(~ pair) +
    theme_classic()+
    labs(x = 'Days', y = expression(paste('Difference in tHb' )))+
    scale_x_continuous(breaks=c(0,2,5,7,10))+
    theme(
        text=element_text(size=18),
        legend.title=element_blank()
    )
```
:::



### Plot all comparisons

::: {.cell}

```{.r .cell-code}
(StO2_CG_MG+StO2_CG_MTD+StO2_MG_MTD)/
(HbO2_CG_MG+HbO2_CG_MTD+HbO2_MG_MTD)/
(tHb_CG_MG+tHb_CG_MTD+tHb_MG_MTD)/
(HbO_CG_MG+HbO_CG_MTD+HbO_MG_MTD)
```

::: {.cell-output-display}
![](GAM_models_all_data_v1_2021_files/figure-html/plot-pairwise-comp-all-1.png){width=672}
:::
:::


