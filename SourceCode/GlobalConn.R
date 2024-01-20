
rm(list = ls())
source('Setup.R')
usedir = dirname(getwd())
figDir = file.path(usedir,'Figs')
SubInfo = read.xlsx(file.path(usedir,'SourceData.xlsx'),'SubjectConds')

# load global conn data frame
dfGC = read.xlsx(file.path(usedir,'SourceData.xlsx'),'Fig2cSFig2bSFig3')

##################################################################

nSess = 2
nruns = 3
ntimes = nruns * 2

UseTMSConds = SubInfo$Cond[SubInfo$Excluded==0]
Subs = SubInfo$Sub[SubInfo$Excluded==0]
nSubs = length(Subs)

dfGC = dfGC %>%
  mutate(ROI = factor(ROI),
         Sub = factor(Sub),
         Sess = factor(Sess),
         TMS = factor(TMS))

## plotting

ROIs = c('OFC','LPFC','func_OFC','func_LPFC')
p = NULL
for(i in 1:2){
  tmp_dfGC = subset(dfGC, ROI == ROIs[i]) 
  avg_df = tmp_dfGC %>%
           group_by(Time, TMS) %>% 
           dplyr::summarise(mean = mean(GC), sd = sd(GC),
                   n = n(), sde = sd/sqrt(n))
  
p[[i]] = ggplot(avg_df,aes(Time)) + # notice here 'Time' can't be a factor
  geom_point(aes(y = mean, color = TMS), size = 3) +
  geom_line(aes(y = mean, color = TMS), linewidth = 1.5) +
  geom_ribbon(aes(ymin = mean - sde, ymax = mean + sde, fill = TMS), alpha = 0.3) +
  scale_color_manual(values = c("sham" = UseColor[2],"cTBS" = UseColor[1])) +
  scale_fill_manual(values = c("sham" = UseColor[2],"cTBS" = UseColor[1])) +
  common +
  labs(x ="", y = "") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  geom_vline(xintercept=c(2.5,4.5), linetype="dotted", color = "black", size=0.5)

}

p[[1]] = p[[1]] +
  labs(title = 'Targeted OFC seed') +
  coord_cartesian(ylim = c(0.095, 0.115), xlim = c(0.6,6.4),
                  expand = FALSE, clip = "off") +
  annotate(geom = "text", x = c(0.9,1.1,1.9,2.1), y = c(0.103,0.103,0.104,0.104), 
           label = '*', size = 6)

p[[2]] = p[[2]] +
  labs(title = 'LPFC stimulation site') +
  coord_cartesian(ylim = c(0.109, 0.128), xlim = c(0.6,6.4),
                  expand = FALSE, clip = "off") +
  annotate(geom = "text", x = c(1.9,2.1,3,4), y = c(0.117,0.117,0.119,0.12), 
           label = '*', size = 6)


pdf(file.path(figDir,'Fig2c.pdf'),5,8)
fig = ggarrange(p[[1]], p[[2]], ncol = 1, nrow = 2)
print(fig)
dev.off()

###### stat testing on OFC & LPFC ROIs

for(r in c('OFC','LPFC')){
  for(t in c('sham','cTBS')){ # test linear trend for each TMS condition
    print(t)
    print('Simple linear regression')
    model <- lm(GC ~ Time, data = subset(dfGC, ROI == r & TMS == t)) 
    print(summary(model))
    print('linear mixed with time')
    lme1 = lmer(GC ~ Time + Sub + (1 | Sub), data = subset(dfGC, ROI == r & TMS == t))
    print(summary(lme1))
    print('linear mixed without time')
    lme2 = lmer(GC ~ Sub + (1 | Sub), data = subset(dfGC, ROI == r & TMS == t))
    print(summary(lme2))
    print('Compare LME models')
    print(anova(lme1,lme2))
  }
  
  # test the difference of TMS conditions
  model1 <- lmer(GC ~ Time * TMS + Sub + (1 | Sub), data = subset(dfGC, ROI == r))
  model2 <- lmer(GC ~ Time + TMS + Sub + (1 | Sub), data = subset(dfGC, ROI == r))
  summary(model)
  anova(model1,model2)
  
}

# test TMS effect at each time point for each ROI

for(r in c('OFC','LPFC')){
for(t in 1:ntimes){
  print(paste('Time',t,'for',r))
  test_data = subset(dfGC, ROI == r & Time == t)
  test_data = test_data[order(test_data$Sub),] # make sure the order is correct!
  
  test_sham = subset(test_data,TMS == 'sham')$GC
  test_cTBS = subset(test_data,TMS == 'cTBS')$GC
  print(wilcox.test(test_cTBS,test_sham,paired = T,alternative = 'l'))
  
  lme1 = lmer(GC ~ TMS + (1 | Sub),data = test_data)
  lme2 = lmer(GC ~ Sess + (1 | Sub),data = test_data)
  lme3 = lmer(GC ~ TMS + Sess + (1 | Sub),data = test_data)
  anova(lme1,lme2)
  anova(lme1,lme3) # Sess is useful?
  anova(lme2,lme3) # TMS is useful?
              
}
}

# conclusion here from LME model comparison
# for OFC
# run1,2: effect of TMS after accounting for Sess
# run3,4: effect of Sess after accounting for TMS
# run5,6: none

# for LPFC
# run1,5: none
# run2: effect of TMS after accounting for Sess
# run3,4: effect of both TMS and Sess
# run6: effect of Sess after accounting for TMS

##################################################################
# look at two functional ROIs
##################################################################

for(i in 3:4){
  for(t in 1:ntimes){
    print(paste('Time',t,'for',ROIs[i]))
    test_data = subset(dfGC, ROI == ROIs[i] & Time == t)
    test_data = test_data[order(test_data$Sub),] # make sure the order is correct!
    
    test_sham = subset(test_data,TMS == 'sham')$GC
    test_cTBS = subset(test_data,TMS == 'cTBS')$GC
    print(wilcox.test(test_cTBS,test_sham,paired = T,alternative = 'l'))
  }}

##################################################################
### plot to look at difference value distributions

bplt = NULL
for(i in 3:4){
  tmp_dfGC = subset(dfGC, ROI == ROIs[i]) 
  tmp_dfGC_cTBS = tmp_dfGC %>%
                  subset(TMS == 'cTBS') %>%
                  arrange(Sub,Time)
  tmp_dfGC_sham = tmp_dfGC %>%
                  subset(TMS == 'sham') %>%
                  arrange(Sub,Time)
  GC_diff = tmp_dfGC_cTBS$GC - tmp_dfGC_sham$GC
  tmp_dfGC_diff = cbind(tmp_dfGC_cTBS,GC_diff)
  tmp_dfGC_diff$GC = NULL
  avg_df = tmp_dfGC_diff %>%
    group_by(Time) %>% 
    dplyr::summarise(mean = mean(GC_diff), sd = sd(GC_diff),
                     n = n(), sde = sd/sqrt(n))
  bplt[[i]] = ggplot(tmp_dfGC_diff,aes(x=factor(Time),y=GC_diff)) + # notice here 'Time' can't be a factor
    geom_line(aes(y = GC_diff, group = Sub), 
              linewidth = 0.3, linetype = 'solid',
              show.legend = F, color = 'gray') +
    geom_point(data = avg_df,aes(y = mean), 
               size = 3,color = 'black') +
    geom_line(data = avg_df,aes(x = Time,y = mean), linewidth = 1.5,
              show.legend = F,color = 'black') +
    geom_ribbon(data = avg_df,aes(x = Time, y = mean, ymin = mean - sde, 
                                  ymax = mean + sde), alpha = 0.3) +
    common +
    labs(x ="", y = "") +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.ticks.x=element_blank(),
          axis.text.x = element_blank()) +
    geom_vline(xintercept=c(2.5,4.5), linetype="dotted", color = "black", size=0.5) +
    geom_hline(yintercept = 0, linetype="dashed", color = "black", size=1)
  
}

bplt[[3]] = bplt[[3]] +
  labs(title = 'OFC (functional ROI)') +
  coord_cartesian(ylim = c(-0.085, 0.06), xlim = c(0.6,6.4),
                  expand = FALSE, clip = "off")
bplt[[4]] = bplt[[4]] +
  labs(title = 'LPFC (functional ROI)') +
  coord_cartesian(ylim = c(-0.14, 0.07), xlim = c(0.6,6.4),
                  expand = FALSE, clip = "off")
fig = ggarrange(bplt[[3]], bplt[[4]], ncol = 1, nrow = 2)
pdf(file.path(figDir,'SFig2b.pdf'),5,8)
print(fig)
dev.off()


##################################################################
# look at other ROIs
# insula, midbrain, mPFC, striatum, thalamus, amygdala
##################################################################

otherROIs = c('Insula_b','MB_b','mPFC','Striatum_b','Thalamus','AMG_b')
otherlabels = c('Insula','Midbrain','mPFC','Striatum','Thalamus','Amygdala')

# run two-way repeated ANOVAs with TMS and time as factors for each ROI
for(r in otherROIs){
  print(r)
  test_data = subset(dfGC, ROI == r)
  res.aov <- anova_test(
    data = test_data, dv = GC, wid = Sub,
    within = c(TMS, Time)
  )
  print(get_anova_table(res.aov))
}
  

# plot: Supp fig 3
ctr = 0
for(r in otherROIs){
  ctr = ctr + 1
  tmp_dfGC = subset(dfGC, ROI == r) 
  avg_df = tmp_dfGC %>%
    group_by(Time, TMS) %>% 
    dplyr::summarise(mean = mean(GC), sd = sd(GC),
                     n = n(), sde = sd/sqrt(n))
  
  p[[ctr]] = ggplot(avg_df,aes(Time)) + # notice here 'Time' can't be a factor
    geom_point(aes(y = mean, color = TMS), size = 3) +
    geom_line(aes(y = mean, color = TMS), linewidth = 1.5) +
    geom_ribbon(aes(ymin = mean - sde, ymax = mean + sde, fill = TMS), alpha = 0.3) +
    scale_color_manual(values = c("sham" = UseColor[2],"cTBS" = UseColor[1])) +
    scale_fill_manual(values = c("sham" = UseColor[2],"cTBS" = UseColor[1])) +
    common +
    labs(x ="", y = "", title = otherlabels[ctr]) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.ticks.x=element_blank(),
          axis.text.x = element_blank()) +
    geom_vline(xintercept=c(2.5,4.5), linetype="dotted", color = "black", size=0.5)
}

pdf(file.path(figDir,'SFig3.pdf'),15,8)
fig = ggarrange(p[[1]],p[[2]],p[[3]],
                p[[4]],p[[5]],p[[6]],
                ncol = 3, nrow = 2, align = "hv")
print(fig)
dev.off()


