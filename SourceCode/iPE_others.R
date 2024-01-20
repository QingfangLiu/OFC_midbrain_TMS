
# test fMRI univariate effect using other ROIs

rm(list = ls())
source('Setup.R')
usedir = dirname(getwd())
figDir = file.path(usedir,'Figs')
SubInfo = read.xlsx(file.path(usedir,'SourceData.xlsx'),'SubjectConds')

## read source data
UseAggBOLD = read.xlsx(file.path(usedir,'SourceData.xlsx'),'SFig6a')
UseDiffAggBOLD = read.xlsx(file.path(usedir,'SourceData.xlsx'),'SFig6b')

##################################################################

ROIs = c('Insula','mPFC','Striatum','Thalamus','Amygdala')
nROIs = length(ROIs)

UseTMSConds = SubInfo$Cond[SubInfo$Excluded==0]
Subs = SubInfo$Sub[SubInfo$Excluded==0]

################ summarize agg BOLD by TMS ###################

SummaryAggBOLD = UseAggBOLD %>%
  group_by(TMS, RevLoc, ROI) %>%
  dplyr::summarise(
    mean = mean(Beta),
    sd = sd(Beta),
    n = n(), 
    sde = sd/sqrt(n)
    )

################ plots ###################

rctr = 0
p = NULL
for(roi in ROIs){
p[[2*rctr+1]] = ggplot(data = subset(SummaryAggBOLD,ROI == roi),
       aes(x = RevLoc, y = mean, group = TMS)) + 
  geom_errorbar(aes(ymin=mean-sde, ymax=mean+sde,color=TMS), width=.2, size=1.1) +
  geom_line(aes(color=TMS),size=1.1)+
  geom_point(aes(color=TMS),show.legend=FALSE) +
  scale_colour_manual(values = c("sham" = UseColor[2],"cTBS" = UseColor[1])) +
  labs(x="", y = "", title = "") + 
  scale_x_discrete(labels=c("-1"=expression(rev[-1]), "0"="rev",
                         "1"=expression(rev[+1]),"2"=expression(rev[+2]))) + common +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none", strip.text.x = element_blank())
p[[2*rctr+2]] = ggplot(data = subset(UseDiffAggBOLD,ROI == roi),
                mapping = aes(x = TMS, y = Beta, fill = TMS)) + 
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  geom_line(aes(group=paired), position = position_dodge(0.2)) +
  geom_point(aes(fill=TMS,group=paired),size=2,shape=21,alpha=1,
             position = position_dodge(0.2)) +
  scale_fill_manual(values = c("sham" = UseColor[2],"cTBS" = UseColor[1])) +
  labs(x="", y="", title = '') + 
  common +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.text.x = element_blank(),legend.position = "none" )
rctr = rctr + 1
}


# range of y limits
ylim_low = c(-2,-3,-3,-3,-2)
ylim_high = c(0.9,1.4,2.2,1.4,2.2)

# line segments
high_y_line = c(0.8,0.9,2.1,0.9,2.1)
low_y_line = c(0.7,0.8,2,0.8,2)
  
pdf(file.path(figDir,'SFig6.pdf'),14,6)
fig = ggarrange(p[[1]], p[[3]], p[[5]], p[[7]], p[[9]], 
                p[[2]], p[[4]], p[[6]], p[[8]], p[[10]], 
                ncol = 5, nrow = 2, align = "hv")
print(fig)
dev.off()

################ stat tests ###################
# t tests and save (rev+1) - rev

for(i in 1:nROIs){
  print(ROIs[i])
  dat_cTBS = subset(UseDiffAggBOLD,ROI==ROIs[i] & TMS =='cTBS')$Beta
  dat_sham = subset(UseDiffAggBOLD,ROI==ROIs[i] & TMS =='sham')$Beta
  print(t.test(dat_cTBS, dat_sham, paired = T, alternative = 'g'))
}

##################################################################
## post-hoc analysis on rev and rev+1 locations separately
# all ns

for(i in 1:nROIs){
  print(ROIs[i])
  
# test on rev trials
beta_rev_cTBS = subset(UseAggBOLD,RevLoc == 0 & ROI == ROIs[i] & TMS == 'cTBS')$Beta
beta_rev_sham = subset(UseAggBOLD,RevLoc == 0 & ROI == ROIs[i] & TMS == 'sham')$Beta
print(t.test(beta_rev_cTBS,beta_rev_sham,paired = T,alternative = 'l'))

# test on rev+1 trials
beta_rev1_cTBS = subset(UseAggBOLD,RevLoc == 1 & ROI == ROIs[i] & TMS == 'cTBS')$Beta
beta_rev1_sham = subset(UseAggBOLD,RevLoc == 1 & ROI == ROIs[i] & TMS == 'sham')$Beta
print(t.test(beta_rev1_cTBS,beta_rev1_sham,paired = T,alternative = 'g'))

}


