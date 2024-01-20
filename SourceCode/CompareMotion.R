
rm(list = ls())
source('Setup.R')
usedir = dirname(getwd())
figDir = file.path(usedir,'Figs')

df_motions = read.xlsx(file.path(usedir,'SourceData.xlsx'),'SFig4')

# load similarly reorganized data from global connectedness
GC_SubsTMS = read.xlsx(file.path(usedir,'SourceData.xlsx'),'GC_SubsTMS')

##################################################################

nSess = 2
nruns = 3
ntimes = nruns * 2

Para.names = c('translation','rotation','mm')
nParas = length(Para.names)

##################################################################
# summarize across subjects

p = NULL
for(i in 1:nParas){
Avgdf = df_motions %>% 
  mutate(TMS=factor(TMS),Time=factor(Time)) %>% 
  subset(Paras == Para.names[i]) %>% 
  group_by(TMS,Time) %>%
  dplyr::summarise(mean = mean(value), sd = sd(value),
                   n = n(), sde = sd/sqrt(n)) %>%
  ungroup() 
  

p[[i]] = ggplot(Avgdf,aes(unfactor(Time))) + # notice here 'Time' can't be a factor
  geom_point(aes(y = mean, color = TMS), size = 3) +
  geom_line(aes(y = mean, color = TMS), linewidth = 1.5, show.legend = F) +
  geom_ribbon(aes(ymin = mean - sde, ymax = mean + sde, fill = TMS), alpha = 0.3) +
  scale_colour_manual(values = c("sham" = UseColor[2],"cTBS" = UseColor[1])) +
  scale_fill_manual(values = c("sham" = UseColor[2],"cTBS" = UseColor[1])) +
  labs(x="", y = "", title = '') +
  common + 
  geom_vline(xintercept=c(2.5,4.5), linetype="dotted", 
             color = "black", linewidth=0.5) +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.x=element_blank(),
        axis.text.x = element_blank(),
        strip.background = element_blank())
}

fig = ggarrange(p[[1]], p[[2]], p[[3]],
                        common.legend = T,
                        legend = 'none',
                        ncol = 3, nrow = 1, align = "hv")

pdf(NULL)
pdf(file.path(figDir,'SFig4.pdf'), 13.5, 4)
print(fig)
dev.off()

# perform a 2-way repeated-measure ANOVA
for(i in 1:nParas){
  print(Para.names[i])
  use.df = subset(df_motions,Paras == Para.names[i])
  anova_result = aov(value ~ TMS * Time + Error(Subs/(TMS * Time)), data = use.df)
  print(summary(anova_result))
}


##################################################################
# re-organize df to look at covariation of motion parameters 
# with global connectedness across subjects
##################################################################

vecs = matrix(NA,372,nParas)
for(i in 1:nParas){
reduced = df_motions %>%
  subset(Paras == Para.names[i]) %>%
  select(Subs,TMS,Time,value) %>%
  arrange(Subs,TMS,Time) 
  vecs[,i] = reduced$value
}

re_df_motion = data.frame(vecs)
names(re_df_motion) = Para.names
re_df_motion$Subs = reduced$Subs
re_df_motion$TMS = reduced$TMS
re_df_motion$Time = reduced$Time


##################################################################

# check the rows are aligned correctly !!!
# combine motion & GC results
df = cbind(GC_SubsTMS,re_df_motion[,Para.names])
df = df %>% mutate(TMS = factor(TMS), Sub = factor(Sub))

# for OFC result
# testing time bins 1, 2 separately
use.df = df %>% subset(Time == 2) 

# lmer1: baseline model with Sub and motion parameters
# lmer2: adding TMS to lmer1
lmer1_OFC <- lmer(OFC ~ Sub + translation + rotation + (1 | Sub), 
              data = use.df, control = lmerControl(optimizer ="Nelder_Mead"))
lmer2_OFC <- lmer(OFC ~ Sub + TMS + translation + rotation + (1 | Sub), 
              data = use.df, control = lmerControl(optimizer ="Nelder_Mead"))
anova(lmer1_OFC,lmer2_OFC) # is TMS useful after accounting for motions?

# repeat model comparison but using FD motion parameter
lmer1_OFC <- lmer(OFC ~ Sub + mm + (1 | Sub), 
                  data = use.df, control = lmerControl(optimizer ="Nelder_Mead"))
lmer2_OFC <- lmer(OFC ~ Sub + TMS + mm + (1 | Sub), 
                  data = use.df, control = lmerControl(optimizer ="Nelder_Mead"))
anova(lmer1_OFC,lmer2_OFC) # is TMS useful after accounting for motions?

# for LPFC result
# testing time bins 2, 3, 4 separately
use.df = df %>% subset(Time %in% c(4))

# lmer1: baseline model with Sub and motion parameters
# lmer2: adding TMS to lmer1
lmer1_LPFC <- lmer(LPFC ~ Sub + translation + rotation + mm + (1 | Sub), 
              data = use.df, control = lmerControl(optimizer ="Nelder_Mead"))
lmer2_LPFC <- lmer(LPFC ~ Sub + TMS + translation + rotation + mm + (1 | Sub), 
              data = use.df, control = lmerControl(optimizer ="Nelder_Mead"))
anova(lmer1_LPFC,lmer2_LPFC) # is TMS useful after accounting for motions?

# repeat model comparison but using FD motion parameter
lmer1_LPFC <- lmer(LPFC ~ Sub + mm + (1 | Sub), 
                   data = use.df, control = lmerControl(optimizer ="Nelder_Mead"))
lmer2_LPFC <- lmer(LPFC ~ Sub + TMS + mm + (1 | Sub), 
                   data = use.df, control = lmerControl(optimizer ="Nelder_Mead"))
anova(lmer1_LPFC,lmer2_LPFC) # is TMS useful after accounting for motions?


