
# test fMRI univariate effect using functional ROI: lOFC

rm(list = ls())
source('Setup.R')
usedir = dirname(getwd())
figDir = file.path(usedir,'Figs')
SubInfo = read.xlsx(file.path(usedir,'SourceData.xlsx'),'SubjectConds')

UseTMSConds = SubInfo$Cond[SubInfo$Excluded==0]
Subs = SubInfo$Sub[SubInfo$Excluded==0]

## read source data
UseAggBOLD = read.xlsx(file.path(usedir,'SourceData.xlsx'),'SFig5d-1')
UseDiffAggBOLD = read.xlsx(file.path(usedir,'SourceData.xlsx'),'SFig5d-2')

################ summarize agg BOLD by TMS ###################

SummaryAggBOLD = UseAggBOLD %>%
  group_by(TMS, RevLoc) %>%
  dplyr::summarise(
    mean = mean(Beta),
    sd = sd(Beta),
    n = n(), 
    sde = sd/sqrt(n)
    )


p = NULL
p[[1]] = ggplot(data = SummaryAggBOLD,
       aes(x = RevLoc, y = mean, group = TMS)) + 
  geom_errorbar(aes(ymin=mean-sde, ymax=mean+sde,color=TMS), width=.2, size=1.1) +
  geom_line(aes(color=TMS),size=1.1)+
  geom_point(aes(color=TMS),show.legend=FALSE) +
  scale_colour_manual(values = c("sham" = UseColor[2],"cTBS" = UseColor[1])) +
  labs(x="", y = "", title = '') + 
  scale_x_discrete(labels= c("-1"=expression(rev[-1]), "0"="rev",
                         "1"=expression(rev[+1]),"2"=expression(rev[+2]))) +
  common + theme(legend.position = 'none', strip.text.x = element_blank())

high_y1 = 1.8
low_y1 = 1.6

dat_text <- data.frame(
  label = c("**"),
  ROI   = c('lOFC'),
  x     = c(1.5),
  y     = c(1.9)
)

p[[2]] = ggplot(data = UseDiffAggBOLD,
                mapping = aes(x = TMS, y = Beta)) + 
  geom_boxplot(aes(fill=TMS),alpha=0.5, outlier.shape = NA) +
  geom_line(aes(group=paired), position = position_dodge(0.2)) +
  geom_point(aes(fill=TMS,group=paired),size=2,shape=21,alpha=1,
             position = position_dodge(0.2)) +
  scale_fill_manual(values = c("sham" = UseColor[2],"cTBS" = UseColor[1])) +
  ylim(-1.8, 2) +
  labs(x="", y = "", title = '') + 
  common + 
  theme(strip.text.x = element_blank(),legend.position = "none" ) +
  geom_segment(aes(x=1,y=high_y1,xend=2,yend=high_y1),size = 0.2) +
  geom_segment(aes(x=1,y=low_y1,xend=1,yend=high_y1),size = 0.2) +
  geom_segment(aes(x=2,y=low_y1,xend=2,yend=high_y1),size = 0.2) +
  geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = label),size=4.5)

pdf(file.path(figDir,'SFig5d.pdf'),8,4)
fig = ggarrange(p[[1]], p[[2]], widths = c(1.3,1), align = "hv")
print(fig)
dev.off()

# t tests and save post-rev beta decrease
attach(UseDiffAggBOLD)
t.test(Beta[TMS=='cTBS'], Beta[TMS=='sham'],
       paired = T, alternative = 'g')
detach(UseDiffAggBOLD)



