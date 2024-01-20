
# This script conducts testing & plotting on ROI-based MVPA 

rm(list = ls())
source('Setup.R')
usedir = dirname(getwd())
figDir = file.path(usedir,'Figs')
SubInfo = read.xlsx(file.path(usedir,'SourceData.xlsx'),'SubjectConds')
Corr_diff_vals_df_summary = read.xlsx(file.path(usedir,'SourceData.xlsx'),'Fig6d')

##################################################################

UseTMSConds = SubInfo$Cond[SubInfo$Excluded==0]
Subs = SubInfo$Sub[SubInfo$Excluded==0]
nsubs = length(Subs)

################ plots ###################


p = ggplot(data = Corr_diff_vals_df_summary,
                mapping = aes(x = TMS, y = Representation, fill = TMS)) + 
  geom_boxplot(alpha=0.5, outlier.shape = NA) +
  geom_line(aes(group=paired), position = position_dodge(0.2)) +
  geom_point(aes(fill=TMS,group=paired),size=2,shape=21,alpha=1,
             position = position_dodge(0.2)) +
  scale_fill_manual(values = c("sham" = UseColor[2],"cTBS" = UseColor[1])) +
  labs(y = "", x = "", title = '') + 
  common +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.text.x = element_blank(),legend.position = "none" ) +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted")


high_y2 = 0.23
low_y2 = 0.22
p = p +
  geom_segment(aes(x=1,y=high_y2,xend=2,yend=high_y2),linewidth = 0.2) +
  geom_segment(aes(x=1,y=low_y2,xend=1,yend=high_y2),linewidth = 0.2) +
  geom_segment(aes(x=2,y=low_y2,xend=2,yend=high_y2),linewidth = 0.2) +
  annotate(geom = "text", x = c(1.5), y = c(0.235), label = '*', size = 6)

pdf(file.path(figDir,'Fig6d.pdf'),3.5,4)
par(mar=c(1,1,1,1))
print(p)
dev.off()


x_sham = subset(Corr_diff_vals_df_summary,TMS == 'sham')$Representation
x_cTBS = subset(Corr_diff_vals_df_summary,TMS == 'cTBS')$Representation
print(wilcox.test(x_sham,alternative = 'g')) # representation > 0 from sham trials?
print(wilcox.test(x_cTBS,alternative = 'g')) # representation > 0 from cTBS trials?
print(wilcox.test(x_sham,x_cTBS,paired = T,alternative = 'g'))

