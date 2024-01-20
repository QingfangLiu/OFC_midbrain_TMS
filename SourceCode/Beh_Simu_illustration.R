
rm(list = ls())

#################### Prepare ######################

source('Setup.R')
usedir = dirname(getwd())
figDir = file.path(usedir,'Figs')
source('Func_Simu.R')

SubInfo = read.xlsx(file.path(usedir,'SourceData.xlsx'),'SubjectConds')

subs = subset(SubInfo,Excluded==0)$Sub 
nsubs = length(subs)
dat = read.xlsx(file.path(usedir,'SourceData.xlsx'),'OdorPredRes')

ncues = 2
nodors = 3
nSess = 2
nrun = 3

Add_processed_vars = function(x){
  LocRev = which(x$Rev==1)
  RevLoc = rep(NA,nrow(x))
  RevLoc[LocRev-1] = -1
  RevLoc[LocRev] = 0
  RevLoc[LocRev+1] = 1
  RevLoc[LocRev+2] = 2
  x$RevLoc = RevLoc
  return(x)
}


#######################################
## model simulation
theta = 5 # temperature parameter in softmax
subj=1
r=1
tms=1 # 1:sham; 2:stim
tmpdata = subset(dat,Sub == subj & TMS == tms & Run == r)

# simulation with high learning rate 0.9 & low 0.5
alphas = c(0.9,0.5)
# define color palette to use (not using the very bounary colors)
sc = scale_fill_scico(palette = "batlow", begin = 0.1, end = 0.9)
# to rescale errors to 0-1 for the ease of plotting scale
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

pweight = NULL
perror = NULL
for (a in 1:2){ 

out = FuncModelSimu(tmpdata,alphas[a],theta,nodors,ncues)
df = melt(out$allw[1,,tmpdata$Cue==1])
colnames(df) <- c("x", "y", "value")
df = subset(df, y < 22)
pweight[[a]] = ggplot(df, aes(x = x, y = y, fill = value)) +
  geom_tile(color = "black") + 
  coord_flip() + 
  theme_classic(base_size = 12) +
  labs(x='',y='') + sc +
  xlim(0,4) + ylim(0,21.5) +
  guides(fill = guide_colourbar(title = "")) + # don't show legend title
  theme_void() # don't show axes
  
dfe = cbind(1:32,apply(abs(out$delta[tmpdata$Cue==1,]),1,sum))
dfe = as.data.frame(dfe)

colnames(dfe) <- c("x", "value")
dfe = subset(dfe, x < 22)
dfe$value = range01(dfe$value) # scale errors to be 0-1
perror[[a]] = ggplot(dfe, aes(x = x, y=1, fill = value)) +
  geom_tile(color = "black")  + 
  theme_classic(base_size = 12) +
  xlim(0,21.5) + labs(x='',y='') + sc +
  theme_void()
}

tmplocs = c(5,6,7,8,
            11,12,13,14,
            15,16,17,18,
            20,21)
length_locs = length(tmplocs)

line_df <- data.frame(
  x = tmplocs,
  y = rep(0.5,length_locs),
  xend = tmplocs,
  yend = rep(0.3,length_locs)
)

blank = dfe
blank$value = NA
pblank = ggplot(blank, aes(x = x, y=1,fill = 'white')) +
  xlim(0,21.5) +
  ylim(0,1) +
  theme_classic(base_size = 12) + 
  theme_void() +
  geom_segment(data = line_df, 
             mapping = aes(x=x, y=y, xend=xend, yend=yend),size = 1)

pnull = ggplot(blank, aes(x = x, y=1,fill = 'white')) +
  xlim(0,21.5) +
  ylim(0,1) +
  theme_classic(base_size = 12) + 
  theme_void()

pdf(NULL)
p=ggarrange(pweight[[1]],perror[[1]],pblank,
            pnull,
            pweight[[2]],perror[[2]],pblank,
            pnull,
            common.legend = T,
            legend = 'none',
            ncol = 1, nrow = 8,
            heights = rep(c(3.8,0.9,0.6,0.8),2)) +
  theme(plot.margin=margin(0.1,0.1,0.1,1,"cm"),
        plot.title = element_text(size=10),
        legend.text=element_text(size=6))

pdf(file.path(figDir,'Fig3a.pdf'),10,6)
print(p)
dev.off()


