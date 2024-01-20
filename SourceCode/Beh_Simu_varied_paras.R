
rm(list = ls())

#################### Prepare ######################

source('Setup.R')
usedir = dirname(getwd())
figDir = file.path(usedir,'Figs')
source('Func_Simu.R')

my_colors <- brewer.pal(9, "GnBu")[4:8]
xlab = c("-1"=expression(rev[-1]), 
         "0"="rev",
         "1"=expression(rev[+1]),
         "2"=expression(rev[+2]))

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

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
tmpdata = subset(dat,Sub == subj & TMS == tms & Run==r)

alphas = seq(0.5,0.9,length=5)
nalphas = length(alphas)
AllAggSimuProbCorr = AllAggSimuError = matrix(NA,length(alphas),4)

for(i in 1:length(alphas)){
  out = FuncModelSimu(tmpdata,alphas[i],theta,nodors,ncues)
  simudata = cbind(tmpdata,
                   apply(abs(out$delta),1,sum),
                   out$ProbCorr)
  names(simudata) = c(names(tmpdata),'SimuError','SimuProbCorr')
  AggSimu = simudata %>% 
    group_by(Cue) %>%
    do(Add_processed_vars(.)) %>% 
    ungroup() %>% 
    arrange(Trial) %>%
    filter(complete.cases(RevLoc)) %>% 
    group_by(RevLoc) %>%
    summarize(SimuProbCorr = mean(SimuProbCorr),
              SimuError = mean(SimuError)) %>% 
    ungroup()
  
  AllAggSimuProbCorr[i,] = AggSimu$SimuProbCorr
  AllAggSimuError[i,] = AggSimu$SimuError
  
}

Simudf = as.data.frame(cbind(rep(alphas,4),
                             as.numeric(AllAggSimuProbCorr),
                             as.numeric(AllAggSimuError),
                          c(rep(-1,nalphas),rep(0,nalphas),
                            rep(1,nalphas),rep(2,nalphas))))
names(Simudf) = c('alpha','ProbCorr','Error','Trials')

Simudf = Simudf %>%
  #mutate(Error = range01(Error)) %>% # to rescale errors to 0-1 
  mutate(alpha = factor(alpha))
  
# simulated prob of corr (more stable)
p1 = ggplot(Simudf, aes(x = Trials, y = ProbCorr, group = alpha, color=alpha))+
  geom_line(show.legend = F) +
  geom_point(size = 2.5) + 
  scale_x_continuous(labels = xlab) +
  scale_color_manual(values = my_colors) +
  labs(x ="", y = "") + 
  common +
  theme(legend.title = element_text(hjust = 0.5)) +
  labs(color = expression(alpha)) 

# simulate iPE using abs sum of errors
p2 = ggplot(Simudf, aes(x = Trials, y = Error, group = alpha, color=alpha))+
  geom_line() +
  geom_point(size = 2.5) + 
  scale_x_continuous(labels = xlab) +
  scale_color_manual(values = my_colors) +
  common +
  labs(x ="", y = "") + 
  common


# simulate the change of neural simi with learning rates
mat = NULL
diff = matrix(NA,nalphas,2)
for(i in 1:nalphas){
  out = FuncModelSimu(tmpdata,alphas[i],theta,nodors,ncues)
  xx = out$allw[1,,tmpdata$Cue==1] # only using cue 1
  rev_loc = which(subset(tmpdata,Cue==1)$Rev==1)
  
  ctr = 0
  cormat = array(NA,c(4,4,6))
  for(rev in rev_loc){
    ctr = ctr + 1
    tmp = xx[,c((rev-1),rev,(rev+1),(rev+2))]
    cormat[,,ctr] = cor(tmp)
  }
  
  mat[[i]] = apply(cormat, c(1,2), mean)
  diff[i,1] = mat[[i]][1,2]
  diff[i,2] = mat[[i]][2,3]
}

SimuCorr = data.frame('alpha' = rep(alphas,2),
                      'Simi' = as.numeric(diff),
                      'type' = c(rep('pre-rev',nalphas),
                                 rep('post-rev',nalphas)))
SimuCorr = SimuCorr %>%
  mutate(alpha = factor(alpha))

lab1 = expression(S[same])
lab2 = expression(S[different])
use.group = rep(1:5,2)
shape_labels <- c(expression(S[different]),expression(S[same]))

p4 = ggplot(SimuCorr, aes(x = alpha, y = Simi, group = type))+
  geom_point(aes(color=alpha,shape = type),show.legend = F, size = 3) +
  scale_shape_manual(values = c(15, 16), labels = shape_labels) +
  geom_line(aes(group=use.group,color=alpha),show.legend = F) + # vertical lines
  scale_color_manual(values = my_colors) +
  labs(x="",y="") +
  common +
  theme(legend.title = element_text(hjust = 0.5)) +
  theme(legend.title=element_blank()) +
  theme(legend.text.align = 0, legend.position = "top") +
  guides(color = "none") +
  labs(color = expression(alpha)) 

pdf(file.path(figDir,'Fig6b.pdf'),3.5,4)
print(p4)
dev.off()

res12 = ggarrange(p1,p2,common.legend = T,legend = 'right',
                  ncol = 2, nrow = 1)
pdf(NULL)
pdf(file.path(figDir,'Fig3bc.pdf'),10,4)
print(res12)
dev.off()

