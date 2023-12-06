
# to analyze posterior samples from the 'diff' model
# and generate posterior predictions

rm(list = ls())
library(openxlsx)
library(plyr)
library(tidyverse)

# modeling dir
behmodelDir = '~/Desktop/BehavioralModeling'

# load function for simulating behaviors
source('Func_Simu.R')

modelname = 'diff'
modeldir = file.path(behmodelDir,'JagsPosteriors',modelname)
# load posterior samples
samples = readRDS(file.path(modeldir,'samples.rds'))
  
# posterior values
alpha <- samples$BUGSoutput$sims.list$alpha
theta <- samples$BUGSoutput$sims.list$theta
mu <- samples$BUGSoutput$sims.list$mu

nsamples = nrow(mu)
x = as.numeric(mu)
group = c(rep("sham", nsamples), rep("cTBS", nsamples))
df_mu <- data.frame(x, group)
write.xlsx(df_mu, file.path(modeldir,'df_mu.xlsx'))

# MAP estimates
alpha_map = apply(alpha,c(2,3),mean) # 31 * 2
theta_map = mean(theta) # scalar
mu_map = apply(mu,2,mean)

alpha_map_df = data.frame(alpha_map)
names(alpha_map_df) = c('sham','cTBS')
write.xlsx(alpha_map_df, file.path(modeldir,'alpha_map.xlsx'))

# load dat before any trials removed
dat = read.xlsx(file.path(usedir,'SourceData.xlsx'),'OdorPredRes')
SubInfo = read.xlsx(file.path(usedir,'SourceData.xlsx'),'SubjectConds')

# exclude 4 subs: only use 31 subjects' data
subs = subset(SubInfo,Excluded==0)$Sub 
nsubs = length(subs)
dat = subset(dat,Sub %in% subs)

ncues = 2
nodors = 3

# function to add RevLoc variable
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

######################################
######### simulate the learning model with MAPs
######################################

nsim = 1e3
nconds = 2
AggSimu = array(NA,c(nsim,nsubs,nconds,4))

for(i in 1:nsim){
  print(paste('Simulation iteration',i))
for(j in 1:nsubs){
  for(c in 1:nconds){ # 1:sham; 2:stim
    tmpdata = subset(dat,Sub == subs[j] & TMS == c & Run == 1)
    out = FuncModelSimu(tmpdata,alpha_map[j,c],theta_map,nodors,ncues)
    simudata = cbind(tmpdata,out$Correct)
    names(simudata) = c(names(tmpdata),'SimuCorr')
    simudata_processed = simudata %>% 
        group_by(Cue) %>%
        do(Add_processed_vars(.)) %>% 
        ungroup() %>% 
        arrange(Trial) %>% 
        filter(complete.cases(RevLoc))  %>%
        group_by(RevLoc) %>%
        summarize(SimuAcc = mean(SimuCorr)) %>% 
        ungroup()
    AggSimu[i,j,c,] = simudata_processed$SimuAcc
  }
  }
}

AggSimudf = structure(AggSimu,.Dim = dim(AggSimu), 
                      .Dimnames = structure(list(nsim = 1:nsim,
                                                 Sub = unique(subs), 
                                                 TMS = c('sham','cTBS'), 
                                                 RevLoc = -1:2), 
                                            .Names = c("nsim", "Sub", "TMS","RevLoc")))
AggSimudf = adply(AggSimudf, c(1,2,3,4))
names(AggSimudf) = c(names(AggSimudf)[1:4],'Acc')

write.xlsx(AggSimudf, file.path(modeldir,'SimuDat_MAP.xlsx'))

