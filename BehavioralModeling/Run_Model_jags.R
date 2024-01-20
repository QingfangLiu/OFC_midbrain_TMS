
rm(list=ls()) 
library(openxlsx)
library(R2jags)
library(plyr)

# source data 
sourceDir = '~/Desktop/SourceData'

# modeling dir
behmodelDir = '~/Desktop/BehavioralModeling'

dat = read.xlsx(file.path(sourceDir,'OdorPredRes.xlsx'))
datinfo = read.xlsx(file.path(sourceDir,'SubjectConds.xlsx'))

# exclude 4 subs: only use 31 subjects' data
subs = subset(datinfo,Excluded==0)$Sub 
nsubs = length(subs)
dat = subset(dat,Sub %in% subs)

# only using data from the first run
dat = subset(dat,Run == 1)

# prepare model inputs
sub = dat$Sub 
sub = mapvalues(sub,from = unique(sub),to = 1:length(unique(sub))) # recode sub into consective numbers
TMS = dat$TMS
Run = dat$Run
nconds = length(unique(TMS))
nsubs = length(unique(sub))
nruns = length(unique(Run))
ntrials = length(unique(dat$Trial))
Cue = Odor = Resp = array(NA,c(nsubs,nconds,nruns,ntrials))
for(j in 1:nsubs){
  for(c in 1:nconds){
    for(r in 1:nruns){
        Cue[j,c,r,] = dat$Cue[sub==j & TMS==c & Run==r]
        Odor[j,c,r,] = dat$Odor[sub==j & TMS==c & Run==r]
        Resp[j,c,r,] = dat$Resp[sub==j & TMS==c & Run==r]
    }
  }
}

w = array(NA,c(2,3,nsubs,nconds,nruns,(ntrials+1)))
w[,,,,,1] = 1/3
prob = array(NA,c(nsubs,nconds,nruns,ntrials,3))
real = array(NA,c(nsubs,nconds,nruns,ntrials,3))
dlist <- list("nsubs","nconds","nruns","ntrials",
              "Cue","Odor","Resp","w","prob","real")

################ set up MCMC ##############

n.chains = 3
n.iter = 5e3
n.burnin = 2e3
n.thin = 1

# two models: 'same', 'diff'
modelname = 'same' 
source('Specify_model_details.R')

# run jags model (this may take a couple minutes)
samples <- jags(data = dlist,
                inits = myinits,
                parameters.to.save = parameters,
                model.file = model.file,
                n.chains = n.chains, 
                n.iter = n.iter,
                n.burnin = n.burnin, 
                n.thin = n.thin)

# save all samples as rds file
saveRDS(samples,file = paste0('./JagsPosteriors/',modelname,'.rds'))


