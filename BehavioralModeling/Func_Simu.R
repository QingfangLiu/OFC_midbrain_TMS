
# function for model simulation

FuncModelSimu = function(tmpdata,alpha,theta,nodors,ncues){
  # func works on a single run's data: tmpdata
  # alpha: learning rate (0-1)
  # theta: temperature in softmax (>0)
  # nodors: # of odors
  # ncues: # of cues
  
  ntrials = nrow(tmpdata)
  Cue = tmpdata$Cue
  Odor = tmpdata$Odor
  delta = matrix(NA,ntrials,nodors) # to save error across trials
  prob = matrix(NA,ntrials,nodors) # prob of choosing each odor across trials
  allw = array(NA,c(ncues,nodors,ntrials)) # track all weights across trials
  w = matrix(1/3,ncues,nodors) 

  for(i in 1:ntrials){
    allw[,,i] = w # record weights before updated from this trial
    prob[i,] = exp(w[Cue[i],]*theta)/sum(exp(w[Cue[i],]*theta)) 
    real = numeric(nodors)
    real[Odor[i]] = 1 # real: vector indicating which odor is present
    delta[i,] = real - w[Cue[i],] # error vector
    w[Cue[i],] = w[Cue[i],] + alpha * delta[i,] 
  }
  
  PredMat = apply(prob,1,rmultinom,n=1,size=1)
  Pred = sapply(1:ncol(PredMat),function(x) which(PredMat[,x]==1))
  Correct = as.numeric(Pred==Odor)
  ProbCorr = sapply(1:nrow(prob),function(x) prob[x,Odor[x]])
  out = list('allw'=allw,
             'prob'=prob,
             'Pred'=Pred,
             'Correct'=Correct,
             'delta'=delta,
             'ProbCorr'=ProbCorr)
  return(out)
}


