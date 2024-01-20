
model.file = paste0('LearningModel_TMS_',modelname,'.txt')

if(modelname == 'same'){
  mu_start = 0.5
  alpha_start = rep(0.5,nsubs)
  theta_start = 1
}

if(modelname == 'diff'){
  mu_start = rep(0.5,nconds)
  alpha_start = matrix(0.5,nrow = nsubs,ncol = nconds)
  theta_start = 1
}

myinits <- rep(list(list(mu = mu_start, 
                         alpha = alpha_start,
                         theta = theta_start)),n.chains)
parameters <- c("mu","alpha","theta")