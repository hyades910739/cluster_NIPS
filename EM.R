#### EM ####
## this code implement Clustering with Normal mixture model(EM)
## function EM is the wapper function of EM_main
## you can use EM function to cluster with random starter,
## or use EM_main with alpha,mean,sigma predefined.
library(mvtnorm)
library(magrittr)
###############
EM_main = function(data,alpha,mean,sigma,error=1e-6,n_exp=300){
  n = length(alpha)
  exp_time = 0
  while(TRUE){
    #E.stemp
    py.x = sapply(1:n,function(i){
      alpha[[i]]*dmvnorm(data,mean=mean[[i]],sigma=sigma[[i]])
    })
    py.x = py.x/rowSums(py.x)
    #set py.x<0.001 = 0.001
    for(i in 1:dim(py.x)[2]){
      sel = which(py.x[,i]<0.001)
      py.x[sel,i] = 0.001
    }
    #M.stemp
    n_alpha = apply(py.x,2,mean)
    n_mean = lapply(1:n,function(i){
      apply(data*py.x[,i]/sum(py.x[,i]),2,sum)
    })
    n_sigma = lapply(1:n,function(x){
      cov.wt(data,py.x[,x])$cov
    })
    #Control flow
    comparison = c(unlist(n_alpha) - unlist(alpha),
                   unlist(n_mean) - unlist(mean))
    diff = abs(comparison) > error 
    exp_time = exp_time+1
    alpha = n_alpha
    mean = n_mean
    sigma = n_sigma
    if(!any(diff)) break
    if(exp_time>n_exp) break
  }
  cluster = apply(py.x,1,which.max)
  names(cluster)=NULL
  res = list(
    cluster = cluster,
    #p = py.x,
    alpha = alpha,
    mean = mean,
    sigma = sigma,
    iter = exp_time
  )
  res$loglike = sapply(1:length(alpha),function(x){
    alpha[[x]]*dmvnorm(data,mean=mean[[x]],sigma=sigma[[x]])
  }) %>% rowSums() %>% log() %>% sum()
  return(res)
}

#####
EM  = function(data,cluster,random=10,error=1e-6,n_exp=300,seed=NULL){
  if(!is.null(seed) & is.numeric(seed) ) set.seed(seed)
  range = apply(data,2,range)
  n_sample = dim(data)[1]
  cluster = as.integer(cluster)
  ##
  res = list()
  ##
  for(i in 1:random){
    random_sel = sample(1:cluster,n_sample,T)
    alpha = rep(1/cluster,cluster)
    mean = lapply(1:cluster,function(x){
      apply(data[random_sel==x,],2,mean)
    })
    sigma = lapply(1:cluster,function(x){
      #cov(data[random_sel==x,])
      diag(1,dim(data)[2])
    })
    tryCatch({
      res[[i]] = EM_main(data,alpha,mean,sigma,error=1e-6,n_exp=300)
    },error = function(e){
      print(e)
      cat("Fail random start at ",i,"\n")
    })
  }
  # find which starter max log likelihood (Q)
  sel = which.max(sapply(1:random,function(x){res[[x]]$Q}))
  return(res[[sel]])
}
###############
# Example: set start by yourself
alpha = list(a=0.33,b=0.34,c=0.33)
mean = list(a=c(5.8,3.0,3.76,1.2),
            b=c(0.8,2.0,1.76,10.2),
            c=c(5.8,30.0,3.76,11.2))
sigma = list(a=diag(1,4),
             b=diag(1,4),
             c=diag(1,4))

res = EM_main(iris[,-5],alpha,mean,sigma)

# Set random point to start:
res = EM(iris[,-5],3,100,seed = 689)

#10  : -194.9202
#20  : -194.9202
#50  : -186.4888
#100 : -186.4888
#200 : 
#plot result:
plot(iris[,1],iris[,3],type='n')
text(iris[,1],iris[,3],iris[,5],col=res$cluster)
