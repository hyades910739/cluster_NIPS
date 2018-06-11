EM <-
function(data,cluster,random=10,error=1e-6,n_exp=100,seed=NULL){
  if(any(!sapply(data,is.numeric))) stop("data must be all numeric,factor and character is not accepted.")
  if(random<1){
    warning("random is lower than 1, set random = 10") 
    random =10
  }
  if(n_exp<1){
    warning("n_exp is lower than 1, set n_exp = 100") 
    n_exp =100
  } 
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
  sel = which.max(sapply(1:random,function(x){res[[x]]$loglike}))
  ret = res[[sel]]
  class(ret)="EM"
  return(ret)
}
