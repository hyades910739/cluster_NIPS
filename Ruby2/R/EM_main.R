#' @import dmvnorm
EM_main <-
function(data,alpha,mean,sigma,error=1e-6,n_exp=100){
  n = length(alpha)
  exp_time = 0
  while(TRUE){
    #E.stemp
    py.x = sapply(1:n,function(i){
      alpha[[i]]*mvtnorm::dmvnorm(data,mean=mean[[i]],sigma=sigma[[i]])
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
    alpha[[x]]*mvtnorm::dmvnorm(data,mean=mean[[x]],sigma=sigma[[x]])
  }) %>% rowSums() %>% log() %>% sum()
  return(res)
}
