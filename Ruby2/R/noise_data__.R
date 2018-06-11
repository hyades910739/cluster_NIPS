noise_data__ <-
function(data,noise_rate=0.1){
  n = dim(data)[1]
  noise = sapply(apply(data,2,sd)*noise_rate,function(x){
    rnorm(n=n,mean=0,sd =x)})
  res = noise+data
  return(res)
}
