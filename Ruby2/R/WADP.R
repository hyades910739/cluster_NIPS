WADP <-
function(data,noise_rate=0.1,n_iter=10,method="EM",...){
  if(method !="EM" & method != "Kmeans") stop("Method should be 'EM' or 'Kmeans'.")
  iter = c()
  for(i in 1:n_iter){
    noise_data = noise_data__(data,noise_rate=noise_rate)
    if(method=="EM"){
      clus1 = EM(data,...)$cluster
      clus2 = EM(noise_data,...)$cluster
    }
    if(method=="Kmeans"){
      clus1 = kmeans(data,...)$cluster
      clus2 = kmeans(noise_data,...)$cluster    
    }
    cluster_res = data.frame(a=clus1,b=clus2)
    res = 0
    for(j in unique(cluster_res$a)){
      len = sum(cluster_res$a==j)
      res = res + len*mean(dist(cluster_res[which(cluster_res$a==j),2])>0)
    }
    iter = c(iter,(res/dim(cluster_res)[1]))
  }
  return(iter)
}
