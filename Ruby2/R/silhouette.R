#' @import cluster
silhouette <-function(cluster,data,method="euclidean"){
  res = cluster::silhouette(x=cluster,dist=dist(data,method=method))
  return(mean(res[,3]))
}