print.EM <-
function(x){
  cat("#### EM for Gaussian Mixture Model ####\n")
  cat("# of cluster:",length(x$alpha),"\n")
  cat("n_iter:",x$iter,"\n\n")
  #cluster mean:
  meandf = t(data.frame(x$mean))
  rownames(meandf) = 1:length(x$alpha)
  cat("$cluster means:\n")
  print(meandf)
  cat("\n$cluster vector:\n")
  print(x$cluster)
  cat("\nAvailable components:\n ")
  print(names(x))
}
