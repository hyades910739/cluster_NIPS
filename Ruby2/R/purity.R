purity <-
function(label,cluster){
  if(length(label) != length(cluster)) stop("length of label and cluster should be equal")
  confuse = table(cluster,label)
  pur = sum(apply(confuse,1,max))/length(label)
  return(pur)
}
