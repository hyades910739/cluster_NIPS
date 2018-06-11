
install.packages("Ruby_1.2.tar.gz",type="source",repos=NULL)
library(Ruby)
##### example : iris
# Run EM and Kmeans
res = EM(iris[,-5],3,100)
res2 = kmeans(iris[,-5],centers = 3,nstart = 100)

#confuse matrix:
library(magrittr)
table(res$cluster,iris[,5]) %>% write.csv("temp.csv")
table(res2$cluster,iris[,5])

#silhouette
silhouette(res$cluster,iris[,-5])
silhouette(res2$cluster,iris[,-5])

# purity
purity(iris[,5],res$cluster)
purity(iris[,5],res2$cluster)

# WADP
a = WADP(iris[,-5],noise_rate = 0.1,n_iter = 100,method = "EM",cluster=3)
b = WADP(iris[,-5],noise_rate = 1,n_iter = 100,method = "EM",cluster=3)
c = WADP(iris[,-5],noise_rate = 0.1,n_iter = 100,method = "Kmeans",centers=3,nstart=10)
d = WADP(iris[,-5],noise_rate = 10,n_iter = 100,method = "Kmeans",centers=3,nstart=10)
df = data.frame(EM=c(mean(a),mean(b)),Kmeans=c(mean(c),mean(d)))
rownames(df) = c(0.1,1)
df

##### example : seeds
data("seeds")
# Run EM and Kmeans
res3 = EM(seeds[,-8],3,100)
res4 = kmeans(seeds[,-8],centers = 3,nstart = 100)

#confuse matrix:
table(res3$cluster,seeds[,8])
table(res4$cluster,seeds[,8])

#silhouette
silhouette(res3$cluster,seeds[,-8])
silhouette(res4$cluster,seeds[,-8])

# purity
purity(seeds[,8],res3$cluster)
purity(seeds[,8],res4$cluster)

# WADP
a = WADP(seeds[,-8],noise_rate = 0.1,n_iter = 100,method = "EM",cluster=3)
b = WADP(seeds[,-8],noise_rate = 1,n_iter = 100,method = "EM",cluster=3)
c = WADP(seeds[,-8],noise_rate = 0.1,n_iter = 100,method = "Kmeans",centers=3,nstart=10)
d = WADP(seeds[,-8],noise_rate = 10,n_iter = 100,method = "Kmeans",centers=3,nstart=10)
df = data.frame(EM=c(mean(a),mean(b)),Kmeans=c(mean(c),mean(d)))
rownames(df) = c(0.1,1)
df
