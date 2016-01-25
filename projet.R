setwd('C:/Users/acer/Desktop/Descartes/Apprentissage non supervisé')
velo<-read.table(file='velo.txt',header=TRUE)

#etude univariée 

summary(velo$longitude)

boxplot(velo$longitude)

summary(velo$h20)

boxplot(velo[,15:25],col=c('blue','royalblue2','green','yellow','magenta','red'),las=2)

#etude bivariée

#verification de la normalité :

shapiro.test(velo$longitude)
shapiro.test(velo$h20)

#test de corrélation :
cor.test( velo$h20,velo$longitude, method ="spearman")

library(Hmisc)
correlation<-rcorr(as.matrix(velo[,12:36]))

library(corrplot)

corrplot(correlation$r, type="upper", order="hclust", tl.col="black", tl.srt=45)

velo.actives<-as.matrix(velo[,4:171])
library('FactoMineR')
velo.pca<-PCA(velo.actives)
velo.actives<-velo.pca$ind$coord

#Classification spectrale :


sigma2=mean(var(velo[,4:171]))


#definition de nombre de cluster
# calcul de la matrice de similarité 

velo.sim<-matrix(0,nrow(velo.actives),nrow(velo.actives))

norm_vec <- function(x){ sqrt(sum(x^2))}

wij<-function(x,y,sigma2){

  return(exp(-norm_vec(x-y)/(2*sigma2)))
  
}




#critère de similarité gaussienne 
w<-function(data,W,sigma2){
  for(i in 1:nrow(W)){
    for(j in 1:ncol(W)){
      W[i,j]=wij(data[i,],data[j,],sigma2)
    
      }
  }
  return(W)
}
velo.sim<-w(velo.actives,velo.sim,sigma2)

#matrice laplacienne
D=diag(rowSums(velo.sim))

L=D-velo.sim

#calcule des valeurs et des vecteur propores de Lrw

L.eigen<-eigen(L,symmetric = TRUE)
values<-L.eigen$values
vectors<-L.eigen$values
#Projection des valeurs propres 
#avec eigen on obtient les valeurs propres en ordre décroissant !!!!!
plot(rev(values))
plot(rev(values[0:200]))
abline(h=0.0055, col="red", lty=2)
abline(h=0.012,col="red",lty=2)
abline(h=0.031,col="red",lty=2)


#application de la classification spectrale :
library(kernlab)

sc <- specc(velo.actives,kernel ='rbfdot',centers=4 ,kpar=list(sigma=sqrt(sigma2)))


#vers une maximisation ou minimisation de sigma
scmin <- specc(velo.actives,kernel ='rbfdot',centers=4 ,kpar=list(sigma=sqrt(0.01)))
scmax <- specc(velo.actives,kernel ='rbfdot',centers=4 ,kpar=list(sigma=sqrt(0.06)))
withinss(sc)
withinss(scmin)
withinss(scmax)

norm_vec(withinss(sc)-withinss(scmin))
norm_vec(withinss(sc)-withinss(scmax))

plot(velo.actives,col=scmin,pch=20)
points(centers(scmin),type="p",col="yellow" , pch=18 ,cex=2)

#classification hierarchique
library('cluster')
d<-dist(velo.actives,method="euclidian")
H <- agnes(d, method="ward")

plot(as.hclust(H),hang=-1)
rect.hclust(as.hclust(H),k=4,border="red")
#voir nb clust 
classes <- cutree(H, k=4)


#calcul du centre de gravité 

find.centre<-function(vec){
  res<-matrix(0,nrow=1,ncol=ncol(vec))
for(i in 1:ncol(vec)) 
 res[,i]<-mean(vec[,i])
 
 return(res)
  
}

form.cluster<-function(k,classes){
  
  
    cluster<-velo.actives[which(classes %in% k),]
  
  return (cluster)
}
cluster1<-form.cluster(1,classes)
center1<-find.centre(cluster1)

cluster2<-form.cluster(2,classes)
center2<-find.centre(cluster2)

cluster3<-form.cluster(3,classes)
center3<-find.centre(cluster3)

cluster4<-form.cluster(4,classes)
center4<-find.centre(cluster4)


plot(velo.actives,col=classes,pch=20)
points(center4,type="p",col="yellow" , pch=18 ,cex=2)



#calcul des distance interclasse
library('GMD')
withinss.cah<-css(d,classes)
withinss.cah$wss
withinss.cah$totwss
#classification par partition 



velo.kmeans<-kmeans(velo.actives,4)
classes<-velo.kmeans$cluster
plot(velo.actives,col=classes,pch=20)
points(velo.kmeans$centers,type="p",col="yellow" , pch=18 ,cex=2)

#comparaison 2
withinss.cah$totwss
velo.kmeans$tot.withinss
sum(withinss(scmin))
#projection des points selon k-means 

library('ggmap')

theme_set(theme_bw(16))
Map <- qmap("paris", zoom = 12, color = "bw")
Map
Map +
  geom_point(aes(x = velo[,2], y = velo[,3], colour =factor( classes),shape= factor( classes),size=1),
             data = velo)


#pour voir à quel heures elles sont utilisées le plus 
velo.actives<-as.matrix(velo[,4:171])
classes
#1
plot(velo.actives[1,1:24],type="l")
plot(velo.actives[2,1:24],type="l")
plot(velo.actives[3,1:24],type="l")

#2
plot(velo.actives[43,1:24],type="l")
plot(velo.actives[44,1:24],type="l")
plot(velo.actives[45,1:24],type="l")

# sur les jours ouvrès 
velo.actives<-as.matrix(velo[,4:148])
velo.pca<-PCA(velo.actives)
velo.actives<-velo.pca$ind$coord
#classification spéctrale
scmin <- specc(velo.actives,kernel ='rbfdot',centers=4 ,kpar=list(sigma=sqrt(0.01)))
withinss(scmin)
#cah
d<-dist(velo.actives,method="euclidian")
H <- agnes(d, method="ward")
classes <- cutree(H, k=4)
withinss.cah<-css(d,classes)
withinss.cah$wss
#classification par partition 

velo.kmeans<-kmeans(velo.actives,4)
velo.kmeans$withinss

#comparaison 2
withinss.cah$totwss
velo.kmeans$tot.withinss
sum(withinss(scmin))



