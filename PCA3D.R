####################################################################
##CLASSIFICATION###
####################################################################
require(class);library(FactoMineR);require(gmodels);require(reshape2);require(ggplot2);require(vegan);require(car);require(rgl);require(pca3d);require(dendextend)
require(FactoMineR):require(missMDA):require(magick)

inDir<-"E:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL" #E
FRA<-read.csv(paste0(inDir,"/","ORINOQUIA.csv"),header = T,sep = "|",row.names = 9)

FRA<-FRA[,23:ncol(FRA)]#data<-data[,10:ncol(data)]


nb <-estim_ncpPCA(FRA,ncp.min=1,ncp.max=5,method.cv="Kfold",nbsim=100)
imputed <- imputePCA(FRA,ncp=nb$ncp)
ss2<-PCA(imputed$completeObs,ncp = 5,scale.unit = T)
ss<-HCPC(ss2,nb.clust=-1,iter.max=1000)




###################################

# Sys.setenv(PATH = paste("c:/Program Files/ImageMagick/bin",
#                         Sys.getenv("PATH"), sep = ";"))
x2<-cbind(FRA,ss$data.clust$clust)
x2<-x2[order(x2$`ss$data.clust$clust`),]
x2$COL<-NA

###################################
x2$COL[which(x2$`ss$data.clust$clust`==1)]<-"#f2766d"#AZUL OSCURO
x2$COL[which(x2$`ss$data.clust$clust`==2)]<-"#7cae00"#AZUL OSCURO2
x2$COL[which(x2$`ss$data.clust$clust`==3)]<-"#00bfc4"#TERRACOTA
x2$COL[which(x2$`ss$data.clust$clust`==4)]<-"#c77cff"#AZUL MUY OSCURO
###################################
###################################################################################################
x2$Vuln_threat[which(is.na(x2$Vuln_threat))]<-0
x2$gini_2002_2012[which(is.na(x2$gini_2002_2012))]<-0
res_pca_prcomp<-prcomp(x2[,-c(15,16)])

#################################EQUALING PCA METHODS TO USE PCA3D FUNCTION###################################
res_pca_prcomp_test<-res_pca_prcomp
#sqrt(res_pca$eig$eigenvalue)
res_pca_prcomp_test$sdev<-sqrt(ss2$eig$eigenvalue)#Standard Deviation  Vs Standard Error

rot<-t(apply(ss2$var$coord, 1, function(x) {x/sqrt(ss2$eig[,1])}))

res_pca_prcomp_test$rotation<-rot[order(row.names(res_pca_prcomp$rotation)),]
colnames(res_pca_prcomp_test$rotation)<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14")
res_pca_prcomp_test$center<-res_pca_prcomp_test$center #NO CAMBIA
res_pca_prcomp_test$scale<-res_pca_prcomp_test$scale   #NO CAMBIA
or_ISO<-matrix(row.names(res_pca_prcomp$x))
ind<-as.data.frame(ss2$ind$coord)
ind$nam<-row.names(ind)
#ind[order(or_ISO,ind$ISO3),]
ind2<-merge(data.frame(nam=row.names(res_pca_prcomp$x)),ind,by.x = "nam",by.y = "nam",all.x = T,all.y = F,sort = F)
row.names(ind2)<-ind2$nam
ind2<-ind2[,-1]
res_pca_prcomp_test$x<- as.matrix(ind2) 

colnames(res_pca_prcomp_test$x)<-c("PC1","PC2","PC3","PC4","PC5")

row.names(res_pca_prcomp_test$x)[35]<-"vichada puerto carreno";gc()



####################################################################################################################################


summary(res_pca_prcomp_test)
summary(ss2)

pca3d(res_pca_prcomp_test,
      group = x2[,15], 
      col=x2[,16],
      labels.col = x2[,16],
      show.plane=F,
      show.ellipses = F,
      #ellipse.ci = 0.95,
      show.shapes = T,
      show.shadows = F,
      show.labels = F,
      fancy=F,
      axes.color= "black",
      #axes.color= "white",
      show.centroids = F,
      biplot=T,
      show.group.labels = F, 
      radius = 1,
      #  bg = "white",
      bg = "snow1",
      biplot.vars=14,
      axe.titles=c("PC1 33.05%","PC2 15.89%","PC3 10.18%"),
      title="Agrupaciones en contexto"
);gc()

#legend3d("topright", legend = paste('Grupo', c('1', '2', '3', '4'))
 
legend3d("topright", legend = paste('Grupo', c('1', '2', '3'))
         , pch = 16, col = unique(x2$COL), cex=2, inset=c(0.01));gc()

setwd("E:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL")

snapshotPCA3d("sc1.png")
#ftp://ftp.imagemagick.org/pub/ImageMagick/binaries/ImageMagick-6.9.5-2-Q16-x64-dll.exe 
#http://ftp.icm.edu.pl/packages/ImageMagick/binaries/ImageMagick-6.9.5-2-Q16-x64-dll.exe
###RUN MANUALLY
makeMoviePCA( dir="E:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL", clean=T,type = "gif")#convert="C:/Program Files/ImageMagick-6.9.5-Q16/convert.exe")
#,legend=unique(res_hcpc$data.clust$clust))

