require(cluster);require(FactoMineR);require(cclust);require(ggplot2);
require(missMDA);require(corrgram);require(corrplot);require(raster);require(shapefiles)
require(ggmap);require(dplyr);require(rgdal);require(rgeos);library(maptools)

inDir<-"E:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL" #E
data<-read.csv(paste0(inDir,"/","ORINOQUIA.csv"),header = T,sep = "|",row.names = 9)
data2<-read.csv(paste0(inDir,"/","ORINOQUIA.csv"),header = T,sep = "|",row.names = 9)

data<-data[,23:ncol(data)]#data<-data[,10:ncol(data)]
corrplot(cor(data))

#shp2<-shapefile("E:/Dropbox/Dropbox/AUGUSTO_2017_05_04/ORINOQUIA.shp")
shp3<-readOGR(dsn = "E:/Dropbox/Dropbox/AUGUSTO_2017_05_04",layer = "ORINOQUIA")

sss2<-cor(x=data)
 write.table(sss2,paste0(inDir,"/","CORR.csv"),row.names = T,quote = F,sep = "|")

nb <-estim_ncpPCA(data,ncp.min=1,ncp.max=5,method.cv="Kfold",nbsim=100)
imputed <- imputePCA(data,ncp=nb$ncp)
ss2<-PCA(imputed$completeObs,ncp = 5,scale.unit = T)

sums<-dimdesc(ss2, axes=c(1,2,3,4,5))
write.csv(sums$Dim.1$quanti,"E:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL/PCA/PC1.csv")
write.csv(sums$Dim.2$quanti,"E:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL/PCA/PC2.csv")
write.csv(sums$Dim.3$quanti,"E:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL/PCA/PC3.csv")
write.csv(sums$Dim.4$quanti,"E:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL/PCA/PC4.csv")
write.csv(sums$Dim.5$quanti,"E:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL/PCA/PC5.csv")


ss<-HCPC(ss2,nb.clust=-1,iter.max=1000)
plot.HCPC(ss,choice="map")
plot.HCPC(ss,choice="3D.map")
plot.HCPC(ss,choice="tree")
shp3$CLUST<-NA
shp3$CLUST<-ss$data.clust$clust
# plot(shp2,col=shp2$CLUST,
#        #c("red","green","orange","blue")
# main="HCPC")
# legend("bottomright",   # location of legend
#        legend=levels(shp2$CLUST), # categories or elements to render in 
#        # the legend
#        fill=shp2$CLUST
#          #c("red","green","orange","blue")) # color palette to use to fill objects in legend.


shapefile_df <- fortify(shp3,region="DANE")
shapefile_df2 <- fortify(shp3,region="DEPARTAMEN")

# dat_clus<-as.data.frame(cbind(shp3$DANE,shp3$CLUST))
# colnames(dat_clus)<-c("DANE","CLUSTER")

plotData <- merge(shapefile_df, shp3@data,
                           by.x="id",by.y = "DANE")
colnames(plotData)[ncol(plotData)]<-"Grupo"
map <- ggplot() +
  geom_polygon(data = plotData, 
            aes(x = long, y = lat, group = group,fill=Grupo,colour=Grupo), size = .2)+
#            color = 'gray', fill = 'white'
  #  geom_path(data = plotData, 
  #            aes(x = long, y = lat, group = group,fill=NULL,colour="gray"), size = .2)+
  # #            color = 'gray', fill = 'white'
  geom_path(data = shapefile_df2, 
            aes(x = long, y = lat, group = group,fill=NULL), size = .5)
  # geom_text(aes(x = long, y = lat, label = NOMBRE_ENT),
  #           data= plotData,
  #           alpha = 1,
  #           color = "black")
print(map) 
ggsave(paste0(inDir,"/","CLUSTER_ORINOQUIA","_",Sys.Date(),".pdf"),units="in",width=20,height=17,scale=0.5,dpi=600,limitsize = F)
data2$CLUST<-ss$data.clust$clust
write.table(data2,paste0(inDir,"/","ORI_CLUSTER.csv"),row.names = T,quote = F,sep = "|")


