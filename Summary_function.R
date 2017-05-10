

df<-read.csv("E:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL/ORI_CLUSTER.csv",header=T,sep="|")


df2<-df[,24:(ncol(df)-1)]#11



#df2<-as.data.frame(cbind(df2$Prom_CO2..2010.,df2$Hectareas_Coca..2001.2010.,df2$Acciones.Armadas..2001.2010.,df2$pobl.tot,df2$g.terreno,df2$desemp.fisc,df2$dismdo,df2$nbi,df2$minorias,df2$Municipalities))

#stages<-c("1.) Forested","4.) Stable non-forested","3.) Stable semi-forested","2.) Unstable forested")

  clus<-unique(df$CLUST)

tmp<-list()
#i=1
for(i in 1:length(clus)){

df3<-df2[which(df$CLUST==clus[[i]]),]


tmp[[i]]<-as.data.frame(matrix(nrow=ncol(df3),ncol=7))

colnames(tmp[[i]])<-c("mean","sd","median","min","max","n","cluster")
row.names(tmp[[i]])<-colnames(df3)
for(j in 1:(ncol(df3))){
  
  if(is.factor(df3[[j]])) {
    cat("factor","/n")
    
    tmp[[i]][j,1]<-NA
    tmp[[i]][j,2]<-NA
    tmp[[i]][j,3]<-NA
    tmp[[i]][j,4]<-NA
    tmp[[i]][j,5]<-NA
    tmp[[i]][j,6]<-NA
    tmp[[i]][j,7]<-NA
    
    
    
    
  } else{
    tmp[[i]][j,1]<-mean(df3[[j]],na.rm=T)
    tmp[[i]][j,2]<-sd(df3[[j]],na.rm=T)
    tmp[[i]][j,3]<-median(df3[[j]],na.rm=T)
    tmp[[i]][j,4]<-min(df3[[j]],na.rm=T)
    tmp[[i]][j,5]<-max(df3[[j]],na.rm=T)
    tmp[[i]][j,6]<-length(df3[[j]])
    tmp[[i]][j,7]<-as.character(clus[[i]])
   
    }
  };rm(j)
};rm(i)



total<-as.data.frame(matrix(nrow=ncol(df2),ncol=7))

colnames(total)<-c("mean","sd","median","min","max","n","cluster")
row.names(total)<-colnames(df3)
  
  
for(i in 1:ncol(df2)){
  
if(is.factor(df2[[i]])) {
  cat("factor","/n")
  
} else{
    total[i,1]<-mean(df2[[i]],na.rm=T)
    total[i,2]<-sd(df2[[i]],na.rm=T)
    total[i,3]<-median(df2[[i]],na.rm=T)
    total[i,4]<-min(df2[[i]],na.rm=T)
    total[i,5]<-max(df2[[i]],na.rm=T)
    total[i,6]<-length(df2[[i]])
    total[i,7]<-"Total"
    
    
  }
};rm(i)


final<-do.call("rbind",tmp)

final<-rbind(total,final)
write.table(final,"E:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL/SUMMARY_CLUSTER.csv",quote=F,row.names = T,sep = "|")
