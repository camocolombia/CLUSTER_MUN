indir<-"E:/Dropbox/Dropbox/AUGUSTO_2017_05_04/dat/CEDE"
#vars<-read.csv(paste0(indir,"/","PANEL_car_general.csv"),header=T,sep = "|")
#vars<-read.csv(paste0(indir,"/","PANEL_TIERRA.csv"),header=T,sep = "|")
#vars<-read.csv(paste0(indir,"/","PANEL_CONFLICTO_Y_VIOLENCIA.csv"),header=T,sep = "|")
vars<-read.csv(paste0(indir,"/","PANEL_CARACTERISTICAS_GENERALES2.csv."),header=T,sep = "|")

to_2<-c("pobl_tot")

#to_2<-c("H_coca","desplazados_expulsion","desplazados_recepcion")
#to_2<-c("vrf_peq_agrem","vrf_medgran_agrem","g_terreno","vrf_alianzas")
#to_2<-c("areaoficialhm2","nbi","minorias","parques","otras")

age<-2002:2013

list2<-list()

for(i in 1:length(to_2)){
  
  x<-vars[,c("codmpio","ano",to_2[[i]])]
#x<-vars[,c("codmpio","depto","municipio","ano",to_2[[i]])]
y2<-matrix(nrow=length(unique(vars$codmpio)),ncol=(length(to_2)+4))
y2<-as.data.frame(y2)

for(j in 1:length(age)){
  j2<-j+1
    y<-subset(x,x$ano==age[[j]])
    
y2[1:nrow(y),1]<-y[,1]
#y2[,j2]<-y[,5]
y2[1:nrow(y),j2]<-y[,3]

colnames(y2)[[1]]<-"codmpio"
colnames(y2)[[j2]]<-as.character(paste0(to_2[[i]],"_",age[[j]]))
  };rm(j)
list2[[i]]<-y2

};rm(i)



for(i in 1:length(list2)){
  cat("VAR ",i,"\n")
  list2[[i]]$mean<-NA
  
  for(j in 1:nrow(list2[[i]])){
    
    cat("row ",j,"\n")
  list2[[i]]$mean[j]<-mean(as.matrix(list2[[i]][j,2:(ncol(list2[[i]])-1)]),na.rm = T)
  
  
  }
  write.table(list2[[i]],paste0(indir,"/",to_2[[i]],"_XANO.csv"),row.names=F,quote = F,sep="|",na = "")
}


