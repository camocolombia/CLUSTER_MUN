library(ggplot2)

outdir<-"D:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL/OLD/2017_09_04"
#outdir<-"D:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL"

df<-read.csv("D:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL/OLD/2017_09_04/ORI_CLUSTER.csv",header=T,sep="|")
#df<-read.csv("D:/Dropbox/Dropbox/AUGUSTO_2017_05_04/DB_FINAL/ORI_CLUSTER.csv",header=T,sep="|")

#df<-df[order(df$CLUST== 1,df$CLUST== 2,df$CLUST== 3,decreasing = T),]
df<-df[order(df$CLUST== 1,df$CLUST== 2,df$CLUST== 3,df$CLUST== 4,decreasing = T),]

df$CLUST<-factor(df$CLUST,unique(df$CLUST))

df$area_mun_ha<-df$area_mun_ha/1000
df$PN_MIN<-df$PN_MIN/1000
df$minorias_2010<-df$minorias_2010/1000
df$parques_2010<-df$parques_2010/1000
df$pobl_tot<-df$pobl_tot/1000

labels<-c(
  "Población municipal (x1000)",
  "Cacao (ha)",
  "Caucho (ha)", 
  "Cultivos forestales comerciales (ha)",
  "Palma (ha)",
  "Índice de incidencia del conflicto armado", 
  "Área municipal (x 1000 ha)",
  "Territorios colectivos (x 1000 ha)",
  "Parques nacionales naturales (x 1000 ha)",
  "Valor crédito a alianzas estratégicas",
  "Población desplazada (Número de personas)",
  "Beneficiarios de alianzas estrategicas agricolas",
  "STATUS",
  "Gini de terreno",
  "Índice de necesidades basicas",
  "Distancia a mercado (Km)",
  # "Territorios colectivos y parques naturales (% Área municipal)",
  # "Área usada para alianzas estrategicas agricolas (% Área municipal)",
  "Territorios colectivos y parques naturales (x 1000 ha)",
  "Área usada para alianzas estrategicas agricolas (x1000 ha)",
  "Valor crédito a agremiaciones de productores",
  "Indicador de desempeño fiscal",
  "Carbono (t/ha)",
  "Perdida de cobertura boscosa (%)",
 # "Pastos introducidos (% Área municipal)",
 "Pastos introducidos (ha)",
  "Índice de amenaza a ecosistemas",
# "Área sembrada en Coca (% Área municipal)",
 "Área sembrada en Coca (x 1000 ha)",
 # "Afectados por conflicto (% Población municipal)",
 "Afectados por conflicto",
  "Número de acciones armadas"
 )

for(i in 11:(ncol(df)-1)){
  cat(i,"\n")
s<-colnames(df)[i]
s2<-df[,c(ncol(df),i)]
colnames(s2)<-c("CLUST","VAR")
#s2<-s2[order(s2$CLUST== "1",s2$CLUST== "2",s2$CLUST== "3",decreasing = T),]
s2<-s2[order(s2$CLUST== "1",s2$CLUST== "2",s2$CLUST== "3",s2$CLUST== "4",decreasing = T),]

cat("calc min and max","\n")


if(max(s2[,2],na.rm = T)<=1){

  y_max<-max(s2[,2],na.rm = T)+0.02
}else{
  y_max<-max(s2[,2],na.rm = T)+0.02
  }

  if((min(s2[,2],na.rm = T)-2)<=0){
  y_min<-0
}else{
  y_min<- min(s2[,2],na.rm = T)-0.02
}

# upper.limit <- quantile(s2[,2],na.rm = T)[4] + 1.5*IQR(s2[,2],na.rm = T)
#   if((min(s2[,2],na.rm = T)-2)<=0){
#     lower.limit<-0
# }else{
# lower.limit <- quantile(s2[,2],na.rm = T)[2] - 1.5*IQR(s2[,2],na.rm = T)
#  }
prom<-ggplot(s2, aes(x =CLUST, y = VAR)) +
  geom_boxplot(aes(fill=CLUST),outlier.size=NA,position=position_dodge(width=6))+
  stat_boxplot(geom ='errorbar') +
 # ggtitle("Cacao") + 
  stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
  stat_summary(fun.y=mean, geom="point")+
  
  #scale_fill_manual(labels = c("1","2","3","4"),values = c("#267a09","#ff5500","#98e600","#e69800"))+
  #coord_cartesian(ylim=c(lower.limit, upper.limit))+
 coord_cartesian(ylim=c(y_min,y_max))+
  #scale_shape_discrete(name="",label=c("Very High","High","Low","Very low"))+
  xlab("Agrupación")+
  ylab(labels[[i-10]])+
  #theme(panel.background = element_rect(fill = "gray95"),text=element_text(size=42),axis.text.x  = element_text(size=42,colour="black"),axis.text.y  = element_text(size=42,colour="black"),legend.position="none")+ 
  theme(panel.background = element_rect(fill = "gray90"),
        text=element_text(size=60),
        #axis.text.x  =element_blank(),
        axis.text.x  = element_text(size=60,colour="black"),
        axis.title=element_text(size=60,face="bold"),
        axis.text.y  = element_text(size=60,colour="black"),
        legend.position="none") 

ggsave(paste0(outdir,"/PDF/",s,"_",Sys.Date(),".pdf"),units="in",width=21,height=23,scale=1,dpi=600)

}
