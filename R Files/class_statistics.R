table=read.csv("project\\best_table.csv", header=TRUE,sep=",")
table$rnorvegicus_homolog_goc_score

png(filename = "project\\%id_Target_boxplot.png")
boxplot( rnorvegicus_homolog_perc_id~ class, data = table,
        ylab = "%id Target", main = "%id Target v/s class",par(las=1,cex.axis=0.9))
dev.off()

png(filename = "project\\%id_Query_boxplot.png")
boxplot( rnorvegicus_homolog_perc_id_r1~ class, data = table,
         ylab = "%id Query", main = "%id Query v/s class",par(las=1,cex.axis=0.9))
dev.off()

png(filename = "project\\goc_score_boxplot.png")
boxplot( rnorvegicus_homolog_goc_score~ class, data = table,
         ylab = "GOC Score", main = "GOC Score v/s class",par(las=1,cex.axis=0.9))
dev.off()

png(filename = "project\\wga_coverage_boxplot.png")
boxplot( rnorvegicus_homolog_wga_coverage~ class, data = table,
         ylab = "WGA Coverage", main = "WGA Coverage v/s class",par(las=1,cex.axis=0.9))
dev.off()

tab<-split(table,table$class)
data<-data.frame(
  class=c()
)

data1<-data.frame(
    percent_id_Target_mean=c(),
    percent_id_Target_sd=c()
)

for (i in tab){
  class=i$class[1]
  mn=mean(i$rnorvegicus_homolog_perc_id,na.rm=TRUE)
  sd=sd(i$rnorvegicus_homolog_perc_id,na.rm = TRUE)
  d<-data.frame(
    percent_id_Target_mean=c(mn),
    percent_id_Target_sd=c(sd)
  )
  c<-data.frame(
    class=c(class)
  )
  data1<-rbind(data1,d)
  data<-rbind(data,c)
}

data2<-data.frame(
  percent_id_Query_mean=c(),
  percent_id_Query_sd=c()
)

for (i in tab){
  mn=mean(i$rnorvegicus_homolog_perc_id_r1,na.rm=TRUE)
  sd=sd(i$rnorvegicus_homolog_perc_id_r1,na.rm = TRUE)
  d<-data.frame(
    percent_id_Query_mean=c(mn),
    percent_id_Query_sd=c(sd)
  )
  data2<-rbind(data2,d)
}

data3<-data.frame(
  goc_score_mean=c(),
  goc_score_sd=c()
)

for (i in tab){
  mn=mean(i$rnorvegicus_homolog_goc_score,na.rm=TRUE)
  sd=sd(i$rnorvegicus_homolog_goc_score,na.rm = TRUE)
  d<-data.frame(
    goc_score_mean=c(mn),
    goc_score_sd=c(sd)
  )
  data3<-rbind(data3,d)
}

data4<-data.frame(
  wga_coverage_mean=c(),
  wga_coverage_sd=c()
)

for (i in tab){
  mn=mean(i$rnorvegicus_homolog_wga_coverage,na.rm=TRUE)
  sd=sd(i$rnorvegicus_homolog_wga_coverage,na.rm = TRUE)
  d<-data.frame(
    wga_coverage_mean=c(mn),
    wga_coverage_sd=c(sd)
  )
  data4<-rbind(data4,d)
}


d<-cbind(data,data1,data2,data3,data4)
rownames(d)<-d$class
d$class<-NULL
d<-t(d)
write.csv(d,"project\\mean_and_sd.csv", row.names = TRUE)
