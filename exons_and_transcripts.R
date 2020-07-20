library(AnnotationHub)

mouse_query <- query(AnnotationHub(), c("mus musculus","EnsDb",99))
mouse_edb <- mouse_query[[1]]

rat_query <- query(AnnotationHub(), c("rattus norvegicus","EnsDb",99))
rat_edb <- rat_query[[1]]

genes=c("Acvr2a","Bmp4","Bmpr1a","Ctnna1","Ctnnb1","Dicer1","Fgfr2","Inhba","Lef1","Msx1","Pax9","Pitx2","Runx2","Shh","Trp63","Apc","Barx1","Bcl11b","Bmp2","Bmp7","Chuk","Eda","Edar","Edaradd","Evc","Fgf10","Fgf20","Fgf3","Foxi3","Fst","Gas1","Jag2","Lrp4","Msx2",
        "Pdgfra","Rps6ka3","Smo","Sostdc1","Sp6","Spry2","Spry4","Wnt10a","Yap1","Alpl","Ambn","Amelx","Amtn","Bmi1","Dmp1","Dspp","Enam",
        "Evc2","Fam20a","Fam20c","Fgfr1","Gdnf","Grem2","Klk4","Lama3","Mmp14","Mmp20","Mtor","Nectin1","Perp","Pkd2","Postn","Slc13a5","Slc39a13","Sp3",
        "Sppl2a","Acvr2b","Dlx1","Dlx2","Dlx3","Dlx4","Dlx5","Dlx6","Gli2","Gli3","Lhx6","Lhx8","Csf1","Fos","Ostm1","Pthlh","Tcirg1","Traf6")
#library(EnsDb.Mmusculus.v79)
#edb=EnsDb.Mmusculus.v79

data=data.frame(
  gene_name=c(),
  ensembl_id=c(),
  transcripts=c(),
  exons=c()
)

get_exon_data<-function(edb,genes,data){
  for (gene in genes){
    g<-genes(edb, filter = GeneNameFilter(gene))
    d=data.frame(g)
    if (nrow(d)==0){
      print(gene)
    }
    else{
      id=g$gene_id
      curr=data.frame(
        gene_name=c(gene),
        gene_ensembl_id=c(id),
        exons=c(exonsBy(edb, by =  "gene",columns = listColumns(edb, "exon"),
                        filter =GeneNameFilter(gene))
        )
      )
      data<-rbind(data,curr)
    }
  }
  data
}

get_transcript_data<-function(edb,genes,data){
  for (gene in genes){
    g<-genes(edb, filter = GeneNameFilter(gene))
    d=data.frame(g)
    if (nrow(d)==0){
      print(gene)
    }
    else{
      id=g$gene_id
      curr=data.frame(
        gene_name=c(gene),
        gene_ensembl_id=c(id),
        transcripts=c(transcriptsBy(edb, by =  "gene",columns = listColumns(edb, "transcript"),
                              filter =GeneNameFilter(gene))
        )
      )
      data<-rbind(data,curr)
    }
  }
  data
}

frame<-data.frame(
  gene_name=c(),
  gene_id=c(),
  number_of_transcripts=c(),
  number_of_exons=c()
)

get_number_of_transcripts_and_exons<-function(edb,genes,frame){
  for (gene in genes){
    g<-genes(edb, filter = GeneNameFilter(gene))
    d=data.frame(g)
    if (nrow(d)==0){
      print(gene)
    }
    else{
      id=g$gene_id
      Tx=transcripts(edb, filter = GeneNameFilter(gene),return.type="data.frame")
      Ex=exons(edb, filter = GeneNameFilter(gene),return.type="data.frame")
      curr_data=data.frame(
        gene_name=c(gene),
        gene_id=c(id),
        number_of_transcripts=c(nrow(Tx)),
        number_of_exons=c(nrow(Ex))
      )
      frame<-rbind(frame,curr_data)
    }
  }
  frame
  
}
  
  
mouse_exon_data<-get_exon_data(mouse_edb,genes,data)
mouse_transcript_data<-get_transcript_data(mouse_edb,genes,data)
number_of_transcripts_and_exons_mouse<-get_number_of_transcripts_and_exons(mouse_edb,genes,frame)
write.csv(mouse_exon_data,"mouse_exons.csv", row.names = FALSE)
write.csv(mouse_transcript_data,"mouse_transcripts.csv", row.names = FALSE)
write.csv(number_of_transcripts_and_exons_mouse,"mouse_number_of_transcripts_and_exons.csv", row.names = FALSE)

print("mouse_finished")
rat_exon_data<-get_exon_data(rat_edb,genes,data)
print("exons_finished")
rat_transcript_data<-get_transcript_data(rat_edb,genes,data)
print("transcripts_finished")
rat_number_of_transcripts_and_exons<-get_number_of_transcripts_and_exons(rat_edb,genes,frame)
write.csv(rat_exon_data,"rat_exons.csv", row.names = FALSE)
write.csv(rat_transcript_data,"rat_transcripts.csv", row.names = FALSE)
write.csv(rat_number_of_transcripts_and_exons,"rat_number_of_transcripts_and_exons.csv", row.names = FALSE)

