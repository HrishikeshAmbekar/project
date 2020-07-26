library(AnnotationHub)

mouse_query <- query(AnnotationHub(), c("mus musculus","EnsDb",99))
mouse_edb <- mouse_query[[1]]

rat_query <- query(AnnotationHub(), c("rattus norvegicus","EnsDb",99))
rat_edb <- rat_query[[1]]

genes=c("Acvr2a","Bmp4","Bmpr1a","Ctnna1","Ctnnb1","Dicer1","Fgfr2","Inhba","Lef1","Msx1","Pax9","Pitx2","Runx2","Shh","Trp63","Apc","Barx1","Bcl11b","Bmp2","Bmp7","Chuk","Eda","Edar","Edaradd","Evc","Fgf10","Fgf20","Fgf3","Foxi3","Fst","Gas1","Jag2","Lrp4","Msx2",
        "Pdgfra","Rps6ka3","Smo","Sostdc1","Sp6","Spry2","Spry4","Wnt10a","Yap1","Alpl","Ambn","Amelx","Amtn","Bmi1","Dmp1","Dspp","Enam",
        "Evc2","Fam20a","Fam20c","Fgfr1","Gdnf","Grem2","Klk4","Lama3","Mmp14","Mmp20","Mtor","Nectin1","Perp","Pkd2","Postn","Slc13a5","Slc39a13","Sp3",
        "Sppl2a","Acvr2b","Dlx1","Dlx2","Dlx3","Dlx4","Dlx5","Dlx6","Gli2","Gli3","Lhx6","Lhx8","Csf1","Fos","Ostm1","Pthlh","Tcirg1","Traf6")


data_frame<-data.frame(
  gene_name=c(),
  mouse_gene_id=c(),
  mouse_number_of_transcripts=c(),
  mouse_number_of_exons=c(),
  rat_gene_id=c(),
  rat_number_of_transcripts=c(),
  rat_number_of_exons=c()
)

get_mouse_rat_comparison<-function(mouse_edb,rat_edb,genes,data_frame){
  for (gene in genes){
    mg<-genes(mouse_edb, filter = GeneNameFilter(gene))
    rg<-genes(rat_edb, filter = GeneNameFilter(gene))
    d=data.frame(rg)
    if (nrow(d)==0){
      M_id=mg$gene_id
      M_Tx=transcripts(mouse_edb, filter = c(GeneNameFilter(gene),TxBiotypeFilter("protein_coding")),return.type="data.frame")
      M_Ex=exons(mouse_edb, filter = c(GeneNameFilter(gene),TxBiotypeFilter("protein_coding")),return.type="data.frame")
      curr<-data.frame(
        gene_name=c(gene),
        mouse_gene_id=c(M_id),
        mouse_number_of_transcripts=c(nrow(M_Tx)),
        mouse_number_of_exons=c(nrow(M_Ex)),
        rat_gene_id=c("NA"),
        rat_number_of_transcripts=c("NA"),
        rat_number_of_exons=c("NA")
      )
      data_frame<-rbind(data_frame,curr)
      
    }
    else{
      M_id=mg$gene_id
      M_Tx=transcripts(mouse_edb, filter = c(GeneNameFilter(gene),TxBiotypeFilter("protein_coding")),return.type="data.frame")
      M_Ex=exons(mouse_edb, filter = c(GeneNameFilter(gene),TxBiotypeFilter("protein_coding")),return.type="data.frame")
      R_id=rg$gene_id
      R_Tx=transcripts(rat_edb, filter = c(GeneNameFilter(gene),TxBiotypeFilter("protein_coding")),return.type="data.frame")
      R_Ex=exons(rat_edb, filter = c(GeneNameFilter(gene),TxBiotypeFilter("protein_coding")),return.type="data.frame")
      curr_data=data.frame(
        gene_name=c(gene),
        mouse_gene_id=c(M_id),
        mouse_number_of_transcripts=c(nrow(M_Tx)),
        mouse_number_of_exons=c(nrow(M_Ex)),
        rat_gene_id=c(R_id),
        rat_number_of_transcripts=c(nrow(R_Tx)),
        rat_number_of_exons=c(nrow(R_Ex))
      )
      data_frame<-rbind(data_frame,curr_data)
    }
  }
  data_frame
} 


mouse_rat_comparison<-get_mouse_rat_comparison(mouse_edb,rat_edb,genes,data_frame)
write.csv(mouse_rat_comparison,"project\\mouse_rat_protein_coding_comparison.csv", row.names = FALSE)

