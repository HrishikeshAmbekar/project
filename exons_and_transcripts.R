library(EnsDb.Mmusculus.v79)
genes=c("Acvr2a","Bmp4","Bmpr1a","Ctnna1","Ctnnb1","Dicer1","Fgfr2","Inhba","Lef1","Msx1","Pax9","Pitx2","Runx2","Shh","Trp63","Apc","Barx1","Bcl11b","Bmp2","Bmp7","Chuk","Eda","Edar","Edaradd","Evc","Fgf10","Fgf20","Fgf3","Foxi3","Fst","Gas1","Jag2","Lrp4","Msx2","Pdgfra","Rps6ka3","Smo","Sostdc1")
edb=EnsDb.Mmusculus.v79
Tx=transcripts(edb, filter = GeneNameFilter("Acvr2a"))
Ex=exons(edb,filter = GeneNameFilter("Acvr2a"))
h=head(start(Tx))
bio_type=head(Tx$tx_biotype)
data=data.frame(
  gene_name=c(),
  ensembl_id=c(),
  transcripts=c(),
  exons=c()
)

get_exon_data<-function(genes,data){
  for (gene in genes){
    curr=data.frame(
      gene_name=c(gene),
      ensembl_id=c(0),
      #transcripts=c(transcriptsBy(edb,filter = GeneNameFilter(gene))),
      exons=c(exonsBy(edb,filter = GeneNameFilter(gene)))
    )
    data<-rbind(data,curr)
  }
  data
}

get_transcript_data<-function(genes,data){
  for (gene in genes){
    curr=data.frame(
      gene_name=c(gene),
      ensembl_id=c(0),
      transcripts=c(transcriptsBy(edb,filter = GeneNameFilter(gene)))
      #exons=c(exonsBy(edb,filter = GeneNameFilter(gene)))
    )
    data<-rbind(data,curr)
  }
  data
}

frame<-data.frame(
  gene_name=c(),
  gene_id=c(),
  number_of_transcripts=c(),
  number_of_exons=c()
)

get_number_of_transcripts_and_exons<-function(genes,frame){
  for (gene in genes){
    g<-genes(edb, filter = GeneNameFilter(gene))
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
  frame
  
}
exon_data<-get_exon_data(genes,data)
transcript_data<-get_transcript_data(genes,data)
number_of_transcripts_and_exons<-get_number_of_transcripts_and_exons(genes,frame)
write.csv(exon_data,"exons.csv", row.names = FALSE)
write.csv(transcript_data,"transcripts.csv", row.names = FALSE)
write.csv(number_of_transcripts_and_exons,"number_of_transcripts_and_exons.csv", row.names = FALSE)

