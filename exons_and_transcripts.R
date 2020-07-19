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
exon_data<-get_exon_data(genes,data)
transcript_data<-get_transcript_data(genes,data)
write.csv(exon_data,"exons.csv", row.names = FALSE)
write.csv(transcript_data,"transcripts.csv", row.names = FALSE)
