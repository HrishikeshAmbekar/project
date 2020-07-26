library(biomaRt)
library(tidyverse)
mouse_rat_comparison=read.csv("project\\mouse_rat_comparison.csv", header=TRUE,sep=",")
ms_ensemble_id<-mouse_rat_comparison[,2]
# ensembl <- useMart("ensembl")
ensembl_ms_mart <- useMart(biomart="ensembl", dataset="mmusculus_gene_ensembl")

mouse_genes <- getBM(
  attributes = c("external_gene_name", "gene_biotype" ),
  filters = "ensembl_gene_id",
  values = ms_ensemble_id,
  uniqueRows = TRUE,
  mart = ensembl_ms_mart
)

BM.attributes = listAttributes(ensembl_ms_mart)

# grep("rnorvegicus.*ortholog", tolower(BM.attributes[,1]), value = TRUE)

# grep("rnorvegicus.*homolog", tolower(BM.attributes[,1]), value = TRUE)

Rat_orthologs <- getBM(
  attributes = c("external_gene_name","rnorvegicus_homolog_associated_gene_name",
                 "rnorvegicus_homolog_orthology_type","ensembl_gene_id",
                 "rnorvegicus_homolog_ensembl_gene","rnorvegicus_homolog_perc_id",
                 "rnorvegicus_homolog_perc_id_r1","rnorvegicus_homolog_goc_score",
                 "rnorvegicus_homolog_wga_coverage","rnorvegicus_homolog_orthology_confidence"
  ),
  filters = "ensembl_gene_id",
  values = ms_ensemble_id,
  uniqueRows = TRUE,
  mart = ensembl_ms_mart
)
# rnorvegicus_homolog_perc_id - %id. target Rat gene identical to query gene homologs
# rnorvegicus_homolog_perc_id_r1 - %id. query gene identical to target Rat gene homologs
# rnorvegicus_homolog_goc_score - Rat Gene-order conservation score homologs
# rnorvegicus_homolog_wga_coverage - Rat Whole-genome alignment coverage homologs
# rnorvegicus_homolog_orthology_confidence - Rat orthology confidence [0 low, 1 high] homologs
Rat_orthologs_type <- merge(Rat_orthologs, mouse_genes)

write.csv(Rat_orthologs_type,"project\\Rat_orthologous_type.csv", row.names = FALSE)
