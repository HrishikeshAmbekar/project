mouse_rat_comparison=read.csv("project\\detailed_mouse_rat_comparison.csv", header=TRUE,sep=",")
rat_orthologous_type=read.csv("project\\Rat_orthologous_type.csv", header=TRUE,sep=",")

#col_names<-c(gene_name,class,HapploSufficient,mouse)
d<-cbind(mouse_rat_comparison$gene_name, rat_orthologous_type$rnorvegicus_homolog_perc_id)
table1<-mouse_rat_comparison[,c(1,2,3,5,6,8,9)]
table1<-table1[order(table1$gene_name),]
table2<-rat_orthologous_type[,c(6,7,8,9,10)]
table<-cbind(table1,table2)
table<-table[order(table$class ,table$HapploSufficient),]
write.csv(table,"project\\best_table.csv", row.names = FALSE)