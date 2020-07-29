table=read.csv("project\\best_table.csv", header=TRUE,sep=",")
tab<-split(table,table$class)
library(exactRankTests)

t_test_pvalues=c()
wilcoxon_test_pvalues=c()
for (i in tab){
  for(j in tab){
    t=t.test(i$rnorvegicus_homolog_perc_id, j$rnorvegicus_homolog_perc_id)
    p=t$p.value
    t_test_pvalues=c(t_test_pvalues,p)
    
    t=wilcox.exact(i$rnorvegicus_homolog_perc_id, j$rnorvegicus_homolog_perc_id)
    p=t$p.value
    wilcoxon_test_pvalues=c(wilcoxon_test_pvalues,p)
  }
}

names=c("double","no_eruption","progression","shape","tissue")
t_test_pvalues<-matrix(t_test_pvalues,nrow=5,byrow = TRUE,dimnames = list(names,names))
write.csv(t_test_pvalues,"project\\t_test.csv")

wilcoxon_test_pvalues<-matrix(wilcoxon_test_pvalues,nrow=5,byrow = TRUE,dimnames = list(names,names))
write.csv(wilcoxon_test_pvalues,"project\\wilcoxon_test.csv")

a=as.data.frame(t_test_pvalues)
b=as.data.frame(wilcoxon_test_pvalues)
tests=rbind(a,b)
write.csv(tests,"project\\tests.csv")