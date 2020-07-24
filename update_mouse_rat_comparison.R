categories=read.csv("project\\Table_S1.csv", header=TRUE,sep=",")
mouse_rat_comparison=read.csv("project\\mouse_rat_comparison.csv", header=TRUE,sep=",")

class=c()
happlosufficient=c()
for (gene in mouse_rat_comparison[,1]){
  class=c(class,"Don't Know")
  happlosufficient=c(happlosufficient,"")
}
mouse_rat_comparison$class=class 
mouse_rat_comparison$HapploSufficient=happlosufficient
happlo=c("Acvr2a","Bmpr1a","Fgfr2","Lef1","Pax9","Pitx2","Runx2","Shh","Apc","Barx1","Bmp2","Bmp7","Chuk","Eda","Edar","Edaradd","Evc","Fgf10","Fgf20","Fgf3","Foxi3","Jag2","Msx2",
        "Rps6ka3","Smo","Sostdc1")
for (g1 in c(1:87)){
  gene1=mouse_rat_comparison[g1,1]
  gene1=toupper(gene1)
  for (g2 in c(1:187)){
    gene2=categories[g2,1]
    if (gene1==gene2){
      mouse_rat_comparison[g1,8]<-categories[g2,3]
    }
  }
}

for (g in c(1:43)){
  gene=mouse_rat_comparison[g,1]
  if (gene %in% happlo){
    mouse_rat_comparison[g,9]<-"Yes"
  }
  else{
    mouse_rat_comparison[g,9]<-"Yes?"
  }
}
mouse_rat_comparison<-mouse_rat_comparison[,c(1,8,9,2,3,4,5,6,7)]
write.csv(mouse_rat_comparison,"project\\detailed_mouse_rat_comparison.csv", row.names = FALSE)
