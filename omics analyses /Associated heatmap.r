data1 = read.csv("../../代谢/BM.txt",sep = "\t",check.names = F,
                 stringsAsFactors = F,header = T,row.names = 1)
data1 = data1[,-c(1,105,106,107,108,109)]
mapping = read.table("../R_BM_mapping.txt",sep = "\t",check.names = F,stringsAsFactors = F,header = T)
mapping = mapping[mapping$Time==1|mapping$Time==8,]
mapping = mapping[mapping$type=="l",]
data1 = data.frame(t(data1),check.names = F,stringsAsFactors = F)
data1 = data1[mapping$BM_sample,]
row.names(data1) = paste(mapping$sample_num,mapping$Time,sep=  "_")
mapping = read.table("../../R-Med.txt",sep = "\t",check.names = F,
                     stringsAsFactors = F,header = T)
mapping = mapping[mapping$Time==8|mapping$Time==1,]
mapping = mapping[mapping$group_ITT=="l",]
mapping[,2] = paste(mapping$sample_n,mapping$Time,sep = "_")
data1 = data1[mapping[,2],]
filter_mgs = read.table("../l_BM_差异.txt",sep = "\t",check.names = F,
                        stringsAsFactors = F,header = T)
data1 = data1[,filter_mgs[,1]]
mapping = mapping[,c(5:6,8:41,43)]
filter_Med = read.table("filter_Med.txt",sep = "\t",check.names = F,
                        stringsAsFactors = F)
mapping = mapping[,filter_Med[,1]]
corr_1 = function(a,b){
  cor_r = matrix(rep(0,ncol(a)*ncol(b)),ncol(a),ncol(b))
  row.names(cor_r) = colnames(a)
  colnames(cor_r) = colnames(b)
  cor_p = matrix(rep(0,ncol(a)*ncol(b)),ncol(a),ncol(b))
  row.names(cor_p) = colnames(a)
  colnames(cor_p) = colnames(b)
  for(i in 1:ncol(a)){
    for(j in 1:ncol(b)){
      cor_1 = cor.test(as.double(a[,i]),as.double(b[,j]),method = "spearman")
      cor_r[i,j] = cor_1$estimate
      cor_p[i,j] = cor_1$p.value
    }
    print(i)
  }
  k = list(p = cor_p,r = cor_r)
  return(k) 
}
cor_1 = corr_1(data1,mapping)
cor_1_r = cor_1$r
cor_1_p = cor_1$p
library(gplots)
heat_map <- function(x,y){
  x = x[,apply(y,2,min)<=0.05]
  y = y[,apply(y,2,min)<=0.05]
  x = x[apply(y,1,min)<=0.05,]
  y = y[apply(y,1,min)<=0.05,]
  #y = y[,apply(abs(x),2,max)>=0.3]
  #x = x[,apply(abs(x),2,max)>=0.3]
  #y = y[apply(abs(x),1,max)>=0.3,]
  #x = x[apply(abs(x),1,max)>=0.3,]
  y[y<0.001] = "**"
  y[y>0.001&y<0.01] = "*"
  y[y>0.01&y<0.05] = "+"
  y[y>0.05] = ""
  
  p<-heatmap.2(x,col = colorRampPalette(c("#EF3F3A", "white", "#05ADAD"))(20), 
               #split = mtcars$cyl,
               key = TRUE, symkey = FALSE, density.info = "none", 
               trace = "none", cexRow = 0.5,
               main = "Heatmap",cellnote = y,notecol = "black"#,行不聚类,Rowv = F列不聚类,Colv = FALSE,
  )
  return(p)
}
pdf("l_BM_MED_heatmap.pdf",width = 8, height = 10)
heat_map(cor_1_r,cor_1_p)
dev.off()
