library(fdrtool)
library(ggraph)
library(igraph)
library(tidyverse)

merge_cor<-function(m,m_name,n_name){
  m = read.table(m,sep = "\t",check.names = F,
                 stringsAsFactors = F,header = T)
  #m$q = p.adjust(m$p,method = "fdr")
  m = m[m$p<0.01,]
  try(
    {m$type1 = m_name},silent=TRUE)
  try({m$type2 = n_name},silent=TRUE)
  return(m)
}

BM_diet_e = merge_cor("diet_BM_cor_e.txt","diet","BM")
BM_Med_e = merge_cor("BM_Med_cor_e.txt","BM","Med")
BM_MGS_e = merge_cor("MGS_BM_cor_e.txt","MGS","BM")
MGS_diet_e = merge_cor("diet_MGS_cor_e.txt","diet","MGS")
MGS_Med_e = merge_cor("MGS_Med_cor_e.txt","MGS","Med")
diet_Med_e = merge_cor("diet_Med_cor_e.txt","diet","Med")

BM_diet_l = merge_cor("diet_BM_cor_l.txt","diet","BM")
BM_Med_l = merge_cor("BM_Med_cor_l.txt","BM","Med")
BM_MGS_l = merge_cor("MGS_BM_cor_l.txt","MGS","BM")
MGS_diet_l = merge_cor("diet_MGS_cor_l.txt","diet","MGS")
MGS_Med_l = merge_cor("MGS_Med_cor_l.txt","MGS","Med")
diet_Med_l = merge_cor("diet_Med_cor_l.txt","diet","Med")

filter_data = read.table("差异指标.txt",sep = "\t",check.names = F,stringsAsFactors = F,
                         header = T)
filter_data = c(filter_data[,1],filter_data[,2],filter_data[,3],filter_data[,4])
all_f_l = rbind(BM_diet_l,BM_Med_l,BM_MGS_l,MGS_diet_l,MGS_Med_l,diet_Med_l)
all_f_e = rbind(BM_diet_e,BM_Med_e,BM_MGS_e,MGS_diet_e,MGS_Med_e,diet_Med_e)
all_f_l = all_f_l[all_f_l[,1]%in%filter_data&all_f_l[,2]%in%filter_data,]
all_f_e = all_f_e[all_f_e[,1]%in%filter_data&all_f_e[,2]%in%filter_data,]
all_f_l$name = paste(all_f_l$Var1,all_f_l$Var2,sep = "_")
all_f_e$name = paste(all_f_e$Var1,all_f_e$Var2,sep = "_")
#all_f_l_share = all_f_l[all_f_l$name%in%all_f_e$name,]
#all_f_e_share = all_f_e[all_f_e$name%in%all_f_l$name,]

#write.table(all_f_l_share,"all_f_l_share.txt",sep = "\t",quote = F,row.names = F)
#write.table(all_f_e_share,"all_f_e_share.txt",sep = "\t",quote = F,row.names = F)
all_f = all_f_e[!all_f_e$name%in%all_f_e_share$name,]
all_f = all_f[,-8]
#all_f = rbind(BM_diet_l,BM_Med_l,BM_MGS_l,MGS_diet_l,MGS_Med_l,diet_Med_l)
#filter_data = read.table("差异指标.txt",sep = "\t",check.names = F,stringsAsFactors = F,
#                         header = T)
#filter_data = c(filter_data[,1],filter_data[,2],filter_data[,3],filter_data[,4])
#all_f = all_f[all_f[,1]%in%filter_data&all_f[,2]%in%filter_data,]
meta_m_1 = all_f[,c(1,6)]
meta_m_2 = all_f[,c(2,7)]
colnames(meta_m_1) = c("ID","Group")
colnames(meta_m_2) = c("ID","Group")
meta_m = rbind(meta_m_1,meta_m_2)
meta_m = meta_m[!duplicated(meta_m$ID),]
all_f$Direct = "NEG"
colnames(all_f) = c("ID","variable","value","pvalue","lab","qvalue","qlab","Direct")
all_f$lab = "+"
all_f$qvalue = all_f$pvalue
all_f$qlab = "+"
all_f$Direct[all_f$value>0] = "POS"
all_f$ID <- as.character(all_f$ID)
all_f$variable <- as.character(all_f$variable)
Net <- all_f[,c('ID','variable','Direct')]

d1 <- data.frame(ID=rep('org',4),variable=c('diet','MGS','BM',"Med"),stringsAsFactors = F)
d2 <- data.frame(variable= unique(c(all_f$ID,all_f$variable)),stringsAsFactors = F)
d2 <- merge(d2,meta_m,all.x = T,by.x='variable',by.y = 'ID')
d2$Group <- as.character(d2$Group)
d2 <- d2[order(d2$Group),]
colnames(d2)[2] <- 'ID'

he <- rbind.data.frame(d1,d2,stringsAsFactors = F)

dat <- data.frame(ID=unique(c(he$ID,he$variable)),stringsAsFactors = F)
node <- merge(dat,meta_m,by='ID',all.x = T)
node <- node[match(dat$ID,node$ID),]
node$Group <- as.character(node$Group)

#Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the labels
f = which(!is.na(node$Group))
node$angle[f]= 90 - 360 * seq(1,length(f)) / length(f)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
node$hjust<-ifelse(node$angle < -90, 1, 0)

# flip angle BY to make them readable
node$angle<-ifelse(node$angle < -90, node$angle+180, node$angle)

#node$Group[2:5] <- 'org' 
row.names(node) <- 1:nrow(node)

node$ID <- factor(node$ID,node$ID)
node$Group <- factor(node$Group,c('BM','MGS',"Med","diet"))

Net$Direct <- factor(Net$Direct,c('POS','NEG'))
mygraph <- graph_from_data_frame(d=he, vertices=node)
#V(mygraph)$class <- as.character(node$ID)


From = match(Net$ID,node$ID)
To = match(Net$variable,node$ID)
Col = as.character(Net$Direct)
From_pos = From[Col=="POS"]
To_pos = To[Col=="POS"]
From_neg = From[Col=="NEG"]
To_neg = To[Col =="NEG"]

ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
    geom_conn_bundle(data = get_con(from = From_pos, to = To_pos),aes(colour="#4a89b6"), alpha=1, tension = 0.8) + 
    geom_conn_bundle(data = get_con(from = From_neg, to = To_neg),aes(colour="#e62425"), alpha=1, tension = 0.8) + 
    #geom_node_point(aes(filter = leaf, x = x*1.01, y=y*1.01)) +
    geom_node_text(aes(x = x*1.01, y=y*1.01, filter = leaf, label=name, angle = angle, hjust=hjust,colour=Group),size=0.8)+
    scale_edge_color_manual(values = c('#4a89b6','#e62425'))+
    scale_color_manual(values = c('#bfcb1a','#18aab4','#a4519a','#e6262f'))+
    guides(fill=F,color=F)+
    theme_void()+
    theme(
      plot.margin = unit(c(6, 6, 6, 6), "lines")
    )

sum(all_f$Direct=="POS")
sum(all_f$Direct=="NEG")
