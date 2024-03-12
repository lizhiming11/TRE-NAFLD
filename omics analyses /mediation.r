suppressPackageStartupMessages(library(argparse))

parser <- ArgumentParser()

parser$add_argument("-n", help = "input profile_test ")

args <- parser$parse_args()
innum <- file.path(args$n)

#install.packages("mediation")
library(mediation)
MGS = read.table("FLD_SGB.profile",sep = "\t",check.names = F,
                 stringsAsFactors = F,header = T,row.names = 1)
MGS_filter = read.table("filter_SGB.txt",sep = "\t",check.names = F,
                        stringsAsFactors = F,header = T)
MGS = MGS[MGS_filter[,1],]
Diet = read.csv("Med.txt",sep = "\t",check.names = F,
                stringsAsFactors = F,header = T,row.names = 1)
row.names(Diet) = Diet[,1]
Diet_filter = read.table("filter_diet.txt",sep = "\t",check.names = F,
                       stringsAsFactors = F,header = T)
Diet = Diet[,Diet_filter[,1]]
Med = read.table("Med.txt",sep = "\t",check.names = F,
                 stringsAsFactors = F,header = T,row.names = 1)
row.names(Med) = Med[,1]
Med_other = Med[,c(10,39,43)]
Med_filter = read.table("filter_Med.txt",sep = "\t",check.names = F,
                         stringsAsFactors = F,header = T)
Med = Med[,Med_filter[,1]]
Med = Med[!is.na(Med[,1]),]
MGS = data.frame(t(MGS),check.rows = F,stringsAsFactors = F)
Med = Med[row.names(Med)%in%row.names(MGS),]
MGS = MGS[row.names(Med),]
MGS = MGS[,apply(MGS,2,sum)!=0]
Diet = Diet[row.names(Med),]
Med_other = Med_other[row.names(Med),]
data1 = data.frame()
for(i in 1:ncol(Med)){
  for(j in 1:ncol(Diet)){
df = data.frame(y = Med[,i],diet = Diet[,j],
                mgs = MGS[,as.double(innum)],bmi = Med_other$BMI_baseline,sex = Med_other$sex,age = Med_other$age)
df <- na.omit(df)
b = lm(mgs~diet+bmi+sex+age,df)
a = lm(y~diet+mgs+bmi+sex+age,df)

result = mediate(b,a,treat = "diet",mediator = "mgs",
                 boot = T)
m = summary(result)
#plot(result)
n = c(colnames(Diet)[j],colnames(MGS)[as.double(innum)],colnames(Med)[i],m$n0,m$n0.p)
data1 = rbind(data1,n)
}}
 colnames(data1) = c("treat","mediator","result","mediation effects","p.value")
write.table(data1,paste(colnames(MGS)[as.double(innum)],"_diet-BM-Med.txt",sep = ""),sep = "\t",quote = F,row.names = F)
