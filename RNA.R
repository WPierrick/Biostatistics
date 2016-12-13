setwd("F:/A trier")

#library(readxl)
#install.packages(readODS)
#install.packages('VennDiagram')
library(VennDiagram)
library("readODS", lib.loc="~/R/win-library/3.2")

data_memzach <- readODS::read_ods("projet_fariza_tools2.ods", sheet = 3)
data_ivanov <- readODS::read_ods("projet_fariza_tools.ods", sheet = 4)
data_CIRI <- readODS::read_ods("projet_fariza_tools.ods", sheet = 5)
data_findcirc <- readODS::read_ods("projet_fariza_tools.ods", sheet = 6)
data_circrnafind <- readODS::read_ods("projet_fariza_tools.ods", sheet = 7)
data_DCC <- readODS::read_ods("projet_fariza_tools.ods", sheet = 8)

data_CIRI$length <- as.numeric(data_CIRI$circRNA_end) - as.numeric(data_CIRI$circRNA_start)
data_findcirc$length <- as.numeric(data_findcirc$end) - as.numeric(data_findcirc$start)
data_circrnafind$length <- as.numeric(data_circrnafind$end) - as.numeric(data_circrnafind$start)
data_DCC$length <- as.numeric(data_DCC$end) - as.numeric(data_DCC$start)

#Genomic length into length
data_ivanov$length <- as.numeric(data_ivanov$`genomic length`)
data_memzach$length <- as.numeric(data_memzach$`genomic length`)

#Outliers
outliermemzach <- data_memzach$length[ which(data_memzach$length> 8000)]
outlierivanov <- data_ivanov$length[ which(data_ivanov$length> 8000)]
outlierCIRI <- data_CIRI$length[ which(data_CIRI$length> 8000)]
outlierFindCIrc <- data_findcirc$length[ which(data_findcirc$length > 8000)]
outliercircrnafind <- data_circrnafind$length[ which(data_circrnafind$length > 8000)]
outlierDCC <- data_DCC$length[ which(data_DCC$length > 8000)]

dlist <- list(outliermemzach, outlierivanov, outlierCIRI , outlierFindCIrc ,outliercircrnafind ,outlierDCC )
datalist <- list(data_memzach, data_ivanov, data_CIRI, data_findcirc, data_circrnafind, data_DCC)

#Percentage of outliers removed at 8000 in each dataset
for (i in dlist){
  percount <- as.numeric(length(i)) / nrow(datalist[i])
}

colnames(data_findcirc)[1] <- "chr"
colnames(data_CIRI) [4] <- "end"

#Moyenne
moy_tools <- c(mean(as.numeric(data_memzach$length[ which(data_memzach$length < 8000)])),
               mean(as.numeric(data_ivanov$length[ which(data_ivanov$length < 8000)]), 
               mean(as.numeric(data_CIRI$length[ which(data_CIRI$length < 8000)])), 
               mean(as.numeric(data_findcirc$length[ which(data_findcirc$length < 8000)])), 
               mean(as.numeric(data_circrnafind$length[ which(data_circrnafind$length < 8000)])), 
               mean(as.numeric(data_DCC$length[ which(data_DCC$length < 8000)]))))

barplot(moy_tools, names.arg = c("Memzach", "Ivanov", "CIRI", "Find_circ", "Circrnafind", "DCC"), main = "Average length of predicted sequences (< 8000bp)")

hist(data_findcirc$length[ which(data_findcirc$length < 5000)], breaks = 100)


#Ecart type
var_tools <- c(sd(as.numeric(data_memzach$length[ which(data_memzach$length < 8000)])),
sd(as.numeric(data_ivanov$length[ which(data_ivanov$length < 8000)])), 
sd(as.numeric(data_CIRI$length[ which(data_CIRI$length < 8000)])), 
sd(as.numeric(data_findcirc$length[ which(data_findcirc$length < 8000)])), 
sd(as.numeric(data_circrnafind$length[ which(data_circrnafind$length < 8000)])), 
sd(as.numeric(data_DCC$length[ which(data_DCC$length < 8000)])))
               
               
barplot(var_tools, names.arg = c("Memzach", "Ivanov", "CIRI", "Find_circ", "Circrnafind", "DCC"), main = "Average standard deviation of predicted sequences (< 8000 bp)")


barplot(c(725,757,279,780,2803,6461), names.arg = c("Memzach", "Ivanov", "CIRI", "Find_circ", "Circrnafind", "DCC"), main = "Number of predicted sequences per tool")


#Chromosome count
#Moyenne de chr pour CIRI
barplot(table(data_CIRI$chr[which(data_CIRI$length < 8000)]), main = "Distribution of predicted circRNA per chromosome (CIRI)")
meanchromoCIRI <- list()
chromo <- list('I', 'II', 'III', 'IV', 'V', 'X')
for (i in chromo) {
  meanchromoCIRI[i] <- list(mean(data_CIRI$length[which(data_CIRI$chr == i)]))
}
barplot(unlist(meanchromoCIRI), main = "Average length of predicted circRNA per chromosome (CIRI)")

#Findcirc
barplot(table(data_findcirc$chrom[which(data_findcirc$length <8000)]), main = "Distribution of predicted circRNA per chromosome (FindCirc)")
meanchromoFINDCIRC <- list()
chromo <- list('I', 'II', 'III', 'IV', 'V', 'X')
for (i in chromo) {
  meanchromoFINDCIRC[i] <- list(mean(data_findcirc$length[which(data_findcirc$chrom == i & data_findcirc$length < 8000)]))
}
barplot(unlist(meanchromoFINDCIRC), main = "Average length of predicted circRNA per chromosome (findCirc)")

#CircRNAFind
barplot(table(data_circrnafind$chrom[which(data_findcirc$length <8000)]), main = "Distribution of predicted circRNA per chromosome (CircRNAFind)")
meanchromoCircRNAFind <- list()
chromo <- list('I', 'II', 'III', 'IV', 'V', 'X')
for (i in chromo) {
  meanchromoCircRNAFind[i] <- list(mean(data_circrnafind$length[which(data_circrnafind$chrom == i & data_circrnafind$length < 8000)]))
}
barplot(unlist(meanchromoCircRNAFind), main = "Average length of predicted circRNA per chromosome (CircRNAFind)")

#DCC
barplot(table(data_DCC$chrom[which(data_DCC$length <8000)]), main = "Distribution of predicted circRNA per chromosome (DCC)")
meanchromoDCC <- list()
chromo <- list('I', 'II', 'III', 'IV', 'V', 'X')
for (i in chromo) {
  meanchromoDCC[i] <- list(mean(data_DCC$length[which(data_DCC$chrom == i & data_DCC$length < 8000)]))
}
barplot(unlist(meanchromoDCC), main = "Average length of predicted circRNA per chromosome (DCC)")


#Memzach
data_memzach$chrom <- read.table("clipboard")
data_memzach2 <- read.csv("memzach.csv", header = FALSE)
barplot(table(data_memzach$chrom$V1[which(data_memzach$length <8000)]), main = "Distribution of predicted circRNA per chromosome (Memzach)")
meanchromomem <- list()
chromo <- list('I', 'II', 'III', 'IV', 'V', 'X')
for (i in chromo) {
  meanchromomem[i] <- list(mean(data_memzach$length[which(data_memzach$chrom$V1 == i & data_memzach$length < 8000)]))
}
barplot(unlist(meanchromomem), main = "Average length of predicted circRNA per chromosome (Memzach)")


#Ivanov
data_ivanov2<- read.table("clipboard")
data_ivanov$chrom <- data_ivanov2$V1
barplot(table(data_ivanov$chrom[which(data_ivanov$length <8000)]), main = "Distribution of predicted circRNA per chromosome (Ivanov)")
meanchromoiva <- list()
chromo <- list('I', 'II', 'III', 'IV', 'V', 'X')
for (i in chromo) {
  meanchromoiva[i] <- list(mean(data_ivanov$length[which(data_ivanov$chrom == i & data_ivanov$length < 8000)]))
}
barplot(unlist(meanchromoiva), main = "Average length of predicted circRNA per chromosome (Ivanov)")


#Venn
grid.newpage()
draw.pairwise.venn(50, 40, 50, 11, category = c("Dog People", "Cat People"), lty = rep("blank", 2), fill = c("light blue", "pink"), alpha = rep(0.5, 2), cat.pos = c(0,0), cat.dist = rep(0.025, 2), scaled = FALSE)
grid.newpage()
draw.triple.venn(8, 6, 3, n12 = 5, n23 = 2, n13 = 2, n123 = 1,category = c("Computer people", "Biology people", "Stats people"), lty = "blank", fill = c("light blue", "pink", "green"))

total <- rbind(data_ivanov, data_memzach)
total2 <- rbind(total, data_findcirc, fill = TRUE)
?rbind


write.csv(data_ivanov, file =  "ivanov.csv", sep = tabl)
write.csv(data_memzach, file =  "memzach.csv")

myfunc <- function(chr1, chr2, start1, start2, end1, end2) {
  if (chr1 == chr2 && (as.numeric(start1) == as.numeric(start2) +10 || as.numeric(start1) ==  as.numeric(start2) - 10)  &&  ( as.numeric(end1) ==  as.numeric(end2) +10 ||  as.numeric(end1) ==  as.numeric(end2) - 10)){
    return (TRUE)
  }
  return (FALSE)
}

as.numeric

myfunc(data_DCC$chrom[2], data_circrnafind$chrom[3], data_DCC$start[2], data_circrnafind$start[3], data_DCC$end[2], data_circrnafind$end[3])

for (i in 1:nrow(data_DCC)){
  for (j in 1:nrow(data_circrnafind)){
    myfunc(data_DCC$chrom[i], data_circrnafind$chrom[j],as.numeric(data_DCC$start[i]), as.numeric(data_circrnafind$start[j]), as.numeric(data_DCC$end[i]), as.numeric(data_circrnafind$end[j]))}}


#DiagvennCIRI
data_CIRI$diagvenn<-c()
for (i in 1:nrow(data_CIRI))
  {
  flag = TRUE
  for (j in 1:nrow(data_memzach))
    {
    if (myfunc(data_CIRI$chr[i], data_memzach$chr[j], data_CIRI$start[i], data_memzach$start[j], data_CIRI$end[i], data_memzach$end[j]))
      {
      data_CIRI$diagvenn[i]<-paste("chr",data_memzach$chr[j],":",data_memzach$start[j],"-", data_memzach$end[j])
      flag = FALSE
      }
    } 
  if (flag)
  {
    data_CIRI$diagvenn[i]<-paste("chr", data_CIRI$chr[i],":",data_CIRI$start[i],"-", data_CIRI$end[i])
  }
}
write.csv(data_CIRI, file = "ciri.csv")


#DiagvennFindCirc
data_findcirc$diagvenn<-c()
for (i in 1:nrow(data_CIRI))
{
  flag = TRUE
  for (j in 1:nrow(data_findcirc))
  {
    if (myfunc(data_findcirc$chr[i], data_CIRI$chr[j], data_findcirc$start[i], data_CIRI$start[j], data_findcirc$end[i], data_CIRI$end[j]))
    {
      data_findcirc$diagvenn[i]<-paste("chr",data_findcirc$chr[j],":",data_findcirc$start[j],"-", data_findcirc$end[j])
      flag = FALSE
    }
  } 
  if (flag)
  {
    data_findcirc$diagvenn[i]<-paste("chr", data_CIRI$chr[i],":",data_CIRI$start[i],"-", data_CIRI$end[i])
  }
}
write.csv(data_CIRI, file = "findcirc.csv")
















