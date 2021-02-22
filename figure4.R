
#Correlations between phenotypes
library(tidyverse)
library(ggbeeswarm)
library(paletteer)
library(ggthemes)
library(ape)
library(RColorBrewer)
library(gplots)

all_data <- read_csv("rootstock_data.csv")

#divide into cab and chard to do analysis separately 

final_pheno <-  all_data %>% filter(Variety =="Cabernet Sauvignon")

#Reduce down to only phenotypes

final_pheno <- final_pheno %>% select(SSC:Ravaz_Index)

#reorder phenotypes
final_pheno <- final_pheno %>% select(SSC, TA,  pH,BerryWt,ClusterNum_Avg, Yield_Avg,Pruning_Avg,  Ravaz_Index)
colnames(final_pheno) <- str_replace(colnames(final_pheno), "ClusterNum_Avg", "Cluster Number")
colnames(final_pheno) <- str_replace(colnames(final_pheno), "Yield_Avg", "Yield")
colnames(final_pheno) <- str_replace(colnames(final_pheno), "BerryWt", "Berry Weight")
colnames(final_pheno) <- str_replace(colnames(final_pheno), "Ravaz_Index", "Ravaz Index")
colnames(final_pheno) <- str_replace(colnames(final_pheno), "Pruning_Avg", "Pruning Weight")

final_pheno <- as.data.frame(final_pheno)

#create an empty matrix for correlations, make matrix with no data and just col and rows from final pheno table.
pairwise_pheno_correlations=matrix(,ncol(final_pheno), ncol(final_pheno))
rownames(pairwise_pheno_correlations)=colnames(final_pheno)
colnames(pairwise_pheno_correlations)=colnames(final_pheno)
#marix for pvalues.
pairwise_pheno_correlations_pval=matrix(,ncol(final_pheno), ncol(final_pheno))
rownames(pairwise_pheno_correlations_pval)=colnames(final_pheno)
colnames(pairwise_pheno_correlations_pval)=colnames(final_pheno)

#Treat the data as all quantitative data and run pearson's correlation.
for (i in 1:ncol(final_pheno)) {
  phenoname_x = colnames(final_pheno)[i]
  for (j in 1:ncol(final_pheno)) {
    phenoname_y = colnames(final_pheno)[j]
    pairwise_pheno_correlations[j,i]=cor.test(final_pheno[,i], final_pheno[,j], method = "spearman")$estimate
    pairwise_pheno_correlations_pval[j,i]= cor.test(final_pheno[,i], final_pheno[,j], method = "spearman")$p.value
  }
}

#Bonferroni correct.

pairwise_pheno_correlations_pval[lower.tri(pairwise_pheno_correlations_pval)] = NA
pairwise_pheno_correlations_pval[upper.tri(pairwise_pheno_correlations_pval)] = p.adjust(pairwise_pheno_correlations_pval[upper.tri(pairwise_pheno_correlations_pval)], method = "bonferroni")

#How many comparisons?
length(pairwise_pheno_correlations_pval[lower.tri(pairwise_pheno_correlations_pval)] )
#28
#how many are significant?
table(pairwise_pheno_correlations_pval[upper.tri(pairwise_pheno_correlations_pval)] < 0.05)
#FALSE  TRUE 
#8    20 
#20 / 28 are significant 

pairwise_pheno_correlations[lower.tri(pairwise_pheno_correlations)] = NA

#What are the strongest correlations?
sort(abs(pairwise_pheno_correlations[upper.tri(pairwise_pheno_correlations)]), dec=T)[1:5]
#0.7778840 0.7221881 0.6704898 0.6425950 0.5792502


#Create heat map.

my_palette <- colorRampPalette(c("#0072B2", "white", "#E69F00"))(n = 299)

#make heat map for correlation for cab sauv
pdf("figure4_cs.pdf", width = 8.5, height = 8.5)
mar.default <- c(0,0,0,0)
heatmap.2(pairwise_pheno_correlations,
          col = my_palette,
          key = T,
          keysize = 1,
          symm=T,
          Colv = F,
          Rowv=F,
          trace="none",
          dendrogram="none",
          density.info="none",
          symbreaks = T,
          lmat=rbind( c(4,3,4), c(2,1,3) ),
          lhei=c(1.5, 4),
          lwid=c(1.5,6,0.75),
          colsep = c(0:17),
          rowsep = c(0:17),
          sepcolor="white",
          sepwidth=c(0.05,0.05),
          symkey = T         
)
dev.off()


write.table(pairwise_pheno_correlations_pval, "correlations_p_val_cs.csv", sep=",", row.names=T, quote=F, col.names=T)

write.table(pairwise_pheno_correlations, "correlations_cs.csv", sep=",", row.names=T, quote=F, col.names=T)

#Do same thing for chard

final_pheno <-  all_data %>% filter(Variety =="Chardonnay")

#Reduce down to only phenotypes

final_pheno <- final_pheno %>% select(SSC:Ravaz_Index)

#reorder phenotypes based on clustering
final_pheno <- final_pheno %>% select(SSC, TA,  pH,BerryWt,ClusterNum_Avg, Yield_Avg,Pruning_Avg,  Ravaz_Index)
colnames(final_pheno) <- str_replace(colnames(final_pheno), "ClusterNum_Avg", "Cluster Number")
colnames(final_pheno) <- str_replace(colnames(final_pheno), "Yield_Avg", "Yield")
colnames(final_pheno) <- str_replace(colnames(final_pheno), "BerryWt", "Berry Weight")
colnames(final_pheno) <- str_replace(colnames(final_pheno), "Ravaz_Index", "Ravaz Index")
colnames(final_pheno) <- str_replace(colnames(final_pheno), "Pruning_Avg", "Pruning Weight")

final_pheno <- as.data.frame(final_pheno)

#create an empty matrix for correlations, make matrix with no data and just col and rows from final pheno table.
pairwise_pheno_correlations=matrix(,ncol(final_pheno), ncol(final_pheno))
rownames(pairwise_pheno_correlations)=colnames(final_pheno)
colnames(pairwise_pheno_correlations)=colnames(final_pheno)
#marix for pvalues.
pairwise_pheno_correlations_pval=matrix(,ncol(final_pheno), ncol(final_pheno))
rownames(pairwise_pheno_correlations_pval)=colnames(final_pheno)
colnames(pairwise_pheno_correlations_pval)=colnames(final_pheno)

#Treat the data as all quantitative data and run pearson's correlation.
for (i in 1:ncol(final_pheno)) {
  phenoname_x = colnames(final_pheno)[i]
  for (j in 1:ncol(final_pheno)) {
    phenoname_y = colnames(final_pheno)[j]
    pairwise_pheno_correlations[j,i]=cor.test(final_pheno[,i], final_pheno[,j], method = "spearman")$estimate
    pairwise_pheno_correlations_pval[j,i]= cor.test(final_pheno[,i], final_pheno[,j], method = "spearman")$p.value
  }
}

#Bonferroni correct.

pairwise_pheno_correlations_pval[lower.tri(pairwise_pheno_correlations_pval)] = NA
pairwise_pheno_correlations_pval[upper.tri(pairwise_pheno_correlations_pval)] = p.adjust(pairwise_pheno_correlations_pval[upper.tri(pairwise_pheno_correlations_pval)], method = "bonferroni")

#How many comparisons?
length(pairwise_pheno_correlations_pval[lower.tri(pairwise_pheno_correlations_pval)] )
#28
#how many are significant?
table(pairwise_pheno_correlations_pval[upper.tri(pairwise_pheno_correlations_pval)] < 0.05)
#FALSE  TRUE 
#8    20 
#20 / 28 are significant 

pairwise_pheno_correlations[lower.tri(pairwise_pheno_correlations)] = NA

#What are the strongest correlations?
sort(abs(pairwise_pheno_correlations[upper.tri(pairwise_pheno_correlations)]), dec=T)[1:5]
#0.8145721 0.7507688 0.6840322 0.6538819 0.6337723

#Create heat map.
pdf("figure4_chard.pdf", width = 8.5, height = 8.5)
mar.default <- c(0,0,0,0)
heatmap.2(pairwise_pheno_correlations,
          col = my_palette,
          key = T,
          keysize = 1,
          symm=T,
          Colv = F,
          Rowv=F,
          trace="none",
          dendrogram="none",
          density.info="none",
          symbreaks = T,
          lmat=rbind( c(4,3,4), c(2,1,3) ),
          lhei=c(1.5, 4),
          lwid=c(1.5,6,0.75),
          colsep = c(0:17),
          rowsep = c(0:17),
          sepcolor="white",
          sepwidth=c(0.05,0.05),
          symkey = T         
)
dev.off()

write.table(pairwise_pheno_correlations_pval, "correlations_p_val_chard.csv", sep=",", row.names=T, quote=F, col.names=T)

write.table(pairwise_pheno_correlations, "correlations_chard.csv", sep=",", row.names=T, quote=F, col.names=T)