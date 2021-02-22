library(tidyverse)
library(ggthemes)
library(lmerTest)
library(broom)
library(agricolae)
library(viridis)

all_data <- read_csv("rootstock_data.csv")

all_data <- all_data %>% mutate(Year2=as.factor(year-min(year))) %>% mutate(Block=as.factor(Block))

#Models have been optimized for analysis 

#TA  

ta_aov <- aov(lm(TA~Year2+Variety+Rootstock+Block+Year2*Variety, data=all_data))

ta_model_results <- tidy(ta_aov)
write.table(ta_model_results, "ta_model_results.csv", col.names=T, row.names = F, quote=F, sep=",")

ta_model_tukey <- tidy(TukeyHSD(ta_aov))
write.table(ta_model_tukey, "ta_tukey_results.csv", col.names=T, row.names = F, quote=F, sep=",")

#SSC 

SSC_aov <- aov(lm(SSC~Year2+Variety+Rootstock+Block+Year2*Rootstock+Variety*Rootstock+Year2*Variety, data=all_data))

SSC_model_results <- tidy(SSC_aov)
write.table(SSC_model_results, "SSC_model_results.csv", col.names=T, row.names = F, quote=F, sep=",")

SSC_model_tukey <- tidy(TukeyHSD(SSC_aov))
write.table(SSC_model_tukey, "SSC_tukey_results.csv", col.names=T, row.names = F, quote=F, sep=",")

#PH RESULTS

ph_aov <- aov(lm(pH~Year2+Variety+Rootstock+Block+Variety*Rootstock+Year2*Variety, data=all_data))

ph_model_results <- tidy(ph_aov)
write.table(ph_model_results, "ph_model_results.csv", col.names=T, row.names = F, quote=F, sep=",")

ph_model_tukey <- tidy(TukeyHSD(ph_aov))
write.table(ph_model_tukey, "ph_model_tukey.csv", col.names=T, row.names = F, quote=F, sep=",")

#BERRY WEIGHT

berry_weight_aov <- aov(lm(BerryWt~Year2+Variety+Rootstock+Block+Year2*Rootstock+Variety*Rootstock+Year2*Variety, data=all_data))

berry_wt_model_results <- tidy(berry_weight_aov)
write.table(berry_wt_model_results, "berry_wt_model_results.csv", col.names=T, row.names = F, quote=F, sep=",")

berry_wt_model_tukey <- tidy(TukeyHSD(berry_weight_aov))
write.table(berry_wt_model_tukey, "berry_wt_model_tukey.csv", col.names=T, row.names = F, quote=F, sep=",")

#YIELD 

yield_avg_aov <- aov(lm(Yield_Avg~Year2+Variety+Rootstock+Block+Variety*Rootstock+Year2*Variety, data=all_data))


yield_avg_model_tukey <- tidy(TukeyHSD(yield_avg_aov))

yield_avg_model_results <- tidy(yield_avg_aov)
write.table(yield_avg_model_results, "yield_model_results.csv", col.names=T, row.names = F, quote=F, sep=",")

yield_avg_model_tukey <- tidy(TukeyHSD(yield_avg_aov))
write.table(yield_avg_model_tukey, "yield_model_tukey.csv", col.names=T, row.names = F, quote=F, sep=",")

#CLUSTER NUMBER 

cluster_num_avg_aov <- aov(lm(ClusterNum_Avg~Year2+Variety+Rootstock+Block+Year2*Rootstock+Variety*Rootstock+Year2*Variety, data=all_data))


cluster_num_avg_model_results <- tidy(cluster_num_avg_aov)
write.table(cluster_num_avg_model_results, "cluster_model_results.csv", col.names=T, row.names = F, quote=F, sep=",")

cluster_num_avg_model_tukey <- tidy(TukeyHSD(cluster_num_avg_aov))
write.table(cluster_num_avg_model_tukey, "cluster_model_tukey.csv", col.names=T, row.names = F, quote=F, sep=",")

#PRUNING WEIGHT 

pruning_avg_aov <- aov(lm(Pruning_Avg~Year2+Variety+Rootstock+Block+Variety*Rootstock+Year2*Variety, data=all_data))

pruning_avg_model_results <- tidy(pruning_avg_aov)
write.table(pruning_avg_model_results, "pruning_model_results.csv", col.names=T, row.names = F, quote=F, sep=",")

pruning_avg_model_tukey <- tidy(TukeyHSD(pruning_avg_aov))
write.table(pruning_avg_model_tukey, "pruning_model_tukey.csv", col.names=T, row.names = F, quote=F, sep=",")

#RAVAZ INDEX

ravaz_aov <- aov(lm(Ravaz_Index~Year2+Variety+Rootstock+Block+Year2*Rootstock+Year2*Variety, data=all_data))

ravaz_model_results <- tidy(ravaz_aov)
write.table(ravaz_model_results, "ravaz_model_results.csv", col.names=T, row.names = F, quote=F, sep=",")

ravaz_model_tukey <- tidy(TukeyHSD(ravaz_aov))
write.table(ravaz_model_tukey, "ravaz_tukey.csv", col.names=T, row.names = F, quote=F, sep=",")

#Calculate the percent variation explained by each rootstock in each model

SSC_model <- read_csv("SSC_model_results.csv")
SSC_var <- SSC_model %>% mutate(total_sum=sum(SSC_model$sumsq)) %>% filter(term !="Residuals") %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="SSC") %>% select(phenotype, term, var, p.value) 

ta_model <- read_csv("ta_model_results.csv")
ta_var <- ta_model %>% filter(term !="Residuals") %>% mutate(total_sum=sum(ta_model$sumsq)) %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="TA") %>% select(phenotype, term, var, p.value)

ph_model <- read_csv("ph_model_results.csv")
ph_var <- ph_model %>% mutate(total_sum=sum(ph_model$sumsq)) %>% filter(term !="Residuals") %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="pH") %>% select(phenotype, term, var, p.value) 

berry_model <- read_csv("berry_wt_model_results.csv")
berry_var <- berry_model %>% mutate(total_sum=sum(berry_model$sumsq)) %>% filter(term !="Residuals")  %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="Berry Weight") %>% select(phenotype, term, var, p.value) 

yield_model <- read_csv("yield_model_results.csv")
yield_var <- yield_model %>% mutate(total_sum=sum(yield_model$sumsq)) %>% filter(term !="Residuals") %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="Yield") %>% select(phenotype, term, var, p.value)

cluster_model <- read_csv("cluster_model_results.csv")
cluster_var <- cluster_model %>% mutate(total_sum=sum(cluster_model$sumsq)) %>% filter(term !="Residuals")  %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="Cluster Number") %>% select(phenotype, term, var, p.value) 

pruning_model <- read_csv("pruning_model_results.csv")
pruning_var <- pruning_model %>% mutate(total_sum=sum(pruning_model$sumsq))  %>% filter(term !="Residuals") %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="Pruning Weight") %>% select(phenotype, term, var, p.value)  

ravaz_model <- read_csv("ravaz_model_results.csv")
ravaz_var <- ravaz_model %>% mutate(total_sum=sum(ravaz_model$sumsq)) %>% filter(term !="Residuals")  %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="Ravaz Index") %>% select(phenotype, term, var, p.value) 

#Merge all data for results table 
all_pheno_var <- bind_rows(SSC_var, ta_var, ph_var, berry_var, yield_var, cluster_var, pruning_var, ravaz_var)
all_pheno_var <- all_pheno_var %>% mutate_if(is.character, str_replace_all, pattern = "Year2", replacement = "Year") 
write.table(all_pheno_var, "all_phenos_variation.csv", sep=",", col.names=T, row.names = F, quote=F)

#Add in empty columns for heatmap

SSC_model <- read_csv("SSC_model_results.csv")
SSC_var <- SSC_model %>% mutate(total_sum=sum(SSC_model$sumsq)) %>% filter(term !="Residuals") %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="SSC") %>% select(phenotype, term, var, p.value) 

ta_model <- read_csv("ta_model_results.csv")
ta_var <- ta_model %>% filter(term !="Residuals") %>% mutate(total_sum=sum(ta_model$sumsq)) %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="TA") %>% select(phenotype, term, var, p.value) %>% add_row(phenotype = "TA", term = "Year2:Rootstock", var = 0, p.value =1)  %>% add_row(phenotype = "TA", term = "Variety:Rootstock", var = 0, p.value =1)

ph_model <- read_csv("ph_model_results.csv")
ph_var <- ph_model %>% mutate(total_sum=sum(ph_model$sumsq)) %>% filter(term !="Residuals") %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="pH") %>% select(phenotype, term, var, p.value) %>% add_row(phenotype = "pH", term = "Year2:Rootstock", var = 0, p.value =1) 

berry_model <- read_csv("berry_wt_model_results.csv")
berry_var <- berry_model %>% mutate(total_sum=sum(berry_model$sumsq)) %>% filter(term !="Residuals")  %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="Berry Weight") %>% select(phenotype, term, var, p.value) 

yield_model <- read_csv("yield_model_results.csv")
yield_var <- yield_model %>% mutate(total_sum=sum(yield_model$sumsq)) %>% filter(term !="Residuals") %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="Yield") %>% select(phenotype, term, var, p.value) %>% add_row(phenotype = "Yield", term = "Year2:Rootstock", var = 0, p.value =1) 

cluster_model <- read_csv("cluster_model_results.csv")
cluster_var <- cluster_model %>% mutate(total_sum=sum(cluster_model$sumsq)) %>% filter(term !="Residuals")  %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="Cluster Number") %>% select(phenotype, term, var, p.value) 

pruning_model <- read_csv("pruning_model_results.csv")
pruning_var <- pruning_model %>% mutate(total_sum=sum(pruning_model$sumsq))  %>% filter(term !="Residuals") %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="Pruning Weight") %>% select(phenotype, term, var, p.value)  %>% add_row(phenotype = "Pruning Weight", term = "Year2:Rootstock", var = 0, p.value =1)

ravaz_model <- read_csv("ravaz_model_results.csv")
ravaz_var <- ravaz_model %>% mutate(total_sum=sum(ravaz_model$sumsq)) %>% filter(term !="Residuals")  %>% select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="Ravaz Index") %>% select(phenotype, term, var, p.value) %>% add_row(phenotype = "TA", term = "Variety:Rootstock", var = 0, p.value =1)  

all_pheno_var <- bind_rows(SSC_var, ta_var, ph_var, berry_var, yield_var, cluster_var, pruning_var, ravaz_var)
all_pheno_var <- all_pheno_var %>% mutate_if(is.character, str_replace_all, pattern = "Year2", replacement = "Year") 

all_pheno_var %>% filter(term=="Rootstock") %>% arrange(desc(var))
all_pheno_var %>% filter(term=="Year") %>% arrange(desc(var))

#Show how much variation can be explained for year, variety and rootstock for each trait 

#Remove Block data because it's not of interest
all_pheno_var <- all_pheno_var %>% filter(term != "Block")
#Reorder factor for terms
all_pheno_var <- all_pheno_var %>% mutate(term=as_factor(term)) 
all_pheno_var$term <- fct_relevel(all_pheno_var$term, "Rootstock", "Variety", "Year","Variety:Rootstock", "Year:Variety", "Year:Rootstock")

pdf("figure2.pdf", width=8, height=4)
all_pheno_var %>% filter(p.value < 0.05) %>% ggplot(aes(y=fct_reorder2(phenotype, desc(term), desc(var)), x=term))+
  geom_tile(aes(fill=var), color="white", size=1)+
  geom_text(aes(label=paste(desc(-round(var, digits=2)),"%")), color="gray65", fontface="bold")+
  theme_few()+
  labs(x = "Factor", y="Phenotype") + 
  theme(axis.text=element_text(size=12, colour="black"),axis.title=element_text(size=14,face="bold", colour="black"),legend.position = "bottom")+
  scale_x_discrete(position="top")+
  scale_fill_viridis(option="inferno",direction=-1,  name="% variance explained",limits=c(0, 55))
dev.off()