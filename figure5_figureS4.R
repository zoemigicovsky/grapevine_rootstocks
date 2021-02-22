library(tidyverse)

all_data <- read_csv("rootstock_data.csv")

all_data_summary <- all_data %>% group_by(Rootstock) %>% summarize_at(vars(SSC:Ravaz_Index), list(median_value=median,mean_value=mean, min_value=min, max_value=max), na.rm=T)

all_data_summary %>% select(Rootstock,Yield_Avg_median_value, Yield_Avg_mean_value) %>% filter(Rootstock=="775 Paulsen"| Rootstock=="Ramsey")
#Rootstock   Yield_Avg_median_value Yield_Avg_mean_value
# 775 Paulsen                    7.8                 8.85
# Ramsey                        13.7                14.1 

(13.7-7.8)/(7.8)*100
#75.64103 So that's a 76% increase in yield

#How different is Ramsey from the median across all rootstock measurements for yield

all_data %>% summarize(median_yield=median(Yield_Avg, na.rm=T))
#12.0

(13.7-12.0)/(12.0)*100
#14.16667 

#So almost 15% better than the overall median across 15 rootstocks

#Calculate percent change for the max value and percent change for the min value from the overall median for each phenotype across all rootstocks 

all_data_reformat <- all_data %>% gather(key="phenotype", value="value", -(Variety:year)) %>% select(Rootstock, phenotype, value)

#Rename phenotypes
all_data_reformat <- all_data_reformat %>% mutate(phenotype=str_replace(phenotype, "ClusterNum_Avg", "Cluster Number"),phenotype=str_replace(phenotype, "Yield_Avg", "Yield"),phenotype=str_replace(phenotype, "BerryWt", "Berry Weight"),phenotype=str_replace(phenotype, "Ravaz_Index", "Ravaz Index"),phenotype=str_replace(phenotype, "Pruning_Avg", "Pruning Weight"))

data_summary_table <- all_data_reformat %>% group_by(phenotype, Rootstock) %>% mutate(median_value=median(value, na.rm=T)) %>% ungroup() %>% select(Rootstock, phenotype, median_value) %>% unique() %>% group_by(phenotype) %>% mutate(max_median=max(median_value, na.rm=T), min_median=min(median_value, na.rm=T), avg_median=mean(median_value, na.rm=T))

data_summary_table_pheno <- data_summary_table %>% select(phenotype, max_median:avg_median) %>% unique() %>% mutate(max_percent_change=((max_median-min_median)/(min_median)*100), avg_percent_change=((max_median-avg_median)/(avg_median)*100))

write_csv(data_summary_table_pheno, "TableS5.csv")

#Calculate median by rootstock by variety
all_data_reformat_var <- all_data %>% gather(key="phenotype", value="value", -(Variety:year)) %>% select(Variety, Rootstock, phenotype, value)

#Rename phenotypes
all_data_reformat_var <- all_data_reformat_var %>% mutate(phenotype=str_replace(phenotype, "ClusterNum_Avg", "Cluster Number"),phenotype=str_replace(phenotype, "Yield_Avg", "Yield"),phenotype=str_replace(phenotype, "BerryWt", "Berry Weight"),phenotype=str_replace(phenotype, "Ravaz_Index", "Ravaz Index"),phenotype=str_replace(phenotype, "Pruning_Avg", "Pruning Weight"))

data_summary_table_var <- all_data_reformat_var %>% group_by(phenotype, Variety, Rootstock) %>% mutate(median_value=median(value, na.rm=T)) %>% ungroup() %>% select(Variety, Rootstock, phenotype, median_value) %>% unique() %>% group_by(Variety, phenotype) %>% mutate(max_median=max(median_value, na.rm=T), min_median=min(median_value, na.rm=T), avg_median=mean(median_value, na.rm=T))

data_summary_table_var <- all_data_reformat_var %>% group_by(phenotype, Variety, Rootstock) %>% mutate(median_value=median(value, na.rm=T), standard_dev_value=sd(value, na.rm=T)) %>% ungroup() %>% select(Variety, Rootstock, phenotype, median_value,standard_dev_value) %>% unique() %>% group_by(Variety, phenotype) %>% mutate(max_median=max(median_value, na.rm=T), min_median=min(median_value, na.rm=T), avg_median=mean(median_value, na.rm=T))

#Try plotting rootstock on x-axis, cab and chard on y-axis (value), facet by phenotype

#Reorder Rootstock as factor so they are ordered by the highest values for each phenotype 

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

variety_palette <- colorblind_pal()(8)[c(6,2)]

library(extrafont)
pdf("figureS4.pdf", width=10, height=12,family="Arial")
data_summary_table_var %>% ggplot(aes(x=reorder_within(Rootstock, median_value, phenotype), y=median_value,colour=Variety))+
  geom_errorbar(aes(ymin = median_value - standard_dev_value, ymax = median_value + standard_dev_value), width=0.3, alpha=0.5)+
  geom_point(alpha=0.5, size=4, stroke=0)+
  theme_few()+
  labs(x = "Rootstock", y="Median Value (+/- Standard Deviation)") + 
  theme(axis.text=element_text(size=12, colour="black"),axis.title=element_text(size=14,face="bold", colour="black"),legend.position = "bottom", axis.text.x= element_text(size = 10, colour="black"),axis.text.y= element_text(size = 10, colour="black"))+
  coord_flip()+
  facet_wrap(~phenotype,ncol=2, scales = "free")+
  scale_color_manual(values = variety_palette)
dev.off()


#Sort phenotypes by percent variance explained

all_pheno_var <- read_csv("all_phenos_variation.csv")
all_pheno_var <- all_pheno_var %>% filter(term=="Rootstock")
data_summary_table_pheno <- data_summary_table_pheno %>% left_join(all_pheno_var) %>% ungroup()

data_summary_table_pheno <- data_summary_table_pheno %>% mutate(phenotype=fct_reorder(as.factor(phenotype), desc(var)))
data_summary_table_pheno <- data_summary_table_pheno %>% gather(key="change", value="value", -phenotype, -term, -var, -p.value)

data_summary_table_pheno %>% filter(change =="max_percent_change") %>% arrange(desc(value))

variety_palette <- colorblind_pal()(8)[c(6,2)]

pdf("figure5.pdf", width=6.5, height=4,family="Arial")
data_summary_table_pheno %>% filter(change %in% c("max_percent_change")) %>% ggplot(aes(y=value, x=fct_reorder(phenotype, value))) +
  geom_col() +
  coord_flip()+
  theme_few()+
  labs(x = "Phenotype", y="% Change From Lowest to Highest Rootstock Median") + 
  theme(axis.text=element_text(size=12, colour="black"),axis.title=element_text(size=14,face="bold", colour="black"),legend.position = "bottom")+
  scale_y_continuous(limits = c(0,100), expand = c(0, 0))
dev.off()