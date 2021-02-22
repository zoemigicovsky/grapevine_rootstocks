
#Make a four panel figure with berry weight, yield, pruning weight, and ravaz index sorted by rootstock with all the rootstock distributions 

library(tidyverse)
library(ggbeeswarm)
library(paletteer)
library(ggthemes)
library(agricolae)
library(emmeans)

all_data <- read_csv("rootstock_data.csv")

all_data <- all_data %>% mutate(Year2=as.factor(year-min(year))) %>% mutate(Block=as.factor(Block)) %>% mutate_if(is.character, str_replace_all, pattern = "1103 Paulsen", replacement = "'1103 P'") %>% mutate_if(is.character, str_replace_all, pattern = "3309 Couderc", replacement = "'3309 C'") %>% mutate_if(is.character, str_replace_all, pattern = "Schwarzmann", replacement = "'Schwarz'") %>%  mutate_if(is.character, str_replace_all, pattern = "110 Richter", replacement = "'110 R'") %>% mutate_if(is.character, str_replace_all, pattern = "775 Paulsen", replacement = "'775 P'") %>% mutate_if(is.character, str_replace_all, pattern = "Kober 5BB", replacement = "'5BB'") %>% mutate_if(is.character, str_replace_all, pattern = "101-14 MGT", replacement = "'101-14'") %>% mutate_if(is.character, str_replace_all, pattern = "K51-32", replacement = "'K51-32'") %>% mutate_if(is.character, str_replace_all, pattern = "Ramsey", replacement = "'Ramsey'") %>% mutate_if(is.character, str_replace_all, pattern = "Freedom", replacement = "'Freedom'") %>% mutate_if(is.character, str_replace_all, pattern = "Teleki 5C", replacement = "'Teleki 5C'") %>% mutate_if(is.character, str_replace_all, pattern = "SO4", replacement = "'SO4'") %>% mutate_if(is.character, str_replace_all, pattern = "140Ru", replacement = "'140Ru'") %>% mutate_if(is.character, str_replace_all, pattern = "039-16", replacement = "'039-16'") %>% mutate_if(is.character, str_replace_all, pattern = "420 A", replacement = "'420 A'")

plot1 <- all_data %>% filter(!is.na(BerryWt)) %>%
  ggplot(aes(y = BerryWt, x = reorder(Rootstock, BerryWt,FUN=mean))) + 
  geom_quasirandom(alpha = 0.4,size = 1,stroke=0)+
  geom_boxplot(aes(fill=as.factor(Rootstock)), outlier.alpha = 0, alpha=0.4) + 
  theme_bw() + 
  coord_flip() + 
  theme(axis.text = element_text(size = 10, colour = "black", face = "plain"), text = element_text(size = 12, face = "bold"),legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  
  labs(x = "Rootstock", y = "Berry Weight (g)") + 
  ylim(0.5,2.1)

berry_weight_aov <- aov(lm(BerryWt~Year2+Variety+Rootstock+Block+Year2*Rootstock+Variety*Rootstock+Year2*Variety, data=all_data))
tukey_ld2 <- HSD.test(berry_weight_aov,"Rootstock", group=TRUE)

# BerryWt groups
# K51-32       1.428675      a
# Freedom      1.423525     ab
# Ramsey       1.418875     ab
# 110 Richter  1.395675    abc
# 039-16       1.381425   abcd
# 1103 Paulsen 1.371775   abcd
# 420 A        1.359750   abcd
# 3309 Couderc 1.359425   abcd
# Teleki 5C    1.339875    bcd
# 101-14 MGT   1.326150    cde
# 140Ru        1.312075    cde
# Schwarzmann  1.298025     de
# SO4          1.250375     ef
# Kober 5BB    1.247050     ef
# 775 Paulsen  1.197625      f

#get confidence intervals

all_data_reorder <- all_data %>%
  mutate(Rootstock = fct_reorder(Rootstock, BerryWt, .fun = mean, na.rm=T))

berry_weight_model <- lm(BerryWt~Year2+Variety+Rootstock+Block+Year2*Rootstock+Variety*Rootstock+Year2*Variety, data=all_data_reorder)
berry_weight_model_rg <- ref_grid(berry_weight_model)
berry_weight_model_emmeans <- emmeans(berry_weight_model_rg, "Rootstock")

plot1_ci <- plot(berry_weight_model_emmeans,xlab="berry weight", colors = "darkgreen")

plot2 <- all_data %>% filter(!is.na(Yield_Avg)) %>% 
  ggplot(aes(y = Yield_Avg, x = reorder(Rootstock, Yield_Avg,FUN=mean))) + 
  geom_quasirandom(alpha = 0.4,size = 1,stroke=0)+
  geom_boxplot(aes(fill=as.factor(Rootstock)), outlier.alpha = 0, alpha=0.4) + 
  theme_bw() + 
  coord_flip() + 
  theme(axis.text = element_text(size = 10, colour = "black", face = "plain"), text = element_text(size = 12, face = "bold"),legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  
  labs(x = "Rootstock", y = "Yield (Kg)") + 
  ylim(0,30)

yield_avg_aov <- aov(lm(Yield_Avg~Year2+Variety+Rootstock+Block+Variety*Rootstock+Year2*Variety, data=all_data))
tukey_ld2 <- HSD.test(yield_avg_aov,"Rootstock", group=TRUE)

# Yield_Avg groups
# Ramsey       14.077500      a
# 110 Richter  13.609167     ab
# Freedom      13.113333    abc
# K51-32       12.860000   abcd
# 420 A        12.643333  abcde
# 039-16       12.568376  abcde
# 1103 Paulsen 12.534167  abcde
# 101-14 MGT   11.756667  bcdef
# Schwarzmann  11.478704   cdef
# Teleki 5C    11.210000   cdef
# 140Ru        11.146667    def
# 3309 Couderc 11.095000    def
# Kober 5BB    10.783750    efg
# SO4          10.466417     fg
# 775 Paulsen   8.851667      g


all_data_reorder <- all_data %>%
  mutate(Rootstock = fct_reorder(Rootstock, Yield_Avg, .fun = mean, na.rm=T))

yield_model <- lm(Yield_Avg~Year2+Variety+Rootstock+Block+Variety*Rootstock+Year2*Variety, data=all_data_reorder)
yield_model_rg <- ref_grid(yield_model)
yield_model_rg_emmeans <- emmeans(yield_model_rg, "Rootstock")

plot2_ci <- plot(yield_model_rg_emmeans,xlab="yield", colors = "darkgreen")


plot3 <- all_data %>% filter(!is.na(Pruning_Avg)) %>% mutate_if(is.character, str_replace_all, pattern = "1103 Paulsen", replacement = "1103 P") %>% mutate_if(is.character, str_replace_all, pattern = "3309 Couderc", replacement = "3309 C") %>% mutate_if(is.character, str_replace_all, pattern = "Schwarzmann", replacement = "Schwarz") %>%  mutate_if(is.character, str_replace_all, pattern = "110 Richter", replacement = "110 R") %>% mutate_if(is.character, str_replace_all, pattern = "775 Paulsen", replacement = "775 P") %>% mutate_if(is.character, str_replace_all, pattern = "Kober 5BB", replacement = "5BB") %>% mutate_if(is.character, str_replace_all, pattern = "101-14 MGT", replacement = "101-14") %>%
  ggplot(aes(y = Pruning_Avg,x = reorder(Rootstock, Pruning_Avg,FUN=mean))) + 
  geom_quasirandom(alpha = 0.4,size = 1,stroke=0)+
  geom_boxplot(aes(fill=as.factor(Rootstock)), outlier.alpha = 0, alpha=0.4) + 
  theme_bw() + 
  coord_flip() + 
  theme(axis.text = element_text(size = 10, colour = "black", face = "plain"), text = element_text(size = 12, face = "bold"),legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  
  labs(x = "Rootstock", y = "Pruning Weight (Kg)") + 
  ylim(0,7)

pruning_avg_aov <- aov(lm(Pruning_Avg~Year2+Variety+Rootstock+Block+Variety*Rootstock+Year2*Variety, data=all_data))

tukey_ld2 <- HSD.test(pruning_avg_aov,"Rootstock", group=TRUE)
# Pruning_Avg groups
# 1103 Paulsen    1.862417      a
# Ramsey          1.789417      a
# 3309 Couderc    1.678583     ab
# Freedom         1.662833     ab
# 110 Richter     1.586333    abc
# 775 Paulsen     1.483250    bcd
# K51-32          1.442500   bcde
# 039-16          1.427000   bcde
# 101-14 MGT      1.309333   cdef
# 140Ru           1.288667    def
# Schwarzmann     1.221667    def
# Teleki 5C       1.205667    def
# Kober 5BB       1.170833     ef
# SO4             1.169167     ef
# 420 A           1.114417      f


all_data_reorder <- all_data %>%
  mutate(Rootstock = fct_reorder(Rootstock, Pruning_Avg, .fun = mean, na.rm=T))

pruning_model <- lm(Pruning_Avg~Year2+Variety+Rootstock+Block+Variety*Rootstock+Year2*Variety, data=all_data_reorder)
pruning_model_rg <- ref_grid(pruning_model)
pruning_model_rg_emmeans <- emmeans(pruning_model_rg, "Rootstock")

plot3_ci <- plot(pruning_model_rg_emmeans,xlab="pruning weight", colors = "darkgreen")


plot4 <- all_data %>% filter(!is.na(Ravaz_Index)) %>%
  ggplot(aes(y = Ravaz_Index,x = reorder(Rootstock, Ravaz_Index,FUN=mean))) + 
  geom_quasirandom(alpha = 0.4,size = 1,stroke=0)+
  geom_boxplot(aes(fill=as.factor(Rootstock)), outlier.alpha = 0, alpha=0.4) + 
  theme_bw() + 
  coord_flip() + 
  theme(axis.text = element_text(size = 10, colour = "black", face = "plain"), text = element_text(size = 12, face = "bold"),legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  
  labs(x = "Rootstock", y = "Ravaz Index") + 
  ylim(0,45)

ravaz_aov <-  aov(lm(Ravaz_Index~Year2+Variety+Rootstock+Block+Year2*Rootstock+Year2*Variety, data=all_data))
tukey_ld2 <- HSD.test(ravaz_aov,"Rootstock", group=TRUE)

# Ravaz_Index groups
# 420 A          14.891532      a
# SO4            11.928538      b
# 140Ru          11.832329      b
# 101-14 MGT     11.784801      b
# Kober 5BB      11.658065      b
# Teleki 5C      11.402642      b
# 039-16         11.135368      b
# Schwarzmann    10.750981     bc
# K51-32         10.645658    bcd
# 110 Richter    10.212271    bcd
# Freedom        10.166251    bcd
# Ramsey          9.654089   bcde
# 3309 Couderc    8.050638    cde
# 1103 Paulsen    7.924543     de
# 775 Paulsen     6.963197      e

all_data_reorder <- all_data %>%
  mutate(Rootstock = fct_reorder(Rootstock, Ravaz_Index, .fun = mean, na.rm=T))

ravaz_model <-lm(Ravaz_Index~Year2+Variety+Rootstock+Block+Year2*Rootstock+Year2*Variety, data=all_data_reorder)
ravaz_model_rg <- ref_grid(ravaz_model)
ravaz_model_rg_emmeans <- emmeans(ravaz_model_rg, "Rootstock")

plot4_ci <- plot(ravaz_model_rg_emmeans,xlab="Ravaz index", colors = "darkgreen")

require(cowplot)
pdf("figure3.pdf", width=6.5, height=8.25)
plot_grid( plot2,plot1, plot3, plot4, nrow = 2,labels="AUTO")
dev.off()


require(cowplot)
pdf("figureS3.pdf", width=6.5, height=8.25)
plot_grid(plot2_ci,plot1_ci, plot3_ci, plot4_ci, nrow = 2,labels="AUTO")
dev.off()