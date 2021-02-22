library(tidyverse)
library(ggthemes)
library(ggbeeswarm)

all_data <- read_csv("rootstock_data.csv")

all_data <- all_data %>% mutate(Variety=gsub("Chardonnay", "'Chardonnay'", Variety),Variety=gsub("Cabernet Sauvignon", "'Cabernet Sauvignon'", Variety))

plot1 <- ggplot(all_data, aes (x = year, y=SSC, color=Rootstock)) + 
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Soluble Solid Content (°Brix)", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))

plot2 <- ggplot(all_data, aes (x = year, y=TA, color=Rootstock)) + 
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Titratable Acidity (g/L)", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))

plot3 <- ggplot(all_data, aes (x = year, y=pH, color=Rootstock)) + 
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="pH", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))


plot4 <- ggplot(all_data, aes (x = year, y=BerryWt, color=Rootstock)) + 
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Berry Weight (g)", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))

plot5 <- ggplot(all_data, aes (x = year, y=ClusterNum_Avg, color=Rootstock)) + 
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Cluster Number", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))


plot6 <- ggplot(all_data, aes (x = year, y=Yield_Avg, color=Rootstock)) + 
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Yield (kg)", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))

plot7 <- ggplot(all_data, aes (x = year, y=Pruning_Avg, color=Rootstock)) + 
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Pruning Weight (kg)", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))

plot8 <- ggplot(all_data, aes (x = year, y=Ravaz_Index, color=Rootstock)) + 
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Ravaz Index", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))

require(cowplot)

pdf("figure1.pdf", width=10, height=13)
plot_grid(plot1, plot2, plot3, plot4,plot5,plot6,plot7, plot8, ncol = 2)
dev.off()

#Plot with individual points

plot1 <- ggplot(all_data, aes (x = year, y=SSC, color=Rootstock)) + 
  geom_quasirandom(alpha = 0.6,size = 1,stroke=0)+
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Soluble Solid Content (°Brix)", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))

plot2 <- ggplot(all_data, aes (x = year, y=TA, color=Rootstock)) + 
  geom_quasirandom(alpha = 0.6,size = 1,stroke=0)+
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Titratable Acidity (g/L)", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))

plot3 <- ggplot(all_data, aes (x = year, y=pH, color=Rootstock)) + 
  geom_quasirandom(alpha = 0.6,size = 1,stroke=0)+
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="pH", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))

plot4 <- ggplot(all_data, aes (x = year, y=BerryWt, color=Rootstock)) + 
  geom_quasirandom(alpha = 0.6,size = 1,stroke=0)+
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Berry Weight (g)", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))

plot5 <- ggplot(all_data, aes (x = year, y=ClusterNum_Avg, color=Rootstock)) + 
  geom_quasirandom(alpha = 0.6,size = 1,stroke=0)+
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Cluster Number", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))


plot6 <- ggplot(all_data, aes (x = year, y=Yield_Avg, color=Rootstock)) +
  geom_quasirandom(alpha = 0.6,size = 1,stroke=0)+
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Yield (kg)", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))

plot7 <- ggplot(all_data, aes (x = year, y=Pruning_Avg, color=Rootstock)) + 
  geom_quasirandom(alpha = 0.6,size = 1,stroke=0)+
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Pruning Weight (kg)", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))

plot8 <- ggplot(all_data, aes (x = year, y=Ravaz_Index, color=Rootstock)) + 
  geom_quasirandom(alpha = 0.6,size = 1,stroke=0)+
  geom_line(stat="smooth",method = "loess",alpha=0.6, se=F)+
  labs(y="Ravaz Index", x="Year")+
  facet_grid(Variety~.)+
  theme_few()+
  theme(axis.text = element_text(colour="black"), axis.title=element_text(face="bold"))

require(cowplot)

pdf("figureS4.pdf", width=10, height=13)
plot_grid(plot1, plot2, plot3, plot4,plot5,plot6,plot7, plot8, ncol = 2)
dev.off()


#Calculate mean values for each phenotype for each year, by variety 

library(tidyverse)
library(ggthemes)
library(ggbeeswarm)

all_data %>%
  group_by(year, Variety) %>%
  summarise(mean = mean(Yield_Avg, na.rm=T), n = n())

# year Variety               mean     n
# <dbl> <chr>                <dbl> <int>
# 1  1995 'Cabernet Sauvignon' 13.3     60
# 2  1995 'Chardonnay'         10.4     60
# 3  1996 'Cabernet Sauvignon' 11.3     60
# 4  1996 'Chardonnay'          7.90    60
# 5  1997 'Cabernet Sauvignon' 17.3     60
# 6  1997 'Chardonnay'         15.3     60
# 7  1998 'Cabernet Sauvignon'  8.53    60
# 8  1998 'Chardonnay'          8.60    60
# 9  1999 'Cabernet Sauvignon' 14.4     60
# 10  1999 'Chardonnay'         11.5     60

all_data %>%
  group_by(year, Variety) %>%
  summarise(mean = mean(ClusterNum_Avg, na.rm=T), n = n())
# 
# year Variety               mean     n
# <dbl> <chr>                <dbl> <int>
#   1  1995 'Cabernet Sauvignon' 111.     60
# 2  1995 'Chardonnay'          63.2    60
# 3  1996 'Cabernet Sauvignon'  80.4    60
# 4  1996 'Chardonnay'          69.5    60
# 5  1997 'Cabernet Sauvignon' 118.     60
# 6  1997 'Chardonnay'          84.1    60
# 7  1998 'Cabernet Sauvignon'  71.8    60
# 8  1998 'Chardonnay'          59.9    60
# 9  1999 'Cabernet Sauvignon' 133.     60
# 10  1999 'Chardonnay'          79.5    60

#Pruning weight

all_data %>%
  group_by(year, Variety) %>%
  summarise(mean = mean(Pruning_Avg, na.rm=T), n = n())

# year Variety               mean     n
# <dbl> <chr>                <dbl> <int>
#   1  1995 'Cabernet Sauvignon' 1.01     60
# 2  1995 'Chardonnay'         0.740    60
# 3  1996 'Cabernet Sauvignon' 2.82     60
# 4  1996 'Chardonnay'         2.13     60
# 5  1997 'Cabernet Sauvignon' 1.43     60
# 6  1997 'Chardonnay'         1.16     60
# 7  1998 'Cabernet Sauvignon' 1.38     60
# 8  1998 'Chardonnay'         1.49     60
# 9  1999 'Cabernet Sauvignon' 1.12     60
# 10  1999 'Chardonnay'         0.990    60
