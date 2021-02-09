library(tidyverse)
library(ggthemes)
library(ggbeeswarm)

all_data <- read_csv("liberty_historical_data.csv")

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
