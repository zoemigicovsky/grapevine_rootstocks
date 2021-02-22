library(tidyverse)
library(readxl)
library(extrafont)
library(ggthemes)

lodi_weather <- read_xlsx("Lodi weather data 94 thru 99.xlsx")

#Keep date column, precipitation and temperature

lodi_weather <- lodi_weather %>% select(DATE,PRCP,TMAX,TMIN)

#Combine precipation so I get total precipitation across each year

lodi_weather <- lodi_weather %>% separate(DATE,c("year", "month", "day"), sep="-", remove=F)

lodi_weather <- lodi_weather %>% group_by(year) %>% mutate(total_precip=cumsum(PRCP))

#turns out some dates are missing so I can't just order them by dates 
lodi_weather <- lodi_weather %>% arrange(year, month, day) %>% mutate(date_order=1:length(year))

lodi_weather <- lodi_weather %>% select(date=DATE, year:day, max_temp=TMAX, min_temp=TMIN, total_precip:date_order)


#weather plots

#Remove weather temps that are 0 

lodi_weather$min_temp <- gsub("\\<0\\>", NA, lodi_weather$min_temp)
lodi_weather$max_temp <- gsub("\\<0\\>", NA, lodi_weather$max_temp)

#precipitation 
library(scales)
lodi_weather$date <- as.Date(lodi_weather$date)

year0 <- lodi_weather %>% filter(year=="1994") %>% ggplot(aes(x=date, y=total_precip)) +
  geom_line(size=1.5)+
  theme_few()+
  labs(x="Date", y="Cumultative Precipitation (in)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(0,32)

year1 <- lodi_weather %>% filter(year=="1995") %>% ggplot(aes(x=date, y=total_precip)) +
  geom_line(size=1.5)+
  theme_few()+
  labs(x="Date", y="Cumultative Precipitation (in)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(0,32)

year2 <- lodi_weather %>% filter(year=="1996") %>% ggplot(aes(x=date, y=total_precip)) +
  geom_line(size=1.5)+
  theme_few()+
  labs(x="Date", y="Cumultative Precipitation (in)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(0,32)

year3 <- lodi_weather %>% filter(year=="1997") %>% ggplot(aes(x=date, y=total_precip)) +
  geom_line(size=1.5)+
  theme_few()+
  labs(x="Date", y="Cumultative Precipitation (in)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(0,32)


year4 <- lodi_weather %>% filter(year=="1998") %>% ggplot(aes(x=date, y=total_precip)) +
  geom_line(size=1.5)+
  theme_few()+
  labs(x="Date", y="Cumultative Precipitation (in)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(0,32)


year5 <- lodi_weather %>% filter(year=="1999") %>% ggplot(aes(x=date, y=total_precip)) +
  geom_line(size=1.5)+
  theme_few()+
  labs(x="Date", y="Cumultative Precipitation (in)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(0,32)

require(gridExtra)
pdf("figureS2_precip.pdf",width = 15, height=10,family="Arial")
grid.arrange(year0, year1, year2, year3,year4, year5, ncol=3)
dev.off()

#TEMPERATURE min
lodi_weather$min_temp <- as.numeric(lodi_weather$min_temp)

year0 <- lodi_weather %>% filter(year=="1994") %>% ggplot(aes(x=date, y=min_temp)) +
  geom_point()+
  theme_few()+
  labs(x="Date", y="min temp (F)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(20,74)

year1 <- lodi_weather %>% filter(year=="1995") %>% ggplot(aes(x=date, y=min_temp)) +
  geom_point()+
  theme_few()+
  labs(x="Date", y="min temp (F)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(20,74)

year2 <- lodi_weather %>% filter(year=="1996") %>% ggplot(aes(x=date, y=min_temp)) +
  geom_point()+
  theme_few()+
  labs(x="Date", y="min temp (F)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(20,74)

year3 <- lodi_weather %>% filter(year=="1997") %>% ggplot(aes(x=date, y=min_temp)) +
  geom_point()+
  theme_few()+
  labs(x="Date", y="min temp (F)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(20,74)

year4 <- lodi_weather %>% filter(year=="1998") %>% ggplot(aes(x=date, y=min_temp)) +
  geom_point()+
  theme_few()+
  labs(x="Date", y="min temp (F)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(20,74)

year5 <- lodi_weather %>% filter(year=="1999") %>% ggplot(aes(x=date, y=min_temp)) +
  geom_point()+
  theme_few()+
  labs(x="Date", y="min temp (F)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(20,74)

require(gridExtra)
pdf("figureS2_min_temp.pdf",width = 15, height=10,family="Arial")
grid.arrange(year0, year1, year2, year3,year4, year5, ncol=3)
dev.off()


#TEMPERATURE max
lodi_weather$max_temp <- as.numeric(lodi_weather$max_temp)

year0 <- lodi_weather %>% filter(year=="1994") %>% ggplot(aes(x=date, y=max_temp)) +
  geom_point()+
  theme_few()+
  labs(x="Date", y="max temp (F)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(38,106)

year1 <- lodi_weather %>% filter(year=="1995") %>% ggplot(aes(x=date, y=max_temp)) +
  geom_point()+
  theme_few()+
  labs(x="Date", y="max temp (F)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(38,106)

year2 <- lodi_weather %>% filter(year=="1996") %>% ggplot(aes(x=date, y=max_temp)) +
  geom_point()+
  theme_few()+
  labs(x="Date", y="max temp (F)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(38,106)

year3 <- lodi_weather %>% filter(year=="1997") %>% ggplot(aes(x=date, y=max_temp)) +
  geom_point()+
  theme_few()+
  labs(x="Date", y="max temp (F)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(38,106)

year4 <- lodi_weather %>% filter(year=="1998") %>% ggplot(aes(x=date, y=max_temp)) +
  geom_point()+
  theme_few()+
  labs(x="Date", y="max temp (F)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(38,106)

year5 <- lodi_weather %>% filter(year=="1999") %>% ggplot(aes(x=date, y=max_temp)) +
  geom_point()+
  theme_few()+
  labs(x="Date", y="max temp (F)") + 
  theme(axis.text=element_text(size=10, colour="black"),axis.title=element_text(size=12,face="bold", colour="black"))+
  theme(legend.title = element_blank())+
  ylim(38,106)

require(gridExtra)
pdf("figureS2_max_temp.pdf",width = 15, height=10,family="Arial")
grid.arrange(year0, year1, year2, year3,year4, year5, ncol=3)
dev.off()