library(tidyverse)
library(gtable)
library(grid)
library(plotly)
library(lubridate)
library(magrittr)
library(gridExtra)
library(ggforce)
library(kableExtra)
library(leaflet)
library(rlang)
library(scales)

df <-  read.csv("owid-covid-data.csv")

#NEPAL
Nepal = subset(df, location == "Nepal")
Nep1 <- Nepal %>% group_by(date) %>%
  summarize(location= "Nepal",
            cases = sum(new_cases, na.rm = T),
            deaths = sum(new_deaths, na.rm = T))
Nep1$confirmed <- cumsum(Nep1$cases)
Nep1$Deaths <- cumsum(Nep1$deaths)
Nep1$date = as.Date(Nep1$date)
Nep1 %>%
  filter(!is.na(date), !is.na(confirmed)) %>%
  group_by(date, confirmed) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = date, y = confirmed)) + 
  geom_line(color= "69b3a2", size=3, alpha=2, linetype=1)

plot1 <- ggplot(Nep1, aes(x = date, y = confirmed)) + 
  geom_point()+
  xlab('') + ylab('Cases') + labs(title= "Cummulative cases") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
plot2 <- ggplot(Nep1, aes(x = date, y = Deaths)) + 
  geom_point()+
  xlab('') + ylab('Deaths') + labs(title= "Cummulative deaths") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
plot3 <- ggplot(Nep1, aes(x = date, y = cases)) + 
  geom_line()+
  xlab('') + ylab('Cases') + labs(title= "Daily cases") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
plot4 <- ggplot(Nep1, aes(x = date, y = deaths)) + 
  geom_line()+
  xlab('') + ylab('Deaths') + labs(title= "Daily deaths") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
grid.arrange(plot1, plot2,plot3, plot4, nrow=2)


#USA
USA = subset(df, location == "United States")
USA1 <- USA %>% group_by(date) %>%
  summarize(location= "United States",
            cases = sum(new_cases, na.rm = T),
            deaths = sum(new_deaths, na.rm = T))
USA1$confirmed <- cumsum(USA1$cases)
USA1$Deaths <- cumsum(USA1$deaths)
USA1$date = as.Date(USA1$date)
USA1 %>%
  filter(!is.na(date), !is.na(confirmed)) %>%
  group_by(date, confirmed) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = date, y = confirmed)) + 
  geom_line(color= "69b3a2", size=3, alpha=2, linetype=1)

plot1 <- ggplot(USA1, aes(x = date, y = confirmed)) + 
  geom_point()+
  xlab('') + ylab('Cases') + labs(title= "Cummulative cases") + scale_y_continuous(labels = comma) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
plot2 <- ggplot(USA1, aes(x = date, y = Deaths)) + scale_y_continuous(labels = comma)+
  geom_point()+
  xlab('') + ylab('Deaths') + labs(title= "Cummulative deaths") + scale_y_continuous(labels = comma)+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
plot3 <- ggplot(USA1, aes(x = date, y = cases)) + 
  geom_line()+
  xlab('') + ylab('Cases') + labs(title= "Daily cases") + scale_y_continuous(labels = comma)+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
plot4 <- ggplot(USA1, aes(x = date, y = deaths)) + 
  geom_line()+
  xlab('') + ylab('Deaths') + labs(title= "Daily deaths") + scale_y_continuous(labels = comma)+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
grid.arrange(plot1, plot2,plot3, plot4, nrow=2)

W <- df %>% group_by(location) %>%
  summarize(country= "location",
            cases = sum(new_cases, na.rm = T),
            deaths = sum(new_deaths, na.rm = T))
W1 <- W %>% filter(deaths>500000)
W2 <- W1 %>% arrange(desc(deaths))
W2 = W2[-1,]
ggplot()+geom_bar(data=W2, aes(x=location, y=deaths), stat = "identity") + 
  geom_point()+geom_text() +  scale_y_continuous(labels = comma)

ggplot()+geom_bar(data=W2, aes(x = reorder(location, deaths), y = deaths)), stat = "identity") + 
  geom_point()+geom_text() +  scale_y_continuous(labels = comma)

#WORLD
W2 %>% 
  ggplot(aes(fct_reorder(location,
                         deaths), 
             deaths))+
  geom_col() + scale_y_continuous(labels = comma) +
  labs(x="Location", title="Deaths")

W2 %>% 
  ggplot(aes(fct_rev(fct_reorder(location,
                         deaths)), 
             deaths))+
  geom_col() + scale_y_continuous(labels = comma) +
  labs(x="Location", title="Deaths")

df$date = as.Date(df$date)


#SAARC Nations
Nepal1 = subset(df, location == "Nepal")
Nep1 <- Nepal1 %>% group_by(date) %>%
  summarize(location= "Nepal",
            cases = sum(new_cases, na.rm = T),
            deaths = sum(new_deaths, na.rm = T))
Nep1$confirmed <- cumsum(Nep1$cases)
Nep1$Deaths <- cumsum(Nep1$deaths)

Nepal2 = subset(df, location == "India")
Ind <- Nepal2 %>% group_by(date) %>%
  summarize(location= "India",
            cases = sum(new_cases, na.rm = T),
            deaths = sum(new_deaths, na.rm = T))
Ind$confirmed <- cumsum(Ind$cases)
Ind$Deaths <- cumsum(Ind$deaths)

Nepal3 = subset(df, location == "Pakistan")
Pak <- Nepal3 %>% group_by(date) %>%
  summarize(location= "Pakistan",
            cases = sum(new_cases, na.rm = T),
            deaths = sum(new_deaths, na.rm = T))
Pak$confirmed <- cumsum(Pak$cases)
Pak$Deaths <- cumsum(Pak$deaths)

Nepal4 = subset(df, location == "Afghanistan")
Afg <- Nepal4 %>% group_by(date) %>%
  summarize(location= "Afghanistan",
            cases = sum(new_cases, na.rm = T),
            deaths = sum(new_deaths, na.rm = T))
Afg$confirmed <- cumsum(Afg$cases)
Afg$Deaths <- cumsum(Afg$deaths)

Nepal5 = subset(df, location == "Maldives")
Mal <- Nepal5 %>% group_by(date) %>%
  summarize(location= "Maldives",
            cases = sum(new_cases, na.rm = T),
            deaths = sum(new_deaths, na.rm = T))
Mal$confirmed <- cumsum(Mal$cases)
Mal$Deaths <- cumsum(Mal$deaths)

Nepal6 = subset(df, location == "Bhutan")
Bhu <- Nepal6 %>% group_by(date) %>%
  summarize(location= "Bhutan",
            cases = sum(new_cases, na.rm = T),
            deaths = sum(new_deaths, na.rm = T))
Bhu$confirmed <- cumsum(Bhu$cases)
Bhu$Deaths <- cumsum(Bhu$deaths)


Nepal7 = subset(df, location == "Sri Lanka")
Sri <- Nepal7 %>% group_by(date) %>%
  summarize(location= "Sri Lanka",
            cases = sum(new_cases, na.rm = T),
            deaths = sum(new_deaths, na.rm = T))
Sri$confirmed <- cumsum(Sri$cases)
Sri$Deaths <- cumsum(Sri$deaths)

Nepal8 = subset(df, location == "Bangladesh")
Ban <- Nepal8 %>% group_by(date) %>%
  summarize(location= "Bangladesh",
            cases = sum(new_cases, na.rm = T),
            deaths = sum(new_deaths, na.rm = T))
Ban$confirmed <- cumsum(Ban$cases)
Ban$Deaths <- cumsum(Ban$deaths)

Nepal9 = subset(df, location == "China")
Chi <- Nepal9 %>% group_by(date) %>%
  summarize(location= "China",
            cases = sum(new_cases, na.rm = T),
            deaths = sum(new_deaths, na.rm = T))
Chi$confirmed <- cumsum(Chi$cases)
Chi$Deaths <- cumsum(Chi$deaths)

plot1 <- ggplot(Nep1, aes(x = date, y = confirmed)) + 
  geom_line()+ scale_y_continuous(labels = comma)+
  xlab('') + ylab('Cases') + labs(title= "Nepal") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme_classic()
plot2 <- ggplot(Ind, aes(x = date, y = confirmed)) + 
  geom_line()+ scale_y_continuous(labels = comma)+
  xlab('') + ylab('Cases') + labs(title= "India") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme_classic()
plot3 <- ggplot(Pak, aes(x = date, y = confirmed)) + 
  geom_line()+scale_y_continuous(labels = comma)+
  xlab('') + ylab('Cases') + labs(title= "Pakistan") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme_classic()
plot4 <- ggplot(Afg, aes(x = date, y = confirmed)) + 
  geom_line()+scale_y_continuous(labels = comma)+
  xlab('') + ylab('Cases') + labs(title= "Afghanistan") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme_classic()
plot5 <- ggplot(Mal, aes(x = date, y = confirmed)) + 
  geom_line()+scale_y_continuous(labels = comma)+
  xlab('') + ylab('Cases') + labs(title= "Maldives") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme_classic()
plot6 <- ggplot(Bhu, aes(x = date, y = confirmed)) + 
  geom_line()+scale_y_continuous(labels = comma)+
  xlab('') + ylab('Cases') + labs(title= "Bhutan") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme_classic()
plot7 <- ggplot(Sri, aes(x = date, y = confirmed)) + 
  geom_line()+scale_y_continuous(labels = comma)+
  xlab('') + ylab('Cases') + labs(title= "SriLanka") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme_classic()
plot8 <- ggplot(Ban, aes(x = date, y = confirmed)) + 
  geom_line()+scale_y_continuous(labels = comma)+
  xlab('') + ylab('Cases') + labs(title= "Bangladesh") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme_classic()
plot9 <- ggplot(Chi, aes(x = date, y = confirmed)) + 
  geom_line()+scale_y_continuous(labels = comma)+
  xlab('') + ylab('Cases') + labs(title= "China") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + theme_classic()

grid.arrange(plot1,
             plot2,
             plot3, 
             plot4, 
             plot5,
             plot6, 
             plot7, 
             plot8,
             plot9,
             nrow=3, 
             top = "COVID-19 Cases in SAARC Nations and China",
             bottom = textGrob("Dr. Sachin Subedi",
                               gp=gpar(fontface = 3, fontsize = 12),
                               hjust = 1.5,
                               x=1
             )
) 

            

