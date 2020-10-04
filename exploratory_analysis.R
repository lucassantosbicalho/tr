library(dplyr)
library(fields)
library(tidyverse)
library(broom)
library(corrplot)
library(ggplot2)

months <- 1:12
months_ <- c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
names(months) <- months_

days <- 1:7
days_ <-c ('mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun')
names(days) <- days_

dados <- read.csv("forestfires.csv")
dados$isBurned <- ifelse(dados$area == 0 , 0 , 1)
dados$lnArea <- log(dados$area + 1)
dados$damage_category <- ifelse(dados$area == 0, 'Sem dano', 
                                ifelse(dados$area <= 1, 'Baixo',
                                       ifelse(dados$area <= 25, 'Moderado', 
                                              ifelse(dados$area <= 100, 'Alto', 'Muito alto'))))
dados$month <- as.character(dados$month)
dados$month_c <- unname(months[dados$month])
dados$day <- as.character(dados$day)
dados$day_c <- unname(days[dados$day])

dados$id <- 1:nrow(dados)
dados %>%
  arrange(month_c) %>%
  mutate(idm = 1:nrow(dados)) -> dados
dados %>%
  arrange(day_c) %>%
  mutate(idd = 1:nrow(dados)) -> dados


# Análise da variável resposta
(prop.table(table(dados$isBurned)))

dados %>%
  ggplot(., aes(x=area)) +
  geom_histogram(aes(y=..density..), colour="#566573", fill="#566573") 
  

dados %>%
  ggplot(., aes(x=lnArea)) +
  geom_histogram(aes(y=..density..), colour="#566573", fill="#566573") 
  

dados %>%
  ggplot(., aes(x=reorder(month, idm))) +
  geom_bar(colour="#566573", fill="#566573") 

dados %>%
  ggplot(., aes(x=reorder(day, idd))) +
  geom_bar(colour="#566573", fill="#566573") 

dados %>%
  select(area) %>%
  ggplot(., aes(x=area)) +
  geom_histogram(aes(y=..density..), colour="#566573", fill="#566573") 
  

dados %>%
  ggplot(., aes(x=FFMC)) +
  geom_histogram(aes(y=..density..), colour="#566573", fill="#566573") 
  

dados %>%
  ggplot(., aes(x=DMC)) +
  geom_histogram(aes(y=..density..), colour="#566573", fill="#566573") 
  
dados %>%
  ggplot(., aes(x=DC)) +
  geom_histogram(aes(y=..density..), colour="#566573", fill="#566573") 


dados %>%
  ggplot(., aes(x=ISI)) +
  geom_histogram(aes(y=..density..), colour="#566573", fill="#566573") 


dados %>%
  ggplot(., aes(x=temp)) +
  geom_histogram(aes(y=..density..), colour="#566573", fill="#566573") 

dados %>%
  ggplot(., aes(x=RH)) +
  geom_histogram(aes(y=..density..), colour="#566573", fill="#566573") 

dados %>%
  ggplot(., aes(x=wind)) +
  geom_histogram(aes(y=..density..), colour="#566573", fill="#566573") 
  
dados %>%
  ggplot(., aes(x=rain)) +
  geom_histogram(aes(y=..density..), colour="#566573", fill="#566573") 


dados %>%
  filter(isBurned == 1) %>%
  ggplot(., aes(x=idm, y=area)) +
  geom_point(aes(col=as.factor(month_c), size=RH))

dados %>%
  select(-isBurned, -damage_category, -month, -day) %>% 
  cor() -> cor_dados
corrplot(cor_dados, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


