library(tidyverse)
library(stargazer)
library(haven)
library(plm)
library(qwraps2)
library(gridExtra)


## Carregando dados 
dados <- read_dta('APS1_dados.dta')

dados %>% 
  select(c(crime_burglary, crime_larceny, crime_vehicle, shall_law, rendamedia,
           densidade, perc_homens_10_29, perc_negros_10_64, tx_encarceramento)) %>% 
  psych::describe(., skew = FALSE, ranges = FALSE)


## plots para burglary
a <- dados %>% 
  mutate(shall_law = as.factor(shall_law)) %>% 
  ggplot(aes(tx_encarceramento, crime_burglary)) +
  geom_point() + 
  geom_smooth(aes(colour = shall_law), method = 'lm', se = F) +
  labs(x = 'Taxa de encarceramento',
       y = 'Taxa de burglary',
       title = 'Relação entre taxa de encarceramento e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)' ,
       color = 'Shall law') + 
  scale_color_manual(labels = c("Não", "Sim"), values = c("blue", "red")) + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )

b <- dados %>% 
  ggplot(aes(rendamedia, crime_burglary)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Renda média',
       y = 'Taxa de burglary',
       title = 'Relação entre renda média e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )

c <- dados %>% 
  ggplot(aes(densidade, crime_burglary)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Densidade',
       y = 'Taxa de burglary',
       title = 'Relação entre densidade e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )

d <- dados %>% 
  ggplot(aes(perc_homens_10_29, crime_burglary)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Percentual de homens entre 10 e 29 anos',
       y = 'Taxa de burglary',
       title = 'Relação entre percentual de homens jovens e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )

e <- dados %>% 
  ggplot(aes(perc_negros_10_64, crime_burglary)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Percentual de homens negros 10 e 64 anos',
       y = 'Taxa de burglary',
       title = 'Relação entre percentual de negros e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )

f <- dados %>% 
  ggplot(aes(log(tx_encarceramento), crime_burglary)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Taxa de encarceramento do período anterior',
       y = 'Taxa de burglary',
       title = 'Relação entre taxa de encarceramento e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )

grid.arrange(arrangeGrob(a,b,c,d,e,f, ncol = 2)) 


## plots para larceny
a <- dados %>% 
  mutate(shall_law = as.factor(shall_law)) %>% 
  ggplot(aes(tx_encarceramento, crime_larceny)) +
  geom_point() + 
  geom_smooth(aes(colour = shall_law), method = 'lm', se = F) +
  labs(x = 'Taxa de encarceramento',
       y = 'Taxa de larceny',
       title = 'Relação entre taxa de encarceramento e taxa de larceny',
       caption = 'Construido com base em Donohue e Ayres (2003)' ,
       color = 'Shall law') + 
  scale_color_manual(labels = c("Não", "Sim"), values = c("blue", "red")) + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )

b <- dados %>% 
  ggplot(aes(rendamedia, crime_larceny)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Renda média',
       y = 'Taxa de larceny',
       title = 'Relação entre renda média e taxa de larceny',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )

c <- dados %>% 
  ggplot(aes(densidade, crime_larceny)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Densidade',
       y = 'Taxa de larceny',
       title = 'Relação entre densidade e taxa de larceny',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )

d <- dados %>% 
  ggplot(aes(perc_homens_10_29, crime_larceny)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Percentual de homens entre 10 e 29 anos',
       y = 'Taxa de larceny',
       title = 'Relação entre percentual de homens jovens e taxa de larceny',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )
  
e <- dados %>% 
  ggplot(aes(perc_negros_10_64, crime_burglary)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Percentual de homens negros 10 e 64 anos',
       y = 'Taxa de burglary',
       title = 'Relação entre percentual de negros e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )
  
f <- dados %>% 
  ggplot(aes(log(tx_encarceramento), crime_burglary)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Taxa de encarceramento do período anterior',
       y = 'Taxa de burglary',
       title = 'Relação entre taxa de encarceramento e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )

grid.arrange(arrangeGrob(a,b,c,d,e,f, ncol = 2)) 


## plots para vehicle
a <- dados %>% 
  mutate(shall_law = as.factor(shall_law)) %>% 
  ggplot(aes(tx_encarceramento, crime_vehicle)) +
  geom_point() + 
  geom_smooth(aes(colour = shall_law), method = 'lm', se = F) +
  labs(x = 'Taxa de encarceramento',
       y = 'Taxa de roubos de veículos',
       title = 'Relação entre taxa de encarceramento e taxa de roubos de veículos',
       caption = 'Construido com base em Donohue e Ayres (2003)' ,
       color = 'Shall law') + 
  scale_color_manual(labels = c("Não", "Sim"), values = c("blue", "red")) + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )

b <- dados %>% 
  ggplot(aes(rendamedia, crime_vehicle)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Renda média',
       y = 'Taxa de roubos de veículos',
       title = 'Relação entre renda média e taxa de roubos de veículos',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )
  
c <- dados %>% 
  ggplot(aes(densidade, crime_vehicle)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Densidade',
       y = 'Taxa de roubos de veículos',
       title = 'Relação entre densidade e taxa de roubos de veículos',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )
  
d <- dados %>% 
  ggplot(aes(perc_homens_10_29, crime_vehicle)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Percentual de homens entre 10 e 29 anos',
       y = 'Taxa de roubos de veículos',
       title = 'Relação entre percentual de homens jovens e taxa de roubos de veículos',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )
  
e <- dados %>% 
  ggplot(aes(perc_negros_10_64, crime_vehicle)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Percentual de homens negros 10 e 64 anos',
       y = 'Taxa de roubos de veículos',
       title = 'Relação entre percentual de negros e taxa de roubos de veículos',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )
  
f <- dados %>% 
  ggplot(aes(log(tx_encarceramento), crime_vehicle)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Taxa de encarceramento do período anterior',
       y = 'Taxa de roubos de veículos',
       title = 'Relação entre taxa de encarceramento e taxa de roubos de veículos',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
        )

gridExtra::grid.arrange(arrangeGrob(a,b,c,d,e,f, ncol = 2)) 

## testando logs

#===============================================================

aa <-dados %>% 
  mutate(shall_law = as.factor(shall_law)) %>% 
  ggplot(aes(log(tx_encarceramento), crime_burglary)) +
  geom_point() + 
  geom_smooth(aes(colour = shall_law), method = 'lm', se = F) +
  labs(x = 'Taxa de encarceramento',
       y = 'Taxa de burglary',
       title = 'Relação entre taxa de encarceramento e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)' ,
       color = 'Shall law') + 
  scale_color_manual(labels = c("Não", "Sim"), values = c("blue", "red")) + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )


bb <- dados %>% 
  ggplot(aes(log(rendamedia), crime_burglary)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Renda média',
       y = 'Taxa de burglary',
       title = 'Relação entre renda média e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )


cc <-dados %>% 
  ggplot(aes(log(densidade), crime_burglary)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Densidade',
       y = 'Taxa de burglary',
       title = 'Relação entre densidade e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )


dd <- dados %>% 
  ggplot(aes(log(perc_homens_10_29), crime_burglary)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Percentual de homens entre 10 e 29 anos',
       y = 'Taxa de burglary',
       title = 'Relação entre percentual de homens jovens e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )

ee <- dados %>% 
  ggplot(aes(log(perc_negros_10_64), crime_burglary)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Percentual de homens negros 10 e 64 anos',
       y = 'Taxa de burglary',
       title = 'Relação entre percentual de negros e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )

ff <- dados %>% 
  ggplot(aes(log(tx_encarceramento), crime_burglary)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Taxa de encarceramento do período anterior',
       y = 'Taxa de burglary',
       title = 'Relação entre taxa de encarceramento e taxa de burglary',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )

gridExtra::grid.arrange(arrangeGrob(aa,bb,cc,dd,ee,ff, ncol = 2))

#===================================================================================================

aaa <-dados %>% 
  mutate(shall_law = as.factor(shall_law)) %>% 
  ggplot(aes(log(tx_encarceramento), crime_larceny)) +
  geom_point() + 
  geom_smooth(aes(colour = shall_law), method = 'lm', se = F) +
  labs(x = 'Taxa de encarceramento',
       y = 'Taxa de larceny',
       title = 'Relação entre taxa de encarceramento e taxa de larceny',
       caption = 'Construido com base em Donohue e Ayres (2003)' ,
       color = 'Shall law') + 
  scale_color_manual(labels = c("Não", "Sim"), values = c("blue", "red")) + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )


bbb <- dados %>% 
  ggplot(aes(log(rendamedia), crime_larceny)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Renda média',
       y = 'Taxa de larceny',
       title = 'Relação entre renda média e taxa de larceny',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )


ccc <-dados %>% 
  ggplot(aes(log(densidade), crime_larceny)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Densidade',
       y = 'Taxa de larceny',
       title = 'Relação entre densidade e taxa de larceny',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )


ddd <- dados %>% 
  ggplot(aes(log(perc_homens_10_29), crime_larceny)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Percentual de homens entre 10 e 29 anos',
       y = 'Taxa de larceny',
       title = 'Relação entre percentual de homens jovens e taxa de larceny',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )

eee <- dados %>% 
  ggplot(aes(log(perc_negros_10_64), crime_larceny)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Percentual de homens negros 10 e 64 anos',
       y = 'Taxa de larceny',
       title = 'Relação entre percentual de negros e taxa de larceny',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )

fff <- dados %>% 
  ggplot(aes(log(tx_encarceramento), crime_larceny)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Taxa de encarceramento do período anterior',
       y = 'Taxa de larceny',
       title = 'Relação entre taxa de encarceramento e taxa de larceny',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )

gridExtra::grid.arrange(arrangeGrob(aaa,bbb,ccc,ddd,eee,fff, ncol = 2))

aaa11 <-dados %>% 
  mutate(shall_law = as.factor(shall_law)) %>% 
  ggplot(aes(log(tx_encarceramento), crime_vehicle)) +
  geom_point() + 
  geom_smooth(aes(colour = shall_law), method = 'lm', se = F) +
  labs(x = 'Taxa de encarceramento',
       y = 'Taxa de Roubo de carros',
       title = 'Relação entre taxa de encarceramento e taxa de roubo de carros',
       caption = 'Construido com base em Donohue e Ayres (2003)' ,
       color = 'Shall law') + 
  scale_color_manual(labels = c("Não", "Sim"), values = c("blue", "red")) + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )


bbb11 <- dados %>% 
  ggplot(aes(log(rendamedia), crime_vehicle)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Renda média',
       y = 'Taxa de Roubo de carros',
       title = 'Relação entre renda média e taxa de roubo de carros',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )


ccc11 <-dados %>% 
  ggplot(aes(log(densidade), crime_vehicle)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Densidade',
       y = 'Taxa de Roubo de carros',
       title = 'Relação entre densidade e taxa de roubo de carros',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )


ddd11 <- dados %>% 
  ggplot(aes(log(perc_homens_10_29), crime_vehicle)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Percentual de homens entre 10 e 29 anos',
       y = 'Taxa de Roubo de carros',
       title = 'Relação entre percentual de homens jovens e taxa de roubo de carros',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )

eee11 <- dados %>% 
  ggplot(aes(log(perc_negros_10_64), crime_vehicle)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Percentual de homens negros 10 e 64 anos',
       y = 'Taxa de Roubo de carros',
       title = 'Relação entre percentual de negros e taxa de roubo de carros',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )

fff11 <- dados %>% 
  ggplot(aes(log(tx_encarceramento), crime_vehicle)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  labs(x = 'Taxa de encarceramento do período anterior',
       y = 'Taxa de Roubo de carros',
       title = 'Relação entre taxa de encarceramento e taxa de roubo de carros',
       caption = 'Construido com base em Donohue e Ayres (2003)') + 
  theme_minimal() +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10, face="bold"),
        legend.title = element_text(size = 10),
  )

gridExtra::grid.arrange(arrangeGrob(aaa11,bbb11,ccc11,ddd11,eee11,fff11, ncol = 2))

## regressão para burglary
reg1 <- plm(crime_burglary ~ shall_law+rendamedia+densidade+perc_homens_10_29+
              perc_negros_10_64 + tx_encarceramento + tx_encarceramento * shall_law, 
            data = dados, model = 'within') # regessão de efeitos fixos

reg1_a <- plm(crime_burglary ~ shall_law+rendamedia+densidade+perc_homens_10_29+
              perc_negros_10_64 + tx_encarceramento + tx_encarceramento * shall_law, 
            data = dados, model = 'random') # regressão de efeitos aleatórios
summary(reg1)


## regressão para larceny
reg2 <- plm(crime_larceny~shall_law+rendamedia+densidade+perc_homens_10_29+
              perc_negros_10_64 + tx_encarceramento + tx_encarceramento * shall_law, 
            data = dados, model = 'within') # regressão de efeitos fixos

reg2_a <- plm(crime_larceny~shall_law+rendamedia+densidade+perc_homens_10_29+
              perc_negros_10_64 + tx_encarceramento + tx_encarceramento * shall_law, 
            data = dados, model = 'random') # regressão de efeitos aleatórios

summary(reg2)



## regressão para vehicle
reg3 <- plm(crime_vehicle~shall_law+rendamedia+densidade+perc_homens_10_29+
              perc_negros_10_64 + tx_encarceramento + tx_encarceramento * shall_law, 
            data = dados, model = 'within') # regressão com efeitos fixos

reg3_a <- plm(crime_vehicle~shall_law+rendamedia+densidade+perc_homens_10_29+
              perc_negros_10_64 + tx_encarceramento + tx_encarceramento * shall_law, 
            data = dados, model = 'random') # regressão com efeitos aleatórios

summary(reg3)


## tabela para as três regressões
stargazer(reg1, reg2, reg3)


## teste de hausman para burglary
phtest(reg1, reg1_a) 


## teste de hausman para larceny
phtest(reg2, reg2_a) 


## teste de hausman para vehicle
phtest(reg3, reg3_a) 

