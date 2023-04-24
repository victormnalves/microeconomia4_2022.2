# carregando pacots
library(tidyverse)
library(lmtest)
library(plm)
library(pastecs)
library(haven)
library(AER)
library(magrittr)
library(RColorBrewer)
library(gridExtra)
library(stargazer)



dados <- read_dta('Star.dta')

dados %<>% mutate(
  class_type = case_when( # criando uma variável para cada tipo de classe para facilitar o trabalho com gráficos
    aide == 1 ~ 'Aide',
    regular == 1 ~ 'Regular',
    small == 1 ~ 'Small',
    TRUE ~ 'Other'),
    # transformando todas as colunas em categóricas para facilitar trabalho com dummies
    id = as_factor(id),
    schid = as_factor(schid),
    boy = as_factor(boy),
    white_asian = as_factor(white_asian),
    black = as_factor(black),
    tchwhite = as_factor(tchwhite),
    tchmasters = as_factor(tchmasters),
    freelunch = as_factor(freelunch),
    schurban = as_factor(schurban),
    schrural = as_factor(schrural),
    small = as_factor(small),
    regular = as_factor(regular),
    aide = as_factor(aide),
  )



# distribuição do tipo de classe 
dados %>% 
  ggplot() + 
  geom_bar(aes(class_type, y = ..count.. / sum(..count..)), fill = '#57075e') +   
  theme_minimal() +
  labs(x = 'Tipo de classe',
       y = 'Frequência relativa',
       title = 'Distribuição do tipo de classe',
       caption = 'Projeto STAR') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))



# gráficos de distribuição de cada nota entre os tipos de classe

## gráfico para teste de matemática
a <- dados %>% 
  ggplot() + 
  geom_density(aes(mathscore, colour = class_type), alpha = 0.1) +   
  geom_vline(aes(xintercept = mean(mathscore[class_type == 'Regular'])), linetype = "longdash") +
  theme_minimal() +
  labs(x = 'Nota em matemática',
       y = 'Frequência relativa',
       title = 'Distribuição da nota em teste por tipo de classe',
       caption = 'Projeto STAR') +
  scale_colour_brewer(palette = 'Purples', name = "Tipo de classe") + 
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))


## gráfico para teste de leitura
b <- dados %>% 
  ggplot() + 
  geom_density(aes(readscore, colour = class_type), alpha = 0.1) +  
  geom_vline(aes(xintercept = mean(readscore[class_type == 'Regular'])), linetype = "longdash") +
  theme_minimal() +
  labs(x = 'Nota em leitura',
       y = 'Frequência relativa',
       title = 'Distribuição da nota em teste por tipo de classe',
       caption = 'Projeto STAR') +
  scale_colour_brewer(palette = 'Purples', name = "Tipo de classe") + 
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))

# gráfico para notal geral
c <- dados %>% 
  ggplot() + 
  geom_density(aes(totalscore, colour = class_type), alpha = 0.1) +  
  geom_vline(aes(xintercept = mean(totalscore[class_type == 'Regular'])), linetype = "longdash") +
  theme_minimal() +
  labs(x = 'Nota total',
       y = 'Frequência relativa',
       title = 'Distribuição da nota em teste por tipo de classe',
       caption = 'Projeto STAR') +
  scale_colour_brewer(palette = 'Purples', name = "Tipo de classe") + 
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))

grid.arrange(arrangeGrob(a,b,c, nrow = 3)) # unindo os três gráficos



# distribuição do sexo dos alunos
dados %>% 
  ggplot() + 
  geom_bar(aes(as.factor(boy), y = ..count.. / sum(..count..), 
               fill = class_type), position="dodge2") +   
  theme_minimal() +
  labs(x = 'Sexo',
       y = 'Frequência relativa',
       title = 'Distribuição do sexo por tipo de classe',
       caption = 'Projeto STAR') +
  scale_x_discrete(labels = c('Menino', 'Menina')) +
  scale_fill_brewer(palette = 'Purples', name = "Tipo de classe") + 
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))



# distribuição da etnia dos alunos
dados %>% 
  ggplot() + 
  geom_bar(aes(as.factor(white_asian), y = ..count.. / sum(..count..), 
               fill = class_type), position="dodge2") +   
  theme_minimal() +
  labs(x = 'Etnia',
       y = 'Frequência relativa',
       title = 'Distribuição da etnia por tipo de classe',
       caption = 'Projeto STAR') +
  scale_x_discrete(labels = c('Branco ou asiático', 'Preto')) +
  scale_fill_brewer(palette = 'Purples', name = "Tipo de classe") + 
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))



# distribuição da etnia dos professores
dados %>% 
  ggplot() + 
  geom_bar(aes(as.factor(tchwhite), y = ..count.. / sum(..count..), 
               fill = class_type), position="dodge2") +   
  theme_minimal() +
  labs(x = 'Etnia',
       y = 'Frequência relativa',
       title = 'Distribuição da etnia do professor por tipo de classe',
       caption = 'Projeto STAR') +
  scale_x_discrete(labels = c('Branco', 'Não braco')) +
  scale_fill_brewer(
    palette = 'Purples',
    name = "Tipo de classe") + 
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))




# distribuição de professores com mestrado
dados %>% 
  ggplot() + 
  geom_bar(aes(as.factor(tchmasters), y = ..count.. / sum(..count..), 
               fill = class_type), position="dodge2") +   
  theme_minimal() +
  labs(x = 'Possui mestrado?',
       y = 'Frequência relativa',
       title = 'Distribuição da formação do professor por tipo de classe',
       caption = 'Projeto STAR') +
  scale_x_discrete(
    labels = c('Sim', 'Não')) +
  scale_fill_brewer(
    palette = 'Purples', 
    name = "Tipo de classe") + 
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))




# distribuição da experiência dos professores
dados %>% 
  ggplot() + 
  geom_density(
    aes(tchexper, colour = class_type), 
    alpha = 0.1) +   
  theme_minimal() +
  labs(x = 'Experiência do professor',
       y = 'Frequência relativa',
       title = 'Distribuição da experiência do professor por tipo de classe',
       caption = 'Projeto STAR') +
  scale_colour_brewer(
    palette = 'Purples', 
    name = "Tipo de classe") + 
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))



# distribuição de faltas entre os alunos
dados %>% 
  ggplot() + 
  geom_density(aes(absent, colour = class_type), alpha = 0.5) +   
  theme_minimal() +
  labs(x = 'Faltas por aluno',
       y = 'Frequência relativa',
       title = 'Distribuição de faltas por tipo de classe',
       caption = 'Projeto STAR') +
  scale_colour_brewer(palette = 'Purples', name = "Tipo de classe") + 
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))



# regressão para teste de leitura
reg_1 <- lm(readscore ~ 
              small + tchexper + absent + boy + white_asian + tchwhite + 
              tchmasters + freelunch + schurban, 
            data = dados
)
summary(reg_1) # obtendo a tabela via console



# regressão para teste de matemática
reg_2 <- lm(mathscore ~  
              small  + aide + tchexper + absent + boy + white_asian + tchwhite + 
              tchmasters + freelunch + schurban, 
            data = dados
)
summary(reg_2) # obtendo a tabela via console



# obtendo tabela em latex para as regressões de leitura e matemática
stargazer(reg_1, reg_2)



# regressão de teste de leitura com efeito fixo de escola
reg_3 <- lm(readscore ~
              small + aide + tchexper + absent + boy + white_asian + tchwhite + 
              tchmasters + freelunch + schurban + schid, 
            data = dados)
summary(reg_3) # obtendo a tabela via console



# regressão de teste de matemática com efeito fixo de escola
reg_4 <- lm(mathscore ~ 
              small + aide + tchexper + absent + boy + white_asian + tchwhite + 
              tchmasters + freelunch + schurban + schid, 
            data = dados
)
summary(reg_4) # obtendo a tabela via console



# tabela em latex para regressões com efeito fixo de escola
stargazer(reg_3, reg_4)



# regressão de readscore restrita
reg_3_r <- lm(readscore ~
              small + aide + tchexper + absent + boy + white_asian + tchwhite + 
              tchmasters + freelunch + schurban, 
            data = dados)
 
anova3 <- anova(reg_3_r, reg_3)
anova3 # obtendo o resultado via console



stargazer(anova3, summary = F) # obtendo a tabela anterior para LaTex



# regressão restrita para mathscore
reg_4_r <- lm(mathscore ~
              small + aide + tchexper + absent + boy + white_asian + tchwhite + 
              tchmasters + freelunch + schurban, 
            data = dados)
 
anova4 <- anova(reg_4_r, reg_4) 
anova4 # obtendo a tabela via console



stargazer(anova4, summary = F) # obtendo a tabela anterior para LaTex



# modelo MPL para verificação de aleatorização do experimento
reg_small <- lm(as.numeric(small) ~ 
              tchexper + absent + boy + white_asian + tchwhite + 
              tchmasters + freelunch + schurban, 
              data = dados)

summary(reg_small) # obtendo a tabela via console


stargazer(reg_small) # obtendo a tabela anterior para LaTex

