## ---- message=FALSE, warning=FALSE-------------------------------------
library(tidyverse)
library(sandwich)
library(stargazer)
library(optmatch)
library(lmtest)
library(Matching) 
library(MatchIt)
library(boot)


## ---- message=FALSE, warning=FALSE-------------------------------------
install.packages("devtools")
devtools::install_github("mj-ribeiro/Microdados")

library(Microdados)


## ---- warning=FALSE, message=FALSE-------------------------------------
pnad2003 <- load_pnad(2003, 'pessoas')
pnad2006 <- load_pnad(2006, 'pessoas')
pnad2009 <- load_pnad(2009, 'pessoas')

write_csv(pnad2003, 'Dados/pnad2003.csv')
write_csv(pnad2006, 'Dados/pnad2006.csv')
write_csv(pnad2009, 'Dados/pnad2009.csv')

pnad2003 <- read_csv('Dados/pnad2003.csv')
pnad2006 <- read_csv('Dados/pnad2006.csv')
pnad2009 <- read_csv('Dados/pnad2009.csv')


## ----message=FALSE, warning=FALSE--------------------------------------
pnad2006_filtrada <- pnad2006 %>%
  filter(v4742 <= quantile(v4742, probs = 0.2, na.rm = T),
         v8005==16|v8005==17|v8005==15) %>%
  mutate(tratamento = case_when
         (v8005==15 ~ 0, TRUE ~ 1),
         educ = if_else(v0602==2, 1, 0),
         ano = rep(0,5664))


pnad2009_filtrada <- pnad2009 %>%
  filter(v4742 <= quantile(v4742, probs = 0.2, na.rm = T),
         v8005==16|v8005==17|v8005==15) %>%
  mutate(tratamento = case_when
         (v8005==15 ~ 0,
          TRUE ~ 1),
         educ = if_else(v0602==2, 1, 0),
         ano = rep(1,5665))


base <- full_join(pnad2006_filtrada, pnad2009_filtrada)


## ----------------------------------------------------------------------
pnad2009_filtrada %>%
  ggplot()+
  geom_density(aes(v0302, stat(density), color = factor(tratamento)))+
  theme_minimal() +
  labs(title = 'Distribuição de Sexos Por Grupo 2009',
       caption = 'PNAD') +
  scale_colour_brewer(palette = 'Reds', name = "Tipo de classe") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8))


## ----------------------------------------------------------------------
pnad2009_filtrada %>%
  ggplot()+
  geom_density(aes(v0602, stat(density), color = factor(tratamento)))+
  theme_minimal() +
  labs(title = 'Frequência Escola ou Creche 2009',
       caption = 'PNAD') +
  scale_colour_brewer(palette = 'Reds', name = "Tipo de classe") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8))


## ----------------------------------------------------------------------

pnad2009_filtrada %>%
  ggplot()+
  geom_density(aes(v9001, stat(density), color = factor(tratamento)))+
  theme_minimal() +
  labs(title = 'Trabalhou na semana de referência 2009',
       caption = 'PNAD') +
  scale_colour_brewer(palette = 'Reds', name = "Tipo de classe") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8))




## ----------------------------------------------------------------------
pnad2009_filtrada %>%
  ggplot()+
  geom_density(aes(v4742, stat(density), color = factor(tratamento)))+
  theme_minimal() +
  labs(title = 'Rendimento mensal domiciliar per capita 2009',
       caption = 'PNAD') +
  scale_colour_brewer(palette = 'Reds', name = "Tipo de classe") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8))




## ----------------------------------------------------------------------
pnad2009_filtrada %>%
  ggplot()+
  geom_density(aes(v4724, stat(density), color = factor(tratamento)))+
  theme_minimal() +
  labs(title = 'Número de componentes da família 2009',
       caption = 'PNAD') +
  scale_colour_brewer(palette = 'Reds', name = "Tipo de classe") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8))


## ----------------------------------------------------------------------
pnad2009_filtrada %>%
  ggplot()+
  geom_density(aes(v4729, stat(density), color = factor(tratamento)))+
  theme_minimal() +
  labs(title = 'Peso da pessoa 2009',
       caption = 'PNAD') +
  scale_colour_brewer(palette = 'Reds', name = "Tipo de classe") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8))


## ----------------------------------------------------------------------
pnad2006_filtrada %>%
  ggplot()+
  geom_density(aes(v0302, stat(density), color = factor(tratamento)))+
  theme_minimal() +
  labs(title = 'Distribuição de Sexos Por Grupo 2006',
       caption = 'PNAD') +
  scale_colour_brewer(palette = 'Reds', name = "Tipo de classe") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8))


## ----------------------------------------------------------------------
pnad2006_filtrada %>%
  ggplot()+
  geom_density(aes(v0602, stat(density), color = factor(tratamento)))+
  theme_minimal() +
  labs(title = 'Frequência Escola ou Creche 2006',
       caption = 'PNAD') +
  scale_colour_brewer(palette = 'Reds', name = "Tipo de classe") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8))



## ----------------------------------------------------------------------

pnad2006_filtrada %>%
  ggplot()+
  geom_density(aes(v9001, stat(density), color = factor(tratamento)))+
  theme_minimal() +
  labs(title = 'Trabalhou na semana de referência 2006',
       caption = 'PNAD') +
  scale_colour_brewer(palette = 'Reds', name = "Tipo de classe") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8))


## ----------------------------------------------------------------------
pnad2006_filtrada %>%
  ggplot()+
  geom_density(aes(v4742, stat(density), color = factor(tratamento)))+
  theme_minimal() +
  labs(title = 'Rendimento mensal domiciliar per capita 2006',
       caption = 'PNAD') +
  scale_colour_brewer(palette = 'Reds', name = "Tipo de classe") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8))


## ----------------------------------------------------------------------
pnad2006_filtrada %>%
  ggplot()+
  geom_density(aes(v4724, stat(density), color = factor(tratamento)))+
  theme_minimal() +
  labs(title = 'Número de componentes da família 2006',
       caption = 'PNAD') +
  scale_colour_brewer(palette = 'Reds', name = "Tipo de classe") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8))


## ----------------------------------------------------------------------
pnad2006_filtrada %>%
  ggplot()+
  geom_density(aes(v4729, stat(density), color = factor(tratamento)))+
  theme_minimal() +
  labs(title = 'Peso da pessoa 2006',
       caption = 'PNAD') +
  scale_colour_brewer(palette = 'Reds', name = "Tipo de classe") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8))


## ----------------------------------------------------------------------
summary(pnad2006_filtrada)


## ----------------------------------------------------------------------
psm <- glm(tratamento~v8005+v0403+v4803+v9001+v4724+v4742+educ, 
           family = binomial(link="probit"), 
           data = pnad2009_filtrada, weights = v4729)

pscore <- predict(psm, type = "response")
summary(pscore)

# Suporte Comum 
# Usamos type="response" para que predict nos devolva P(Y=1|X)

ggplot(pnad2009_filtrada, mapping=aes(x=pscore)) + 
  geom_density() +
  facet_wrap(~tratamento) +
  xlab("Probabilidade de ser tratado")


## ----------------------------------------------------------------------
stargazer(psm)


## ----------------------------------------------------------------------
df_match <- pnad2009_filtrada  %>% 
  mutate(u=runif(length(tratamento),0,1)) %>%
  arrange(u)

match_ATT <- matchit(tratamento~v8005+v0403+v4803+v9001+v4724+v4742+educ,
                     data = df_match,
                     link = "probit",
                     replace = T)

ATT_reg <- lm(educ~tratamento,
              data = df_match,
              weights = v4729)

summary(ATT_reg)


## ----------------------------------------------------------------------
stargazer(ATT_reg)


## ----------------------------------------------------------------------
# Teste para grupo de controle
t.test(pnad2006_filtrada$educ[pnad2006_filtrada$tratamento==0],
       pnad2009_filtrada$educ[pnad2009_filtrada$tratamento==0])


## ----------------------------------------------------------------------
# Teste para grupo de tratamento
t.test(pnad2006_filtrada$educ[pnad2006_filtrada$tratamento==1],
       pnad2009_filtrada$educ[pnad2009_filtrada$tratamento==1])


## ----------------------------------------------------------------------
controle_antes <- mean(pnad2006_filtrada$educ[pnad2006_filtrada$tratamento==0])
controle_depois <- mean(pnad2009_filtrada$educ[pnad2009_filtrada$tratamento==0])
tratamento_antes <- mean(pnad2006_filtrada$educ[pnad2006_filtrada$tratamento==1])
tratamento_depois <- mean(pnad2009_filtrada$educ[pnad2009_filtrada$tratamento==1])

(tratamento_depois - tratamento_antes) - (controle_depois - controle_antes) # diferença em diferenças


## ----------------------------------------------------------------------
did <- lm(educ~tratamento+ano+(ano*tratamento)+
               v8005+v0403+v9001+v4724+v4742,
             data = base)
summary(did)


## ----------------------------------------------------------------------
stargazer(did)

