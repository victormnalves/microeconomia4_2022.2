## ---- echo=FALSE, message=FALSE----------------
library(tidyverse)
library(magrittr)
library(gridExtra)
library(ggthemes)
library(questionr)
library(survey)
library(skimr)
library(VGAM)
library(margins)
library(ROCit)
library(lmtest)
library(ResourceSelection)
library(stargazer)


## ---- echo=FALSE-------------------------------
wvs <- read.csv('W7.csv') %>% 
select( # selecionando as variáveis de interesse
  Q1, Q4, Q5, Q46, Q49, Q37, Q32,
  Q28, Q190, Q143,
  W_WEIGHT, Q267,
  Q260, Q262, Q273, Q274, Q275, Q275R, Q279,
  Q288, Q288R, H_URBRURAL, Q290, B_COUNTRY_ALPHA
) %>%
mutate_if( # transformando negativo em NA
  is.numeric, function(x) {x[x < 0] <- NA; x}
  ) %>% 
filter( # filtrando NAs
  !is.na(Q1), 
  !is.na(Q4),
  !is.na(Q5),
  !is.na(Q46),
  !is.na(Q49),
  !is.na(Q37),
  !is.na(Q32),
  !is.na(Q28),
  !is.na(Q190),
  !is.na(Q143),
  !is.na(W_WEIGHT),
  !is.na(Q267),
  !is.na(Q260),
  !is.na(Q262),
  !is.na(Q273),
  !is.na(Q274),
  !is.na(Q275),
  !is.na(Q275R),
  !is.na(Q279),
  !is.na(Q288),
  !is.na(Q288R),
  !is.na(Q290),
  !is.na(H_URBRURAL)
) %>% 
rename(
  family_important = Q1,
  politics_important = Q4,
  work_not_important = Q5,
  duty_have_children = Q37,
  happy_as_housewife = Q32,
  satisfaction = Q49,
  children_suffers_work_mom = Q28,
  not_acceptable_beat_children = Q190,
  worries_education_children = Q143,
  country_code = B_COUNTRY_ALPHA,
  sample_weight = W_WEIGHT,
  happy = Q46,
  mother_country = Q267,
  sex = Q260,
  age = Q262,
  marital_status = Q273,
  children = Q274,
  education_scale = Q275,
  education_level = Q275R,
  employment_status = Q279,
  income_decile = Q288,
  income_level = Q288R,
  rural_area = H_URBRURAL,
  ethnic_group = Q290
) %>% 
mutate(
  family_important = case_when(
    family_important <= 2 ~ 1, 
    TRUE ~ 0), # considera família importante ou não
  politics_important = case_when(
    politics_important <= 2 ~ 1, 
    TRUE ~ 0), # considera política importante ou não
  work_not_important = case_when(
    work_not_important > 2 ~ 1, 
    TRUE ~ 0), # considera trabalho importante ou não
  happy = case_when(
    happy <= 2 ~ 1, 
    TRUE ~ 0), # se considera feliz ou não
  duty_have_children = case_when(
    duty_have_children <= 2 ~ 1, 
    TRUE ~ 0), # concorda com dever em ter crianças ou não
  happy_as_housewife = case_when(
    happy_as_housewife <= 2 ~ 1, 
    TRUE ~ 0), # concorda em ser housewife ou não
  children_suffers_work_mom = case_when(
    children_suffers_work_mom <= 2 ~ 1, 
    TRUE ~ 0), # concorda que a criança sofre com a mãe trabalhando
  not_acceptable_beat_children = case_when(
    not_acceptable_beat_children == 1 ~ 1, 
    TRUE ~ 0), #  não concorda em bater em crianças ou sim
  worries_education_children = case_when(
    worries_education_children <= 2 ~ 1, 
    TRUE ~ 0), # se preocupa em não dar boa eduação para crianças
  age = case_when(
    age >= 0 & age <= 17 ~ 'Abaixo de 18',
    age >= 18 & age <= 30 ~ '18 - 30',
    age >= 31 & age <= 40 ~ '31 - 40',
    age >= 41 & age <= 50 ~ '41 - 50',
    age >= 61 & age <= 70 ~ '61 - 70',
    TRUE ~ 'Acima de 70'), # faixa de idade
  sex = case_when(
    sex == 1 ~ 'Masculino',
    TRUE ~ 'Feminino'
  ),
  education_level = case_when(
    education_level == 1 ~ 'Baixo',
    education_level == 2 ~ 'Médio',
    TRUE ~ 'Alto'), # nível educacional
  employment_status = case_when(
    employment_status == 1 ~ 'Integral',
    employment_status == 2 ~ 'Parcial',
    employment_status == 3 ~ 'Autônomo',
    employment_status == 4 ~ 'Aposentado',
    employment_status == 5 ~ 'Do lar',
    employment_status %in% c(6,7) ~ 'Desempregado', 
    TRUE ~ 'Outros'),
  income_quantile = case_when(
    income_decile <= 2 ~ 1,
    income_decile >= 3 & income_decile <= 4 ~ 2,
    income_decile >= 5 & income_decile <= 6 ~ 3,
    TRUE ~ 4), # quantil de renda com base no decil
  marital_status = case_when(
    marital_status %in% c(1:2) ~ 'Casada ou morando junto',
    marital_status %in% c(3:5) ~ 'Divorciada ou viúvia',
    TRUE ~ 'Solteira'), # status civil
  married = case_when(
    marital_status == 'Casada ou morando junto' ~ 1,
    TRUE ~ 0
  ),
  number_children = case_when(
    children == 0 ~ 'Sem filhos',
    children == 1 ~ 'Um filho',
    children == 2 ~ 'Dois filhos',
    TRUE ~ 'Três ou mais filhos'),
  proper_level = family_important + work_not_important + duty_have_children + happy_as_housewife + 
    children_suffers_work_mom + not_acceptable_beat_children + worries_education_children + married,
  conformity = case_when(
    proper_level >= 5 ~ 1,
    TRUE ~ 0)
  ) %>% 
mutate( # transformando todas as colunas em categóricas
  family_important = as_factor(family_important),
  politics_important = as_factor(politics_important),
  work_not_important = as_factor(work_not_important),
  duty_have_children = as_factor(duty_have_children),
  happy_as_housewife = as_factor(happy_as_housewife),
  satisfaction = as_factor(satisfaction),
  children_suffers_work_mom = as_factor(children_suffers_work_mom),
  not_acceptable_beat_children = as_factor(not_acceptable_beat_children),
  worries_education_children = as_factor(worries_education_children),
  country_code = as_factor(country_code),
  happy = as_factor(happy),
  mother_country = as_factor(mother_country),
  sex = as_factor(sex),
  age = as_factor(age),
  marital_status = as_factor(marital_status),
  married = as_factor(married),
  number_children = as_factor(number_children),
  education_scale = as_factor(education_scale),
  education_level = as_factor(education_level),
  employment_status = as_factor(employment_status),
  income_decile = as_factor(income_decile),
  income_level = as_factor(income_level),
  income_quantile = as_factor(income_quantile),
  rural_area = as_factor(rural_area),
  ethnic_group = as_factor(ethnic_group),
  conformity = as_factor(conformity)
)
  

wvs$country <- countrycode::countrycode(
  wvs$country_code, # criando variável para país
  origin = 'iso3c',
  destination = 'country.name')

svy <- svydesign(id = ~1, weights = wvs$sample_weight, data = wvs) # aplicando o peso amostral  

wvs_mulheres <- wvs %>%  
  filter(sex == 'Feminino') # filtrando somente mulheres

svy_mulheres <- svydesign(id = ~1, weights = wvs_mulheres$sample_weight, data = wvs_mulheres) # aplicando o peso amostral  


## ----------------------------------------------
svy_mulheres %>% 
  ggsurvey() +
  geom_bar(aes(x = happy, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Se considera feliz',
       y = 'Frequência relativa',
       title = 'Distribuição da felicidade entre mulheres',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_x_discrete(labels = c('Não feliz', 'Feliz'))


## ----------------------------------------------
wvs_mulheres2 <- wvs_mulheres %>% 
  mutate(
    married = case_when(
      married == 1 ~ 'Casada',
      TRUE ~ 'Não casada'
    )
  )

svy_mulheres2 <- svydesign(id = ~1, 
                           weights = wvs_mulheres2$sample_weight, 
                           data = wvs_mulheres2)
svy_mulheres2 %>% 
  ggsurvey() +
  geom_bar(aes(x = happy, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Status matrimonial',
       y = 'Frequência relativa',
       title = 'Distribuição da felicidade',
       subtitle = 'Entre casadas e solteiras',
       caption = 'Fonte: WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_x_discrete(labels = c('Não feliz', 'Feliz'))+
  facet_grid(cols = vars(married))


## ----------------------------------------------
wvs_mulheres0 <- wvs_mulheres %>% 
  mutate(
    children = case_when(
      children == 0 ~ 'Sem filhos',
      TRUE ~ 'Com filhos'))

svy_mulheres0 <- svydesign(id = ~1, 
                           weights = wvs_mulheres0$sample_weight, 
                           data = wvs_mulheres0)
svy_mulheres0 %>% 
  ggsurvey() +
  geom_bar(aes(x = happy, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Se considera feliz',
       y = 'Frequência relativa',
       title = 'Distribuição da da felicidade',
       subtitle = 'Entre mulheres com e sem filhos',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_x_discrete(labels = c('Não feliz', 'Feliz'))+
  facet_grid(cols = vars(children))


## ----------------------------------------------
wvs_mulheres_filhos <- wvs_mulheres %>%  
  filter(children != 0)
svy_mulheres_filhos <- svydesign(id = ~1, weights = wvs_mulheres_filhos$sample_weight, 
                                 data = wvs_mulheres_filhos)

wvs_mulheres_sem_filhos <- wvs_mulheres %>% 
  filter(children == 0)
svy_mulheres_sem_filhos <- svydesign(id = ~1, weights = wvs_mulheres_sem_filhos$sample_weight, 
                                     data = wvs_mulheres_sem_filhos)

a <-  svy_mulheres_filhos %>% 
  ggsurvey() +
  geom_bar(aes(x = happy, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Se considera feliz',
       y = 'Frequência relativa',
       subtitle = 'Entre mulheres com filhos') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_x_discrete(labels = c('Não feliz', 'Feliz'))


b <- svy_mulheres_sem_filhos %>% 
  ggsurvey() +
  geom_bar(aes(x = happy, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Se considera feliz',
       y = '',
       subtitle = 'Entre mulheres sem filhos',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_x_discrete(labels = c('Não feliz', 'Feliz'))

grid.arrange(arrangeGrob(a,b, ncol = 2), top = 'Distribuição da felidade') 


## ----------------------------------------------
wvs_mulheres_casadas <- wvs_mulheres %>%  
  filter(married != 0)
svy_mulheres_casadas <- svydesign(id = ~1, weights = wvs_mulheres_casadas$sample_weight, 
                                 data = wvs_mulheres_casadas)

wvs_mulheres_nao_casadas <- wvs_mulheres %>% 
  filter(married == 0)
svy_mulheres_nao_casadas <- svydesign(id = ~1, weights = wvs_mulheres_nao_casadas$sample_weight, 
                                     data = wvs_mulheres_nao_casadas)

a <-  svy_mulheres_casadas %>% 
  ggsurvey() +
  geom_bar(aes(x = happy, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Se considera feliz',
       y = 'Frequência relativa',
       subtitle = 'Entre mulheres casadas') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_x_discrete(labels = c('Não feliz', 'Feliz'))


b <- svy_mulheres_casadas %>% 
  ggsurvey() +
  geom_bar(aes(x = happy, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Se considera feliz',
       y = '',
       subtitle = 'Entre mulheres solteiras',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_x_discrete(labels = c('Não feliz', 'Feliz'))

grid.arrange(arrangeGrob(a,b, ncol = 2), top = 'Distribuição da felidade') 


## ----------------------------------------------
svy_mulheres %>% 
  ggsurvey() +
  geom_bar(aes(x = as_factor(children), y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Filhos',
       y = 'Frequência relativa',
       title = 'Distribuição da quantidade de filhos entre as mulheres',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))



## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = family_important, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Você considera a família algo importante?',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres por importância da familía',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))+
  scale_x_discrete(labels = c('Não considera família importante', 'Considera família importante'))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = duty_have_children, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'É seu dever para a sociedade ter filhos?',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres por opinião sobre o dever de ter crianças',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))+
  scale_x_discrete(labels = c('Não concorda', 'Concorda'))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = happy_as_housewife, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Ser dona de casa é tão bom quanto trabalhar fora de casa?',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres sobre trabalhar ou ser dona de casa',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))+
  scale_x_discrete(labels = c('Não concorda', 'Concorda'))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = children_suffers_work_mom, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'A criança sofre quando a mãe trabalha?',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres de acordo opinião sobre sofrimento de criança',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))+
  scale_x_discrete(labels = c('Não concorda ', 'Concorda'))


## ----------------------------------------------
svy_mulheres %>% 
  ggsurvey() +
  geom_bar(aes(x = conformity, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Conformidade',
       y = 'Frequência relativa',
       title = 'Distribuição da conformidade à teoria entre mulheres',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_x_discrete(labels = c('Não se conforma', 'Se conforma'))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = not_acceptable_beat_children, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'É válido que os pais batam em seus filhos?',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres de acordo opinião sobre bater em crianças',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))+
  scale_x_discrete(labels = c('Não concorda ', 'Concorda'))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = worries_education_children, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Se preocupa em dar boa educação para seus filhos?',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres de acordo opinião sobre boa eduação para crianças',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))+
  scale_x_discrete(labels = c('Não concorda ', 'Concorda'))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = country, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'País',
       y = 'Frequência relativa',
       title = 'Distribuição do país das mulheres',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ----------------------------------------------
svy %>%
  ggsurvey() +
  geom_bar(aes(x = sex, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Sexo',
       y = 'Frequência relativa',
       title = 'Distribuição por sexo',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = age, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Idade',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres por faixa etária',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = married, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Estado Civil',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres por estado civil',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10)) +
    scale_x_discrete(labels = c('Solteira', 'Casada'))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = fct_infreq(number_children), y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Número de Filhos',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres por número de filhos',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = education_scale, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Escolaridade',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres por escolaridade',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))+
  scale_x_discrete(labels = c('Analfabeta', 'Ensino Infantil', 'Ensino Fundamental I',
                              'Ensino Fundamental II','Ensino Médio','Superior Incompleto',
                              'Superior Completo','Mestrado | Equivalentes','Doutorado')) + 
  coord_flip()


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = education_level, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Escolaridade',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres por escolaridade',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = employment_status, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Período de Trabalho',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres por emprego',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = income_quantile, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Renda (quartil)',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres por renda',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))+
  scale_x_discrete(labels = c('Primeiro', 'Segundo', 'Terceiro','Quarto'))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = income_level, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Nível de renda',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres por nível de renda',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))+
  scale_x_discrete(labels = c('Baixa', 'Média', 'Alta'))


## ----------------------------------------------
svy_mulheres %>%
  ggsurvey() +
  geom_bar(aes(x = rural_area, y= ..count.. / sum(..count..)), fill = '#69b3a2') +
  theme_minimal() +
  labs(x = 'Nível de urbanização',
       y = 'Frequência relativa',
       title = 'Distribuição de mulheres por nível de urbanização no local de residência',
       caption = 'WVS (wave 7)') +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10))+
  scale_x_discrete(labels = c('Urbano', 'Periferia urbana', 'rural'))


## ---- warning=FALSE----------------------------
modelo_mpl <- svyglm(as.numeric(happy) ~ conformity + age + income_quantile + number_children + 
                       education_level  + employment_status, 
                     design=svy_mulheres,
                     family=stats::gaussian(link="identity")) # modelo MPL

modelo_logit <- svyglm(happy ~ conformity +  age + income_quantile + number_children + 
                         education_level  + employment_status, 
                     design=svy_mulheres,
                     family=binomial(link="logit")) # modelo logit

modelo_probit <- svyglm(happy ~ conformity +  age + income_quantile + number_children + 
                        education_level  + employment_status, 
                      design=svy_mulheres,
                      family=binomial(link="probit")) # modelo probit


## ----------------------------------------------
margins_logit <- margins(modelo_logit, design=svy_mulheres) # efeitos marginais de logit

margins_probit <- margins(modelo_probit, design=svy_mulheres) # efeitos marginais de probit


## ----------------------------------------------
summary(modelo_logit) # tabela de coeficientes para logit
summary(margins_logit) # tabela de efeitos marginais para logit


## ----------------------------------------------
summary(modelo_probit) # tabela de coeficientes para probit
summary(margins_probit) # tabela de efeitos marginais para probit


## ----------------------------------------------
odds_logit <- exp(cbind(coef(modelo_logit),confint(modelo_logit))) # odds para o modelo logit
stargazer(odds_logit, summary = FALSE)


## ----------------------------------------------
stargazer(modelo_mpl, modelo_logit, modelo_probit) # tabela de coeficientes dos três modelos


## ----------------------------------------------
stargazer(summary(margins_logit), summary = FALSE) # tabela de efeitos marginais do modelo logit


## ----------------------------------------------
stargazer(summary(margins_probit), summary = FALSE) # tabela de efeitos marginais do moelo probit


## ----------------------------------------------
roc_logit <- rocit(score = modelo_logit$fitted.values, class = modelo_logit$y) # obtendo a curva roc do modelo logit

measure_logit <- measureit(roc_logit, measure = c("ACC","SENS","SPEC")) # obtendo acurácia, sensibilidade e especificidade do modelo logit

#Acurácia total do modelo logit
plot(measure_logit$Cutoff,measure_logit$ACC)

#Focando especificamente na sensibilidade do modelo logit
plot(measure_logit$Cutoff,measure_logit$SENS)

#Focando na especificidade do modelo logit
plot(measure_logit$Cutoff,measure_logit$SPEC)

plot(roc_logit) # plotando a curva roc do modelo logit


## ----------------------------------------------
hoslem.test(modelo_logit$y,fitted(modelo_logit),g=10)


## ----------------------------------------------
roc_probit <- rocit(score = modelo_probit$fitted.values, class = modelo_probit$y) # obtendo a curva roc do modelo probit

measure_probit <- measureit(roc_probit, measure = c("ACC","SENS","SPEC")) # obtendo acurácia, sensibilidade e especificidade do modelo probit

#Acurácia total do modelo probit
plot(measure_probit$Cutoff,measure_probit$ACC)

#Focando especificamente na sensibilidade do modelo probit
plot(measure_probit$Cutoff,measure_probit$SENS)

#Focando na especificidade do modelo probit
plot(measure_probit$Cutoff,measure_probit$SPEC)

plot(roc_probit) # plotando a curva roc do modelo probit


## ----------------------------------------------
hoslem.test(modelo_probit$y,fitted(modelo_probit),g=10)

