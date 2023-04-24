title: "Micro IV - Trabalho Final"
subtitle: "O impacto da legalização do casamento LGBT sobre a opinião pública na Finlândia e Islândia"
author: "Victor Alves"
date: "2022-10-28"
output:
  html_document:
  toc: yes
toc_depth: 2
pdf_document:
  toc: yes
toc_depth: '2'
---
  
  ```{r, warning=FALSE, message=FALSE}
library(tidyverse)
library(haven)
library(survey)
library(questionr)
library(Synth)
library(stargazer)
library(sandwich)
library(optmatch)
library(lmtest)
library(Matching) 
library(MatchIt)
library(boot)
library(dplyr)
library(tweenr)
library(ggthemes)
library(viridis)
library(rgeos)
library(countrycode)
library(devtools)
library(data.table)
library(rworldmap)
library(curl)
library(GGally) 
library(rgdal) 
library(usmap) 
library(magick)
library(gridExtra)
library(grid)
library(gridtext)
```
# Importação e manipulação da base de dados

```{r}
ivs <- read_dta('Bases/IVS/Integrated_values_surveys_1981-2021.dta')
```

```{r}
ivs_nordic <- ivs %>% 
  filter(S009 %in% c('IS', 'FI')) %>% 
  mutate(
    S002VS = as.numeric(S002VS),
    wave = case_when(
      S020 %in% c(1981:1984) ~ 1,
      S020 %in% c(1989:1993) ~ 2,
      S020 %in% c(1994:1998) ~ 3,
      S020 %in% c(1999:2004) ~ 4,
      S020 %in% c(2005:2009) ~ 5,
      S020 %in% c(2010:2014) ~ 6,
      S020 %in% c(2017:2019) ~ 7,
    ),
    ano_tratamento = case_when(
      wave >= 5 ~ 1,
      TRUE~ 0),
    grupo  = case_when(
      S009 %in% c('SE', 'NO', 'DK', 'IS') ~ 1,
      S009 %in% c('FI') ~ 0),
    ano_tratamento = as.factor(ano_tratamento)
  ) %>% 
  dplyr::select(wave, ano_tratamento,
                S020, S009, S017, pwght,
                A006,
                F118,
                X001, X003, X007, X025, X028, X047R_EVS,
                grupo) %>% 
  drop_na() %>% 
  rename(importancia_religiao = A006,
         aceitacao_homossexualidade = F118,
         sexo = X001,
         idade = X003,
         estado_civil = X007,
         educacao = X025,
         ocupacao = X028,
         renda = X047R_EVS) %>% 
  filter(wave %in% c(4:7)) %>% 
  mutate(
    importancia_religiao = as_factor(importancia_religiao),
    sexo = as_factor(sexo),
    estado_civil = as_factor(estado_civil),
    educacao = as_factor(educacao),
    ocupacao = as_factor(ocupacao),
    renda = as_factor(renda)
  )

svy_nordicos <- svydesign(id = ~1, weights = ivs_nordic$S017, data = ivs_nordic)
```

```{r}
ivs_nordicos_media <- svyby(~aceitacao_homossexualidade, 
                            ~wave+grupo, svy_nordicos, svymean)

ivs_nordicos_media %>%
  ggplot(aes(wave, aceitacao_homossexualidade)) + 
  geom_point() +
  geom_smooth(aes(colour = as.factor(grupo)), se = F) +
  labs(title = 'Evolução do nível de aceitação de homossexualidade', 
       y = 'Grau de aceitação de homossexualidade', 
       x = 'Wave', 
       color = 'Grupo',
       caption = 'Integrated Value Survey') +
  scale_colour_discrete(labels = c("Controle", "Tratamento")) +   
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10)) + 
  theme_minimal()

ivs_nordicos_media %>%
  ggplot(aes(wave, aceitacao_homossexualidade)) + 
  geom_line(aes(colour = as.factor(grupo))) +
  labs(title = 'Evolução do nível de aceitação de homossexualidade', 
       subtitle = "Pré matching",
       y = 'Grau de aceitação de homossexualidade', 
       x = 'Wave', 
       color = 'Grupo',
       caption = 'Integrated Value Survey') +
  theme_minimal() +
  scale_colour_discrete(labels = c("Controle", "Tratamento")) +   
  theme(
    plot.title=element_text(size=20, face="bold", hjust = 0.5),
    legend.text=element_text(size=10),
    axis.text=element_text(size=10),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10))  
```
# Descritivas

```{r}
ivs_nordic_descritivas <- ivs %>% 
  filter(S009 %in% c('SE', 'NO', 'DK', 'IS', 'FI')) %>% 
  mutate(
    S002VS = as.numeric(S002VS),
    wave = case_when(
      S020 %in% c(1981:1984) ~ 1,
      S020 %in% c(1989:1993) ~ 2,
      S020 %in% c(1994:1998) ~ 3,
      S020 %in% c(1999:2004) ~ 4,
      S020 %in% c(2005:2009) ~ 5,
      S020 %in% c(2010:2014) ~ 6,
      S020 %in% c(2017:2019) ~ 7,
    )
  ) %>% 
  dplyr::select(wave, S020, S009, S017,
                F118) %>% 
  drop_na() %>% 
  rename(
    aceitacao_homossexualidade = F118)

svy_nordicos_descritiva <- svydesign(id = ~1, 
                                     weights = ivs_nordic_descritivas$S017, 
                                     data = ivs_nordic_descritivas)

ivs_nordicos_descritiva_media <- svyby(~aceitacao_homossexualidade, 
                                       ~wave+S009, 
                                       svy_nordicos_descritiva, svymean)



```


```{r}
# get world map
wmap <- getMap(resolution="low")

# small edits
wmap <- spTransform(wmap, CRS("+proj=robin")) # reproject
wmap <-   subset(wmap, 
                 (NAME %in% 
                    c('Norway', 'Iceland', 
                      'Sweden', 'Finland', 'Denmark')))

# get centroids of countries
centroids <- gCentroid( wmap , byid=TRUE, id = wmap@data$ISO3)
centroids <- data.frame(centroids)
setDT(centroids, keep.rownames = TRUE)[]
setnames(centroids, "rn", "S009")

# join data to map
wmap_df <- fortify(wmap, region = "ISO_A2")
wmap_df <- left_join(wmap_df, 
                     ivs_nordicos_descritiva_media, 
                     by = c('id'='S009'))
wmap_df <- left_join(wmap_df, centroids, 
                     by = c('id'='S009')) # centroids
```


```{r}
devtools::install_github('thomasp85/gganimate')
library(gganimate)
devtools::install_github('thomasp85/transformr')
library(transformr)

o <- wmap_df %>% 
  ggplot()+
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = aceitacao_homossexualidade))  +
  geom_text(aes(x = x, y = y, label = id), hjust=0, vjust=0, size = 4.5) +
  scale_fill_viridis(
    name="Grau de aceitação",
    begin = 0, end = 1,
    limits = c(min(wmap_df$aceitacao_homossexualidade),
               max(wmap_df$aceitacao_homossexualidade)),
    na.value="black") +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(title = "Grau de aceitação de homossexualidade",
       subtitle = "Wave: {round(frame_time)}",
       caption = "Fonte: Integrated Values Survey (1981-2020)") +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top")) +
  coord_cartesian(xlim = c(-1740000, 5800000)) +
  theme(
    panel.background = element_rect(fill = "#E4F0FC", 
                                    size = 0.5),
    legend.position = c(0.9, 0.5),
    legend.direction = "vertical", 
    legend.title.align = 0,
    legend.key.size = unit(2, "cm"),
    legend.title=element_text(size=12, face="bold"),
    legend.text=element_text(size=12),
    plot.title=element_text(size=20, face="bold", hjust = 0.5),
    plot.subtitle=element_text(size=14, hjust = 0.5),
    plot.caption = element_text(hjust = 0, face="bold", size=12)
  ) +
  transition_time(wave)

o_animado <- animate(o, nframes = 75, height = 600, width = 600)

anim_save("paises.gif")

o_animado

```

# PSM

## Pré análise



### Analisando os covariados

```{r}
ivs_nordic_cov <- c('importancia_religiao', 
                    'sexo', 'idade', 'estado_civil', 'educacao', 
                    'ocupacao', 'renda')
```

```{r}
t.test(ivs_nordic$idade ~ ivs_nordic$grupo)
```

## Estimação do PSM

```{r, warning=FALSE}
m_ps <- svyglm(grupo ~  importancia_religiao + sexo + idade + 
                 estado_civil + educacao + ocupacao + renda + wave,
               design=svy_nordicos,
               family=binomial(link="logit"))
```

```{r}
prs_df <- data.frame(
  pr_score = predict(m_ps, type = "response"),
  grupo = m_ps$model
)
```

### Suporte comum

```{r}
labs <- paste("Legalização do casamento LGBT:", c("Legalizou", "Não legalizou"))
suporte_comum <- prs_df %>%
  mutate(grupo = ifelse(grupo.grupo == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score.response)) +
  geom_density(aes(fill = grupo), alpha = 0.5) +
  labs(x = "Probabilidade de participar do grupo que legalizou", 
       y = "Densidade", 
       title = "Suporte comum",
       caption = "Integrated Value Survey (dados via matching)")
theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
      legend.text=element_text(size=8),
      axis.text=element_text(size=8),
      axis.title = element_text(size = 10),
      legend.title = element_text(size = 10)) + 
  theme_minimal()

ggsave('suporte_comum.png')
suporte_comum
```

```{r}
proporcao_psm <- prs_df %>%
  count(grupo.grupo, grupo.importancia_religiao, grupo.sexo, 
        grupo.idade, grupo.estado_civil, 
        grupo.educacao, grupo.ocupacao, grupo.renda) %>%
  group_by(grupo.grupo) %>%          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n))
```


## Matching

```{r}
ivs_nordic_nomiss <- ivs_nordic %>% 
  dplyr::select(aceitacao_homossexualidade, grupo, 
                one_of(ivs_nordic_cov), S017, ano_tratamento, wave) %>%
  na.omit()

mod_match <- matchit(
  grupo ~ importancia_religiao + sexo + idade + estado_civil +
    educacao + ocupacao + renda + wave,
  method = "cem",
  link = 'probit',
  replace = F,
  s.weights = ivs_nordic_nomiss$S017,
  data = ivs_nordic_nomiss)

mod_match_radius <- matchit(
  grupo ~ importancia_religiao + sexo + idade + estado_civil + 
    educacao + ocupacao + renda + wave, 
  link = 'probit',
  discard="both",
  replace = TRUE,
  ratio=10000,
  caliper=0.0001,
  std.caliper=FALSE,
  s.weights = ivs_nordic_nomiss$S017,
  data = ivs_nordic_nomiss)
```

```{r}
dta_m <- match.data(mod_match)
dta_m <- dta_m %>% 
  mutate(ano_tratamento = as.numeric(ano_tratamento),
         estimador_did = ano_tratamento*grupo)
```

## Balançeamento dos covariados


### Análise de proporções

```{r}
t.test(dta_m$idade ~ dta_m$grupo)
```

```{r}
dta_m2 <- dta_m %>% 
  mutate(
    controle_tratamento = 
      case_when(
        grupo == 0 ~ "Controle",
        grupo == 1 ~ "Tratamento"
      ))


dta_m2 %>% 
  ggplot() +
  geom_bar(aes(x = sexo, 
               y= ..count.. / sum(..count..), fill = controle_tratamento),
           alpha = 0.5,
           position = 'identity') +
  theme_minimal() +
  labs(x = 'Sexo do indivíduo',
       y = 'Frequência relativa',
       title = 'Distribuição do sexo entre os indivíduos',
       caption = 'Integrated Value Survey (dados via matching)',
       fill = "Grupo") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) 

dta_m2 %>% 
  ggplot() +
  geom_bar(aes(x = importancia_religiao, 
               y= ..count.. / sum(..count..), fill = controle_tratamento),
           alpha = 0.5,
           position = 'identity') +
  theme_minimal() +
  labs(x = 'Importância da religião',
       y = 'Frequência relativa',
       title = 'Distribuição da importância da religião',        
       caption = 'Integrated Value Survey (dados via matching)',
       fill = "Grupo") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) 

dta_m2 %>% 
  ggplot() +
  geom_density(aes(x = idade, fill = controle_tratamento),
               alpha = 0.5,
               position = 'identity') +
  theme_minimal() +
  labs(x = 'Idade',
       y = 'Frequência relativa',
       title = 'Distribuição da idade',
       caption = 'Integrated Value Survey (dados via matching)',
       fill = "Grupo") + 
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)) 

dta_m2 %>% 
  ggplot() +
  geom_bar(aes(x = estado_civil, 
               y= ..count.. / sum(..count..), fill = controle_tratamento),
           alpha = 0.5,
           position = 'identity') +
  theme_minimal() +
  labs(x = 'Estado civil',
       y = 'Frequência relativa',
       title = 'Distribuição do estado civil',       
       caption = 'Integrated Value Survey (dados via matching)',
       fill = "Grupo") + 
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12))

dta_m2 %>% 
  ggplot() +
  geom_bar(aes(x = educacao, 
               y= ..count.. / sum(..count..), fill = controle_tratamento),
           alpha = 0.5,
           position = 'identity') +
  theme_minimal() +
  labs(x = 'Nível educacional',
       y = 'Frequência relativa',
       title = 'Distribuição do nível educacional',        
       caption = 'Integrated Value Survey (dados via matching)',
       fill = "Grupo") + 
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12))

dta_m2 %>% 
  ggplot() +
  geom_bar(aes(x = ocupacao, 
               y= ..count.. / sum(..count..), fill = controle_tratamento),
           alpha = 0.5,
           position = 'identity') +
  theme_minimal() +
  labs(x = 'Ocupação',
       y = 'Frequência relativa',
       title = 'Distribuição do grupo ocupacional',       
       caption = 'Integrated Value Survey (dados via matching)',
       fill = "Grupo") + 
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12))

dta_m2 %>% 
  ggplot() +
  geom_bar(aes(x = renda, 
               y= ..count.. / sum(..count..), fill = controle_tratamento),
           alpha = 0.5,
           position = 'identity') +
  theme_minimal() +
  labs(x = 'Decil de renda',
       y = 'Frequência relativa',
       title = 'Distribuição dos decis de renda',
       caption = 'Integrated Value Survey (dados via matching)',
       fill = "Grupo") +
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12))
```


```{r}
svy_matched <- svydesign(id = ~1, weights = dta_m$weights, data = dta_m)

dta_m_media <- svyby(~aceitacao_homossexualidade, 
                     ~wave+grupo, svy_matched, svymean)

dta_m_media %>%
  ggplot(aes(wave, aceitacao_homossexualidade)) + 
  geom_point() +
  geom_smooth(aes(colour = as.factor(grupo)), se = F) +
  labs(title = 'Evolução do nível de aceitação de homossexualidade', 
       y = 'Grau de aceitação de homossexualidade', 
       x = 'Wave', 
       color = 'Grupo',
       caption = 'Integrated Value Survey (dados via matching)') +
  scale_colour_discrete(labels = c("Controle", "Tratamento")) +   
  theme(plot.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10)) + 
  theme_minimal()

dta_m_media %>%
  ggplot(aes(wave, aceitacao_homossexualidade)) + 
  geom_line(aes(colour = as.factor(grupo))) +
  labs(title = 'Evolução do nível de aceitação de homossexualidade', 
       subtitle = "Pós matching",
       y = 'Grau de aceitação de homossexualidade', 
       x = 'Wave', 
       color = 'Grupo',
       caption = 'Integrated Value Survey (dados via matching)') + 
  theme_minimal() +
  scale_colour_discrete(labels = c("Controle", "Tratamento")) +   
  theme(plot.title=element_text(size=20, face="bold", hjust = 0.5),
        legend.text=element_text(size=10),
        axis.text=element_text(size=10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10))
```

```{r}
antes <- ivs_nordicos_media %>%
  ggplot(aes(wave, aceitacao_homossexualidade)) + 
  geom_line(aes(colour = as.factor(grupo))) +
  labs(subtitle = 'Pré matching', 
       y = 'Grau de aceitação de homossexualidade', 
       x = 'Wave', 
       color = 'Grupo') +
  scale_colour_discrete(labels = c("Controle", "Tratamento")) +   
  theme(plot.title=element_text(size=10, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10)) + 
  guides(color = "none")+ 
  theme_minimal()


depois <- dta_m_media %>%
  ggplot(aes(wave, aceitacao_homossexualidade)) + 
  geom_line(aes(colour = as.factor(grupo))) +
  labs(subtitle = 'Pós matching', 
       y = '', 
       x = 'Wave', 
       color = 'Grupo') +
  scale_colour_discrete(labels = c("Controle", "Tratamento")) +   
  theme(plot.title=element_text(size=10, face="bold", hjust = 0.5),
        legend.text=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank()) +
  theme_minimal()


tendencias <- gridExtra::grid.arrange(antes,depois, ncol = 2, top = 'Tendências paralelas') 

ggsave('tendencias_paralelas.png', tendencias, width = 12, height = 7)
tendencias
```


## Estimando o efeito do tratamento

```{r}
lm_treat1 <- lm(aceitacao_homossexualidade ~ wave + grupo + estimador_did, 
                data = dta_m,
                weights = dta_m$weights)
summary(lm_treat1)
```

```{r}
lm_treat2 <- lm(aceitacao_homossexualidade ~ wave + grupo + estimador_did +
                  importancia_religiao + sexo + idade + 
                  estado_civil + educacao +
                  ocupacao + renda,
                data = dta_m,
                weights = dta_m$weights)
summary(lm_treat2)
```

```{r}
stargazer(lm_treat1, lm_treat2, type = "html")
```


### Teste de placebo

```{r}
dta_m_placebo <- dta_m %>% 
  filter(wave %in% c(4,5)) %>% 
  mutate(
    wave_fake = case_when(
      wave == 4 ~ 0,
      wave == 5 ~ 1 
    ),
    estimador_did_fake = wave_fake*grupo
  )

lm_placebo <- lm(aceitacao_homossexualidade ~ wave_fake + grupo + estimador_did_fake  + 
                   importancia_religiao + 
                   sexo + idade + 
                   estado_civil + educacao +
                   ocupacao + renda,
                 data = dta_m_placebo,
                 weights = dta_m_placebo$weights)
summary(lm_placebo)
```