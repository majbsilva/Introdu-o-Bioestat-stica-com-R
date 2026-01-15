# =============================================================================
# Aula: Análise de variância (ANOVA) - Passo a passo
# Curso: Estatística aplicada
# Assunto: Demonstração prática do raciocínio por trás da ANOVA
# Objetivo: Ilustração detalhada do cálculo manual da ANOVA para dois fatores (Gênero x Consumo de álcool),
#           destacando a decomposição da variância total (SSt) em efeitos principais e interação.
# =============================================================================

# =============================================================================
# Carregando bibliotecas
# =============================================================================
library(modelsummary)
library(tidyverse)

# =============================================================================
# Importando os dados
# =============================================================================
ds <- foreign::read.spss('data/goggles.sav', to.data.frame = T, use.value.labels = T)

# =============================================================================
# Dados brutos (exemplo de valores para cálculo manual)
# =============================================================================
valor <- ds %>% pull(Attractiveness)

# =============================================================================
# Cálculo da soma total dos quadrados (SSt)
# =============================================================================
var(valor)
sum((valor - mean(valor))^2) # SSt é a variância * (N-1)
SSt <- var(valor)*(length(valor) - 1)
df <- (length(valor) - 1) # N - 1

# =============================================================================
# Sumarização e balanceamento dos dados para obtenção da média de cada grupo
# =============================================================================
modelsummary::datasummary_balance(Attractiveness~Alcohol, data = ds[ds$Gender == 'Female',])

# =============================================================================
# Cálculo da soma dos quadrados do modelo (SSm)
# =============================================================================
# SSm é a somatória (média de cada grupo - a grande média)^2 * o n de cada grupo
SSm <- 8*(66.9-58.33)^2 + 8*(66.9-58.33)^2 + 8*(35.6-58.33)^2 + 8*(60.6-58.33)^2 + 8*(62.5-58.33)^2 + 8*(57.5-58.33)^2
# Graus de liberdade é o número de grupos - 1 (6-1)
# Nesse ponto, sabemos que o modelo (nossa manipulação experimental) pode explicar 5494.18 unidades de variância dentro do total de unidades de variância (SSt) de 8966.66.

# =============================================================================
# Efeito principal: gênero
# =============================================================================
# Vamos separar os valores entre os dois grupos "Male" e "Female"
male <- ds %>% filter(Gender == 'Male') %>% pull(Attractiveness)
female <- ds %>% filter(Gender == 'Female') %>% pull(Attractiveness)
mean_male <- mean(male)
mean_female <- mean(female)
SSgender <- 24*(56.45 - 58.33)^2 + 24*(60.20 - 58.33)^2
# Os graus de liberdade é o número de grupos - 1 (2 - 1)
# GENDER pode explicar 168.75 unidades de variância dentro do total de unidades de variância (SSt)

# =============================================================================
# Efeito principal: álcool
# =============================================================================
none <- ds %>% filter(Alcohol == 'None') %>% pull(Attractiveness)
doispit <- ds %>% filter(Alcohol == '2 Pints') %>% pull(Attractiveness)
quatropit <- ds %>% filter(Alcohol == '4 Pints') %>% pull(Attractiveness)
mean_none <- mean(none)
mean_2pit <- mean(doispit)
mean_4pit <- mean(quatropit)
SSalcohol <- 16*(63.75-58.33)^2 + 16*(64.68-58.33)^2 + 16*(46.56-58.33)^2
# Os graus de liberdade são o número de grupos - 1 (3 - 1)
# ALCOHOL pode explicar 3331.70 unidades de variância dentro do total de unidades de variância (SSt)

# =============================================================================
# Efeito da interação: gênero * álcool
# =============================================================================
# Baseado nisso, não podemos esquecer que SSm = SSgender + SSalcohol + SSgender*alcohol
SSgender_alcohol <- SSm - SSgender - SSalcohol
# O grau de liberdade da interação é a multiplicação do grau de liberdade de cada variável (dfgender = 1 * dfalcohol = 2)

# =============================================================================
# Cálculo da soma dos quadrados residual (SSr)
# =============================================================================
# Ele é calculado pela soma da variância de cada grupo * (n do grupo - 1)
# Obtendo o vetor de cada um dos 6 grupos
male_none <- ds %>% filter(Gender == 'Male' & Alcohol == 'None') %>% pull(Attractiveness)
male_doispit <- ds %>% filter(Gender == 'Male' & Alcohol == '2 Pints') %>% pull(Attractiveness)
male_quatropit <- ds %>% filter(Gender == 'Male' & Alcohol == '4 Pints') %>% pull(Attractiveness)
female_none <- ds %>% filter(Gender == 'Female' & Alcohol == 'None') %>% pull(Attractiveness)
female_doispit <- ds %>% filter(Gender == 'Female' & Alcohol == '2 Pints') %>% pull(Attractiveness)
female_quatropit <- ds %>% filter(Gender == 'Female' & Alcohol == '4 Pints') %>% pull(Attractiveness)

SSr <- var(male_none)*(length(male_none) - 1) + var(male_doispit)*(length(male_doispit) - 1) + var(male_quatropit)*(length(male_quatropit) - 1) + var(female_none)*(length(female_none) - 1) + var(female_doispit)*(length(female_doispit) - 1) + var(female_quatropit)*(length(female_quatropit) - 1)
# Os graus de liberdade do SSr é o número de indivíduos por grupo - 1 (8-1) para cada grupo. Como são 6*(8-1) = 42
# SSr é a variância que não é explicada por GENDER ou ALCOHOL.

# =============================================================================
# Cálculo dos quadrados médios (MS) e razão F
# =============================================================================
MSgender <- SSgender / 1 # SSgender dividido pelos graus de liberdade de gender (número grupos - 1)
MSalcohol <- SSalcohol / 2 # SSalcohol dividido pelos graus de liberdade de alcohol (número grupos - 1)
MSgender_alcohol <- SSgender_alcohol / 2 # SS da interação dividido pelos graus de liberdade da interação (graus de liberdade de gender * graus liberdade alcohol)
MSr <- SSr / 42 # SSr dividido pelos graus de liberdade de SSr. Os graus de liberdade do SSr é o número de indivíduos por grupo - 1 (8-1)
# Agora, calculando o F ratio
Fgender <- MSgender / MSr
Falcohol <- MSalcohol / MSr
Fgender_alcohol <- MSgender_alcohol / MSr
# Sendo assim, os Fratios (Fgender, Falcohol, Fgender_alcohol) representam a proporção que aquela variável consegue explicar a variância no modelo em relação ao que não consegue explicar (MSr)

# =============================================================================
# Conclusão
# =============================================================================
# Cada um desses valores F pode ser comparado com valores críticos (baseados em seus graus de liberdade, que podem ser diferentes para cada efeito) para determinar se esses efeitos provavelmente refletem dados surgidos por acaso ou se indicam realmente um efeito das nossas manipulações experimentais (esses valores críticos podem ser encontrados no Apêndice).
# Se o valor F observado for maior que o valor crítico correspondente, então ele é estatisticamente significativo.
# O R calculará automaticamente cada um desses valores F e sua significância exata.


#=============================================================================
# Realizando a análise fatorial
# =============================================================================
library(hrbrthemes)
library(ggthemes)
library(rstatix)

ds %>% 
  ggplot(aes(x = Alcohol, y = Attractiveness, shape = Gender)) +
  stat_summary(aes(group = Gender), fun = mean, geom = 'line') +
  stat_summary(aes(group = Gender), fun = mean, geom = 'point', size = 5) +
  stat_summary(aes(group = Gender), fun.data = mean_sdl, geom = 'errorbar', fun.args = list(mult = 0.5), width = 0.3) +
  theme_ipsum(base_size = 22, axis_title_size = 20,) +
  theme(legend.position = 'top')

ds %>% 
  ggplot(aes(x = Alcohol, y = Attractiveness)) +
  geom_boxplot() + 
  facet_grid(~Gender)

ds %>% group_by(Gender) %>% get_summary_stats(Attractiveness)
ds %>% group_by(Alcohol) %>% get_summary_stats(Attractiveness)
ds %>% group_by(Gender,Alcohol) %>% get_summary_stats(Attractiveness)

ds %>% levene_test(Attractiveness~Gender)
ds %>% levene_test(Attractiveness~Alcohol)
ds %>% levene_test(Attractiveness~Gender*Alcohol)
