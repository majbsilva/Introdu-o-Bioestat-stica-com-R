library(tidyverse)

colonias <- readxl:: read_xlsx('dados/dados_colonias_emilia.xlsx')
glimpse(colonias)

ds <- pivot_longer(
  colonias,
  cols = c(3:8),
  names_to = 'tratamento',
  values_to = 'valor'
)

ds <- ds %>% 
  mutate(
    tratamento = factor(tratamento)
  )

View(ds)

c3a_ds <- ds %>% filter(linhagem == 'UM-HMC3A')

modelo_3a <- glm(valor ~ tratamento*tipo_tratamento, family = poisson(link = 'log'), data = c3a_ds)
summary(modelo_3a)


# Verificando overdispersão
# O modelo de regressão de Poisson assume que a variância é igual à média. Se seus dados apresentam superdispersão (variância > média), devo considerar usar family = quasipoisson() ou family = negative.binomial().

deviance_residual <- deviance(modelo_3a)
graus_liberdade <- df.residual(modelo_3a)
dispersão <- deviance_residual/graus_liberdade

# Eu tenho uma overdispersao. Portanto, vou fazer um modelo glm com qausipoisson. Computacionalmente ele é mais leve. Ele relaxa com relação a suposição de que a variância é igual a média (como pressuposto pela distribuição Poisson). Não é uma distribuição de probabilidade, portanto, não entrega intervalos de confiança. Usar quando a dispersão é leve ou moderada, ou só preciso de inferência sobre as médias. 

modelo_quasi <- glm(valor~tratamento*tipo_tratamento, family = quasipoisson(link = 'log'), data = c3a_ds)
summary(modelo_quasi)

# Eu tenho uma overdipersao. Portanto, vou fazer um modelo glm com distribuição binomial negativa. Interessante para quando tenho uma overdispersao severa, suspeita de variabilidade entre indivíduos ou grupos (heterogeneidade não observada) e quando preciso de previsões probabilisticas (ex: intervalos de confiança)
modelo_binomial_neg <- MASS::glm.nb(valor~tratamento*tipo_tratamento, data = c3a_ds)
summary(modelo_binomial_neg)

# Comparando a qualidade dos modelos entre eles
anova(modelo_3a,modelo_binomial_neg, modelo_quasi)

library(emmeans)

marginal_means <- emmeans(modelo_binomial_neg, specs = ~tratamento|tipo_tratamento, type = 'response')

summary(marginal_means)
pairs(marginal_means, adjust = 'tukey')

# ajuda dele ?plot.emmGrid

plot(
  marginal_means,
  comparisons = T,
  type = "response",
  by = "tipo_tratamento",
  conf.int = TRUE,
  interval = 0.95,
  colors = c("tomato4", "blue", "blue", "blue"),
  xlab = 'Médias marginais',
  ylab = "Tratamento"
)

