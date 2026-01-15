# =============================================================================
# Aula: Análise Estatística com Teste para MAIS de dois grupos com co-variável
# Curso: Estatística Aplicada
# Assunto: ANCOVA  - Estudo de Caso - Viagra e efeito no libido.
# Avaliação da variação de dose do viagra na líbido e como a líbido do parceiro influencia.
# =============================================================================

# ANCOVA é um tipo de análise controlando o efeito de uma co-variável em cima do efeito principal.

# =============================================================================
# Carregando Bibliotecas
# =============================================================================
library(tidyverse)
library(rstatix)
library(emmeans)
library(WRS2)
library(performance)

# =============================================================================
# Importando os dados
# =============================================================================

ViagraCovariate <- foreign::read.spss('dados/ViagraCovariate.sav',use.value.labels = T, to.data.frame = T)

ds <- ViagraCovariate


# =============================================================================
# Sumarização e Balanceamento dos Dados
# =============================================================================
modelsummary::datasummary_skim(ds, dinm=F)
modelsummary::datasummary_balance(Libido ~ Dose, data = ds, dinm=F)
modelsummary::datasummary_balance(Partner_Libido ~ Dose, data = ds, dinm=F)

# =============================================================================
# Visualização inicial dos dados
# =============================================================================
ds %>%
  ggplot(aes(x = Dose, y = Partner_Libido)) +
  geom_boxplot()

ds %>%
  ggplot(aes(x = Dose, y = Libido)) +
  geom_boxplot()

# =============================================================================
# Testes de Homogeneidade de Variância
# =============================================================================
ds %>% levene_test(Libido ~ Dose)
ds %>% levene_test(Partner_Libido ~ Dose)

# =============================================================================
# Avaliação da possibilidade do uso da covariável
# =============================================================================
# O teste ANOVA entre os grupos não pode ser significativo para usar Partner_Libido como covariável.
ds %>% anova_test(Partner_Libido ~ Dose)

# =============================================================================
# Modelagem ANCOVA
# =============================================================================
# ANCOVA com Dose e Partner_Libido (demonstrando a importância da escolha do tipo de SUM of SQUARES) (UTILIZANDO O TIPO 1)
ds %>% anova_test(Libido ~ Dose + Partner_Libido, effect.size = 'pes', type = 1)
ds %>% anova_test(Libido ~ Partner_Libido + Dose, effect.size = 'pes', type = 1)

# Agora, utilizando o a SUM OF SQUARES TIPO 2
ds %>% anova_test(Libido ~ Dose + Partner_Libido, effect.size = 'pes', type = 2)
ds %>% anova_test(Libido ~ Partner_Libido + Dose , effect.size = 'pes', type = 2)

# ANCOVA com interação (Dose * Partner_Libido)
ds %>% anova_test(Libido ~ Dose * Partner_Libido, effect.size = 'pes', type = 3)


# =============================================================================
# Ajuste do Modelo Linear e cálculo do tamanho do efeito
# =============================================================================
contrasts(ds$Dose) <- contr.helmert(3)
modelo <- aov(Libido ~ Partner_Libido + Dose, data = ds)
Anova(modelo, type = 2) # Tipo de soma dos quadrados para o modelo gerado no aov

effectsize::eta_squared(modelo, partial = T, alternative = 'two.sided')

# O que significa o partial eta squared: Ele mostra a proporção da variação que explicada por uma variável que não é explicada por outra. Baseado no resultado abaixo, após controlar o efeito Dose, a variável Partner_libido explica 8% da variância residual (nao explicada por outros fatores) na Libido. Após controlar o efeito Partner_Libido, a variável Dose explica 24% (0.24) da variância residual (não exlicada por outros fatores) na Libido.

# O que significa o eta squared: Ele é mais simples de entender. A variável Partner_Libido explica 6% (0.06) da variância total observada em Libido. E a variável Dose explica 23% (0.26) da variância total observada em Libido. 


# =============================================================================
# Sumarização e médias ajustadas
# =============================================================================
emmeans(modelo, specs = 'Dose', type = 'response')
summary.lm(modelo)

# =============================================================================
# Visualização da relação entre Libido e Partner_Libido
# =============================================================================
ds %>%
  ggplot(aes(Partner_Libido, Libido)) +
  geom_point() +
  geom_smooth(method = 'lm')

# =============================================================================
# Post Hoc nas médias ajustadas
# =============================================================================
emmeans(modelo, pairwise ~ Dose)

# =============================================================================
# Diagnóstico do Modelo
# =============================================================================
plot(modelo)
performance::check_model(modelo)
performance::check_outliers(modelo)
performance::check_heteroscedasticity(modelo)

# =============================================================================
# Checagem da Homogeneidade dos Slopes
# =============================================================================
ds %>%
  ggplot(aes(Partner_Libido, Libido, color = Dose)) +
  geom_point() +
  geom_smooth(method = 'lm')

# =============================================================================
# Modelo com interação
# =============================================================================
modelo1 <- aov(Libido ~ Partner_Libido * Dose, data = ViagraCovariate)
Anova(modelo1, type = 3) # Avalia a interação entre fator e cofator
effectsize::eta_squared(modelo1, partial = T, alternative = 'two.sided')

# =============================================================================
# ANCOVA Robusta
# =============================================================================

modelo2 <- WRS2::t1way(Libido ~ Dose, cov = ds$Partner_Libido, data = ds, nboot = 2000)
modelo2


# =============================================================================
# Post Hoc Robusto
# =============================================================================
WRS2::lincon(Libido ~ Dose, data = ds, method = 'holm')

# =============================================================================
# Médias Marginais e Tamanho do Efeito
# =============================================================================
marginal_means <- emmeans(modelo, specs = 'Dose')
post_test <- pairs(marginal_means, adjust = 'tukey') # é necessário criar o post_test com o pairs, baseado no marginal_means para calcular o tamanho do efeito.
post_test

# =============================================================================
# Cálculo do Tamanho do Efeito
# =============================================================================
df.residual(modelo)
sigma(modelo) # Sigma é o desvio padrão dos residuos
post_test_effect_size <- eff_size(post_test, edf = 26, sigma = 1.74, method = 'identity')
post_test_effect_size


