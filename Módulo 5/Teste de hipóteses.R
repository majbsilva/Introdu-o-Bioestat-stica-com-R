# ======================================================
# Módulo 5 – Testes de Hipótese
# ======================================================

# 📚 Neste módulo, vamos aprender a formular hipóteses e aplicar testes estatísticos inferenciais como o teste t e ANOVA.

## 📌 Seção 5.1 – Teste t (para dois grupos)

# 🎯 Objetivo: comparar a média de duas populações (ex: grupo_idade)
# H0: As médias dos dois grupos são iguais
# H1: As médias dos dois grupos são diferentes

# ✅ Pré-requisitos do teste t:
# 1. Normalidade das distribuições → Shapiro-Wilk
# 2. Homogeneidade de variâncias → Teste de Levene ou F-test

# 📦 Carregar pacotes necessários
library(tidyverse) # ggplot
library(car) # LeveneTest
library(MASS) # dataset anorexia

# Carregando o dataset anorexia
data("anorexia")

# 1️⃣ Teste de normalidade (Shapiro-Wilk)
anorexia %>%
  filter(Treat %in% c('Cont', 'CBT')) %>% 
  group_by(Treat) %>% 
  summarise(p_shapiro = shapiro.test(Prewt)$p.value)

# 2️⃣ Teste de homogeneidade de variâncias (Levene)
anorexia %>% 
  filter(Treat %in% c('Cont', 'CBT')) %>% 
  with(leveneTest(Prewt ~ Treat, center = 'mean'))


# 3️⃣ Teste t
teste_t <- anorexia %>% 
  filter(Treat %in% c('Cont', 'CBT')) %>%
  with(t.test(Prewt ~ Treat, var.equal = T)) # Use var.equal=FALSE se variâncias forem diferentes

## 📌 Seção 5.2 – ANOVA (para 3 ou mais grupos)

# 🎯 Objetivo: comparar médias entre três ou mais grupos (ex: grupo)
# H0: Todas as médias são iguais
# H1: Pelo menos uma média é diferente

# ✅ Pré-requisitos da ANOVA:
# 1. Normalidade dos resíduos → Shapiro-Wilk nos resíduos
# 2. Homogeneidade de variâncias → Teste de Levene
# 3. Independência das observações (garantido pelo desenho experimental)

# 🔍 Visualização prévia dos grupos

anorexia %>% 
  with(boxplot(Prewt ~ Treat, main = 'Boxplot por grupo'))

# 1️⃣ Ajustar modelo de ANOVA

aov_res <- anorexia %>% 
  with(aov(Prewt ~ Treat))
summary(aov_res)

# 2️⃣ Teste de normalidade dos resíduos
shapiro.test(residuals(aov_res))

# 3️⃣ Teste de homogeneidade de variâncias
anorexia %>% 
  with(leveneTest(Prewt ~ Treat, center = 'mean'))

# 4️⃣ Teste post-hoc (Tukey)
TukeyHSD(aov_res)


## 📌 Seção 5.3 – Visualização e interpretação

# 📈 Gráfico com média e erro padrão
anorexia %>% 
  group_by(Treat) %>% 
  summarise(
    media = mean(Prewt, na.rm = TRUE),
    erro = sd(Prewt, na.rm = TRUE) / sqrt(n())
  ) %>% 
  ggplot(aes(x = Treat, y = media)) +
  geom_col(fill = "skyblue") +
  geom_errorbar(aes(ymin = media - erro, ymax = media + erro), width = 0.2) +
  labs(title = "Média de Idade por Grupo", y = "Idade Média") +
  theme_classic()


## 📝 Exercício Final – Módulo 5

# No dataset anorexia, comparar se as médias de Cont e FT são estatisticamente diferentes.
# No dataset anorexia, comparar se as médias de entre os grupos de tratamento são estatisticamente diferentes para Postwt.

# Considere a variável "idade" em diferentes grupos (2 ou mais):
# 1. Verifique a normalidade por grupo com shapiro.test()
# 2. Teste a homogeneidade de variâncias com leveneTest()
# 3. Realize o teste t ou ANOVA conforme o número de grupos
# 4. Interprete os p-valores e conclua sobre a hipótese nula
# 5. Se ANOVA for significativa, aplique o teste de Tukey
# 6. Construa um gráfico com média e erro padrão

# ✅ Fim do Módulo 5 – Testes de Hipótese
