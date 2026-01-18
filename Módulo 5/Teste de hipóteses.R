# ==============================================================================
# Modulo 5: Testes de Hipotese
# ==============================================================================

# Carregar pacotes necessarios
library(tidyverse) # Manipulacao e ggplot
library(car)       # Teste de Levene
library(MASS)      # Dataset anorexia
library(rstatix)   # Testes estatisticos em formato tidy

# Preparacao dos dados
data("anorexia")

# ------------------------------------------------------------------------------
# Secao 5.1: Teste t de Student Independente (Comparacao de 2 grupos)
# ------------------------------------------------------------------------------

# 1. Visualização dos dados

anorexia %>% 
  filter(Treat %in% c('Cont', 'CBT')) %>%
  ggplot(aes(x = Treat, y = Prewt)) +
  geom_boxplot()

anorexia %>% 
  filter(Treat %in% c('Cont', 'CBT')) %>%
  ggplot(aes(sample = Prewt)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(~Treat)

# 1. Teste de normalidade (Shapiro-Wilk) por grupo
## Vamos comparar se a diferença entre os grupos Cont e CBT no peso pré-tratamento (Prewt)
anorexia %>%
  filter(Treat %in% c('Cont', 'CBT')) %>%
  group_by(Treat) %>%
  shapiro_test(Prewt)

# 2. Teste de homogeneidade de variancias (Levene)
anorexia %>%
  filter(Treat %in% c('Cont', 'CBT')) %>%
  levene_test(Prewt ~ Treat, center = mean)

# 3. Teste t Independente
teste_t_res <- anorexia %>%
  filter(Treat %in% c('Cont', 'CBT')) %>% 
  t_test(Prewt ~ Treat, var.equal = TRUE)

print(teste_t_res)

# 4. Teste t Independente removendo outliers
teste_t_res <- anorexia %>%
  filter(Treat %in% c('Cont', 'CBT')) %>% 
  filter(!Prewt %in% c(94.9, 70)) %>% 
  t_test(Prewt ~ Treat, var.equal = TRUE)

print(teste_t_res)

# ------------------------------------------------------------------------------
# Secao 5.2: ANOVA One-Way (Comparacao de 3 ou mais grupos)
# ------------------------------------------------------------------------------

## Objetivo: Comparar o peso pré-tratamento (Prewt) entre os três grupos de tratamento

# 1. Visualizacao exploratoria rapida

anorexia %>% 
  ggplot(aes(x = Treat, y = Prewt)) +
  geom_boxplot()

# 2. Ajuste do modelo de ANOVA
modelo_aov <- aov(Prewt ~ Treat, data = anorexia)
summary(modelo_aov)

# 3. Teste de normalidade dos residuos (Pressuposto da ANOVA)
residuals(modelo_aov) %>% shapiro_test()

qqnorm(residuals(modelo_aov))
qqline(residuals(modelo_aov))
hist(residuals(modelo_aov))

# 4. Teste de homogeneidade de variancias (Levene)
anorexia %>% levene_test(Prewt ~ Treat, center = mean)

# 5. Teste post-hoc (Tukey HSD)
anorexia %>% tukey_hsd(Prewt ~ Treat) %>% view()

# ------------------------------------------------------------------------------
# Secao 5.3: Visualizacao e Estatistica Integrada com ggstatsplot
# ------------------------------------------------------------------------------

# O ggbetweenstats executa a ANOVA (ou Kruskal-Wallis) e o Post-hoc automaticamente.
# Ele exibe a distribuicao (violino), o boxplot e as medias.

ggbetweenstats(
  data = anorexia,
  x = Treat,
  y = Prewt,
  type = "parametric",             # "parametric" para ANOVA / "nonparametric" para Kruskal-Wallis
  pairwise.display = "significant", # Exibe apenas comparacoes com p < 0.05
  p.adjust.method = "bonferroni",   # Metodo de correcao para comparacoes multiplas
  effsize.type = "eta",
  bf.prior = 0.707,
  bf.message = F,
  results.subtitle = F,
  xlab = "Tipo de Tratamento",
  ylab = "Peso (lbs)",
  caption = NULL,
  title = NULL,
  subtitle = NULL,
  digits = 2L,
  var.equal = FALSE,
  conf.level = 0.95,
  nboot = 100L,
  tr = 0.2,
  centrality.plotting = TRUE,
  centrality.type = 'parameteric',
  centrality.point.args = list(size = 5, color = "darkred"),
  centrality.label.args = list(size = 3, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), alpha =
                      0.4, size = 3, stroke = 0, na.rm = TRUE),
  boxplot.args = list(width = 0.3, alpha = 0.2, na.rm = TRUE),
  violin.args = list(width = 0.0, alpha = 0.2, na.rm = TRUE),
  ggsignif.args = list(textsize = 3, tip_length = 0.01, na.rm = TRUE),
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Dark2",
  ggplot.component = NULL
)

# ------------------------------------------------------------------------------
# Secao 5.4: Teste t Pareado (Comparacao Antes vs. Depois)
# ------------------------------------------------------------------------------

# Objetivo: Avaliar se houve mudanca significativa dentro de cada grupo.
# Pressuposto: A diferenca entre os pares deve seguir distribuicao normal.

# 1. Criacao da variavel de diferenca
anorexia_diff <- anorexia %>%
  mutate(diferenca = Postwt - Prewt)

# 2. Verificacao da normalidade das diferencas
# Importante: O teste de normalidade e feito na diferenca, nao nos dados brutos.
anorexia_diff %>%
  group_by(Treat) %>%
  shapiro_test(diferenca)

anorexia_diff %>% 
  ggplot(aes(sample = diferenca)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(~Treat)

# 3. Execucao do Teste t Pareado por grupo
anorexia <- anorexia %>% 
  mutate(id = row_number())

anorexia_long <- anorexia %>% 
  pivot_longer(
    cols = c(Prewt, Postwt),
    names_to = "tempo",
    values_to = "peso"
  )

## Teste pareado para o grupo Cont

res_pareado <- anorexia_long %>%
  filter(Treat == 'Cont') %>% 
  t_test(peso ~ tempo, paired = TRUE)

print(res_pareado)

## Testando o teste não paramétrico para o grupo CBT

res_pareado <- anorexia_long %>%
  filter(Treat == 'CBT') %>% 
  wilcox_test(peso ~ tempo, paired = TRUE)

print(res_pareado)

## Teste pareado para o grupo FT

res_pareado <- anorexia_long %>%
  filter(Treat == 'FT') %>% 
  t_test(peso ~ tempo, paired = TRUE)

print(res_pareado)


# ------------------------------------------------------------------------------
# Secao 5.5: Visualizacao de dados pareados (Slopegraph)
# ------------------------------------------------------------------------------

# Grafico de linhas pareadas para observar comportamento individual
ggplot(anorexia_long, aes(x = tempo, y = peso, group = id)) +
  geom_line(alpha = 0.2, color = "black") + 
  geom_point(alpha = 0.4) +
  facet_wrap(~Treat) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", size = 1.2, color = "blue") +
  stat_summary(aes(group = 1), fun = mean, geom = "point", size = 3, color = "blue") +
  labs(
    title = "",
    subtitle = "Linha azul representa a tendencia da media",
    y = "Peso (lbs)",
    x = "Tempo"
  ) +
  theme_bw()

ggstatsplot::grouped_ggwithinstats(
  data = anorexia_long,
  x = tempo,
  y = peso,
  grouping.var = Treat,
  type = "parametric", # Teste t pareado (dentro de cada grupo)
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "cohen",
  bf.message = FALSE,
  results.subtitle = FALSE,
  xlab = "Tempo",
  ylab = "Peso (lbs)",
  digits = 2L,
  conf.level = 0.95,
  centrality.plotting = F,
  centrality.type = "parametric", # CORRIGIDO: removido o 'e' extra
  centrality.point.args = list(size = 5, color = "darkred"),
  centrality.label.args = list(size = 3, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  # Nota: position_jitterdodge pode afastar os pontos das linhas de conexao
  point.args = list(alpha = 0.4, size = 3, stroke = 0, na.rm = TRUE),
  boxplot.args = list(width = 0.3, alpha = 0.2, na.rm = TRUE),
  violin.args = list(width = 0.0, alpha = 0.2, na.rm = TRUE),
  ggsignif.args = list(textsize = 3, tip_length = 0.01, na.rm = TRUE),
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  palette = "Dark2"
)

# ------------------------------------------------------------------------------
# Exercicios 
# ------------------------------------------------------------------------------

# 1. No grupo 'FT', a diferenca entre Pre e Post segue uma distribuicao normal?
# 2. Houve ganho de peso significativo no grupo 'FT'?
# 3. Compare o Postwt entre os tres grupos usando ANOVA e verifique se as
#    premissas foram atendidas.