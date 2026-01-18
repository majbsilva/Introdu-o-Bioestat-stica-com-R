# =============================================================================
# Aula: Análise Estatística com Teste para dois grupos
# Curso: Estatística Aplicada
# Assunto: Teste T e Visualização de Dados - Estudo de Caso Metabric
# =============================================================================


# =============================================================================
# Carregando Bibliotecas
# =============================================================================

library(tidyverse) # BIBLIOTECAS 
library(rstatix) # get_summary_stats, 
library(flextable)
library(DescTools)
library(ggstatsplot) #GRÁFICO

# =============================================================================
# Resumo estatístico
# =============================================================================

## Fazer uma comparação entre grupos HER2 status da variável Idade.no diagnóstico. Teste de comparação: normalidade e homogeneidade.

metabric <- read.csv('dados/metabric.csv')

glimpse(metabric)

ds <- metabric %>% select(HER2.Status,Idade.no.Diagnóstico)

# =============================================================================
# Descrição por Status
# ====================================================================

ds %>% get_summary_stats()

ds %>% 
  group_by(HER2.Status) %>% 
  get_summary_stats()

# =============================================================================
# Gráfico Histograma
# =============================================================================

ds %>% 
  ggplot(aes(x = Idade.no.Diagnóstico)) +
  geom_histogram() +
  facet_grid(~HER2.Status)

# =============================================================================
# Gráfico Q-Q Plot
# =============================================================================

ds %>% 
  ggplot(aes(sample = Idade.no.Diagnóstico)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(~HER2.Status)


# =============================================================================
# Teste de Normalidade de Shapiro-Wilk
# =============================================================================

ds %>% 
  group_by(HER2.Status) %>% 
  shapiro_test(Idade.no.Diagnóstico)

# =============================================================================
# Boxplot da Idade no Diagnóstico
# =============================================================================

ds %>% 
  ggplot(aes(x = HER2.Status, y = Idade.no.Diagnóstico)) +
  geom_boxplot()

# =============================================================================
# Teste de Levene para Homogeneidade de Variâncias
# =============================================================================

ds %>% 
  levene_test(Idade.no.Diagnóstico~HER2.Status) 

# =============================================================================
# Teste T 
# =============================================================================

ds %>% 
  t_test(Idade.no.Diagnóstico~HER2.Status, detailed = T, var.equal = T)

## se as variâncias fossem diferentes, ou seja, os grupos não fossem homogêneos

ds %>% 
  t_test(Idade.no.Diagnóstico~HER2.Status, detailed = T, var.equal = F)

## Tamanho de efeito=============
library(effectsize)
cohens_d(ds$Idade.no.Diagnóstico~ds$HER2.Status)
hedges_g(ds$Idade.no.Diagnóstico~ds$HER2.Status)


# =============================================================================
# Gráfico de Comparação entre Grupos
# =============================================================================
ggbetweenstats(
  ds,
  HER2.Status,
  Idade.no.Diagnóstico,
  type = "parametric",
  pairwise.display = "significant",
  p.adjust.method = "holm",
  effsize.type = "unbiased",
  bf.prior = 0.707,
  bf.message = F,
  results.subtitle = TRUE,
  xlab = 'HER2 status',
  ylab = 'Idade no diagnóstico',
  caption = NULL,
  title = NULL,
  subtitle = NULL,
  digits = 2L,
  var.equal = FALSE,
  conf.level = 0.95,
  nboot = 100L,
  tr = 0.2,
  centrality.plotting = TRUE,
  centrality.type = 'type',
  centrality.point.args = list(size = 5, color = "darkred"),
  centrality.label.args = list(size = 3, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), alpha =
                      0.4, size = 3, stroke = 0, na.rm = TRUE),
  boxplot.args = list(width = 0.3, alpha = 0.2, na.rm = TRUE),
  violin.args = list(width = 0.5, alpha = 0.2, na.rm = TRUE),
  ggsignif.args = list(textsize = 3, tip_length = 0.01, na.rm = TRUE),
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Dark2",
  ggplot.component = NULL
)


