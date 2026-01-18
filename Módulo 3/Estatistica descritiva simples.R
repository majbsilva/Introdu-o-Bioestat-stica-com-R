# ==============================================================================
# Módulo 3: Estatística Descritiva e Visualização de Dados
# ==============================================================================

# Carregando pacotes essenciais
library(tidyverse)    # Manipulação (dplyr) e gráficos (ggplot2)
library(modelsummary) # Função datasummary_skim e balance
library(gtsummary)    # Função tbl_summary para tabelas biomédicas
library(ggpubr)       # Função stat_compare_means
library(DescTools)    # Função Desc
library(rstatix)      # Testes de Shapiro, Levene, ANOVA e Tukey
library(ggstatsplot)  # Gráficos estatísticos complexos (p e p2)
library(patchwork)    # Combinação de gráficos (p + p2)

# ------------------------------------------------------------------------------
# Exploração Inicial (Dataset mtcars)
# ------------------------------------------------------------------------------

# Resumo estatístico completo do dataset mtcars
mtcars %>% Desc()

# Resumos específicos com modelsummary
mtcars %>% datasummary_skim()
datasummary_balance(mpg ~ vs, mtcars)

# Resumos específicos e estatística com o gtsummary
mtcars %>% 
  select(mpg,cyl,vs, am, hp, wt) %>% 
  tbl_summary(by = vs) %>% 
  add_p()

# ------------------------------------------------------------------------------
# Seção 3.1: Estatística descritiva (Dataset PlantGrowth)
# ------------------------------------------------------------------------------

# Carregando dataset nativo
plant <- PlantGrowth
plant %>% glimpse()


# Resumos de balanço entre grupos
datasummary_balance(weight~group,plant)
plant %>% tbl_summary(by = group) %>% 
  add_p()


# ------------------------------------------------------------------------------
# Avaliação de distribuição (Normalidade)
# ------------------------------------------------------------------------------

# Teste de Shapiro-Wilk por grupo
plant %>% 
  group_by(group) %>% 
  shapiro_test(weight)

# Gráfico QQ-Plot para verificar normalidade visualmente
plant %>% 
  ggplot(aes(sample = weight)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(~group) +
  theme_bw()

# Histograma de distribuição
plant %>% 
  ggplot(aes(x = weight)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white") +
  facet_grid(~group) +
  theme_light()

# ------------------------------------------------------------------------------
# Verificação e remoção de outliers
# ------------------------------------------------------------------------------

# Identificação via Boxplot
boxplot_info <- boxplot(weight ~ group, data = plant)
boxplot_info$out

# Remoção manual conforme definido no protocolo original
plant <- plant[plant$weight != 6.03, ]


# ------------------------------------------------------------------------------
# Avaliação da Homogeneidade e ANOVA
# ------------------------------------------------------------------------------

# Teste de Levene para homogeneidade de variância
plant %>% levene_test(weight ~ group, center = mean)

# ANOVA com cálculo de Tamanho do Efeito (Partial Eta Squared - PES)
# white.adjust = F assume variâncias homogêneas
anova_res <- plant %>% anova_test(weight ~ group, effect.size = 'pes', white.adjust = FALSE)
print(anova_res)

# Realizando o teste de post-hoc (Tukey)
plant %>% tukey_hsd(weight ~ group) %>% view()

# Visualizando o gráfico com o resultado da estatistica

group_compar <- list(
  c("ctrl", "trt1"),
  c("ctrl", "trt2"),
  c("trt1", "trt2")
)

plant %>% 
  ggplot(aes(x = group, y = weight)) +
  geom_boxplot(outliers = F) +
  stat_compare_means(comparisons = group_compar) +
  theme_minimal()

# ------------------------------------------------------------------------------
# Seção 3.2: Visualização avançada com ggstatsplot
# ------------------------------------------------------------------------------

# Criando o primeiro gráfico (PlantGrowth)
p <- ggbetweenstats(
  data = plant,
  x = group,
  y = weight,
  type = "parametric",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "eta",
  bf.message = FALSE,
  results.subtitle = TRUE,
  xlab = "Experimental Group",
  ylab = "Weight (lb)",
  centrality.point.args = list(size = 5, color = "darkred"),
  point.args = list(position = position_jitterdodge(dodge.width = 0.6), alpha = 0.8, size = 3),
  boxplot.args = list(width = 0.2, alpha = 0.2),
  violin.args = list(width = 0, alpha = 0.2),
  ggtheme = theme_ggstatsplot(),
  palette = "Dark2"
)

# Ajustes estéticos finais no Plot 1
p <- p + scale_x_discrete(labels = c('Control', 'Treatment 1', 'Treatment 2')) +
  theme(
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12, color = 'black', face = 'bold')
  )

# ------------------------------------------------------------------------------
# Seção 3.3: Análise de Dados Simulados (Plant2)
# ------------------------------------------------------------------------------

# Gerando dados fictícios
set.seed(42)
weight_sim <- rnorm(30, 6, 1)
group_sim <- rep(c('ctrl', 'trt1', 'trt2'), each = 10)

plant2 <- data.frame(group = factor(group_sim), weight = as.numeric(weight_sim))

# Criando o segundo gráfico (Plant2)
p2 <- ggbetweenstats(
  data = plant2,
  x = group,
  y = weight,
  type = "parametric",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "eta",
  bf.message = FALSE,
  results.subtitle = TRUE,
  xlab = "Experimental Group",
  ylab = "Weight (lb)",
  centrality.point.args = list(size = 5, color = "darkred"),
  point.args = list(position = position_jitterdodge(dodge.width = 0.6), alpha = 0.8, size = 3),
  boxplot.args = list(width = 0.2, alpha = 0.2),
  violin.args = list(width = 0, alpha = 0.2),
  ggtheme = theme_ggstatsplot(),
  palette = "Set1"
)

# Ajustes estéticos finais no Plot 2
p2 <- p2 + scale_x_discrete(labels = c('Control', 'Treatment 1', 'Treatment 2')) +
  theme(
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12, color = 'black', face = 'bold')
  )

# ------------------------------------------------------------------------------
# Seção 3.4: Composição final e exportação
# ------------------------------------------------------------------------------

# Adicionando tags de identificação para painéis de figura
p <- p + labs(tag = '(A)') + theme(plot.tag = element_text(size = 18, face = "bold"))
p2 <- p2 + labs(tag = '(B)') + theme(plot.tag = element_text(size = 18, face = "bold"))

# Combinando os gráficos com patchwork
layout_final <- p + p2

# Visualizar
print(layout_final)

# Exportação para diretório (Certifique-se que a pasta 'Modulo 3' existe)
# ggsave(
#   filename = 'plot_composto.png',
#   plot = layout_final,
#   height = 9,
#   width = 18,
#   dpi = 300
# )