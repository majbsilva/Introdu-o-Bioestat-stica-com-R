
# =============================================================================
# Aula: Análise Estatística com Teste para MAIS de dois grupos
# Curso: Estatística Aplicada
# Assunto: ANOVA e Visualização de Dados - Estudo de Caso Combinação de drogas.
# Comparação do fechamento da ferida do ensaio de invasão frente diferente combinações de drogas.
# =============================================================================

# =============================================================================
# Carregando Bibliotecas
# =============================================================================

library(tidyverse) # ggplot, dplyr
library(rstatix) # get_summary_stats, 
library(gt)
library(ggstatsplot) # gráfico

# =============================================================================
# Obtendo e organizando os dados
# =============================================================================

emilia <- readxl::read_xlsx('dados/dados_fechamento_emilia.xlsx')

## Transformando em uma tabela longa
ds <- pivot_longer(
  emilia,
  col = c(2:7),
  names_to = 'tratamento',
  values_to = 'valor'
)

## Filtrando para um tipo de linhagem celular
ds_c3a <- ds %>% 
  filter(linhagem == 'UM-HMC3A') %>% 
  select(!linhagem)

# =============================================================================
# Descrição por grupo
# =============================================================================

ds_c3a %>% group_by(tratamento) %>% get_summary_stats()

# =============================================================================
# Gráfico Histograma
# =============================================================================


ds_c3a %>% 
  ggplot(aes(x = valor)) +
  geom_histogram() +
  facet_grid(~tratamento)

# =============================================================================
# Gráfico Q-Q plot
# =============================================================================

ds_c3a %>% 
  ggplot(aes(sample = valor)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(~tratamento)

# =============================================================================
# Gráfico box plot
# =============================================================================

ds_c3a %>% 
  ggplot(aes(x = tratamento, y = valor)) +
  geom_boxplot()

ds_c3a$tratamento <- factor(ds_c3a$tratamento, levels =  c("ctrl",    "cis",     "pan",     "pan+cis", "rom",     "rom+cis"))

ds_c3a %>% 
  ggplot(aes(x = tratamento, y = valor)) +
  geom_boxplot()

# =============================================================================
# Analisando os outliers
# =============================================================================

## Identificando pelo intervalo interquantil
ds_c3a %>% group_by(tratamento) %>%  identify_outliers(valor)

## Removendo o outlier

clean_ds_c3a <- ds_c3a[ -32 , ] # clean_ds_c3a <- ds_c3a[ linhas , colunas ] 

clean_ds_c3a %>% group_by(tratamento) %>% identify_outliers(valor)

# =============================================================================
# Teste de Normalidade de Shapiro-Wilk
# =============================================================================  

clean_ds_c3a %>% group_by(tratamento) %>% shapiro_test(valor)

# =============================================================================
# Teste de Levene para homogeneidade
# =============================================================================  

## Os pressupostos do ANOVA são normalidade (verificado pelo theste de Shapiro-Wilk) e homogeneidade

clean_ds_c3a %>% levene_test(valor ~ tratamento)

clean_ds_c3a %>% 
  ggplot(aes(x = tratamento, y = valor)) +
  geom_boxplot()

## Baseado no boxplot, eu não vou considerar o valor do teste de levene, uma vez que sei da possibilidade de erro do tipo II desse teste no caso de n pequeno.

# =============================================================================
# ANOVA
# ============================================================================= 

clean_ds_c3a %>% anova_test(valor ~ tratamento, white.adjust = T) # White.adjust = T, eu falo para a fórmula que a variancia é diferente

# Calculando o tamanho do efeito
## O tamanho do efeito será o partial eta squared porque ele leva em consideração o número de co-fatores. 

clean_ds_c3a %>% anova_test(valor ~ tratamento, effect.size = 'pes')

# =============================================================================
# GRÁFICO
# ============================================================================= 

plot <- ggbetweenstats(
  data = clean_ds_c3a,
  x = tratamento,
  y = valor,
  type = "parametric",
  pairwise.display = "statistic",
  p.adjust.method = "holm",
  effsize.type = "unbiased",
  bf.prior = 0.707,
  bf.message = F,
  results.subtitle = T,
  xlab = '',
  ylab = 'Wound healing',
  caption = NULL,
  title = NULL,
  subtitle = NULL,
  digits = 2L,
  var.equal = FALSE,
  conf.level = 0.95,
  nboot = 100L,
  tr = 0.2,
  centrality.plotting = T,
  centrality.type = 'type',
  centrality.point.args = list(size = 5, color = "darkred"),
  centrality.label.args = list(size = 4, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), alpha =
                      0.9, size = 4, stroke = 0, na.rm = TRUE),
  boxplot.args = list(width = 0.5, alpha = 0.2, na.rm = TRUE),
  violin.args = list(width = 0.0, alpha = 0.2, na.rm = TRUE),
  ggsignif.args = list(textsize = 4, tip_length = 0.02, na.rm = TRUE),
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Dark2",
  ggplot.component = NULL
)
plot

extract_stats(plot)

