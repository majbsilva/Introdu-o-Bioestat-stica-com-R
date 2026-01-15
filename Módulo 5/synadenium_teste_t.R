# =============================================================================
# Aula: Análise Estatística com Teste para dois grupos
# Curso: Estatística Aplicada
# Assunto: Teste T e Visualização de Dados - Estudo de Caso Synadenium.
# Comparação do número de colônias de células frente tratamento com extrato dessa planta.
# =============================================================================

# =============================================================================
# Carregando Bibliotecas
# =============================================================================

library(tidyverse) 
library(rstatix) # get_summary_stats, 
library(flextable)
library(DescTools)
library(ggstatsplot) # gráfico

# =============================================================================
# Carregando os dados
# =============================================================================

syna <- readxl::read_xlsx('dados/synadenium.xlsx')
syna <- syna %>% 
  select(Control,Treated)

syna <- stack(syna)

# =============================================================================
# Descrição por grupo
# =============================================================================

get_summary_stats(syna)

syna %>% group_by(ind) %>% get_summary_stats(values)

# =============================================================================
# Gráfico Histograma
# =============================================================================

syna %>% 
  ggplot(aes(x = values)) +
  geom_histogram() +
  facet_grid(~ind) # essa função faceia (separa) a geom solicitada na variável solicitada.

# =============================================================================
# Gráfico Q-Q Plot
# =============================================================================

syna %>% 
  ggplot(aes(sample = values)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(~ind)

# =============================================================================
# Gráfico Box Plot
# =============================================================================

syna %>% 
  ggplot(aes(ind,values)) +
  geom_boxplot() +
  facet_grid(~ind)

# =============================================================================
# Identificando o outlier
# =============================================================================
  
syna %>% group_by(ind) %>% identify_outliers(values)

# =============================================================================
# Removendo o outlier
# =============================================================================

clean_syna <- syna[syna$values != 144,]

clean_syna %>% 
  ggplot(aes(ind, values)) +
  geom_boxplot()

# =============================================================================
# Teste de Normalidade de Shapiro-Wilk
# =============================================================================

clean_syna %>% group_by(ind) %>% shapiro_test(values) # os dados seguem uma distribuição normal. Posso utilizar um teste paramétrico

# Os testes paramétricos assumem que a distribuição é normal e que os grupos possuem variâncias iguas (homogeneidade)

# =============================================================================
# Teste de Levene para Homogeneidade de Variâncias
# =============================================================================

clean_syna %>% levene_test(values ~ ind, center = 'median')
# o teste de levene mostra que a variância entre os grupos são iguais.

# =============================================================================
# Teste T 
# =============================================================================

clean_syna %>% t_test(values ~ ind, var.equal = TRUE )
# O valor de p do test foi iugal a 0.0005, menor do que 0.05 indicando que a diferença entre controle e tratados é estatística.

clean_syna %>% t_test(values ~ ind, var.equal = FALSE) # p = 0.0004
# aqui, estou aplicando a correção de Welch. De acordo com Zimmerman (2004), ele preconiza que para grupos pequenos (menor do que 30), sempre aplique o teste de Welch. Isso garante que você não esta errando no teste de homogeneidade. Portanto, o custo de usar a correção de Welch é pequena perto do custo de você não utilizá-la. ( https://doi.org/10.1348/000711004849222)

# =============================================================================
# Tamanho do efeito
# =============================================================================

effectsize::cohens_d(clean_syna$values ~ clean_syna$ind, pooled_sd = FALSE)
effectsize::hedges_g(clean_syna$values ~ clean_syna$ind, pooled_sd = F) # Ideal para amostras com n menor do que 20, onde o SD pooled é corrigido para o número de amostras. 

# =============================================================================
# Gráfico de Comparação entre Grupos
# =============================================================================

plot <- ggbetweenstats(
  data = clean_syna,
  x = ind,
  y = values,
  type = "parametric",
  pairwise.display = "significant",
  p.adjust.method = "holm",
  effsize.type = "unbiased", # Unbiased traz o g de hedge (melhor para amostras pequenas) O biased traz o d_cohen para amostras maiores.
  bf.prior = 0.707,
  bf.message = F,
  results.subtitle = T,
  xlab = '',
  ylab = 'Number of colonies',
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
  centrality.label.args = list(size = 7, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), alpha =
                      0.8, size = 4, stroke = 0, na.rm = TRUE),
  boxplot.args = list(width = 0.3, alpha = 0.2, na.rm = TRUE),
  violin.args = list(width = 0.0, alpha = 0.2, na.rm = TRUE),
  ggsignif.args = list(textsize = 3, tip_length = 0.01, na.rm = TRUE),
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Dark2",
  ggplot.component = NULL
)

plot <- plot + theme(
  plot.subtitle = element_text(size = 18),
  axis.title = element_text(size = 24),
  axis.text = element_text(size = 24, color = 'black')
)

ggsave(
  filename = 'Módulo 5/graph_number_colonies.png',
  plot = plot,
  dpi = 300,
  width = 5.2, 
  height = 4.75,
  scale = 1.76
  )







