# ======================================================
# Módulo 3 – Estatística Descritiva e Visualização
# ======================================================

## Carregando pacotes
library(tidyverse) #ggplot
library(janitor) # Tabelas de frequência
library(modelsummary)
library(pastecs)


Desc(mtcars)


datasummary_skim(mtcars)
datasummary_balance(mpg~vs, mtcars)

## 📌 Seção 3.1 – Estatística descritiva

# Carregando dataset # do pacote datastes, PlantGrowth
plant <- PlantGrowth
glimpse(plant)

# Obtendo dados descritivos da tabela

datasummary_skim(plant)
datasummary_balance(weight~group,plant) 

# Obtendo e visualizando distribuição

by(plant$weight, plant$group,shapiro.test)

plant %>% 
  ggplot(aes(sample = weight)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(~group)

plant %>% 
  ggplot(aes(x = weight)) +
  geom_histogram() +
  facet_grid(~group)


# Verificando outlier

boxplot <- boxplot(weight~group,data = plant)
boxplot$out

plant <- plant[plant$weight != 6.03,]
plant <- plant[plant$weight != 5.87,]

# AVALIANDO A DIFERENÇA ENTRE OS GRUPOS

library(rstatix) # Levene test

## Avaliando a homogeneidade da variância entre grupos

plant %>% levene_test(weight~group, center = mean)

anova <- plant %>% anova_test(weight~group, white.adjust = F) # white.adjust = F fala para a função que os grupos são homogêneos.
anova

### Sobre o cálculo do tamanho do efeito. 
# Podemos avaliar o tamanho do efeito da variação do tratamento no peso. Utilizar o pes (partial eta squared) porque ele leva em consideraçao o número de fatores partial eta = SS entre grupos ? (SS entre grupos + SS dentro dos grupos)

anova <- plant %>% anova_test(weight~group, effect.size = 'pes')
anova

## Realizando o post-test

plant %>% tukey_hsd(weight~group)

## 📌 Seção 3.2 – Visualização com ggstatsplot

library(ggstatsplot)

p <- ggbetweenstats(
  plant,
  group,
  weight,
  type = "parametric",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "eta",
  bf.prior = 0.707,
  bf.message = F,
  results.subtitle = TRUE,
  xlab = '',
  ylab = 'Weight (lb)',
  caption = NULL,
  title = NULL,
  subtitle = NULL,
  digits = 2L,
  var.equal = T,
  conf.level = 0.95,
  nboot = 100L,
  tr = 0.2,
  centrality.plotting = TRUE,
  centrality.type = 'type',
  centrality.point.args = list(size = 5, color = "darkred"),
  centrality.label.args = list(size = 5, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), alpha =
                      0.8, size = 3, stroke = 0, na.rm = TRUE),
  boxplot.args = list(width = 0.2, alpha = 0.2, na.rm = TRUE),
  violin.args = list(width = 0, alpha = 0.2, na.rm = TRUE),
  ggsignif.args = list(textsize = 7, tip_length = 0.03, na.rm = TRUE),
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Dark2",
  ggplot.component = NULL
)

p <- p + scale_x_discrete(labels = c('Control', 'Treatment 1', 'Treatment 2')) +
  theme(
  plot.subtitle = element_text(size = 20),
  axis.title = element_text(size = 26),
  axis.text = element_text(size = 24, color = 'black', face = 'bold')
)
p

ggsave(
  filename = 'Módulo 3/plot.png',
  plot = p,
  height = 9,
  width = 10,
  dpi = 300
)


## PLOT 2 - PLANTA 2

weight <- rnorm(30, 6, 1)
group <- rep(c('ctrl', 'trt1', 'trt2'), each = 10)

plant2 <- data.frame(cbind(group,weight))

plant2$group = factor(plant2$group)
plant2$weight = as.numeric(plant2$weight)

## CRIANDO O GRÁFICO

p2 <- ggbetweenstats(
  plant2,
  group,
  weight,
  type = "parametric",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "eta",
  bf.prior = 0.707,
  bf.message = F,
  results.subtitle = TRUE,
  xlab = '',
  ylab = 'Weight (lb)',
  caption = NULL,
  title = NULL,
  subtitle = NULL,
  digits = 2L,
  var.equal = T,
  conf.level = 0.95,
  nboot = 100L,
  tr = 0.2,
  centrality.plotting = TRUE,
  centrality.type = 'type',
  centrality.point.args = list(size = 5, color = "darkred"),
  centrality.label.args = list(size = 5, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), alpha =
                      0.8, size = 3, stroke = 0, na.rm = TRUE),
  boxplot.args = list(width = 0.2, alpha = 0.2, na.rm = TRUE),
  violin.args = list(width = 0, alpha = 0.2, na.rm = TRUE),
  ggsignif.args = list(textsize = 7, tip_length = 0.03, na.rm = TRUE),
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  package = "RColorBrewer",
  palette = "Dark2",
  ggplot.component = NULL
)

p2 <- p2 + scale_x_discrete(labels = c('Control', 'Treatment 1', 'Treatment 2')) +
  theme(
    plot.subtitle = element_text(size = 20),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 24, color = 'black', face = 'bold')
  )
p2

ggsave(
  filename = 'Módulo 3/plot2.png',
  plot = p2,
  height = 9,
  width = 10,
  dpi = 300
)

p <- p + labs(tag = '(A)') + theme(plot.tag = element_text(size = 18))

p2 <- p2 + labs(tag = '(B)') + theme(plot.tag = element_text(size = 18))

library(patchwork)

p + p2
