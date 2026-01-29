# ==============================================================================
# Módulo 6: Testes Não Paramétricos
# ==============================================================================

# Carregar pacotes necessários
library(tidyverse)   # Manipulação e ggplot
library(rstatix)     # Testes estatísticos tidy
library(ggstatsplot)  # Visualização com estatística integrada
library(ggpubr)      # Complemento para gráficos bioestatísticos

# ------------------------------------------------------------------------------
# Seção 6.1: Introdução aos Ranks (Postos)
# ------------------------------------------------------------------------------

# Os testes não paramétricos não utilizam os valores brutos, mas sim a sua
# ordem (ranks). Isso os torna imunes a outliers extremos.

exemplo_ranks <- c(5, 20, 100, 1000)
rank(exemplo_ranks) 

# ------------------------------------------------------------------------------
# Seção 6.2: Teste de Mann-Whitney (Wilcoxon Rank-Sum)
# ------------------------------------------------------------------------------

# Objetivo: Comparar 2 grupos independentes quando a normalidade falha.
# Exemplo: Dataset 'ds' (Efeito da droga A na expressão de um determinado gene).

ds <- readxl::read_xlsx("Módulo 6/dados_nao_normais.xlsx")

# 1. Verificação de pressupostos (Normalidade)
# Nota: em amostras muito pequenas, o teste de Shapiro-Wilk é rigoroso.
ds %>%
  group_by(tratamento) %>%
  shapiro_test(expressao_gene)

# 2. Execução do Teste de Mann-Whitney
# Comparando Controle e Droga A
# H0: A expressão gênica não é influenciada pela Droga A.

ds %>% filter(tratamento %in% c('Controle', "DrogA")) %>%
  wilcox_test(expressao_gene ~ tratamento, paired = FALSE) %>%
  add_significance()

# Comparando Controle e Droga B

ds %>% filter(tratamento %in% c('Controle', "DrogB")) %>%
  wilcox_test(expressao_gene ~ tratamento, paired = FALSE) %>%
  add_significance()


# ------------------------------------------------------------------------------
# Seção 6.3: Teste de Kruskal-Wallis e Post-hoc de Dunn
# ------------------------------------------------------------------------------

# Comparando todos os grupos
# H0: A expressão gênica não é influenciada pelo uso ou não das drogas.

ds %>% kruskal_test(expressao_gene ~ tratamento)

# Realziando o pós-test
# Teste de Dunn (Dunn's Test): É o padrão ouro e o mais utilizado em publicações científicas. Ele realiza comparações par a par baseadas nos postos (ranks) e já incorpora métodos de correção para múltiplos testes (como Bonferroni ou Holm).

ds %>% dunn_test(expressao_gene ~ tratamento, p.adjust.method = "holm")


# 3. Visualização com estatística integrada
ggbetweenstats(
  data = ds,
  x = tratamento,
  y = expressao_gene,
  type = "nonparametric", # Ativa o teste de Mann-Whitney automaticamente
  plot.type = "box",
  title = "Teste de Mann-Whitney (n=10/grupo)",
  messages = FALSE
)



# Objetivo: Comparar 3 ou mais grupos independentes.
# Exemplo: Dataset 'ToothGrowth' (3 doses de Vitamina C).

data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# 1. Teste de Kruskal-Wallis (Omnibus)
# H0: As medianas de crescimento dentário são idênticas entre as doses.
res_kruskal <- ToothGrowth %>%
  kruskal_test(len ~ dose)

print(res_kruskal)

# 2. Teste Post-hoc de Dunn
# Realizado apenas se o Kruskal-Wallis for significativo (p < 0.05).
res_dunn <- ToothGrowth %>%
  dunn_test(len ~ dose, p.adjust.method = "bonferroni")

print(res_dunn)


# ------------------------------------------------------------------------------
# Exercício Final – Módulo 6
# ------------------------------------------------------------------------------

# 1. Carregue o dataset 'chickwts'.
# 2. Selecione apenas 3 tipos de dietas (ex: 'soybean', 'sunflower', 'linseed').
# 3. Aplique o teste de Kruskal-Wallis para ver se o peso médio difere.
# 4. Caso p < 0.05, identifique quais pares de dietas são diferentes entre si.