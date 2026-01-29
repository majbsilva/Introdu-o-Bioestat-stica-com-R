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
ds %>% filter(expressao_gene != 500) %>% 
  group_by(tratamento) %>%
  shapiro_test(expressao_gene)

# 2. Gráfico Q-Q plot para verificar normalidade visualmente

ds %>% 
  ggplot(aes(sample = expressao_gene, color = tratamento)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~tratamento) +
  labs(title = "QQ-Plot: Expressão Gênica por Tratamento") +
  theme_minimal()

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

# ------------------------------------------------------------------------------
# Seção 6.4: Calculando tamanho do efeito
# ------------------------------------------------------------------------------

# Tamanho de efeito para o Wilcox test (Mann Whitney)

ds_filtered <- ds %>% filter(tratamento %in% c('Controle', "DrogB"))
  
ds_filtered %>% wilcox_effsize(expressao_gene ~ tratamento, ci = TRUE)

# Tamanho de efeito para o kruskall Wallis

ds %>% kruskal_effsize(expressao_gene ~ tratamento, ci = TRUE)


# 3. Visualização com estatística integrada
ggbetweenstats(
  data = ds,
  x = tratamento,
  y = expressao_gene,
  type = "nonparametric", # Ativa o teste de Mann-Whitney automaticamente
  effsize.type = "unbiased",
  plot.type = "box",
  title = "Teste de Mann-Whitney (n=10/grupo)",
  messages = FALSE
)


# ------------------------------------------------------------------------------
# Exercício Final – Módulo 6
# ------------------------------------------------------------------------------

# 1. Carregue o dataset 'chickwts'.
# 2. Selecione apenas 3 tipos de dietas (ex: 'soybean', 'sunflower', 'linseed').
# 3. Aplique o teste de Kruskal-Wallis para ver se o peso médio difere.
# 4. Caso p < 0.05, identifique quais pares de dietas são diferentes entre si.