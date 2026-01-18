# ==============================================================================
# Módulo 4: Correlação e Regressão Linear Simples
# ==============================================================================

# Carregar pacotes necessários
library(tidyverse)   # Manipulação e gráficos
library(readxl)      # Importação de Excel
library(ggpubr)      # Adicionar equações e p-valores nos gráficos
library(rstatix)     # Testes estatísticos compatíveis com tidyverse
library(performance) # Diagnóstico de modelos (função check_model)

# ------------------------------------------------------------------------------
# Seção 4.1: Importação de dados
# ------------------------------------------------------------------------------

# Cenário: Curva Padrão de Proteína (Ensaio de Bradford)
# Variável X: Concentração de BSA (mg/mL)
# Variável Y: Absorbância a 595nm

# Importando a planilha gerada pelo leitor de placas
dados_correlacao <- read_excel("Módulo 4/dados_correlacao.xlsx")

# Visualizar estrutura
glimpse(dados_correlacao)

# ------------------------------------------------------------------------------
# Seção 4.2: Visualização exploratória (Scatter Plot)
# ------------------------------------------------------------------------------

dados_correlacao %>%
  ggplot(aes(x = concentracao_bsa, y = absorbancia_595)) +
  geom_point(color = "blue", size = 3, alpha = 0.6) +
  labs(
    title = "Dispersão: Concentração vs. Absorbância",
    x = "Concentração BSA (mg/mL)",
    y = "Absorbância (595 nm)"
  ) +
  theme_bw()

# ------------------------------------------------------------------------------
# Seção 4.3: Verificação de pressupostos (Normalidade das Variáveis)
# ------------------------------------------------------------------------------

# Teste de Shapiro-Wilk para ambas as variáveis (Decisão Pearson vs Spearman)
dados_correlacao %>% shapiro_test(concentracao_bsa)
dados_correlacao %>% shapiro_test(absorbancia_595)

# Visualização QQ plot simples
p1 <- dados_correlacao %>%
  ggplot(aes(sample = absorbancia_595)) +
  stat_qq() + stat_qq_line() +
  labs(title = "QQ Plot - Absorbância") + theme_minimal()

p2 <- dados_correlacao %>%
  ggplot(aes(sample = concentracao_bsa)) +
  stat_qq() + stat_qq_line() +
  labs(title = "QQ Plot - Concentração") + theme_minimal()

ggarrange(p1, p2, ncol = 2)

# ------------------------------------------------------------------------------
# Seção 4.4: Testes de correlação
# ------------------------------------------------------------------------------

# Opção A: Correlação de Pearson (Paramétrica)
res_pearson <- dados_correlacao %>% 
  cor_test(concentracao_bsa, absorbancia_595, method = "pearson")
print(res_pearson)

# Opção B: Correlação de Spearman (Não-Paramétrica)
res_spearman <- dados_correlacao %>% 
  cor_test(concentracao_bsa, absorbancia_595, method = "spearman")
print(res_spearman)

# ------------------------------------------------------------------------------
# Seção 4.5: Regressão Linear (Modelagem)
# ------------------------------------------------------------------------------

# Objetivo: Obter a equação da reta (y = mx + b)
modelo_linear <- lm(absorbancia_595 ~ concentracao_bsa, data = dados_correlacao)

# Resumo estatístico
summary(modelo_linear)

# ------------------------------------------------------------------------------
# Seção 4.5.1: Diagnóstico completo do modelo (Pacote performance)
# ------------------------------------------------------------------------------

# A função check_model verifica visualmente todos os pressupostos:
# 1. Posteriror Predictive Check (O modelo reproduz bem os dados?)
# 2. Linearidade (Resíduos vs Fitted)
# 3. Homogeneidade da Variância (Homocedasticidade)
# 4. Influência (Distância de Cook - Outliers)
# 5. Normalidade dos Resíduos (Pontos devem seguir a linha)

# Gera um painel diagnóstico completo
check_model(modelo_linear)

# Dica de interpretação:
# O pacote performance fornece linhas verdes/guias.
# Se os seus dados seguirem as linhas de referência verdes, o modelo é válido.

check_autocorrelation(modelo_linear)  # Verifica autocorrelação dos resíduos
check_heteroscedasticity(modelo_linear)  # Verifica homocedasticidade
check_normality(modelo_linear)  # Verifica normalidade dos resíduos
check_outliers(modelo_linear)  # Identifica possíveis outliers

# ------------------------------------------------------------------------------
# Seção 4.6: Visualização final para publicação
# ------------------------------------------------------------------------------

dados_correlacao %>%
  ggplot(aes(x = concentracao_bsa, y = absorbancia_595)) +
  geom_point(size = 3, color = "black", alpha = 0.8) +
  
  # Regressão Linear com Intervalo de Confiança
  geom_smooth(method = "lm", color = "firebrick", fill = "gray20") +
  
  # Estatísticas (R e p)
  stat_cor(method = "pearson", label.x = 0.1, label.y = 1.4, size = 5) +
  
  # Equação da reta
  stat_regline_equation(label.x = 0.1, label.y = 1.25, size = 5) +
  
  labs(
    title = "",
    x = "Concentração de BSA (mg/mL)",
    y = "Absorbância (595 nm)"
  ) +
  theme_azurelight(base_size = 14)


