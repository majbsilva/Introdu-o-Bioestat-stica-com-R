# ======================================================
# Módulo 7 – Dados categóricos
# ======================================================

# 📦 Carregar pacotes necessários
library(tidyverse)
library(reshape2)
library(vcd)
library(vcdExtra)

# 📚 Neste módulo, vamos trabalhar com variáveis categóricas e aplicar testes estatísticos apropriados para frequências observadas.
# Os principais testes são o teste do Qui-quadrado (χ²) e o teste exato de Fisher.

## 📌 Seção 7.1 – Teste do Qui-quadrado (χ²)

# 🎯 Objetivo: verificar se há associação entre duas variáveis categóricas
# H0: As variáveis são independentes
# H1: Existe associação entre as variáveis

# ✅ Pressupostos do teste χ²:
# 1. As observações devem ser independentes
# 2. Espera-se que **pelo menos 80% das células** tenham frequência esperada ≥ 5
# 3. **Nenhuma** célula deve ter frequência esperada menor que 1

# 🧮 Tabela de contingência
# Carregando os dados
data("Arthritis")

# Exemplo: associação entre tratamento e melhora no dataset Arthritis
tab <- xtabs(~Treatment + Improved, data = Arthritis)

# 🧠 Frequência esperada
chisq_result <- chisq.test(tab)
chisq_result$expected

# 🔍 Aplicando o teste χ²
assocstats(tab)
chisq.test(tab)

# 📉 Visualização da tabela
tab_melt <- melt(tab)
colnames(tab_melt) <- c("Tratamento", "Melhora", "Frequencia")

ggplot(tab_melt, aes(x = Tratamento, y = Frequencia, fill = Melhora)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "📊 Frequência Tratamento e melhora") +
  theme_minimal()


## 📌 Seção 7.2 – Teste exato de Fisher

# 🎯 Usado quando os pressupostos do teste χ² são violados, especialmente em tabelas 2x2 com valores baixos
# H0: As variáveis são independentes
# H1: Existe associação entre as variáveis

# ⚠️ Quando usar:
# ➤ Se qualquer frequência esperada < 5 (em especial em tabelas pequenas 2x2)
# ➤ Mais apropriado para amostras pequenas

# 🔍 Teste de Fisher
fisher.test(tab)


## 📝 Exercício Final – Módulo 7
# Para esse exercício, utilize o dataset Titanic.

# 1. Crie uma tabela de contingência com duas variáveis categóricas do seu dataset
# 2. Verifique as frequências esperadas com chisq.test(..., simulate.p.value = FALSE)$expected
# 3. Aplique o teste do Qui-quadrado ou, se necessário, o teste de Fisher
# 4. Construa um gráfico de barras agrupado com ggplot2
# 5. Interprete os resultados e conclua sobre a independência entre as variáveis

# ✅ Fim do Módulo 7 – Dados categóricos
