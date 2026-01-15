# ======================================================
# Módulo 8 – Correlação e Regressão
# ======================================================

# 📦 Carregar pacotes necessários
library(tidyverse)
library(performance) # check model

# 📚 Neste módulo, vamos explorar relações entre variáveis numéricas usando correlação e regressão linear.
# A correlação mede o grau de associação entre duas variáveis, enquanto a regressão avalia como uma variável pode ser predita por outra.

# Carregando os dados necessários
data("iris")
?iris


## 📌 Seção 8.1 – Correlação

# 🎯 Objetivo: verificar a intensidade e direção da associação linear entre duas variáveis quantitativas
# H0: não há correlação (ρ = 0)
# H1: há correlação (ρ ≠ 0)

# 🔍 Correlação de Pearson (paramétrica)
iris %>% with(cor.test(Sepal.Length, Sepal.Width, method = "pearson"))

# 📐 Outras opções:
# ➤ Spearman (não paramétrica): method = "spearman"
# ➤ Kendall (não paramétrica): method = "kendall"

# 📈 Visualização
iris %>% 
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "📈 Dispersão: Comprimento sépala x Largura sépala", x = "Comprimento sépala", y = "Largura sépala") +
  theme_minimal(base_size = 16)


## 📌 Seção 8.2 – Regressão linear simples

# 🎯 Objetivo: modelar a relação entre uma variável dependente e uma independente
# Modelo: Y = β0 + β1X + ε

# 🔍 Ajustando o modelo
modelo <- lm(Sepal.Length ~ Sepal.Width, data = iris)
summary(modelo)  # Inclui R², coeficientes e p-valores

# 📉 Visualizando a reta de regressão
plot_regressao <- iris %>% 
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(title = "Comprimento sépala x Largura sépala", x = "Comprimento sépala", y = "Largura sépala") +
  theme_minimal()
print(plot_regressao)


## 📌 Seção 8.3 – Diagnóstico do modelo

# 🎯 Verificar suposições do modelo:
# 1. Resíduos com média zero
# 2. Homocedasticidade (variância constante dos resíduos)
# 3. Independência dos resíduos
# 4. Normalidade dos resíduos

# 📊 Plots diagnósticos
par(mfrow = c(2, 2))
plot(modelo)
par(mfrow = c(1, 1))

check_model(modelo)
check_outliers(modelo,method = 'zscore', threshold = list('zscore' = 3))
check_autocorrelation(modelo)

# 📏 Extra: plotando resíduos manualmente
residuos <- resid(modelo)
fitted <- fitted(modelo)

ggplot(data.frame(fitted, residuos), aes(x = fitted, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "📏 Resíduos vs Valores Ajustados") +
  theme_minimal()


## 📝 Exercício Final – Módulo 8

# 1. Escolha duas variáveis quantitativas do dataset e calcule a correlação (Pearson ou Spearman)
# 2. Crie um gráfico de dispersão com reta de tendência
# 3. Ajuste um modelo de regressão linear simples
# 4. Interprete os coeficientes, R² e valor de p
# 5. Realize o diagnóstico dos resíduos
# 6. Escreva uma conclusão sobre a adequação do modelo

# ✅ Fim do Módulo 8 – Correlação e Regressão
