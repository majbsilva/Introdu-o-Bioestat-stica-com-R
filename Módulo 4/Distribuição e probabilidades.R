# ======================================================
# Módulo 4 – Distribuições e Probabilidades
# ======================================================

# 📚 Neste módulo, vamos explorar distribuições probabilísticas, simulações e cálculos de probabilidades.


## 📌 Seção 4.1 – Distribuição Normal

# Curva da distribuição normal padrão N(0,1)
curve(dnorm(x, mean = 0, sd = 1), 
      from = -3, to = 3, 
      col = "blue", lwd = 2,
      main = "📈 Distribuição Normal Padrão", 
      xlab = "x", ylab = "Densidade")

# Área sob a curva até um ponto específico
pnorm(1.96)  # P(Z ≤ 1.96)

# Valor de corte (quantil) correspondente a 97,5%
qnorm(0.975)  # Retorna o valor z tal que P(Z ≤ z) = 0.975

### 📝 Exercício 4.1
# 1. Qual a probabilidade de Z < -1.5?
# 2. Qual valor z corresponde ao percentil 90%?

## 📌 Seção 4.2 – Simulação de dados

# 🎲 Gerando uma amostra aleatória de uma normal com média 100 e desvio 15
set.seed(123)
amostra <- rnorm(1000, mean = 100, sd = 15)

# Histograma da amostra simulada
hist(amostra, 
     breaks = 30,
     freq = F,
     col = "lightgreen", 
     main = "🎲 Histograma de Amostra Normal",
     xlab = "Valor", ylab = "Frequência")

# Curva de densidade sobreposta ao histograma
lines(density(amostra), col = "darkgreen", lwd = 2)

### 📝 Exercício 4.2
# 1. Gere uma nova amostra com média 50 e desvio 10.
# 2. Compare graficamente as duas distribuições usando density().

## 📌 Seção 4.3 – Cálculo de probabilidades

# Probabilidade de X < 120 se X ~ N(100, 15²)
pnorm(120, mean = 100, sd = 15)

# Probabilidade de X > 120 se X ~ N(100, 15²)
1 - pnorm(120, mean = 100, sd = 15)

# Quantil de 90% de uma N(100, 15²)
qnorm(0.90, mean = 100, sd = 15)

# Probabilidade entre dois valores: P(85 < X < 115)
pnorm(115, 100, 15) - pnorm(85, 100, 15)

### 📝 Exercício 4.3
# 1. Qual a probabilidade de um paciente ter mais de 130 mg/dL de glicemia se a média é 100 e sd = 15?
# 2. Calcule o intervalo central que contém 95% dos dados de uma distribuição normal com média 80 e sd = 10.

## 📌 Seção 4.4 – Outras distribuições

# 📦 Distribuição Binomial: P(X = 3) onde X ~ Bin(n = 10, p = 0.5)
dbinom(3, size = 10, prob = 0.5)

# Gráfico da distribuição binomial
barplot(dbinom(0:10, size = 10, prob = 0.5),
        names.arg = 0:10,
        col = "lightblue",
        main = "📦 Distribuição Binomial (n=10, p=0.5)",
        xlab = "Sucessos", ylab = "Probabilidade")

# 🔔 Distribuição de Poisson: P(X = 2) com λ = 4
dpois(2, lambda = 4)

# Gráfico da Poisson
barplot(dpois(0:10, lambda = 4),
        names.arg = 0:10,
        col = "orange",
        main = "🔔 Distribuição de Poisson (λ = 4)",
        xlab = "Ocorrências", ylab = "Probabilidade")

# 🎯 Distribuição Qui-quadrado com 5 graus de liberdade
curve(dchisq(x, df = 5), from = 0, to = 20, 
      col = "purple", lwd = 2,
      main = "🎯 Distribuição Qui-quadrado (GL = 5)",
      xlab = "x", ylab = "Densidade")

# Probabilidade acumulada até o valor 10
pchisq(10, df = 5)

### 📝 Exercício 4.4
# 1. Calcule a probabilidade de obter 4 sucessos em 8 tentativas com p = 0.6.
# 2. Qual a probabilidade de obter até 2 eventos se X ~ Poisson(λ = 3)?
# 3. Use curve() para visualizar a distribuição Qui-quadrado com df = 10.

# Testando o tipo de distribuição com testes de distribuição

## Teste de Shapiro Wilk para testar distribuição normal
shapiro.test(amostra)

### Exemplo do resultado desse teste para uma distribuição que não é normal

set.seed(123)
poisson <- rpois(100,0.1)
shapiro.test(poisson)
fit <- fitdistr(poisson, 'poisson')
qqplot(rpois(length(poisson), lambda = fit$estimate), poisson)

# Avaliando qual é a distribuição quando essa não é normal----

# Load necessary libraries
library(fitdistrplus) # For fitdist
library(ggplot2) # For ggplot

# Generate Beta distributed data
set.seed(123) # For reproducibility
beta <- rbeta(100, shape1 = 1, shape2 = 2)

# Plot histogram of the Beta data
hist(beta)

# Fit Beta distribution to the data
beta_fit <- fitdist(beta, 'beta')

# Create theoretical quantiles for Q-Q plot
beta_teorico_q <- qbeta(ppoints(length(beta)),
                        shape1 = beta_fit$estimate["shape1"],
                        shape2 = beta_fit$estimate["shape2"])

# Create empirical quantiles for Q-Q plot
beta_empirico_q <- sort(beta)

# Create a data frame for the Q-Q plot
beta_qqplot_df <- data.frame(
  teórico = beta_teorico_q,
  empírico = beta_empirico_q
)

# Create the Q-Q plot
beta_qqplot_df %>% 
  ggplot(aes(x = teórico, y = empírico)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black', linewidth = 0.5) +
  labs(title = "Q-Q Plot - Beta",
       x = "Quantis Teóricos",
       y = "Quantis Empíricos") +
  theme_minimal()


## Exemplo 2: distribuição poisson----
library(MASS) # fitdistr - para analisar distribuição poisson

poisson <- rpois(100,2)
hist(poisson)

### Definindo parametros da distribuição
poisson_fit <- fitdistr(poisson, 'poisson')

### Criando os eixos do grafico qqplot
poisson_teorico_q <- qpois(ppoints(length(beta)),
                           lambda = poisson_fit$estimate
)
poisson_empirico_q <- sort(poisson)

### Criando o gráfico de visualizaçao

poisson_qqplot_df <- data.frame(
  teórico = poisson_teorico_q,
  empírico = poisson_empirico_q
)

poisson_qqplot_df %>% 
  ggplot(aes(x = teórico, y = empírico)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black', linewidth = 0.5) +
  labs(title = "Q-Q Plot - Poisson",
       x = "Quantis Teóricos",
       y = "Quantis Empíricos") +
  theme_minimal()

## Exemplo 3: distribuição exponencial

# Carregar bibliotecas necessárias
library(fitdistrplus) # Para fitdist
library(ggplot2) # Para ggplot

# Gerar dados com distribuição exponencial
set.seed(123) # Para reprodutibilidade
exponential_data <- rexp(100, rate = 0.5) # Você pode ajustar o parâmetro de taxa conforme necessário

# Plotar histograma dos dados exponenciais
hist(exponential_data)

# Ajustar distribuição exponencial aos dados
exponential_fit <- fitdist(exponential_data, 'exp')

# Criar quantis teóricos para o gráfico Q-Q
exponential_teorico_q <- qexp(ppoints(length(exponential_data)), rate = exponential_fit$estimate["rate"])

# Criar quantis empíricos para o gráfico Q-Q
exponential_empirico_q <- sort(exponential_data)

# Criar um data frame para o gráfico Q-Q
exponential_qqplot_df <- data.frame(
  teórico = exponential_teorico_q,
  empírico = exponential_empirico_q
)

# Criar o gráfico Q-Q
ggplot(exponential_qqplot_df, aes(x = teórico, y = empírico)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black', linewidth = 0.5) +
  labs(title = "Gráfico Q-Q - Exponencial",
       x = "Quantis Teóricos",
       y = "Quantis Empíricos") +
  theme_minimal()


## Exemplo 4: distribuição lognormal

# Carregar bibliotecas necessárias
library(fitdistrplus) # Para fitdist
library(ggplot2) # Para ggplot

# Gerar dados com distribuição lognormal
set.seed(123) # Para reprodutibilidade
lognormal_data <- rlnorm(100, meanlog = 0, sdlog = 1) # Você pode ajustar os parâmetros conforme necessário

# Plotar histograma dos dados lognormal
hist(lognormal_data)

# Ajustar distribuição lognormal aos dados
lognormal_fit <- fitdist(lognormal_data, 'lnorm')

# Criar quantis teóricos para o gráfico Q-Q
lognormal_teorico_q <- qlnorm(ppoints(length(lognormal_data)),
                              meanlog = lognormal_fit$estimate["meanlog"],
                              sdlog = lognormal_fit$estimate["sdlog"])

# Criar quantis empíricos para o gráfico Q-Q
lognormal_empirico_q <- sort(lognormal_data)

# Criar um data frame para o gráfico Q-Q
lognormal_qqplot_df <- data.frame(
  teórico = lognormal_teorico_q,
  empírico = lognormal_empirico_q
)

# Criar o gráfico Q-Q
ggplot(lognormal_qqplot_df, aes(x = teórico, y = empírico)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black', linewidth = 0.5) +
  labs(title = "Gráfico Q-Q - Lognormal",
       x = "Quantis Teóricos",
       y = "Quantis Empíricos") +
  theme_minimal()


# ✅ Fim do Módulo 4 – Distribuições e Probabilidades
