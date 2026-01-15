# ============================================
# Curso: Bioestatística com R – Fundamentos e Aplicações
# ============================================

# ======================================================
# Módulo 1 – Introdução ao R e RStudio
# ======================================================

## 📌 Seção 1.1 – Conhecendo o R
# ⚙️ Operações básicas
2 + 2
sqrt(16)
log(10)
exp(2)

# 💾 Atribuição de objetos
x <- 5
y <- 10
z <- x + y
z

## 📌 Seção 1.2 – Tipos de dados
# 🔢 Vetores
idades <- c(23, 45, 32, 27)
class(idades)

# 🧬 Fatores
sexo <- factor(c("F", "M", "F", "M"))
levels(sexo)

# 📋 Data frames
df <- data.frame(ID = 1:4, Idade = idades, Sexo = sexo)
df

# 🧺 Listas
lista <- list(numeros = idades, tabela = df)
str(lista)


# ======================================================
# Módulo 2 – Importação e manipulação de dados
# ======================================================

## 📌 Seção 2.1 – Importando dados
library(readr)
dados <- read_csv("dados/exemplo.csv")
head(dados)

## 📌 Seção 2.2 – Limpando e organizando dados
library(janitor)
dados <- clean_names(dados)

library(dplyr)
dados <- dados %>% 
  filter(!is.na(idade)) %>% 
  mutate(grupo_idade = ifelse(idade > 40, "acima_40", "ate_40"))

glimpse(dados)


# ======================================================
# Módulo 3 – Estatística Descritiva e Visualização
# ======================================================

## 📌 Seção 3.1 – Estatística descritiva
summary(dados$idade)
mean(dados$idade, na.rm = TRUE)
sd(dados$idade, na.rm = TRUE)

## 📌 Seção 3.2 – Visualização com ggplot2
library(ggplot2)

# 📦 Boxplot
ggplot(dados, aes(x = grupo_idade, y = idade)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "📦 Boxplot de Idade por Grupo") +
  theme_minimal()

# 📊 Histograma
ggplot(dados, aes(x = idade)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "📊 Histograma de Idade", x = "Idade", y = "Frequência") +
  theme_minimal()


# ======================================================
# Módulo 4 – Distribuições e Probabilidades
# ======================================================

## 📌 Seção 4.1 – Distribuição normal
curve(dnorm(x, mean = 0, sd = 1), from = -4, to = 4, col = "blue", lwd = 2,
      main = "📈 Distribuição Normal", xlab = "x", ylab = "Densidade")

## 📌 Seção 4.2 – Simulação de dados
amostra <- rnorm(1000, mean = 100, sd = 15)
hist(amostra, breaks = 30, col = "lightgreen", main = "🎲 Histograma da Amostra")


# ======================================================
# Módulo 5 – Testes de Hipótese
# ======================================================

## 📌 Seção 5.1 – Teste t
with(dados, t.test(idade ~ grupo_idade))

## 📌 Seção 5.2 – ANOVA
# Suponha variável "grupo" com 3 categorias
aov_res <- aov(idade ~ grupo, data = dados)
summary(aov_res)


# ======================================================
# Módulo 6 – Testes não paramétricos
# ======================================================

## 📌 Seção 6.1 – Mann-Whitney
wilcox.test(idade ~ grupo_idade, data = dados)

## 📌 Seção 6.2 – Kruskal-Wallis
kruskal.test(idade ~ grupo, data = dados)


# ======================================================
# Módulo 7 – Dados categóricos
# ======================================================

## 📌 Seção 7.1 – Qui-quadrado
# 🧮 Tabela de contingência
tab <- table(dados$sexo, dados$grupo_idade)
tab
chisq.test(tab)

## 📌 Seção 7.2 – Teste de Fisher
fisher.test(tab)


# ======================================================
# Módulo 8 – Correlação e Regressão
# ======================================================

## 📌 Seção 8.1 – Correlação
cor.test(dados$idade, dados$peso, method = "pearson")

## 📌 Seção 8.2 – Regressão linear
modelo <- lm(peso ~ idade, data = dados)
summary(modelo)

# 🔍 Diagnóstico visual
diagnostic_plots <- par(mfrow = c(2,2))
plot(modelo)
par(diagnostic_plots)


# ======================================================
# Módulo 9 – RMarkdown e relatório final
# ======================================================

## 📌 Seção 9.1 – RMarkdown básico
# Abrir um novo arquivo RMarkdown no RStudio e incluir trechos como:
# ```{r}
# summary(dados)
# ggplot(dados, aes(x = idade)) + geom_histogram()
# ```

## 📌 Seção 9.2 – Projeto aplicado
# ✅ Passos sugeridos:
# 1️⃣ Escolher conjunto de dados (real ou simulado)
# 2️⃣ Realizar análise descritiva e inferencial
# 3️⃣ Apresentar conclusões via RMarkdown (HTML ou PDF)
