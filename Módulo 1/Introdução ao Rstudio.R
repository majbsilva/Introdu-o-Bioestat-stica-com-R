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
3^2

# 💾 Atribuição de objetos
x <- 5
y <- 10
z <- x + y
z

# 🖨️ Impressão de objetos
print(z)
cat("O valor de z é:", z, "\n")

# ❓ Funções de ajuda
help(mean)
?mean

# 🧠 Dica: Use TAB no RStudio para completar comandos


## 📌 Seção 1.2 – Tipos de dados

# 🔢 Vetores
idades <- c(23, 45, 32, 27)
class(idades)
length(idades)
mean(idades)

# 🧬 Fatores
sexo <- factor(c("F", "M", "F", "M"))
levels(sexo)
table(sexo)

# 📋 Data frames
df <- data.frame(ID = 1:4, Idade = idades, Sexo = sexo)
df
str(df)
summary(df)

# 🧺 Listas
lista <- list(numeros = idades, tabela = df)
str(lista)
lista$tabela


## 📌 Seção 1.3 – Estruturas de controle

# 🔁 Condicionais
idade <- 25
if (idade >= 18) {
  cat("✅ Maior de idade\n")
} else {
  cat("🚫 Menor de idade\n")
}

# 🔄 Repetições
for (i in 1:5) {
  cat("Número:", i, "\n")
}

# 🔂 Funções personalizadas
soma_quadrado <- function(a, b) {
  return((a + b)^2)
}
soma_quadrado(2, 3)


## 📌 Seção 1.4 – Boas práticas no R

# ✅ Nomeação clara de objetos
media_idade <- mean(idades)

# ✅ Comentários claros e objetivos
# Este cálculo retorna a média das idades

# 🧼 Limpar o ambiente (quando necessário)
# rm(list = ls())

# 💾 Salvar e carregar dados
save(df, file = "dados/meu_dataframe.RData")
load("dados/meu_dataframe.RData")

# 📦 Dica: use projetos do RStudio para manter tudo organizado


