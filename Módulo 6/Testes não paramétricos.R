# ======================================================
# Módulo 6 – Testes não paramétricos
# ======================================================

# 📚 Neste módulo, exploramos testes não paramétricos — usados quando os pressupostos de normalidade e/ou homogeneidade de variâncias não são atendidos.
# Esses testes trabalham com **ranks** (posições dos valores ordenados) em vez dos próprios valores, sendo mais robustos a outliers e distribuições assimétricas.

## 🔍 Por que transformar dados em Ranks?
# ➤ Em vez de comparar médias, os testes não paramétricos comparam **posições relativas** (ranks).
# ➤ Exemplo: valores 5, 20, 100 têm ranks 1, 2, 3 respectivamente.
# ➤ Isso reduz o impacto de valores extremos e permite comparações mais robustas sem normalidade.

## 📌 Seção 6.1 – Teste de Mann-Whitney (Wilcoxon rank-sum)

# 🎯 Objetivo: comparar dois grupos independentes quando a normalidade não é assumida
# Equivalente não paramétrico do teste t
# H0: distribuições dos dois grupos são iguais

# 🔍 Teste de Mann-Whitney
wilcox.test(idade ~ grupo_idade, data = dados)

# 📈 Gráfico para visualização
library(ggpubr)
ggboxplot(dados, x = "grupo_idade", y = "idade",
          color = "grupo_idade", palette = "jco",
          add = "jitter") +
  labs(title = "📦 Boxplot para o Teste de Mann-Whitney")


## 📌 Seção 6.2 – Teste de Kruskal-Wallis

# 🎯 Objetivo: comparar 3 ou mais grupos independentes sem assumir normalidade
# Equivalente não paramétrico da ANOVA
# H0: as distribuições dos grupos são iguais

# 🔍 Teste de Kruskal-Wallis
kruskal.test(idade ~ grupo, data = dados)

# 📈 Gráfico
ggboxplot(dados, x = "grupo", y = "idade",
          color = "grupo", palette = "Dark2",
          add = "jitter") +
  labs(title = "📦 Boxplot para Kruskal-Wallis")

# ➕ Se significativo, realizar testes post-hoc com ajuste de p-valor
pairwise.wilcox.test(dados$idade, dados$grupo, p.adjust.method = "bonferroni")


## 📝 Exercício Final – Módulo 6

# 1. Verifique se os dados de idade são normalmente distribuídos nos grupos.
# 2. Se não forem, aplique o teste de Mann-Whitney (2 grupos) ou Kruskal-Wallis (3+ grupos).
# 3. Construa um gráfico com ggboxplot para ilustrar os grupos.
# 4. Se o Kruskal-Wallis for significativo, realize comparações múltiplas com correção de p-valor.
# 5. Interprete os resultados e compare com os testes paramétricos do módulo anterior.

# ✅ Fim do Módulo 6 – Testes não paramétricos
