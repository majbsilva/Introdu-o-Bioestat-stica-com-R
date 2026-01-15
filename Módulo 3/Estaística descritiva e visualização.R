# ======================================================
# Módulo 3 – Estatística Descritiva e Visualização
# ======================================================

## Carregando pacotes
library(tidyverse) #ggplot
library(janitor) # Tabelas de frequência
library(flextable) # Para utilizar a função flextable
library(modelsummary)
library(DescTools)


Desc(mtcars)

datasummary_skim(mtcars)
datasummary_balance(mpg~vs, mtcars)

## 📌 Seção 3.1 – Estatística descritiva

# Carregando dataset
data("mtcars")
?mtcars

# Preparando o dataset para as análises

mtcars <- mtcars %>% rename(
  'milhas por galão' = mpg,
  'número de cilindros' = cyl,
  'Cilindradas' = disp,
  'Cavalos de força' = hp,
  'Relação eixo traseiro' = drat,
  'Peso(lb)' = wt,
  'Tempo 1/4 milha' = qsec,
  'Tipo motor' = vs,
  'Transmissão' = am
)
glimpse(mtcars)

mtcars <- mtcars %>% mutate(
  `Tipo motor` = recode(factor(`Tipo motor`),
                        '0' = 'V-shaped',
                        '1' = 'Straight'),
  Transmissão = recode(factor(Transmissão),
         '0' = 'Automático',
         '1' = 'Manual')
)


# 📉 Medidas de tendência central e dispersão
summary(mtcars)
mean(mtcars$`milhas por galão`, na.rm = TRUE)
median(mtcars$`milhas por galão`, na.rm = TRUE)
min(mtcars$`milhas por galão`, na.rm = TRUE)
max(mtcars$`milhas por galão`, na.rm = TRUE)
sd(mtcars$`milhas por galão`, na.rm = TRUE)
IQR(mtcars$`milhas por galão`, na.rm = TRUE)

# 📊 Tabela de frequências

tab_freq <- mtcars %>%
  tabyl(`Cavalos de força`) %>%
  adorn_totals()

# 📊 Salvando a Tabela de frequências com o Flextable

freq_trans <- mtcars %>%
  tabyl(Transmissão) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 0) %>% # Essa linha define a porcentagem
  adorn_totals(c("row")) %>% 
  flextable(.)

freq_motor <- mtcars %>% 
  tabyl(`Tipo motor`) %>% 
  adorn_percentages('col') %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_totals('row') %>% 
  flextable(.)

## 📌 Seção 3.2 – Visualização com ggplot2

# 📦 Boxplot
boxplot_milhas_galão <- mtcars %>% 
  ggplot(aes(x = Transmissão , y = `milhas por galão`)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(title = "Boxplot de milhas por galão por tipo de transmissão", x = "Tipo de transmissão", y = "Milhas por galão") +
  theme_minimal()
print(boxplot_milhas_galão)

# 📊 Histograma
histograma_milhas_galão <- mtcars %>% 
  ggplot(aes(x = `milhas por galão`)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "📊 Histograma de milhas por galão", x = "Milhas por galão", y = "Frequência") +
  theme_minimal()
print(histograma_milhas_galão)

# 🟢 Gráfico de densidade
densidade_milhas_galão <- mtcars %>% 
  ggplot(aes(x = `milhas por galão`)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  labs(title = "📈 Curva de Densidade de milhas por galão", x = "Milhas por galão", y = "Densidade") +
  theme_minimal()
print(densidade_milhas_galão)

# 🧊 Gráfico de barras (dados categóricos)
grafico_barras_milhas_galão <- mtcars %>% 
  ggplot(aes(x = Transmissão)) +
  geom_bar(fill = "mediumpurple") +
  labs(title = "Contagem por Tipo de Transmissão", x = "Tipo de Transmissão", y = "Contagem") +
  theme_minimal()
print(grafico_barras_milhas_galão)

# 🔵 Gráfico de dispersão (scatter plot)
# Gráfico do mtcars: milhas por galão vs peso
scatter_mtcars <- mtcars %>% 
  ggplot(aes(x = `Peso(lb)`, y = `milhas por galão`)) +
  geom_point(alpha = 1, color = "darkorange") +
  geom_smooth(method = 'lm', color = 'tomato3') +
  labs(title = "🔵 Dispersão: Peso (wt) vs Milhas por Galão (mpg)",
       x = "Peso do Carro (1000 lbs)", y = "Milhas por Galão") +
  theme_dark()
print(scatter_mtcars)

## 📌 Seção 3.3 – Gráficos Alternativos e Criativos

# 📍 Lollipop plot
lollipop <- ggplot(dados, aes(x = reorder(nome, idade), y = idade)) +
  geom_segment(aes(xend = nome, y = 0, yend = idade), color = "gray") +
  geom_point(size = 4, color = "darkorange") +
  coord_flip() +
  labs(title = "📍 Lollipop Plot – Idade por Pessoa", x = "Nome", y = "Idade") +
  theme_minimal()
print(lollipop)

# 🌲 Gráfico de floresta (simples)
library(dplyr)
library(ggplot2)

# Criando um exemplo fictício de odds ratio com IC
floresta <- tibble(
  variavel = c("Var1", "Var2", "Var3"),
  OR = c(1.5, 0.8, 2.1),
  lower = c(1.1, 0.6, 1.3),
  upper = c(2.0, 1.0, 3.4)
)

forest_plot <- ggplot(floresta, aes(x = variavel, y = OR)) +
  geom_point(size = 3, color = "forestgreen") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "🌲 Forest Plot – Odds Ratio", x = "Variável", y = "Odds Ratio") +
  theme_minimal()
print(forest_plot)

# 🎯 Point plot estilizado (scatter com média)
point_skill <- mtcars %>% 
  ggplot(aes(x = Transmissão, y = `Peso(lb)`)) +
  geom_jitter(width = 0.2, alpha = 1, color = "blue", size = 3, shape = 21) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, color = "red") +
  stat_summary(
    fun.data = mean_sdl,  # Calculates mean and standard deviation
    fun.args = list(mult = 1),  # mult = 1 for one standard deviation
    geom = "errorbar",  # Adds error bars
    width = 0.1,  # Width of the error bars
    linewidth = 1,
    color = "red"  # Color of the error bars
  ) +
  labs(
    x = "Tipo de transmissão",
    y = "Peso (1000 lb)"
  ) +
  theme_classic(base_size = 18, base_family = 'helvetica', base_line_size = 1)+
  theme(aspect.ratio = 1.5)+
  scale_x_discrete(expand = expansion(add = 0.99))
print(point_skill)

