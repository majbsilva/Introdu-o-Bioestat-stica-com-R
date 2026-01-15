# ======================================================
# Módulo 2 – Importação e manipulação de dados
# ======================================================

# 📦 Carregar pacotes necessários
library(tidyverse)
library(modelsummary)
library(janitor) # função clear_names
library(flextable)

## 📌 Seção 2.1 – Importando dados


# 📂 Importação de um arquivo CSV
dados <- read_csv2("dados/Album Sales - mulitvariate example.csv")

# Melhorando a visualização de propaganda
dados$propaganda <- dados$propaganda/1000

dados <- dados %>% rename(
  propaganda,
  'propaganda(x1000)' = 'propaganda'
)

# 👀 Visualizar as primeiras linhas 
dim(dados)
head(dados)

# 🧠 Dica: arquivos Excel podem ser lidos com readxl::read_excel()


## 📌 Seção 2.2 – Limpando e organizando dados

# 🧽 Padronizar nomes das variáveis
dados <- clean_names(dados)

# 🧮 Manipulação com dplyr dentro do pacote tidyverse

# 🔍 Filtragem de dados não faltantes e criação de nova variável
# Se gasto com propaganda for menor que 120, entre 120 e 830 e maior do que 830
dados <- dados %>% 
  filter(!is.na(`propaganda_x1000`)) %>% mutate(
    propaganda_cat = case_when(
       `propaganda_x1000`<=120 ~ 'Pequeno gasto',
       `propaganda_x1000` >121 & `propaganda_x1000`<=830 ~ 'Médio gasto',
       `propaganda_x1000`>831 ~ 'Alto gasto'
    ))
  
# 🧵 Visualização da estrutura do dataset e da estatística básica
glimpse(dados)
datasummary_skim(dados)

## 📌 Seção 2.3 – Seleção, ordenação e renomeação

# 🔍 Selecionar colunas específicas
dados_select <- dados %>% 
  select(propaganda_x1000, propaganda_cat, vendas, atracao_do_album)

# ↕️ Ordenar por gasto com propaganda decrescente
dados_arrumados <- dados %>% 
  arrange(dados_select, desc(propaganda_x1000))

# 🏷️ Renomear colunas
dados_renomeado <- dados_arrumados %>% rename('Atração do álbum' = `Atracao do album`)

# 📦 Visualizar resultado
head(dados_renomeado)


## 📌 Seção 2.4 – Agrupamento e sumarização

# 📊 Média de gasto com propaganda por grupo
resumo <- dados %>%
  group_by(propaganda_cat) %>%
  summarise(
    n = n(),
    Média = mean(`propaganda_x1000`, na.rm = TRUE),
    `Desvio padrão` = sd(`propaganda_x1000`, na.rm = TRUE)
  )
print(resumo)

## 📊 Preparando a tabela resumo para publicação

resumo <- resumo %>% 
  flextable() %>% 
  autofit() %>% 
  set_header_labels(propaganda_cat = '') %>% 
  colformat_double(j = 'Média', digits = 1, prefix = "R$ ") %>% 
  colformat_double(j = 'Desvio padrão', digits = 1, prefix = "R$ ")

print(resumo)

# 📈 Gráfico opcional para visualização
resumo %>% 
  ggplot(aes(x = propaganda_cat, y = Média)) +
  geom_col(fill = "coral") +
  labs(title = "Média de Idade por Grupo", y = "Média", x = "Grupo") +
  theme_minimal()

# 📈 Gráfico opcional para visualização 
dados %>%
  ggplot(aes(x = propaganda_cat, y = propaganda_x1000)) +
  stat_summary(fun = mean, geom = 'bar', color = 'black', fill = 'white', linewidth = 0.8, width = 0.7)+
  stat_summary(fun.data = mean_sdl, 
               geom = 'errorbar', 
               fun.args = list(mult = 1),
               linewidth = 0.7,
               width = 0.2
               ) +
  labs(
    x = 'Faixa de gasto',
    y = 'Gasto propaganda (x R$1000)'
  ) +
  theme_classic(base_size = 18, base_family = 'helvetica') +
  theme(aspect.ratio = 1.1) + 
  scale_y_continuous(expand = expansion(mult = c(0,0)),
                     limits = c(0,1600)
                     )
  

  

