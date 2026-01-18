# ==============================================================================
# Módulo 2: Importação e Manipulação de Dados Experimentais
# ==============================================================================

# Carregar pacotes necessários
library(readxl)       # Específico para importar Excel
library(tidyverse)    # Manipulação e visualização
library(modelsummary) # Resumos estatísticos
library(janitor)      # Limpeza de nomes
library(flextable)    # Tabelas formatadas

# ------------------------------------------------------------------------------
# Seção 2.1: Importação de dados
# ------------------------------------------------------------------------------

# Importando a planilha do Excel
# Certifique-se de que o arquivo está na sua pasta de projeto
dados_laboratorio <- read_excel("Módulo 2/dados_experimento.xlsx")

# Visualizar as primeiras linhas e a estrutura dos dados importados
dados_laboratorio %>% dim()
dados_laboratorio %>% head()
dados_laboratorio %>% glimpse()
dados_laboratorio %>% skimr::skim()
dados_laboratorio %>% DescTools::Desc()


# ------------------------------------------------------------------------------
# Seção 2.2: Limpando e organizando dados
# ------------------------------------------------------------------------------

# Padronizar nomes das variáveis (converte para snake_case)
dados <- clean_names(dados_laboratorio)

# Manipulação com dplyr: Filtragem e criação de novas categorias
# Categorizando a resposta citotóxica baseada na viabilidade (%)
dados <- dados %>% 
  filter(!is.na(viabilidade)) %>% 
  mutate(
    status_citotoxico = case_when(
      viabilidade >= 80 ~ "Baixa citotoxicidade",
      viabilidade >= 50 & viabilidade < 80 ~ "Citotoxicidade moderada",
      viabilidade < 50 ~ "Alta citotoxicidade"
    )
  )

# Resumo estatístico rápido das variáveis numéricas
dados %>% group_by(status_citotoxico) %>%  datasummary_skim()
dados %>% str()

# ------------------------------------------------------------------------------
# Seção 2.3: Seleção, Ordenação e Renomeação
# ------------------------------------------------------------------------------

# Selecionar colunas de interesse e ordenar por viabilidade decrescente
dados_final <- dados %>% 
  select(tratamento, tempo, viabilidade, status_citotoxico) %>% 
  arrange(desc(viabilidade)) %>% 
  rename(
    "Grupo de Tratamento" = tratamento,
    "Tempo de Exposição" = tempo,
    "Percentual Viabilidade" = viabilidade
  )

dados_final %>% head()
dados_final %>% glimpse()


# ------------------------------------------------------------------------------
# Seção 2.4: Agrupamento e sumarização
# ------------------------------------------------------------------------------

# Calcular Média e Desvio Padrão por Grupo e Tempo
resumo_estatistico <- dados %>%
  group_by(tratamento, tempo, status_citotoxico) %>%
  summarise(
    n = n(),
    Media = mean(viabilidade, na.rm = TRUE),
    Desvio_Padrao = sd(viabilidade, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Seção 2.5: Preparando tabela para publicação
# ------------------------------------------------------------------------------

# Certifique-se de carregar o pacote
library(gt)

tabela_publicacao <- resumo_estatistico %>% 
  gt() %>% 
  # Renomear as colunas
  cols_label(
    tratamento = "Tratamento",
    tempo = "Tempo",
    n = "N",
    Media = "Média (%)",
    Desvio_Padrao = "D.P."
  ) %>% 
  # Formatar números (2 casas decimais)
  fmt_number(
    columns = c(Media, Desvio_Padrao),
    decimals = 2
  ) %>% 
  # Estilização: Negrito no cabeçalho
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>% 
  # Alinhamento centralizado para colunas numéricas
  cols_align(
    align = "center",
    columns = c(n, Media, Desvio_Padrao)
  ) %>% 
  # Adicionar uma linha de separação ou título (opcional)
  tab_header(
    title = "Resumo da Viabilidade Celular",
    subtitle = "Comparação entre tratamentos e tempos de exposição"
  )

# Visualizar a tabela
print(tabela_publicacao)

# ------------------------------------------------------------------------------
# Seção 2.6: Visualização Gráfica (Padrão Acadêmico)
# ------------------------------------------------------------------------------

dados %>%
  ggplot(aes(x = tratamento, y = viabilidade, fill = tempo)) +
  stat_summary(
    fun = mean, 
    geom = "bar", 
    position = position_dodge(0.8), 
    color = "black", 
    width = 0.7
  ) +
  stat_summary(
    fun.data = mean_se, 
    geom = "errorbar", 
    position = position_dodge(0.8), 
    width = 0.2
  ) +
  labs(
    title = "Efeito dos Compostos na Viabilidade Celular",
    x = "Tratamento",
    y = "Viabilidade (%)",
    fill = "Tempo"
  ) +
  scale_fill_grey(start = 0.4, end = 0.9) + 
  theme_classic(base_size = 14) +
  theme(aspect.ratio = 0.8) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 110))