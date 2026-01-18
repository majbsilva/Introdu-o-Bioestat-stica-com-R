# ==============================================================================
# CURSO: BIOESTATÍSTICA COM R – FUNDAMENTOS E APLICAÇÕES
# Módulo 1: Introdução ao R e RStudio para Pesquisadores
# ==============================================================================

# ------------------------------------------------------------------------------
# Seção 1.1: Operações Básicas e Interface
# ------------------------------------------------------------------------------

# Operações matemáticas (úteis para cálculos de diluição e preparo de reagentes)
2 + 2          # Adição
sqrt(16)       # Raiz quadrada
log(10)        # Logaritmo natural
log10(100)     # Logaritmo na base 10
exp(2)         # Exponencial
3^2            # Potência

# Atribuição de objetos (armazenando valores na memória)
concentracao_estoque <- 500  # em mg/mL
volume_final <- 10           # em mL
massa_total <- concentracao_estoque * volume_final
massa_total

# Impressão de resultados no console
print(massa_total)
cat("A massa total necessária é:", massa_total, "mg\n")

# Consulta de documentação (essencial para entender novos pacotes)
help(mean)
?sd  # Atalho para o desvio padrão

# ------------------------------------------------------------------------------
# Seção 1.2: Tipos e Estruturas de Dados
# ------------------------------------------------------------------------------

# Vetores (sequência de dados do mesmo tipo, ex: absorbância ou peso)
pesos_camundongos <- c(23.5, 25.1, 22.8, 27.3)
class(pesos_camundongos)
length(pesos_camundongos)
mean(pesos_camundongos)

# Fatores (variáveis categóricas, ex: grupos de tratamento ou genótipos)
tratamento <- factor(c("Controle", "Droga_A", "Controle", "Droga_A"))
levels(tratamento)
table(tratamento)

# Data frames (tabelas de dados - a estrutura mais comum na pesquisa)
experimento_1 <- data.frame(
  ID = 1:4, 
  Peso = pesos_camundongos, 
  Grupo = tratamento
)
experimento_1
str(experimento_1)    # Visualiza a estrutura da tabela
summary(experimento_1) # Resumo estatístico rápido

# Listas (coleções de objetos de diferentes tipos e tamanhos)
lista_resultados <- list(dados = experimento_1, data_experimento = "2026-01-15")
str(lista_resultados)
lista_resultados$dados

# ------------------------------------------------------------------------------
# Seção 1.3: Estruturas de Controle e Funções
# ------------------------------------------------------------------------------

# Condicionais (automatização de decisões com base em resultados)
valor_p <- 0.034
if (valor_p < 0.05) {
  cat("Resultado estatisticamente significativo\n")
} else {
  cat("Não houve significância estatística\n")
}

# Repetições (Loops - úteis para processar vários arquivos ou genes)
for (i in 1:3) {
  cat("Processando Triplicata", i, "\n")
}

# Funções personalizadas (para cálculos repetitivos no laboratório)
calcular_diluicao <- function(c1, v2, c2) {
  v1 <- (c2 * v2) / c1
  return(v1)
}
# Exemplo: Quanto preciso de um estoque de 100mM para ter 50ml a 5mM?
calcular_diluicao(100, 50, 5)

# ------------------------------------------------------------------------------
# Seção 1.4: Boas Práticas e Organização
# ------------------------------------------------------------------------------

# Nomes de objetos: use letras minúsculas e underscores para clareza
media_grupo_controle <- mean(pesos_camundongos)

# Comentários: sempre documente o PORQUÊ do comando
# O cálculo abaixo remove outliers detectados pelo critério X
# (comando de exemplo)

# Gestão de Ambiente
# ls()                # Lista todos os objetos criados
# rm(list = ls())     # Limpa todo o ambiente (use com cautela)

# Exportação e Importação de Dados
# write.csv(experimento_1, "resultados_bancada.csv", row.names = FALSE)
# dados_importados <- read.csv("resultados_bancada.csv")

# Dica de Fluxo de Trabalho:
# Sempre utilize Projetos do RStudio (.Rproj) para manter caminhos de arquivos relativos.