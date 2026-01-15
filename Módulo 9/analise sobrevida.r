# =============================================================================
# Aula: Análise de sobrevida
# Curso: Estatística aplicada
# Assunto: Curvas de Kaplan-Meier, testes de comparação e modelo de Cox
# =============================================================================
# =============================================================================
# 1. Carregando bibliotecas
# =============================================================================
library(survival)    # Funções para análise de sobrevida
library(survminer)    # Visualização de curvas de sobrevida
library(tidyverse)    # Manipulação e visualização de dados
library(broom)        # Organização de outputs estatísticos
library(gt)
# =============================================================================
# 2. Simulando dados de sobrevida (exemplo didático)
# =============================================================================
# Criando um dataset simulado para dois grupos: controle e tratamento
set.seed(123)  # Para reprodutibilidade
sobrevida <- data.frame(
  tempo = c(rnorm(6, 30, 4), rnorm(6, 43, 3)),  # Tempos de sobrevida
  status = c(0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0), # 1 = evento observado, 0 = censurado
  grupo = rep(c("ctrl", "trat"), each = 6)      # Grupos: controle e tratamento
)
# =============================================================================
# 3. Explorando os dados
# =============================================================================
# Visualizando os primeiros casos
head(sobrevida) %>% gt()
# Estatísticas descritivas por grupo
sobrevida %>%
  group_by(grupo) %>%
  summarise(
    n = n(),
    eventos = sum(status),
    media_tempo = round(mean(tempo),2),
    dp_tempo = round(sd(tempo),2)
  ) %>% gt()
# =============================================================================
# 4. Estimando a curva de Kaplan-Meier
# =============================================================================
# Criando objeto Surv (combina tempo e status)
surv_obj <- Surv(sobrevida$tempo, sobrevida$status)
# Ajustando o modelo de Kaplan-Meier
fit_km <- survfit(surv_obj ~ grupo, data = sobrevida)
# Visualizando a curva de sobrevida
ggsurvplot(
  fit_km,
  pval = TRUE,                # Adiciona p-valor do teste de Log-Rank
  pval.method = T,
  conf.int = F,            # Intervalo de confiança
  risk.table = F,          # Tabela de risco
  palette = c("#E7B800", "#2E9FDF"),  # Cores personalizadas
  title = "Curva de sobrevida por grupo",
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de sobrevida",
  risk.table.height = 0.25    # Ajuste da altura da tabela de risco
)
# =============================================================================
# 5. Testes de comparação entre curvas
# =============================================================================
# Teste de Log-Rank (sensível a diferenças ao longo de todo o tempo)
logrank_test <- survdiff(surv_obj ~ grupo, data = sobrevida, rho = 0)
logrank_test
# Teste de Peto-Peto (sensível a diferenças precoces)
petopeto_test <- survdiff(surv_obj ~ grupo, data = sobrevida, rho = 1)
petopeto_test
# Teste de Tarone-Ware (sensível a diferenças intermediárias (entre início e fim))
taroneware_test <- survdiff(surv_obj ~ grupo, data = sobrevida, rho = 0.5)
taroneware_test
# =============================================================================
# 6. Modelo de regressão de Cox
# =============================================================================
# Usando dataset "lung" do pacote survival (exemplo real)
lung <- survival::lung
lung$status <- lung$status - 1  # Ajustando status para 0/1
lung$sex <- factor(lung$sex, labels = c('Male', 'Female'))
# Ajustando modelo de Cox para idade e sexo
modelo_cox <- coxph(Surv(time, status) ~ age + sex, data = lung)
# Sumário do modelo
summary(modelo_cox)

# =============================================================================
# 7. Verificando pressupostos do modelo de Cox
# =============================================================================
# Pressuposto de proporcionalidade (resíduos de Schoenfeld)
cox_zph <- cox.zph(modelo_cox)
cox_zph  # Teste formal
# Gráfico dos resíduos de Schoenfeld
plot(cox_zph)  # Avaliando proporcionalidade para "sexo"

# =============================================================================
# 8. Ajustando modelo com interação ou efeitos não lineares
# =============================================================================
# Modelo com interação entre idade e sexo
modelo_cox_interacao <- coxph(Surv(time, status) ~ age * sex, data = lung)
summary(modelo_cox_interacao)
# Comparando modelos com e sem interação (ANOVA)
anova(modelo_cox, modelo_cox_interacao, test = "LRT")
# Modelo com spline para idade (avaliando não linearidade)
modelo_cox_spline <- coxph(Surv(time, status) ~ pspline(age), data = lung)
summary(modelo_cox_spline) 

# =============================================================================
# 9. Visualização do modelo de Cox
# =============================================================================
# Curvas de sobrevida ajustadas por idade e sexo
fit_cox_age_sex <- survfit(modelo_cox)
ggsurvplot(
  fit_cox_age_sex,
  data = lung,
  risk.table = F,
  pval = FALSE,
  title = "Sobrevida ajustada por sexo (modelo de Cox)",
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de sobrevida"
)

# Curvas de sobrevida comparando o grupo sexo

fit_km <- survfit(Surv(time, status) ~ sex, data = lung)

ggsurvplot(
  fit_km,
  pval = T,
  pval.method = T
)

# Comparando as curvas com relação ao grupo sexo

# Teste de Log-Rank (sensível a diferenças ao longo de todo o tempo)
logrank_test <- survdiff(surv_obj ~ grupo, data = lung, rho = 0)
logrank_test
# Teste de Peto-Peto (sensível a diferenças precoces)
petopeto_test <- survdiff(surv_obj ~ grupo, data = lung, rho = 1)
petopeto_test
# Teste de Tarone-Ware (sensível a diferenças intermediárias (entre início e fim))
taroneware_test <- survdiff(surv_obj ~ grupo, data = lung, rho = 0.5)
taroneware_test

# =============================================================================
# 10. Salvando resultados
# =============================================================================
analise_sobrevida <- list(
  LOGRANK = logrank_test,
  PETO_PETO = petopeto_test,
  TARONE_WARE = taroneware_test,
  COX_SIMPLES = summary(modelo_cox),
  COX_INTERACAO = summary(modelo_cox_interacao),
  PRESSUPOSTOS = cox_zph
)
# Salvando saída em arquivo de texto
capture.output(analise_sobrevida, file = "Módulo 9/analise_sobrevida.txt")
# =============================================================================
# 11. Informações teóricas para aula
# =============================================================================
# --- Conceitos-chave ---
# 1. **Função Surv()**: Combina tempo e status em um objeto para análise de sobrevida.
# 2. **Kaplan-Meier**: Estima a probabilidade de sobrevida ao longo do tempo, considerando censuras.
# 3. **Testes de comparação**:
#    - Log-Rank: Sensível a diferenças em todo o período.
#    - Peto-Peto: Sensível a diferenças precoces. Criado por Richard Peto and Julian Peto
#    - Tarone-Ware: Sensível a diferenças intermediárias.
# 4. **Modelo de Cox**:
#    - Semiparamétrico: Não assume distribuição para o tempo de sobrevida.
#    - Hazard Ratio (HR): Razão de risco (ex: HR=2 significa dobro do risco).
# 5. **Pressupostos do Cox**:
#    - Proporcionalidade: O efeito das covariáveis deve ser constante ao longo do tempo.
#    - Linearidade: Variáveis contínuas devem ter relação linear com o log(HR).
# 6. **Interpretação**:
#    - p < 0.05: Diferença significativa entre curvas ou efeito da covariável.
#    - HR > 1: Maior risco; HR < 1: Menor risco.
