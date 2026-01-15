# =============================================================================
# AULA: REGRESSÃO LOGÍSTICA BINÁRIA COM DADOS DE MELANOMA
# CURSO: ESTATÍSTICA APLICADA À SAÚDE E CIÊNCIAS BIOMÉDICAS
# OBJETIVO: DEMONSTRAR COMO CONSTRUIR, INTERPRETAR E AVALIAR UM MODELO DE REGRESSÃO LOGÍSTICA BINÁRIA PARA PREDIZER ÓBITO EM PACIENTES COM MELANOMA.
# Estudo de caso: tese de doutorado com 954 caes atendido no Hospital Veterinário da UFU. Vamos desevenvolver um modelo para avaliar se CASTRAÇÃO, IDADE, PESO, RAÇA e SEXO são possíveis preditores para o desfecho do diagnóstico de  os MASTOCITOMA.
# =============================================================================

# =============================================================================
# CARREGANDO BIBLIOTECAS E DADOS
# =============================================================================
# A biblioteca vem com um monte de função.

library(tidyverse)  # manipular planilhas
library(performance) # olhar os pressupostos da regressão logística.
library(sjPlot) # umas tabelas bonitinhas
library(pROC) # no nosso, eu quero que o modelo discrimine qual cao tem ou nao diagnostico de mastocitoma. A curva ROC vai me dar o poder discrimantório de SIM ou NAO. 
library(ResourceSelection) # funçao hoslem.test() para avaliar o ajuste.


ds_completo <- foreign::read.spss('data/Mastocitoma.sav', to.data.frame = TRUE) # alt + -
names(ds_completo)

# Selecionar as variáveis que vou trabalhar
# operador pipe control + shift + M
# ATRIBUIÇÃO

ds <-  ds_completo %>% select(Número, Ano, Sexo, Castração, Idade, Peso, Porte, Mastocitoma, Raça) 
names(ds)
ds <- janitor::clean_names(ds)
names(ds)


# =============================================================================
# EXPLORAÇÃO INICIAL DOS DADOS
# =============================================================================
glimpse(ds)

# Observamos que peso esta como variavel categórica. Preciso tranformar em variavel numérica. Mas para isso, eu preciso tranformar em texo.

ds <- ds %>% mutate(
  peso = as.character(peso),
  peso = as.numeric(peso)
)

# Explorar os dados faltantes
naniar::miss_var_summary(ds) # NA = not annoted

# Eliminar os casos incompletos

ds <- ds %>% filter(complete.cases(ds))

DescTools::Desc(ds)


# =============================================================================
# VISUALIZANDO OS DADOS
# =============================================================================

ds %>% 
  ggplot(aes(x = peso, fill = mastocitoma )) + 
  geom_density(alpha = 0.5)

ds %>% 
  ggplot(aes(x = idade, fill = mastocitoma)) +
  geom_density(alpha = 0.5)

ds %>% 
  ggplot(aes(x = peso, fill = mastocitoma)) +
  geom_density()

# =============================================================================
# VERIFICANDO AS CATEGORIAS DE REFERÊNCIA
# =============================================================================

## Categoria de referência é aquela em que o modelo vai comparar. Por exemplo, se eu digo que a chance de ter câncer de pulmão em fumantes é 4 vezes maior do que em não fumantes, o nível de referência da variável HÁBITO DE FUMAR é não fumantes. Isso é importante para podermos interpretar o modelo.
## Para isso, vamos utilizar a função levels() para ver os níveis da variável categórica. No resultado dessa função, o nível que aparece primeiro é o nível de referência.

# Variavel categórica não tem numero, ela tem NIVEIS. (SIM NAO)
levels(ds$raca) # variável categórica --> níveis (levels) --> "SRD" "CRD" --> que o primeiro nível que aparece é a minha categoria de referencia. 
levels(ds$castracao)

ds <- ds %>% mutate(
  castracao = relevel(
    castracao,
    ref = 'Não'
  )
)

levels(ds$mastocitoma)

# na variável desfecho, é melhor trabalhar não como variável categórica, mas sim como variável numérica (0 , 1). Na verdade, toda variável binária em um modelo logistico pode ser trabalhado como 0 e 1.

glimpse(ds)

ds <- ds %>% mutate(
  mastocitoma = as.numeric(mastocitoma)
)

ds$mastocitoma <- ds$mastocitoma - 1

# =============================================================================
# CONSTRUINDO O MODELO DE REGRESSÃO LOGÍSTICA BINÁRIA
# =============================================================================

# formula é a minha variável desfecho em relaçao aos meus preditores. O primeiro modelo vai ser um modelo de tentativa de adivinhar se o animal tem ou nao mastocitoma sem nenhum preditor. 
modelo0 <- glm(mastocitoma ~ 1, data = ds, family = binomial) # a minha variável desfecho (mastocitoma) é uma variável binárica (0,1)
summary(modelo0)

modelo1 <- glm(mastocitoma ~ castracao, data = ds, family = binomial)
summary(modelo1)

anova(modelo0,modelo1)

# como avaliar se um modelo é melhor do que outro
# tidyverse - manipular 
# naniar- dados faltantes # ggplot2 (ggplot) # rstatix, pastecs, performance


modelo <- glm(mastocitoma ~ castracao + idade + peso + raca + sexo, data = ds, family = binomial)
summary(modelo)

anova(modelo0, modelo1, modelo)

library(ggstatsplot)

ggcoefstats(modelo)

sjPlot::tab_model(modelo, show.intercept = F)

# Descriçao de dados

report:: report(modelo)

# O beta é o meu coeficiente (log do odds ratio). No modelo logistico, para eu transfomar o beta em odds ratio, eu preciso exponenciar os coeficientes

odds_ratio <- exp(coef(modelo)) 

odds_ratio_intervalo_confianca  <- exp(confint(modelo)
)  
# =============================================================================
# AVALIANDO OS PRESSUPOSTOS DO MODELO
# =============================================================================

## RELAÇÃO LINEAR

# No modelo logisticos (binário) existe um pressuposto de que o log de Odds (chances) tem uma relacao linear com as variáveis preditoras continuas (numéricas).
# Vou mostrar de uma forma visual. Para isso, eu vou plotar variável numerica (peso e idade) em relaçao aos desvios dos resíduos do meu modelo. Vou usar o desvio dos resíduos porque ele captura fenônemos que não são previstos no modelo (quebra da relacao linear.)

desvio_residuos <- residuals(modelo, type = 'deviance')

plot(ds$idade,desvio_residuos)
abline(h = 0)

plot(ds$peso , residuals(modelo, type = 'deviance') )
abline(h = 0)

# podemos obsrvar no gráfico que os desvios dos residuos não tem um comportamento linear em relacao ao peso.

ds$peso_log <- ds$peso * log(ds$peso) # essa variável vai me mostrar se realmente tem quebra de linearidade.

modelo_teste_linearidade <- glm(mastocitoma ~ castracao + idade + peso + raca + sexo + peso_log, data = ds, family = binomial)
summary(modelo_teste_linearidade)

# observamos no modelo acima, que a variável (peso * log(peso)) quando incluida no modelo deu um valor de p menor que 0.05, portanto, eu confirmo a quebra de linearidade para peso. Quando existe quebra de linearidade, uma das estratégias mais utilizadas é transformar a variável em log(peso) ou raiz quadrada de peso ou inverso de peso (1/peso) ou elevar peso a segunda (peso^2).

modelo_peso_log <- glm(mastocitoma ~ castracao + idade + log(peso) + raca + sexo, data = ds, family = binomial)
summary(modelo_peso_log)

ds$peso_log_log <- log(ds$peso) * log(log(ds$peso))  

modelo_peso_log <- glm(mastocitoma ~ castracao + idade + log(peso) + raca + sexo, data = ds, family = binomial)
summary(modelo_peso_log)

modelo_porte <- glm(mastocitoma ~ castracao + idade + peso + raca + sexo + porte, data = ds, family = binomial)
summary(modelo_porte)

check_collinearity(modelo_porte)

## MULTICOLINEARIDADE

# A multicolinearidade só é avaliada em modelos multilineares (com mais de uma variável preditora). As variáveis preditoras (as variáveis independentes, ou X's) não devem ser altamente correlacionadas entre si.
# Isso é importante porque alta multicolinearidade torna os erros-padrão dos coeficientes instáveis e inflacionados, dificultando a interpretação e superestimando o valor de p, podendo levar a um erro do tipo II.
# Para verificar a multicolinearidade, podemos calcular o Fator de Inflação da Variância (VIF). Geralmente, valores de VIF acima de 5 ou 10 indicam problemas.

modelo_porte <- glm(mastocitoma ~ castracao + idade + peso + raca + sexo + porte, data = ds, family = binomial)
summary(modelo_porte)

check_collinearity(modelo_porte)

modelo_peso_log <- glm(mastocitoma ~ castracao + idade + log(peso) + raca + sexo, data = ds, family = binomial)
summary(modelo_peso_log)

check_collinearity(modelo_peso_log)

modelo_porte <- glm(mastocitoma ~ castracao + idade + raca + sexo + porte, data = ds, family = binomial)
summary(modelo_porte)

# Qual é o melhor modelo, porte ou peso_log

anova(modelo_peso_log, modelo_porte)

# Teste de Hosmer and Lemeshow
# Avalia se os valores preditos pelo modelo é igual ao valor observado. Ou seja, ele avalia o ajuste do modelo.
## H0: é que os valores preditos pelo modelo é igual aos valores observados, ou seja, o meu modelo tem um bom ajuste. o valor p > 0.05.
## H1: é que os valores preditos pelo modelo não é igual aos valores observados, ou seja, o meu modelo não tem um bom ajuste. o valor de p < 0.05. 

ds$valor_predito <- predict(modelo_peso_log, type = 'response')

ResourceSelection::hoslem.test(ds$mastocitoma, ds$valor_predito )
# baseado no teste acima, o meu valor de p foi de 0.75 (maidor do que 0.05), sendo assim eu fico com a hipotese nula, ou seja, o meu modelo tem um bom ajuste.


ResourceSelection::hoslem.test(ds$mastocitoma, predict(modelo_porte, type = 'response') )

ResourceSelection::hoslem.test(ds$mastocitoma, predict(modelo, type = 'response') )

## VALORES INFLUENTES
# Valores influentes são aqueles que, se removidos do modelo, influenciam os valores dos coeficientes dos preditores. Outliers são valores que destoam do restante. 


### Calculando a distância de Cook

# Distância de Cook: o quanto uma observação influenciaria o meu modelo se eu a removesse. Uma distância de Cook alta significa que aquele valor é influente. Distâncias acima de 1 são influentes. Distâncias acima de 4/N são sinal de alerta. N é o número de observações no estudo.

plot(cooks.distance(modelo_peso_log)) 
abline(h = 0.004)

check_outliers(modelo_peso_log)



### Calculando o Leaverage (Alavancagem)

# Leverage: valor ideal é 1 + K / N. K é o número de preditores do modelo e N o número de observações no estudo. Se 2 ou 3 vezes maior do que isso, atenção. O leverage significa o quão extremo é uma observação em relação aos valores das variáveis preditoras. Ele mede o quanto uma observação pode puxar a curva S da regressão logística.

plot(hatvalues(modelo_peso_log)) # valores leaverage (alavancagem)
abline(h = 0.012, col = 'red') # o valor 1 + k / N * 2
abline(h = 0.018) # o valor 1 + k / N * 3cooks.distance(modelo_peso_log)

which(hatvalues(modelo_peso_log) > 0.018)

modelo_peso_log_sem_linas <- glm(mastocitoma ~ castracao + idade + log(peso) + raca + sexo, data = ds[ -c(536,103) , ], family = binomial)
summary(modelo_peso_log)

modelo_peso_log <- glm(mastocitoma ~ castracao + idade + log(peso) + raca + sexo, data = ds, family = binomial)
summary(modelo_peso_log)



### ENCONTRANDO OUTLIERS

# Resíduos padronizados: são considerados valores extremos aqueles que estão fora da faixa -2/2. Se você quiser ser mais conservador, pode adotar a faixa entre -3/3.

rstandard(modelo_peso_log) # o Zscore (valores padronizados) dos residuos do modelo

plot(rstandard(modelo_peso_log))

check_outliers(rstandard(modelo_peso_log), method = 'zscore', threshold = list('zscore' = 3))

# PRESTEM ATENÇAO, NO CHECK_OUTLIERS PARA ZSCORE, NAO UTILIZE O OBJETO MODELO PORQUE ELE VAI TRAZER O OUTLIER DE CADA VARIAVEL (PESO E IDADE) E NAO DOS ERROS PADRONIZADOS

check_outliers(modelo_peso_log, method = 'zscore', threshold = list('zscore' = 3)) # NAO UTILIZAR PORQUE VOCE VAI ACHAR QUE SAO IMPORTANTES ESSES OUTLIERS.

## INDEPENDÊNCIA DOS RESÍDUOS

# Os resíduos eles devem ser independentes, ou seja, não pode ter animais repetidos (dados pareados) e agrupamentos ocultos (clusters).  Para verificar a independencia dos resíduos, vou utilizar um teste chamado Durbin Watson. Se o valor do teste estiver entre 1.5 e 2.5, eu considero os meus resíduos INDEPENDENTES. 

check_autocorrelation(modelo_peso_log)
car::durbinWatsonTest(modelo_peso_log)

## Para visualizar a autocorrelacao, eu preciso ver o comportamento dos residuos do modelo em relaçao à ordem (indice) que o meus dados estao no banco de dados. Mas, eu nao consigo plotar o indice, portanto, eu vou criar uma variável chamad ordem na tabela.

ds$ordem <- 1:936

plot(ds$ordem, residuals(modelo_peso_log))
abline(h = 0)

# no plot, podemos observar que os residuos não sao independentes. Eu supeito da ordem, para isso, vou embaralhar a ordem. Vou bagunçar o banco de dados da Elis.

set.seed(123)
ds_embaralhado <- ds[sample(nrow(ds)), ]
# Agora que eu embaralhei os dados, preciso criar uma nova ordem

ds_embaralhado$ordem <- 1:936


modelo_peso_log_embaralhado <- glm(mastocitoma ~ castracao + idade + log(peso) + raca + sexo, data = ds_embaralhado, family = binomial)


plot(ds_embaralhado$ordem, residuals(modelo_peso_log_embaralhado))
abline(h = 0)

check_autocorrelation(modelo_peso_log_embaralhado)

# =============================================================================
# AVALIANDO O AJUSTE DO MODELO
# =============================================================================

# Teste de Hosmer-Lemeshow
# Esse teste verifica o ajuste do modelo (goodness of fit), comparando os dados da ocorrência do mastocitoma (frequências observadas) com os valores ajustados, que são as probabilidade previstas pelo modelo. 
# Como hipóteses, esse teste apresenta:
# H0 = p > 0.05 = o ajuste do modelo é bom, ou seja, não há diferença entre as frequencias esperadas e as frequencias observadas.
# H1 = p < 0.05 = o ajuste do modelo NÃO é bom, ou seja, há diferença entre as frequencias esperadas e as frequencias observadas.



# =============================================================================
# DESCRIÇÃO DOS RESULTADOS
# =============================================================================

report::report(exp(modelo_peso_log))

# =============================================================================
# AVALIANDO OS EFEITOS DOS PREDITORES
# =============================================================================

# Para avaliar os efeitos dos preditores, precisamos transformá-los. Lembre-se de que os coeficientes estão no formato de log de Odds ou log da chance, o que dificulta a interpretação. Para isso, temos que fazer o exponencial do valor de cada um dos coeficientes. Para isso, podemos utilizar uma função chamada tab_model() do pacote sjPlot, em que ele calcula isso e já entrega uma tabela formatada.

modelo_peso_log <- glm(mastocitoma ~ castracao + idade + log(peso) + raca + sexo, data = ds, family = binomial)
summary(modelo_peso_log)

sjPlot::tab_model(modelo_peso_log)

# =============================================================================
# AVALIANDO CAPACIDADE DISCRIMINATÓRIA DO MODELO
# =============================================================================






# =============================================================================
# SALVANDO RESULTADOS
# =============================================================================

